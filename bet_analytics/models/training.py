"""Unified training pipeline producing market-ready betting signals."""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Dict, Iterable, List, Mapping, MutableMapping, Optional, Sequence, Tuple

import numpy as np
import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import HistGradientBoostingRegressor
from sklearn.impute import SimpleImputer
from sklearn.metrics import brier_score_loss, log_loss, mean_absolute_error, mean_squared_error
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import TimeSeriesSplit
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder

from bet_analytics.features import FeatureSet, american_to_probability


@dataclass(frozen=True)
class ModelTrainingConfig:
    """Hyperparameters and strategy toggles for unified model training."""

    time_series_splits: int = 5
    test_window: Optional[int] = None
    edge_threshold: float = 0.02
    kelly_fraction: float = 0.5
    kelly_cap: float = 0.2
    min_bet_size: float = 0.05
    unit_stake: float = 1.0
    stake_mode: str = "kelly_bankroll"
    starting_bankroll: float = 100.0
    use_calibrated_probabilities: bool = True
    probability_calibration: str = "logistic"
    random_state: int = 42


@dataclass(frozen=True)
class MarketSimulationMetrics:
    """Profitability summary for a specific betting market."""

    market: str
    bets: int
    roi: float
    hit_rate: float
    avg_edge: float
    avg_kelly_fraction: float
    total_return: float
    total_staked: float
    avg_clv: float
    ending_bankroll: float
    bankroll_return: float
    max_drawdown: float

    def to_dict(self) -> Mapping[str, object]:
        return {
            "market": self.market,
            "bets": self.bets,
            "roi": self.roi,
            "hit_rate": self.hit_rate,
            "avg_edge": self.avg_edge,
            "avg_kelly_fraction": self.avg_kelly_fraction,
            "total_return": self.total_return,
            "total_staked": self.total_staked,
            "avg_clv": self.avg_clv,
            "ending_bankroll": self.ending_bankroll,
            "bankroll_return": self.bankroll_return,
            "max_drawdown": self.max_drawdown,
    }


@dataclass
class ProbabilityCalibrator:
    """Wrapper providing calibrated probabilities."""

    model: LogisticRegression

    def calibrate(self, probabilities: pd.Series) -> pd.Series:
        calibrated = probabilities.astype("float64").copy()
        mask = calibrated.notna()
        if not mask.any():
            return calibrated
        inputs = calibrated.loc[mask].to_numpy().reshape(-1, 1)
        inputs = np.clip(inputs, 1e-4, 1 - 1e-4)
        calibrated.loc[mask] = self.model.predict_proba(inputs)[:, 1]
        return calibrated


@dataclass
class UnifiedGameModel:
    """Container for fitted models and distributional assumptions."""

    margin_pipeline: Pipeline
    total_pipeline: Pipeline
    margin_sigma: float
    total_sigma: float
    model_columns: Sequence[str]
    calibrators: Mapping[str, Optional[ProbabilityCalibrator]]
    use_calibrated_probabilities: bool

    def prepare_features(self, features: pd.DataFrame) -> pd.DataFrame:
        frame = features.copy().reset_index(drop=True)
        if "game_date" in frame.columns:
            game_dates = pd.to_datetime(frame["game_date"], utc=True, errors="coerce")
            frame["game_date_ordinal"] = (
                game_dates.view("int64") // 86_400_000_000
            ).astype("float64")
        frame = frame.drop(columns=["game_id", "game_date"], errors="ignore")
        for column in self.model_columns:
            if column not in frame.columns:
                frame[column] = 0.0
        frame = frame.loc[:, list(self.model_columns)]
        return frame

    def predict(self, features: pd.DataFrame) -> pd.DataFrame:
        base_features = features.reset_index(drop=True).copy()
        prepared = self.prepare_features(base_features)
        margin_mean = self.margin_pipeline.predict(prepared)
        total_mean = self.total_pipeline.predict(prepared)
        margin_std = np.full_like(margin_mean, self.margin_sigma, dtype="float64")
        total_std = np.full_like(total_mean, self.total_sigma, dtype="float64")

        if not {"close_home_spread", "close_total"}.issubset(base_features.columns):
            msg = "Features must include close_home_spread and close_total for probability estimation"
            raise KeyError(msg)

        probabilities = _compute_probabilities(
            margin_mean=margin_mean,
            margin_sigma=self.margin_sigma,
            total_mean=total_mean,
            total_sigma=self.total_sigma,
            close_spread=base_features["close_home_spread"].to_numpy(),
            close_total=base_features["close_total"].to_numpy(),
        )

        result = pd.DataFrame(
            {
                "pred_margin_mean": margin_mean,
                "pred_margin_std": margin_std,
                "pred_total_mean": total_mean,
                "pred_total_std": total_std,
            }
        )
        result = pd.concat([result, probabilities], axis=1)

        if self.calibrators:
            result = _apply_probability_calibrators(result, self.calibrators)

        result["pred_home_cover_prob_model"] = _select_probability_column(
            result,
            base_col="pred_home_cover_prob",
            use_calibrated=self.use_calibrated_probabilities,
        )
        result["pred_home_win_prob_model"] = _select_probability_column(
            result,
            base_col="pred_home_win_prob",
            use_calibrated=self.use_calibrated_probabilities,
        )
        result["pred_over_prob_model"] = _select_probability_column(
            result,
            base_col="pred_over_prob",
            use_calibrated=self.use_calibrated_probabilities,
        )
        return result


@dataclass(frozen=True)
class TrainingReport:
    """Aggregate artefacts produced during unified model training."""

    model: UnifiedGameModel
    regression_metrics: Mapping[str, float]
    classification_metrics: Mapping[str, float]
    profitability: Mapping[str, MarketSimulationMetrics]
    evaluation_frame: pd.DataFrame


@dataclass
class UnifiedModelTrainer:
    """Train a joint scoreline model and derive betting opportunities."""

    config: ModelTrainingConfig = field(default_factory=ModelTrainingConfig)

    def train(self, feature_set: FeatureSet) -> TrainingReport:
        dataset = _prepare_training_data(feature_set)
        X = dataset.features
        targets = dataset.targets
        metadata = dataset.metadata
        categorical = dataset.categorical_features
        numeric = dataset.numeric_features

        fold_frames: List[pd.DataFrame] = []
        split = _build_splitter(self.config)

        for fold_index, (train_idx, test_idx) in enumerate(split.split(X), start=1):
            margin_pipeline = _build_regression_pipeline(
                categorical, numeric, random_state=self.config.random_state + fold_index
            )
            total_pipeline = _build_regression_pipeline(
                categorical, numeric, random_state=self.config.random_state + 2 * fold_index
            )

            X_train = X.iloc[train_idx]
            X_test = X.iloc[test_idx]

            y_margin_train = targets["target_home_margin"].iloc[train_idx]
            y_margin_test = targets["target_home_margin"].iloc[test_idx]
            y_total_train = targets["target_total_points"].iloc[train_idx]
            y_total_test = targets["target_total_points"].iloc[test_idx]

            margin_pipeline.fit(X_train, y_margin_train)
            total_pipeline.fit(X_train, y_total_train)

            margin_train_pred = margin_pipeline.predict(X_train)
            total_train_pred = total_pipeline.predict(X_train)
            sigma_margin = float(np.std(y_margin_train - margin_train_pred, ddof=1))
            sigma_total = float(np.std(y_total_train - total_train_pred, ddof=1))
            sigma_margin = max(sigma_margin, 1.0)
            sigma_total = max(sigma_total, 7.5)

            margin_pred = margin_pipeline.predict(X_test)
            total_pred = total_pipeline.predict(X_test)

            fold_meta = metadata.iloc[test_idx].copy().reset_index(drop=True)
            fold_meta["pred_margin_mean"] = margin_pred
            fold_meta["pred_margin_std"] = sigma_margin
            fold_meta["pred_total_mean"] = total_pred
            fold_meta["pred_total_std"] = sigma_total

            probabilities = _compute_probabilities(
                margin_mean=margin_pred,
                margin_sigma=sigma_margin,
                total_mean=total_pred,
                total_sigma=sigma_total,
                close_spread=fold_meta["close_home_spread"].to_numpy(),
                close_total=fold_meta["close_total"].to_numpy(),
            )
            fold_meta = pd.concat([fold_meta, probabilities], axis=1)

            for target_name, series in targets.items():
                fold_meta[target_name] = series.iloc[test_idx].reset_index(drop=True)

            fold_frames.append(fold_meta)

        evaluation_frame = pd.concat(fold_frames, ignore_index=True)

        probability_calibrators, calibrated_probs = _fit_probability_calibrators(
            evaluation_frame, self.config
        )
        if not calibrated_probs.empty:
            evaluation_frame = pd.concat([evaluation_frame, calibrated_probs], axis=1)

        regression_metrics = {
            "margin_mae": mean_absolute_error(
                evaluation_frame["target_home_margin"], evaluation_frame["pred_margin_mean"]
            ),
            "margin_rmse": math.sqrt(
                mean_squared_error(
                    evaluation_frame["target_home_margin"], evaluation_frame["pred_margin_mean"]
                )
            ),
            "total_mae": mean_absolute_error(
                evaluation_frame["target_total_points"], evaluation_frame["pred_total_mean"]
            ),
            "total_rmse": math.sqrt(
                mean_squared_error(
                    evaluation_frame["target_total_points"], evaluation_frame["pred_total_mean"]
                )
            ),
        }

        classification_metrics = _compute_classification_metrics(evaluation_frame)
        profitability = simulate_profitability(
            evaluation_frame, self.config
        )

        final_margin_pipeline = _build_regression_pipeline(
            categorical, numeric, random_state=self.config.random_state
        )
        final_total_pipeline = _build_regression_pipeline(
            categorical, numeric, random_state=self.config.random_state + 11
        )
        final_margin_pipeline.fit(X, targets["target_home_margin"])
        final_total_pipeline.fit(X, targets["target_total_points"])

        full_margin_pred = final_margin_pipeline.predict(X)
        full_total_pred = final_total_pipeline.predict(X)
        final_margin_sigma = float(
            np.std(targets["target_home_margin"] - full_margin_pred, ddof=1)
        )
        final_total_sigma = float(
            np.std(targets["target_total_points"] - full_total_pred, ddof=1)
        )
        final_margin_sigma = max(final_margin_sigma, 1.0)
        final_total_sigma = max(final_total_sigma, 7.5)

        model = UnifiedGameModel(
            margin_pipeline=final_margin_pipeline,
            total_pipeline=final_total_pipeline,
            margin_sigma=final_margin_sigma,
            total_sigma=final_total_sigma,
            model_columns=list(X.columns),
            calibrators=probability_calibrators,
            use_calibrated_probabilities=self.config.use_calibrated_probabilities,
        )

        return TrainingReport(
            model=model,
            regression_metrics=regression_metrics,
            classification_metrics=classification_metrics,
            profitability=profitability,
            evaluation_frame=evaluation_frame,
        )


def simulate_profitability(
    evaluation_frame: pd.DataFrame, config: ModelTrainingConfig
) -> Mapping[str, MarketSimulationMetrics]:
    def _resolve_probability_column_name(
        frame: pd.DataFrame, base_col: str, use_calibrated: bool
    ) -> str:
        calibrated = f"{base_col}_calibrated"
        if use_calibrated and calibrated in frame.columns:
            return calibrated
        return base_col

    spread_metrics = _simulate_market(
        evaluation_frame,
        market="spread",
        home_prob_col=_resolve_probability_column_name(
            evaluation_frame, "pred_home_cover_prob", config.use_calibrated_probabilities
        ),
        implied_home_col="home_spread_implied_close",
        implied_away_col="away_spread_implied_close",
        price_home_col="close_home_spread_price",
        price_away_col="close_away_spread_price",
        target_home_col="target_home_cover",
        config=config,
    )
    total_metrics = _simulate_market(
        evaluation_frame,
        market="total",
        home_prob_col=_resolve_probability_column_name(
            evaluation_frame, "pred_over_prob", config.use_calibrated_probabilities
        ),
        implied_home_col="over_implied_close",
        implied_away_col="under_implied_close",
        price_home_col="close_over_price",
        price_away_col="close_under_price",
        target_home_col="target_over_hits",
        config=config,
    )
    moneyline_metrics = _simulate_market(
        evaluation_frame,
        market="moneyline",
        home_prob_col=_resolve_probability_column_name(
            evaluation_frame, "pred_home_win_prob", config.use_calibrated_probabilities
        ),
        implied_home_col="home_moneyline_implied_close",
        implied_away_col="away_moneyline_implied_close",
        price_home_col="close_home_moneyline",
        price_away_col="close_away_moneyline",
        target_home_col="target_home_win",
        config=config,
    )
    return {
        "spread": spread_metrics,
        "total": total_metrics,
        "moneyline": moneyline_metrics,
    }


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class _TrainingDataset:
    features: pd.DataFrame
    targets: Mapping[str, pd.Series]
    metadata: pd.DataFrame
    categorical_features: Sequence[str]
    numeric_features: Sequence[str]


def _prepare_training_data(feature_set: FeatureSet) -> _TrainingDataset:
    features = feature_set.features.reset_index(drop=True).copy()
    targets = {name: series.reset_index(drop=True) for name, series in feature_set.targets.items()}

    features["game_date"] = pd.to_datetime(features["game_date"], utc=True, errors="coerce")
    features["game_date_ordinal"] = (
        features["game_date"].view("int64") // 86_400_000_000
    ).astype("float64")

    metadata_columns = [
        "game_id",
        "game_date",
        "home_team",
        "away_team",
        "close_home_spread",
        "close_home_spread_price",
        "close_away_spread_price",
        "close_total",
        "close_over_price",
        "close_under_price",
        "close_home_moneyline",
        "close_away_moneyline",
        "home_spread_implied_close",
        "away_spread_implied_close",
        "over_implied_close",
        "under_implied_close",
        "home_moneyline_implied_close",
        "away_moneyline_implied_close",
    ]
    metadata = features.loc[:, [col for col in metadata_columns if col in features.columns]].copy()

    model_frame = features.drop(columns=["game_id", "game_date"], errors="ignore")
    categorical_features = [col for col in ("home_team", "away_team") if col in model_frame.columns]
    numeric_features = [col for col in model_frame.columns if col not in categorical_features]
    return _TrainingDataset(
        features=model_frame,
        targets=targets,
        metadata=metadata,
        categorical_features=categorical_features,
        numeric_features=numeric_features,
    )


def _build_splitter(config: ModelTrainingConfig) -> TimeSeriesSplit:
    if config.test_window:
        return TimeSeriesSplit(n_splits=config.time_series_splits, test_size=config.test_window)
    return TimeSeriesSplit(n_splits=config.time_series_splits)


def _build_regression_pipeline(
    categorical: Sequence[str], numeric: Sequence[str], *, random_state: int
) -> Pipeline:
    categorical_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="most_frequent")),
            (
                "encoder",
                OneHotEncoder(handle_unknown="ignore", sparse_output=False),
            ),
        ]
    )
    numeric_transformer = Pipeline(
        steps=[
            ("imputer", SimpleImputer(strategy="median")),
        ]
    )

    preprocessor = ColumnTransformer(
        transformers=[
            ("categorical", categorical_transformer, list(categorical)),
            ("numeric", numeric_transformer, list(numeric)),
        ]
    )

    regressor = HistGradientBoostingRegressor(
        max_depth=6,
        learning_rate=0.05,
        max_iter=400,
        random_state=random_state,
    )

    return Pipeline(steps=[("preprocess", preprocessor), ("regressor", regressor)])


def _compute_probabilities(
    *,
    margin_mean: np.ndarray,
    margin_sigma: float,
    total_mean: np.ndarray,
    total_sigma: float,
    close_spread: np.ndarray,
    close_total: np.ndarray,
) -> pd.DataFrame:
    sigma_margin = max(margin_sigma, 1.0)
    sigma_total = max(total_sigma, 7.5)

    margin_cdf_threshold = (-close_spread - margin_mean) / (sigma_margin * math.sqrt(2.0))
    margin_cdf = 0.5 * (1 + np.erf(margin_cdf_threshold))
    home_cover_prob = 1 - margin_cdf

    home_win_cdf_threshold = (-margin_mean) / (sigma_margin * math.sqrt(2.0))
    home_win_prob = 1 - (0.5 * (1 + np.erf(home_win_cdf_threshold)))

    total_cdf_threshold = (close_total - total_mean) / (sigma_total * math.sqrt(2.0))
    over_prob = 1 - (0.5 * (1 + np.erf(total_cdf_threshold)))

    frame = pd.DataFrame(
        {
            "pred_home_cover_prob": np.clip(home_cover_prob, 0.01, 0.99),
            "pred_home_win_prob": np.clip(home_win_prob, 0.01, 0.99),
            "pred_over_prob": np.clip(over_prob, 0.01, 0.99),
        }
    )
    frame["pred_away_cover_prob"] = 1.0 - frame["pred_home_cover_prob"]
    frame["pred_under_prob"] = 1.0 - frame["pred_over_prob"]
    frame["pred_away_win_prob"] = 1.0 - frame["pred_home_win_prob"]
    return frame


def _apply_probability_calibrators(
    frame: pd.DataFrame, calibrators: Mapping[str, Optional[ProbabilityCalibrator]]
) -> pd.DataFrame:
    result = frame.copy()
    mapping = {
        "home_cover": "pred_home_cover_prob",
        "home_win": "pred_home_win_prob",
        "over": "pred_over_prob",
    }
    for name, base_column in mapping.items():
        calibrator = calibrators.get(name)
        if calibrator is None or base_column not in result.columns:
            continue
        calibrated = calibrator.calibrate(result[base_column])
        result[f"{base_column}_calibrated"] = calibrated
        if base_column == "pred_home_cover_prob":
            result["pred_away_cover_prob_calibrated"] = 1.0 - calibrated
        if base_column == "pred_home_win_prob":
            result["pred_away_win_prob_calibrated"] = 1.0 - calibrated
        if base_column == "pred_over_prob":
            result["pred_under_prob_calibrated"] = 1.0 - calibrated
    return result


def _select_probability_column(
    frame: pd.DataFrame, *, base_col: str, use_calibrated: bool
) -> pd.Series:
    calibrated_col = f"{base_col}_calibrated"
    if use_calibrated and calibrated_col in frame.columns:
        return frame[calibrated_col]
    return frame[base_col]


def _fit_probability_calibrators(
    evaluation_frame: pd.DataFrame, config: ModelTrainingConfig
) -> Tuple[Mapping[str, Optional[ProbabilityCalibrator]], pd.DataFrame]:
    if config.probability_calibration.lower() != "logistic":
        return {}, pd.DataFrame()

    mapping = {
        "home_cover": ("target_home_cover", "pred_home_cover_prob"),
        "home_win": ("target_home_win", "pred_home_win_prob"),
        "over": ("target_over_hits", "pred_over_prob"),
    }
    calibrators: MutableMapping[str, Optional[ProbabilityCalibrator]] = {}
    calibrated_columns: MutableMapping[str, pd.Series] = {}

    for name, (target_col, prob_col) in mapping.items():
        if target_col not in evaluation_frame or prob_col not in evaluation_frame:
            calibrators[name] = None
            continue
        calibrator = _train_probability_calibrator(
            evaluation_frame[target_col], evaluation_frame[prob_col]
        )
        calibrators[name] = calibrator
        if calibrator is not None:
            calibrated = calibrator.calibrate(evaluation_frame[prob_col])
        else:
            calibrated = evaluation_frame[prob_col]
        calibrated_columns[f"{prob_col}_calibrated"] = calibrated
        if prob_col == "pred_home_cover_prob":
            calibrated_columns["pred_away_cover_prob_calibrated"] = 1.0 - calibrated
        if prob_col == "pred_home_win_prob":
            calibrated_columns["pred_away_win_prob_calibrated"] = 1.0 - calibrated
        if prob_col == "pred_over_prob":
            calibrated_columns["pred_under_prob_calibrated"] = 1.0 - calibrated

    calibrated_frame = pd.DataFrame(calibrated_columns) if calibrated_columns else pd.DataFrame()
    return calibrators, calibrated_frame


def _train_probability_calibrator(
    targets: pd.Series, probabilities: pd.Series
) -> Optional[ProbabilityCalibrator]:
    mask = targets.notna() & probabilities.notna()
    if mask.sum() < 30:
        return None
    X = probabilities.loc[mask].astype("float64").to_numpy().reshape(-1, 1)
    X = np.clip(X, 1e-4, 1 - 1e-4)
    y = targets.loc[mask].astype(int).to_numpy()
    model = LogisticRegression(max_iter=1000)
    model.fit(X, y)
    return ProbabilityCalibrator(model=model)


def _compute_classification_metrics(evaluation_frame: pd.DataFrame) -> Mapping[str, float]:
    metrics: MutableMapping[str, float] = {}

    def _safe_metric(true_series: pd.Series, prob_series: pd.Series, name_prefix: str) -> None:
        mask = true_series.notna()
        if mask.sum() == 0:
            metrics[f"{name_prefix}_brier"] = float("nan")
            metrics[f"{name_prefix}_log_loss"] = float("nan")
            return
        y_true = true_series.loc[mask].astype(float)
        y_prob = np.clip(prob_series.loc[mask].astype(float), 1e-4, 1 - 1e-4)
        metrics[f"{name_prefix}_brier"] = brier_score_loss(y_true, y_prob)
        metrics[f"{name_prefix}_log_loss"] = log_loss(y_true, y_prob)

    _safe_metric(
        evaluation_frame["target_home_cover"],
        evaluation_frame["pred_home_cover_prob"],
        "home_cover",
    )
    _safe_metric(
        evaluation_frame["target_home_win"],
        evaluation_frame["pred_home_win_prob"],
        "home_win",
    )
    _safe_metric(
        evaluation_frame["target_over_hits"],
        evaluation_frame["pred_over_prob"],
        "over",
    )

    if "pred_home_cover_prob_calibrated" in evaluation_frame.columns:
        _safe_metric(
            evaluation_frame["target_home_cover"],
            evaluation_frame["pred_home_cover_prob_calibrated"],
            "home_cover_calibrated",
        )
    if "pred_home_win_prob_calibrated" in evaluation_frame.columns:
        _safe_metric(
            evaluation_frame["target_home_win"],
            evaluation_frame["pred_home_win_prob_calibrated"],
            "home_win_calibrated",
        )
    if "pred_over_prob_calibrated" in evaluation_frame.columns:
        _safe_metric(
            evaluation_frame["target_over_hits"],
            evaluation_frame["pred_over_prob_calibrated"],
            "over_calibrated",
        )
    return metrics


def _simulate_market(
    frame: pd.DataFrame,
    *,
    market: str,
    home_prob_col: str,
    implied_home_col: str,
    implied_away_col: str,
    price_home_col: str,
    price_away_col: str,
    target_home_col: str,
    config: ModelTrainingConfig,
) -> MarketSimulationMetrics:
    bankroll = config.starting_bankroll
    total_staked = 0.0
    wins = 0
    bet_edges: List[float] = []
    kelly_allocations: List[float] = []
    clv_values: List[float] = []
    max_drawdown = 0.0
    peak_bankroll = bankroll

    for _, row in frame.iterrows():
        if bankroll <= 0:
            break

        home_prob = float(row.get(home_prob_col, np.nan))
        implied_home = float(row.get(implied_home_col, np.nan))
        implied_away = float(row.get(implied_away_col, np.nan))
        price_home = row.get(price_home_col)
        price_away = row.get(price_away_col)
        result_home = row.get(target_home_col)
        if any(math.isnan(value) for value in (home_prob, implied_home, implied_away)):
            continue
        if price_home is None or price_away is None or pd.isna(result_home):
            continue

        away_prob = 1.0 - home_prob
        home_edge = home_prob - implied_home
        away_edge = away_prob - implied_away
        best_edge = max(home_edge, away_edge)
        if best_edge < config.edge_threshold:
            continue

        if home_edge >= away_edge:
            prob = home_prob
            implied_prob = implied_home
            price = price_home
            outcome = float(result_home)
        else:
            prob = away_prob
            implied_prob = implied_away
            price = price_away
            outcome = 1.0 - float(result_home)

        decimal_odds = american_to_decimal(price)
        if math.isnan(decimal_odds) or decimal_odds <= 1.0:
            continue

        raw_kelly = _kelly_fraction(prob, decimal_odds)
        if raw_kelly <= 0:
            continue
        capped_kelly = min(raw_kelly, config.kelly_cap)
        actual_fraction = config.kelly_fraction * capped_kelly
        if config.stake_mode.lower() == "kelly_bankroll":
            stake = bankroll * actual_fraction
        else:
            stake = config.unit_stake * actual_fraction
        if stake < config.min_bet_size:
            continue

        stake = float(stake)
        if config.stake_mode.lower() == "kelly_bankroll" and stake > bankroll:
            stake = bankroll
        bet_edges.append(best_edge)
        kelly_allocations.append(actual_fraction)
        clv_values.append(prob - implied_prob)
        total_staked += stake

        if outcome >= 0.5:
            bankroll += stake * (decimal_odds - 1.0)
            wins += 1
        else:
            bankroll -= stake

        peak_bankroll = max(peak_bankroll, bankroll)
        if peak_bankroll > 0:
            drawdown = (peak_bankroll - bankroll) / peak_bankroll
            max_drawdown = max(max_drawdown, drawdown)

    bets = len(bet_edges)
    starting_bankroll = config.starting_bankroll
    total_return = bankroll - starting_bankroll
    roi = (total_return / total_staked) if total_staked > 0 else 0.0
    hit_rate = (wins / bets) if bets > 0 else 0.0
    avg_edge = float(np.mean(bet_edges)) if bet_edges else 0.0
    avg_fraction = float(np.mean(kelly_allocations)) if kelly_allocations else 0.0
    avg_clv = float(np.mean(clv_values)) if clv_values else 0.0
    bankroll_return = (
        (bankroll / starting_bankroll) - 1.0 if starting_bankroll > 0 else 0.0
    )
    return MarketSimulationMetrics(
        market=market,
        bets=bets,
        roi=roi,
        hit_rate=hit_rate,
        avg_edge=avg_edge,
        avg_kelly_fraction=avg_fraction,
        total_return=total_return,
        total_staked=total_staked,
        avg_clv=avg_clv,
        ending_bankroll=bankroll,
        bankroll_return=bankroll_return,
        max_drawdown=max_drawdown,
    )


def american_to_decimal(value: float) -> float:
    try:
        odds = float(value)
    except (TypeError, ValueError):
        return float("nan")
    if math.isnan(odds):
        return float("nan")
    if odds > 0:
        return odds / 100.0 + 1.0
    if odds < 0:
        return 100.0 / abs(odds) + 1.0
    return float("nan")


def _kelly_fraction(prob: float, decimal_odds: float) -> float:
    b = decimal_odds - 1.0
    q = 1.0 - prob
    numerator = b * prob - q
    if b <= 0:
        return 0.0
    return max(0.0, numerator / b)


__all__ = [
    "MarketSimulationMetrics",
    "ModelTrainingConfig",
    "TrainingReport",
    "UnifiedGameModel",
    "UnifiedModelTrainer",
    "simulate_profitability",
]
