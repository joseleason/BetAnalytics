"""Feature engineering primitives for Sportsbook Review odds data."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Mapping, MutableMapping, Optional, Sequence

import numpy as np
import pandas as pd

REQUIRED_COLUMNS = {
    "game_id",
    "game_date",
    "season",
    "home_team",
    "away_team",
    "home_team_score",
    "away_team_score",
    "close_home_spread",
    "open_home_spread",
    "close_home_spread_price",
    "open_home_spread_price",
    "close_away_spread_price",
    "open_away_spread_price",
    "close_home_moneyline",
    "open_home_moneyline",
    "close_away_moneyline",
    "open_away_moneyline",
    "close_total",
    "open_total",
    "close_over_price",
    "open_over_price",
    "close_under_price",
    "open_under_price",
}


@dataclass(frozen=True)
class FeaturePipelineConfig:
    """Configuration for Sportsbook Review-derived feature sets."""

    trailing_windows: Sequence[int] = (5, 10, 20)
    min_games_for_trailing: int = 3
    include_opening_lines: bool = True
    include_rest_days: bool = True
    include_reference_team_stats: bool = True


@dataclass(frozen=True)
class FeatureSet:
    """Container for engineered features and downstream targets."""

    features: pd.DataFrame
    targets: Mapping[str, pd.Series]
    metadata: Mapping[str, object]

    def as_training_frame(self, target_name: str) -> pd.DataFrame:
        if target_name not in self.targets:
            msg = f"Target '{target_name}' is not available"
            raise KeyError(msg)
        frame = self.features.copy()
        frame[target_name] = self.targets[target_name]
        return frame


def engineer_sportsbookreview_features(
    frame: pd.DataFrame,
    *,
    config: FeaturePipelineConfig | None = None,
    team_metrics: Optional[pd.DataFrame] = None,
) -> FeatureSet:
    """Return engineered features and targets from Sportsbook Review odds."""

    if frame.empty:
        msg = "Cannot engineer features from an empty DataFrame"
        raise ValueError(msg)
    missing = REQUIRED_COLUMNS.difference(frame.columns)
    if missing:
        msg = f"Input frame missing columns: {sorted(missing)}"
        raise ValueError(msg)

    cfg = config or FeaturePipelineConfig()
    include_team_metrics = (
        cfg.include_reference_team_stats and team_metrics is not None and not team_metrics.empty
    )
    if include_team_metrics:
        team_metrics = team_metrics.copy()

    working = frame.copy()
    working.sort_values(["game_date", "game_id"], inplace=True)
    working["game_date"] = pd.to_datetime(working["game_date"], utc=True)
    working["game_dayofweek"] = working["game_date"].dt.dayofweek.astype("int8")
    working["game_month"] = working["game_date"].dt.month.astype("int8")

    working["home_margin"] = working["home_team_score"] - working["away_team_score"]
    working["total_points"] = working["home_team_score"] + working["away_team_score"]

    spread_margin = working["home_margin"] + working["close_home_spread"]
    home_cover = pd.Series(pd.NA, index=working.index, dtype="float64")
    home_cover.loc[spread_margin > 0] = 1.0
    home_cover.loc[spread_margin < 0] = 0.0

    total_margin = working["total_points"] - working["close_total"]
    over_result = pd.Series(pd.NA, index=working.index, dtype="float64")
    over_result.loc[total_margin > 0] = 1.0
    over_result.loc[total_margin < 0] = 0.0

    working["target_home_cover"] = home_cover
    working["target_over_hits"] = over_result
    working["target_home_win"] = (working["home_margin"] > 0).astype("int8")
    working["target_home_margin"] = working["home_margin"].astype("float32")
    working["target_total_points"] = working["total_points"].astype("float32")

    working["home_is_favorite"] = (working["close_home_spread"] < 0).astype("int8")
    working["spread_movement"] = (
        working["close_home_spread"] - working["open_home_spread"]
    )
    working["total_movement"] = working["close_total"] - working["open_total"]

    working["home_moneyline_implied_close"] = american_to_probability(
        working["close_home_moneyline"]
    )
    working["away_moneyline_implied_close"] = american_to_probability(
        working["close_away_moneyline"]
    )
    working["home_moneyline_implied_open"] = american_to_probability(
        working["open_home_moneyline"]
    )
    working["away_moneyline_implied_open"] = american_to_probability(
        working["open_away_moneyline"]
    )

    working["home_moneyline_edge_movement"] = (
        working["home_moneyline_implied_close"] - working["home_moneyline_implied_open"]
    )
    working["away_moneyline_edge_movement"] = (
        working["away_moneyline_implied_close"] - working["away_moneyline_implied_open"]
    )

    working["moneyline_vig_close"] = (
        working["home_moneyline_implied_close"]
        + working["away_moneyline_implied_close"]
        - 1.0
    )
    working["moneyline_vig_open"] = (
        working["home_moneyline_implied_open"]
        + working["away_moneyline_implied_open"]
        - 1.0
    )

    working["home_spread_implied_close"] = american_to_probability(
        working["close_home_spread_price"]
    )
    working["away_spread_implied_close"] = american_to_probability(
        working["close_away_spread_price"]
    )
    working["home_spread_implied_open"] = american_to_probability(
        working["open_home_spread_price"]
    )
    working["away_spread_implied_open"] = american_to_probability(
        working["open_away_spread_price"]
    )

    working["spread_vig_close"] = (
        working["home_spread_implied_close"]
        + working["away_spread_implied_close"]
        - 1.0
    )
    working["spread_vig_open"] = (
        working["home_spread_implied_open"]
        + working["away_spread_implied_open"]
        - 1.0
    )
    working["spread_price_movement"] = (
        working["home_spread_implied_close"] - working["home_spread_implied_open"]
    )

    working["over_implied_close"] = american_to_probability(working["close_over_price"])
    working["under_implied_close"] = american_to_probability(working["close_under_price"])
    working["over_implied_open"] = american_to_probability(working["open_over_price"])
    working["under_implied_open"] = american_to_probability(working["open_under_price"])
    working["total_vig_close"] = working["over_implied_close"] + working["under_implied_close"] - 1.0
    working["total_vig_open"] = working["over_implied_open"] + working["under_implied_open"] - 1.0
    working["total_prob_movement"] = working["over_implied_close"] - working["over_implied_open"]

    if cfg.include_rest_days:
        working["home_rest_days"] = _compute_rest_days(working, "home_team")
        working["away_rest_days"] = _compute_rest_days(working, "away_team")
        home_congestion = _compute_congestion_flags(
            working, team_col="home_team", rest_col="home_rest_days", prefix="home"
        )
        away_congestion = _compute_congestion_flags(
            working, team_col="away_team", rest_col="away_rest_days", prefix="away"
        )
        for column, series in home_congestion.items():
            working[column] = series
        for column, series in away_congestion.items():
            working[column] = series
    else:
        working["home_rest_days"] = np.nan
        working["away_rest_days"] = np.nan
        working["home_back_to_back"] = 0
        working["away_back_to_back"] = 0
        working["home_third_in_four"] = 0
        working["away_third_in_four"] = 0
        working["home_fourth_in_five"] = 0
        working["away_fourth_in_five"] = 0

    working["home_rest_advantage"] = (
        working["home_rest_days"] - working["away_rest_days"]
    )

    working["home_games_played"] = working.groupby("home_team").cumcount()
    working["away_games_played"] = working.groupby("away_team").cumcount()

    for window in cfg.trailing_windows:
        working[f"home_margin_avg_{window}"] = _lagged_rolling_mean(
            working,
            group_col="home_team",
            value_col="home_margin",
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"away_margin_avg_{window}"] = _lagged_rolling_mean(
            working,
            group_col="away_team",
            value_col=-working["home_margin"],
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"home_total_avg_{window}"] = _lagged_rolling_mean(
            working,
            group_col="home_team",
            value_col="total_points",
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"away_total_avg_{window}"] = _lagged_rolling_mean(
            working,
            group_col="away_team",
            value_col="total_points",
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"home_cover_rate_{window}"] = _lagged_rolling_mean(
            working,
            group_col="home_team",
            value_col=working["target_home_cover"].fillna(0.5),
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"away_cover_rate_{window}"] = _lagged_rolling_mean(
            working,
            group_col="away_team",
            value_col=1.0 - working["target_home_cover"].fillna(0.5),
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"home_win_rate_{window}"] = _lagged_rolling_mean(
            working,
            group_col="home_team",
            value_col=working["target_home_win"],
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )
        working[f"away_win_rate_{window}"] = _lagged_rolling_mean(
            working,
            group_col="away_team",
            value_col=1 - working["target_home_win"],
            window=window,
            min_periods=cfg.min_games_for_trailing,
        )

    team_metrics_columns: Sequence[str] = []
    if include_team_metrics:
        working, team_metrics_columns = _attach_team_metrics(working, team_metrics)

    feature_columns = _select_feature_columns(working, cfg)
    feature_frame = working.loc[:, feature_columns].copy()
    feature_frame = feature_frame.dropna(subset=["close_home_spread", "close_total"], how="any")

    id_columns = {"game_id", "game_date", "home_team", "away_team", "season"}
    numeric_columns = [
        name
        for name in feature_frame.columns
        if name not in id_columns and pd.api.types.is_numeric_dtype(feature_frame[name])
    ]
    for column in numeric_columns:
        series = feature_frame[column]
        if series.isna().any():
            feature_frame[column] = series.fillna(series.mean())

    targets: MutableMapping[str, pd.Series] = {
        "target_home_cover": working["target_home_cover"].astype("float64"),
        "target_over_hits": working["target_over_hits"].astype("float64"),
        "target_home_win": working["target_home_win"].astype("int8"),
        "target_home_margin": working["target_home_margin"].astype("float32"),
        "target_total_points": working["target_total_points"].astype("float32"),
    }

    for key, series in targets.items():
        valid_index = series.dropna().index
        feature_frame = feature_frame.loc[feature_frame.index.intersection(valid_index)]
        targets[key] = series.loc[feature_frame.index]

    metadata: Dict[str, object] = {
        "rows": int(feature_frame.shape[0]),
        "columns": list(feature_frame.columns),
        "trailing_windows": list(cfg.trailing_windows),
        "team_metrics_columns": list(team_metrics_columns),
    }
    return FeatureSet(features=feature_frame, targets=targets, metadata=metadata)


def _select_feature_columns(frame: pd.DataFrame, config: FeaturePipelineConfig) -> Sequence[str]:
    base_columns = [
        "game_id",
        "season",
        "game_date",
        "game_dayofweek",
        "game_month",
        "home_team",
        "away_team",
        "close_home_spread",
        "close_total",
        "home_is_favorite",
        "spread_movement",
        "total_movement",
        "home_moneyline_implied_close",
        "away_moneyline_implied_close",
        "moneyline_vig_close",
        "home_spread_implied_close",
        "away_spread_implied_close",
        "spread_vig_close",
        "over_implied_close",
        "under_implied_close",
        "total_vig_close",
        "home_games_played",
        "away_games_played",
    ]
    if config.include_opening_lines:
        base_columns.extend(
            [
                "open_home_spread",
                "open_total",
                "home_moneyline_implied_open",
                "away_moneyline_implied_open",
                "moneyline_vig_open",
                "home_spread_implied_open",
                "away_spread_implied_open",
                "spread_vig_open",
                "over_implied_open",
                "under_implied_open",
                "total_vig_open",
                "home_moneyline_edge_movement",
                "away_moneyline_edge_movement",
                "spread_price_movement",
                "total_prob_movement",
            ]
        )
    if config.include_rest_days:
        base_columns.extend(
            [
                "home_rest_days",
                "away_rest_days",
                "home_rest_advantage",
                "home_back_to_back",
                "away_back_to_back",
                "home_third_in_four",
                "away_third_in_four",
                "home_fourth_in_five",
                "away_fourth_in_five",
            ]
        )

    for window in config.trailing_windows:
        base_columns.extend(
            [
                f"home_margin_avg_{window}",
                f"away_margin_avg_{window}",
                f"home_total_avg_{window}",
                f"away_total_avg_{window}",
                f"home_cover_rate_{window}",
                f"away_cover_rate_{window}",
                f"home_win_rate_{window}",
                f"away_win_rate_{window}",
            ]
        )
    base_columns.extend(
        [
            "home_team_off_rtg",
            "away_team_off_rtg",
            "home_team_def_rtg",
            "away_team_def_rtg",
            "home_team_net_rtg",
            "away_team_net_rtg",
            "home_team_pace",
            "away_team_pace",
            "home_team_srs",
            "away_team_srs",
            "home_team_mov",
            "away_team_mov",
            "team_off_rtg_delta",
            "team_def_rtg_delta",
            "team_net_rtg_delta",
            "team_pace_delta",
            "team_srs_delta",
        ]
    )
    return [column for column in base_columns if column in frame.columns]


def american_to_probability(series: pd.Series) -> pd.Series:
    values = series.astype("float64")
    result = pd.Series(np.nan, index=values.index, dtype="float64")
    mask_positive = values > 0
    mask_negative = values < 0
    result.loc[mask_positive] = 100.0 / (values.loc[mask_positive] + 100.0)
    result.loc[mask_negative] = (-values.loc[mask_negative]) / ((-values.loc[mask_negative]) + 100.0)
    result.loc[values == 0] = np.nan
    return result


def _compute_rest_days(frame: pd.DataFrame, team_col: str) -> pd.Series:
    return (
        frame.groupby(team_col)["game_date"].diff().dt.days.fillna(7.0).astype("float32")
    )


def _compute_congestion_flags(
    frame: pd.DataFrame, *, team_col: str, rest_col: str, prefix: str
) -> Dict[str, pd.Series]:
    rest = frame[rest_col].fillna(7.0)
    grouped = rest.groupby(frame[team_col])
    rolling_two = grouped.transform(lambda s: s.rolling(window=2, min_periods=2).sum())
    rolling_three = grouped.transform(lambda s: s.rolling(window=3, min_periods=3).sum())

    features: Dict[str, pd.Series] = {}
    features[f"{prefix}_back_to_back"] = (rest <= 1).astype("int8")
    features[f"{prefix}_third_in_four"] = (rolling_two <= 3).fillna(0).astype("int8")
    features[f"{prefix}_fourth_in_five"] = (rolling_three <= 4).fillna(0).astype("int8")
    return features


def _attach_team_metrics(
    frame: pd.DataFrame, team_metrics: pd.DataFrame
) -> tuple[pd.DataFrame, Sequence[str]]:
    metrics = team_metrics.copy()
    if "team" not in metrics.columns or "season" not in metrics.columns:
        return frame, []

    metrics["team"] = metrics["team"].astype(str).str.strip()
    metrics = metrics.dropna(subset=["team"]).drop_duplicates(
        subset=["team", "season"], keep="first"
    )
    metrics["team_key"] = metrics["team"].map(_normalize_team_label)

    frame = frame.copy()
    frame["home_team_key"] = frame["home_team"].map(_normalize_team_label)
    frame["away_team_key"] = frame["away_team"].map(_normalize_team_label)

    value_columns = [
        column for column in metrics.columns if column not in {"team", "season", "team_key"}
    ]

    home_metrics = metrics.rename(columns={col: f"home_{col}" for col in value_columns})
    away_metrics = metrics.rename(columns={col: f"away_{col}" for col in value_columns})

    frame = frame.merge(
        home_metrics.drop(columns=["team"], errors="ignore").rename(
            columns={"team_key": "home_team_key"}
        ),
        on=["season", "home_team_key"],
        how="left",
    )
    frame = frame.merge(
        away_metrics.drop(columns=["team"], errors="ignore").rename(
            columns={"team_key": "away_team_key"}
        ),
        on=["season", "away_team_key"],
        how="left",
    )

    frame = frame.drop(columns=["home_team_key", "away_team_key"], errors="ignore")

    team_metrics_columns = [
        column
        for column in frame.columns
        if column.startswith("home_team_") or column.startswith("away_team_")
    ]

    if {"home_team_off_rtg", "away_team_off_rtg"}.issubset(frame.columns):
        frame["team_off_rtg_delta"] = frame["home_team_off_rtg"] - frame["away_team_off_rtg"]
    if {"home_team_def_rtg", "away_team_def_rtg"}.issubset(frame.columns):
        frame["team_def_rtg_delta"] = frame["home_team_def_rtg"] - frame["away_team_def_rtg"]
    if {"home_team_net_rtg", "away_team_net_rtg"}.issubset(frame.columns):
        frame["team_net_rtg_delta"] = frame["home_team_net_rtg"] - frame["away_team_net_rtg"]
    if {"home_team_pace", "away_team_pace"}.issubset(frame.columns):
        frame["team_pace_delta"] = frame["home_team_pace"] - frame["away_team_pace"]
    if {"home_team_srs", "away_team_srs"}.issubset(frame.columns):
        frame["team_srs_delta"] = frame["home_team_srs"] - frame["away_team_srs"]

    return frame, team_metrics_columns


def _normalize_team_label(label: object) -> str:
    return (
        str(label)
        .replace("*", "")
        .replace(".", "")
        .replace("-", " ")
        .strip()
        .lower()
    )


def _lagged_rolling_mean(
    frame: pd.DataFrame,
    *,
    group_col: str,
    value_col: pd.Series | str,
    window: int,
    min_periods: int = 1,
) -> pd.Series:
    if isinstance(value_col, str):
        series = frame[value_col]
    else:
        series = pd.Series(value_col, index=frame.index)
    series = series.astype("float64")
    grouped = series.groupby(frame[group_col])
    shifted = grouped.shift(1)
    minimum = max(1, min_periods)
    return shifted.rolling(window=window, min_periods=minimum).mean().astype("float32")


__all__ = [
    "FeaturePipelineConfig",
    "FeatureSet",
    "american_to_probability",
    "engineer_sportsbookreview_features",
]
