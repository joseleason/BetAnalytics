from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from bet_analytics.features import (
    FeaturePipelineConfig,
    FeatureSet,
    engineer_sportsbookreview_features,
)
from bet_analytics.models import ModelTrainingConfig, UnifiedModelTrainer


@pytest.fixture
def synthetic_odds_frame() -> pd.DataFrame:
    games = 60
    rng = np.random.default_rng(42)
    base_date = pd.Timestamp("2023-10-01", tz="UTC")
    home_teams = ["Lakers", "Warriors", "Celtics", "Bucks"]
    away_teams = ["Nets", "Heat", "Suns", "Knicks"]

    records = []
    for idx in range(games):
        home = home_teams[idx % len(home_teams)]
        away = away_teams[idx % len(away_teams)]
        game_date = base_date + pd.Timedelta(days=idx)
        away_score = 100 + int(rng.integers(-8, 15))
        home_score = away_score + int(rng.integers(-10, 15))
        open_spread = float(-3 + rng.integers(-2, 3))
        close_spread = open_spread + float(rng.normal(0, 0.5))
        open_total = float(220 + rng.integers(-5, 6))
        close_total = open_total + float(rng.normal(0, 1.5))

        records.append(
            {
                "game_id": f"{game_date.date()}-{home}-{away}",
                "season": 2024,
                "game_date": game_date,
                "home_team": home,
                "away_team": away,
                "home_team_score": home_score,
                "away_team_score": away_score,
                "open_home_spread": open_spread,
                "close_home_spread": close_spread,
                "open_home_spread_price": -110.0,
                "close_home_spread_price": -110.0,
                "open_away_spread_price": -110.0,
                "close_away_spread_price": -110.0,
                "open_home_moneyline": -150.0 + rng.integers(-40, 40),
                "close_home_moneyline": -150.0 + rng.integers(-40, 40),
                "open_away_moneyline": 130.0 + rng.integers(-40, 40),
                "close_away_moneyline": 130.0 + rng.integers(-40, 40),
                "open_total": open_total,
                "close_total": close_total,
                "open_over_price": -110.0,
                "close_over_price": -110.0,
                "open_under_price": -110.0,
                "close_under_price": -110.0,
            }
        )
    return pd.DataFrame.from_records(records)


@pytest.fixture
def synthetic_team_metrics() -> pd.DataFrame:
    rng = np.random.default_rng(7)
    teams = [
        "Lakers",
        "Warriors",
        "Celtics",
        "Bucks",
        "Nets",
        "Heat",
        "Suns",
        "Knicks",
    ]
    records = []
    for team in teams:
        records.append(
            {
                "team": team,
                "season": 2024,
                "team_off_rtg": 110 + rng.normal(0, 3),
                "team_def_rtg": 108 + rng.normal(0, 3),
                "team_net_rtg": 2 + rng.normal(0, 1),
                "team_pace": 99 + rng.normal(0, 1),
                "team_srs": rng.normal(0, 1),
                "team_mov": rng.normal(0, 1),
            }
        )
    return pd.DataFrame.from_records(records)


@pytest.fixture
def synthetic_feature_set(
    synthetic_odds_frame: pd.DataFrame, synthetic_team_metrics: pd.DataFrame
) -> FeatureSet:
    config = FeaturePipelineConfig(trailing_windows=(3, 5, 10))
    return engineer_sportsbookreview_features(
        synthetic_odds_frame, config=config, team_metrics=synthetic_team_metrics
    )


def test_feature_engineering_produces_targets(synthetic_feature_set: FeatureSet) -> None:
    assert synthetic_feature_set.features.shape[0] > 0
    expected_targets = {
        "target_home_cover",
        "target_over_hits",
        "target_home_win",
        "target_home_margin",
        "target_total_points",
    }
    assert expected_targets.issubset(synthetic_feature_set.targets.keys())
    assert "home_cover_rate_3" in synthetic_feature_set.features.columns
    assert "home_margin_avg_10" in synthetic_feature_set.features.columns
    assert "home_rest_advantage" in synthetic_feature_set.features.columns
    assert "team_net_rtg_delta" in synthetic_feature_set.features.columns


def test_unified_model_trainer_builds_profitable_signals(
    synthetic_feature_set: FeatureSet,
) -> None:
    trainer = UnifiedModelTrainer(
        config=ModelTrainingConfig(
            time_series_splits=3,
            test_window=12,
            edge_threshold=0.0,
            min_bet_size=0.0,
            kelly_fraction=0.25,
        )
    )
    report = trainer.train(synthetic_feature_set)
    assert "margin_mae" in report.regression_metrics
    assert report.model.margin_sigma > 0
    predictions = report.model.predict(synthetic_feature_set.features.head(5))
    assert {
        "pred_margin_mean",
        "pred_total_mean",
        "pred_home_cover_prob_model",
    }.issubset(predictions.columns)
    spread_metrics = report.profitability["spread"]
    assert spread_metrics.bets >= 0
    assert spread_metrics.ending_bankroll != 0
    assert np.isfinite(spread_metrics.avg_clv)
