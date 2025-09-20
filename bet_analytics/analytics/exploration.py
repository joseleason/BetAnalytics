"""Exploratory data analysis utilities for odds datasets."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Mapping

import pandas as pd

from bet_analytics.data import SportsbookReviewOddsDataset
from bet_analytics.features import american_to_probability


@dataclass(frozen=True)
class ExplorationReport:
    """Structured exploratory summary for downstream consumption."""

    coverage: Mapping[str, object]
    season_breakdown: pd.DataFrame
    vig_summary: pd.DataFrame
    movement_summary: pd.DataFrame
    clv_summary: pd.DataFrame

    def to_dict(self) -> Mapping[str, object]:
        return {
            "coverage": dict(self.coverage),
            "season_breakdown": self.season_breakdown.to_dict(orient="list"),
            "vig_summary": self.vig_summary.to_dict(orient="list"),
            "movement_summary": self.movement_summary.to_dict(orient="list"),
            "clv_summary": self.clv_summary.to_dict(orient="list"),
        }


def summarize_odds_dataset(dataset: SportsbookReviewOddsDataset) -> ExplorationReport:
    frame = dataset.to_frame().copy()
    frame["game_date"] = pd.to_datetime(frame["game_date"], utc=True)

    frame["spread_movement"] = frame["close_home_spread"] - frame["open_home_spread"]
    frame["total_movement"] = frame["close_total"] - frame["open_total"]
    frame["home_moneyline_implied_close"] = american_to_probability(
        frame["close_home_moneyline"]
    )
    frame["away_moneyline_implied_close"] = american_to_probability(
        frame["close_away_moneyline"]
    )
    frame["home_moneyline_implied_open"] = american_to_probability(
        frame["open_home_moneyline"]
    )
    frame["home_moneyline_edge_movement"] = (
        frame["home_moneyline_implied_close"] - frame["home_moneyline_implied_open"]
    )
    frame["spread_implied_open"] = american_to_probability(frame["open_home_spread_price"])
    frame["spread_implied_close"] = american_to_probability(frame["close_home_spread_price"])
    frame["total_implied_open"] = american_to_probability(frame["open_over_price"])
    frame["total_implied_close"] = american_to_probability(frame["close_over_price"])
    frame["moneyline_vig_close"] = (
        frame["home_moneyline_implied_close"]
        + american_to_probability(frame["close_away_moneyline"])
        - 1.0
    )
    frame["spread_vig_close"] = (
        american_to_probability(frame["close_home_spread_price"])
        + american_to_probability(frame["close_away_spread_price"])
        - 1.0
    )
    frame["total_vig_close"] = (
        american_to_probability(frame["close_over_price"])
        + american_to_probability(frame["close_under_price"])
        - 1.0
    )

    coverage = {
        "seasons": dataset.seasons_available(),
        "rows": int(frame.shape[0]),
        "date_min": frame["game_date"].min().date().isoformat() if not frame.empty else None,
        "date_max": frame["game_date"].max().date().isoformat() if not frame.empty else None,
    }

    season_breakdown = (
        frame.groupby("season")
        .agg(
            games=("game_id", "nunique"),
            avg_moneyline_vig=("moneyline_vig_close", "mean"),
            avg_spread_vig=("spread_vig_close", "mean"),
            avg_total_vig=("total_vig_close", "mean"),
        )
        .reset_index()
    )

    vig_summary = (
        frame[["moneyline_vig_close", "spread_vig_close", "total_vig_close"]]
        .agg(["mean", "median", "max", "min"])
        .rename_axis("statistic")
        .reset_index()
    )

    movement_summary = (
        frame[["spread_movement", "total_movement", "home_moneyline_edge_movement"]]
        .agg(["mean", "median", "std"])
        .rename_axis("statistic")
        .reset_index()
    )

    clv_summary = pd.DataFrame(
        {
            "market": ["spread", "total", "moneyline"],
            "avg_implied_movement": [
                (frame["spread_implied_close"] - frame["spread_implied_open"]).mean(),
                (frame["total_implied_close"] - frame["total_implied_open"]).mean(),
                frame["home_moneyline_edge_movement"].mean(),
            ],
            "median_implied_movement": [
                (frame["spread_implied_close"] - frame["spread_implied_open"]).median(),
                (frame["total_implied_close"] - frame["total_implied_open"]).median(),
                frame["home_moneyline_edge_movement"].median(),
            ],
        }
    )

    return ExplorationReport(
        coverage=coverage,
        season_breakdown=season_breakdown,
        vig_summary=vig_summary,
        movement_summary=movement_summary,
        clv_summary=clv_summary,
    )


__all__ = ["ExplorationReport", "summarize_odds_dataset"]
