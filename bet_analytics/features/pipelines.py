"""Feature pipeline orchestration for Sportsbook Review data."""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import pandas as pd

from bet_analytics.config import BetAnalyticsSettings, get_settings
from bet_analytics.data import (
    DatasetNotFoundError,
    SportsbookReviewOddsDataset,
    load_basketball_reference_team_stats,
)

from .engineering import FeaturePipelineConfig, FeatureSet, engineer_sportsbookreview_features


logger = logging.getLogger(__name__)


@dataclass
class SportsbookReviewFeaturePipeline:
    """Construct and persist feature tables from Sportsbook Review odds."""

    settings: BetAnalyticsSettings = field(default_factory=get_settings)
    config: FeaturePipelineConfig = field(default_factory=FeaturePipelineConfig)
    table_name: str = "sportsbookreview_joint_features"

    def build(self, dataset: SportsbookReviewOddsDataset) -> FeatureSet:
        frame = dataset.to_frame()
        team_metrics = None
        if self.config.include_reference_team_stats:
            try:
                team_metrics = load_basketball_reference_team_stats(
                    seasons=dataset.seasons_available(), settings=self.settings
                )
            except DatasetNotFoundError:
                logger.warning("Team metrics not available; proceeding without Basketball Reference features")
        return engineer_sportsbookreview_features(
            frame, config=self.config, team_metrics=team_metrics
        )

    def persist(self, feature_set: FeatureSet, *, table_name: Optional[str] = None) -> Path:
        name = table_name or self.table_name
        payload = _compose_payload(feature_set)
        target_dir = self.settings.feature_store_dir
        target_dir.mkdir(parents=True, exist_ok=True)
        parquet_path = target_dir / f"{name}.parquet"
        payload.to_parquet(parquet_path, index=False)
        manifest_path = target_dir / f"{name}.json"
        manifest = {
            "name": name,
            "rows": int(payload.shape[0]),
            "columns": list(payload.columns),
            "config": {
                "trailing_windows": list(self.config.trailing_windows),
                "min_games_for_trailing": self.config.min_games_for_trailing,
                "include_opening_lines": self.config.include_opening_lines,
                "include_rest_days": self.config.include_rest_days,
                "include_reference_team_stats": self.config.include_reference_team_stats,
            },
            "team_metrics_columns": feature_set.metadata.get("team_metrics_columns", []),
        }
        manifest_path.write_text(json.dumps(manifest, indent=2))
        return parquet_path

    def run(self, dataset: SportsbookReviewOddsDataset, *, persist: bool = True) -> FeatureSet:
        feature_set = self.build(dataset)
        if persist:
            self.persist(feature_set)
        return feature_set

    def load_from_store(self, *, table_name: Optional[str] = None) -> FeatureSet:
        name = table_name or self.table_name
        parquet_path = self.settings.feature_store_dir / f"{name}.parquet"
        if not parquet_path.is_file():
            msg = f"Feature table not found at {parquet_path}"
            raise FileNotFoundError(msg)
        frame = pd.read_parquet(parquet_path)
        target_columns = [
            "target_home_cover",
            "target_over_hits",
            "target_home_win",
            "target_home_margin",
            "target_total_points",
        ]
        missing = [column for column in target_columns if column not in frame.columns]
        if missing:
            msg = f"Persisted feature table missing targets: {missing}"
            raise ValueError(msg)
        targets = {name: frame.pop(name) for name in target_columns}
        return FeatureSet(features=frame, targets=targets, metadata={"source_path": str(parquet_path)})


def _compose_payload(feature_set: FeatureSet) -> pd.DataFrame:
    frame = feature_set.features.copy()
    for target_name, series in feature_set.targets.items():
        frame[target_name] = series
    return frame


__all__ = ["SportsbookReviewFeaturePipeline"]
