"""Structured data loading utilities for modelling pipelines."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import List, Mapping, MutableMapping, Optional, Sequence

import pandas as pd

from bet_analytics.config import BetAnalyticsSettings, get_settings


class DatasetNotFoundError(FileNotFoundError):
    """Raised when an expected dataset is missing from the data lake."""


@dataclass(frozen=True)
class SeasonOddsData:
    """Materialized odds table and metadata for a single NBA season."""

    season: int
    odds: pd.DataFrame
    metadata: Mapping[str, object] | None = None

    def validate(self) -> None:
        if self.odds.empty:
            msg = f"Season {self.season} odds table is empty"
            raise ValueError(msg)
        required_columns = {
            "game_id",
            "game_date",
            "home_team",
            "away_team",
            "home_team_score",
            "away_team_score",
        }
        missing = required_columns.difference(self.odds.columns)
        if missing:
            msg = f"Season {self.season} odds table missing columns: {sorted(missing)}"
            raise ValueError(msg)


@dataclass(frozen=True)
class SportsbookReviewOddsDataset:
    """Collection of Sportsbook Review odds across seasons."""

    seasons: Sequence[SeasonOddsData]

    def to_frame(self) -> pd.DataFrame:
        frames = [season_data.odds.assign(season=season_data.season) for season_data in self.seasons]
        return pd.concat(frames, ignore_index=True)

    def metadata(self) -> Mapping[int, Mapping[str, object]]:
        manifest: MutableMapping[int, Mapping[str, object]] = {}
        for season_data in self.seasons:
            if season_data.metadata is not None:
                manifest[season_data.season] = season_data.metadata
        return manifest

    def seasons_available(self) -> List[int]:
        return sorted({season_data.season for season_data in self.seasons})


def load_sportsbookreview_odds(
    *,
    seasons: Sequence[int] | None = None,
    settings: Optional[BetAnalyticsSettings] = None,
    validate: bool = True,
) -> SportsbookReviewOddsDataset:
    """Load consolidated Sportsbook Review odds for the requested seasons."""

    resolved_settings = settings or get_settings()
    base_dir = resolved_settings.raw_data_dir / "sportsbookreview"
    if not base_dir.exists():
        msg = f"Sportsbook Review data directory not found: {base_dir}"
        raise DatasetNotFoundError(msg)

    available_directories = _discover_season_directories(base_dir)
    if not available_directories:
        msg = f"No season directories found under {base_dir}"
        raise DatasetNotFoundError(msg)

    target_seasons = set(seasons) if seasons else set(available_directories)
    season_data: List[SeasonOddsData] = []
    for season in sorted(target_seasons):
        season_dir = available_directories.get(season)
        if season_dir is None:
            msg = f"Season {season} odds data is not present under {base_dir}"
            raise DatasetNotFoundError(msg)
        data_path = season_dir / "odds.parquet"
        if not data_path.is_file():
            msg = f"Odds parquet missing for season {season}: {data_path}"
            raise DatasetNotFoundError(msg)
        frame = pd.read_parquet(data_path)
        metadata = _load_metadata(season_dir)
        season_record = SeasonOddsData(season=season, odds=frame, metadata=metadata)
        if validate:
            season_record.validate()
        season_data.append(season_record)

    dataset = SportsbookReviewOddsDataset(seasons=tuple(season_data))
    return dataset


def load_basketball_reference_team_stats(
    *,
    seasons: Sequence[int] | None = None,
    settings: Optional[BetAnalyticsSettings] = None,
) -> pd.DataFrame:
    """Load season-level team metrics collected from Basketball Reference."""

    resolved_settings = settings or get_settings()
    base_dir = resolved_settings.raw_data_dir / "basketball_reference"
    if not base_dir.exists():
        msg = f"Basketball Reference data directory not found: {base_dir}"
        raise DatasetNotFoundError(msg)

    available_directories = _discover_season_directories(base_dir)
    if not available_directories:
        msg = f"No season directories found under {base_dir}"
        raise DatasetNotFoundError(msg)

    target_seasons = set(seasons) if seasons else set(available_directories)
    frames: List[pd.DataFrame] = []
    for season in sorted(target_seasons):
        season_dir = available_directories.get(season)
        if season_dir is None:
            msg = f"Season {season} Basketball Reference data is not present under {base_dir}"
            raise DatasetNotFoundError(msg)
        season_frame = _assemble_team_metrics(season_dir, season)
        if season_frame.empty:
            continue
        frames.append(season_frame)

    if not frames:
        msg = "No Basketball Reference team metrics could be assembled"
        raise DatasetNotFoundError(msg)

    combined = pd.concat(frames, ignore_index=True)
    combined = combined.sort_values(["season", "team"]).reset_index(drop=True)
    return combined


def _discover_season_directories(base_dir: Path) -> Mapping[int, Path]:
    mapping: MutableMapping[int, Path] = {}
    for child in base_dir.iterdir():
        if not child.is_dir():
            continue
        if not child.name.startswith("season_"):
            continue
        suffix = child.name.replace("season_", "", 1)
        try:
            season = int(suffix)
        except ValueError:
            continue
        mapping[season] = child
    return mapping


def _load_metadata(season_dir: Path) -> Mapping[str, object] | None:
    metadata_path = season_dir / "metadata.json"
    if not metadata_path.is_file():
        return None
    with metadata_path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def _assemble_team_metrics(season_dir: Path, season: int) -> pd.DataFrame:
    tables = {
        "advanced": _load_table(season_dir, "team_advanced"),
        "misc": _load_table(season_dir, "team_misc"),
        "per_game": _load_table(season_dir, "team_per_game"),
    }

    base: Optional[pd.DataFrame] = None
    for table_name, table in tables.items():
        if table is None or table.empty:
            continue
        normalized = _normalize_team_table(table)
        if normalized is None:
            continue
        if base is None:
            base = normalized
        else:
            base = base.merge(normalized, on="team", how="outer")

    if base is None or base.empty:
        return pd.DataFrame()

    base["season"] = season
    base = base.dropna(how="all", subset=[col for col in base.columns if col != "team"])
    base = base.reset_index(drop=True)
    return base


def _load_table(season_dir: Path, name: str) -> Optional[pd.DataFrame]:
    path = season_dir / f"{name}.parquet"
    if not path.is_file():
        return None
    return pd.read_parquet(path)


def _normalize_team_table(frame: pd.DataFrame) -> Optional[pd.DataFrame]:
    if frame.empty:
        return None
    df = frame.copy()
    column_variants = {col: _normalize_team_column(col) for col in df.columns}
    df.columns = [column_variants.get(col, col) for col in df.columns]
    df = df.rename(columns={"Team": "team"})
    if "team" not in df.columns:
        return None
    df["team"] = df["team"].astype(str).str.replace("*", "", regex=False).str.strip()
    df = df[df["team"].str.lower() != "league average"]
    df = df[df["team"].str.lower() != "team"]
    df = df.dropna(subset=["team"]).drop_duplicates(subset=["team"], keep="first")

    value_columns = [
        column
        for column in df.columns
        if column.startswith("team_") and df[column].dtype.kind in {"f", "i", "u"}
    ]
    selected_columns = ["team"] + value_columns
    result = df.loc[:, selected_columns].copy()
    result[value_columns] = result[value_columns].apply(pd.to_numeric, errors="coerce")
    return result


def _normalize_team_column(column: str) -> str:
    mapping = {
        "Pace": "team_pace",
        "PACE": "team_pace",
        "ORtg": "team_off_rtg",
        "Off Rtg": "team_off_rtg",
        "DRtg": "team_def_rtg",
        "Def Rtg": "team_def_rtg",
        "NRtg": "team_net_rtg",
        "Net Rtg": "team_net_rtg",
        "MOV": "team_mov",
        "SOS": "team_sos",
        "SRS": "team_srs",
        "TS%": "team_ts_pct",
        "eFG%": "team_efg_pct",
        "ORB%": "team_orb_pct",
        "DRB%": "team_drb_pct",
        "TOV%": "team_tov_pct",
        "FT/FGA": "team_ft_fga",
        "3PAr": "team_3par",
        "FTr": "team_ftr",
        "AST%": "team_ast_pct",
        "STL%": "team_stl_pct",
        "BLK%": "team_blk_pct",
        "Opp eFG%": "team_opp_efg_pct",
        "Opp TOV%": "team_opp_tov_pct",
        "Opp ORB%": "team_opp_orb_pct",
        "Opp FT/FGA": "team_opp_ft_fga",
        "FT": "team_ft_per_game",
        "FTA": "team_fta_per_game",
        "3P": "team_3p_per_game",
        "3PA": "team_3pa_per_game",
        "2P": "team_2p_per_game",
        "2PA": "team_2pa_per_game",
        "ORB": "team_orb_per_game",
        "DRB": "team_drb_per_game",
        "AST": "team_ast_per_game",
        "TOV": "team_tov_per_game",
        "STL": "team_stl_per_game",
        "BLK": "team_blk_per_game",
        "PF": "team_pf_per_game",
        "PTS": "team_pts_per_game",
        "Opp PTS": "team_opp_pts_per_game",
        "Opp FG": "team_opp_fg_per_game",
        "Opp FGA": "team_opp_fga_per_game",
    }
    return mapping.get(column, column)


__all__ = [
    "DatasetNotFoundError",
    "SeasonOddsData",
    "SportsbookReviewOddsDataset",
    "load_sportsbookreview_odds",
    "load_basketball_reference_team_stats",
]
