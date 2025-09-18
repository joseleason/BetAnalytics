#!/usr/bin/env python3
"""CLI for collecting Basketball Reference datasets."""

from __future__ import annotations

import argparse
import logging
import sys
from datetime import UTC, datetime
from pathlib import Path
from typing import Iterable, List

from bet_analytics.data import BasketballReferenceCollector, SeasonDataWriter

logger = logging.getLogger(__name__)


def _default_seasons(window: int = 5) -> List[int]:
    now = datetime.now(UTC)
    latest = now.year if now.month >= 7 else now.year - 1
    seasons = [latest - offset for offset in range(window)]
    seasons.sort()
    return seasons


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Collect NBA statistics from Basketball Reference with polite defaults."
    )
    parser.add_argument(
        "--seasons",
        type=int,
        nargs="+",
        default=_default_seasons(),
        help="Season end years to collect (e.g. 2024 for the 2023-24 season). Default: last five seasons.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("data/raw/basketball_reference"),
        help="Directory where parquet datasets will be written.",
    )
    parser.add_argument(
        "--rate-limit",
        type=float,
        default=3.0,
        help="Minimum delay in seconds between requests. Defaults to 3 seconds per robots.txt guidance.",
    )
    parser.add_argument(
        "--contact-email",
        type=str,
        default=None,
        help="Contact email included in the From header for transparency with the data provider.",
    )
    parser.add_argument(
        "--ignore-robots",
        action="store_true",
        help="Disable robots.txt checks. Use with caution and only if permitted by the data provider.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite existing parquet outputs for a season. Defaults to skipping existing data.",
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Logging verbosity.",
    )
    return parser.parse_args(argv)


def configure_logging(level: str) -> None:
    logging.basicConfig(
        level=getattr(logging, level),
        format="%(asctime)s %(levelname)s [%(name)s] %(message)s",
    )


def season_already_materialized(output_dir: Path, season: int) -> bool:
    season_dir = output_dir / f"season_{season}"
    return season_dir.exists() and any(season_dir.glob("*.parquet"))


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    configure_logging(args.log_level)

    collector = BasketballReferenceCollector(
        rate_limit_seconds=args.rate_limit,
        contact_email=args.contact_email,
        respect_robots=not args.ignore_robots,
    )
    writer = SeasonDataWriter(output_root=args.output_dir)

    for season in args.seasons:
        if not args.overwrite and season_already_materialized(args.output_dir, season):
            logger.info("Skipping season with existing outputs", extra={"season": season})
            continue
        try:
            tables = collector.collect_season(season)
        except Exception as exc:  # pragma: no cover - network heavy
            logger.exception("Failed to collect season", extra={"season": season})
            return 1
        writer.write(season, tables)
    return 0


if __name__ == "__main__":
    sys.exit(main())
