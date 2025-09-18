#!/usr/bin/env python3
"""CLI for collecting Sportsbook Review NBA odds datasets."""

from __future__ import annotations

import argparse
import logging
import sys
from datetime import UTC, date, datetime
from pathlib import Path
from typing import Iterable

if __package__ is None or __package__ == "":  # pragma: no cover - runtime import adjustment
    repo_root = Path(__file__).resolve().parents[1]
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))

from bet_analytics.data import SportsbookReviewCollector, SportsbookReviewOddsWriter

logger = logging.getLogger(__name__)


def _parse_date(value: str) -> date:
    try:
        return datetime.fromisoformat(value).date()
    except ValueError as exc:  # pragma: no cover - argparse handles error presentation
        msg = f"Invalid date '{value}'. Expected ISO format YYYY-MM-DD."
        raise argparse.ArgumentTypeError(msg) from exc


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Collect NBA betting odds from Sportsbook Review and persist them as parquet datasets.",
    )
    parser.add_argument(
        "--start-date",
        type=_parse_date,
        default=None,
        help="First calendar date to collect (inclusive). Defaults to today in UTC.",
    )
    parser.add_argument(
        "--end-date",
        type=_parse_date,
        default=None,
        help="Last calendar date to collect (inclusive). Defaults to the start date.",
    )
    parser.add_argument(
        "--sportsbooks",
        type=str,
        nargs="+",
        default=None,
        help="Optional list of sportsbook short names to include when computing medians.",
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("data/raw/sportsbookreview"),
        help="Directory where parquet datasets will be written.",
    )
    parser.add_argument(
        "--rate-limit",
        type=float,
        default=1.5,
        help="Minimum delay in seconds between HTTP requests. Defaults to 1.5 seconds.",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="HTTP request timeout in seconds.",
    )
    parser.add_argument(
        "--user-agent",
        type=str,
        default=None,
        help="Custom user-agent to include with HTTP requests.",
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite existing parquet outputs instead of merging with them.",
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


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    configure_logging(args.log_level)

    today = datetime.now(UTC).date()
    start_date = args.start_date or today
    end_date = args.end_date or start_date
    if end_date < start_date:
        logger.error(
            "End date must not be earlier than start date",
            extra={"start_date": start_date.isoformat(), "end_date": end_date.isoformat()},
        )
        return 1

    collector = SportsbookReviewCollector(
        rate_limit_seconds=args.rate_limit,
        user_agent=args.user_agent,
        allowed_sportsbooks=args.sportsbooks,
        request_timeout=args.timeout,
    )
    writer = SportsbookReviewOddsWriter(output_root=args.output_dir)

    try:
        records = collector.collect_range(start_date, end_date)
    except Exception as exc:  # pragma: no cover - network heavy
        logger.exception(
            "Failed to collect Sportsbook Review odds",
            extra={
                "start_date": start_date.isoformat(),
                "end_date": end_date.isoformat(),
            },
        )
        return 1

    if not records:
        logger.warning(
            "No Sportsbook Review odds collected",
            extra={
                "start_date": start_date.isoformat(),
                "end_date": end_date.isoformat(),
            },
        )
        return 0

    writer.write(records, overwrite=args.overwrite)
    return 0


if __name__ == "__main__":
    sys.exit(main())
