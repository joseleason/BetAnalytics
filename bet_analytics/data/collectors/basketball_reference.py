"""Basketball Reference data collector with polite scraping defaults."""

from __future__ import annotations

import json
import logging
import time
from io import StringIO
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Dict, Iterable, List, Optional
from urllib.parse import urljoin
from urllib.robotparser import RobotFileParser

import pandas as pd
import requests
from bs4 import BeautifulSoup, Comment
from pandas import DataFrame
from requests import Response
from requests.exceptions import HTTPError, RequestException

logger = logging.getLogger(__name__)


class TableNotFoundError(RuntimeError):
    """Raised when an expected table is not present in a fetched HTML page."""


@dataclass
class TableResult:
    """Container for a collected table."""

    name: str
    dataframe: DataFrame
    source_url: str


class RateLimiter:
    """Simple rate limiter ensuring a minimum delay between consecutive calls."""

    def __init__(self, min_interval_seconds: float = 3.0) -> None:
        if min_interval_seconds < 0:
            msg = "Rate limiter interval must be non-negative"
            raise ValueError(msg)
        self.min_interval_seconds = float(min_interval_seconds)
        self._last_call: Optional[float] = None

    def wait(self) -> None:
        """Sleep if the previous call occurred too recently."""

        if self._last_call is None:
            self._last_call = time.monotonic()
            return

        elapsed = time.monotonic() - self._last_call
        remaining = self.min_interval_seconds - elapsed
        if remaining > 0:
            time.sleep(remaining)
        self._last_call = time.monotonic()


class BasketballReferenceCollector:
    """Collector responsible for retrieving datasets from Basketball Reference."""

    BASE_URL = "https://www.basketball-reference.com"
    DEFAULT_MONTHS: List[str] = [
        "october",
        "november",
        "december",
        "january",
        "february",
        "march",
        "april",
        "may",
        "june",
        "july",
        "august",
        "september",
    ]

    TEAM_TABLES: Dict[str, str] = {
        "team_per_game": "team-stats-per_game",
        "opponent_per_game": "opponent-stats-per_game",
        "team_totals": "team-stats-base",
        "opponent_totals": "opponent-stats-base",
        "team_misc": "team-stats-misc",
        "opponent_misc": "opponent-stats-misc",
        "team_advanced": "team-stats-advanced",
    }

    PLAYER_TABLE_PATHS: Dict[str, tuple[str, str]] = {
        "player_totals": ("/leagues/NBA_{season}_totals.html", "totals_stats"),
        "player_per_game": ("/leagues/NBA_{season}_per_game.html", "per_game_stats"),
        "player_advanced": ("/leagues/NBA_{season}_advanced.html", "advanced_stats"),
        "player_shooting": ("/leagues/NBA_{season}_shooting.html", "shooting_stats"),
    }

    def __init__(
        self,
        *,
        rate_limit_seconds: float = 3.0,
        user_agent: str | None = None,
        contact_email: str | None = None,
        respect_robots: bool = True,
        session: Optional[requests.Session] = None,
    ) -> None:
        self.session = session or requests.Session()
        self.rate_limiter = RateLimiter(rate_limit_seconds)
        self.user_agent = user_agent or (
            "Mozilla/5.0 (compatible; BetAnalyticsCollector/0.1; +https://github.com/bet-analytics)"
        )
        headers = {"User-Agent": self.user_agent}
        if contact_email:
            headers["From"] = contact_email
        self.session.headers.update(headers)
        self.respect_robots = respect_robots
        self._robots_parser: Optional[RobotFileParser] = None
        if respect_robots:
            self._robots_parser = self._load_robots()

    # ---------------------------------------------------------------------
    # Public API
    # ---------------------------------------------------------------------
    def collect_season(self, season: int) -> List[TableResult]:
        """Collect a bundle of game, team, and player tables for a season."""

        logger.info("Collecting Basketball Reference data", extra={"season": season})
        tables: List[TableResult] = []
        tables.extend(self._collect_games(season))
        tables.extend(self._collect_team_tables(season))
        tables.extend(self._collect_player_tables(season))
        return tables

    # ------------------------------------------------------------------
    # HTTP helpers
    # ------------------------------------------------------------------
    def _load_robots(self) -> Optional[RobotFileParser]:
        """Retrieve and parse robots.txt, returning a configured parser."""

        robots_url = urljoin(self.BASE_URL, "/robots.txt")
        parser = RobotFileParser()
        try:
            self.rate_limiter.wait()
            response = self.session.get(robots_url, timeout=30)
            response.raise_for_status()
        except RequestException as exc:  # pragma: no cover - network side effect
            logger.warning("Unable to load robots.txt", extra={"error": str(exc)})
            return None
        parser.parse(response.text.splitlines())
        return parser

    def _get(self, path: str) -> Response:
        """Issue a GET request honoring robots.txt and the configured rate limit."""

        url = urljoin(self.BASE_URL, path)
        if self.respect_robots and self._robots_parser is not None:
            can_fetch = self._robots_parser.can_fetch(self.user_agent, url)
            if not can_fetch:
                msg = f"robots.txt prevents fetching {url}"
                raise PermissionError(msg)
        self.rate_limiter.wait()
        response = self.session.get(url, timeout=30)
        response.raise_for_status()
        return response

    # ------------------------------------------------------------------
    # Collection helpers
    # ------------------------------------------------------------------
    def _collect_games(self, season: int) -> List[TableResult]:
        """Collect regular season and playoff game results tables."""

        results: List[TableResult] = []
        for month in self.DEFAULT_MONTHS:
            path = f"/leagues/NBA_{season}_games-{month}.html"
            try:
                response = self._get(path)
            except HTTPError as exc:
                if exc.response is not None and exc.response.status_code == 404:
                    continue
                raise
            except PermissionError:
                logger.warning("Skipping month disallowed by robots.txt", extra={"month": month})
                continue

            try:
                table = _extract_table(response.text, table_id="schedule")
            except TableNotFoundError:
                logger.warning(
                    "Schedule table missing from month page", extra={"season": season, "month": month}
                )
                continue

            table = _clean_dataframe(table)
            if table.empty:
                continue
            table["season"] = season
            table["month"] = month
            results.append(TableResult(name=f"games_{month}", dataframe=table, source_url=urljoin(self.BASE_URL, path)))

        if not results:
            logger.warning("No monthly game tables collected", extra={"season": season})
        return results

    def _collect_team_tables(self, season: int) -> List[TableResult]:
        """Collect team-level tables from the season summary page."""

        path = f"/leagues/NBA_{season}.html"
        try:
            response = self._get(path)
        except PermissionError:
            logger.warning("Skipping team tables due to robots.txt", extra={"season": season})
            return []

        html = response.text
        tables: List[TableResult] = []
        for name, table_id in self.TEAM_TABLES.items():
            try:
                df = _extract_table(html, table_id=table_id)
            except TableNotFoundError:
                logger.warning(
                    "Team table not found",
                    extra={"season": season, "table_id": table_id, "table_name": name},
                )
                continue
            df = _clean_dataframe(df)
            if df.empty:
                continue
            df["season"] = season
            tables.append(TableResult(name=name, dataframe=df, source_url=urljoin(self.BASE_URL, path)))
        return tables

    def _collect_player_tables(self, season: int) -> List[TableResult]:
        """Collect player-level season tables."""

        tables: List[TableResult] = []
        for name, (path_template, table_id) in self.PLAYER_TABLE_PATHS.items():
            path = path_template.format(season=season)
            try:
                response = self._get(path)
            except HTTPError as exc:
                if exc.response is not None and exc.response.status_code == 404:
                    logger.info(
                        "Player table missing for season", extra={"season": season, "path": path}
                    )
                    continue
                raise
            except PermissionError:
                logger.warning("Skipping player table due to robots.txt", extra={"table": name})
                continue

            try:
                df = _extract_table(response.text, table_id=table_id)
            except TableNotFoundError:
                logger.warning(
                    "Player table not found",
                    extra={"season": season, "table_id": table_id, "table_name": name},
                )
                continue
            df = _clean_dataframe(df)
            if df.empty:
                continue
            df["season"] = season
            tables.append(TableResult(name=name, dataframe=df, source_url=urljoin(self.BASE_URL, path)))
        return tables


@dataclass
class SeasonDataWriter:
    """Persist season datasets to parquet along with collection metadata."""

    output_root: Path

    def write(self, season: int, tables: Iterable[TableResult]) -> None:
        season_dir = self.output_root / f"season_{season}"
        season_dir.mkdir(parents=True, exist_ok=True)
        manifest: Dict[str, Dict[str, object]] = {}

        for table in tables:
            file_path = season_dir / f"{table.name}.parquet"
            table.dataframe.to_parquet(file_path, index=False)
            manifest[table.name] = {
                "source_url": table.source_url,
                "rows": int(table.dataframe.shape[0]),
                "columns": list(map(str, table.dataframe.columns)),
            }

        metadata_path = season_dir / "metadata.json"
        metadata = {
            "season": season,
            "generated_at_utc": datetime.now(UTC).isoformat(timespec="seconds"),
            "tables": manifest,
        }
        metadata_path.write_text(json.dumps(metadata, indent=2))
        logger.info("Wrote season data", extra={"season": season, "path": str(season_dir)})


# ---------------------------------------------------------------------------
# HTML parsing helpers
# ---------------------------------------------------------------------------

def _extract_table(html: str, *, table_id: str) -> DataFrame:
    """Extract a table by id, accounting for tables wrapped in HTML comments."""

    soup = BeautifulSoup(html, "lxml")
    table = soup.find("table", id=table_id)
    if table is None:
        for comment in soup.find_all(string=lambda text: isinstance(text, Comment)):
            comment_soup = BeautifulSoup(comment, "lxml")
            table = comment_soup.find("table", id=table_id)
            if table is not None:
                break
    if table is None:
        raise TableNotFoundError(f"table '{table_id}' not found")
    frames = pd.read_html(StringIO(str(table)))
    if not frames:
        raise TableNotFoundError(f"table '{table_id}' has no readable content")
    return frames[0]


def _clean_dataframe(frame: DataFrame) -> DataFrame:
    """Remove duplicate header rows and ensure column uniqueness."""

    df = frame.copy()
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [
            "_".join(filter(None, (str(level).strip() for level in levels)))
            for levels in df.columns.values
        ]
    df.columns = [str(col).strip() for col in df.columns]
    df = df.loc[:, ~df.columns.duplicated()]

    def _is_repeated_header(row: pd.Series) -> bool:
        return all(str(value).strip() == str(col) for value, col in zip(row, df.columns))

    mask = df.apply(_is_repeated_header, axis=1)
    df = df.loc[~mask]
    df = df.dropna(how="all")
    df = df.reset_index(drop=True)
    return df


__all__ = ["BasketballReferenceCollector", "SeasonDataWriter", "TableResult"]
