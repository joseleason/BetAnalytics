"""Collector for Sportsbook Review NBA odds tables."""

from __future__ import annotations

import json
import logging
import time
from dataclasses import dataclass, field
from datetime import UTC, date, datetime, timedelta
from pathlib import Path
from statistics import median
from typing import Dict, Iterable, List, Mapping, MutableMapping, Optional, Sequence

import pandas as pd
import requests
from bs4 import BeautifulSoup
from pandas import DataFrame
from requests import Response
from requests.exceptions import HTTPError, RequestException

logger = logging.getLogger(__name__)


class RateLimiter:
    """Simple rate limiter ensuring a minimum delay between consecutive calls."""

    def __init__(self, min_interval_seconds: float = 1.0) -> None:
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


@dataclass(frozen=True)
class GameOddsRecord:
    """Container for a single game's odds snapshot."""

    game_id: str
    season: int
    game_date: date
    game_city: Optional[str]
    game_state: Optional[str]
    game_country: Optional[str]
    away_team: Optional[str]
    away_team_abbr: Optional[str]
    away_team_score: Optional[int]
    home_team: Optional[str]
    home_team_abbr: Optional[str]
    home_team_score: Optional[int]
    open_home_spread: Optional[float]
    open_home_spread_price: Optional[float]
    open_away_spread_price: Optional[float]
    close_home_spread: Optional[float]
    close_home_spread_price: Optional[float]
    close_away_spread_price: Optional[float]
    open_home_moneyline: Optional[float]
    open_away_moneyline: Optional[float]
    close_home_moneyline: Optional[float]
    close_away_moneyline: Optional[float]
    open_total: Optional[float]
    open_over_price: Optional[float]
    open_under_price: Optional[float]
    close_total: Optional[float]
    close_over_price: Optional[float]
    close_under_price: Optional[float]
    spread_source_url: Optional[str]
    moneyline_source_url: Optional[str]
    totals_source_url: Optional[str]
    collected_at_utc: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> Dict[str, object]:
        """Convert the record into a dictionary suitable for DataFrame construction."""

        return {
            "game_id": self.game_id,
            "season": self.season,
            "game_date": self.game_date,
            "game_city": self.game_city,
            "game_state": self.game_state,
            "game_country": self.game_country,
            "away_team": self.away_team,
            "away_team_abbr": self.away_team_abbr,
            "away_team_score": self.away_team_score,
            "home_team": self.home_team,
            "home_team_abbr": self.home_team_abbr,
            "home_team_score": self.home_team_score,
            "open_home_spread": self.open_home_spread,
            "open_home_spread_price": self.open_home_spread_price,
            "open_away_spread_price": self.open_away_spread_price,
            "close_home_spread": self.close_home_spread,
            "close_home_spread_price": self.close_home_spread_price,
            "close_away_spread_price": self.close_away_spread_price,
            "open_home_moneyline": self.open_home_moneyline,
            "open_away_moneyline": self.open_away_moneyline,
            "close_home_moneyline": self.close_home_moneyline,
            "close_away_moneyline": self.close_away_moneyline,
            "open_total": self.open_total,
            "open_over_price": self.open_over_price,
            "open_under_price": self.open_under_price,
            "close_total": self.close_total,
            "close_over_price": self.close_over_price,
            "close_under_price": self.close_under_price,
            "spread_source_url": self.spread_source_url,
            "moneyline_source_url": self.moneyline_source_url,
            "totals_source_url": self.totals_source_url,
            "collected_at_utc": self.collected_at_utc,
        }


class SportsbookReviewCollector:
    """Collector responsible for retrieving NBA odds from Sportsbook Review."""

    BASE_URLS: Mapping[str, str] = {
        "spread": "https://www.sportsbookreview.com/betting-odds/nba-basketball/?date=",
        "moneyline": "https://www.sportsbookreview.com/betting-odds/nba-basketball/money-line/full-game/?date=",
        "totals": "https://www.sportsbookreview.com/betting-odds/nba-basketball/totals/full-game/?date=",
    }

    def __init__(
        self,
        *,
        rate_limit_seconds: float = 1.5,
        user_agent: Optional[str] = None,
        allowed_sportsbooks: Optional[Sequence[str]] = None,
        session: Optional[requests.Session] = None,
        request_timeout: float = 30.0,
        max_attempts: int = 3,
        retry_backoff_seconds: float = 1.5,
    ) -> None:
        self.session = session or requests.Session()
        self.rate_limiter = RateLimiter(rate_limit_seconds)
        self.request_timeout = float(request_timeout)
        ua = user_agent or "Mozilla/5.0 (compatible; BetAnalyticsCollector/0.1; +https://github.com/bet-analytics)"
        self.session.headers.update({"User-Agent": ua})
        if allowed_sportsbooks:
            self.allowed_sportsbooks = {name.lower() for name in allowed_sportsbooks}
        else:
            self.allowed_sportsbooks = None
        if max_attempts < 1:
            msg = "max_attempts must be at least 1"
            raise ValueError(msg)
        if retry_backoff_seconds < 0:
            msg = "retry_backoff_seconds must be non-negative"
            raise ValueError(msg)
        self.max_attempts = int(max_attempts)
        self.retry_backoff_seconds = float(retry_backoff_seconds)

    def collect_range(self, start_date: date, end_date: date) -> List[GameOddsRecord]:
        """Collect odds for each date in the inclusive range."""

        if end_date < start_date:
            msg = "end_date must not be earlier than start_date"
            raise ValueError(msg)

        current = start_date
        results: List[GameOddsRecord] = []
        while current <= end_date:
            logger.info("Collecting Sportsbook Review odds", extra={"date": current.isoformat()})
            records = self.collect_day(current)
            results.extend(records)
            current = current + timedelta(days=1)
        return results

    def collect_day(self, target_date: date) -> List[GameOddsRecord]:
        """Collect all available games for a single calendar date."""

        formatted_date = target_date.strftime("%Y/%m/%d")
        collected_at = datetime.now(UTC)

        markets: Dict[str, Dict[str, MutableMapping[str, object]]] = {}
        for market, base_url in self.BASE_URLS.items():
            markets[market] = self._fetch_market_data(market, base_url, formatted_date)

        game_ids = set().union(*(market_data.keys() for market_data in markets.values()))
        if not game_ids:
            logger.warning("No games returned for date", extra={"date": target_date.isoformat()})
            return []

        records: List[GameOddsRecord] = []
        for game_id in sorted(game_ids):
            spread_entry = markets["spread"].get(game_id)
            moneyline_entry = markets["moneyline"].get(game_id)
            totals_entry = markets["totals"].get(game_id)

            game_view = None
            for entry in (spread_entry, moneyline_entry, totals_entry):
                if entry and entry.get("gameView"):
                    game_view = entry["gameView"]
                    break
            if not game_view:
                logger.debug(
                    "Skipping game without metadata",
                    extra={"date": target_date.isoformat(), "game_id": game_id},
                )
                continue

            game_date = _parse_game_date(game_view.get("startDate")) or target_date
            season = _season_for_date(game_date)

            record = GameOddsRecord(
                game_id=str(game_id),
                season=season,
                game_date=game_date,
                game_city=game_view.get("city"),
                game_state=game_view.get("state"),
                game_country=game_view.get("country"),
                away_team=_safe_str(game_view.get("awayTeam"), "fullName"),
                away_team_abbr=_safe_str(game_view.get("awayTeam"), "shortName"),
                away_team_score=_coerce_int(game_view.get("awayTeamScore")),
                home_team=_safe_str(game_view.get("homeTeam"), "fullName"),
                home_team_abbr=_safe_str(game_view.get("homeTeam"), "shortName"),
                home_team_score=_coerce_int(game_view.get("homeTeamScore")),
                open_home_spread=self._median_value(spread_entry, "openingLine", "homeSpread"),
                open_home_spread_price=self._median_value(spread_entry, "openingLine", "homeOdds"),
                open_away_spread_price=self._median_value(spread_entry, "openingLine", "awayOdds"),
                close_home_spread=self._median_value(spread_entry, "currentLine", "homeSpread"),
                close_home_spread_price=self._median_value(spread_entry, "currentLine", "homeOdds"),
                close_away_spread_price=self._median_value(spread_entry, "currentLine", "awayOdds"),
                open_home_moneyline=self._median_value(moneyline_entry, "openingLine", "homeOdds"),
                open_away_moneyline=self._median_value(moneyline_entry, "openingLine", "awayOdds"),
                close_home_moneyline=self._median_value(moneyline_entry, "currentLine", "homeOdds"),
                close_away_moneyline=self._median_value(moneyline_entry, "currentLine", "awayOdds"),
                open_total=self._median_value(totals_entry, "openingLine", "total"),
                open_over_price=self._median_value(totals_entry, "openingLine", "overOdds"),
                open_under_price=self._median_value(totals_entry, "openingLine", "underOdds"),
                close_total=self._median_value(totals_entry, "currentLine", "total"),
                close_over_price=self._median_value(totals_entry, "currentLine", "overOdds"),
                close_under_price=self._median_value(totals_entry, "currentLine", "underOdds"),
                spread_source_url=_market_url(self.BASE_URLS["spread"], formatted_date)
                if spread_entry
                else None,
                moneyline_source_url=_market_url(self.BASE_URLS["moneyline"], formatted_date)
                if moneyline_entry
                else None,
                totals_source_url=_market_url(self.BASE_URLS["totals"], formatted_date)
                if totals_entry
                else None,
                collected_at_utc=collected_at,
            )
            records.append(record)
        return records

    # ------------------------------------------------------------------
    # HTTP and parsing helpers
    # ------------------------------------------------------------------
    def _fetch_market_data(
        self, market: str, base_url: str, formatted_date: str
    ) -> Dict[str, MutableMapping[str, object]]:
        url = _market_url(base_url, formatted_date)
        try:
            response = self._request_with_retries(url, market)
        except HTTPError as exc:
            status = exc.response.status_code if exc.response is not None else None
            if status and 500 <= status < 600:
                logger.warning(
                    "Skipping market due to upstream error",
                    extra={
                        "market": market,
                        "url": url,
                        "status": status,
                        "attempts": self.max_attempts,
                    },
                )
                return {}
            raise
        except RequestException as exc:
            logger.warning(
                "Skipping market due to request failure",
                extra={
                    "market": market,
                    "url": url,
                    "attempts": self.max_attempts,
                    "error": str(exc),
                },
            )
            return {}

        return self._parse_market_payload(response, market, url)

    def _request_with_retries(self, url: str, market: str) -> Response:
        attempt = 0
        while True:
            attempt += 1
            self.rate_limiter.wait()
            try:
                response = self.session.get(url, timeout=self.request_timeout)
                response.raise_for_status()
                return response
            except HTTPError as exc:
                status = exc.response.status_code if exc.response is not None else None
                retriable = status is not None and 500 <= status < 600
                if not retriable or attempt >= self.max_attempts:
                    raise
                logger.warning(
                    "Server error when fetching odds, retrying",
                    extra={
                        "market": market,
                        "url": url,
                        "attempt": attempt,
                        "max_attempts": self.max_attempts,
                        "status": status,
                    },
                )
            except RequestException as exc:
                if attempt >= self.max_attempts:
                    raise
                logger.warning(
                    "Request error when fetching odds, retrying",
                    extra={
                        "market": market,
                        "url": url,
                        "attempt": attempt,
                        "max_attempts": self.max_attempts,
                        "error": str(exc),
                    },
                )

            if attempt < self.max_attempts:
                self._sleep_before_retry(attempt)

    def _sleep_before_retry(self, attempt: int) -> None:
        delay = min(self.retry_backoff_seconds * attempt, self.request_timeout)
        if delay > 0:
            time.sleep(delay)

    def _parse_market_payload(
        self, response: Response, market: str, url: str
    ) -> Dict[str, MutableMapping[str, object]]:
        soup = BeautifulSoup(response.text, "html.parser")
        script_tag = soup.find("script", id="__NEXT_DATA__")
        if not script_tag or not script_tag.string:
            logger.warning("Missing odds payload", extra={"market": market, "url": url})
            return {}
        try:
            data = json.loads(script_tag.string)
        except json.JSONDecodeError:
            logger.exception("Unable to decode odds payload", extra={"market": market, "url": url})
            return {}

        try:
            odds_tables = data["props"]["pageProps"]["oddsTables"]
        except (KeyError, TypeError):
            logger.warning("Unexpected odds payload structure", extra={"market": market, "url": url})
            return {}

        if not isinstance(odds_tables, list):
            logger.warning("Unexpected odds payload structure", extra={"market": market, "url": url})
            return {}

        if not odds_tables:
            logger.debug("No odds tables returned", extra={"market": market, "url": url})
            return {}

        try:
            table = odds_tables[0]["oddsTableModel"]["gameRows"]
        except (KeyError, IndexError, TypeError):
            logger.warning("Unexpected odds payload structure", extra={"market": market, "url": url})
            return {}

        results: Dict[str, MutableMapping[str, object]] = {}
        for row in table:
            game_view = row.get("gameView")
            if not game_view:
                continue
            game_id = game_view.get("gameId")
            if not game_id:
                continue
            results[str(game_id)] = row
        return results

    def _median_value(
        self,
        entry: Optional[Mapping[str, object]],
        line_name: str,
        value_key: str,
    ) -> Optional[float]:
        if not entry:
            return None
        odds_views = entry.get("oddsViews")
        if not isinstance(odds_views, list):
            return None

        values: List[float] = []
        for view in odds_views:
            if not isinstance(view, Mapping):
                continue
            if self.allowed_sportsbooks:
                sportsbook_info = view.get("sportsBook") or {}
                sportsbook_name = None
                if isinstance(sportsbook_info, Mapping):
                    raw_name = sportsbook_info.get("shortName") or sportsbook_info.get("name")
                    if raw_name is not None:
                        sportsbook_name = str(raw_name).lower()
                if not sportsbook_name or sportsbook_name not in self.allowed_sportsbooks:
                    continue
            line = view.get(line_name)
            if not isinstance(line, Mapping):
                continue
            value = line.get(value_key)
            number = _coerce_float(value)
            if number is None:
                continue
            values.append(number)
        if not values:
            return None
        return float(median(values))


@dataclass
class SportsbookReviewOddsWriter:
    """Persist collected odds into season-partitioned parquet datasets."""

    output_root: Path

    def write(self, records: Iterable[GameOddsRecord], *, overwrite: bool = False) -> None:
        frame = _records_to_dataframe(records)
        if frame.empty:
            logger.warning("No odds records to write")
            return

        frame = _normalize_odds_frame(frame)
        for season, season_frame in frame.groupby("season"):
            season_dir = self.output_root / f"season_{season}"
            season_dir.mkdir(parents=True, exist_ok=True)
            data_path = season_dir / "odds.parquet"

            if data_path.exists() and not overwrite:
                existing = pd.read_parquet(data_path)
                combined = pd.concat([existing, season_frame], ignore_index=True)
            else:
                combined = season_frame.copy()

            combined = _consolidate_odds_frame(combined)
            combined.to_parquet(data_path, index=False)
            metadata_path = season_dir / "metadata.json"
            table_manifest = _build_odds_manifest(combined)
            metadata = _build_metadata(season, table_manifest)
            metadata_path.write_text(json.dumps(metadata, indent=2))
            logger.info(
                "Wrote Sportsbook Review odds",
                extra={
                    "season": season,
                    "path": str(data_path),
                    "records": int(table_manifest["rows"]),
                },
            )


def _records_to_dataframe(records: Iterable[GameOddsRecord]) -> DataFrame:
    rows = [record.to_dict() for record in records]
    if not rows:
        return pd.DataFrame()
    return pd.DataFrame(rows)


def _normalize_odds_frame(frame: DataFrame) -> DataFrame:
    normalized = frame.copy()
    if "collected_at_utc" in normalized.columns:
        normalized["collected_at_utc"] = pd.to_datetime(
            normalized["collected_at_utc"], utc=True, errors="coerce"
        )
        sort_columns = ["game_date", "game_id", "collected_at_utc"]
    else:
        sort_columns = ["game_date", "game_id"]
    normalized = normalized.sort_values(sort_columns).reset_index(drop=True)
    return normalized


def _consolidate_odds_frame(frame: DataFrame) -> DataFrame:
    consolidated = _normalize_odds_frame(frame)
    if {"game_id", "game_date"}.issubset(consolidated.columns):
        consolidated = consolidated.drop_duplicates(
            subset=["game_id", "game_date"], keep="last"
        )
        consolidated = consolidated.reset_index(drop=True)
    return consolidated


def _build_odds_manifest(frame: DataFrame) -> Dict[str, object]:
    min_date = frame["game_date"].min() if "game_date" in frame else None
    max_date = frame["game_date"].max() if "game_date" in frame else None
    min_iso = pd.Timestamp(min_date).date().isoformat() if pd.notna(min_date) else None
    max_iso = pd.Timestamp(max_date).date().isoformat() if pd.notna(max_date) else None
    source_columns = ["spread_source_url", "moneyline_source_url", "totals_source_url"]
    sources = {
        str(url)
        for column in source_columns
        if column in frame
        for url in frame[column].dropna().unique().tolist()
    }
    manifest = {
        "rows": int(frame.shape[0]),
        "columns": list(map(str, frame.columns)),
        "date_range": {"min": min_iso, "max": max_iso},
        "source_urls": sorted(sources),
    }
    return manifest


def _build_metadata(season: int, table_manifest: Mapping[str, object]) -> Dict[str, object]:
    metadata = {
        "season": season,
        "generated_at_utc": datetime.now(UTC).isoformat(timespec="seconds"),
        "tables": {"game_odds": dict(table_manifest)},
    }
    return metadata


def _market_url(base_url: str, formatted_date: str) -> str:
    return f"{base_url}{formatted_date}"


def _parse_game_date(value: Optional[str]) -> Optional[date]:
    if not value:
        return None
    try:
        cleaned = value.replace("Z", "+00:00")
        parsed = datetime.fromisoformat(cleaned)
    except ValueError:
        logger.debug("Failed to parse game date", extra={"value": value})
        return None
    return parsed.date()


def _season_for_date(game_date: date) -> int:
    return game_date.year + 1 if game_date.month >= 7 else game_date.year


def _safe_str(container: Optional[Mapping[str, object]], key: str) -> Optional[str]:
    if not container:
        return None
    value = container.get(key)
    return str(value) if value is not None else None


def _coerce_int(value: object) -> Optional[int]:
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def _coerce_float(value: object) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


__all__ = [
    "GameOddsRecord",
    "SportsbookReviewCollector",
    "SportsbookReviewOddsWriter",
]
