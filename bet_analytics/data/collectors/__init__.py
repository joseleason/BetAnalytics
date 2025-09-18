"""Collectors responsible for retrieving external datasets."""

from __future__ import annotations

from .basketball_reference import BasketballReferenceCollector, SeasonDataWriter
from .sportsbookreview import (
    GameOddsRecord,
    SportsbookReviewCollector,
    SportsbookReviewOddsWriter,
)

__all__ = [
    "BasketballReferenceCollector",
    "SeasonDataWriter",
    "GameOddsRecord",
    "SportsbookReviewCollector",
    "SportsbookReviewOddsWriter",
]
