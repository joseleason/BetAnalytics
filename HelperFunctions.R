calculate_recency_stats <- function(column, gamesToSum) {
  result <-
    frollsum(column, n = gamesToSum, align = "right") %>%  shift(n = 1, type = "lag")
}

calculate_assist_percentage <-
  function(assists,
           fieldGoalsMade,
           teamFieldGoalsMade,
           mintesPlayed,
           teamMinutesPlayed) {
    result <-
      100 * assists / (((mintesPlayed /  (
        teamMinutesPlayed / 5
      )) * teamFieldGoalsMade) - fieldGoalsMade)
  }

calculate_block_percentage <- function(blocks,
                                       opponentFieldGoalAttempt,
                                       opponentThreePointAttempt,
                                       mintesPlayed,
                                       teamMinutesPlayed) {
  result <-
    100 * (blocks * (teamMinutesPlayed / 5)) / (mintesPlayed * (opponentFieldGoalAttempt - opponentThreePointAttempt))
}

calculate_defensive_rebound_percentage <-
  function(defensiveRebounds,
           teamDefensiveRebounds,
           opponentOffensiveRebounds,
           mintesPlayed,
           teamMinutesPlayed) {
    result <-
      100 * (defensiveRebounds * (teamMinutesPlayed / 5)) / (mintesPlayed * (teamDefensiveRebounds + opponentOffensiveRebounds))
  }

calculate_offensive_rebound_percentage <-
  function(offensiveRebounds,
           teamOffensiveRebounds,
           opponentDefensiveRebounds,
           mintesPlayed,
           teamMinutesPlayed) {
    result <-
      100 * (offensiveRebounds * (teamMinutesPlayed / 5)) / (mintesPlayed * (teamOffensiveRebounds + opponentDefensiveRebounds))
  }

calculate_total_rebound_percentage <- function(totalRebounds,
                                               teamTotalRebounds,
                                               opponentTotalRebounds,
                                               mintesPlayed,
                                               teamMinutesPlayed) {
  result <-
    100 * (totalRebounds * (teamMinutesPlayed / 5)) / (mintesPlayed * (teamTotalRebounds + opponentTotalRebounds))
}

calculate_effective_field_goal_percentage <-
  function(fieldGoalsMade,
           threePointMade,
           fieldGoalAttempts) {
    result <-
      (fieldGoalsMade + 0.5 * threePointMade) / fieldGoalAttempts
    
  }

calculate_true_shooting_percentage <-
  function(points,
           fieldGoalAttempts,
           freeThrowAttempts) {
    result <-
      points / (2 * (fieldGoalAttempts + 0.4 * freeThrowAttempts))
    
  }

calculate_usage_percentage <-
  function(fieldGoalAttempts,
           freeThrowAttempts,
           turnovers,
           teamFieldGoalAttempts,
           teamFreeThrowAttempts,
           teamTurnovers,
           mintesPlayed,
           teamMinutesPlayed) {
    result <-
      100 * ((fieldGoalAttempts + 0.44 * freeThrowAttempts + turnovers) * (teamMinutesPlayed / 5)) / (
        mintesPlayed * (
          teamFieldGoalAttempts + 0.44 * teamFreeThrowAttempts + teamTurnovers
        )
      )
  }

calculate_steal_percentage <- function(steals,
                                       opponentPossessions,
                                       mintesPlayed,
                                       teamMinutesPlayed) {
  result <-
    100 * (steals * (teamMinutesPlayed / 5)) / (mintesPlayed * opponentPossessions)
}

calculate_turnover_percentage <- function(turnovers,
                                          fieldGoalAttempts,
                                          freeThrowAttempts) {
  result <-
    100 * turnovers / (fieldGoalAttempts + 0.44 * freeThrowAttempts + turnovers)
}

calculate_possessions <-
  function(teamFieldGoalsMade,
           teamFieldGoalAttempts,
           teamFreeThrowAttempts,
           teamOffensiveRebounds,
           teamDefensiveRebounds,
           teamTurnovers,
           opponentFieldGoalsMade,
           opponentFieldGoalAttempts,
           opponentFreeThrowAttempts,
           opponentOffensiveRebounds,
           opponentDefensiveRebounds,
           opponentTurnovers) {
    result <-
      0.5 * ((
        teamFieldGoalAttempts + 0.4 * teamFreeThrowAttempts - 1.07 * (
          teamOffensiveRebounds / (teamOffensiveRebounds + opponentDefensiveRebounds)
        ) * (teamFieldGoalAttempts - teamFieldGoalsMade) + teamTurnovers
      ) + (
        opponentFieldGoalAttempts + 0.4 * opponentFreeThrowAttempts - 1.07 * (
          opponentOffensiveRebounds / (opponentOffensiveRebounds + teamDefensiveRebounds)
        ) * (opponentFieldGoalAttempts - opponentFieldGoalsMade) + opponentTurnovers
      )
      )
  }