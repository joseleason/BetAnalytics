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

calculate_team_assist_percentage <-
  function(teamAssists,
           teamFieldGoalsMade) {
    result <-
      100 * assists / teamFieldGoalsMade
  }

calculate_block_percentage <- function(blocks,
                                       opponentFieldGoalAttempt,
                                       opponentThreePointAttempt,
                                       mintesPlayed,
                                       teamMinutesPlayed) {
  result <-
    100 * (blocks * (teamMinutesPlayed / 5)) / (mintesPlayed * (opponentFieldGoalAttempt - opponentThreePointAttempt))
}

calculate_team_block_percentage <- function(teamBlocks,
                                            opponentFieldGoalAttempt,
                                            opponentThreePointAttempt) {
  result <-
    100 * (blocks / (opponentFieldGoalAttempt - opponentThreePointAttempt))
  
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

calculate_team_defensive_rebound_percentage <-
  function(teamDefensiveRebounds,
           opponentOffensiveRebounds) {
    result <-
      100 * (teamDefensiveRebounds / (teamDefensiveRebounds + opponentOffensiveRebounds))
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

calculate_team_offensive_rebound_percentage <-
  function(teamOffensiveRebounds,
           opponentDefensiveRebounds) {
    result <-
      100 * (teamOffensiveRebounds / (teamOffensiveRebounds + opponentDefensiveRebounds))
  }

calculate_total_rebound_percentage <- function(totalRebounds,
                                               teamTotalRebounds,
                                               opponentTotalRebounds,
                                               mintesPlayed,
                                               teamMinutesPlayed) {
  result <-
    100 * (totalRebounds * (teamMinutesPlayed / 5)) / (mintesPlayed * (teamTotalRebounds + opponentTotalRebounds))
}

calculate_team_total_rebound_percentage <- function(teamTotalRebounds,
                                               opponentTotalRebounds) {
  result <-
    100 * (teamTotalRebounds / (teamTotalRebounds + opponentTotalRebounds))
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

calculate_free_throw_rate <-
  function(fieldGoalAttempts,
           freeThrowAttempts) {
    result <-
      freeThrowAttempts / fieldGoalAttempts
    
  }

calculate_three_point_rate <-
  function(fieldGoalAttempts,
           threePointerAttempts) {
    result <-
      threePointerAttempts / fieldGoalAttempts
    
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

calculate_team_steal_percentage <- function(steals,
                                       opponentPossessions,
                                       mintesPlayed,
                                       teamMinutesPlayed) {
  result <-
    100 * (steals / opponentPossessions)
}

calculate_team_turnover_percentage <- function(teamTurnovers,
                                               teamFieldGoalAttempts,
                                               teamFreeThrowAttempts) {
  result <-
    100 * teamTurnovers / (teamFieldGoalAttempts + 0.44 * teamFreeThrowAttempts + teamTurnovers)
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

calculate_threshold_points <-
  function(points,
           fieldGoalAttempts,
           freeThrowAttempts,
           teamPoints,
           teamFieldGoalAttempts,
           TeamFreeThrowAttempts) {
    trueShootingAttempts <- fieldGoalAttempts + 0.44 * freeThrowAttempts
    teamTrueShootingAttempts <-
      teamFieldGoalAttempts + 0.44 * TeamFreeThrowAttempts
    
    result <-
      (points / trueShootingAttempts) - (teamPoints / teamTrueShootingAttempts - 0.33)
  }

calculate_unadjusted_offensive_role <-
  function(assists,
           thresholdPoints,
           teamAssists,
           teamThresholdPoints) {
    result <-
      6.00 - 6.642 * (assists / teamAssists) - 8.544 * (thresholdPoints / teamThresholdPoints)
  }

calculate_offensive_role <- function(unadjustedOffensiveRole, teamMinuteWeightedAverageOffensiveRole){
  result <- pmin(5, pmax(1, unadjustedOffensiveRole + (3.0 - teamMinuteWeightedAverageOffensiveRole)))
}

calculate_unadjusted_position <-
  function(totalRebounds,
           steals,
           personalFouls,
           assists,
           blocks,
           teamTotalRebounds,
           teamSteals,
           teamPersonalFouls,
           teamAssists,
           teamBlocks) {
    result <-
      2.130 + 8.668 * (totalRebounds / teamTotalRebounds) - 2.486 * (steals / teamSteals) + 0.992 * (personalFouls / teamPersonalFouls) - 3.536 * (assists / teamAssists) + 1.667 * (blocks / teamBlocks)
  }

calculate_position <- function(unadjustedPosition, teamMinuteWeightedAveragePosition){
  result <- pmin(5, pmax(1, unadjustedPosition + (3.0 - teamMinuteWeightedAveragePosition)))
}

calculate_player_scoring_possessions <-
  function(fieldGoalsMade,
           assists,
           freeThrowsMade,
           minutes,
           teamFieldGoalsMade,
           teamAssists,
           teamMinutes) {
    Q <- 5 * minutes * teamAssists / teamMinutes - assists
    R <- 5 * minutes * teamFieldGoalsMade / teamMinutes - assists
    
    result <-
      fieldGoalsMade - 0.37 * fieldGoalsMade * Q / R + 0.37 * assists + 0.5 * freeThrowsMade
  }

calculate_player_non_scoring_possessions <-
  function(fieldGoalsMade,
           fieldGoalAttempts,
           freeThrowAttempts,
           turnovers) {
    
    result <- fieldGoalAttempts - fieldGoalsMade + 0.4 * freeThrowAttempts + turnovers
  }

calculate_player_possessions <-
  function(scoringPossessions,
           nonScoringPossessions) {
    result <- scoringPossessions + nonScoringPossessions
  }

calculate_touches <-
  function(assists,
           fieldGoalAttempts,
           freeThrowAttempts,
           turnovers,
           teamFreeThrowAttempts,
           opponentPersonalFouls) {
    result <-
      fieldGoalAttempts + turnovers + (freeThrowAttempts / (teamFreeThrowAttempts / opponentPersonalFouls)) + assists / 0.17
  }

calculate_turnover_percentage <- function(turnovers, touches) {
  result <- 100 * turnovers / touches
  
}

calculate_pass_percentage <- function(assists, touches) {
  result <- 100 * (assists / 0.17) / touches
  
}

calculate_shoot_percentage <- function(fieldGoalAttempts, touches) {
  result <- 100 * fieldGoalAttempts / touches
  
}

calculate_fouled_percentage <- function(freeThrowAttempts,
                                        teamFreeThrowAttempts,
                                        opponentPersonalFouls,
                                        touches) {
  result <-
    100 * (freeThrowAttempts / (teamFreeThrowAttempts / opponentPersonalFouls)) / touches
  
}

calculate_offensive_efficiency <- function(teamPoints, possessions){
  
  results <- 100 * (teamPoints / possessions)
}