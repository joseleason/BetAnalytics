library(tidyverse)
library(tidymodels)
library(data.table)
library(lubridate)
library(shapviz)
library(kernelshap)

source("HelperFunctions.R")
dat <- readRDS(file = "BoxscoreData20230210.RDS") %>% data.table()

dat <- dat[MP != "Did Not Play"]

timeFields <- "MP"
dateFields <- "GameDate"
numericFields <- c('FG',
                   'FGA',
                   'FG%',
                   '3P',
                   '3PA',
                   '3P%',
                   'FT',
                   'FTA',
                   'FT%',
                   'ORB',
                   'DRB',
                   'TRB',
                   'AST',
                   'STL',
                   'BLK',
                   'TOV',
                   'PF',
                   'PTS',
                   '+/-',
                   'TS%',
                   'eFG%',
                   '3PAr',
                   'FTr',
                   'ORB%',
                   'DRB%',
                   'TRB%',
                   'AST%',
                   'STL%',
                   'BLK%',
                   'TOV%',
                   'USG%',
                   'ORtg',
                   'DRtg',
                   'BPM')

factorFields <- names(dat) %>% setdiff(c(timeFields, dateFields, numericFields))

dat[, (dateFields) := lapply(.SD,  lubridate::as_date), .SDcols = dateFields]
dat[, (numericFields) := lapply(.SD,  as.numeric), .SDcols = numericFields]
dat[, (factorFields) := lapply(.SD,  as.factor), .SDcols = factorFields]

dat[, c("Minutes", "Seconds") := tstrsplit(MP, split = ":", fixed = TRUE)]
dat[, Minutes := as.numeric(Minutes)]
dat[, Seconds := as.numeric(Seconds)]
dat[, MinutesPlayed := Minutes + Seconds / 60] %>% 
  .[, c("MP", "Minutes", "Seconds") := NULL]

dat <- dat[!is.na(MinutesPlayed)]

setnames(
  dat,
  old = c(
    'Player',
    'FG',
    'FGA',
    'FG%',
    '3P',
    '3PA',
    '3P%',
    'FT',
    'FTA',
    'FT%',
    'ORB',
    'DRB',
    'TRB',
    'AST',
    'STL',
    'BLK',
    'TOV',
    'PF',
    'PTS',
    '+/-',
    'Team',
    'Location',
    'Opponent',
    'GameDate',
    'TS%',
    'eFG%',
    '3PAr',
    'FTr',
    'ORB%',
    'DRB%',
    'TRB%',
    'AST%',
    'STL%',
    'BLK%',
    'TOV%',
    'USG%',
    'ORtg',
    'DRtg',
    'BPM',
    'MinutesPlayed'
  ),
  new = c(
    'Player',
    'FieldGoalsMade',
    'FieldGoalsAttempted',
    'FieldGoalsPercentage',
    'ThreePointersMade',
    'ThreePointersAttempted',
    'ThreePointersPercentage',
    'FreeThrowsMade',
    'FreeThrowsAttempted',
    'FreeThrowsPercentage',
    'OffensiveRebounds',
    'DefensiveRebounds',
    'TotalRebounds',
    'Assists',
    'Steals',
    'Blocks',
    'Turnovers',
    'PersonalFouls',
    'Points',
    'PlusMinus',
    'Team',
    'Location',
    'Opponent',
    'GameDate',
    'TrueShootingPercentage',
    'EffectiveFieldGoalPercentage',
    'ThreePointAttemptRate',
    'FreeThrowAttemptRate',
    'OffensiveReboundPercentage',
    'DefensiveReboundPercentage',
    'TotalReboundPercentage',
    'AssistPercentage',
    'StealPercentage',
    'BlockPercentage',
    'TurnoverPercentage',
    'UsagePercentage',
    'OffensiveRating',
    'DefensiveRating',
    'BoxPlusMinus',
    'MinutesPlayed'
  ) 
)

dropUs <- c('TrueShootingPercentage',
            'EffectiveFieldGoalPercentage',
            'ThreePointAttemptRate',
            'FreeThrowAttemptRate',
            'OffensiveReboundPercentage',
            'DefensiveReboundPercentage',
            'TotalReboundPercentage',
            'AssistPercentage',
            'StealPercentage',
            'BlockPercentage',
            'TurnoverPercentage',
            'UsagePercentage',
            'OffensiveRating',
            'DefensiveRating',
            'BoxPlusMinus',
            'FieldGoalsPercentage',
            'ThreePointersPercentage',
            'FreeThrowsPercentage',
            'PlusMinus'
)

dat[, (dropUs) := NULL]

columnToSum <-
  c(
    "Points",
    "FieldGoalsMade",
    "FieldGoalsAttempted",
    "ThreePointersMade",
    "ThreePointersAttempted",
    "FreeThrowsMade",
    "FreeThrowsAttempted",
    "OffensiveRebounds",
    "DefensiveRebounds",
    "TotalRebounds",
    "Assists",
    "Steals",
    "Blocks",
    "Turnovers",
    "PersonalFouls",
    "MinutesPlayed"
  )

# Player Recency Stats ####
setorder(dat, Team, Player, GameDate)

dat[, 
    paste0(columnToSum, 1) := lapply(.SD, function(x) calculate_recency_stats(column = x, gamesToSum = 1)), 
    .SDcols = columnToSum,
    .(Player)]
dat[, 
    paste0(columnToSum, 3) := lapply(.SD, function(x) calculate_recency_stats(column = x, gamesToSum = 3)), 
    .SDcols = columnToSum,
    .(Player)]
dat[, 
    paste0(columnToSum, 5) := lapply(.SD, function(x) calculate_recency_stats(column = x, gamesToSum = 5)), 
    .SDcols = columnToSum,
    .(Player)]
dat[, 
    paste0(columnToSum, 10) := lapply(.SD, function(x) calculate_recency_stats(column = x, gamesToSum = 10)), 
    .SDcols = columnToSum,
    .(Player)]
dat[,
    paste0(columnToSum, "ToDate") := lapply(.SD, function(x)
      cumsum(x) %>% shift(n = 1, type = "lag")),
    .SDcols = columnToSum,
    .(Player)]

# Team Recency Stats ####
teamDat <- dat[, lapply(.SD, sum), .SDcols = columnToSum, .(Team, GameDate)]

setorder(teamDat, Team, GameDate)

teamDat[,
        paste0("Team", columnToSum, 1) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 1)),
        .SDcols = columnToSum,
        .(Team)]
teamDat[,
        paste0("Team", columnToSum, 3) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 3)),
        .SDcols = columnToSum,
        .(Team)]
teamDat[,
        paste0("Team", columnToSum, 5) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 5)),
        .SDcols = columnToSum,
        .(Team)]
teamDat[,
        paste0("Team", columnToSum, 10) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 10)),
        .SDcols = columnToSum,
        .(Team)]
teamDat[,
        paste0("Team", columnToSum, "ToDate") := lapply(.SD, function(x)
          cumsum(x) %>% shift(n = 1, type = "lag")),
        .SDcols = columnToSum,
        .(Team)]

teamDat[,(columnToSum) := NULL]

# Opponent Recency Stats ####
opponentDat <- dat[, lapply(.SD, sum), .SDcols = columnToSum, .(Opponent, GameDate)]

setorder(opponentDat, Opponent, GameDate)

opponentDat[,
        paste0("Opponent", columnToSum, 1) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 1)),
        .SDcols = columnToSum,
        .(Opponent)]
opponentDat[,
        paste0("Opponent", columnToSum, 3) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 3)),
        .SDcols = columnToSum,
        .(Opponent)]
opponentDat[,
        paste0("Opponent", columnToSum, 5) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 5)),
        .SDcols = columnToSum,
        .(Opponent)]
opponentDat[,
        paste0("Opponent", columnToSum, 10) := lapply(.SD, function(x)
          calculate_recency_stats(column = x, gamesToSum = 10)),
        .SDcols = columnToSum,
        .(Opponent)]
opponentDat[,
        paste0("Opponent", columnToSum, "ToDate") := lapply(.SD, function(x)
          cumsum(x) %>% shift(n = 1, type = "lag")),
        .SDcols = columnToSum,
        .(Opponent)]

opponentDat[,(columnToSum) := NULL]

# Merge Datasets ####
dat <- dat[teamDat,
           on = .(Team, GameDate)] %>%
  .[opponentDat,
    on = .(Opponent, GameDate)]

rm(opponentDat, teamDat)

setorder(dat, Team, Player, GameDate)

# Estimate Player Positions ####

## Season ####
dat[,
    UnadjustedPositionToDate := calculate_unadjusted_position(
      totalRebounds = TotalReboundsToDate,
      steals = StealsToDate,
      personalFouls = PersonalFoulsToDate,
      assists = AssistsToDate,
      blocks = BlocksToDate,
      teamTotalRebounds = TeamTotalReboundsToDate,
      teamSteals = TeamStealsToDate,
      teamPersonalFouls = TeamPersonalFoulsToDate,
      teamAssists = TeamAssistsToDate,
      teamBlocks = TeamBlocksToDate
    )]

dat[,
    TeamMinuteWeightedAveragePosition := sum(MinutesPlayedToDate * UnadjustedPositionToDate)/ sum(MinutesPlayedToDate),
    .(Team, GameDate)]

dat[, PositionToDate := calculate_position(unadjustedPosition = UnadjustedPositionToDate,
                                           teamMinuteWeightedAveragePosition = TeamMinuteWeightedAveragePosition)]

## 1 Game ####
dat[,
    UnadjustedPosition1 := calculate_unadjusted_position(
      totalRebounds = TotalRebounds1,
      steals = Steals1,
      personalFouls = PersonalFouls1,
      assists = Assists1,
      blocks = Blocks1,
      teamTotalRebounds = TeamTotalRebounds1,
      teamSteals = TeamSteals1,
      teamPersonalFouls = TeamPersonalFouls1,
      teamAssists = TeamAssists1,
      teamBlocks = TeamBlocks1
    )]

dat[,
    TeamMinuteWeightedAveragePosition := sum(MinutesPlayed1 * UnadjustedPosition1)/ sum(MinutesPlayed1),
    .(Team, GameDate)]

dat[, Position1 := calculate_position(unadjustedPosition = UnadjustedPosition1,
                                           teamMinuteWeightedAveragePosition = TeamMinuteWeightedAveragePosition)]

## 3 Game ####
dat[,
    UnadjustedPosition3 := calculate_unadjusted_position(
      totalRebounds = TotalRebounds3,
      steals = Steals3,
      personalFouls = PersonalFouls3,
      assists = Assists3,
      blocks = Blocks3,
      teamTotalRebounds = TeamTotalRebounds3,
      teamSteals = TeamSteals3,
      teamPersonalFouls = TeamPersonalFouls3,
      teamAssists = TeamAssists3,
      teamBlocks = TeamBlocks3
    )]

dat[,
    TeamMinuteWeightedAveragePosition := sum(MinutesPlayed3 * UnadjustedPosition3)/ sum(MinutesPlayed3),
    .(Team, GameDate)]

dat[, Position3 := calculate_position(unadjustedPosition = UnadjustedPosition3,
                                           teamMinuteWeightedAveragePosition = TeamMinuteWeightedAveragePosition)]

## 5 Game ####
dat[,
    UnadjustedPosition5 := calculate_unadjusted_position(
      totalRebounds = TotalRebounds5,
      steals = Steals5,
      personalFouls = PersonalFouls5,
      assists = Assists5,
      blocks = Blocks5,
      teamTotalRebounds = TeamTotalRebounds5,
      teamSteals = TeamSteals5,
      teamPersonalFouls = TeamPersonalFouls5,
      teamAssists = TeamAssists5,
      teamBlocks = TeamBlocks5
    )]

dat[,
    TeamMinuteWeightedAveragePosition := sum(MinutesPlayed5 * UnadjustedPosition5)/ sum(MinutesPlayed5),
    .(Team, GameDate)]

dat[, Position5 := calculate_position(unadjustedPosition = UnadjustedPosition5,
                                           teamMinuteWeightedAveragePosition = TeamMinuteWeightedAveragePosition)]

## 10 Game ####
dat[,
    UnadjustedPosition10 := calculate_unadjusted_position(
      totalRebounds = TotalRebounds10,
      steals = Steals10,
      personalFouls = PersonalFouls10,
      assists = Assists10,
      blocks = Blocks10,
      teamTotalRebounds = TeamTotalRebounds10,
      teamSteals = TeamSteals10,
      teamPersonalFouls = TeamPersonalFouls10,
      teamAssists = TeamAssists10,
      teamBlocks = TeamBlocks10
    )]

dat[,
    TeamMinuteWeightedAveragePosition := sum(MinutesPlayed10 * UnadjustedPosition10)/ sum(MinutesPlayed10),
    .(Team, GameDate)]

dat[, Position10 := calculate_position(unadjustedPosition = UnadjustedPosition10,
                                           teamMinuteWeightedAveragePosition = TeamMinuteWeightedAveragePosition)]

# Estimate Offensive Role ####

## Season ####
dat[,
    ThresholdPointToDate := calculate_threshold_points(
      points = PointsToDate,
      fieldGoalAttempts = FieldGoalsAttemptedToDate,
      freeThrowAttempts = FreeThrowsAttemptedToDate,
      teamPoints = TeamPointsToDate,
      teamFieldGoalAttempts = TeamFieldGoalsAttemptedToDate,
      TeamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate
    )]

dat[,
    TeamThresholdPointToDate := sum(ThresholdPointToDate),
    .(Team, GameDate)]

dat[,
    UnadjustedOffensiveRoleToDate := calculate_unadjusted_offensive_role(
      assists = AssistsToDate,
      thresholdPoints = ThresholdPointToDate,
      teamAssists = TeamAssistsToDate,
      teamThresholdPoints = TeamThresholdPointToDate
    )]

dat[,
    TeamMinuteWeightedAverageOffensiveRole := sum(MinutesPlayedToDate * UnadjustedOffensiveRoleToDate) / sum(MinutesPlayedToDate),
    .(Team, GameDate)]

dat[,
    OffensiveRoleToDate := calculate_offensive_role(
      unadjustedOffensiveRole = UnadjustedOffensiveRoleToDate,
      teamMinuteWeightedAverageOffensiveRole = TeamMinuteWeightedAverageOffensiveRole
    )]

## 1 Game ####
dat[,
    ThresholdPoint1 := calculate_threshold_points(
      points = Points1,
      fieldGoalAttempts = FieldGoalsAttempted1,
      freeThrowAttempts = FreeThrowsAttempted1,
      teamPoints = TeamPoints1,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted1,
      TeamFreeThrowAttempts = TeamFreeThrowsAttempted1
    )]

dat[,
    TeamThresholdPoint1 := sum(ThresholdPoint1),
    .(Team, GameDate)]

dat[,
    UnadjustedOffensiveRole1 := calculate_unadjusted_offensive_role(
      assists = Assists1,
      thresholdPoints = ThresholdPoint1,
      teamAssists = TeamAssists1,
      teamThresholdPoints = TeamThresholdPoint1
    )]

dat[,
    TeamMinuteWeightedAverageOffensiveRole := sum(MinutesPlayed1 * UnadjustedOffensiveRole1) / sum(MinutesPlayed1),
    .(Team, GameDate)]

dat[,
    OffensiveRole1 := calculate_offensive_role(
      unadjustedOffensiveRole = UnadjustedOffensiveRole1,
      teamMinuteWeightedAverageOffensiveRole = TeamMinuteWeightedAverageOffensiveRole
    )]

## 3 Game ####
dat[,
    ThresholdPoint3 := calculate_threshold_points(
      points = Points3,
      fieldGoalAttempts = FieldGoalsAttempted3,
      freeThrowAttempts = FreeThrowsAttempted3,
      teamPoints = TeamPoints3,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted3,
      TeamFreeThrowAttempts = TeamFreeThrowsAttempted3
    )]

dat[,
    TeamThresholdPoint3 := sum(ThresholdPoint3),
    .(Team, GameDate)]

dat[,
    UnadjustedOffensiveRole3 := calculate_unadjusted_offensive_role(
      assists = Assists3,
      thresholdPoints = ThresholdPoint3,
      teamAssists = TeamAssists3,
      teamThresholdPoints = TeamThresholdPoint3
    )]

dat[,
    TeamMinuteWeightedAverageOffensiveRole := sum(MinutesPlayed3 * UnadjustedOffensiveRole3) / sum(MinutesPlayed3),
    .(Team, GameDate)]

dat[,
    OffensiveRole3 := calculate_offensive_role(
      unadjustedOffensiveRole = UnadjustedOffensiveRole3,
      teamMinuteWeightedAverageOffensiveRole = TeamMinuteWeightedAverageOffensiveRole
    )]

## 5 Game ####
dat[,
    ThresholdPoint5 := calculate_threshold_points(
      points = Points5,
      fieldGoalAttempts = FieldGoalsAttempted5,
      freeThrowAttempts = FreeThrowsAttempted5,
      teamPoints = TeamPoints5,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted5,
      TeamFreeThrowAttempts = TeamFreeThrowsAttempted5
    )]

dat[,
    TeamThresholdPoint5 := sum(ThresholdPoint5),
    .(Team, GameDate)]

dat[,
    UnadjustedOffensiveRole5 := calculate_unadjusted_offensive_role(
      assists = Assists5,
      thresholdPoints = ThresholdPoint5,
      teamAssists = TeamAssists5,
      teamThresholdPoints = TeamThresholdPoint5
    )]

dat[,
    TeamMinuteWeightedAverageOffensiveRole := sum(MinutesPlayed5 * UnadjustedOffensiveRole5) / sum(MinutesPlayed5),
    .(Team, GameDate)]

dat[,
    OffensiveRole5 := calculate_offensive_role(
      unadjustedOffensiveRole = UnadjustedOffensiveRole5,
      teamMinuteWeightedAverageOffensiveRole = TeamMinuteWeightedAverageOffensiveRole
    )]

## 10 Game ####
dat[,
    ThresholdPoint10 := calculate_threshold_points(
      points = Points10,
      fieldGoalAttempts = FieldGoalsAttempted10,
      freeThrowAttempts = FreeThrowsAttempted10,
      teamPoints = TeamPoints10,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted10,
      TeamFreeThrowAttempts = TeamFreeThrowsAttempted10
    )]

dat[,
    TeamThresholdPoint10 := sum(ThresholdPoint10),
    .(Team, GameDate)]

dat[,
    UnadjustedOffensiveRole10 := calculate_unadjusted_offensive_role(
      assists = Assists10,
      thresholdPoints = ThresholdPoint10,
      teamAssists = TeamAssists10,
      teamThresholdPoints = TeamThresholdPoint10
    )]

dat[,
    TeamMinuteWeightedAverageOffensiveRole := sum(MinutesPlayed10 * UnadjustedOffensiveRole10) / sum(MinutesPlayed10),
    .(Team, GameDate)]

dat[,
    OffensiveRole10 := calculate_offensive_role(
      unadjustedOffensiveRole = UnadjustedOffensiveRole10,
      teamMinuteWeightedAverageOffensiveRole = TeamMinuteWeightedAverageOffensiveRole
    )]

fieldsToDrop <- names(dat) %>%  grep(pattern = "Unadjusted|Threshold|MinuteWeighted", value = TRUE)
dat[, (fieldsToDrop) := NULL]
# Touches ####

## Season ####
dat[,
    TouchesToDate := calculate_touches(
      assists = AssistsToDate,
      fieldGoalAttempts = FreeThrowsAttemptedToDate,
      freeThrowAttempts = FreeThrowsAttemptedToDate,
      turnovers = TurnoversToDate,
      teamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate,
      opponentPersonalFouls = OpponentPersonalFoulsToDate
    )]

## 1 Game ####
dat[,
    Touches1 := calculate_touches(
      assists = Assists1,
      fieldGoalAttempts = FreeThrowsAttempted1,
      freeThrowAttempts = FreeThrowsAttempted1,
      turnovers = Turnovers1,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted1,
      opponentPersonalFouls = OpponentPersonalFouls1
    )]

## 3 Game ####
dat[,
    Touches3 := calculate_touches(
      assists = Assists3,
      fieldGoalAttempts = FreeThrowsAttempted3,
      freeThrowAttempts = FreeThrowsAttempted3,
      turnovers = Turnovers3,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted3,
      opponentPersonalFouls = OpponentPersonalFouls3
    )]

## 5 Game ####
dat[,
    Touches5 := calculate_touches(
      assists = Assists5,
      fieldGoalAttempts = FreeThrowsAttempted5,
      freeThrowAttempts = FreeThrowsAttempted5,
      turnovers = Turnovers5,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted5,
      opponentPersonalFouls = OpponentPersonalFouls5
    )]

## 10 Game ####
dat[,
    Touches10 := calculate_touches(
      assists = Assists10,
      fieldGoalAttempts = FreeThrowsAttempted10,
      freeThrowAttempts = FreeThrowsAttempted10,
      turnovers = Turnovers10,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted10,
      opponentPersonalFouls = OpponentPersonalFouls10
    )]

# Possesions ####

## Season ####
dat[,
    TeamPossesionsToDate := calculate_possessions(
      teamFieldGoalsMade = TeamFieldGoalsMadeToDate,
      teamFieldGoalAttempts = TeamFieldGoalsAttemptedToDate,
      teamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate,
      teamOffensiveRebounds = TeamOffensiveReboundsToDate,
      teamDefensiveRebounds = TeamDefensiveReboundsToDate,
      teamTurnovers = TeamTurnoversToDate,
      opponentFieldGoalsMade = OpponentFieldGoalsMadeToDate,
      opponentFieldGoalAttempts = OpponentFieldGoalsAttemptedToDate,
      opponentFreeThrowAttempts = OpponentFreeThrowsAttemptedToDate,
      opponentOffensiveRebounds = OpponentOffensiveReboundsToDate,
      opponentDefensiveRebounds = OpponentDefensiveReboundsToDate,
      opponentTurnovers = OpponentTurnoversToDate
    )]

## 1 Game ####
dat[,
    TeamPossesions1 := calculate_possessions(
      teamFieldGoalsMade = TeamFieldGoalsMade1,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted1,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted1,
      teamOffensiveRebounds = TeamOffensiveRebounds1,
      teamDefensiveRebounds = TeamDefensiveRebounds1,
      teamTurnovers = TeamTurnovers1,
      opponentFieldGoalsMade = OpponentFieldGoalsMade1,
      opponentFieldGoalAttempts = OpponentFieldGoalsAttempted1,
      opponentFreeThrowAttempts = OpponentFreeThrowsAttempted1,
      opponentOffensiveRebounds = OpponentOffensiveRebounds1,
      opponentDefensiveRebounds = OpponentDefensiveRebounds1,
      opponentTurnovers = OpponentTurnovers1
    )]

## 3 Game ####
dat[,
    TeamPossesions3 := calculate_possessions(
      teamFieldGoalsMade = TeamFieldGoalsMade3,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted3,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted3,
      teamOffensiveRebounds = TeamOffensiveRebounds3,
      teamDefensiveRebounds = TeamDefensiveRebounds3,
      teamTurnovers = TeamTurnovers3,
      opponentFieldGoalsMade = OpponentFieldGoalsMade3,
      opponentFieldGoalAttempts = OpponentFieldGoalsAttempted3,
      opponentFreeThrowAttempts = OpponentFreeThrowsAttempted3,
      opponentOffensiveRebounds = OpponentOffensiveRebounds3,
      opponentDefensiveRebounds = OpponentDefensiveRebounds3,
      opponentTurnovers = OpponentTurnovers3
    )]

## 5 Game ####
dat[,
    TeamPossesions5 := calculate_possessions(
      teamFieldGoalsMade = TeamFieldGoalsMade5,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted5,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted5,
      teamOffensiveRebounds = TeamOffensiveRebounds5,
      teamDefensiveRebounds = TeamDefensiveRebounds5,
      teamTurnovers = TeamTurnovers5,
      opponentFieldGoalsMade = OpponentFieldGoalsMade5,
      opponentFieldGoalAttempts = OpponentFieldGoalsAttempted5,
      opponentFreeThrowAttempts = OpponentFreeThrowsAttempted5,
      opponentOffensiveRebounds = OpponentOffensiveRebounds5,
      opponentDefensiveRebounds = OpponentDefensiveRebounds5,
      opponentTurnovers = OpponentTurnovers5
    )]

## 10 Game ####
dat[,
    TeamPossesions10 := calculate_possessions(
      teamFieldGoalsMade = TeamFieldGoalsMade10,
      teamFieldGoalAttempts = TeamFieldGoalsAttempted10,
      teamFreeThrowAttempts = TeamFreeThrowsAttempted10,
      teamOffensiveRebounds = TeamOffensiveRebounds10,
      teamDefensiveRebounds = TeamDefensiveRebounds10,
      teamTurnovers = TeamTurnovers10,
      opponentFieldGoalsMade = OpponentFieldGoalsMade10,
      opponentFieldGoalAttempts = OpponentFieldGoalsAttempted10,
      opponentFreeThrowAttempts = OpponentFreeThrowsAttempted10,
      opponentOffensiveRebounds = OpponentOffensiveRebounds10,
      opponentDefensiveRebounds = OpponentDefensiveRebounds10,
      opponentTurnovers = OpponentTurnovers10
    )]

# Player Possessions ####

## Season ####
dat[,
    ScoringPossesionsToDate := calculate_player_scoring_possessions(
      fieldGoalsMade = FieldGoalsMadeToDate,
      assists = AssistsToDate,
      freeThrowsMade = FreeThrowsMadeToDate,
      minutes = MinutesPlayedToDate,
      teamFieldGoalsMade = TeamFieldGoalsMadeToDate,
      teamAssists = TeamAssistsToDate,
      teamMinutes = TeamMinutesPlayedToDate
    )]

dat[,
    NonScoringPossesionsToDate := calculate_player_non_scoring_possessions(
      fieldGoalsMade = FieldGoalsMadeToDate,
      fieldGoalAttempts = FieldGoalsAttemptedToDate,
      freeThrowAttempts = FreeThrowsAttemptedToDate,
      turnovers = TurnoversToDate
    )]

dat[,
    PossessionsToDate := calculate_player_possessions(scoringPossessions = ScoringPossesionsToDate,
                                                      nonScoringPossessions = NonScoringPossesionsToDate)]

## 1 Game ####
dat[,
    ScoringPossesions1 := calculate_player_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade1,
      assists = Assists1,
      freeThrowsMade = FreeThrowsMade1,
      minutes = MinutesPlayed1,
      teamFieldGoalsMade = TeamFieldGoalsMade1,
      teamAssists = TeamAssists1,
      teamMinutes = TeamMinutesPlayed1
    )]

dat[,
    NonScoringPossesions1 := calculate_player_non_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade1,
      fieldGoalAttempts = FieldGoalsAttempted1,
      freeThrowAttempts = FreeThrowsAttempted1,
      turnovers = Turnovers1
    )]

dat[,
    Possessions1 := calculate_player_possessions(scoringPossessions = ScoringPossesions1,
                                                      nonScoringPossessions = NonScoringPossesions1)]

## 3 Game ####
dat[,
    ScoringPossesions3 := calculate_player_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade3,
      assists = Assists3,
      freeThrowsMade = FreeThrowsMade3,
      minutes = MinutesPlayed3,
      teamFieldGoalsMade = TeamFieldGoalsMade3,
      teamAssists = TeamAssists3,
      teamMinutes = TeamMinutesPlayed3
    )]

dat[,
    NonScoringPossesions3 := calculate_player_non_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade3,
      fieldGoalAttempts = FieldGoalsAttempted3,
      freeThrowAttempts = FreeThrowsAttempted3,
      turnovers = Turnovers3
    )]

dat[,
    Possessions3 := calculate_player_possessions(scoringPossessions = ScoringPossesions3,
                                                      nonScoringPossessions = NonScoringPossesions3)]

## 5 Game ####
dat[,
    ScoringPossesions5 := calculate_player_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade5,
      assists = Assists5,
      freeThrowsMade = FreeThrowsMade5,
      minutes = MinutesPlayed5,
      teamFieldGoalsMade = TeamFieldGoalsMade5,
      teamAssists = TeamAssists5,
      teamMinutes = TeamMinutesPlayed5
    )]

dat[,
    NonScoringPossesions5 := calculate_player_non_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade5,
      fieldGoalAttempts = FieldGoalsAttempted5,
      freeThrowAttempts = FreeThrowsAttempted5,
      turnovers = Turnovers5
    )]

dat[,
    Possessions5 := calculate_player_possessions(scoringPossessions = ScoringPossesions5,
                                                      nonScoringPossessions = NonScoringPossesions5)]

## 10 Game ####
dat[,
    ScoringPossesions10 := calculate_player_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade10,
      assists = Assists10,
      freeThrowsMade = FreeThrowsMade10,
      minutes = MinutesPlayed10,
      teamFieldGoalsMade = TeamFieldGoalsMade10,
      teamAssists = TeamAssists10,
      teamMinutes = TeamMinutesPlayed10
    )]

dat[,
    NonScoringPossesions10 := calculate_player_non_scoring_possessions(
      fieldGoalsMade = FieldGoalsMade10,
      fieldGoalAttempts = FieldGoalsAttempted10,
      freeThrowAttempts = FreeThrowsAttempted10,
      turnovers = Turnovers10
    )]

dat[,
    Possessions10 := calculate_player_possessions(scoringPossessions = ScoringPossesions10,
                                                  nonScoringPossessions = NonScoringPossesions10)]

# Advance Player Stats ####

## Usage Pct ####

### Season ####

dat[, UsagePercentageToDate := calculate_usage_percentage(
  fieldGoalAttempts = FieldGoalsAttemptedToDate,
  freeThrowAttempts = FreeThrowsAttemptedToDate,
  turnovers = TurnoversToDate,
  teamFieldGoalAttempts = TeamFieldGoalsAttemptedToDate,
  teamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate,
  teamTurnovers = TeamTurnoversToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, UsagePercentage1 := calculate_usage_percentage(
  fieldGoalAttempts = FieldGoalsAttempted1,
  freeThrowAttempts = FreeThrowsAttempted1,
  turnovers = Turnovers1,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted1,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted1,
  teamTurnovers = TeamTurnovers1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, UsagePercentage3 := calculate_usage_percentage(
  fieldGoalAttempts = FieldGoalsAttempted3,
  freeThrowAttempts = FreeThrowsAttempted3,
  turnovers = Turnovers3,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted3,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted3,
  teamTurnovers = TeamTurnovers3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, UsagePercentage5 := calculate_usage_percentage(
  fieldGoalAttempts = FieldGoalsAttempted5,
  freeThrowAttempts = FreeThrowsAttempted5,
  turnovers = Turnovers5,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted5,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted5,
  teamTurnovers = TeamTurnovers5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, UsagePercentage10 := calculate_usage_percentage(
  fieldGoalAttempts = FieldGoalsAttempted10,
  freeThrowAttempts = FreeThrowsAttempted10,
  turnovers = Turnovers10,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted10,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted10,
  teamTurnovers = TeamTurnovers10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Assist Pct ####

### Season ####
dat[, AssistPercentageToDate := calculate_assist_percentage(
  assists = AssistsToDate,
  fieldGoalsMade = FieldGoalsMadeToDate,
  teamFieldGoalsMade = TeamFieldGoalsMadeToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####
dat[, AssistPercentage1 := calculate_assist_percentage(
  assists = Assists1,
  fieldGoalsMade = FieldGoalsMade1,
  teamFieldGoalsMade = TeamFieldGoalsMade1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####
dat[, AssistPercentage3 := calculate_assist_percentage(
  assists = Assists3,
  fieldGoalsMade = FieldGoalsMade3,
  teamFieldGoalsMade = TeamFieldGoalsMade3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####
dat[, AssistPercentage5 := calculate_assist_percentage(
  assists = Assists5,
  fieldGoalsMade = FieldGoalsMade5,
  teamFieldGoalsMade = TeamFieldGoalsMade5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####
dat[, AssistPercentage10 := calculate_assist_percentage(
  assists = Assists10,
  fieldGoalsMade = FieldGoalsMade10,
  teamFieldGoalsMade = TeamFieldGoalsMade10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Block Pct ####

### Season ####

dat[, BlockPercentageToDate := calculate_block_percentage(
  blocks = BlocksToDate,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttemptedToDate,
  opponentThreePointAttempt = OpponentThreePointersAttemptedToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, BlockPercentage1 := calculate_block_percentage(
  blocks = Blocks1,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted1,
  opponentThreePointAttempt = OpponentThreePointersAttempted1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, BlockPercentage3 := calculate_block_percentage(
  blocks = Blocks3,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted3,
  opponentThreePointAttempt = OpponentThreePointersAttempted3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, BlockPercentage5 := calculate_block_percentage(
  blocks = Blocks5,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted5,
  opponentThreePointAttempt = OpponentThreePointersAttempted5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, BlockPercentage10 := calculate_block_percentage(
  blocks = Blocks10,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted10,
  opponentThreePointAttempt = OpponentThreePointersAttempted10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Defensive Rebound Pct ####

### Season ####

dat[, DefensiveReboundPercentageToDate := calculate_defensive_rebound_percentage(
  defensiveRebounds = DefensiveReboundsToDate,
  teamDefensiveRebounds = TeamDefensiveReboundsToDate,
  opponentOffensiveRebounds = OpponentOffensiveReboundsToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, DefensiveReboundPercentage1 := calculate_defensive_rebound_percentage(
  defensiveRebounds = DefensiveRebounds1,
  teamDefensiveRebounds = TeamDefensiveRebounds1,
  opponentOffensiveRebounds = OpponentOffensiveRebounds1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, DefensiveReboundPercentage3 := calculate_defensive_rebound_percentage(
  defensiveRebounds = DefensiveRebounds3,
  teamDefensiveRebounds = TeamDefensiveRebounds3,
  opponentOffensiveRebounds = OpponentOffensiveRebounds3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, DefensiveReboundPercentage5 := calculate_defensive_rebound_percentage(
  defensiveRebounds = DefensiveRebounds5,
  teamDefensiveRebounds = TeamDefensiveRebounds5,
  opponentOffensiveRebounds = OpponentOffensiveRebounds5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, DefensiveReboundPercentage10 := calculate_defensive_rebound_percentage(
  defensiveRebounds = DefensiveRebounds10,
  teamDefensiveRebounds = TeamDefensiveRebounds10,
  opponentOffensiveRebounds = OpponentOffensiveRebounds10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Offensive Rebound Pct ####

### Season ####

dat[, OffensiveReboundPercentageToDate := calculate_offensive_rebound_percentage(
  offensiveRebounds = OffensiveReboundsToDate,
  teamOffensiveRebounds =  TeamOffensiveReboundsToDate,
  opponentDefensiveRebounds = OpponentDefensiveReboundsToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, OffensiveReboundPercentage1 := calculate_offensive_rebound_percentage(
  offensiveRebounds = OffensiveRebounds1,
  teamOffensiveRebounds =  TeamOffensiveRebounds1,
  opponentDefensiveRebounds = OpponentDefensiveRebounds1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, OffensiveReboundPercentage3 := calculate_offensive_rebound_percentage(
  offensiveRebounds = OffensiveRebounds3,
  teamOffensiveRebounds =  TeamOffensiveRebounds3,
  opponentDefensiveRebounds = OpponentDefensiveRebounds3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, OffensiveReboundPercentage5 := calculate_offensive_rebound_percentage(
  offensiveRebounds = OffensiveRebounds5,
  teamOffensiveRebounds =  TeamOffensiveRebounds5,
  opponentDefensiveRebounds = OpponentDefensiveRebounds5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, OffensiveReboundPercentage10 := calculate_offensive_rebound_percentage(
  offensiveRebounds = OffensiveRebounds10,
  teamOffensiveRebounds =  TeamOffensiveRebounds10,
  opponentDefensiveRebounds = OpponentDefensiveRebounds10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Total Rebound Pct ####

### Season ####

dat[, TotalReboundPercentageToDate := calculate_total_rebound_percentage(
  totalRebounds =  TotalReboundsToDate,
  teamTotalRebounds = TeamTotalReboundsToDate,
  opponentTotalRebounds = OpponentTotalReboundsToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, TotalReboundPercentage1 := calculate_total_rebound_percentage(
  totalRebounds =  TotalRebounds1,
  teamTotalRebounds = TeamTotalRebounds1,
  opponentTotalRebounds = OpponentTotalRebounds1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, TotalReboundPercentage3 := calculate_total_rebound_percentage(
  totalRebounds =  TotalRebounds3,
  teamTotalRebounds = TeamTotalRebounds3,
  opponentTotalRebounds = OpponentTotalRebounds3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, TotalReboundPercentage5 := calculate_total_rebound_percentage(
  totalRebounds =  TotalRebounds5,
  teamTotalRebounds = TeamTotalRebounds5,
  opponentTotalRebounds = OpponentTotalRebounds5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, TotalReboundPercentage10 := calculate_total_rebound_percentage(
  totalRebounds =  TotalRebounds10,
  teamTotalRebounds = TeamTotalRebounds10,
  opponentTotalRebounds = OpponentTotalRebounds10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Effective FG Pct ####

### Season ####

dat[, EffectiveFieldGoalPercentageToDate := calculate_effective_field_goal_percentage(
  fieldGoalsMade = FieldGoalsMadeToDate,
  threePointMade = ThreePointersMadeToDate,
  fieldGoalAttempts = FieldGoalsAttemptedToDate
)]

### 1 Game ####

dat[, EffectiveFieldGoalPercentage1 := calculate_effective_field_goal_percentage(
  fieldGoalsMade = FieldGoalsMade1,
  threePointMade = ThreePointersMade1,
  fieldGoalAttempts = FieldGoalsAttempted1
)]

### 3 Game ####

dat[, EffectiveFieldGoalPercentage3 := calculate_effective_field_goal_percentage(
  fieldGoalsMade = FieldGoalsMade3,
  threePointMade = ThreePointersMade3,
  fieldGoalAttempts = FieldGoalsAttempted3
)]

### 5 Game ####

dat[, EffectiveFieldGoalPercentage5 := calculate_effective_field_goal_percentage(
  fieldGoalsMade = FieldGoalsMade5,
  threePointMade = ThreePointersMade5,
  fieldGoalAttempts = FieldGoalsAttempted5
)]

### 10 Game ####

dat[, EffectiveFieldGoalPercentage10 := calculate_effective_field_goal_percentage(
  fieldGoalsMade = FieldGoalsMade10,
  threePointMade = ThreePointersMade10,
  fieldGoalAttempts = FieldGoalsAttempted10
)]

## True Shooting Pct ####

### Season ####

dat[, TrueShootingPercentageToDate := calculate_true_shooting_percentage(
  points = PointsToDate,
  fieldGoalAttempts = FieldGoalsAttemptedToDate,
  freeThrowAttempts = FreeThrowsAttemptedToDate
)]

### 1 Game ####

dat[, TrueShootingPercentage1 := calculate_true_shooting_percentage(
  points = Points1,
  fieldGoalAttempts = FieldGoalsAttempted1,
  freeThrowAttempts = FreeThrowsAttempted1
)]

### 3 Game ####

dat[, TrueShootingPercentage3 := calculate_true_shooting_percentage(
  points = Points3,
  fieldGoalAttempts = FieldGoalsAttempted3,
  freeThrowAttempts = FreeThrowsAttempted3
)]

### 5 Game ####

dat[, TrueShootingPercentage5 := calculate_true_shooting_percentage(
  points = Points5,
  fieldGoalAttempts = FieldGoalsAttempted5,
  freeThrowAttempts = FreeThrowsAttempted5
)]

### 10 Game ####

dat[, TrueShootingPercentage10 := calculate_true_shooting_percentage(
  points = Points10,
  fieldGoalAttempts = FieldGoalsAttempted10,
  freeThrowAttempts = FreeThrowsAttempted10
)]

## Free Throw Rate ####

### Season ####

dat[, FreeThrowAttemptRateToDate := calculate_free_throw_rate(fieldGoalAttempts = FieldGoalsAttemptedToDate, freeThrowAttempts = FreeThrowsAttemptedToDate)]

### 1 Game ####

dat[, FreeThrowAttemptRate1 := calculate_free_throw_rate(fieldGoalAttempts = FieldGoalsAttempted1, freeThrowAttempts = FreeThrowsAttempted1)]

### 3 Game ####

dat[, FreeThrowAttemptRate3 := calculate_free_throw_rate(fieldGoalAttempts = FieldGoalsAttempted3, freeThrowAttempts = FreeThrowsAttempted3)]

### 5 Game ####

dat[, FreeThrowAttemptRate5 := calculate_free_throw_rate(fieldGoalAttempts = FieldGoalsAttempted5, freeThrowAttempts = FreeThrowsAttempted5)]

### 10 Game ####

dat[, FreeThrowAttemptRate10 := calculate_free_throw_rate(fieldGoalAttempts = FieldGoalsAttempted10, freeThrowAttempts = FreeThrowsAttempted10)]

## Three Point Rate ####

### Season ####

dat[, ThreePointAttemptRateToDate := calculate_three_point_rate(fieldGoalAttempts = FieldGoalsAttemptedToDate,
                                                                threePointerAttempts = ThreePointersAttemptedToDate)]

### 1 Game ####

dat[, ThreePointAttemptRate1 := calculate_three_point_rate(fieldGoalAttempts = FieldGoalsAttempted1,
                                                                threePointerAttempts = ThreePointersAttempted1)]

### 3 Game ####

dat[, ThreePointAttemptRate3 := calculate_three_point_rate(fieldGoalAttempts = FieldGoalsAttempted3,
                                                                threePointerAttempts = ThreePointersAttempted3)]

### 5 Game ####

dat[, ThreePointAttemptRate5 := calculate_three_point_rate(fieldGoalAttempts = FieldGoalsAttempted5,
                                                                threePointerAttempts = ThreePointersAttempted5)]

### 10 Game ####

dat[, ThreePointAttemptRate10 := calculate_three_point_rate(fieldGoalAttempts = FieldGoalsAttempted10,
                                                                threePointerAttempts = ThreePointersAttempted10)]

## Steal Pct ####

### Season ####

dat[, StealPercentageToDate := calculate_steal_percentage(
  steals = StealsToDate,
  opponentPossessions = TeamPossesionsToDate,
  mintesPlayed = MinutesPlayedToDate,
  teamMinutesPlayed = TeamMinutesPlayedToDate
)]

### 1 Game ####

dat[, StealPercentage1 := calculate_steal_percentage(
  steals = Steals1,
  opponentPossessions = TeamPossesions1,
  mintesPlayed = MinutesPlayed1,
  teamMinutesPlayed = TeamMinutesPlayed1
)]

### 3 Game ####

dat[, StealPercentage3 := calculate_steal_percentage(
  steals = Steals3,
  opponentPossessions = TeamPossesions3,
  mintesPlayed = MinutesPlayed3,
  teamMinutesPlayed = TeamMinutesPlayed3
)]

### 5 Game ####

dat[, StealPercentage5 := calculate_steal_percentage(
  steals = Steals5,
  opponentPossessions = TeamPossesions5,
  mintesPlayed = MinutesPlayed5,
  teamMinutesPlayed = TeamMinutesPlayed5
)]

### 10 Game ####

dat[, StealPercentage10 := calculate_steal_percentage(
  steals = Steals10,
  opponentPossessions = TeamPossesions10,
  mintesPlayed = MinutesPlayed10,
  teamMinutesPlayed = TeamMinutesPlayed10
)]

## Turnover Pct ####

### Season ####

dat[, TurnoverPercentageToDate := calculate_turnover_percentage(turnovers = TurnoversToDate, touches = TouchesToDate)]

### 1 Game ####

dat[, TurnoverPercentage1 := calculate_turnover_percentage(turnovers = Turnovers1, touches = Touches1)]

### 3 Game ####

dat[, TurnoverPercentage3 := calculate_turnover_percentage(turnovers = Turnovers3, touches = Touches3)]

### 5 Game ####

dat[, TurnoverPercentage5 := calculate_turnover_percentage(turnovers = TurnoversToDate, touches = TouchesToDate)]

### 10 Game ####

dat[, TurnoverPercentage10 := calculate_turnover_percentage(turnovers = Turnovers10, touches = Touches10)]

## Pass Pct ####

### Season ####

dat[, PassPercentageToDate := calculate_pass_percentage(assists = AssistsToDate, touches = TouchesToDate)]

### 1 Game ####

dat[, PassPercentage1 := calculate_pass_percentage(assists = Assists1, touches = Touches1)]

### 3 Game ####

dat[, PassPercentage3 := calculate_pass_percentage(assists = Assists3, touches = Touches3)]

### 5 Game ####

dat[, PassPercentage5 := calculate_pass_percentage(assists = Assists5, touches = Touches5)]

### 10 Game ####

dat[, PassPercentage10 := calculate_pass_percentage(assists = Assists10, touches = Touches10)]

## Shoot Pct ####

### Season ####

dat[, ShotPercentageToDate := calculate_shoot_percentage(fieldGoalAttempts = FieldGoalsAttemptedToDate, touches = TouchesToDate)]

### 1 Game ####

dat[, ShotPercentage1 := calculate_shoot_percentage(fieldGoalAttempts = FieldGoalsAttempted1, touches = Touches1)]

### 3 Game ####

dat[, ShotPercentage3 := calculate_shoot_percentage(fieldGoalAttempts = FieldGoalsAttempted3, touches = Touches3)]

### 5 Game ####

dat[, ShotPercentage5 := calculate_shoot_percentage(fieldGoalAttempts = FieldGoalsAttempted5, touches = Touches5)]

### 10 Game ####

dat[, ShotPercentage10 := calculate_shoot_percentage(fieldGoalAttempts = FieldGoalsAttempted10, touches = Touches10)]

## Fouled Pct ####

### Season ####

dat[,FouledPercentageToDate := calculate_fouled_percentage(
  freeThrowAttempts = FreeThrowsAttemptedToDate,
  teamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate,
  opponentPersonalFouls = OpponentPersonalFoulsToDate,
  touches = TouchesToDate
)]

### 1 Game ####

dat[,FouledPercentage1 := calculate_fouled_percentage(
  freeThrowAttempts = FreeThrowsAttempted1,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted1,
  opponentPersonalFouls = OpponentPersonalFouls1,
  touches = Touches1
)]

### 3 Game ####

dat[,FouledPercentage3 := calculate_fouled_percentage(
  freeThrowAttempts = FreeThrowsAttempted3,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted3,
  opponentPersonalFouls = OpponentPersonalFouls3,
  touches = Touches3
)]

### 5 Game ####

dat[,FouledPercentage5 := calculate_fouled_percentage(
  freeThrowAttempts = FreeThrowsAttempted5,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted5,
  opponentPersonalFouls = OpponentPersonalFouls5,
  touches = Touches5
)]

### 10 Game ####

dat[,FouledPercentage10 := calculate_fouled_percentage(
  freeThrowAttempts = FreeThrowsAttempted10,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted10,
  opponentPersonalFouls = OpponentPersonalFouls10,
  touches = Touches10
)]

# Advanced Team Stats ####

## Assist Percentage ####

### Season ####

dat[, TeamAssistPercentageToDate := calculate_team_assist_percentage(teamAssists = TeamAssistsToDate, teamFieldGoalsMade = TeamFieldGoalsMadeToDate)]

### 1 Game ####

dat[, TeamAssistPercentage1 := calculate_team_assist_percentage(teamAssists = TeamAssists1, teamFieldGoalsMade = TeamFieldGoalsMade1)]

### 3 Game ####

dat[, TeamAssistPercentage3 := calculate_team_assist_percentage(teamAssists = TeamAssists3, teamFieldGoalsMade = TeamFieldGoalsMade3)]

### 5 Game ####

dat[, TeamAssistPercentage5 := calculate_team_assist_percentage(teamAssists = TeamAssists5, teamFieldGoalsMade = TeamFieldGoalsMade5)]

### 10 Game ####

dat[, TeamAssistPercentage10 := calculate_team_assist_percentage(teamAssists = TeamAssists10, teamFieldGoalsMade = TeamFieldGoalsMade10)]

## Block Percentage ####

### Season ####

dat[, TeamBlockPercentageToDate := calculate_team_block_percentage(
  teamBlocks = TeamBlocksToDate,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttemptedToDate,
  opponentThreePointAttempt = OpponentThreePointersAttemptedToDate
)]

### 1 Game ####

dat[, TeamBlockPercentage1 := calculate_team_block_percentage(
  teamBlocks = TeamBlocks1,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted1,
  opponentThreePointAttempt = OpponentThreePointersAttempted1
)]

### 3 Game ####

dat[, TeamBlockPercentage3 := calculate_team_block_percentage(
  teamBlocks = TeamBlocks3,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted3,
  opponentThreePointAttempt = OpponentThreePointersAttempted3
)]

### 5 Game ####

dat[, TeamBlockPercentage5 := calculate_team_block_percentage(
  teamBlocks = TeamBlocks5,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted5,
  opponentThreePointAttempt = OpponentThreePointersAttempted5
)]

### 10 Game ####

dat[, TeamBlockPercentage10 := calculate_team_block_percentage(
  teamBlocks = TeamBlocks10,
  opponentFieldGoalAttempt = OpponentFieldGoalsAttempted10,
  opponentThreePointAttempt = OpponentThreePointersAttempted10
)]

## Defensive Rebound Percentage ####

### Season ####

dat[, TeamDefensiveReboundPercentageToDate := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = TeamDefensiveReboundsToDate,
                                                                                          opponentOffensiveRebounds = OpponentOffensiveReboundsToDate)]

### 1 Game ####

dat[, TeamDefensiveReboundPercentage1 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = TeamDefensiveRebounds1,
                                                                                          opponentOffensiveRebounds = OpponentOffensiveRebounds1)]

### 3 Game ####

dat[, TeamDefensiveReboundPercentage3 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = TeamDefensiveRebounds3,
                                                                                          opponentOffensiveRebounds = OpponentOffensiveRebounds3)]

### 5 Game ####

dat[, TeamDefensiveReboundPercentage5 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = TeamDefensiveRebounds5,
                                                                                          opponentOffensiveRebounds = OpponentOffensiveRebounds5)]

### 10 Game ####

dat[, TeamDefensiveReboundPercentage10 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = TeamDefensiveRebounds10,
                                                                                          opponentOffensiveRebounds = OpponentOffensiveRebounds10)]

## Offensive Rebound Pct ####

### Season ####

dat[, TeamOffensiveReboundPercentageToDate := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = TeamOffensiveReboundsToDate,
                                                                                          opponentDefensiveRebounds = OpponentDefensiveReboundsToDate)]
### 1 Game ####

dat[, TeamOffensiveReboundPercentage1 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = TeamOffensiveRebounds1,
                                                                                          opponentDefensiveRebounds = OpponentDefensiveRebounds1)]

### 3 Game ####

dat[, TeamOffensiveReboundPercentage3 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = TeamOffensiveRebounds3,
                                                                                          opponentDefensiveRebounds = OpponentDefensiveRebounds3)]

### 5 Game ####

dat[, TeamOffensiveReboundPercentage5 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = TeamOffensiveRebounds5,
                                                                                          opponentDefensiveRebounds = OpponentDefensiveRebounds5)]

### 10 Game ####

dat[, TeamOffensiveReboundPercentage10 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = TeamOffensiveRebounds10,
                                                                                          opponentDefensiveRebounds = OpponentDefensiveRebounds10)]

## Total Rebound Pct ####

### Season ####

dat[, TeamTotalReboundPercentageToDate := calculate_team_total_rebound_percentage(teamTotalRebounds = TeamTotalReboundsToDate,
                                                                                  opponentTotalRebounds = OpponentTotalReboundsToDate)]

### 1 Game ####

dat[, TeamTotalReboundPercentage1 := calculate_team_total_rebound_percentage(teamTotalRebounds = TeamTotalRebounds1,
                                                                                  opponentTotalRebounds = OpponentTotalRebounds1)]

### 3 Game ####

dat[, TeamTotalReboundPercentage3 := calculate_team_total_rebound_percentage(teamTotalRebounds = TeamTotalRebounds3,
                                                                                  opponentTotalRebounds = OpponentTotalRebounds3)]

### 5 Game ####

dat[, TeamTotalReboundPercentage5 := calculate_team_total_rebound_percentage(teamTotalRebounds = TeamTotalRebounds5,
                                                                                  opponentTotalRebounds = OpponentTotalRebounds5)]

### 10 Game ####

dat[, TeamTotalReboundPercentage10 := calculate_team_total_rebound_percentage(teamTotalRebounds = TeamTotalRebounds10,
                                                                                  opponentTotalRebounds = OpponentTotalRebounds10)]

## Steal Pct ####

### Season ####

dat[, TeamStealPercentageToDate := calculate_team_steal_percentage(steals = TeamStealsToDate, opponentPossessions = PossessionsToDate)]

### 1 Game ####

dat[, TeamStealPercentage1 := calculate_team_steal_percentage(steals = TeamSteals1, opponentPossessions = Possessions1)]

### 3 Game ####

dat[, TeamStealPercentage3 := calculate_team_steal_percentage(steals = TeamSteals3, opponentPossessions = Possessions3)]

### 5 Game ####

dat[, TeamStealPercentage5 := calculate_team_steal_percentage(steals = TeamSteals5, opponentPossessions = Possessions5)]

### 10 Game ####

dat[, TeamStealPercentage10 := calculate_team_steal_percentage(steals = TeamSteals10, opponentPossessions = Possessions10)]

## Turnover Pct ####

### Season ####

dat[, TeamTurnoverPercentageToDate := calculate_team_turnover_percentage(
  teamTurnovers = TeamTurnoversToDate,
  teamFieldGoalAttempts = TeamFieldGoalsAttemptedToDate,
  teamFreeThrowAttempts = TeamFreeThrowsAttemptedToDate
)]

### 1 Game ####

dat[, TeamTurnoverPercentage1 := calculate_team_turnover_percentage(
  teamTurnovers = TeamTurnovers1,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted1,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted1
)]

### 3 Game ####

dat[, TeamTurnoverPercentage3 := calculate_team_turnover_percentage(
  teamTurnovers = TeamTurnovers3,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted3,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted3
)]

### 5 Game ####

dat[, TeamTurnoverPercentage5 := calculate_team_turnover_percentage(
  teamTurnovers = TeamTurnovers5,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted5,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted5
)]

### 10 Game ####

dat[, TeamTurnoverPercentage10 := calculate_team_turnover_percentage(
  teamTurnovers = TeamTurnovers10,
  teamFieldGoalAttempts = TeamFieldGoalsAttempted10,
  teamFreeThrowAttempts = TeamFreeThrowsAttempted10
)]

## Offensive Efficiency ####

### Season ####

dat[, TeamOffensiveEfficiencyToDate := calculate_offensive_efficiency(teamPoints = TeamPointsToDate, possessions = PossessionsToDate)]

### 1 Game ####

dat[, TeamOffensiveEfficiency1 := calculate_offensive_efficiency(teamPoints = TeamPoints1, possessions = Possessions1)]

### 3 Game ####

dat[, TeamOffensiveEfficiency3 := calculate_offensive_efficiency(teamPoints = TeamPoints3, possessions = Possessions3)]

### 5 Game ####

dat[, TeamOffensiveEfficiency5 := calculate_offensive_efficiency(teamPoints = TeamPoints5, possessions = Possessions5)]

### 10 Game ####

dat[, TeamOffensiveEfficiency10 := calculate_offensive_efficiency(teamPoints = TeamPoints10, possessions = Possessions10)]

# Opponent Advanced Stats ####

## Offensive Efficiency ####

### Season ####

dat[, OpponentDefensiveEfficiencyToDate := calculate_offensive_efficiency(teamPoints = OpponentPointsToDate, possessions = PossessionsToDate)]

### 1 Game ####

dat[, OpponentDefensiveEfficiency1 := calculate_offensive_efficiency(teamPoints = OpponentPoints1, possessions = Possessions1)]

### 3 Game ####

dat[, OpponentDefensiveEfficiency3 := calculate_offensive_efficiency(teamPoints = OpponentPoints3, possessions = Possessions3)]

### 5 Game ####

dat[, OpponentDefensiveEfficiency5 := calculate_offensive_efficiency(teamPoints = OpponentPoints5, possessions = Possessions5)]

### 10 Game ####

dat[, OpponentDefensiveEfficiency10 := calculate_offensive_efficiency(teamPoints = OpponentPoints10, possessions = Possessions10)]

## Turnover Pct ####

### Season ####

dat[, OpponentTurnoverPercentageToDate := calculate_team_turnover_percentage(
  teamTurnovers = OpponentTurnoversToDate,
  teamFieldGoalAttempts = OpponentFieldGoalsAttemptedToDate,
  teamFreeThrowAttempts = OpponentFreeThrowsAttemptedToDate
)]

### 1 Game ####

dat[, OpponentTurnoverPercentage1 := calculate_team_turnover_percentage(
  teamTurnovers = OpponentTurnovers1,
  teamFieldGoalAttempts = OpponentFieldGoalsAttempted1,
  teamFreeThrowAttempts = OpponentFreeThrowsAttempted1
)]

### 3 Game ####

dat[, OpponentTurnoverPercentage3 := calculate_team_turnover_percentage(
  teamTurnovers = OpponentTurnovers3,
  teamFieldGoalAttempts = OpponentFieldGoalsAttempted3,
  teamFreeThrowAttempts = OpponentFreeThrowsAttempted3
)]

### 5 Game ####

dat[, OpponentTurnoverPercentage5 := calculate_team_turnover_percentage(
  teamTurnovers = OpponentTurnovers5,
  teamFieldGoalAttempts = OpponentFieldGoalsAttempted5,
  teamFreeThrowAttempts = OpponentFreeThrowsAttempted5
)]

### 10 Game ####

dat[, OpponentTurnoverPercentage10 := calculate_team_turnover_percentage(
  teamTurnovers = OpponentTurnovers10,
  teamFieldGoalAttempts = OpponentFieldGoalsAttempted10,
  teamFreeThrowAttempts = OpponentFreeThrowsAttempted10
)]

## Assist Percentage ####

### Season ####

dat[, OpponentAssistPercentageToDate := calculate_team_assist_percentage(teamAssists = OpponentAssistsToDate, teamFieldGoalsMade = OpponentFieldGoalsMadeToDate)]

### 1 Game ####

dat[, OpponentAssistPercentage1 := calculate_team_assist_percentage(teamAssists = OpponentAssists1, teamFieldGoalsMade = OpponentFieldGoalsMade1)]

### 3 Game ####

dat[, OpponentAssistPercentage3 := calculate_team_assist_percentage(teamAssists = OpponentAssists3, teamFieldGoalsMade = OpponentFieldGoalsMade3)]

### 5 Game ####

dat[, OpponentAssistPercentage5 := calculate_team_assist_percentage(teamAssists = OpponentAssists5, teamFieldGoalsMade = OpponentFieldGoalsMade5)]

### 10 Game ####

dat[, OpponentAssistPercentage10 := calculate_team_assist_percentage(teamAssists = OpponentAssists10, teamFieldGoalsMade = OpponentFieldGoalsMade10)]

## Block Percentage ####

### Season ####

dat[, OpponentBlockPercentageToDate := calculate_team_block_percentage(
  teamBlocks = OpponentBlocksToDate,
  opponentFieldGoalAttempt = TeamFieldGoalsAttemptedToDate,
  opponentThreePointAttempt = TeamThreePointersAttemptedToDate
)]

### 1 Game ####

dat[, OpponentBlockPercentage1 := calculate_team_block_percentage(
  teamBlocks = OpponentBlocks1,
  opponentFieldGoalAttempt = TeamFieldGoalsAttempted1,
  opponentThreePointAttempt = TeamThreePointersAttempted1
)]

### 3 Game ####

dat[, OpponentBlockPercentage3 := calculate_team_block_percentage(
  teamBlocks = OpponentBlocks3,
  opponentFieldGoalAttempt = TeamFieldGoalsAttempted3,
  opponentThreePointAttempt = TeamThreePointersAttempted3
)]

### 5 Game ####

dat[, OpponentBlockPercentage5 := calculate_team_block_percentage(
  teamBlocks = OpponentBlocks5,
  opponentFieldGoalAttempt = TeamFieldGoalsAttempted5,
  opponentThreePointAttempt = TeamThreePointersAttempted5
)]

### 10 Game ####

dat[, OpponentBlockPercentage10 := calculate_team_block_percentage(
  teamBlocks = OpponentBlocks10,
  opponentFieldGoalAttempt = TeamFieldGoalsAttempted10,
  opponentThreePointAttempt = TeamThreePointersAttempted10
)]

## Defensive Rebound Percentage ####

### Season ####

dat[, OpponentDefensiveReboundPercentageToDate := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = OpponentDefensiveReboundsToDate,
                                                                                          opponentOffensiveRebounds = TeamOffensiveReboundsToDate)]

### 1 Game ####

dat[, OpponentDefensiveReboundPercentage1 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = OpponentDefensiveRebounds1,
                                                                                     opponentOffensiveRebounds = TeamOffensiveRebounds1)]

### 3 Game ####

dat[, OpponentDefensiveReboundPercentage3 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = OpponentDefensiveRebounds3,
                                                                                     opponentOffensiveRebounds = TeamOffensiveRebounds3)]

### 5 Game ####

dat[, OpponentDefensiveReboundPercentage5 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = OpponentDefensiveRebounds5,
                                                                                     opponentOffensiveRebounds = TeamOffensiveRebounds5)]

### 10 Game ####

dat[, OpponentDefensiveReboundPercentage10 := calculate_team_defensive_rebound_percentage(teamDefensiveRebounds = OpponentDefensiveRebounds10,
                                                                                      opponentOffensiveRebounds = TeamOffensiveRebounds10)]

## Offensive Rebound Pct ####

### Season ####

dat[, OpponentOffensiveReboundPercentageToDate := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = OpponentOffensiveReboundsToDate,
                                                                                          opponentDefensiveRebounds = TeamDefensiveReboundsToDate)]
### 1 Game ####

dat[, OpponentOffensiveReboundPercentage1 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = OpponentOffensiveRebounds1,
                                                                                     opponentDefensiveRebounds = TeamDefensiveRebounds1)]

### 3 Game ####

dat[, OpponentOffensiveReboundPercentage3 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = OpponentOffensiveRebounds3,
                                                                                     opponentDefensiveRebounds = TeamDefensiveRebounds3)]

### 5 Game ####

dat[, OpponentOffensiveReboundPercentage5 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = OpponentOffensiveRebounds5,
                                                                                     opponentDefensiveRebounds = TeamDefensiveRebounds5)]

### 10 Game ####

dat[, OpponentOffensiveReboundPercentage10 := calculate_team_offensive_rebound_percentage(teamOffensiveRebounds = OpponentOffensiveRebounds10,
                                                                                      opponentDefensiveRebounds = TeamDefensiveRebounds10)]

## Total Rebound Pct ####

### Season ####

dat[, OpponentTotalReboundPercentageToDate := calculate_team_total_rebound_percentage(teamTotalRebounds = OpponentTotalReboundsToDate,
                                                                                  opponentTotalRebounds = TeamTotalReboundsToDate)]

### 1 Game ####

dat[, OpponentTotalReboundPercentage1 := calculate_team_total_rebound_percentage(teamTotalRebounds = OpponentTotalRebounds1,
                                                                             opponentTotalRebounds = TeamTotalRebounds1)]

### 3 Game ####

dat[, OpponentTotalReboundPercentage3 := calculate_team_total_rebound_percentage(teamTotalRebounds = OpponentTotalRebounds3,
                                                                             opponentTotalRebounds = TeamTotalRebounds3)]

### 5 Game ####

dat[, OpponentTotalReboundPercentage5 := calculate_team_total_rebound_percentage(teamTotalRebounds = OpponentTotalRebounds5,
                                                                             opponentTotalRebounds = TeamTotalRebounds5)]

### 10 Game ####

dat[, OpponentTotalReboundPercentage10 := calculate_team_total_rebound_percentage(teamTotalRebounds = OpponentTotalRebounds10,
                                                                              opponentTotalRebounds = TeamTotalRebounds10)]

## Steal Pct ####

### Season ####

dat[, OpponentStealPercentageToDate := calculate_team_steal_percentage(steals = OpponentStealsToDate, opponentPossessions = PossessionsToDate)]

### 1 Game ####

dat[, OpponentStealPercentage1 := calculate_team_steal_percentage(steals = OpponentSteals1, opponentPossessions = Possessions1)]

### 3 Game ####

dat[, OpponentStealPercentage3 := calculate_team_steal_percentage(steals = OpponentSteals3, opponentPossessions = Possessions3)]

### 5 Game ####

dat[, OpponentStealPercentage5 := calculate_team_steal_percentage(steals = OpponentSteals5, opponentPossessions = Possessions5)]

### 10 Game ####

dat[, OpponentStealPercentage10 := calculate_team_steal_percentage(steals = OpponentSteals10, opponentPossessions = Possessions10)]


# Model Fitting ####

Outcome <- "Points"

fieldsToIgnore <- setdiff(columnToSum, Outcome)

cutOffDate <- dat[, max(GameDate)] - dweeks()

trainDat <- dat[GameDate <= cutOffDate] %>% 
              .[, (fieldsToIgnore) := NULL]

testDat <- dat[GameDate > cutOffDate] %>% 
              .[, (fieldsToIgnore) := NULL]

pointModel <-
  nnet::nnet(
    formula = log(Points + 1) ~ .,
    data = trainDat,
    MaxNWts = 1e7,
    size = 20,
    skip = TRUE, 
    linout = TRUE, 
    maxit = 1000
  )

trainDat[, PredictedPoints := exp(predict(pointModel, newdata = .SD)) - 1]
trainDat[, Residual := Points - PredictedPoints]
testDat[, PredictedPoints := exp(predict(pointModel, newdata = .SD)) - 1]
testDat[, Residual := Points - PredictedPoints]

ggplot(data = trainDat, mapping = aes(x = Points, y = PredictedPoints)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", size = 2)

ggplot(data = trainDat, mapping = aes(x = Points, y = Residual)) +
  geom_point()

shap <-
  kernelshap(pointModel, X = trainDat[,-"Points"], bg_X = trainDat[sample(1:nrow(trainDat), size = 200, replace = FALSE)])
sv <- shapviz(shap)

sv_importance(sv)
sv_importance(sv, kind = "bee")
sv_dependence(sv, "Species", color_var = "auto")

# Single observations
sv_waterfall(sv, 1)
sv_force(sv, 1)