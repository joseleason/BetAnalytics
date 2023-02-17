library(tidyverse)
library(tidymodels)
library(data.table)

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

# dat[, (timeFields) := lapply(.SD,  lubridate::ms), .SDcols = timeFields]
dat[, (dateFields) := lapply(.SD,  lubridate::as_date), .SDcols = dateFields]
dat[, (numericFields) := lapply(.SD,  as.numeric), .SDcols = numericFields]
dat[, (factorFields) := lapply(.SD,  as.factor), .SDcols = factorFields]

dat[, c("Minutes", "Seconds") := tstrsplit(MP, split = ":", fixed = TRUE)]
dat[, Minutes := as.numeric(Minutes)]
dat[, Seconds := as.numeric(Seconds)]
dat[, MinutesPlayed := Minutes + Seconds / 60] %>% 
  .[, c("MP", "Minutes", "Seconds") := NULL]

dat <- dat[!is.na(MinutesPlayed)]
dat[, `FG%` := FG / FGA]
dat[, `3P%` := `3P` / `3PA`]
dat[, `FT%` := FT / FTA]

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

columnToShift <- c('PlusMinus',
                   'UsagePercentage',
                   'OffensiveRating',
                   'DefensiveRating',
                   'BoxPlusMinus')

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
    paste0("Total", columnToSum, "ToDate") := lapply(.SD, function(x)
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
        paste0("TeamTotal", columnToSum, "ToDate") := lapply(.SD, function(x)
          cumsum(x) %>% shift(n = 1, type = "lag")),
        .SDcols = columnToSum,
        .(Team)]

teamDat[,(columnToSum) := NULL]

# Team Recency Stats ####
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
        paste0("OpponentTotal", columnToSum, "ToDate") := lapply(.SD, function(x)
          cumsum(x) %>% shift(n = 1, type = "lag")),
        .SDcols = columnToSum,
        .(Opponent)]

opponentDat[,(columnToSum) := NULL]

# Merge Datasets ####
dat <- dat[teamDat,
           on = .(Team, GameDate)] %>%
  .[opponentDat,
    on = .(Opponent, GameDate)]
