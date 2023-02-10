library(tidyverse)
library(tidymodels)
library(data.table)

dat <- readRDS(file = "BoxscoreData20230207.RDS") %>% data.table()

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
