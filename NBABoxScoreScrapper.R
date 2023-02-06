library(tidyverse)
library(tidymodels)
library(data.table)
library(rvest)
library(polite)

teamAbbreviations <- c(
  "ATL",
  "BOS",
  "CHO",
  "CHI",
  "CLE",
  "DAL",
  "DEN",
  "DET",
  "GSW",
  "HOU",
  "IND",
  "LAC",
  "LAL",
  "MEM",
  "MIA",
  "MIL",
  "MIN",
  "NOP",
  "NYK",
  "BRK",
  "OKC",
  "ORL",
  "PHI",
  "PHO",
  "POR",
  "SAC",
  "SAS",
  "TOR",
  "UTA",
  "WAS"
)

nbaGameURLs <- lapply(teamAbbreviations, function(team) {
  # browser()
  
  teamUrl <-
    paste0("https://www.basketball-reference.com/teams/",
           team,
           "/2023_games.html")
  
  sessionNBA <- polite::bow(teamUrl)
  
  gamesToPull <- scrape(sessionNBA) %>%
    html_element(".stats_table") %>%
    html_element("tbody") %>%
    as.character() %>%
    strsplit(split = "/boxscores/") %>%
    lapply(., function(x)
      substr(x, start = 1, stop = 17)) %>%
    unlist() %>%
    grep(pattern = "^\\d{9}", value = TRUE) %>%
    data.table(GameFiles = .) %>% 
    .[, GameDate := substr(GameFiles,1,8) %>% lubridate::ymd()] %>% 
    .[GameDate < Sys.Date(), GameFiles]
  
  wait <- sessionNBA$delay
  
  Sys.sleep(wait)
  
  return(gamesToPull)
  
}) %>% 
  unlist() %>% 
  unique()

nbaGameURLs <- paste0("https://www.basketball-reference.com/boxscores/",nbaGameURLs)

nbaDat <- lapply(nbaGameURLs, function(gameURL){
  
  # browser()
  
  sessionNBA <- polite::bow(gameURL)
  
  boxscore <- scrape(sessionNBA)
  
  awayTeam <- boxscore %>%
    html_elements(".table_wrapper") %>%
    .[[3]] %>%
    html_attr("id") %>%
    gsub(pattern = "^all_box-", replacement = "") %>%
    substr(start = 1, stop = 3)
  
  homeTeam <- boxscore %>%
    html_elements(".table_wrapper") %>%
    .[[11]] %>%
    html_attr("id") %>%
    gsub(pattern = "^all_box-", replacement = "") %>%
    substr(start = 1, stop = 3)
  
  awayBoxsore <- boxscore %>%
    html_elements(".table_wrapper") %>%
    .[[3]] %>% 
    html_elements(".table_container") %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    .[[1]] %>% 
    data.table()
  
  names(awayBoxsore) <- awayBoxsore[1] %>% as.character()
  names(awayBoxsore)[1] <- "Player"
  
  awayBoxsore <- awayBoxsore[!Player %in% c("Starters","Reserves","Team Totals")]
  awayBoxsore[, Team := awayTeam]
  
  homeBoxsore <- boxscore %>%
    html_elements(".table_wrapper") %>%
    .[[11]] %>% 
    html_elements(".table_container") %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    .[[1]] %>% 
    data.table()
  
  names(homeBoxsore) <- homeBoxsore[1] %>% as.character()
  names(homeBoxsore)[1] <- "Player"
  
  homeBoxsore <- homeBoxsore[!Player %in% c("Starters","Reserves","Team Totals")]
  homeBoxsore[, Team := homeTeam]
  
  boxscore <- rbind(awayBoxsore, homeBoxsore)
  
  wait <- sessionNBA$delay
  
  Sys.sleep(wait)
  
  return(boxscore)
  
  })
