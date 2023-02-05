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

nbaDat <- lapply(teamAbbreviations[1], function(team) {
  browser()
  
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
  
  return(gamesToPull)
  
})

%>%
  paste0("https://www.basketball-reference.com/boxscores/", .)




teamDat %>% html_elements("a")
session(teamUrl)

html_element("body") %>%
  html_element("div") %>% 
  html_element(".nba-stats-content-block") %>% 
  html_element("section")
%>% html_element('.Crom_table__p1iZz')

nbaDat %>% 
  html_element("body") %>% 
  html_element("div") %>% 
  html_element(".Layout_withSubNav__ByKRF") %>% 
  html_element(".Layout_mainContent__jXliI") %>% 
  html_element(".MaxWidthContainer_mwc__ID5AG") %>% 
  html_element(".Block_block__62M07.nba-stats-content-block") %>% 
  html_element(".Block_blockContent__6iJ_n") %>% 
  html_element(".StatsTableSkeleton_skeleton__ccF3w") %>% 
  html_element(".StatsTableSkeleton_pagination__gWVO8")

nbaDat %>% 
  html_element(css = ".GameBoxscoreTablePlayer_gbp__mPF20") %>% 
  html_element("div") %>% 
  html_element(".Layout_withSubNav__ByKRF") %>% 
  html_element(".Layout_mainContent__jXliI") %>% 
  html_element(".MaxWidthContainer_mwc__ID5AG") %>% html_attrs()
