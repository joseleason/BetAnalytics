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

nbaDat <- lapply(nbaGameURLs, function(gameURL){
  
  # browser()
  gameDate <- substr(gameURL,1,8) %>% lubridate::ymd()
  gameURL <- paste0("https://www.basketball-reference.com/boxscores/",gameURL)
  
  sessionNBA <- polite::bow(gameURL)
  
  boxscore <- scrape(sessionNBA)
  
  teams <- boxscore %>%
    html_elements(".table_wrapper") %>%
    lapply(., function(node) {
      html_attr(x = node, name = "id") %>%
        gsub(pattern = "^all_box-", replacement = "") %>%
        substr(start = 1, stop = 3)
    }) %>%
    unique() %>%
    unlist() %>%
    setdiff("all")
  
  if(length(teams) != 2)
    next
  
  awayTeam <- teams[1]
  homeTeam <- teams[2]
  
  # Away Box Score Basic
  awayBoxsoreBasic <- boxscore %>%
    html_element(paste0("#all_box-",awayTeam,"-game-basic")) %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    data.table()
  
  names(awayBoxsoreBasic) <- awayBoxsoreBasic[1] %>% as.character()
  names(awayBoxsoreBasic)[1] <- "Player"
  
  awayBoxsoreBasic <- awayBoxsoreBasic[!Player %in% c("Starters","Reserves","Team Totals")]
  awayBoxsoreBasic[, Team := awayTeam]
  awayBoxsoreBasic[, Location := "Away"]
  awayBoxsoreBasic[, Opponent := homeTeam]
  awayBoxsoreBasic[, GameDate := gameDate]
  
  # Away Box Score Advanced
  awayBoxsoreAdv <- boxscore %>%
    html_element(paste0("#all_box-",awayTeam,"-game-advanced")) %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    data.table()
  
  names(awayBoxsoreAdv) <- awayBoxsoreAdv[1] %>% as.character()
  names(awayBoxsoreAdv)[1] <- "Player"
  
  awayBoxsoreAdv <- awayBoxsoreAdv[!Player %in% c("Starters","Reserves","Team Totals")]
  
  # Home Box Score Basic
  homeBoxsoreBasic <- boxscore %>%
    html_element(paste0("#all_box-",homeTeam,"-game-basic")) %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    data.table()
  
  names(homeBoxsoreBasic) <- homeBoxsoreBasic[1] %>% as.character()
  names(homeBoxsoreBasic)[1] <- "Player"
  
  homeBoxsoreBasic <- homeBoxsoreBasic[!Player %in% c("Starters","Reserves","Team Totals")]
  homeBoxsoreBasic[, Team := homeTeam]
  homeBoxsoreBasic[, Location := "Home"]
  homeBoxsoreBasic[, Opponent := awayTeam]
  homeBoxsoreBasic[, GameDate := gameDate]
  
  # Home Box Score Advanced
  homeBoxsoreAdv <- boxscore %>%
    html_element(paste0("#all_box-",homeTeam,"-game-advanced")) %>% 
    html_table(header = TRUE, trim = TRUE) %>% 
    data.table()
  
  names(homeBoxsoreAdv) <- homeBoxsoreAdv[1] %>% as.character()
  names(homeBoxsoreAdv)[1] <- "Player"
  
  homeBoxsoreAdv <- homeBoxsoreAdv[!Player %in% c("Starters","Reserves","Team Totals")]
  
  # Merge Tables
  awayBoxsore <- awayBoxsoreBasic[awayBoxsoreAdv[,-"MP"],
                                  on = .(Player)]
  
  homeBoxsore <- homeBoxsoreBasic[homeBoxsoreAdv[,-"MP"],
                                  on = .(Player)]
  
  boxscore <- rbindlist(list(homeBoxsore, awayBoxsore), fill = TRUE, use.names = TRUE)
  
  wait <- sessionNBA$delay
  
  Sys.sleep(wait)
  
  return(boxscore)
  
  })

nbaDat <- nbaDat %>% rbindlist()

saveRDS(object = nbaDat, file = paste0(getwd(),"/BoxscoreData",format(Sys.Date(), "%Y%m%d"),".RDS"))
