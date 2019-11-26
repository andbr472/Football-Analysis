#Possible league values: La_liga, EPL, Bundesliga, Serie_A, Ligue_1
#Possible nodes value: 
# match: 2 = shotsData, 3 = rostersData
# player: 2 = groupsData, 3 = minMaxPlayerStats, 4 = shotsData, 5 = matchesData
# team: 2 = datesData, 3 = statisticsData, 4 = playersData
# league: 2 = datesData, 3  = teamsData, 4 = playersData

require(xml2)
require(dplyr)
require(rvest)
require(jsonlite)
require(purrr)
require(stringi)
require(stringr)

get_data <- function(team = NULL, year = "2019", player_id = NULL, match_id = NULL, league = NULL, node){
  
  if(!is.null(team)){
    url <- paste("https://understat.com/team/", team, "/", year, sep = "")
  }else if(!is.null(league)){
    url <- paste("https://understat.com/league/", league, "/", year, sep = "")
  }else if(!is.null(player_id)){
    url <- paste("https://understat.com/player/", player_id, sep = "")
  }else if(!is.null(match_id)){
    url <- paste("https://understat.com/match/", match_id, sep = "")
  }
  data_url <- read_html(url)
  data <- data_url %>% html_node("body") %>% xml_find_all("//script") 
  
  data1 <- data[node] %>% xml_text()
  data1 <- str_extract(data1, "'.*'")
  data1 <- gsub("\\", "", data1, fixed = T)
  data1 <- gsub("x7B", "{", data1, fixed = T)
  data1 <- gsub("x7D", "}", data1, fixed = T)
  data1 <- gsub("x5B", "[", data1, fixed = T)
  data1 <- gsub("x5D", "]", data1, fixed = T)
  data1 <- gsub("x3A", ":", data1, fixed = T)
  data1 <- gsub("x20", " ", data1, fixed = T)
  data1 <- gsub("x2D", "-", data1, fixed = T)
  data1 <- gsub("x5Cu00e1", "á", data1, fixed = T)
  data1 <- gsub("x5Cu00e9", "é", data1, fixed = T)
  data1 <- gsub("x5Cu00f1", "ñ", data1, fixed = T)
  data1 <- gsub("x5Cu00fa", "ú", data1, fixed = T)
  data1 <- gsub("x5Cu00ed", "í", data1, fixed = T)
  data1 <- gsub("x5Cu00e3", "ã", data1, fixed = T)
  data1 <- gsub("x5Cu00c1", "Á", data1, fixed = T)
  data1 <- gsub("x5Cu00f2", "ò", data1, fixed = T)
  data1 <- gsub("x5Cu00dc", "Ü", data1, fixed = T)
  data1 <- gsub("x5Cu00f3", "ó", data1, fixed = T)
  data1 <- gsub("x5Cu00f4", "ô", data1, fixed = T)
  data1 <- gsub("x5Cu00fc", "ü", data1, fixed = T)
  data1 <- gsub("x5Cu00e0", "à", data1, fixed = T)
  data1 <- gsub("x26x23039x3B", "' ", data1, fixed = T)
  data1 <- gsub("x22", "\"", data1, fixed = T)
  data1 <- gsub("'", "", data1, fixed = T)
  
  res <- fromJSON(data1)
  
  return(res)
}


list_league <- function(data){
  for(i in 1:length(data)){
    df_single_team <- data[[i]]$history[,c(1:5, 8:19)] %>% 
      mutate(ppda_att = data[[i]]$history$ppda[1][,1],
             ppda_def = data[[i]]$history$ppda[2][,1],
             ppda_allowed_att = data[[i]]$history$ppda_allowed[1][,1],
             ppda_allowed_def = data[[i]]$history$ppda_allowed[2][,1],
             Team = data[[i]]$title,
             team_id = data[[i]]$id,
             Game_Number = row_number()
      ) 
    if(i == 1){
      df_teams <- df_single_team
    }else{
      df_teams <- bind_rows(df_teams, df_single_team)
    }
    
  }
  colnames(df_teams) <- trimws(colnames(df_teams))
  df_teams <- mutate(df_teams, Team = trimws(Team))
  return(df_teams)
}

list_team <- function(data, stats){
  
}

list_player <- function(data, stats){
  
}


list_match <- function(data, stats){
  
}