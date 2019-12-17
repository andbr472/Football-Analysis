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
  data1 <- gsub("x2D", "-", data1, fixed = T)	
  data1 <- gsub("x3C", "<", data1, fixed = T)	
  data1 <- gsub("x2B1", "+", data1, fixed = T)	
  data1 <- gsub("x3E", ">", data1, fixed = T)
  data1 <- gsub("x20", " ", data1, fixed = T)	
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
  data1 <- gsub("x5Cu00e8", "è", data1, fixed = T)
  data1 <- gsub("x5Cu00ef", "ï", data1, fixed = T)
  data1 <- gsub("x5Cu00eb", "ë", data1, fixed = T)
  data1 <- gsub("x5Cu00c9", "É", data1, fixed = T)
  data1 <- gsub("x5Cu00e7", "ç", data1, fixed = T)
  data1 <- gsub("x5Cu00d1", "Ñ", data1, fixed = T)
  data1 <- gsub("x5Cu00d3", "Ó", data1, fixed = T)
  data1 <- gsub("x5Cu00cd", "Í", data1, fixed = T)
  data1 <- gsub("x5Cu00f6", "ö", data1, fixed = T)
  data1 <- gsub("x5Cu00ea", "ê", data1, fixed = T)
  data1 <- gsub("x5Cu00e4", "ä", data1, fixed = T)
  data1 <- gsub("x5Cu00d6", "Ö", data1, fixed = T)
  data1 <- gsub("x5Cu00e5", "å", data1, fixed = T)
  data1 <- gsub("x5Cu00df", "ß", data1, fixed = T)
  data1 <- gsub("x26x23039x3B", "' ", data1, fixed = T)	
  data1 <- gsub("x22", "\"", data1, fixed = T)	
  data1 <- gsub("'", "", data1, fixed = T)	
  
  res <- fromJSON(data1)	
  
  if(!is.null(team)){	
    df <- list_team(res, node)	
  }else if(!is.null(league)){	
    df <- list_league(res, node)
  }else if(!is.null(player_id)){	
    df <-res #list_player(res, node)	
  }else if(!is.null(match_id)){	
    df <- list_match(res, node)	
  }	
  
  return(df)	
}	



# LEAGUE: 2 = datesData, 3  = teamsData, 4 = playersData (already DF)
list_league <- function(data, node){
  if(node == 2){
    df <- 1:nrow(data) %>% 
      map_df(function(x) bind_cols(data[x, 1:2], data$h[x,], data$a[x, ], data$goals[x,], 
                                   data$xG[x,], data[x,7, drop = F], data$forecast[x,])) 
    colnames(df) <- c("id", "isResult", "h_id", "h_team", "h_short",
                      "a_id", "a_team", "a_short", "h_goal", "a_goal",
                      "h_xG", "a_xG", "datetime", "w", "d", "l")
    
    return(df)
  }else if(node == 3){
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
    
    # df_teams <- 1:length(data) %>% 
    #   bind_rows(function(i) data[[i]]$history[,c(1:5, 8:19), drop = F] %>% 	
    #   mutate(ppda_att = data[[i]]$history$ppda[1][,1],	
    #          ppda_def = data[[i]]$history$ppda[2][,1],	
    #          ppda_allowed_att = data[[i]]$history$ppda_allowed[1][,1],	
    #          ppda_allowed_def = data[[i]]$history$ppda_allowed[2][,1],	
    #          Team = data[[i]]$title,	
    #          team_id = data[[i]]$id,	
    #          Game_Number = row_number()))
    colnames(df_teams) <- trimws(colnames(df_teams))	
    df_teams <- mutate(df_teams, Team = trimws(Team))	
    return(df_teams)	
  }else{
    return(data)
  }

}	



## TEAM: 2 = datesData, 3 = statisticsData, 4 = playersData	(already DF)
list_team <- function(data, node){	
  if(node == 2){
    df <- 1:nrow(data) %>% 
      map_df(function(x) bind_cols(data[x, 1:3], data$h[x,], data$a[x, ], data$goals[x,], 
                                   data$xG[x,], data[x,8, drop = F], data$forecast[x,], data[x,10, drop = F])) 
    colnames(df) <- c("id", "isResult", "side", "h_id", "h_team", "h_short",
                      "a_id", "a_team", "a_short", "h_goal", "a_goal",
                      "h_xG", "a_xG", "datetime", "w", "d", "l", "result")
    
    return(df)
  }else if(node == 3){
    n_result <- length(data$result)
    stat <- data.frame(x = names(data$result), stringsAsFactors = F)
    result <- 1:(n_result) %>% purrr::map_df(function(x) bind_rows(c(unlist(data$result[[x]]))))
    result <- bind_cols(stat, result)
    
    n_attackSpeed <- length(data$attackSpeed)
    attackSpeed <- 1:n_attackSpeed %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$attackSpeed[[x]]))))
    
    n_shotzone <- length(data$shotZone)
    shotzone <- 1:n_shotzone %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$shotZone[[x]]))))
    
    n_timing <- length(data$timing)
    timing <- 1:n_timing %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$timing[[x]]))))
    
    n_gameState <- length(data$gameState)
    gameState <- 1:n_gameState %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$gameState[[x]]))))
    
    n_formation <- length(data$formation)
    formation <- 1:n_formation %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$formation[[x]]))))
    
    n_situation <- length(data$situation)
    stat <- data.frame(x = names(data$situation), stringsAsFactors = F)
    situation <- 1:n_situation %>% 
      purrr::map_df(function(x) bind_rows(c(unlist(data$situation[[x]]))))
    situation <- bind_cols(stat, situation)
    
    res <- list(result = result, attackSpeed = attackSpeed, shotzone = shotzone, 
                timing = timing, gameState = gameState, formation = formation, situation = situation)
    return(res)
  }else{
    return(data)
  }
}	


# PLAYER: 2 = groupsData, 3 = minMaxPlayerStats, 4 = shotsData (already DF), 5 = matchesData (already DF)
list_player <- function(data, node){	
  
}	





# MATCH: 2 = shotsData, 3 = rostersData	
list_match <- function(data, node){	
  if(node == 2){
    df1 <- data$h %>% mutate(Team = h_team)
    df2 <- data$a %>% mutate(Team = a_team)
    df <- bind_rows(df1, df2)
    
    return(df)
  }else{
    n_h <- length(data$h)
    team_h <- 1:n_h  %>% purrr::map_df(function(x) bind_rows(c(unlist(data$h[[x]]))))
    n_a <- length(data$a)
    team_a <- 1:n_a %>% purrr::map_df(function(x) bind_rows(c(unlist(data$a[[x]]))))
    
    teams <- bind_rows(team_h, team_a) %>% distinct()
    return(teams)
  }
} 


