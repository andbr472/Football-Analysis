require(ggsoccer)
require(ggplot2)
require(ggthemes)
require(patchwork)

source("get_data_functions.R")

theme_set(theme_light(base_size = 15))

Piatek_single_game <- get_data(player_id = 7090, node = 5)
Piatek_single_game <- Piatek_single_game %>% 
  mutate(xg90  = pmap_dbl(list(time, xG),
                          function(x, y){
                            res <- (sum(as.double(y)) / sum(as.integer(x)))*90
                            return(res)
                          }),
         npxg90 =  pmap_dbl(list(time, npxG),
                            function(x, y){
                              res <- (sum(as.double(y)) / sum(as.integer(x)))*90
                              return(res)
                            }))
Piatek_single_game_Genoa <- Piatek_single_game %>% filter(date < "2019-01-26") 
game_id_Genoa_single <- Piatek_single_game_Genoa %>% 
  group_by(date) %>% 
  summarise(count = n()) %>% 
  mutate(game = row_number()) %>% 
  select(-count)
Piatek_single_game_Genoa <- left_join(Piatek_single_game_Genoa, game_id_Genoa_single, by = "date")

Piatek_single_game_Milan <- Piatek_single_game %>% filter(date >= "2019-01-26") 
game_id_Milan_single <- Piatek_single_game_Milan %>% 
  group_by(season, date) %>% 
  summarise(count = n()) %>% 
  mutate(game = row_number()) %>% 
  select(-count)
Piatek_single_game_Milan <- left_join(Piatek_single_game_Milan, game_id_Milan_single, by = "date")

p1 <- ggplot(Piatek_single_game_Milan) + 
  geom_line(data = Piatek_single_game_Milan %>% filter(season.x == 2018) %>% arrange(game), 
            aes(x = game, y = cumsum(xg90), colour = "Milan 18/19"), size = 2) + 
  geom_line(data = Piatek_single_game_Milan %>% filter(season.x == 2019) %>% arrange(game), 
            aes(x = game, y = cumsum(xg90), colour = "Milan 19/20"), size = 2) +
  geom_line(data = Piatek_single_game_Genoa %>% arrange(game), 
            aes(x = game, y = cumsum(xg90), colour = "Genoa 18/19"), size = 2) +
  scale_x_continuous(breaks = seq(1:19)) +
  scale_y_continuous(breaks = 0:11) +
  scale_colour_manual(name="", values=c(`Milan 18/19` = "red", `Milan 19/20` = "blue", `Genoa 18/19` = "black")) +
  labs(title = "Krzysztof Piatek",
       subtitle = "",
       y = "Cumulative xG per 90 minutes",
       x = "Game played"#,
       #caption = "Visualisation by: Andrea Bruzzone"
       ) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom") 

p2 <- ggplot(Piatek_single_game_Milan) + 
  geom_line(data = Piatek_single_game_Milan %>% filter(season.x == 2018) %>% arrange(game), 
            aes(x = game, y = cumsum(npxg90), colour = "Milan 18/19"), size = 2) + 
  geom_line(data = Piatek_single_game_Milan %>% filter(season.x == 2019) %>% arrange(game), 
            aes(x = game, y = cumsum(npxg90), colour = "Milan 19/20"), size = 2)  +
  geom_line(data = Piatek_single_game_Genoa %>% arrange(game), 
            aes(x = game, y = cumsum(npxg90), colour = "Genoa 18/19"), size = 2) +
  scale_x_continuous(breaks = seq(1:19)) +
  scale_y_continuous(breaks = 0:11) +
  scale_colour_manual(name="", values=c(`Milan 18/19` = "red", `Milan 19/20` = "blue", `Genoa 18/19` = "black")) +
  labs(title = "Krzysztof Piatek",
       subtitle = "",
       y = "Cumulative non penalty xG per 90 minutes",
       x = "Game played",
       caption = "Visualisation by: Andrea Bruzzone") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom") 

p1 + p2


Piatek <- get_data(player_id = 7090, node = 4)
Piatek <- Piatek %>% 
  mutate(X = as.double(X),
         Y = as.double(Y),
         xG = as.double(xG),
         date = as.Date(date),
         season = as.factor(season))

Piatek_Milan <- Piatek %>% filter(date >= "2019-01-26") 
Piatek_Milan18 <- Piatek_Milan %>% filter(season == 2018) %>% arrange(date) %>% mutate(team = "Milan18")
Piatek_Milan19 <- Piatek_Milan %>% filter(season == 2019) %>% arrange(date) %>% mutate(team = "Milan19")

games19 <- length(unique(Piatek_Milan19$match_id))

Piatek_Milan18 <- Piatek_Milan18 %>% filter(match_id %in% unique(Piatek_Milan18$match_id)[1:games19])

Piatek18 <- Piatek %>% 
  filter(season == 2018) %>% 
  arrange(date) %>% 
  filter(match_id %in% unique(Piatek$match_id)[1:games19]) %>% 
  mutate(team = "Genoa")

Piatek_Milan19_for_plot <- Piatek_Milan %>% filter(season == 2019) %>% arrange(date) %>% mutate(team = "Milan")
combined_Piatek <- bind_rows(Piatek18, Piatek_Milan19_for_plot) 

p3 <- ggplot(combined_Piatek) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(X*100, y =  100- (Y*100), fill = team, size = xG),
              shape = 21, colour = 'red') +
  theme_pitch() +
  coord_flip(xlim = c(49, 101), ylim = c(-1, 101)) +
  scale_fill_manual(name = "", values = c("blue", "black")) + 
  scale_size_continuous(name = "", breaks  = NULL) +
  guides(fill = guide_legend(title = "", override.aes = list(size = 6))) +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        plot.title = element_text(colour = "white", size = 18),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 10),
        plot.subtitle = element_text(colour = "white", size = 12)#,
        #plot.caption = element_text(colour = "white", size = 12)
        ) + 
  labs(title = "Krzysztof Piatek shotmap",
       subtitle = "Comparing first 13 matches with Genoa (season 18/19) vs current season with Milan (13 matches)"#, 
       #caption = "Visualisation by: Andrea Bruzzone"
       ) 
  
combined_Piatek_Milan <- bind_rows(Piatek_Milan18, Piatek_Milan19)  

p4 <- ggplot(combined_Piatek_Milan) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(X*100, y =  100- (Y*100), fill = team, size = xG),
             shape = 21, colour = 'red') +
  theme_pitch() +
  coord_flip(xlim = c(49, 101), ylim = c(-1, 101)) +
  scale_fill_manual(name = "", values = c("white", "black")) + 
  scale_size_continuous(name = "", breaks  = NULL) +
  guides(fill = guide_legend(title = "", override.aes = list(size = 6))) +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        plot.title = element_text(colour = "white", size = 18),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 10),
        plot.subtitle = element_text(colour = "white", size = 12),
        plot.caption = element_text(colour = "white", size = 12)
  ) + 
  labs(title = "Krzysztof Piatek shotmap",
       subtitle = "Comparing first 13 matches with Milan (season 18/19) vs current season (13 matches)", 
       caption = "Visualisation by: Andrea Bruzzone")

p3 / p4




Milan_game19 <- get_data(league = "Serie_A", node = 3)
Milan_game18 <- get_data(league = "Serie_A", node = 3, year = 2018)
Milan_game19 <- list_league(Milan_game19)
Milan_game18 <- list_league(Milan_game18)
Milan_game19 <- Milan_game19 %>% filter(Team == "AC Milan")
Milan_game18 <- Milan_game18 %>% filter(Team == "AC Milan")
Milan_games <- bind_rows(Milan_game18, Milan_game19) %>% mutate(date = as.Date(date))
Piatek_single_game_Milan <- Piatek_single_game_Milan %>% mutate(date = as.Date(date))
milan_Piatek_together <- left_join(Piatek_single_game_Milan, Milan_games, by = "date") 
milan_Piatek_together <- milan_Piatek_together %>% mutate(xG.x = as.double(xG.x),
                                                          xG.y = as.double(xG.y))
ggplot(milan_Piatek_together, aes(x=date)) + 
  geom_area(aes(y=xG.y, fill="psavert")) + 
  geom_area(aes(y=xG.x, fill="uempmed")) + 
  labs(title="Area Chart of Returns Percentage", 
       subtitle="From Wide Data format", 
       caption="Source: Economics", 
       y="Returns %") +  # title and caption
  scale_fill_manual(name="", 
                    values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(panel.grid.minor = element_blank()) 

#----------------------------

ggplot(Piatek) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(aes(x = X*100, y =  100- (Y*100), fill = season, color = season, size = xG), alpha = .7) +
  theme_pitch() +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        title = element_text(colour = "white"),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 10),
        plot.caption = element_text(size = 12)) + 
  coord_flip(xlim = c(49, 101),
             ylim = c(-1, 101)) +
  labs(title = "Simple shotmap",
      subtitle = "ggsoccer example", 
      caption = "Visualisation by: Andrea Bruzzone") +
  scale_color_discrete(name = "") + 
  scale_fill_discrete(name = "") + 
  scale_size_continuous(name = "", breaks  = NULL) + 
  guides(colour = guide_legend(title = "", override.aes = list(size = 6)))