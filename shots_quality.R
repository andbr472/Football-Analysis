library(grid)
library(ggplot2)
library(ggthemes)
library(ggrepel)

source("get_data_functions.R")

players_list_2018 <- get_data(league = "Bundesliga", year = 2018, node = 4)
players_id <- players_list_2018 %>% 
  mutate(shots = as.numeric(shots),
         goals = as.numeric(goals)) %>% 
  filter(shots >= 20) %>% 
  filter(goals >= 6) %>% 
  select(id, player_name)

missed_info <- data.frame(id = "", missed = 0, stringsAsFactors = F)
goal_info <- data.frame(id = "", goal = 0, stringsAsFactors = F)
for(i in players_id$id){
  player <- get_data(player_id = i, node = 4)
  player <- player %>% filter(season == 2018) %>% mutate(xG = as.double(xG))
  temp_info_missed <- player %>% 
    filter(situation != "Penalty") %>% 
    filter(result != "Goal") %>% 
    summarise(n = n(), sum = sum(xG), missed = sum / n)
  temp_df_missed <- data.frame(id = i, missed = temp_info_missed$missed, stringsAsFactors = F)
  missed_info <- bind_rows(missed_info, temp_df_missed)
  
  temp_info_goal <- player %>% 
    filter(situation != "Penalty") %>% 
    filter(result == "Goal") %>% 
    summarise(n = n(), sum = sum(xG), goal = sum / n)
  temp_df_goal <- data.frame(id = i, goal = temp_info_goal$goal, stringsAsFactors = F)
  goal_info <- bind_rows(goal_info, temp_df_goal)
  
}

missed_info <- missed_info[-1,]
goal_info <- goal_info[-1,]

complete_df <- left_join(players_id, missed_info, by = "id")
complete_df <- left_join(complete_df, goal_info, by = "id")

complete_df$player_name <- gsub("Fabio Quagliarella", "Quagliarella", complete_df$player_name)
complete_df$player_name <- gsub("Duván Zapata", "D. Zapata", complete_df$player_name)
complete_df$player_name <- gsub("Krzysztof Piatek", "Piatek", complete_df$player_name)
complete_df$player_name <- gsub("Cristiano Ronaldo", "C. Ronaldo", complete_df$player_name)
complete_df$player_name <- gsub("Arkadiusz Milik", "Milik", complete_df$player_name)
complete_df$player_name <- gsub("Leonardo Pavoletti", "Pavoletti", complete_df$player_name)
complete_df$player_name <- gsub("Dries Mertens", "Mertens", complete_df$player_name)
complete_df$player_name <- gsub("Andrea Petagna", "Petagna", complete_df$player_name)
complete_df$player_name <- gsub("Francesco Caputo", "Caputo", complete_df$player_name)
complete_df$player_name <- gsub("Andrea Belotti", "Belotti", complete_df$player_name)
complete_df$player_name <- gsub("Ciro Immobile", "Immobile", complete_df$player_name)
complete_df$player_name <- gsub("Josip Ilicic", "Ilicic", complete_df$player_name)
complete_df$player_name <- gsub("Stephan El Shaarawy", "El Shaarawy", complete_df$player_name)
complete_df$player_name <- gsub("Gregoire Defrel", "Defrel", complete_df$player_name)
complete_df$player_name <- gsub("Mauro Icardi", "Icardi", complete_df$player_name)
complete_df$player_name <- gsub("Lorenzo Insigne", "Insigne", complete_df$player_name)
complete_df$player_name <- gsub("Rodrigo de Paul", "De Paul", complete_df$player_name)
complete_df$player_name <- gsub("Edin Dzeko", "Dzeko", complete_df$player_name)
complete_df$player_name <- gsub("Mario Mandzukic", "Mandzukic", complete_df$player_name)
complete_df$player_name <- gsub("Roberto Inglese", "Inglese", complete_df$player_name)
complete_df$player_name <- gsub("Ivan Perisic", "Perisic", complete_df$player_name)
complete_df$player_name <- gsub("Aleksandar Kolarov", "Kolarov", complete_df$player_name)
complete_df$player_name <- gsub("Domenico Berardi", "Berardi", complete_df$player_name)
complete_df$player_name <- gsub("Felipe Caicedo", "Caicedo", complete_df$player_name)
complete_df$player_name <- gsub("Riccardo Orsolini", "Orsolini", complete_df$player_name)
complete_df$player_name <- gsub("Federico Santander", "Santander", complete_df$player_name)
complete_df$player_name <- gsub("Marco Benassi", "Benassi", complete_df$player_name)
complete_df$player_name <- gsub("Federico Chiesa", "Chiesa", complete_df$player_name)
complete_df$player_name <- gsub("Khouma Babacar", "Babacar", complete_df$player_name)
complete_df$player_name <- gsub("Franck Kessié", "Kessié", complete_df$player_name)
complete_df$player_name <- gsub("Alejandro Gomez", "A. Gomez", complete_df$player_name)
complete_df$player_name <- gsub("Diego Farias", "Farias", complete_df$player_name)
complete_df$player_name <- gsub("Camillo Ciano", "Ciano", complete_df$player_name)
complete_df$player_name <- gsub("Giovanni Simeone", "Simeone", complete_df$player_name)
complete_df$player_name <- gsub("Gianluca Caprari", "Caprari", complete_df$player_name)
complete_df$player_name <- gsub("Luis Muriel", "Muriel", complete_df$player_name)
complete_df$player_name <- gsub("Radja Nainggolan", "Nainggolan", complete_df$player_name)
complete_df$player_name <- gsub("Gonzalo Higuaín", "Higuaín", complete_df$player_name)
complete_df$player_name <- gsub("Moise Kean", "Kean", complete_df$player_name)
complete_df$player_name <- gsub("Piotr Zielinski", "Zielinski", complete_df$player_name)
complete_df$player_name <- gsub("Erick Pulgar", "Pulgar", complete_df$player_name)
complete_df$player_name <- gsub("Jasmin Kurtic", "Kurtic", complete_df$player_name)
complete_df$player_name <- gsub("Kevin Lasagna", "Lasagna", complete_df$player_name)
complete_df$player_name <- gsub("Mariusz Stepinski", "Stepinski", complete_df$player_name)
complete_df$player_name <- gsub("Lautaro Martínez", "L. Martínez", complete_df$player_name)

ligue1 <- complete_df
ligue1 <- ligue1 %>% mutate(ligue = "Ligue1")
bundes <- complete_df
bundes <- bundes %>% mutate(ligue = "Bundesliga")
epl <- complete_df
epl <- epl %>% mutate(ligue = "EPL")
liga <- complete_df
liga <- liga %>% mutate(ligue = "La Liga")
seriaA <- complete_df
seriaA <- seriaA %>% mutate(ligue = "Seria A")
all_ligue <- bind_rows(ligue1, bundes, epl, liga, seriaA)

p <- ggplot(epl, aes(x = goal, y = missed, label = player_name)) + 
  geom_point(size = 2) +
  geom_label_repel(label.padding = 0.15, size = 5) +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(expand = c(0,0.01), breaks = seq(0, 0.5, 0.1), 
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5")) +
  scale_y_continuous(expand = c(0,0.01), breaks = seq(0, 0.15, 0.025), 
                     labels = c("0", "0.025", "0.05", "0.075", "0.1", "0.125", "0.15")) +
  coord_cartesian(clip = "off") +
  theme_fivethirtyeight() +
  labs(x = "xG per shot scored", y = "xG per shot missed",
       title = "Serie A 2018/2019 season analysis about shots xG", 
       subtitle = "Players with at least 20 shots and 6 goals (no penalties)",
       caption = "Visualisation by: Andrea Bruzzone") +
  theme(panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(size = 18),
        plot.caption = element_text(size = 15),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 18),
        axis.text = element_text(size = 12))  
  
ggsave("shots_xg.pdf", p, limitsize = F, width = 15, height = 10) 


  
