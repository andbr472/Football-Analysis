source("get_data_functions.R")

require(ggsoccer)
require(ggplot2)
require(ggthemes)
require(patchwork)


ronaldo <- get_data(player_id = 2371, node = 4)
ronaldo <- ronaldo %>% mutate(X = as.numeric(X), Y = as.numeric(Y),
                              xG = as.numeric(xG))
field_ronaldo <- ggplot(ronaldo) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(data = ronaldo %>% filter(result != "Goal"),
             aes(X*100, y =  100 - (Y*100), fill = "Missed"), 
             size = 5, shape = 21, alpha = .5) +
  geom_point(data = ronaldo %>% filter(result == "Goal"),
             aes(X*100, y =  100 - (Y*100), fill = "Goal"), size = 5,
             shape = 21, alpha = .8)  + 
  theme_pitch() +
  coord_flip(xlim = c(60, 101), ylim = c(-1, 101)) +
  scale_fill_manual(name = "", values = c("Goal" = "red", "Missed" = "grey20")) + 
  scale_size_continuous(name = "", breaks  = NULL) +
  guides(fill = guide_legend(title = "", override.aes = list(size = 6))) +
  theme(plot.background = element_rect(fill = "chartreuse4"),
        plot.title = element_text(face = "bold", colour = "white", size = 18),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(colour = "white", size = 10),
        plot.subtitle = element_text(colour = "white", size = 12)
  ) + 
  labs(title = "Cristiano Ronaldo's shotmap",
       subtitle = "Data available from season 2014/2015 and league only"
  ) 

accuracy_by_season_ronaldo <- ronaldo %>% group_by(season) %>% summarise(accuracy = sum(result == "Goal") / n())

# ggplot(accuracy_by_season) + 
#   geom_line(aes(x = season, y = accuracy, group = 1)) +
#   scale_y_continuous(limits = c(0, 0.22)) +
#   theme_minimal()

accuracy_by_season_h_a_ronaldo <- ronaldo %>% group_by(season, h_a) %>% summarise(accuracy = sum(result == "Goal") / n()) %>% ungroup()

acc_ronaldo <- ggplot(accuracy_by_season_h_a_ronaldo) + 
  geom_line(data = accuracy_by_season_h_a_ronaldo %>% filter(h_a == "a"),
            aes(x = season, y = accuracy, group = 1, colour = "Away"), size = 3) +
  geom_line(data = accuracy_by_season_h_a_ronaldo %>% filter(h_a == "h"),
            aes(x = season, y = accuracy, group = 1, colour = "Home"), size = 3) +
  geom_line(data = accuracy_by_season_ronaldo, aes(x = season, y = accuracy, group = 1, colour = "Overall"),
            linetype = 3, size = 2) +
  scale_colour_manual(name = "", values = c("Home" = "blue", "Away" = "green", "Overall" = "black")) +
  scale_y_continuous(limits = c(0, 0.38)) +
  theme_minimal() +
  guides(colour = guide_legend(title = "", override.aes = list(size = 6))) +
  theme(plot.title = element_text(face = "bold", size = 18),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        plot.subtitle = element_text(size = 12)
  ) + 
  labs(title = "Cristiano Ronaldo's accuracy",
       subtitle = "Data available from season 2014/2015 and league only",
       x = "Season",
       y = "Accuracy"
  ) 

field_ronaldo + acc_ronaldo



#----Tries of other plots
ggplot(ronaldo) + 
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  stat_density_2d(aes(X*100, y =  100 - (Y*100))) +
  theme_pitch() +
  coord_flip(xlim = c(49, 101), ylim = c(-1, 101)) 




ronaldo1 <- ronaldo %>% mutate(result = ifelse(result == "Goal", "Goal", "Missed"))
ggplot(ronaldo1, aes(x = minute, y = xG, color = result)) + geom_jitter()

ggplot(ronaldo1, aes(x = as.numeric(minute), y = result, fill = result)) + 
 geom_density_ridges(stat = "density", aes(height = stat(density))) +
  theme_minimal()


ggplot(ronaldo1) + geom_bar(aes(x = season, fill = result))

ronaldo <- ronaldo %>% mutate(half = ifelse(minute <= 45, 1, 2))
accuracy_by_half <- ronaldo %>% group_by(season, half) %>% summarise(accuracy = sum(result == "Goal") / n()) %>% ungroup()

acc2 <- ggplot(accuracy_by_half) + 
  geom_line(data = accuracy_by_half %>% filter(half == 1),
            aes(x = season, y = accuracy, group = 1, colour = "First Half"), size = 3) +
  geom_line(data = accuracy_by_half %>% filter(half == 2),
            aes(x = season, y = accuracy, group = 1, colour = "Second Half"), size = 3) +
  geom_line(data = accuracy_by_season, aes(x = season, y = accuracy, group = 1, colour = "Overall"),
            linetype = 3, size = 2) +
  scale_colour_manual(name="", values=c(`First Half` = "blue", `Second Half` = "green", "Overall" = "black")) +
  scale_y_continuous(limits = c(0, 0.38)) +
  theme_minimal() +
  guides(colour = guide_legend(title = "", override.aes = list(size = 6))) +
  theme(plot.title = element_text(face = "bold", size = 18),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 12)
  ) + 
  labs(title = "Cristiano Ronaldo's accuracy",
       subtitle = "Data available from season 2014/2015 and league only", 
       caption = "Visualisation by: Andrea Bruzzone",
       x = "Season",
       y = "Accuracy"
  ) 

