source("get_data_functions.R")

require(ggsoccer)
require(ggplot2)
require(ggthemes)
require(patchwork)
require(png)
require(gridExtra)
require(grid)


messi <- get_data(player_id = 2097, node = 4)
messi <- messi %>% mutate(X = as.numeric(X), Y = as.numeric(Y),
                              xG = as.numeric(xG))
field_messi <- ggplot(messi) +
  annotate_pitch(colour = "white",
                 fill   = "chartreuse4",
                 limits = FALSE) +
  geom_point(data = messi %>% filter(result != "Goal"),
             aes(X*100, y =  100 - (Y*100), fill = "Missed"), 
             size = 5, shape = 21, alpha = .5) +
  geom_point(data = messi %>% filter(result == "Goal"),
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
        plot.subtitle = element_text(colour = "white", size = 12),
        plot.caption = element_text(hjust = 0, size = 12, colour = "white")
  ) + 
  labs(title = "Lionel Messi's shotmap",
       subtitle = "Data available from season 2014/2015 and league only",
       caption = "Data source: Understat.com"
  ) 

accuracy_by_season_messi <- messi %>% group_by(season) %>% summarise(accuracy = sum(result == "Goal") / n())


accuracy_by_season_h_a_messi <- messi %>% group_by(season, h_a) %>% summarise(accuracy = sum(result == "Goal") / n()) %>% ungroup()

acc_messi <- ggplot(accuracy_by_season_h_a_messi) + 
  geom_line(data = accuracy_by_season_h_a_messi %>% filter(h_a == "a"),
            aes(x = season, y = accuracy, group = 1, colour = "Away"), size = 3) +
  geom_line(data = accuracy_by_season_h_a_messi %>% filter(h_a == "h"),
            aes(x = season, y = accuracy, group = 1, colour = "Home"), size = 3) +
  geom_line(data = accuracy_by_season_messi, aes(x = season, y = accuracy, group = 1, colour = "Overall"),
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
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 12)
  ) + 
  labs(title = "Lionel Messi's accuracy",
       subtitle = "Data available from season 2014/2015 and league only", 
       caption = "Visualisation by: Andrea Bruzzone",
       x = "Season",
       y = "Accuracy"
  ) 

ggsave("Messi_Ronaldo2.png", (field_ronaldo + acc_ronaldo) / (field_messi + acc_messi) , 
       limitsize = F, height = 10, width = 20)



#Try with pictures
messi_pic <- rasterGrob(readPNG("messi-2018-barcelona.png"))
field_messi1 <- field_messi + annotation_custom(messi_pic, xmin = 15, 
                      xmax = +Inf, 
                      ymin = -Inf, 
                      ymax = +40)

ronaldo_pic <- rasterGrob(readPNG("Cristiano-Ronaldo-119.png"))
field_ronaldo1 <- field_ronaldo + annotation_custom(ronaldo_pic, xmin = 20, 
                                xmax = +Inf, 
                                ymin = 55, 
                                ymax = +Inf)
(field_messi1 + acc_messi) / (field_ronaldo1 + acc_ronaldo)
