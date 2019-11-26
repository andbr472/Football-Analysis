require(ggplot2)
require(extrafont)
require(png)
require(grid)
require(ggrepel)

source("get_data_functions.R")

teamsData <- get_data(league = "Serie_A", node = 3)
df_teams <- list_league(teamsData)
df_teams$Team <- if_else(df_teams$Team == "Parma  Calcio  1913 ", "Parma", df_teams$Team)
df_teams$Team <- if_else(df_teams$Team == "AC  Milan", "Milan", df_teams$Team)

img <- png::readPNG("field.png")

labels_to_put <- df_teams %>% 
  group_by(Game_Number) %>% 
  summarise(max = max(xG), Team = df_teams[which(df_teams$xG == max(xG)), "Team"]) 

all_other_points <- df_teams %>% 
  group_by(Game_Number) %>% 
  mutate(max = max(xG)) %>% 
  filter(xG != max)

theme_set(theme_light(base_size = 15))
set.seed(123)
p <- ggplot(data = df_teams, aes(x = Game_Number, y = xG, label = Team)) + 
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_point(data = labels_to_put, aes(x = Game_Number, y = max, colour = Team, fill = Team), size = 3) + 
  geom_jitter(data = all_other_points, aes(x = Game_Number, y = xG), alpha = .5, width = 0.15, size = 3) +
  scale_colour_manual(values = c("blue", "chartreuse2", "pink", "cyan", "violetred1", "orange4", "purple",
                                 "orange", "gold")) + 
  stat_summary(fun.y = "median", geom = "point", size = 4, colour = "red") +
  geom_label_repel(data = labels_to_put, aes(x = Game_Number, y = max, fill = factor(Team)),
                   color = "white", fontface = "bold", box.padding = 1, size = 5,
                   point.padding = .3, direction = "y", nudge_y = 0.5) + 
  scale_fill_manual(values = c("blue", "chartreuse2", "pink", "cyan", "violetred1", "orange4", "purple",
                               "orange", "gold")) + 
  scale_x_continuous(breaks = seq(1, 12, 1), expand = c(0, .8), limits = c(0, 13)) +
  scale_y_continuous(breaks = seq(0, 6, 1), expand = c(0, .8), limits = c(0, 6)) +
  labs(x = "Game",
       title = "Expected goals by team per game",
       subtitle = "Serie A 2019/2020",
       caption = "Visualisation by: Andrea Bruzzone | Data source: understat.com") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.caption = element_text(colour = "grey50", size = 15),
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 15),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "none")

ggsave("xgPerGame.pdf", p, limitsize = FALSE, width = 15, height = 10)

