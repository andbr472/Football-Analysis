source("get_data_functions.R")

require(ggplot2)
require(ggthemes)
require(ggforce)

inter <- 2014:2019 %>% map_df(function(x) get_data(league = "Serie_A", node = 3, year = x)) %>% filter(Team == "Inter")
season <- 2014:2018 %>% map(function(x) rep(x, 38)) %>% unlist()
inter$season <- c(season, rep(2019, 14))

inter <- inter %>% select(season, Game_Number, xpts) %>% 
  tibble::add_row(season = 2014:2019, Game_Number = rep(0, 6), xpts = rep(0, 6)) %>% 
  arrange(season, Game_Number)


theme_set(theme_light(base_size = 15))
p <- ggplot(data = inter) + 
  geom_line(data = inter %>% filter(season == 2014), aes(x = Game_Number, y = cumsum(xpts), color = "2014"), size = 2) +
  geom_line(data = inter %>% filter(season == 2015), aes(x = Game_Number, y = cumsum(xpts), color = "2015"), size = 2) +
  geom_line(data = inter %>% filter(season == 2016), aes(x = Game_Number, y = cumsum(xpts), color = "2016"), size = 2) +
  geom_line(data = inter %>% filter(season == 2017), aes(x = Game_Number, y = cumsum(xpts), color = "2017"), size = 2) +
  geom_line(data = inter %>% filter(season == 2018), aes(x = Game_Number, y = cumsum(xpts), color = "2018"), size = 2) +
  geom_line(data = inter %>% filter(season == 2019), aes(x = Game_Number, y = cumsum(xpts), color = "2019"), size = 2) +
  scale_color_manual(name="", values=c("2014" = "red", "2015" = "blue", "2016" = "purple",
                                       "2017" = "darkorange4", "2018" = "pink", "2019" = "green4")) +
  scale_x_continuous(breaks = seq(0, 38, 1), labels = as.character(seq(0, 38, 1))) +
  scale_y_continuous(breaks = seq(0, 72, 3), labels = as.character(seq(0, 72, 3))) +
  labs(y = "Cumulative xPts", x = "Game", 
       caption = "Data source: understat.com | Visualisation by: Andrea Bruzzone",
       title = "Inter's cumulative xPts per season from 14/15 to 19/20") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.caption = element_text(size = 24),
        plot.title = element_text(face = "bold", size = 35),
        legend.text = element_text(size = 20),
        legend.key.size = unit(20, "mm")) +
  facet_zoom(x = Game_Number == 0:14, zoom.size = 1, xlim = c(0, 14)) 


ggsave("Inter.pdf", p, limitsize = F, width = 22, height = 22) 
