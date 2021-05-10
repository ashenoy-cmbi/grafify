db <- data.frame(Values = c(rnorm(2500, 20, 3),
                                   rnorm(2500, 30 ,3),
                                   rnorm(2500, 35, 3),
                                   rnorm(2500, 18, 3),
                                   rnorm(2500, 20, 3),
                                   rnorm(2500, 25, 3)),
                        Groups = rep(c("A", "B", "C", "D", "E", "F"), each = 2500),
                        YValues = rep(c(1:6), each = 2500))

library(grafify)

page1 <-
  ggplot(db, aes(x = Values, y = YValues))+
  geom_point(size = 1, 
             shape = 22,
             alpha = 0.1,
             position = position_jitter(height = 0.3),
             aes(colour = Groups))+
  theme_cowplot(22)+
  coord_flip()+
  theme(axis.line = element_line(size = 0.4, 
                                 colour = "grey40"))+
  theme_transparent(axis.title = element_blank(),
                    legend.position = "NULL",
                    axis.ticks = element_blank(),
                    axis.text = element_blank())+
  scale_colour_grafify2(palette = "muted")

ggsave(page1, file = "MISpage5.png", width = 7, height = 6, bg = "transparent")  

library(hexSticker)
library(showtext)
font_add_google("Fredericka the Great", "fred")

sticker(page1, package = "stats", 
        p_color = "grey20", p_family = "fred", 
        p_x = 1, p_y = 1.3,
        p_size = 60, s_width = 1.8, s_height = 1.4, 
        s_x = .95, s_y = .95, 
        h_color = "#E69F00", h_fill = "#FFFFE5", 
        filename = "smib_logo2.png")
  
