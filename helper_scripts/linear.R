library(tidyverse)

library(paletteer)
cols <- paletteer_d("ggthemes::wsj_rgby")

linear_data <- tibble(x = -10:10,
                      y1 = 2*x,
                      y2 = -x,
                      y3 = 2*x + 2,
                      y4 = -x - 1) |> 
  pivot_longer(-x, names_to = "y", values_to = "value")

linear_data |> 
  ggplot(aes(x, value, color = y)) +
  geom_line(linewidth = 0.8, show.legend = FALSE) + 
  geom_hline(yintercept = 0, col = "grey40") +
  geom_vline(xintercept = 0, col = "grey40") +
  annotate(geom = "label", label = "  y1 = 2x  ", 
           x = 4, y = 3, col = cols[1]) +
  annotate(geom = "label", label = "  y2 = -x  ", 
           x = 4, y = 1, col = cols[2]) +
  annotate(geom = "label", label = "y3 = 2x+2", 
           x = 4, y = -1, col = cols[3]) +
  annotate(geom = "label", label = "y2 = -x-1", 
           x = 4, y = -3, col = cols[4]) +
  coord_cartesian(xlim = c(-5,5), ylim = c(-5,5)) +
  scale_x_continuous(breaks = seq(-10, 10, 1)) +
  scale_y_continuous(breaks = seq(-10, 10, 1)) + 
  scale_color_manual("", values = cols) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

ggsave("linear.png", bg = "white")
