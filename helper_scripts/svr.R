library(ggplot2)
set.seed(42)

# Данные
x <- seq(0, 1, length.out = 100)
y <- 0.5 + x + rnorm(100, sd = 0.1)
model <- lm(y ~ x)
epsilon <- 0.15

# Data frame
df <- data.frame(x = x, y = y)
df$pred <- predict(model, newdata = df)
df$upper <- df$pred + epsilon
df$lower <- df$pred - epsilon

# Потери и опорные точки
df$loss <- df$y > df$upper | df$y < df$lower
df$support <- abs(df$y - df$pred) >= (epsilon - 0.01) & abs(df$y - df$pred) <= (epsilon + 0.01)

# График
ggplot(df, aes(x, y)) +
  # ε-труба
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#dbe9f4", alpha = 0.5) +
  
  # Линия регрессии
  geom_line(aes(y = pred), color = "#4a6785", size = 1.5) +
  
  # Все точки
  geom_point(color = "#444444", size = 1.5, alpha = 0.5) +
  
  # Опорные точки
  geom_point(data = subset(df, support), color = "#556b2f", fill = "#cddbb2", shape = 21, size = 3) +
  
  # Точки с потерями
  geom_point(data = subset(df, loss), color = "#556b2f", fill = "#cddbb2", shape = 21, size = 3) +
  
  # Стрелка потерь (одна демонстрационная)
  annotate("segment",
           x = df$x[which(df$loss)[11]],
           xend = df$x[which(df$loss)[11]],
           y = df$y[which(df$loss)[11]],
           yend = ifelse(df$y[which(df$loss)[11]] > df$upper[which(df$loss)[11]],
                         df$upper[which(df$loss)[11]],
                         df$lower[which(df$loss)[11]]),
           arrow = arrow(length = unit(0.2, "cm")),
           color = "#b7410e") +
  
  # Подписи
  annotate("text", x = 0.72, y = 1.42, label = "ε-зона", color = "#4a6785") +
  annotate("text", x = 0.60, y = 0.7, label = "Потери", color = "#b7410e") +
  annotate("text", x = 0.4, y = 1.2, label = "Опорные точки", color = "#556b2f") +
  
  labs(title = "Support Vector Regression (SVR)",
       x = "X", y = "Y") +
  theme_minimal()

ggsave("svr.png", bg = "white")
