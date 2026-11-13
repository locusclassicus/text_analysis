library(ggplot2)
library(dplyr)

# сигмоида
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# создаем значения логитов
logits <- seq(-10, 10, by = 0.1)
probabilities <- sigmoid(logits)

# датафрейм
data <- tibble(
  z = logits,
  p = probabilities
)

# построение графика
ggplot(data, aes(x = z, y = p)) +
  geom_line(color = "royalblue", linewidth = 1.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "tomato") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  annotate("text", x = 6, y = 0.6, label = "p ≥ 0.5 → класс 1", size = 5, color = "darkolivegreen") +
  annotate("text", x = -6, y = 0.4, label = "p < 0.5 → класс 0", size = 5, color = "tomato") +
  labs(
    title = "Решение логистической регрессии на основе сигмоиды",
    x = "Логит (z)",
    y = "Вероятность положительного класса (p)"
  ) +
  theme_minimal(base_size = 12)

ggsave("logit.png", bg = "white")
