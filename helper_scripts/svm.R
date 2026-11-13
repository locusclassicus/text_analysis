# Подключение необходимых пакетов
library(e1071)
library(ggplot2)

# Генерация данных
set.seed(123)

# Два класса с нормальным распределением
class1 <- data.frame(
  x = rnorm(20, mean = 2),
  y = rnorm(20, mean = 2),
  label = factor("Class 1")
)

class2 <- data.frame(
  x = rnorm(20, mean = 4),
  y = rnorm(20, mean = 4),
  label = factor("Class 2")
)

# Объединяем в один набор
my_data <- rbind(class1, class2)

# Обучаем линейную SVM-модель
model <- svm(label ~ ., data = my_data, kernel = "linear", cost = 1, scale = FALSE)

# Получаем коэффициенты w и b
w <- t(model$coefs) %*% model$SV  # веса w для гиперплоскости
b <- -model$rho                   # смещение

# Функция для построения разделяющей границы и маржи
plot_margin <- function() {
  
  # Создаем сетку для прогнозов (зернистость)
  make_grid <- function(data, resolution = 100) {
    x_range <- seq(min(data$x) - 1, max(data$x) + 1, length.out = resolution)
    y_range <- seq(min(data$y) - 1, max(data$y) + 1, length.out = resolution)
    grid <- expand.grid(x = x_range, y = y_range)
    return(grid)
  }
  
  grid <- make_grid(my_data)
  grid$pred <- predict(model, grid)
  sv <- my_data[model$index, ]  # Опорные векторы
  
  # Основной график
  p <- ggplot() +
    geom_tile(data = grid, aes(x = x, y = y, fill = pred), alpha = 0.3) +  # область решений
    geom_point(data = my_data, aes(x = x, y = y, color = label), size = 2) +  # точки
    geom_point(data = sv, aes(x = x, y = y), shape = 1, size = 5, stroke = 1.5, color = "grey30") +  # опорные векторы
    ggtitle("SVM: Разделяющая гиперплоскость и маржа") +
    theme_minimal()
  
  # Добавим разделяющую прямую и маржи
  # Формула прямой: w1*x + w2*y + b = 0 => y = -(w1/w2)*x - b/w2
  slope <- -w[1] / w[2]
  intercept <- -b / w[2]
  margin <- 1 / sqrt(sum(w^2))
  
  # Коэффициенты для линий маржи
  margin_up <- intercept + margin
  margin_down <- intercept - margin
  
  # Добавим границу и маржи в график
  p <- p +
    geom_abline(intercept = intercept, slope = slope, color = "royalblue", size = 1.2) + # разделяющая гиперплоскость
    geom_abline(intercept = margin_up, slope = slope, linetype = "dashed", color = "royalblue") +  # верхняя маржа
    geom_abline(intercept = margin_down, slope = slope, linetype = "dashed", color = "royalblue")   # нижняя маржа
  
  return(p)
}

# Отображение графика
plot_margin()

ggsave("svm.png", bg = "white")
