library(tidyverse)
news <- read_csv("./files/lenta_ru_news_2019_2023.csv")

news_2019 <- news |> 
  filter(str_detect(date, "2019")) |> 
  select(text, topic) 

# library(tidymodels)
# tidymodels_conflicts()
# news_split <- initial_split(news_2019_2023, prop = 0.4, strata = topic)
# news_train <- training(news_split)
# news_2019_2023 <- news_train

save(news_2019, file = "./data/news.Rdata")

