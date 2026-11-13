library(chromote)
library(tidyverse)
library(rvest)

b <- ChromoteSession$new()
url <- "https://chre.ashmus.ox.ac.uk/search/?ox_hs%5BhoardDiscoveryYearRangeStart%5D=900&ox_hs%5BhoardDiscoveryYearRangeEnd%5D=2026&ox_hs%5BhoardTerminalYearRangeStart%5D=-500&ox_hs%5BhoardTerminalYearRangeEnd%5D=1000&ox_hs%5BhoardOpeningYearRangeStart%5D=-500&ox_hs%5BhoardOpeningYearRangeEnd%5D=1000&ox_hs%5BhoardFindSpotName%5D=&ox_hs%5BhoardType%5D=&ox_hs%5BhoardAddress%5D=&ox_hs%5BhoardCity%5D=&ox_hs%5BhoardCounty%5D=&ox_hs%5BhoardRegion%5D=&ox_hs%5BhoardAncientPlace%5D=&ox_hs%5BhoardInternalNote%5D=&ox_hs%5BhoardReference_string%5D=&ox_hs%5BhoardCoinLevelData%5D=&ox_hs%5BhoardClosingReign1%5D=&ox_hs%5BhoardClosingReign2%5D=&ox_hs%5BhoardRating%5D=&ox_hs%5BhoardContextualRating%5D=&ox_hs%5BhoardFindSpotRating%5D=&ox_hs%5BhoardComment%5D=&ox_hs%5BhoardCredit%5D=&ox_hs%5BcoinYearRangeStart%5D=-500&ox_hs%5BcoinYearRangeEnd%5D=1000&ox_hs%5BhoardCoinCountMin%5D=0&ox_hs%5BhoardCoinCountMax%5D=100000&ox_hs%5BcoinPeriod%5D%5B%5D=23&ox_hs%5BcoinMaterials%5D=&ox_hs%5BcoinReferenceString%5D=&ox_hs%5BhoardObjectType%5D=&page=1&sort=&direction=&format=&search_type=search-hoards&ox_hs%5Brules%5D=&ox_hs%5Bboundary%5D=&ox_hs%5Bsubmit%5D=&ox_hs%5B_token%5D=1627a8.5aIFLy_RxPYpROtLrbKHp1ljE2sAoyN4SazB3DJf4bY.hOloYGOHp6VqcaIyw_b2whBQewJX0nkCLp-Qm2sZjYCTmzUdHIbwskNp2w#tabs-4"

b$Page$navigate(url)
Sys.sleep(10)

# Кликаем по вкладке
tab_selector <- "a[href='#tabs-4']"
b$Runtime$evaluate(sprintf("document.querySelector('%s').click()", tab_selector))
Sys.sleep(15)

# Проверяем наличие таблицы
table_selectors <- c(".grid-container .backgrid", ".backgrid", "table")
table_found <- FALSE
for (sel in table_selectors) {
  res <- b$Runtime$evaluate(sprintf("document.querySelector('%s') !== null", sel))
  if (!is.null(res$result$value) && res$result$value == TRUE) {
    table_selector <- sel
    table_found <- TRUE
    break
  }
}
if (!table_found) stop("Таблица не загрузилась")

# Извлекаем HTML
page_html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value |>
  read_html()
b$close()

# Парсим таблицу
raw_table <- page_html |>
  html_element(table_selector) |>
  html_table()

# Очищаем данные
clean_data <- raw_table |>
  as_tibble() |>
  rename_with(str_to_lower) |>
  mutate(across(where(is.character), str_squish))

# Определяем столбец с ID
id_col_name <- names(clean_data)[str_detect(names(clean_data), "id")][1]
if (is.na(id_col_name)) {
  warning("Столбец ID не найден, используется первый столбец")
  id_col_name <- names(clean_data)[1]
}

# Базовый URL сайта
base_url <- "https://chre.ashmus.ox.ac.uk"

# Генерируем ссылки
clean_data <- clean_data |>
  mutate(link = paste0(base_url, "/hoard/", .data[[id_col_name]]))

glimpse(clean_data)

byzcoins <- clean_data 
save(byzcoins, file = "./data/byzcoins.Rdata")
