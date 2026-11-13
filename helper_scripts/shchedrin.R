url <- "https://ru.wikisource.org/wiki/%D0%9C%D0%B8%D1%85%D0%B0%D0%B8%D0%BB_%D0%95%D0%B2%D0%B3%D1%80%D0%B0%D1%84%D0%BE%D0%B2%D0%B8%D1%87_%D0%A1%D0%B0%D0%BB%D1%82%D1%8B%D0%BA%D0%BE%D0%B2-%D0%A9%D0%B5%D0%B4%D1%80%D0%B8%D0%BD"
html = read_html(url)

toc <- html |> 
  html_elements("ul:nth-child(22) a")

tales <- tibble(
  title = toc %>%
    html_attr("title"),
  href = toc %>% 
    html_attr("href")
)

tales <- tales |> 
  mutate(href = paste0("https://ru.wikisource.org", href))

get_text <- function(url) {
  read_html(url) %>% 
    html_elements(".indent p") %>% 
    html_text2() %>% 
    paste(collapse= " ")
}

tales_text <- tales |> 
  mutate(text = map(href, get_text))

tales_text <- tales_text |> 
  mutate(title = str_remove(title, " \\(Салтыков-Щедрин\\)")) |> 
  select(-href) |>
  mutate(text = unlist(text)) |> 
  filter(text != "")

save(tales_text, file = "tales_text.Rdata")
