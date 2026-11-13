library(XML)
# url <- "https://raw.githubusercontent.com/dracor-org/rusdracor/main/tei/griboyedov-gore-ot-uma.xml"
# download.file(url, destfile = "griboedov.xml")

doc <- xmlTreeParse("griboedov.xml", useInternalNodes = TRUE)
rootnode <- xmlRoot(doc)

ns <- xmlNamespace(rootnode)

# узлы со стихами
line_nodes <- getNodeSet(rootnode, "//tei:l", 
                         namespaces = c(tei = ns))

line_text <- sapply(line_nodes, xmlValue)

# кто говорит
get_speaker <- function(line_node){
  line_node |>
    xmlParent() |> 
    xmlParent() |> 
    pluck(1) |> 
    xmlValue()
} 

line_speaker <- sapply(line_nodes, get_speaker)

# ищем явление

get_scene <- function(line_node){
  line_node |>
    xmlParent() |> 
    xmlParent() |> 
    xmlParent() |> 
    pluck("head") |> 
    xmlValue()
}

line_scene <- sapply(line_nodes, get_scene)
  
# найти действие
get_act <- function(line_node){
  line_node |>
    xmlParent() |> 
    xmlParent() |> 
    xmlParent() |> 
    xmlParent() |> 
    pluck("head") |> 
    xmlValue()
}

line_act <- sapply(line_nodes, get_act)

# преобразуем в таблицу
library(dplyr)
gore_tbl <- tibble(
  act = line_act, 
  scene = line_scene,
  speaker = line_speaker,
  text = line_text
)



