library(showtext)
font_add_google("Permanent Marker", family = "friends")
showtext_auto()

# install.packages("remotes")
#remotes::install_github("MatthewBJane/ThemePark")
library(ThemePark)
library(ggimage)

pca_coords <- as.data.frame(pca_fit$x) |> 
  mutate(image_file = paste0("friends_images/", rownames(friends_tf), ".jpg"))

fviz_pca_biplot(pca_fit,  geom = c("image"),
                select.var = list(cos2 = 20),
                col.var = "steelblue",
                alpha.var = 0.3,
                repel = TRUE,
                ggtheme = theme_friends(),
                font.family = "friends") +
  theme(legend.position = "none") +
  geom_image(data = pca_coords, aes(x = PC1, y = PC2, image = image_file), size = 0.1) 