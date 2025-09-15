# Gera mapas de erro de comissao em classes de alta priorizacao
# Modelo de predição. Usa o resultado do treinamento random forest
library(tidymodels)
library(tidyverse)
library(stringr)
if (!require(googlesheets4)) install.packages("googlesheets4")
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(sf)) install.packages("sf")
if (!require(raster)) install.packages("raster")
if (!require(terra)) install.packages("terra")
if (!require(exactextractr)) install.packages("exactextractr")
if (!require(purrr)) install.packages("purrr")
if (!require(kableExtra)) install.packages("kableExtra")
if (!require(patchwork)) install.packages("patchwork")
if (!require(RANN)) install.packages("RANN")
library(scales)

sf::sf_use_s2(FALSE)  # Disable S2 engine globally for sf
# Data frame filtrado pelo mes de interesse para 2023

folder <- "C:/Users/Lucas/Desktop/lucas/inpe_mids/projeto_fase_2"

setwd(folder)

deter <- st_read(file.path(folder,"base/deter-amz-deter-public.shp")) # Deforestation (DETER) 

# Visualizacao deter

for (i in 1:12) {
  
  prio <- st_read(paste0(folder,"/outputs/v",i,"/priorizacao.gpkg")) # Deforestation (DETER) 
  
  if (st_crs(prio) != st_crs(deter)) {
    prio <- st_transform(prio, st_crs(deter))
  }
  
  d <- deter %>%
    filter(year(VIEW_DATE) == 2022 & month(VIEW_DATE) == i &
             (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) 
  
  has_patch <- lengths(st_intersects(prio, deter)) > 0
  
  prio1 <- prio %>% mutate(has_patch = has_patch)
  
  grid_with_patches <- prio1 %>%
    mutate(category = case_when(
      has_patch & priority == "High" ~ "Correto",
      !has_patch & priority == "High" ~ "Erro",
      TRUE ~ "Outras classes"
    )); head(grid_with_patches)
  
  png(paste0(folder,"/outputs/v",i,"/erro_comissao_",i,".png"), width = 3000, height = 2400, res = 300)  # pixels and DPI
  
  # Step 4: plot
  print(ggplot() +
    geom_sf(data = grid_with_patches, aes(fill = category), color = "grey30", size = 0.1) +
    geom_sf(data = d, fill = NA, color = "black", size = 0.2) +
    scale_fill_manual(values = c(
      "Correto" = "green",
      "Erro" = "red",
      "Other" = "lightgrey"
    )) + ggtitle(paste0("Erro de comissao 2022")) +
    theme_minimal()) 
  
  dev.off()
  
  
}
