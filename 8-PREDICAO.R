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

folder <- "C:/Users/luktr/Desktop/lucas/r/mird/"

setwd(folder)

pt_infra_f <- read.csv("tabelas/infra_pa.csv") %>% dplyr::select(-X) %>%
  rename( area_PA = PA_area, dist_road = dist_rod, dist_hidro = dist_hid, dist_hidro_road = dist_hidro_rod)

deter <- st_read(file.path(folder,"base/shp/deter-amz-deter-public.shp")) # Deforestation (DETER) 


# Gerar NEW_DATA, OU SEJA, AGREGAR AS VARIÁVEIS PARA O ANO DE PREDIÇÃO
# IREI PREVER PARA 2023 E 2024, PORTANTO DEVO FORNECER O DESMATAMENTO DE 2022 E 2024
# MAS O DESMATAMENTO 2022 FOI USADO PARA TREINO. NÃO IMPORTA, AO USAR PREDICT() ELE USA
# OS PARAMETROS DO MODELO VS A BASE DE DESMATAMENTO 2022

# USE ESTE LOOP PARA AGREGAR O DESMATAMENTO 2023, 2024, AND SO ON....
# Loop over lag
# year_list <- data.frame()
# for (i in 2022:2022) { # For para def_year (-1), def_1_yr (0), def_2_yr (1), def_4_yr (2)
#   month_list <- data.frame()
#   for (j in 1:12) { #12
#     lag_list <- data.frame()
#     for (k in c(-1,0,1,3)) { # ,1,3 Fluxo filtra deter para 1, 2 e 4 anos acumulados
#       
#       if (k == -1) {
#         
#         d <- deter %>%
#           filter(year(VIEW_DATE) == i & month(VIEW_DATE) == j &
#                    (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) %>%
#           dplyr::select("CLASSNAME", "UF", "MUNICIPALI") %>%
#           #mutate(id = 1) %>%
#           st_transform(st_crs(deter)) }
#       
#       
#       if (k > -1){
#         
#         d <- deter %>%
#           filter((year(VIEW_DATE) >=  i-1-k & year(VIEW_DATE) <= i-1)  & month(VIEW_DATE) == j &
#                    (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) %>%
#           dplyr::select("CLASSNAME", "UF", "MUNICIPALI") %>%
#           #mutate(id = 1) %>%
#           st_transform(crs(deter))
#         
#       }
#       
#       # Computa área do DETER
#       # Step 1: Clip patches within region
#       patches_in_region <- d %>%
#         st_intersection(q_deter) %>%
#         mutate(area_m2 = st_area(.), year = i) %>%
#         group_by(OBJECTID) %>%
#         summarise(s = sum(area_m2))
#       
#       # Computa distancia
#       ## ── choose the polygon columns you want ─────────────────────────
#       keep_cols <- c("CLASSNAME", "UF", "MUNICIPALI")   # example
#       
#       # Which point is closest to each polygon?
#       nearest_id <- st_nearest_feature(points,patches_in_region)        # vector of row indices
#       
#       # Actual distance in metres, one value per point
#       dist_m <- st_distance(points, patches_in_region[nearest_id, ],by_element = TRUE)
#       
#       # pull those rows *in the same order as pts*, drop geometry
#       poly_atts <- st_drop_geometry(d[nearest_id, keep_cols, drop = FALSE])
#       
#       # Append both pieces of info to the point layer
#       points$poly_id   <- nearest_id           # ID you can use for joins
#       points$dist_m    <- as.numeric(dist_m)   # drop units class if you prefer
#       points <- cbind(points, poly_atts)  # adds the extra columns
#       
#       points_no_geom <- points %>% st_drop_geometry()
#       
#       # Extract focos  ──────────────────────────────────────────────────
#       f <- df_focos %>%
#         filter(Month == j) %>%
#         st_transform(st_crs(qt)) %>%
#         st_intersection(qt) %>%
#         group_by(Id,Month) %>%
#         na.omit() %>%
#         summarise(n = n()) %>%
#         #summarise(sum(pnt_cnt)) %>%
#         rename(OBJECTID = Id)
#       
#       # Funde distancia, quantitativo do desmatamento e Focos
#       patches_in_region_transf <-  patches_in_region %>% st_transform(st_crs(points)) %>% st_drop_geometry()
#       fire_transf <-  f %>% st_transform(st_crs(points)) %>% st_drop_geometry()
#       
#       df_final <- fire_transf %>%
#         left_join(patches_in_region_transf[,c("OBJECTID", "s")] , by = "OBJECTID") %>%
#         left_join(points_no_geom[, c("OBJECTID", "dist_m")], by = "OBJECTID")
#       
#       df_final_f <- df_final %>%
#         rename(focos = n, area_deter_m2 = s) %>%
#         mutate(year = i, lag = k)
#       
#       lag_list <- rbind(lag_list, df_final_f)
#     }
#     month_list <- rbind(lag_list, month_list)
#   }
#   year_list <- rbind(month_list,year_list)
# }
# 
# write.csv(year_list, "tabelas/df_operacionalizado6_only_2022.csv")

# Aqui começa o loop


new_data <- read.csv("tabelas/df_operacionalizado6_only_2022_v1.csv"); nrow(new_data)
#colnames(new_data$) <- "month" 

ano_da_vez <- 2022

result <- data.frame()
for (i in 1:12) {
  
  out_dir <- file.path(folder,paste0("outputs/v",i))
  
  final_model <- out_dir %>% list.files(pattern = "final_model.rds",
                                        full.names = TRUE) %>% readRDS() #%>%
  #dplyr::mutate(pred_def_km2 = pred_def/1000^2) # EU INSIRO ISSO NO CODIGO LÁ EMBAIXO
  
  df_wide <- new_data %>%
    dplyr::select(-X) %>%
    filter(Month == i) %>%
    mutate(lagg = case_when(
      lag == "-1" ~ "atual",
      lag == "0"  ~ "ano_ant",
      lag == "1"  ~ "dois_ant",
      lag == "3"  ~ "quatro_ant"
    )) %>% dplyr::select(-lag) %>%
    mutate(month_year = paste0(year, "_", sprintf("%02d", Month))) %>%  # ensures 2-digit months
    pivot_wider(
      id_cols = c(OBJECTID),
      names_from = c(lagg,month_year),
      values_from = c(area_deter_m2,focos , dist_m),
      names_glue = "{.value}_lag{lagg}_{month_year}",
      values_fn = mean  # or first, if there are duplicates
    )
  
  # Funde com year_list
  
  df11 <- df_wide %>%
    left_join(pt_infra_f, by = "OBJECTID") %>% # Inclui dist roads, PA,
    #dplyr::select(-"dist_1_pecent_ly_-1", -dist_1_pecent_ly_0) %>%
    mutate(across(
      where(~ inherits(.x, "units") && as.character(units(.x)) == "m^2"),
      ~ .x / 1e6
    )) %>%
    mutate(across(where(~ inherits(.x, "units")), as.numeric)) #%>%
  #rename(area_PA = PA_area)
  
  #-----------------------------------------------------------------------------
  # Função para deixar cabeçalho igual ao conjunto de treino
  decrease_year_month_simple <- function(colname) {
    # Extract year and month
    year <- as.numeric(str_extract(colname, "(\\d{4})(?=_\\d{2}$)"))
    month <- as.numeric(str_extract(colname, "(?<=_)\\d{2}$"))
    
    # Subtract 1
    year <- year - i
    month <- month - i - 1 # Alterar para 1 se fevereiro..sempre n - 1
    
    # Replace in the column name
    colname <- str_replace(colname, "(\\d{4})(?=_\\d{2}$)", as.character(year))
    colname <- str_replace(colname, "(?<=_)\\d{2}$", sprintf("%02d", month))
    return(colname)
  }
  
  # Apply to all column names
  if (ano_da_vez == 2022) {
    
    df111 <- df11 %>%
      mutate(pred <- predict(final_model,df11)) %>%
      mutate(pred_def = exp(.pred)) %>%
      mutate(pred_def_km2 = pred_def/1000^2)
  } else {
    colnames(df11) <- sapply(colnames(df11), decrease_year_month_simple)
    
    df111 <- df11 %>%
      mutate(pred <- predict(final_model,df11)) %>%
      mutate(pred_def = exp(.pred)) %>%
      mutate(pred_def_km2 = pred_def/1000^2)
  }
  
  # ------------------------------------------------------------------------------
  probs = c(0,0.6,0.9,1)
  labels = c("Low", "Average", "High")
  
  results_year <- df111 %>%
    #dplyr::filter(ref_year == 1) %>%
    dplyr::mutate(priority = cut(pred_def_km2,
                                 labels = labels,include.lowest = T,
                                 breaks = stats::quantile(pred_def_km2,
                                                          probs = probs)))  
  
  results_year_tmp <- prioritizedeforestationhotspots::deforestation_grid %>%
    dplyr::left_join(results_year %>% rename(id = OBJECTID ),by = "id")
  
  write_sf(results_year_tmp, file.path(out_dir,"pred_2023.gpkg"))
  
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  # Visualização da predição
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  
  # Resultado da priorização
  #prio <- st_read(paste0(folder,"/outputs/v",i,"/pred_2023.gpkg"))
  prio <- results_year_tmp
  
  d <- deter %>%
    filter(year(VIEW_DATE) == 2023 & month(VIEW_DATE) == i &
             (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) 
  
  #prio1 <- prio %>% filter(ref_yer == i)
  
  # Step 2: Transform polygons1 to match polygons2 CRS (or vice versa)
  # (Pick the CRS that makes sense for your analysis)
  if (st_crs(prio) != st_crs(d)) {
    prio <- st_transform(prio, st_crs(d))
  }
  
  # Try to fix invalid geometries
  d <- st_make_valid(d)
  
  png(paste0(out_dir,"/plot_predicao_",i,".png"), width = 3000, height = 2400, res = 300)  # pixels and DPI
  
  print(ggplot(prio) +
          geom_sf(aes(fill = priority)) +
          geom_sf(data = d) +  # overlay, no fill
          #scale_fill_viridis_d() +  # optional, nicer color scale
          theme_minimal())
  
  dev.off()
  
  # Intersection (sobre deter)
  # columns you want from prio1
  cols_to_keep <- c("priority")   # change as needed
  
  patches_touching <- d %>%                                 # original layer
    st_join(                                                # bring in columns
      prio %>%                                             #   from prio1
        dplyr::select(all_of(cols_to_keep)),                       #   only these cols
      join  = st_intersects,                                # spatial predicate
      left  = FALSE                                         # keep only matches
    ) %>%
    dplyr::distinct() %>%
    mutate(area_m2 = as.numeric(st_area(geometry)))
  
  # Aqui é para visualização gráfica
  patches_touching1 <- patches_touching  %>% # Calcula área deter
    st_drop_geometry()
  
  # Computa a área prevista como High mas sem desmatamento DETER
  # Step 1: Identify which grid cells intersect with patches
  grid_with_patches <- prio %>%
    mutate(has_patch = lengths(st_intersects(geometry, d)) > 0)
  
  high_cells <- grid_with_patches %>%
    filter(priority == "High")
  
  n_total_high <- nrow(high_cells)
  
  # Step 2: Commission error = classified as "high" but has_patch == FALSE
  commission_errors <- grid_with_patches %>%
    filter(priority == "High" & has_patch == FALSE)  
  
  # Step 3: Optionally, count how many cells are commission errors
  n_commission <- nrow(commission_errors)
  
  # Step 5. Proportion of commission error
  commission_rate <- n_commission / n_total_high
  
  # Finalmente consegui!
  area_by_region <-  patches_touching1 %>%
    group_by(priority) %>% # Agrupa por priorização
    summarise(area_tot = sum(area_m2)) %>% # area total deter por priorizacao
    mutate(area_comp = sum(area_tot)) %>% # # area total deter 
    group_by(priority) %>%
    summarise(100 * area_tot/area_comp) %>% 
    mutate(ref_yer = i) %>%
    mutate(comission_rate = commission_rate)
  
  # Save to list with a unique name
  result <- rbind(result,area_by_region)
  
  # Criar pasta caso não tenha
  #write.csv(df11, paste0("tabelas/df_operecionalizado_4_completo_mes_",i,".csv"))
  
}


png(paste0(folder,"/outputs","/plot_percentual_projetado_2023",".png"), width = 5000, height = 3000, res = 300)  # pixels and DPI

print(result %>%
  ggplot() + aes(x = ref_yer, y = `100 * area_tot/area_comp`, color = priority) +
  geom_point(size = 2.5) + 
  geom_line(size = 1.1) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.name
  ) +
  xlab("Months") + ylab("%") + #ggtitle("Distribuição das classes prioritárias em 2023 - DETER") +
  geom_text(
    aes(label = round(`100 * area_tot/area_comp`,2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,  # pushes the label above the bar
    size = 4       # adjust size if needed
  ) + #facet_grid(.~ class) +
  #theme_bw(base_size = 16) +
  theme_classic(base_size = 22) +
    scale_color_manual(values = c("lightblue", "orange", "darkgreen")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.title = element_blank(),
        #legend.direction = "horizontal",
        #legend.position = c(-10, -10),
        legend.justification = c(1,0),
        legend.key.size = unit(0.4, "cm"),      # size of legend keys (symbols)
        legend.text = element_text(size = 12),   # text size
        legend.spacing.y = unit(0.05, "cm"),
        legend.background = element_rect(
          fill = alpha("white", 0.7),
          colour = "grey80"
        )))

dev.off()
#----

png(paste0(folder,"/outputs","/plot_comissao_projetada",".png"), width = 5000, height = 3000, res = 300)  # pixels and DPI

print(result %>%
        group_by(ref_yer) %>%
        summarise(n = mean(comission_rate * 100)) %>%
        ggplot() + aes(x = ref_yer, y = n) +
        geom_point(size = 2.5) + 
        geom_line(size = 1.1) +
        scale_x_continuous(
          breaks = 1:12,
          labels = month.name
        ) +
        xlab("Months") + ylab("%") + #ggtitle("Distribuição das classes prioritárias em 2023 - DETER") +
        geom_text(
          aes(label = round(n,2)),
          position = position_dodge(width = 0.9),
          vjust = -1,  # pushes the label above the bar
          hjust = -0.2,
          size = 4       # adjust size if needed
        ) + #facet_grid(.~ class) +
        theme_classic(base_size = 22) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
              legend.title = element_blank(),
              #legend.direction = "horizontal",
              #legend.position = c(-10, -10),
              legend.justification = c(1,0),
              legend.key.size = unit(0.4, "cm"),      # size of legend keys (symbols)
              legend.text = element_text(size = 12),   # text size
              legend.spacing.y = unit(0.05, "cm"),
              legend.background = element_rect(
                fill = alpha("white", 0.7),
                colour = "grey80"
              )) + ggtitle("Erro de comissão"))

dev.off()