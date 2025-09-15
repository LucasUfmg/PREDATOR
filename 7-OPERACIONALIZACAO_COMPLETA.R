#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Fase 1: Agrega variáveis na grade 25km
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# By Lucas Rodrigues
# 28.03.2025
# Operacionalização mensal: Aggregating annual variables in 25 km grid 
# Deforestation: DETER
# Fires: INPE BD
# Infrastrucutures: Ministerio dos Transportes
# Protected Areas: TerraBrasilis INPE

# Este script gera dois arquivos: df_operacionalizado4 e infra_pa, que 
# posteriormente são utilizados no MODELO_PRIORIZACAO_RODAR_MAQUINA_32GB
#------------------------------------------------------------------
### Install and load R packages
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

folder <- "C:/Users/luktr/Desktop/lucas/r/mird"

setwd(folder)

# Cria pasta necessária
if (!dir.exists(file.path(folder, "base"))) {
  dir.create(file.path(folder, "base"))
}

# Cria pasta necessária
if (!dir.exists(file.path(folder, "tabelas"))) {
  dir.create(file.path(folder, "tabelas"))
}

# Carrega deter, malha de pontos, focos e a grade de 25 km
deter <- st_read("base/shp/deter-amz-deter-public.shp") # Deforestation (DETER) 
p <- st_read("base/shp/Pontos.shp")  # Points
q <- st_read("base/shp/Grade_Random_Forest.shp")  # Quadriculate 
df_focos <- read_sf("base/shp/focos_2016_2024.shp")

# Faz transformações necessárias
qt <- st_transform(q, crs(deter)); q_deter <- qt
# Adjust points mask to resolution
points <- st_transform(p, st_crs(deter))

q_focos <- st_transform(df_focos, crs(deter))

#----------------------------------
# Loop over lag
year_list <- data.frame()
for (i in 2022:2022) { # For para def_year (-1), def_1_yr (0), def_2_yr (1), def_4_yr (2)
  Month_list <- data.frame()
  for (j in 1:12) { #12
    lag_list <- data.frame()
    for (k in c(-1,0,1,3)) { # 0,1,3 Fluxo filtra deter para 1, 2 e 4 anos acumulados
      
      if (k == -1) {
        
        d <- deter %>%
          filter(year(VIEW_DATE) == i & month(VIEW_DATE) == j &
                   (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) %>%
          dplyr::select("CLASSNAME", "UF", "MUNICIPALI") %>%
          #mutate(id = 1) %>%
          st_transform(st_crs(deter)) }
      
      
      if (k > -1){
        
        d <- deter %>%
          filter((year(VIEW_DATE) >=  i-1-k & year(VIEW_DATE) <= i-1)  & month(VIEW_DATE) == j &
                   (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) %>%
          dplyr::select("CLASSNAME", "UF", "MUNICIPALI") %>%
          #mutate(id = 1) %>%
          st_transform(crs(deter))
        
      }
      
      patches_in_region <- q_deter %>%
        # left join keeps ALL q_deter grids
        left_join(
          d %>%
            mutate(area_m2 = st_area(.), year = i) %>%
            st_join(q_deter, join = st_intersects) %>%
            st_drop_geometry() %>%
            group_by(OBJECTID) %>%
            summarise(s = sum(area_m2, na.rm = TRUE)),
          by = "OBJECTID"
        ) %>%
        # replace NA (no deforestation) with 0
        mutate(s = ifelse(is.na(s), 0, s))
     
      ## Computa distancia
      ## ── choose the polygon columns you want ─────────────────────────
      keep_cols <- c("CLASSNAME", "UF", "MUNICIPALI")   # example
      
      # Which point is closest to each polygon?
      nearest_id <- st_nearest_feature(points,patches_in_region)        # vector of row indices
      
      # Actual distance in metres, one value per point
      dist_m <- st_distance(points, patches_in_region[nearest_id, ],by_element = TRUE)
      
      # pull those rows *in the same order as pts*, drop geometry
      poly_atts <- st_drop_geometry(d[nearest_id, keep_cols, drop = FALSE])
      
      # Append both pieces of info to the point layer
      points$poly_id   <- nearest_id           # ID you can use for joins
      points$dist_m    <- as.numeric(dist_m)   # drop units class if you prefer
      points <- cbind(points, poly_atts)  # adds the extra columns
      
      points_no_geom <- points %>% st_drop_geometry()
      
      # Extract focos  ──────────────────────────────────────────────────

            f <- qt %>%
        # left join keeps all grids
        left_join(
          df_focos %>%
            filter(Month == j) %>%
            st_transform(st_crs(qt)) %>%
            st_join(qt) %>%                        # assign points to grids
            st_drop_geometry() %>%
            group_by(Id, Month) %>%
            summarise(n = n(), .groups = "drop") %>%
            rename(OBJECTID = Id),
          by = "OBJECTID"
        ) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        mutate(Month = j) # Fazisso para garantir o mês em todas as observações
     
      # Funde distancia, quantitativo do desmatamento e Focos
      patches_in_region_transf <-  patches_in_region %>% st_transform(st_crs(points)) %>% st_drop_geometry()
      fire_transf <-  f %>% st_transform(st_crs(points)) %>% st_drop_geometry()
      
      df_final <- fire_transf %>%
        left_join(patches_in_region_transf[,c("OBJECTID", "s")] , by = "OBJECTID") %>%
        left_join(points_no_geom[, c("OBJECTID", "dist_m")], by = "OBJECTID")
      
      df_final_f <- df_final %>%
        rename(focos = n, area_deter_m2 = s) %>%
        mutate(year = i, lag = k) 
      
      
      lag_list <- rbind(lag_list, df_final_f)
    }
    Month_list <- rbind(lag_list, Month_list)
  }
  year_list <- rbind(Month_list,year_list)
}

write.csv(year_list, "tabelas/df_operacionalizado6_only_2022_v1.csv")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Fase 2: Roda a priorização
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Vector of required packages
pkgs <- c(
  "lwgeom", "googlesheets4", "googledrive", "tidyverse", "sf", "raster", "terra",
  "exactextractr", "purrr", "kableExtra", "patchwork", "RANN", "writexl",
  "ranger", "tidymodels", "devtools", "tune", "prioritizedeforestationhotspots"
)

# Install missing packages
installed <- pkgs %in% rownames(installed.packages())
if (any(!installed)) install.packages(pkgs[!installed])

# Load all packages

invisible(lapply(pkgs, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))))

sf::sf_use_s2(FALSE)  # Disable S2 engine globally for sf

# Cria pasta necessária
if (!dir.exists(file.path(folder, "tabelas"))) {
  dir.create(file.path(folder, "tabelas"))
}

# Cria pasta necessária
if (!dir.exists(file.path(folder, "outputs"))) {
  dir.create(file.path(folder, "outputs"))
}

pt_infra_f <- read.csv("tabelas/infra_pa.csv") %>% dplyr::select(-X) %>%
  rename( area_PA = PA_area, dist_road = dist_rod, dist_hidro = dist_hid, dist_hidro_road = dist_hidro_rod)

# IMPORTANTE SEMPRE CASAR O LOADING AQUI COM O SAVE DA AGREGAÇÃO DE DADOS NA MASCARA LINHA 150
year_list <- read.csv(file.path(folder,"/tabelas/df_operacionalizado6_only_2022_v1.csv")); head(year_list)
year_list$area_deter_m2[is.na(year_list$area_deter_m2)] <- 0 # Elimana NAs
#colnames(year_list)[3] <- "Month"

#write.csv(year_list, paste0(folder,"/tabelas/df_operacionalizado6_inflated.csv"))

for (i in 1:12) {
  
  df1_f <- year_list %>%
    filter(Month == i) %>%
    mutate(lagg = case_when(
      lag == "-1" ~ "atual",
      lag == "0"  ~ "ano_ant",
      lag == "1"  ~ "dois_ant",
      lag == "3"  ~ "quatro_ant"
    )) %>% dplyr::select(-lag) #%>% 
  #slice_sample(n = 50) # apagar isto
  
  # Ajusta year list deixa ele longo 
  
  df_long <- df1_f %>%
    mutate(Month_year = paste0(year, "_", sprintf("%02d", Month)))  # ensures 2-digit Months
  
  df_wide <- df_long %>%
    pivot_wider(
      id_cols = c(OBJECTID),
      names_from = c(lagg,Month_year),
      values_from = c(area_deter_m2,focos , dist_m),
      names_glue = "{.value}_lag{lagg}_{Month_year}",
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
  
  df11[is.na(df11)] <- 0 # elimina de vez NAs 
  
  # Criar pasta caso não tenha
  #write.csv(df11, paste0("tabelas/df_operecionalizado_5_completo_mes_",i,".csv"))
  
  # Criar pasta caso não tenha
  
  if (!dir.exists(file.path(folder,"outputs",paste0("v",i)))) {
    dir.create(file.path(folder,"outputs",paste0("v",i)))
  }
  
  out_dir <- file.path(folder,paste0("outputs/v",i))
  
  cols <- names(df11)[grepl("^area_deter_m2_lagatual_2022_\\d{2}$", names(df11))]
  
  read_data <- function (raw = FALSE) {
    col_sym <- rlang::sym(cols)
    
    data_tb <- df11 %>%
      dplyr::mutate(
        def = !!col_sym * 1000^2,
        log_def = log(!!col_sym) # As !! unqote e permite entender
      )
    
    if (!raw) {
      data_tb <- data_tb %>% dplyr::filter(def > 0)
    }
    
    return(data_tb)
  }
  
  # Assuming df is your data
  predictors <- df11 %>%
    dplyr::select(-OBJECTID) %>%
    dplyr::select(where(is.numeric),- !!sym(cols)) %>%
    names()
  
  response <- "log_def"
  
  my_formula <- reformulate(termlabels = predictors, response = response)
  
  # Change variable models
  build_formula <- function() {
    as.formula(my_formula)
  }
  
  # Modifica no pacote
  assignInNamespace(".read_data", read_data, ns = "prioritizedeforestationhotspots")
  assignInNamespace(".build_formula", build_formula, ns = "prioritizedeforestationhotspots")
  
  fit_model(out_dir) # Roda modelo
  
  results_tb <- out_dir %>% list.files(pattern = "new_data_tb.rds",
                                       full.names = TRUE) %>% readRDS() %>%
    dplyr::mutate(pred_def_km2 = pred_def/1000^2)
  
  probs = c(0,0.6,0.9,1)
  labels = c("Low", "Average", "High")
  
  results_year <- results_tb %>%
    #dplyr::filter(ref_year == 1) %>%
    dplyr::mutate(priority = cut(pred_def_km2,
                                 labels = labels,include.lowest = T,
                                 breaks = stats::quantile(pred_def_km2,
                                                          probs = probs)))  
  
  results_year_tmp <- prioritizedeforestationhotspots::deforestation_grid %>%
    dplyr::left_join(results_year %>% rename(id = OBJECTID ),by = "id")
  
  write_sf(results_year_tmp, file.path(out_dir,"priorizacao.gpkg"))
  
}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Fase 3: Visualiza mapa, quantifica % deter em cada classe e erro de comissão
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# By Lucas Rodrigues
# 28.03.2025
# Operacionalização anual: Aggregating annual variables in 25 km grid 
# Deforestation: PRODES
# Fires: INPE BD
# Infrastrucutures: Ministerio dos Transportes
# Protected Areas: TerraBrasilis INPE
#------------------------------------------------------------------
### Install and load R packages
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

#folder <- "C:/Users/luktr/Desktop/lucas/r/mird"

# Compara desmatamento PRODES
#pr <- raster(file.path(folder,"base/shp"))  # Deforestation (PRODES)
#deter <- st_read(file.path(folder,"base/shp/deter-amz-deter-public.shp")) # Deforestation (DETER) 

result <- data.frame()
for (i in 1:12) { # Mes

  # Resultado da priorização
  prio <- st_read(paste0(folder,"/outputs/v",i,"/priorizacao.gpkg"))
  
  prio <- prio %>% mutate(priority1 = case_when(
    is.na(priority) ~ "Low",
    TRUE     ~ as.character(priority)   # keep other values
  )) 
  
  
  d <- deter %>%
    filter(year(VIEW_DATE) == 2022 & month(VIEW_DATE) == i &
             (CLASSNAME == "DESMATAMENTO_CR" | CLASSNAME == "DESMATAMENTO_VEG" | CLASSNAME == "MINERACAO")) 
  
  
  #prio1 <- prio %>% filter(ref_yer == i)
  
  # Step 2: Transform polygons1 to match polygons2 CRS (or vice versa)
  # (Pick the CRS that makes sense for your analysis)
  if (st_crs(prio) != st_crs(d)) {
    prio <- st_transform(prio, st_crs(d))
  }
  
  # Try to fix invalid geometries
  d <- st_make_valid(d)
  
  png(paste0(folder,"/outputs/v",i,"/plot_",i,".png"), width = 3000, height = 2400, res = 300)  # pixels and DPI
  
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
    mutate(area_m2 = as.numeric(st_area(geometry))) %>% # Calcula área deter
    st_drop_geometry()
  
  # Aqui é para visualização gráfica
  patches_touching1 <- patches_touching  %>% # Calcula área deter
    st_drop_geometry()
  
  # Computa a área prevista como High mas sem desmatamento DETER
  # Step 1: Identify which grid cells intersect with patches
  grid_with_patches <- prio %>%
    mutate(has_patch = lengths(st_intersects(geom, d)) > 0)
  
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
  
}

png(paste0(folder,"/outputs","/plot_percentual_",i,".png"), width = 3000, height = 3000, res = 300)  # pixels and DPI

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
        theme_bw(base_size = 16) +
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
              )))

dev.off()


#----
png(paste0(folder,"/outputs","/comissao_",i,".png"), width = 3000, height = 3000, res = 300)  # pixels and DPI

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
