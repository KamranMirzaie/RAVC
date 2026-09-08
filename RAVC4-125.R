# ============================================================
# R Code for HPAI Value Chain Risk Assessment - OPTIMIZED VERSION
# WITH TWO-STAGE HIERARCHICAL MCDA AND SPATIAL MCDA - FINAL
# ============================================================

rm(list = ls()); cat("\014"); graphics.off()
setwd("D:\\RAVC\\15 R\\Data")

# ============================================================
# Part 1: Setup - Install Required Packages
# ============================================================

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

packages <- c("readxl", "writexl", "dplyr", "tidyr", "ggplot2", "scales", 
              "ggrepel", "viridis", "leaflet", "htmlwidgets", "sf", 
              "rnaturalearth", "rnaturalearthdata", "ggspatial", "webshot", 
              "osmdata", "gridExtra", "MASS")

for (pkg in packages) {
  tryCatch(install_if_missing(pkg), error = function(e) 
    message("Warning: Could not install ", pkg, " - ", e$message))
}

message("\nAll packages loaded successfully!")

# ============================================================
# Part 2: Global Constants
# ============================================================

CONNECTIVITY_MAP <- c(
  "Input_Supply" = 3, "Hatchery" = 5, "Pullet" = 4, "Breeder" = 6, "Broiler" = 8,
  "Layer" = 7, "Backyard_Poultry" = 2, "Transport" = 10, "Live_Bird_Market" = 9,
  "Slaughterhouse" = 5, "Distribution_Retail" = 6, "Consumer_Household" = 4
)

SHAPE_VALUES <- c(16, 17, 15, 18, 19, 20, 8, 4, 0, 2, 5, 6, 7)
SHAPE_SYMBOLS <- c("\u25CF", "\u25B2", "\u25A0", "\u25C6", "\u2605", "\u271A",
                   "\u2715", "\u25C9", "\u25C8", "\u25CA", "\u25A3", "\u25A4", "\u25A5")
TILE_PROVIDERS <- list(providers$CartoDB.Positron, providers$OpenStreetMap.Mapnik, 
                       providers$Esri.WorldStreetMap)
TILE_NAMES <- c("CartoDB", "OpenStreetMap", "Esri")

# ============================================================
# Part 3: Helper Functions
# ============================================================

convert_coord <- function(x) {
  if (is.character(x)) { x <- gsub(",", ".", x); x <- gsub(" ", "", x) }
  as.numeric(x)
}

get_xy_indices <- function(col_names, fallback_x = 19, fallback_y = 20) {
  x_idx <- grep("^x$|longitude|lng", col_names, ignore.case = TRUE)
  y_idx <- grep("^y$|latitude|lat", col_names, ignore.case = TRUE)
  if (length(x_idx) > 0 && length(y_idx) > 0) return(c(x = x_idx[1], y = y_idx[1]))
  if (length(col_names) >= max(fallback_x, fallback_y)) return(c(x = fallback_x, y = fallback_y))
  return(NULL)
}

safe_ggsave <- function(plot, filepath, width = 14, height = 10, dpi = 300, bg = "white", limitsize = TRUE) {
  if (is.null(plot)) return(invisible(FALSE))
  tryCatch({
    ggsave(filepath, plot, width = width, height = height, dpi = dpi, bg = bg, limitsize = limitsize)
    message(" Saved: ", basename(filepath))
    invisible(TRUE)
  }, error = function(e) { message(" ERROR saving ", basename(filepath), ": ", e$message); invisible(FALSE) })
}

build_shape_color_map <- function(unit_types) {
  n <- length(unit_types)
  list(
    shapes = setNames(SHAPE_VALUES[seq_len(n)], unit_types),
    colors = setNames(viridis::viridis(n), unit_types),
    symbols = setNames(SHAPE_SYMBOLS[seq_len(n)], unit_types)
  )
}

add_leaflet_tiles <- function(m) {
  for (i in seq_along(TILE_PROVIDERS)) {
    tryCatch({ m <- m %>% addProviderTiles(TILE_PROVIDERS[[i]], group = TILE_NAMES[i]) }, error = function(e) {})
  }
  m
}

clean_wetlands <- function(wetlands_data, simplify_tolerance = 0.001) {
  if (is.null(wetlands_data) || nrow(wetlands_data) == 0) return(NULL)
  
  wetlands_data <- wetlands_data[!st_is_empty(wetlands_data), ]
  if (nrow(wetlands_data) == 0) return(NULL)
  
  wetlands_data <- tryCatch({
    st_collection_extract(wetlands_data, c("POLYGON", "MULTIPOLYGON"))
  }, error = function(e) wetlands_data)
  
  wetlands_data <- wetlands_data[!st_is_empty(wetlands_data), ]
  if (nrow(wetlands_data) == 0) return(NULL)
  
  wetlands_data <- tryCatch({
    st_simplify(wetlands_data, dTolerance = simplify_tolerance)
  }, error = function(e) wetlands_data)
  
  wetlands_data <- wetlands_data[!st_is_empty(wetlands_data), ]
  wetlands_data <- tryCatch({
    st_make_valid(wetlands_data)
  }, error = function(e) wetlands_data)
  wetlands_data <- wetlands_data[!st_is_empty(wetlands_data), ]
  
  geom_types <- st_geometry_type(wetlands_data)
  if (any(geom_types == "GEOMETRYCOLLECTION")) {
    wetlands_data <- wetlands_data[geom_types != "GEOMETRYCOLLECTION", ]
  }
  
  return(wetlands_data)
}

add_leaflet_context_layers <- function(m, provinces_data, roads_data, wetlands_data,
                                       bbox = NULL, road_weight = 2, road_color = "#FF8F00",
                                       wetland_max_render = 2000) {
  groups <- character(0)
  
  if (!is.null(provinces_data) && nrow(provinces_data) > 0) {
    col_names <- names(provinces_data)
    label_col <- if ("name" %in% col_names) "name" else if ("NAME_1" %in% col_names) "NAME_1" else col_names[1]
    m <- m %>% addPolygons(data = provinces_data, fillColor = "#E8F4FD", fillOpacity = 0.1,
                           color = "#1565C0", weight = 2,
                           label = ~as.character(provinces_data[[label_col]]), group = "Province Borders")
    groups <- c(groups, "Province Borders")
  }
  
  if (!is.null(wetlands_data) && nrow(wetlands_data) > 0) {
    wetlands_render <- clean_wetlands(wetlands_data)
    if (!is.null(wetlands_render) && nrow(wetlands_render) > 0) {
      if (!is.null(bbox)) {
        wetlands_render <- tryCatch(st_crop(wetlands_render, bbox), error = function(e) wetlands_render)
      }
      if (nrow(wetlands_render) > 0 && nrow(wetlands_render) <= wetland_max_render) {
        m <- m %>% addPolygons(data = wetlands_render, fillColor = "#4FC3F7", fillOpacity = 0.3,
                               color = "#0288D1", weight = 1, label = "Wetland", group = "Wetlands")
        groups <- c(groups, "Wetlands")
      }
    }
  }
  
  if (!is.null(roads_data) && nrow(roads_data) > 0) {
    m <- m %>% addPolylines(data = roads_data, color = road_color, weight = road_weight,
                            opacity = 0.6, label = "Road", group = "Roads")
    groups <- c(groups, "Roads")
  }
  
  list(map = m, groups = groups)
}

normalize_persian_name <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\u200C", "", x)
  x <- gsub(" ", "", x)
  x <- gsub("\u064A", "\u06CC", x)
  x <- gsub("\u0643", "\u06A9", x)
  x
}

create_leaflet_label_options <- function(border_color = "#999", text_color = "black", font_size = "10px") {
  labelOptions(
    style = list(
      "font-weight" = "bold", "color" = text_color, "font-size" = font_size,
      "background" = "rgba(255,255,255,0.9)", "padding" = "3px 6px",
      "border-radius" = "4px", "border" = paste("1px solid", border_color),
      "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)", "text-align" = "center"
    ),
    direction = "top", textOnly = TRUE, offset = c(0, 8)
  )
}

minmax_norm <- function(x, invert = FALSE) {
  rng <- range(x, na.rm = TRUE)
  if (rng[2] <= rng[1]) return(rep(0.5, length(x)))
  norm <- (x - rng[1]) / (rng[2] - rng[1])
  if (invert) 1 - norm else norm
}

weights_to_df <- function(weights_info) {
  if (is.null(weights_info)) return(NULL)
  data.frame(Criterion = names(weights_info$Weights), Weight = weights_info$Weights,
             Criterion_Type = weights_info$Criteria_Types[names(weights_info$Weights)], 
             stringsAsFactors = FALSE)
}

# ============================================================
# Part 4: Load Spatial Data
# ============================================================

load_high_res_iran <- function() {
  cache_file <- "Shapefiles/iran_cache.rds"
  if (file.exists(cache_file)) {
    iran <- readRDS(cache_file)
    if (!is.null(iran)) return(iran)
  }
  
  tryCatch({
    if (!require("rnaturalearthhires")) 
      install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev")
    library(rnaturalearthhires)
    iran <- ne_countries(country = "Iran", scale = "large", returnclass = "sf")
    if (!is.null(iran) && nrow(iran) > 0) { saveRDS(iran, cache_file); return(iran) }
  }, error = function(e) {})
  
  tryCatch({
    iran <- ne_countries(country = "Iran", scale = "large", returnclass = "sf")
    if (!is.null(iran) && nrow(iran) > 0) { saveRDS(iran, cache_file); return(iran) }
  }, error = function(e) {})
  
  tryCatch({
    if (file.exists("Shapefiles/IRN_adm0.shp")) {
      iran <- st_read("Shapefiles/IRN_adm0.shp", quiet = TRUE)
      saveRDS(iran, cache_file); return(iran)
    }
  }, error = function(e) {})
  
  iran_coords <- matrix(c(44.0,39.5,44.5,39.8,45.5,39.7,46.5,39.5,47.5,39.2,48.0,39.0,49.0,38.5,
                          50.0,38.2,51.0,38.0,52.0,38.0,53.0,38.0,54.0,38.0,55.0,37.8,56.0,37.5,57.0,37.5,58.0,37.5,
                          59.0,37.8,60.0,38.0,61.0,38.0,61.5,37.5,62.0,37.0,63.0,36.5,63.5,36.0,64.0,35.5,65.0,35.0,
                          65.5,34.5,66.0,34.0,66.0,33.5,65.5,33.0,65.0,32.5,64.5,32.0,64.0,31.5,63.5,31.0,63.0,30.5,
                          62.5,30.0,62.0,29.5,61.5,29.0,61.0,28.5,60.5,28.0,60.0,27.5,59.5,27.0,59.0,26.5,58.5,26.0,
                          58.0,25.5,57.5,25.0,57.0,25.0,56.5,25.0,56.0,25.0,55.5,25.0,55.0,25.0,54.5,25.0,54.0,25.0,
                          53.5,25.5,53.0,26.0,52.5,26.5,52.0,26.5,51.5,26.5,51.0,26.5,50.5,26.5,50.0,27.0,49.5,27.0,
                          49.0,27.0,48.5,27.0,48.0,27.5,47.5,27.5,47.0,28.0,46.5,28.0,46.0,28.0,45.5,28.0,45.0,28.5,
                          44.5,28.5,44.0,29.0,43.5,29.5,43.0,29.5,42.5,30.0,42.0,30.5,41.5,31.0,41.0,31.5,40.5,32.0,
                          40.0,32.5,39.5,33.0,39.0,33.5,38.5,34.0,38.0,34.5,37.5,35.0,37.0,35.5,37.0,36.0,37.0,36.5,
                          37.0,37.0,37.5,37.5,38.0,38.0,38.5,38.5,39.0,38.5,39.5,39.0,40.0,39.0,40.5,39.0,41.0,39.0,
                          41.5,39.0,42.0,39.0,42.5,39.0,43.0,39.0,43.5,39.0,44.0,39.5), ncol=2, byrow=TRUE)
  polygon <- st_polygon(list(iran_coords))
  iran_sf <- st_sf(geometry = st_sfc(polygon, crs = 4326), admin = "Iran")
  saveRDS(iran_sf, cache_file); return(iran_sf)
}

load_iran_provinces <- function() {
  cache_file <- "Shapefiles/provinces_cache.rds"
  if (file.exists(cache_file)) { provinces <- readRDS(cache_file); if (!is.null(provinces)) return(provinces) }
  
  tryCatch({
    provinces <- ne_states(country = "Iran", returnclass = "sf")
    if (!is.null(provinces) && nrow(provinces) > 0) { saveRDS(provinces, cache_file); return(provinces) }
  }, error = function(e) {})
  
  tryCatch({
    if (file.exists("Shapefiles/IRN_adm1.shp")) {
      provinces <- st_read("Shapefiles/IRN_adm1.shp", quiet = TRUE)
      saveRDS(provinces, cache_file); return(provinces)
    }
  }, error = function(e) {})
  return(NULL)
}

load_real_iran_roads <- function(city_boundary = NULL, city_name = NULL) {
  if (is.null(city_boundary) || is.null(city_name)) return(NULL)
  
  if (!require("osmdata")) { install.packages("osmdata", dependencies = TRUE); library(osmdata) }
  if (!dir.exists("Shapefiles")) dir.create("Shapefiles")
  
  city_name_clean <- trimws(gsub(" ", "_", gsub("county|province", "", city_name)))
  local_road_file <- paste0("Shapefiles/IRN_roads_", city_name_clean, ".shp")
  
  if (file.exists(local_road_file)) {
    roads <- tryCatch(st_read(local_road_file, quiet = TRUE), error = function(e) NULL)
    if (!is.null(roads) && nrow(roads) > 0) {
      roads <- st_simplify(roads, dTolerance = 0.002)
      return(roads)
    }
  }
  
  iran_roads_file <- "Shapefiles/IRN_roads.shp"
  if (file.exists(iran_roads_file)) {
    iran_roads <- tryCatch(st_read(iran_roads_file, quiet = TRUE), error = function(e) NULL)
    if (!is.null(iran_roads) && nrow(iran_roads) > 0) {
      tryCatch({
        iran_roads_simple <- st_simplify(iran_roads, dTolerance = 0.001)
        iran_roads_simple <- iran_roads_simple[!st_is_empty(iran_roads_simple), ]
        if (nrow(iran_roads_simple) > 0) {
          roads_cropped <- st_intersection(iran_roads_simple, city_boundary)
          roads_cropped <- roads_cropped[!st_is_empty(roads_cropped), ]
          if (nrow(roads_cropped) > 0) {
            roads_cropped <- st_simplify(roads_cropped, dTolerance = 0.002)
            roads_cropped <- roads_cropped[!st_is_empty(roads_cropped), ]
            tryCatch({
              st_write(roads_cropped, local_road_file, delete_layer = TRUE, quiet = TRUE)
            }, error = function(e) {})
            return(roads_cropped)
          }
        }
      }, error = function(e) {})
    }
  }
  
  bbox <- st_bbox(city_boundary)
  tryCatch({
    roads_query <- opq(bbox = c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]), timeout = 300) %>%
      add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary"))
    roads_data <- osmdata_sf(roads_query)
    roads <- roads_data$osm_lines
    if (!is.null(roads) && nrow(roads) > 0) {
      roads <- st_intersection(roads, city_boundary) %>% st_simplify(dTolerance = 0.002)
      roads <- roads[!st_is_empty(roads), ]
      if (nrow(roads) > 0) { 
        st_write(roads, local_road_file, delete_layer = TRUE, quiet = TRUE)
        return(roads)
      }
    }
  }, error = function(e) {})
  
  return(NULL)
}

load_real_iran_wetlands <- function() {
  if (!dir.exists("Shapefiles")) dir.create("Shapefiles")
  
  local_files <- c("Shapefiles/IRN_wetlands_real.shp", "Shapefiles/IRN_water.shp", "Shapefiles/lakes_iran.shp")
  for (file in local_files) {
    if (file.exists(file)) {
      wetlands <- tryCatch(st_read(file, quiet = TRUE), error = function(e) NULL)
      if (!is.null(wetlands) && nrow(wetlands) > 0) return(wetlands)
    }
  }
  
  tryCatch({
    lakes <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf")
    wetlands <- st_crop(lakes, c(xmin = 44, ymin = 25, xmax = 63, ymax = 40))
    if (!is.null(wetlands) && nrow(wetlands) > 0) { 
      st_write(wetlands, "Shapefiles/IRN_wetlands_real.shp", delete_layer = TRUE, quiet = TRUE)
      return(wetlands) 
    }
  }, error = function(e) {})
  return(NULL)
}

# ============================================================
# Part 5: Create Input Template
# ============================================================

create_input_template <- function(file_path = "HPAI_Risk_Matrix_Data.xlsx") {
  if (file.exists(file_path)) { return(file_path) }
  
  chain_stages <- names(CONNECTIVITY_MAP)
  hazards <- c("Entry_Buyer_Pullet", "Vehicle_Disinfection_Inadequate", "Market_Poultry_Mixing",
               "Mortality_Not_Reported", "Shared_Vaccinator", "Wild_Bird_Contact",
               "Covert_Sale_Suspicious_Flock", "Shared_Worker", "Manure_Unknown_Destination",
               "Contaminated_Crates", "Contaminated_PPE", "Unhygienic_Waste_Disposal")
  
  template_data <- expand.grid(Chain_Segment = chain_stages, Hazard = hazards, stringsAsFactors = FALSE)
  set.seed(123)
  
  template_data <- template_data %>%
    mutate(
      Hazard_Type = case_when(
        Hazard %in% c("Mortality_Not_Reported", "Shared_Vaccinator", "Shared_Worker") ~ "Behavioral",
        Hazard %in% c("Covert_Sale_Suspicious_Flock", "Manure_Unknown_Destination") ~ "Economic",
        Hazard %in% c("Contaminated_Crates", "Contaminated_PPE", "Unhygienic_Waste_Disposal") ~ "Fomite",
        Hazard %in% c("Wild_Bird_Contact", "Market_Poultry_Mixing") ~ "Biological",
        Hazard %in% c("Entry_Buyer_Pullet") ~ "Structural", TRUE ~ "Managerial"),
      H = case_when(
        Hazard %in% c("Mortality_Not_Reported", "Shared_Vaccinator", "Covert_Sale_Suspicious_Flock") ~ 
          sample(4:5, n(), replace=TRUE, prob=c(0.3,0.7)),
        Hazard %in% c("Entry_Buyer_Pullet", "Shared_Worker", "Wild_Bird_Contact", "Market_Poultry_Mixing") ~ 
          sample(3:5, n(), replace=TRUE, prob=c(0.2,0.4,0.4)),
        Hazard %in% c("Vehicle_Disinfection_Inadequate", "Manure_Unknown_Destination", "Contaminated_Crates") ~ 
          sample(2:4, n(), replace=TRUE, prob=c(0.2,0.5,0.3)),
        TRUE ~ sample(2:4, n(), replace=TRUE, prob=c(0.3,0.4,0.3))),
      E = case_when(
        Chain_Segment %in% c("Broiler", "Layer", "Live_Bird_Market") ~ 
          sample(4:5, n(), replace=TRUE, prob=c(0.2,0.8)),
        Chain_Segment %in% c("Pullet", "Breeder", "Transport") ~ 
          sample(3:5, n(), replace=TRUE, prob=c(0.3,0.4,0.3)),
        Chain_Segment %in% c("Hatchery", "Slaughterhouse") ~ 
          sample(2:4, n(), replace=TRUE, prob=c(0.2,0.5,0.3)),
        TRUE ~ sample(2:4, n(), replace=TRUE, prob=c(0.3,0.4,0.3))),
      C = case_when(
        Chain_Segment %in% c("Broiler", "Layer", "Breeder") ~ 
          sample(4:5, n(), replace=TRUE, prob=c(0.3,0.7)),
        Chain_Segment %in% c("Pullet", "Live_Bird_Market") ~ 
          sample(3:5, n(), replace=TRUE, prob=c(0.3,0.4,0.3)),
        Chain_Segment %in% c("Slaughterhouse", "Transport") ~ 
          sample(2:4, n(), replace=TRUE, prob=c(0.2,0.4,0.4)),
        TRUE ~ sample(2:4, n(), replace=TRUE, prob=c(0.3,0.4,0.3))),
      R = case_when(
        Chain_Segment %in% c("Broiler", "Layer") ~ 
          sample(1:3, n(), replace=TRUE, prob=c(0.4,0.4,0.2)),
        Chain_Segment %in% c("Live_Bird_Market", "Transport") ~ 
          sample(1:2, n(), replace=TRUE, prob=c(0.6,0.4)),
        Chain_Segment %in% c("Pullet", "Breeder") ~ 
          sample(2:4, n(), replace=TRUE, prob=c(0.2,0.4,0.4)),
        TRUE ~ sample(2:4, n(), replace=TRUE, prob=c(0.3,0.4,0.3))),
      Connections = CONNECTIVITY_MAP[Chain_Segment]) %>%
    mutate(
      Risk_Score = round((H * E * C) / R, 2),
      Risk_Level = case_when(Risk_Score >= 250 ~ "Critical", Risk_Score >= 125 ~ "High", 
                             Risk_Score >= 50 ~ "Moderate", TRUE ~ "Low"),
      Intervention_Type = case_when(Risk_Level == "Critical" ~ "Emergency", 
                                    Risk_Level == "High" ~ "Priority",
                                    Risk_Level == "Moderate" ~ "Regular", TRUE ~ "Routine")) %>%
    mutate(Row = row_number(), .before = Chain_Segment) %>%
    dplyr::select(Row, Chain_Segment, Hazard, Hazard_Type, H, E, C, R, Connections, 
                  Risk_Score, Risk_Level, Intervention_Type)
  
  sheet2_data <- data.frame(City_Name = "Enter City Name Here")
  weights_data <- data.frame(Criterion = c("Critical_Percent", "Mean_Resilience", "Risk_Density", 
                                           "Connections", "Unit_Count", "Total_Capacity"),
                             Weight = c(0.25, 0.15, 0.25, 0.15, 0.10, 0.10),
                             Criterion_Type = c("Beneficial", "Non_Beneficial", "Beneficial", 
                                                "Beneficial", "Beneficial", "Beneficial"))
  unit_data_input <- data.frame(Row = 1:10, Unit_Name = paste("Unit", 1:10), 
                                Chain_Segment = sample(chain_stages, 10, replace=TRUE),
                                H = sample(2:5, 10, replace=TRUE), E = sample(2:5, 10, replace=TRUE), 
                                C = sample(2:5, 10, replace=TRUE), R = round(runif(10, 0.5, 3), 1), 
                                Risk_Score = round(runif(10, 50, 300), 2),
                                Connections = sample(2:10, 10, replace=TRUE), 
                                Total_Capacity = round(runif(10, 1000, 50000), 0),
                                X = round(runif(10, 44, 48), 4), Y = round(runif(10, 25, 30), 4))
  unit_weights_data <- data.frame(Criterion = c("Risk_Score", "Connections", "Total_Capacity", "Chain_MCDA_Score"),
                                  Weight = c(0.30, 0.20, 0.20, 0.30),
                                  Criterion_Type = c("Beneficial", "Beneficial", "Beneficial", "Beneficial"))
  spatial_weights_data <- data.frame(Criterion = c("Road_Norm", "Wetland_Norm", "Density_Norm"), 
                                     Weight = c(0.40, 0.30, 0.30),
                                     Criterion_Type = c("Beneficial", "Beneficial", "Beneficial"))
  
  write_xlsx(list(Sheet1 = template_data, Sheet2 = sheet2_data, Sheet3 = weights_data, 
                  Sheet4 = unit_data_input, Sheet5 = unit_weights_data, 
                  Sheet6 = spatial_weights_data), file_path)
  return(file_path)
}

# ============================================================
# Part 6: Load Data and Weights
# ============================================================

load_weights_from_excel <- function(file_path, sheet_name) {
  if (!file.exists(file_path)) { create_input_template(file_path); stop("Template created. Please add data and re-run.") }
  all_sheets <- excel_sheets(file_path)
  if (!sheet_name %in% all_sheets) {
    alt_names <- c("Sheet3", "Sheet5", "Sheet6", "Sheet3_Chain_Weights", 
                   "Sheet5_Unit_Weights", "Sheet6_Spatial_Weights")
    for (alt in alt_names) { if (alt %in% all_sheets) { sheet_name <- alt; break } }
    if (!sheet_name %in% all_sheets) stop("Sheet not found. Available: ", paste(all_sheets, collapse = ", "))
  }
  
  weights_data <- read_excel(file_path, sheet = sheet_name)
  if (!"Criterion_Type" %in% colnames(weights_data)) {
    default_types <- c("Risk_Score"="Beneficial", "Connections"="Beneficial", "Total_Capacity"="Beneficial", 
                       "Chain_MCDA_Score"="Beneficial", "Critical_Percent"="Beneficial", 
                       "Mean_Resilience"="Non_Beneficial", "Risk_Density"="Beneficial",
                       "Unit_Count"="Beneficial", "Road_Norm"="Beneficial", 
                       "Wetland_Norm"="Beneficial", "Density_Norm"="Beneficial")
    weights_data$Criterion_Type <- default_types[weights_data$Criterion]
  }
  
  required_cols <- c("Criterion", "Weight", "Criterion_Type")
  missing_cols <- setdiff(required_cols, colnames(weights_data))
  if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  
  weights <- weights_data$Weight
  if (abs(sum(weights) - 1.0) > 0.01) weights <- weights / sum(weights)
  names(weights) <- weights_data$Criterion
  criteria_types <- setNames(weights_data$Criterion_Type, weights_data$Criterion)
  return(list(Weights = weights, Criteria_Types = criteria_types))
}

load_and_validate_data <- function(file_path) {
  if (!file.exists(file_path)) { create_input_template(file_path); stop("Template created. Please add data and re-run.") }
  all_sheets <- excel_sheets(file_path)
  sheet_name <- NULL
  for (name in c("Sheet1", "01_Raw_Data", "Sheet1_Risk_Matrix")) { 
    if (name %in% all_sheets) { sheet_name <- name; break } 
  }
  if (is.null(sheet_name)) stop("No data sheet found. Available: ", paste(all_sheets, collapse = ", "))
  
  raw_data <- read_excel(file_path, sheet = sheet_name)
  required_cols <- c("Chain_Segment", "Hazard", "Hazard_Type", "H", "E", "C", "R")
  missing_cols <- setdiff(required_cols, colnames(raw_data))
  if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  
  if (!"Connections" %in% colnames(raw_data)) {
    raw_data$Connections <- CONNECTIVITY_MAP[raw_data$Chain_Segment]
  }
  
  raw_data <- raw_data %>% 
    mutate(H = as.numeric(H), E = as.numeric(E), C = as.numeric(C), R = as.numeric(R), 
           Connections = as.numeric(Connections),
           Chain_Segment = as.character(Chain_Segment), Hazard = as.character(Hazard), 
           Hazard_Type = as.character(Hazard_Type))
  
  if (any(raw_data$H < 1 | raw_data$H > 5, na.rm=TRUE) || any(raw_data$E < 1 | raw_data$E > 5, na.rm=TRUE) ||
      any(raw_data$C < 1 | raw_data$C > 5, na.rm=TRUE) || any(raw_data$R < 0.5 | raw_data$R > 5, na.rm=TRUE))
    stop("H, E, C must be 1-5, R must be 0.5-5")
  
  raw_data <- raw_data %>% filter(!is.na(H) & !is.na(E) & !is.na(C) & !is.na(R)) %>%
    rowwise() %>% mutate(Risk_Score = round((H * E * C) / R, 2)) %>% ungroup() %>%
    mutate(
      Risk_Level = case_when(Risk_Score >= 125 ~ "Critical", Risk_Score >= 50 ~ "High",
                             Risk_Score >= 25 ~ "Moderate", TRUE ~ "Low"),
      Intervention_Type = case_when(Risk_Level == "Critical" ~ "Emergency", 
                                    Risk_Level == "High" ~ "Priority",
                                    Risk_Level == "Moderate" ~ "Regular", TRUE ~ "Routine"),
      Priority_Score = Risk_Score * (5 - R) / 5,
      Vulnerability = round((E + C) / 2, 2)
    )
  if (!"Row" %in% colnames(raw_data)) raw_data <- raw_data %>% mutate(Row = row_number(), .before = Chain_Segment)
  return(list(Data = raw_data))
}

read_city_name <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  all_sheets <- excel_sheets(file_path)
  sheet_name <- NULL
  for (name in c("Sheet2", "Sheet2_City")) { if (name %in% all_sheets) { sheet_name <- name; break } }
  if (is.null(sheet_name)) return(NULL)
  sheet2 <- tryCatch(read_excel(file_path, sheet = sheet_name), error = function(e) NULL)
  if (is.null(sheet2) || nrow(sheet2) == 0 || ncol(sheet2) == 0) return(NULL)
  city_name <- as.character(sheet2[1, 1])
  if (is.na(city_name) || trimws(city_name) == "" || city_name %in% c("Enter City Name Here", "City_Name")) return(NULL)
  return(city_name)
}

filter_units_by_city <- function(units_file_path, city_name) {
  if (!file.exists(units_file_path)) return(NULL)
  units_data <- tryCatch(read_excel(units_file_path, sheet = 1), error = function(e) NULL)
  if (is.null(units_data) || nrow(units_data) == 0) return(NULL)
  
  col_names <- colnames(units_data)
  city_col_idx <- grep("city|location|region|area", col_names, ignore.case = TRUE)
  if (length(city_col_idx) == 0) {
    if (ncol(units_data) >= 8) { city_col_idx <- 8 } 
    else { return(NULL) }
  }
  
  city_col <- city_col_idx[1]
  units_data[[city_col]] <- as.character(units_data[[city_col]])
  
  clean_name <- function(x) gsub(" ", "", trimws(as.character(x)))
  units_data$City_Clean <- clean_name(units_data[[city_col]])
  city_clean <- clean_name(city_name)
  
  filtered_data <- units_data %>% filter(City_Clean == city_clean)
  if (nrow(filtered_data) == 0) {
    filtered_data <- units_data %>% filter(tolower(units_data[[city_col]]) == tolower(city_name))
  }
  if (nrow(filtered_data) == 0) return(NULL)
  
  xy_idx <- get_xy_indices(col_names)
  if (is.null(xy_idx)) return(NULL)
  
  filtered_data$X <- convert_coord(filtered_data[[xy_idx["x"]]])
  filtered_data$Y <- convert_coord(filtered_data[[xy_idx["y"]]])
  
  valid_coords <- !is.na(filtered_data$X) & !is.na(filtered_data$Y)
  if (sum(valid_coords) == 0) return(NULL)
  filtered_data <- filtered_data[valid_coords, ]
  
  return(filtered_data)
}

# ============================================================
# Part 7: MCDA Functions
# ============================================================

apply_mcda <- function(data, weights_info, criteria_cols) {
  weights <- weights_info$Weights
  criteria_types <- weights_info$Criteria_Types
  available_weights <- weights[names(weights) %in% criteria_cols]
  available_weights <- available_weights[available_weights > 0]
  
  if (length(available_weights) == 0) {
    available_weights <- setNames(rep(1/length(criteria_cols), length(criteria_cols)), criteria_cols)
  }
  
  for (criterion in names(available_weights)) {
    if (criterion %in% colnames(data)) {
      min_val <- min(data[[criterion]], na.rm = TRUE)
      max_val <- max(data[[criterion]], na.rm = TRUE)
      if (max_val > min_val) {
        if (criteria_types[criterion] == "Beneficial") {
          data[[paste0(criterion, "_Norm")]] <- (data[[criterion]] - min_val) / (max_val - min_val)
        } else {
          data[[paste0(criterion, "_Norm")]] <- 1 - ((data[[criterion]] - min_val) / (max_val - min_val))
        }
      } else {
        data[[paste0(criterion, "_Norm")]] <- 0.5
      }
    }
  }
  
  data$MCDA_Score <- 0
  total_weight <- 0
  for (criterion in names(available_weights)) {
    norm_col <- paste0(criterion, "_Norm")
    if (norm_col %in% colnames(data)) {
      data$MCDA_Score <- data$MCDA_Score + (data[[norm_col]] * available_weights[criterion])
      total_weight <- total_weight + available_weights[criterion]
    }
  }
  if (total_weight > 0) data$MCDA_Score <- data$MCDA_Score / total_weight
  data$MCDA_Score <- round(data$MCDA_Score, 4)
  
  data <- data %>% arrange(desc(MCDA_Score))
  data$MCDA_Rank <- seq_len(nrow(data))
  data$Risk_Level_MCDA <- case_when(
    data$MCDA_Score >= 0.7 ~ "Critical",
    data$MCDA_Score >= 0.5 ~ "High",
    data$MCDA_Score >= 0.3 ~ "Moderate",
    TRUE ~ "Low"
  )
  
  return(data)
}

calculate_chain_summary <- function(data, filtered_units_data = NULL, weights_info = NULL) {
  data <- data %>% mutate(Chain_Segment_Norm = normalize_persian_name(Chain_Segment))
  
  if (!is.null(filtered_units_data) && nrow(filtered_units_data) > 0) {
    filtered_units_clean <- filtered_units_data %>%
      mutate(Unit_Type = as.character(.[[5]]),
             Unit_Type_Norm = normalize_persian_name(Unit_Type),
             Capacity = as.numeric(as.character(.[[10]]))) %>%
      filter(!is.na(Capacity))
    
    unit_counts <- filtered_units_clean %>%
      group_by(Chain_Segment = Unit_Type_Norm) %>%
      summarise(Unit_Count = n(), Total_Capacity = sum(Capacity, na.rm = TRUE)) %>%
      ungroup()
  }
  
  chain_connections <- data %>%
    group_by(Chain_Segment = Chain_Segment_Norm) %>%
    summarise(Connections = sum(Connections, na.rm = TRUE)) %>%
    ungroup()
  
  summary_data <- data %>%
    group_by(Chain_Segment = Chain_Segment_Norm) %>%
    summarise(
      Rank = n(), Hazard_Count = n_distinct(Hazard),
      Median_Risk = round(median(Risk_Score, na.rm = TRUE), 2),
      Max_Risk = round(max(Risk_Score, na.rm = TRUE), 2),
      SD_Risk = round(sd(Risk_Score, na.rm = TRUE), 2),
      CV_Risk = round(sd(Risk_Score, na.rm = TRUE) / mean(Risk_Score, na.rm = TRUE) * 100, 2),
      Critical_Percent = round(sum(Risk_Level == "Critical", na.rm = TRUE) / n() * 100, 2),
      Mean_Vulnerability = round(mean(Vulnerability, na.rm = TRUE), 2),
      Mean_Resilience = round(mean(R, na.rm = TRUE), 2),
      Risk_Density = round(mean(Risk_Score, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    left_join(chain_connections, by = "Chain_Segment") %>%
    left_join(unit_counts, by = "Chain_Segment") %>%
    mutate(Unit_Count = ifelse(is.na(Unit_Count), 0, Unit_Count),
           Total_Capacity = ifelse(is.na(Total_Capacity), 0, Total_Capacity))
  
  if (!is.null(weights_info)) {
    chain_criteria <- c("Critical_Percent", "Mean_Resilience", "Risk_Density",
                        "Connections", "Unit_Count", "Total_Capacity")
    summary_data <- apply_mcda(summary_data, weights_info, chain_criteria)
  }
  
  name_mapping <- data %>%
    distinct(Chain_Segment_Norm, Chain_Segment_Original = Chain_Segment) %>%
    group_by(Chain_Segment_Norm) %>%
    summarise(Chain_Segment_Original = first(Chain_Segment_Original)) %>%
    ungroup()
  
  summary_data <- summary_data %>%
    left_join(name_mapping, by = c("Chain_Segment" = "Chain_Segment_Norm")) %>%
    mutate(Chain_Segment = ifelse(is.na(Chain_Segment_Original), Chain_Segment, Chain_Segment_Original)) %>%
    dplyr::select(-Chain_Segment_Original)
  
  return(summary_data)
}

analyze_unit_mcda <- function(file_path, weights_info, map_data = NULL, chain_summary = NULL) {
  unit_data_input <- read_excel(file_path, sheet = "Sheet4")
  required_cols <- c("Row", "Unit_Name", "Chain_Segment", "H", "E", "C", "R", "Risk_Score", "Connections", "Total_Capacity")
  missing_cols <- setdiff(required_cols, colnames(unit_data_input))
  if (length(missing_cols) > 0) return(NULL)
  
  unit_data <- unit_data_input %>%
    mutate(H = as.numeric(H), E = as.numeric(E), C = as.numeric(C), R = as.numeric(R),
           Risk_Score = as.numeric(Risk_Score), Connections = as.numeric(Connections),
           Total_Capacity = as.numeric(Total_Capacity)) %>%
    filter(!is.na(Risk_Score) & !is.na(Connections) & !is.na(Total_Capacity))
  
  if (!is.null(chain_summary) && "MCDA_Score" %in% colnames(chain_summary)) {
    chain_mcda_scores <- chain_summary %>%
      dplyr::select(Chain_Segment, Chain_MCDA_Score = MCDA_Score, Chain_MCDA_Rank = MCDA_Rank)
    unit_data <- unit_data %>% left_join(chain_mcda_scores, by = "Chain_Segment")
    if (any(is.na(unit_data$Chain_MCDA_Score))) {
      mean_score <- mean(unit_data$Chain_MCDA_Score, na.rm = TRUE)
      unit_data$Chain_MCDA_Score[is.na(unit_data$Chain_MCDA_Score)] <- mean_score
    }
  } else {
    unit_data$Chain_MCDA_Score <- unit_data$Risk_Score / max(unit_data$Risk_Score, na.rm = TRUE)
    unit_data$Chain_MCDA_Rank <- rank(desc(unit_data$Chain_MCDA_Score))
  }
  
  if (!is.null(map_data) && nrow(map_data) > 0) {
    map_cols <- colnames(map_data)
    needed_cols <- c("Unit_Name", "X", "Y", "Unit_Type", "Is_High_Risk")
    available_cols <- needed_cols[needed_cols %in% map_cols]
    if (length(available_cols) > 0) {
      unit_data <- unit_data %>% left_join(map_data %>% dplyr::select(all_of(available_cols)), by = "Unit_Name")
    }
  }
  
  unit_criteria <- c("Risk_Score", "Connections", "Total_Capacity", "Chain_MCDA_Score")
  unit_data <- apply_mcda(unit_data, weights_info, unit_criteria)
  
  return(unit_data)
}

analyze_spatial_unit_mcda <- function(file_path, weights_info, map_data = NULL, 
                                      roads_data = NULL, wetlands_data = NULL,
                                      chain_summary = NULL, city_boundary = NULL) {
  if (!dir.exists("Output")) dir.create("Output")
  log_file <- "Output/spatial_mcda_debug.log"
  write(paste("=== Spatial MCDA Debug Log ===", Sys.time(), "\n"), log_file)
  
  debug_log <- function(msg) { write(paste(msg, "\n"), log_file, append = TRUE) }
  
  if (is.null(map_data) || nrow(map_data) == 0) {
    debug_log("ERROR: No map_data provided")
    return(NULL)
  }
  
  unit_data <- map_data %>%
    mutate(Row = row_number(), Unit_Name = as.character(Unit_Name), 
           Chain_Segment = as.character(Unit_Type), X = as.numeric(X), Y = as.numeric(Y)) %>%
    filter(!is.na(X) & !is.na(Y))
  
  if (nrow(unit_data) == 0) {
    debug_log("ERROR: No valid coordinates in map_data")
    return(NULL)
  }
  
  units_sf <- tryCatch(st_as_sf(unit_data, coords = c("X", "Y"), crs = 4326, remove = FALSE), 
                       error = function(e) NULL)
  if (is.null(units_sf)) {
    debug_log("ERROR: Could not convert to sf object")
    return(NULL)
  }
  
  nearest_distance <- function(units_sf, feature_data, default_value = 10000, simplify_above = NULL) {
    if (is.null(feature_data) || nrow(feature_data) == 0) return(rep(default_value, nrow(units_sf)))
    tryCatch({
      if (st_crs(feature_data) != st_crs(4326)) feature_data <- st_transform(feature_data, 4326)
      feature_data <- feature_data[!st_is_empty(feature_data), ]
      if (nrow(feature_data) == 0) return(rep(default_value, nrow(units_sf)))
      if (!is.null(simplify_above) && nrow(feature_data) > simplify_above) {
        feature_data <- st_simplify(feature_data, dTolerance = 0.001, preserveTopology = FALSE)
        feature_data <- feature_data[!st_is_empty(feature_data), ]
      }
      dist_matrix <- st_distance(units_sf, feature_data)
      min_dist <- apply(dist_matrix, 1, min, na.rm = TRUE)
      finite_vals <- min_dist[is.finite(min_dist)]
      if (length(finite_vals) > 0) min_dist[!is.finite(min_dist)] <- max(finite_vals) 
      else min_dist[!is.finite(min_dist)] <- default_value
      as.numeric(min_dist)
    }, error = function(e) rep(default_value, nrow(units_sf)))
  }
  
  unit_data$Dist_Road_m <- nearest_distance(units_sf, roads_data, default_value = 10000, simplify_above = 10000)
  unit_data$Dist_Wetland_m <- nearest_distance(units_sf, wetlands_data, default_value = 10000, simplify_above = 5000)
  
  tryCatch({
    if (nrow(unit_data) >= 3) {
      dens_estimate <- MASS::kde2d(unit_data$X, unit_data$Y, n = 100, h = c(0.05, 0.05))
      x_idx <- findInterval(unit_data$X, dens_estimate$x, all.inside = TRUE)
      y_idx <- findInterval(unit_data$Y, dens_estimate$y, all.inside = TRUE)
      unit_data$Density <- round(dens_estimate$z[cbind(x_idx, y_idx)], 6)
    } else {
      unit_data$Density <- runif(nrow(unit_data), 0, 0.01)
    }
  }, error = function(e) { unit_data$Density <- runif(nrow(unit_data), 0, 0.01) })
  
  unit_data$Road_Norm <- minmax_norm(unit_data$Dist_Road_m, invert = TRUE)
  unit_data$Wetland_Norm <- minmax_norm(unit_data$Dist_Wetland_m, invert = TRUE)
  unit_data$Density_Norm <- minmax_norm(unit_data$Density, invert = FALSE)
  
  default_weights <- c(Road_Norm = 0.40, Wetland_Norm = 0.30, Density_Norm = 0.30)
  final_weights <- default_weights
  
  if (!is.null(weights_info)) {
    available_weights <- weights_info$Weights[names(weights_info$Weights) %in% names(default_weights)]
    available_weights <- available_weights[available_weights > 0]
    if (length(available_weights) == 3) {
      final_weights <- available_weights
      if (abs(sum(final_weights) - 1.0) > 0.01) final_weights <- final_weights / sum(final_weights)
    }
  }
  
  unit_data$Spatial_MCDA_Score <- 0
  total_weight <- 0
  for (criterion in names(final_weights)) {
    if (criterion %in% colnames(unit_data)) {
      unit_data$Spatial_MCDA_Score <- unit_data$Spatial_MCDA_Score + (unit_data[[criterion]] * final_weights[criterion])
      total_weight <- total_weight + final_weights[criterion]
    }
  }
  if (total_weight > 0) unit_data$Spatial_MCDA_Score <- unit_data$Spatial_MCDA_Score / total_weight
  unit_data$Spatial_MCDA_Score <- round(unit_data$Spatial_MCDA_Score, 4)
  
  unit_data <- unit_data %>% arrange(desc(Spatial_MCDA_Score))
  unit_data$Spatial_MCDA_Rank <- seq_len(nrow(unit_data))
  unit_data$Spatial_Risk_Level <- case_when(
    unit_data$Spatial_MCDA_Score >= 0.7 ~ "Critical",
    unit_data$Spatial_MCDA_Score >= 0.5 ~ "High",
    unit_data$Spatial_MCDA_Score >= 0.3 ~ "Moderate",
    TRUE ~ "Low"
  )
  
  output_cols <- c("Row", "Unit_Name", "Chain_Segment", "X", "Y", "Unit_Type", "Is_High_Risk",
                   "Dist_Road_m", "Dist_Wetland_m", "Density", "Road_Norm", "Wetland_Norm",
                   "Density_Norm", "Spatial_MCDA_Score", "Spatial_MCDA_Rank", "Spatial_Risk_Level")
  output_cols <- output_cols[output_cols %in% colnames(unit_data)]
  unit_data <- unit_data %>% dplyr::select(all_of(output_cols))
  
  debug_log(paste("Spatial MCDA completed with", nrow(unit_data), "units"))
  return(unit_data)
}

# ============================================================
# Part 8: Map Creation Functions
# ============================================================

create_geographic_map <- function(filtered_units_data, chain_summary, city_name,
                                  iran_data, provinces_data, roads_data, wetlands_data) {
  if (is.null(filtered_units_data) || nrow(filtered_units_data) == 0) return(NULL)
  
  highest_risk_chain <- if ("MCDA_Rank" %in% colnames(chain_summary))
    chain_summary$Chain_Segment[chain_summary$MCDA_Rank == 1][1] else chain_summary$Chain_Segment[1]
  
  map_data <- filtered_units_data %>%
    mutate(X = convert_coord(.[[19]]), Y = convert_coord(.[[20]]),
           Unit_Type = as.character(.[[5]]), Unit_Name = as.character(.[[2]]),
           Is_High_Risk = Unit_Type == highest_risk_chain) %>%
    filter(!is.na(X) & !is.na(Y) & !is.na(Unit_Type))
  if (nrow(map_data) == 0) return(NULL)
  
  regular_units <- setdiff(unique(map_data$Unit_Type), highest_risk_chain)
  all_unit_types <- unique(map_data$Unit_Type)
  palette <- build_shape_color_map(all_unit_types)
  
  x_min <- min(map_data$X) - 0.5; x_max <- max(map_data$X) + 0.5
  y_min <- min(map_data$Y) - 0.5; y_max <- max(map_data$Y) + 0.5
  
  p <- ggplot() +
    {if (!is.null(iran_data)) geom_sf(data = iran_data, fill = "#E8F4FD", color = "#1a237e", size = 1.5)} +
    {if (!is.null(provinces_data) && nrow(provinces_data) > 0) 
      geom_sf(data = provinces_data, fill = NA, color = "#1565C0", size = 0.8)} +
    {if (!is.null(wetlands_data) && nrow(wetlands_data) > 0) 
      geom_sf(data = wetlands_data, fill = "#4FC3F7", color = "#0288D1", alpha = 0.5, size = 0.3)} +
    {if (!is.null(roads_data) && nrow(roads_data) > 0) 
      geom_sf(data = roads_data, color = "#FF8F00", size = 0.5, alpha = 0.6)} +
    geom_point(data = subset(map_data, Unit_Type %in% regular_units),
               aes(x = X, y = Y, color = Unit_Type, shape = Unit_Type),
               size = 0.8, alpha = 0.9, stroke = 0.1) +
    geom_point(data = subset(map_data, Is_High_Risk == TRUE),
               aes(x = X, y = Y, shape = Unit_Type),
               color = "red", size = 1.2, stroke = 0.5, fill = "red") +
    geom_text_repel(data = map_data,
                    aes(x = X, y = Y, label = Unit_Name),
                    size = 1.2, color = "black", box.padding = 0.08,
                    point.padding = 0.02, max.overlaps = 60,
                    segment.color = NA, segment.size = 0,
                    fontface = "plain", lineheight = 0.4) +
    scale_shape_manual(values = palette$shapes, name = "Unit Type") +
    scale_color_manual(values = palette$colors, name = "Unit Type") +
    guides(shape = guide_legend(override.aes = list(size = 2.0)),
           color = guide_legend(override.aes = list(size = 2.0))) +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right", legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10), legend.key.size = unit(0.5, "cm"),
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray40"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray90", size = 0.2),
          axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
    labs(title = paste("Geographic Distribution -", city_name),
         subtitle = paste("Red highlighted:", highest_risk_chain, "(highest risk chain - MCDA)"),
         x = "Longitude", y = "Latitude")
  
  tryCatch({ p <- p + annotation_scale(location = "br", width_hint = 0.2) }, error = function(e) {})
  
  leaflet_map <- if (nrow(map_data) > 0)
    create_interactive_map(map_data, highest_risk_chain, city_name, provinces_data, roads_data, wetlands_data) else NULL
  
  return(list(Map = p, Leaflet_Map = leaflet_map, Map_Data = map_data, Highest_Risk_Chain = highest_risk_chain))
}

create_interactive_map <- function(map_data, highest_risk_chain, city_name, provinces_data, roads_data, wetlands_data) {
  unit_types <- unique(map_data$Unit_Type)
  regular_units <- setdiff(unit_types, highest_risk_chain)
  all_unit_types <- c(regular_units, highest_risk_chain)
  
  palette <- build_shape_color_map(all_unit_types)
  color_pal <- palette$colors
  high_risk_color <- "red"
  unit_counts <- table(map_data$Unit_Type)
  
  m <- leaflet(map_data) %>%
    setView(lng = mean(map_data$X, na.rm = TRUE), lat = mean(map_data$Y, na.rm = TRUE), zoom = 10) %>%
    add_leaflet_tiles()
  
  ctx <- add_leaflet_context_layers(m, provinces_data, roads_data, wetlands_data,
                                    bbox = st_bbox(c(xmin = min(map_data$X) - 0.5, xmax = max(map_data$X) + 0.5,
                                                     ymin = min(map_data$Y) - 0.5, ymax = max(map_data$Y) + 0.5), 
                                                   crs = st_crs(4326)),
                                    road_color = "#FF8F00", road_weight = 2)
  m <- ctx$map
  context_groups <- ctx$groups
  label_opts <- create_leaflet_label_options()
  
  for (unit_type in regular_units) {
    subset_data <- map_data[map_data$Unit_Type == unit_type, ]
    color <- color_pal[unit_type]
    m <- m %>% addCircleMarkers(
      data = subset_data, lng = ~X, lat = ~Y, radius = 6,
      color = color, fillColor = color, fillOpacity = 0.9, weight = 0.5,
      popup = paste("<b>Unit:</b>", subset_data$Unit_Name, "<br>",
                    "<b>Type:</b>", unit_type, "<br>",
                    "<b>Coordinates:</b>", round(subset_data$X, 4), ",", round(subset_data$Y, 4)),
      label = ~paste(Unit_Name, "\n", unit_type, sep = ""),
      group = unit_type, labelOptions = label_opts)
  }
  
  high_risk_data <- subset(map_data, Is_High_Risk == TRUE)
  if (nrow(high_risk_data) > 0) {
    m <- m %>% addCircleMarkers(
      data = high_risk_data, lng = ~X, lat = ~Y, radius = 7,
      color = high_risk_color, fillColor = high_risk_color, fillOpacity = 1, weight = 0.8,
      popup = paste("<b>Unit:</b>", high_risk_data$Unit_Name, "<br>",
                    "<b>Type:</b>", highest_risk_chain, "<br>",
                    "<b>Coordinates:</b>", round(high_risk_data$X, 4), ",", round(high_risk_data$Y, 4), "<br>",
                    "Highest Risk Chain (MCDA)"),
      label = ~paste(Unit_Name, "\n", highest_risk_chain, " \u2605", sep = ""),
      group = "Highest Risk", labelOptions = create_leaflet_label_options("red", "red"))
  }
  
  overlay_groups <- c(regular_units, context_groups[context_groups %in% c("Province Borders", "Roads", "Wetlands")], "Highest Risk")
  m <- m %>% addLayersControl(baseGroups = TILE_NAMES, overlayGroups = overlay_groups,
                              options = layersControlOptions(collapsed = FALSE), position = "topleft")
  
  legend_colors <- c(); legend_labels <- c()
  for (unit_type in regular_units) {
    legend_colors <- c(legend_colors, color_pal[unit_type])
    legend_labels <- c(legend_labels, paste(palette$symbols[unit_type], unit_type, " (n=", unit_counts[unit_type], ")", sep = ""))
  }
  if (nrow(high_risk_data) > 0) {
    legend_colors <- c(legend_colors, high_risk_color)
    legend_labels <- c(legend_labels, paste("\u2605", highest_risk_chain, "(Highest Risk - MCDA)"))
  }
  if ("Roads" %in% context_groups) { legend_colors <- c(legend_colors, "#FF8F00"); legend_labels <- c(legend_labels, "Roads") }
  if ("Wetlands" %in% context_groups) { legend_colors <- c(legend_colors, "#4FC3F7"); legend_labels <- c(legend_labels, "Wetlands") }
  if ("Province Borders" %in% context_groups) { legend_colors <- c(legend_colors, "#1565C0"); legend_labels <- c(legend_labels, "Province Borders") }
  
  if (length(legend_colors) > 0) {
    m <- m %>% addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels,
                         title = "Legend", opacity = 0.9)
  }
  
  return(m)
}

create_mcda_ranking_plot <- function(data, title, subtitle, x_label, score_col = "MCDA_Score", 
                                     rank_col = "MCDA_Rank", name_col = "Chain_Segment", 
                                     level_col = "Risk_Level_MCDA", top_n = NULL) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  plot_data <- data %>% arrange(desc(.data[[score_col]]))
  if (!is.null(top_n) && top_n < nrow(plot_data)) plot_data <- head(plot_data, top_n)
  plot_data <- plot_data %>%
    mutate(Label = paste0(.data[[rank_col]], ": ", .data[[name_col]]),
           Label = factor(Label, levels = rev(Label)),
           Color_Level = case_when(.data[[level_col]] == "Critical" ~ "#8B0000", 
                                   .data[[level_col]] == "High" ~ "#F44336",
                                   .data[[level_col]] == "Moderate" ~ "#FF9800", 
                                   TRUE ~ "#4CAF50"))
  ggplot(plot_data, aes(x = Label, y = .data[[score_col]], fill = Color_Level)) +
    geom_col(alpha = 0.85) +
    geom_text(aes(label = paste0(round(.data[[score_col]] * 100, 1), "%"), hjust = -0.1), size = 3) +
    coord_flip() + scale_fill_identity() + theme_minimal() +
    theme(axis.text.y = element_text(size = 8), legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"),
          axis.title = element_text(size = 10)) +
    labs(title = title, subtitle = subtitle, x = x_label, y = "MCDA Score")
}

create_risk_heatmap <- function(unit_data, city_name, iran_data, provinces_data) {
  if (is.null(unit_data) || nrow(unit_data) == 0 || !all(c("X", "Y") %in% colnames(unit_data))) return(NULL)
  unit_data <- unit_data %>% filter(!is.na(X) & !is.na(Y))
  if (nrow(unit_data) == 0) return(NULL)
  if (!is.null(iran_data) && st_crs(iran_data) != st_crs(4326)) iran_data <- st_transform(iran_data, 4326)
  
  palette <- build_shape_color_map(unique(unit_data$Chain_Segment))
  
  x_min <- min(unit_data$X) - 0.3; x_max <- max(unit_data$X) + 0.3
  y_min <- min(unit_data$Y) - 0.3; y_max <- max(unit_data$Y) + 0.3
  
  p <- ggplot() +
    {if (!is.null(iran_data)) geom_sf(data = iran_data, fill = "#F5F5F5", color = "#1a237e", size = 1.2)} +
    {if (!is.null(provinces_data) && nrow(provinces_data) > 0) 
      geom_sf(data = provinces_data, fill = NA, color = "#1565C0", size = 0.6)} +
    geom_point(data = unit_data, aes(x = X, y = Y, color = Spatial_MCDA_Score, 
                                     size = Spatial_MCDA_Score, shape = Chain_Segment), alpha = 0.85) +
    geom_text_repel(data = unit_data,
                    aes(x = X, y = Y, label = paste0(Unit_Name, "\n", Chain_Segment)),
                    size = 1.6, color = "black", box.padding = 0.06,
                    point.padding = 0.04, max.overlaps = 30,
                    segment.color = "gray60", segment.size = 0.08,
                    fontface = "bold", lineheight = 0.6) +
    scale_color_gradient2(low = "#2E7D32", mid = "#FFD54F", high = "#D32F2F", 
                          midpoint = 0.5, name = "Spatial MCDA Score", labels = scales::percent) +
    scale_size_continuous(range = c(1.2, 2.5), guide = "none") +
    scale_shape_manual(values = palette$shapes, name = "Unit Type") +
    guides(shape = guide_legend(override.aes = list(size = 1.5)), 
           color = guide_legend(override.aes = list(size = 1.5))) +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right", legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 7), legend.key.size = unit(0.35, "cm"),
          plot.title = element_text(size = 14, face = "bold"), 
          plot.subtitle = element_text(size = 10, color = "gray40"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray90", size = 0.2)) +
    labs(title = paste("Spatial Risk Heatmap -", city_name), 
         subtitle = "Red = High Risk, Green = Low Risk", x = "Longitude", y = "Latitude")
  
  tryCatch({ p <- p + annotation_scale(location = "br", width_hint = 0.2) }, error = function(e) {})
  return(p)
}

create_kernel_density_map <- function(unit_data, city_name, iran_data, provinces_data) {
  if (is.null(unit_data) || nrow(unit_data) == 0 || !all(c("X", "Y") %in% colnames(unit_data))) return(NULL)
  unit_data <- unit_data %>% filter(!is.na(X) & !is.na(Y))
  if (nrow(unit_data) == 0) return(NULL)
  if (!is.null(iran_data) && st_crs(iran_data) != st_crs(4326)) iran_data <- st_transform(iran_data, 4326)
  if (!is.null(provinces_data) && nrow(provinces_data) > 0 && st_crs(provinces_data) != st_crs(4326)) 
    provinces_data <- st_transform(provinces_data, 4326)
  
  x_min <- min(unit_data$X) - 0.3; x_max <- max(unit_data$X) + 0.3
  y_min <- min(unit_data$Y) - 0.3; y_max <- max(unit_data$Y) + 0.3
  
  p <- ggplot() +
    {if (!is.null(iran_data)) geom_sf(data = iran_data, fill = "#F5F5F5", color = "#1a237e", size = 1.2)} +
    {if (!is.null(provinces_data) && nrow(provinces_data) > 0) 
      geom_sf(data = provinces_data, fill = NA, color = "#1565C0", size = 0.6)} +
    stat_density2d(data = unit_data, aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)),
                   geom = "polygon", contour = TRUE, n = 100, h = c(0.05, 0.05)) +
    stat_density2d(data = unit_data, aes(x = X, y = Y, color = after_stat(level)),
                   geom = "contour", n = 100, h = c(0.05, 0.05), size = 0.3, alpha = 0.5) +
    geom_point(data = unit_data, aes(x = X, y = Y), size = 1.2, alpha = 0.4, color = "black") +
    scale_fill_viridis_c(option = "inferno", name = "Unit Density", 
                         labels = scales::number_format(accuracy = 0.01)) +
    scale_color_viridis_c(option = "inferno", guide = "none") +
    scale_alpha_continuous(range = c(0.1, 0.6), guide = "none") +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right", legend.title = element_text(size = 10, face = "bold"),
          legend.text = element_text(size = 8), plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray90", size = 0.2),
          axis.title = element_text(size = 10), axis.text = element_text(size = 8)) +
    labs(title = paste("Kernel Density Map -", city_name), 
         subtitle = "Hotspots of poultry unit concentration", x = "Longitude", y = "Latitude")
  
  tryCatch({ p <- p + annotation_scale(location = "br", width_hint = 0.2) }, error = function(e) {})
  return(p)
}

calculate_mean_risk_ranking <- function(data) {
  data %>% group_by(Chain_Segment) %>%
    summarise(Mean_Risk = round(mean(Risk_Score, na.rm = TRUE), 2), Count = n(),
              SD_Risk = round(sd(Risk_Score, na.rm = TRUE), 2), 
              Min_Risk = round(min(Risk_Score, na.rm = TRUE), 2),
              Max_Risk = round(max(Risk_Score, na.rm = TRUE), 2), .groups = "drop") %>%
    arrange(desc(Mean_Risk)) %>%
    mutate(Rank = row_number(), 
           Risk_Level = case_when(Mean_Risk >= 250 ~ "Critical", Mean_Risk >= 125 ~ "High",
                                  Mean_Risk >= 50 ~ "Moderate", TRUE ~ "Low"))
}

extract_unique_values <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  raw_data <- read_excel(file_path, sheet = "Sheet1")
  required_cols <- c("Chain_Segment", "Hazard", "Hazard_Type")
  missing_cols <- setdiff(required_cols, colnames(raw_data))
  if (length(missing_cols) > 0) return(NULL)
  
  chain_segments <- raw_data %>% pull(Chain_Segment) %>% unique() %>% na.omit() %>% as.character()
  hazards <- raw_data %>% pull(Hazard) %>% unique() %>% na.omit() %>% as.character()
  hazard_types <- raw_data %>% pull(Hazard_Type) %>% unique() %>% na.omit() %>% as.character()
  
  return(list(Chain_Segments = chain_segments, Hazards = hazards, Hazard_Types = hazard_types,
              Total_Chains = length(chain_segments), Total_Hazards = length(hazards), 
              Total_Hazard_Types = length(hazard_types)))
}

# ============================================================
# Part 9: Export Results
# ============================================================

export_results <- function(data, mean_risk_ranking, chain_summary, unit_mcda_results,
                           spatial_unit_results, filtered_units_data, unique_values,
                           city_name, weights_chain, weights_unit, weights_spatial) {
  completed_data <- data %>% dplyr::select(Row, Chain_Segment, Hazard, Hazard_Type, H, E, C, R, 
                                           Connections, Risk_Score, Risk_Level, Intervention_Type, 
                                           Priority_Score, Vulnerability)
  
  unique_summary <- if (!is.null(unique_values)) {
    data.frame(Category = c("Chain Segments", "Hazards", "Hazard Types"),
               Count = c(unique_values$Total_Chains, unique_values$Total_Hazards, 
                         unique_values$Total_Hazard_Types),
               Items = c(paste(unique_values$Chain_Segments, collapse = ", "), 
                         paste(unique_values$Hazards, collapse = ", "), 
                         paste(unique_values$Hazard_Types, collapse = ", ")))
  } else data.frame(Category = "No data extracted", Count = 0, Items = "N/A")
  
  sheets_list <- list("01_Raw_Data" = data, "02_Completed_Data" = completed_data, 
                      "03_Mean_Risk_Ranking" = mean_risk_ranking,
                      "04_Chain_Summary" = chain_summary, "05_Unique_Values" = unique_summary)
  if (!is.null(filtered_units_data) && nrow(filtered_units_data) > 0) 
    sheets_list[["06_Filtered_Units"]] <- filtered_units_data
  if (!is.null(weights_chain)) sheets_list[["07_Chain_MCDA_Weights"]] <- weights_to_df(weights_chain)
  if (!is.null(unit_mcda_results) && nrow(unit_mcda_results) > 0) {
    sheets_list[["08_Unit_MCDA_Ranking"]] <- unit_mcda_results
    sheets_list[["09_Top_10_Units"]] <- head(unit_mcda_results, 10)
  }
  if (!is.null(weights_unit)) sheets_list[["10_Unit_MCDA_Weights"]] <- weights_to_df(weights_unit)
  
  if (!is.null(spatial_unit_results) && nrow(spatial_unit_results) > 0) {
    sheets_list[["11_Spatial_Unit_MCDA"]] <- spatial_unit_results
  } else {
    sheets_list[["11_Spatial_Unit_MCDA"]] <- data.frame(Message = "Spatial MCDA could not be calculated", 
                                                        Reason = "Check Output/spatial_mcda_debug.log for details", 
                                                        Timestamp = Sys.time())
  }
  if (!is.null(weights_spatial)) sheets_list[["12_Spatial_Weights"]] <- weights_to_df(weights_spatial)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0("Output/HPAI_Analysis_Results_", timestamp, ".xlsx")
  if (!dir.exists("Output")) dir.create("Output")
  write_xlsx(sheets_list, output_file)
  return(output_file)
}

# ============================================================
# Part 10: County Heatmap
# ============================================================

create_county_heatmap <- function(spatial_unit_results, city_name, iran_data,
                                  provinces_data, roads_data, wetlands_data,
                                  city_boundary = NULL) {
  
  if (is.null(spatial_unit_results) || nrow(spatial_unit_results) == 0) return(NULL)
  
  spatial_data <- spatial_unit_results %>%
    filter(!is.na(X) & !is.na(Y) & !is.na(Spatial_MCDA_Score))
  
  if (nrow(spatial_data) == 0) return(NULL)
  
  if (!is.null(city_boundary)) {
    spatial_sf <- st_as_sf(spatial_data, coords = c("X", "Y"), crs = 4326)
    inside <- tryCatch(st_intersection(spatial_sf, city_boundary), error = function(e) NULL)
    if (!is.null(inside) && nrow(inside) > 0) {
      spatial_data <- inside %>%
        mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) %>%
        st_drop_geometry()
    }
  }
  
  if (nrow(spatial_data) == 0) return(NULL)
  
  unit_types <- unique(spatial_data$Chain_Segment)
  n_types <- length(unit_types)
  shape_values <- c(16, 17, 15, 18, 19, 20, 8, 4, 0, 2, 5, 6, 7)
  shape_map <- setNames(shape_values[1:n_types], unit_types)
  
  x_min <- min(spatial_data$X, na.rm = TRUE) - 0.05
  x_max <- max(spatial_data$X, na.rm = TRUE) + 0.05
  y_min <- min(spatial_data$Y, na.rm = TRUE) - 0.05
  y_max <- max(spatial_data$Y, na.rm = TRUE) + 0.05
  bbox <- st_bbox(c(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max), crs = 4326)
  
  roads_display <- NULL
  if (!is.null(roads_data) && nrow(roads_data) > 0) {
    roads_display <- roads_data
    if (nrow(roads_display) > 5000) {
      roads_display <- st_simplify(roads_display, dTolerance = 0.002)
      roads_display <- roads_display[!st_is_empty(roads_display), ]
    }
    if (nrow(roads_display) > 5000) {
      roads_display <- roads_display[sample(1:nrow(roads_display), 5000), ]
    }
  }
  
  wetlands_cropped <- NULL
  use_wetlands <- FALSE
  if (!is.null(wetlands_data) && nrow(wetlands_data) > 0) {
    wetlands_clean <- clean_wetlands(wetlands_data)
    if (!is.null(wetlands_clean) && nrow(wetlands_clean) > 0) {
      tryCatch({
        cropped <- st_crop(wetlands_clean, bbox)
        if (!is.null(cropped) && nrow(cropped) > 0) {
          if (nrow(cropped) > 5000) {
            cropped <- st_simplify(cropped, dTolerance = 0.001)
            cropped <- cropped[!st_is_empty(cropped), ]
          }
          wetlands_cropped <- cropped
          use_wetlands <- TRUE
        }
      }, error = function(e) {})
    }
  }
  
  p <- ggplot() +
    {if (!is.null(iran_data)) geom_sf(data = iran_data, fill = "#F5F5F5", color = "#1a237e", size = 0.8)} +
    {if (!is.null(provinces_data) && nrow(provinces_data) > 0) 
      geom_sf(data = provinces_data, fill = NA, color = "#1565C0", size = 0.5, alpha = 0.5)} +
    {if (use_wetlands && !is.null(wetlands_cropped) && nrow(wetlands_cropped) > 0) 
      geom_sf(data = wetlands_cropped, fill = "#4FC3F7", color = "#0288D1", alpha = 0.3, size = 0.2)} +
    {if (!is.null(roads_display) && nrow(roads_display) > 0) 
      geom_sf(data = roads_display, color = "#8D6E63", size = 0.3, alpha = 0.5)} +
    geom_point(data = spatial_data,
               aes(x = X, y = Y, color = Spatial_MCDA_Score, size = Spatial_MCDA_Score, shape = Chain_Segment),
               alpha = 0.9, stroke = 0.1) +
    scale_color_gradient2(low = "#2E7D32", mid = "#FFD54F", high = "#D32F2F",
                          midpoint = 0.5, name = "Spatial Risk Score", labels = scales::percent) +
    scale_size_continuous(range = c(0.5, 1.5), guide = "none") +
    scale_shape_manual(values = shape_map, name = "Unit Type") +
    geom_text_repel(data = spatial_data,
                    aes(x = X, y = Y, label = Unit_Name),
                    size = 1.2, color = "black", box.padding = 0.08,
                    point.padding = 0.02, max.overlaps = 60,
                    segment.color = NA, segment.size = 0,
                    fontface = "plain", lineheight = 0.4) +
    coord_sf(xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
    theme_minimal() +
    theme(legend.position = "right", legend.title = element_text(size = 8, face = "bold"),
          legend.text = element_text(size = 6.5), legend.key.size = unit(0.3, "cm"),
          plot.title = element_text(size = 14, face = "bold"), 
          plot.subtitle = element_text(size = 10, color = "gray40"),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray90", size = 0.2),
          axis.title = element_text(size = 9), axis.text = element_text(size = 8),
          legend.box = "vertical") +
    labs(title = paste("Spatial Risk Heatmap -", city_name),
         subtitle = paste(nrow(spatial_data), "units | Red = High Risk, Green = Low Risk"),
         x = "Longitude", y = "Latitude")
  
  tryCatch({ p <- p + annotation_scale(location = "br", width_hint = 0.15) }, error = function(e) {})
  
  leaflet_map <- NULL
  if (nrow(spatial_data) > 0) {
    leaflet_map <- create_county_interactive_map(spatial_data, city_name, 
                                                 provinces_data, roads_display, 
                                                 wetlands_cropped)
  }
  
  return(list(Static = p, Leaflet = leaflet_map, Data = spatial_data))
}

create_county_interactive_map <- function(spatial_data, city_name, 
                                          provinces_data, roads_data, wetlands_data) {
  unit_types <- unique(spatial_data$Chain_Segment)
  n_types <- length(unit_types)
  type_colors <- viridis::viridis(n_types)
  names(type_colors) <- unit_types
  shape_symbols <- c("●", "▲", "■", "◆", "★", "✚", "✕", "◉", "◈", "◊", "▣", "▤", "▥")
  
  m <- leaflet(spatial_data) %>%
    setView(lng = mean(spatial_data$X, na.rm = TRUE), lat = mean(spatial_data$Y, na.rm = TRUE), zoom = 12) %>%
    add_leaflet_tiles()
  
  if (!is.null(provinces_data) && nrow(provinces_data) > 0) {
    col_names <- names(provinces_data)
    label_col <- if("name" %in% col_names) "name" else if("NAME_1" %in% col_names) "NAME_1" else col_names[1]
    m <- m %>% addPolygons(data = provinces_data, fillColor = "#E8F4FD", fillOpacity = 0.1, 
                           color = "#1565C0", weight = 2, 
                           label = ~as.character(provinces_data[[label_col]]), group = "Province Borders")
  }
  
  wetlands_clean <- NULL
  if (!is.null(wetlands_data) && nrow(wetlands_data) > 0) {
    wetlands_clean <- clean_wetlands(wetlands_data)
    if (!is.null(wetlands_clean) && nrow(wetlands_clean) > 0 && nrow(wetlands_clean) <= 2000) {
      m <- m %>% addPolygons(data = wetlands_clean, fillColor = "#4FC3F7", fillOpacity = 0.3,
                             color = "#0288D1", weight = 1, label = "Wetland", group = "Wetlands")
    }
  }
  
  if (!is.null(roads_data) && nrow(roads_data) > 0) {
    m <- m %>% addPolylines(data = roads_data, color = "#8D6E63", weight = 1.5, 
                            opacity = 0.5, label = "Road", group = "Roads")
  }
  
  label_opts <- create_leaflet_label_options(font_size = "9px")
  
  for (type in unit_types) {
    type_data <- spatial_data %>% filter(Chain_Segment == type)
    color <- type_colors[type]
    
    m <- m %>% addCircleMarkers(
      data = type_data, lng = ~X, lat = ~Y, radius = 6,
      color = color, fillColor = color, fillOpacity = 0.9, weight = 0.5,
      popup = paste("<b>Unit:</b>", type_data$Unit_Name, "<br>",
                    "<b>Type:</b>", type, "<br>",
                    "<b>Risk Score:</b>", round(type_data$Spatial_MCDA_Score, 3), "<br>",
                    "<b>Risk Level:</b>", type_data$Spatial_Risk_Level, "<br>",
                    "<b>Road Distance:</b>", round(type_data$Dist_Road_m, 0), "m<br>",
                    "<b>Wetland Distance:</b>", round(type_data$Dist_Wetland_m, 0), "m<br>",
                    "<b>Density:</b>", round(type_data$Density, 6)),
      label = ~paste(Unit_Name, "\n", type, "\nRisk:", round(Spatial_MCDA_Score, 3), sep = ""),
      group = type, labelOptions = label_opts)
  }
  
  overlay_groups <- c(unit_types, "Province Borders", "Roads")
  if (!is.null(wetlands_clean) && nrow(wetlands_clean) > 0 && nrow(wetlands_clean) <= 2000) {
    overlay_groups <- c(overlay_groups, "Wetlands")
  }
  
  m <- m %>% addLayersControl(baseGroups = TILE_NAMES, overlayGroups = overlay_groups,
                              options = layersControlOptions(collapsed = FALSE), position = "topleft")
  
  legend_colors <- type_colors
  legend_labels <- paste(shape_symbols[1:n_types], names(type_colors))
  
  if (!is.null(provinces_data) && nrow(provinces_data) > 0) {
    legend_colors <- c(legend_colors, "#1565C0"); legend_labels <- c(legend_labels, "Province Borders")
  }
  if (!is.null(roads_data) && nrow(roads_data) > 0) {
    legend_colors <- c(legend_colors, "#8D6E63"); legend_labels <- c(legend_labels, "Roads")
  }
  if (!is.null(wetlands_clean) && nrow(wetlands_clean) > 0 && nrow(wetlands_clean) <= 2000) {
    legend_colors <- c(legend_colors, "#4FC3F7"); legend_labels <- c(legend_labels, "Wetlands")
  }
  
  m <- m %>% addLegend(position = "bottomright", colors = legend_colors, labels = legend_labels,
                       title = "Legend", opacity = 0.9) %>%
    addLegend(position = "bottomright",
              pal = leaflet::colorNumeric(palette = c("#2E7D32", "#FFD54F", "#D32F2F"),
                                          domain = spatial_data$Spatial_MCDA_Score, na.color = "transparent"),
              values = spatial_data$Spatial_MCDA_Score,
              title = "Spatial Risk<br>(MCDA Score)", opacity = 0.8,
              labFormat = labelFormat(suffix = "", transform = function(x) round(x, 2)))
  
  return(m)
}

# ============================================================
# Part 11: Monte Carlo Uncertainty Analysis
# ============================================================

run_monte_carlo_uncertainty <- function(data, n_simulations = 1000, confidence_level = 0.95,
                                        output_file = NULL, city_name = "Unknown") {
  
  if (!dir.exists("Output")) dir.create("Output")
  
  simulate_parameter <- function(original_value, param_name, n_sims) {
    if (param_name %in% c("H", "E", "C")) {
      if (is.na(original_value) || original_value < 1 || original_value > 5) return(rep(3, n_sims))
      prob_weights <- exp(-((1:5) - original_value)^2 / 2)
      prob_weights <- prob_weights / sum(prob_weights)
      return(sample(1:5, n_sims, replace = TRUE, prob = prob_weights))
    } else if (param_name == "R") {
      if (is.na(original_value) || original_value < 0.5 || original_value > 5) return(rep(2.5, n_sims))
      sd_value <- max(original_value * 0.15, 0.2)
      samples <- rnorm(n_sims, mean = original_value, sd = sd_value)
      return(pmax(0.5, pmin(5, samples)))
    } else if (param_name == "Connections") {
      if (is.na(original_value) || original_value < 1) return(rep(5, n_sims))
      samples <- round(original_value * (1 + rnorm(n_sims, 0, 0.1)))
      return(pmax(1, samples))
    } else {
      return(rep(original_value, n_sims))
    }
  }
  
  n_rows <- nrow(data)
  simulation_results <- vector("list", n_rows)
  pb <- txtProgressBar(min = 0, max = n_rows, style = 3, width = 50)
  
  for (i in 1:n_rows) {
    setTxtProgressBar(pb, i)
    original_row <- data[i, ]
    
    H_sim <- simulate_parameter(original_row$H, "H", n_simulations)
    E_sim <- simulate_parameter(original_row$E, "E", n_simulations)
    C_sim <- simulate_parameter(original_row$C, "C", n_simulations)
    R_sim <- simulate_parameter(original_row$R, "R", n_simulations)
    
    Risk_Scores <- (H_sim * E_sim * C_sim) / R_sim
    Risk_Levels <- case_when(Risk_Scores >= 125 ~ "Critical", Risk_Scores >= 50 ~ "High",
                             Risk_Scores >= 25 ~ "Moderate", TRUE ~ "Low")
    
    simulation_results[[i]] <- data.frame(
      Row = i, Chain_Segment = original_row$Chain_Segment, Hazard = original_row$Hazard,
      Original_H = original_row$H, Original_E = original_row$E, Original_C = original_row$C,
      Original_R = original_row$R, Original_Risk = original_row$Risk_Score,
      Mean_Risk = mean(Risk_Scores, na.rm = TRUE), Median_Risk = median(Risk_Scores, na.rm = TRUE),
      SD_Risk = sd(Risk_Scores, na.rm = TRUE), 
      CV_Risk = sd(Risk_Scores, na.rm = TRUE) / mean(Risk_Scores, na.rm = TRUE) * 100,
      Min_Risk = min(Risk_Scores, na.rm = TRUE), Max_Risk = max(Risk_Scores, na.rm = TRUE),
      P_5 = quantile(Risk_Scores, 0.05, na.rm = TRUE), P_25 = quantile(Risk_Scores, 0.25, na.rm = TRUE),
      P_50 = quantile(Risk_Scores, 0.50, na.rm = TRUE), P_75 = quantile(Risk_Scores, 0.75, na.rm = TRUE),
      P_95 = quantile(Risk_Scores, 0.95, na.rm = TRUE),
      P_Critical = mean(Risk_Levels == "Critical", na.rm = TRUE) * 100,
      P_High = mean(Risk_Levels == "High", na.rm = TRUE) * 100,
      P_Moderate = mean(Risk_Levels == "Moderate", na.rm = TRUE) * 100,
      P_Low = mean(Risk_Levels == "Low", na.rm = TRUE) * 100,
      stringsAsFactors = FALSE
    )
  }
  close(pb)
  
  mc_results <- do.call(rbind, simulation_results)
  mc_results$CI_Lower <- mc_results$P_5
  mc_results$CI_Upper <- mc_results$P_95
  
  chain_mc_summary <- mc_results %>%
    group_by(Chain_Segment) %>%
    summarise(
      n_Hazards = n(), Mean_Mean_Risk = round(mean(Mean_Risk, na.rm = TRUE), 2),
      Mean_Median_Risk = round(mean(Median_Risk, na.rm = TRUE), 2),
      Mean_SD_Risk = round(mean(SD_Risk, na.rm = TRUE), 2),
      Mean_CV_Risk = round(mean(CV_Risk, na.rm = TRUE), 2),
      Mean_P_Critical = round(mean(P_Critical, na.rm = TRUE), 2),
      Mean_P_High = round(mean(P_High, na.rm = TRUE), 2),
      Mean_P_Moderate = round(mean(P_Moderate, na.rm = TRUE), 2),
      Mean_P_Low = round(mean(P_Low, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Mean_Mean_Risk)) %>%
    mutate(Chain_Rank = row_number(),
           Risk_Stability = case_when(Mean_CV_Risk < 15 ~ "Very Stable",
                                      Mean_CV_Risk < 30 ~ "Stable",
                                      Mean_CV_Risk < 50 ~ "Moderate",
                                      TRUE ~ "Uncertain"))
  
  risk_percentiles <- mc_results %>%
    group_by(Row) %>%
    summarise(Chain_Segment = first(Chain_Segment), Hazard = first(Hazard),
              Mean_Risk = first(Mean_Risk), Median_Risk = first(Median_Risk),
              SD_Risk = first(SD_Risk), P_Critical = first(P_Critical),
              P_High = first(P_High), .groups = "drop") %>%
    arrange(desc(Mean_Risk)) %>%
    mutate(Risk_Percentile = (row_number() - 0.5) / n() * 100,
           Stability_Score = round((100 - SD_Risk / Mean_Risk * 100), 2),
           Stability_Level = case_when(Stability_Score >= 85 ~ "Very Stable",
                                       Stability_Score >= 70 ~ "Stable",
                                       Stability_Score >= 50 ~ "Moderate",
                                       TRUE ~ "Uncertain"))
  
  sample_rows <- min(50, n_rows)
  sample_indices <- sample(1:n_rows, sample_rows)
  params <- c("H", "E", "C", "R")
  
  sensitivity_results <- bind_rows(lapply(sample_indices, function(i) {
    original_row <- data[i, ]
    base_risk <- (original_row$H * original_row$E * original_row$C) / original_row$R
    param_changes <- list(
      H = seq(max(1, original_row$H - 1), min(5, original_row$H + 1), by = 0.5),
      E = seq(max(1, original_row$E - 1), min(5, original_row$E + 1), by = 0.5),
      C = seq(max(1, original_row$C - 1), min(5, original_row$C + 1), by = 0.5),
      R = seq(max(0.5, original_row$R - 1), min(5, original_row$R + 1), by = 0.25)
    )
    bind_rows(lapply(params, function(param) {
      values <- param_changes[[param]]
      new_risk <- switch(param,
                         H = (values * original_row$E * original_row$C) / original_row$R,
                         E = (original_row$H * values * original_row$C) / original_row$R,
                         C = (original_row$H * original_row$E * values) / original_row$R,
                         R = (original_row$H * original_row$E * original_row$C) / values)
      data.frame(Row = i, Chain_Segment = original_row$Chain_Segment, Hazard = original_row$Hazard,
                 Parameter = param, Original_Value = original_row[[param]], New_Value = values,
                 Original_Risk = base_risk, New_Risk = new_risk,
                 Change_Percent = (new_risk - base_risk) / base_risk * 100,
                 stringsAsFactors = FALSE)
    }))
  }))
  
  sensitivity_summary <- sensitivity_results %>%
    group_by(Parameter) %>%
    summarise(Mean_Change = round(mean(abs(Change_Percent), na.rm = TRUE), 2),
              Max_Change = round(max(abs(Change_Percent), na.rm = TRUE), 2),
              Median_Change = round(median(abs(Change_Percent), na.rm = TRUE), 2),
              SD_Change = round(sd(abs(Change_Percent), na.rm = TRUE), 2), .groups = "drop") %>%
    arrange(desc(Mean_Change))
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  top_risks <- mc_results %>%
    group_by(Hazard) %>%
    summarise(Mean_Risk = mean(Mean_Risk, na.rm = TRUE)) %>%
    arrange(desc(Mean_Risk)) %>%
    head(10) %>% pull(Hazard)
  
  plot_data <- mc_results %>% filter(Hazard %in% top_risks)
  
  p1 <- ggplot(plot_data, aes(x = reorder(Hazard, Mean_Risk), y = Mean_Risk, fill = Hazard)) +
    geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) + coord_flip() +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"), axis.title = element_text(size = 10)) +
    labs(title = paste("Monte Carlo Uncertainty Analysis - Top 10 Hazards -", city_name),
         subtitle = paste(n_simulations, "simulations per hazard, 95% confidence interval shown"),
         x = "Hazard Type", y = "Mean Risk Score")
  
  chain_plot_data <- chain_mc_summary %>%
    mutate(Risk_Category = case_when(Mean_Mean_Risk >= 125 ~ "Critical",
                                     Mean_Mean_Risk >= 50 ~ "High",
                                     Mean_Mean_Risk >= 25 ~ "Moderate",
                                     TRUE ~ "Low"))
  
  p2 <- ggplot(chain_plot_data, aes(x = reorder(Chain_Segment, Mean_Mean_Risk), 
                                    y = Mean_Mean_Risk, fill = Risk_Category)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = Mean_Mean_Risk - Mean_SD_Risk, 
                      ymax = Mean_Mean_Risk + Mean_SD_Risk), width = 0.2, alpha = 0.6) +
    geom_text(aes(label = paste0(round(Mean_P_Critical, 1), "% Critical")), hjust = -0.1, size = 3) +
    coord_flip() +
    scale_fill_manual(values = c("Critical" = "#8B0000", "High" = "#F44336", 
                                 "Moderate" = "#FF9800", "Low" = "#4CAF50"), name = "Risk Category") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"),
          axis.title = element_text(size = 10), legend.position = "bottom") +
    labs(title = paste("Chain Risk with Uncertainty -", city_name),
         subtitle = "Bars show mean risk, error bars show +/- 1 SD, labels show % critical",
         x = "Chain Segment", y = "Mean Risk Score")
  
  p3 <- ggplot(risk_percentiles, aes(x = Mean_Risk, y = Stability_Score, 
                                     color = Stability_Level, size = P_Critical / 100)) +
    geom_point(alpha = 0.7) +
    geom_text_repel(aes(label = ifelse(Stability_Score < 50 | Mean_Risk > 150,
                                       paste0(Chain_Segment, "-", Hazard), "")),
                    size = 2.5, max.overlaps = 20) +
    scale_color_manual(values = c("Very Stable" = "#2E7D32", "Stable" = "#4CAF50",
                                  "Moderate" = "#FF9800", "Uncertain" = "#D32F2F"),
                       name = "Stability Level") +
    scale_size_continuous(range = c(1, 6), name = "P(Critical)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"),
          legend.position = "right", legend.key.size = unit(0.4, "cm")) +
    labs(title = paste("Risk-Stability Trade-off -", city_name),
         subtitle = "High risk + Low stability = Highest priority for intervention",
         x = "Mean Risk Score", y = "Stability Score (higher = more certain)")
  
  p4 <- ggplot(sensitivity_summary, aes(x = reorder(Parameter, Mean_Change), 
                                        y = Mean_Change, fill = Parameter)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = Mean_Change - SD_Change, ymax = Mean_Change + SD_Change),
                  width = 0.2, alpha = 0.6) +
    geom_text(aes(label = paste0(round(Mean_Change, 1), "%")), vjust = -0.5, size = 4) +
    scale_fill_viridis_d() +
    theme_minimal() +
    theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, color = "gray40"), axis.title = element_text(size = 10)) +
    labs(title = paste("Parameter Sensitivity Analysis -", city_name),
         subtitle = "Mean absolute change in risk score per unit change in parameter",
         x = "Parameter", y = "Mean % Change in Risk Score")
  
  safe_ggsave(p1, paste0("Output/MC01_Risk_Distribution_", timestamp, ".png"), width = 12, height = 8)
  safe_ggsave(p2, paste0("Output/MC02_Chain_Uncertainty_", timestamp, ".png"), width = 12, height = 8)
  safe_ggsave(p3, paste0("Output/MC03_Risk_Stability_", timestamp, ".png"), width = 12, height = 8)
  safe_ggsave(p4, paste0("Output/MC04_Sensitivity_", timestamp, ".png"), width = 10, height = 6)
  
  if (is.null(output_file)) {
    output_file <- paste0("Output/Monte_Carlo_Results_", city_name, "_", timestamp, ".xlsx")
  }
  
  sheets_list <- list("MC_Full_Results" = mc_results, "MC_Chain_Summary" = chain_mc_summary,
                      "MC_Risk_Stability" = risk_percentiles, "MC_Sensitivity" = sensitivity_summary)
  
  tryCatch({ write_xlsx(sheets_list, output_file) }, error = function(e) {})
  
  return(list(Monte_Carlo_Results = mc_results, Chain_Summary = chain_mc_summary,
              Risk_Stability = risk_percentiles, Sensitivity_Summary = sensitivity_summary,
              Plots = list(Risk_Distribution = p1, Chain_Uncertainty = p2, 
                           Risk_Stability = p3, Sensitivity = p4),
              Parameters = list(n_simulations = n_simulations, confidence_level = confidence_level,
                                timestamp = timestamp, city_name = city_name)))
}

# ============================================================
# Part 12: Main Execution
# ============================================================

run_analysis <- function(file_path = "HPAI_Risk_Matrix_Data.xlsx", units_file_path = "Units.xlsx") {
  if (!dir.exists("Output")) dir.create("Output")
  
  weights_chain <- load_weights_from_excel(file_path, "Sheet3")
  weights_unit <- load_weights_from_excel(file_path, "Sheet5")
  weights_spatial <- load_weights_from_excel(file_path, "Sheet6")
  
  city_name <- read_city_name(file_path)
  load_result <- load_and_validate_data(file_path)
  data <- load_result$Data
  unique_values <- extract_unique_values(file_path)
  
  iran_data <- load_high_res_iran()
  provinces_data <- load_iran_provinces()
  
  filtered_units_data <- NULL
  if (!is.null(city_name)) filtered_units_data <- filter_units_by_city(units_file_path, city_name)
  if (is.null(filtered_units_data) || nrow(filtered_units_data) == 0) return(NULL)
  
  xy_idx <- get_xy_indices(colnames(filtered_units_data))
  if (is.null(xy_idx)) return(NULL)
  
  x_coords <- convert_coord(filtered_units_data[[xy_idx["x"]]])
  y_coords <- convert_coord(filtered_units_data[[xy_idx["y"]]])
  valid_idx <- !is.na(x_coords) & !is.na(y_coords)
  x_coords <- x_coords[valid_idx]; y_coords <- y_coords[valid_idx]
  if (length(x_coords) < 3) return(NULL)
  
  x_range <- max(x_coords) - min(x_coords); y_range <- max(y_coords) - min(y_coords)
  buffer_x <- x_range * 0.5; buffer_y <- y_range * 0.5
  bbox <- st_bbox(c(xmin = min(x_coords) - buffer_x, xmax = max(x_coords) + buffer_x,
                    ymin = min(y_coords) - buffer_y, ymax = max(y_coords) + buffer_y), crs = st_crs(4326))
  city_boundary <- st_sf(geometry = st_as_sfc(bbox))
  
  roads_data <- load_real_iran_roads(city_boundary, city_name)
  wetlands_data <- load_real_iran_wetlands()
  
  chain_summary <- calculate_chain_summary(data, filtered_units_data, weights_chain)
  mean_risk_ranking <- calculate_mean_risk_ranking(data)
  
  map_result <- create_geographic_map(filtered_units_data, chain_summary, city_name, 
                                      iran_data, provinces_data, roads_data, wetlands_data)
  
  unit_mcda_results <- NULL
  if (!is.null(map_result)) unit_mcda_results <- analyze_unit_mcda(file_path, weights_unit, 
                                                                   map_result$Map_Data, chain_summary)
  
  spatial_unit_results <- NULL
  if (!is.null(map_result)) {
    if (is.null(weights_spatial)) {
      weights_spatial <- list(Weights = c(Road_Norm = 0.40, Wetland_Norm = 0.30, Density_Norm = 0.30),
                              Criteria_Types = c(Road_Norm = "Beneficial", Wetland_Norm = "Beneficial", 
                                                 Density_Norm = "Beneficial"))
    }
    spatial_unit_results <- analyze_spatial_unit_mcda(file_path, weights_spatial, map_result$Map_Data,
                                                      roads_data, wetlands_data, chain_summary, city_boundary)
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  chain_plot <- create_mcda_ranking_plot(chain_summary, 
                                         title = paste("Chain MCDA Ranking -", city_name),
                                         subtitle = "6 criteria: Critical_Percent, Mean_Resilience, Risk_Density, Connections, Unit_Count, Total_Capacity",
                                         x_label = "Chain Stage")
  safe_ggsave(chain_plot, paste0("Output/01_Chain_MCDA_Ranking_", timestamp, ".png"))
  
  if (!is.null(unit_mcda_results) && nrow(unit_mcda_results) > 0) {
    unit_plot <- create_mcda_ranking_plot(unit_mcda_results,
                                          title = paste("Top High-Risk Units (MCDA) -", city_name),
                                          subtitle = "4 criteria: Risk_Score, Connections, Total_Capacity, Chain_MCDA_Score",
                                          x_label = "Unit Name", name_col = "Unit_Name", top_n = 20)
    safe_ggsave(unit_plot, paste0("Output/02_Unit_MCDA_Ranking_", timestamp, ".png"))
  }
  
  if (!is.null(spatial_unit_results) && nrow(spatial_unit_results) > 0) {
    spatial_plot <- create_mcda_ranking_plot(spatial_unit_results,
                                             title = paste("Top High-Risk Units (Spatial MCDA) -", city_name),
                                             subtitle = "3 criteria: Road_Norm, Wetland_Norm, Density_Norm",
                                             x_label = "Unit Name", name_col = "Unit_Name", score_col = "Spatial_MCDA_Score",
                                             rank_col = "Spatial_MCDA_Rank", level_col = "Spatial_Risk_Level", top_n = 20)
    safe_ggsave(spatial_plot, paste0("Output/03_Spatial_MCDA_Ranking_", timestamp, ".png"))
  }
  
  if (!is.null(spatial_unit_results) && nrow(spatial_unit_results) > 0) {
    heatmap_plot <- create_risk_heatmap(spatial_unit_results, city_name, iran_data, provinces_data)
    safe_ggsave(heatmap_plot, paste0("Output/04_Risk_Heatmap_", timestamp, ".png"))
    
    density_plot <- create_kernel_density_map(spatial_unit_results, city_name, iran_data, provinces_data)
    safe_ggsave(density_plot, paste0("Output/05_Kernel_Density_", timestamp, ".png"))
  }
  
  if (!is.null(map_result)) {
    safe_ggsave(map_result$Map, paste0("Output/06_Geographic_Map_", timestamp, ".png"), limitsize = FALSE)
  }
  
  if (!is.null(map_result) && !is.null(map_result$Leaflet_Map)) {
    html_file <- paste0("Output/07_Interactive_Map_", timestamp, ".html")
    tryCatch({ saveWidget(map_result$Leaflet_Map, html_file) }, error = function(e) {})
  }
  
  output_file <- export_results(data, mean_risk_ranking, chain_summary, unit_mcda_results,
                                spatial_unit_results, filtered_units_data, unique_values,
                                city_name, weights_chain, weights_unit, weights_spatial)
  
  if (!is.null(spatial_unit_results) && nrow(spatial_unit_results) > 0) {
    county_heatmap <- create_county_heatmap(spatial_unit_results, city_name, iran_data,
                                            provinces_data, roads_data, wetlands_data,
                                            city_boundary)
    if (!is.null(county_heatmap)) {
      safe_ggsave(county_heatmap$Static, paste0("Output/08_County_Heatmap_", timestamp, ".png"), width = 12)
      if (!is.null(county_heatmap$Leaflet)) {
        html_file <- paste0("Output/09_County_Heatmap_Interactive_", timestamp, ".html")
        tryCatch({ saveWidget(county_heatmap$Leaflet, html_file) }, error = function(e) {})
      }
    }
  }
  
  mc_results <- run_monte_carlo_uncertainty(data = data, n_simulations = 1000, 
                                            confidence_level = 0.95, city_name = city_name)
  
  return(list(Data = data, Mean_Risk_Ranking = mean_risk_ranking, Chain_Summary = chain_summary,
              Unique_Values = unique_values, Unit_MCDA_Results = unit_mcda_results,
              Spatial_Unit_Results = spatial_unit_results, Filtered_Units = filtered_units_data,
              City_Name = city_name, Map_Result = map_result, Weights_Chain = weights_chain,
              Weights_Unit = weights_unit, Weights_Spatial = weights_spatial, Monte_Carlo = mc_results))
}

# ============================================================
# Part 13: Run Section
# ============================================================

if (!file.exists("HPAI_Risk_Matrix_Data.xlsx")) {
  create_input_template()
} else {
  suppressWarnings({ results <- run_analysis("HPAI_Risk_Matrix_Data.xlsx", "Units.xlsx") })
  if (!is.null(results)) {
    if (!is.null(results$Map_Result) && !is.null(results$Map_Result$Map)) 
      print(results$Map_Result$Map)
    
    message("\n========================================")
    message("SUMMARY")
    message("========================================")
    message("City: ", results$City_Name)
    message("Total chains: ", nrow(results$Chain_Summary))
    
    if (!is.null(results$Chain_Summary) && "MCDA_Rank" %in% colnames(results$Chain_Summary)) {
      top_3_chains <- results$Chain_Summary %>% filter(MCDA_Rank <= 3) %>% pull(Chain_Segment)
      message("Top 3 highest risk chains: ", paste(top_3_chains, collapse = ", "))
    }
    
    message("\nTOP 5 CHAINS (MCDA):")
    print(head(results$Chain_Summary[, c("Chain_Segment", "MCDA_Rank", "MCDA_Score", "Risk_Level_MCDA")], 5))
    
    if (!is.null(results$Unit_MCDA_Results)) {
      message("\nTOP 5 UNITS (MCDA - 4 criteria including Chain_MCDA_Score):")
      print(head(results$Unit_MCDA_Results[, c("Unit_Name", "Chain_Segment", "Chain_MCDA_Score", 
                                               "Risk_Score", "Connections", "Total_Capacity", 
                                               "MCDA_Score", "MCDA_Rank")], 5))
    }
    
    if (!is.null(results$Spatial_Unit_Results) && nrow(results$Spatial_Unit_Results) > 0) {
      message("\nTOP 5 UNITS (SPATIAL MCDA - 3 criteria: Road, Wetland, Density):")
      print(head(results$Spatial_Unit_Results[, c("Unit_Name", "Chain_Segment", "Road_Norm", 
                                                  "Wetland_Norm", "Density_Norm", 
                                                  "Spatial_MCDA_Score", "Spatial_MCDA_Rank")], 5))
    }
    message("========================================")
  }
}

# ============================================================
# End of Code
# ============================================================