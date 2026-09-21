# ============================================================
# 1. PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================
# 2. LOAD DATA
# ============================================================

setwd("C:/Users/apchr/OneDrive/Desktop/USB for Thesis/Graph")

datei <- "vent species.xlsx"

old <- read_excel(datei, sheet = "old")
new <- read_excel(datei, sheet = "new ")


# ============================================================
# 3. DEFINE IUCN CATEGORIES
# ============================================================

kategorien <- c("LC", "NT", "VU", "EN", "CR")


# ============================================================
# 4. COUNT SPECIES BY IUCN CATEGORY
# ============================================================

old_count <- old %>%
  filter(category %in% kategorien) %>%
  count(category) %>%
  rename(Old = n)

new_count <- new %>%
  filter(category %in% kategorien) %>%
  count(category) %>%
  rename(New = n)


# ============================================================
# 5. COMBINE DATASETS
# ============================================================

daten <- full_join(old_count, new_count, by = "category") %>%
  mutate(
    Old = replace_na(Old, 0),
    New = replace_na(New, 0),
    Both = Old + New
  )

daten$category <- factor(
  daten$category,
  levels = kategorien
)


# ============================================================
# 6. ABSOLUTE NUMBER OF SPECIES
# ============================================================

daten_plot <- daten %>%
  pivot_longer(
    cols = c(Old, New, Both),
    names_to = "Gruppe",
    values_to = "Anzahl"
  )

daten_plot$Gruppe <- factor(
  daten_plot$Gruppe,
  levels = c("Old", "New", "Both")
)


ggplot(
  daten_plot,
  aes(x = category, y = Anzahl, fill = Gruppe)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 10),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual(
    values = c(
      "Old" = "#0072B2",
      "New" = "#D55E00",
      "Both" = "grey50"
    ),
    labels = c(
      "Old" = "Vent molluscs",
      "New" = "This study",
      "Both" = "Combined total"
    )
  ) +
  labs(
    x = "IUCN category",
    y = "Number of species",
    fill = ""
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )


# ============================================================
# 7. CALCULATE PERCENTAGES
# ============================================================

daten_prozent <- daten %>%
  mutate(
    Old = Old / sum(Old) * 100,
    New = New / sum(New) * 100,
    Both = Both / sum(Both) * 100
  )


# ============================================================
# 8. PERCENTAGE PLOT
#    Blue / orange / grey
# ============================================================

daten_prozent_plot <- daten_prozent %>%
  pivot_longer(
    cols = c(Old, New, Both),
    names_to = "Gruppe",
    values_to = "Prozent"
  )

daten_prozent_plot$Gruppe <- factor(
  daten_prozent_plot$Gruppe,
  levels = c("Old", "New", "Both")
)


ggplot(
  daten_prozent_plot,
  aes(x = category, y = Prozent, fill = Gruppe)
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual(
    values = c(
      "Old" = "#0072B2",
      "New" = "#D55E00",
      "Both" = "grey50"
    ),
    labels = c(
      "Old" = "Vent molluscs",
      "New" = "This study",
      "Both" = "Combined total"
    )
  ) +
  labs(
    x = "IUCN category",
    y = "Percentage of species",
    fill = ""
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )


# ============================================================
# 9. 100% STACKED BAR PLOT
#    IUCN categories
# ============================================================

daten_stacked <- daten_prozent %>%
  pivot_longer(
    cols = c(Old, New, Both),
    names_to = "Gruppe",
    values_to = "Prozent"
  )

daten_stacked$Gruppe <- factor(
  daten_stacked$Gruppe,
  levels = c("Old", "New", "Both")
)


ggplot(
  daten_stacked,
  aes(x = Gruppe, y = Prozent, fill = category)
) +
  geom_col(width = 0.7) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual(
    values = c(
      "LC" = "#009E73",
      "NT" = "#8BC34A",
      "VU" = "#F0E442",
      "EN" = "#E69F00",
      "CR" = "#CC0000"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Old" = "Vent molluscs",
      "New" = "This study",
      "Both" = "Combined total"
    )
  ) +
  labs(
    x = "",
    y = "Percentage of species",
    fill = "IUCN category"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )

# ============================================================
# SPECIES RICHNESS ACROSS GLOBAL 5° HEXAGONAL GRID
# ============================================================

library(icosa)
library(sf)

# ============================================================
# 1. LOAD AND COMBINE OCCURRENCE DATA
# ============================================================

folder <- "C:/Users/apchr/OneDrive/Desktop/USB for Thesis/Graph/Heat maps"

files <- list.files(
  folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

all_data_list <- lapply(files, function(file) {
  
  lines <- readLines(file, warn = FALSE)
  
  if (length(lines) < 2) {
    return(NULL)
  }
  
  header <- strsplit(lines[1], ",", fixed = TRUE)[[1]]
  
  lat_pos <- match("Latitude", header)
  lon_pos <- match("Longitude", header)
  dec_lat_pos <- match("dec_lat", header)
  dec_lon_pos <- match("dec_long", header)
  sci_pos <- match("sci_name", header)
  
  data_lines <- lines[-1]
  
  result <- lapply(data_lines, function(line) {
    
    x <- strsplit(line, ",", fixed = TRUE)[[1]]
    
    if (!is.na(lat_pos) && !is.na(lon_pos) &&
        length(x) >= lon_pos) {
      
      lat <- x[lat_pos]
      lon <- x[lon_pos]
      
    } else if (!is.na(dec_lat_pos) && !is.na(dec_lon_pos) &&
               length(x) >= dec_lon_pos) {
      
      lat <- x[dec_lat_pos]
      lon <- x[dec_lon_pos]
      
    } else {
      return(NULL)
    }
    
    if (!is.na(sci_pos) && length(x) >= sci_pos) {
      species <- x[sci_pos]
    } else {
      species <- tools::file_path_sans_ext(basename(file))
    }
    
    data.frame(
      Latitude = suppressWarnings(as.numeric(lat)),
      Longitude = suppressWarnings(as.numeric(lon)),
      species = species,
      stringsAsFactors = FALSE
    )
  })
  
  result <- result[!sapply(result, is.null)]
  
  if (length(result) == 0) {
    return(NULL)
  }
  
  do.call(rbind, result)
})

all_data <- do.call(rbind, all_data_list)

all_data <- all_data[
  !is.na(all_data$Latitude) &
    !is.na(all_data$Longitude),
]


# ============================================================
# 2. PREPARE COORDINATES
# ============================================================

coords <- all_data[, c(
  "Longitude",
  "Latitude",
  "species"
)]


# ============================================================
# 3. CREATE GLOBAL HEXAGONAL GRID
# ============================================================

hex <- hexagrid(
  deg = 5,
  sf = TRUE
)


# ============================================================
# 4. ASSIGN RECORDS TO GRID CELLS
# ============================================================

coords$cell <- locate(
  hex,
  coords[, c("Longitude", "Latitude")]
)


# ============================================================
# 5. CALCULATE SPECIES RICHNESS
# ============================================================

species_richness <- tapply(
  coords$species,
  coords$cell,
  function(x) length(unique(x))
)


# ============================================================
# 6. LOAD AND TRANSFORM WORLD MAP
# ============================================================

ne <- st_read(
  file.path(
    system.file(package = "icosa"),
    "extdata/ne_110m_land.shx"
  ),
  quiet = TRUE
)

ne <- st_transform(
  ne,
  st_crs(hex@sf)
)


# ============================================================
# 7. PLOT SPECIES RICHNESS
# ============================================================

plot(
  hex,
  species_richness,
  border = "white",
  reset = FALSE,
  main = "Species richness of hydrothermal-vent endemics across 5° grid cells",
  cex.main = 0.8,
  legend.pos = "bottom"
)

plot(
  ne$geometry,
  add = TRUE,
  col = "grey",
  border = NA
)

points(
  coords$Longitude,
  coords$Latitude,
  pch = 20,
  cex = 0.3
)




