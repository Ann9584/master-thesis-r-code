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

setwd("C:/Users/apchr/OneDrive/Desktop/Supplementary Material (S3)/master_thesis_R")

datei <- "vent species.xlsx"
# The list includes the vent mollusc species, the species
# examined in this study, and the decapod species,
# together with their respective IUCN categories.

# Vent molluscs
old <- read_excel(
  datei,
  sheet = "Vent molluscs"
)

# This study
new <- read_excel(
  datei,
  sheet = "This study"
)

# Decapoda
decapoda <- read_excel(
  datei,
  sheet = "Decapoda"
)


# ============================================================
# 3. DEFINE IUCN CATEGORIES
# ============================================================

kategorien <- c("LC", "NT", "VU", "EN", "CR")


# ============================================================
# 4. COUNT SPECIES BY IUCN CATEGORY
# ============================================================
##This section counts the number of species assigned to each IUCN category for each of the three datasets.
#First, only the predefined IUCN categories (LC, NT, VU, EN, and CR) are retained. The count(category) function then counts how many entries fall into each category. The resulting counts are renamed according to the respective dataset: Vent molluscs, This study, and Decapoda.
#Thus, the code determines how many species in each dataset belong to each IUCN category.

# Vent molluscs
old_count <- old %>%
  filter(category %in% kategorien) %>%
  count(category) %>%
  rename(`Vent molluscs` = n)


# This study
new_count <- new %>%
  filter(category %in% kategorien) %>%
  count(category) %>%
  rename(`This study` = n)


# Decapoda
decapoda_count <- decapoda %>%
  filter(category %in% kategorien) %>%
  count(category) %>%
  rename(Decapoda = n)


# ============================================================
# 5. COMBINE DATASETS
# ============================================================
# This section combines the three datasets by their IUCN category.
# The full_join() function ensures that all IUCN categories present
# in at least one of the datasets are retained. Missing values are
# replaced with zero using replace_na(), indicating that no species
# were recorded in that category in the respective dataset.
# Finally, category is converted into a factor to ensure that the
# IUCN categories appear in the predefined order: LC, NT, VU, EN, and CR.

daten <- old_count %>%
  full_join(
    new_count,
    by = "category"
  ) %>%
  full_join(
    decapoda_count,
    by = "category"
  ) %>%
  mutate(
    `Vent molluscs` = replace_na(`Vent molluscs`, 0),
    `This study` = replace_na(`This study`, 0),
    Decapoda = replace_na(Decapoda, 0)
  )


# Set order of IUCN categories
daten$category <- factor(
  daten$category,
  levels = kategorien
)


# ============================================================
# 6. CALCULATE COMBINED TOTAL
# ============================================================
# This section calculates the combined total number of species
# for each IUCN category.
# The combined total is calculated by summing the number of species
# from the three datasets: Vent molluscs, This study, and Decapoda.
# The resulting values are stored in a new column called
# "Combined total".

# Combined total includes:
# Vent molluscs + This study + Decapoda


daten <- daten %>%
  mutate(
    `Combined total` =
      `Vent molluscs` +
      `This study` +
      Decapoda
  )


# ============================================================
# 7. RESHAPE DATA FOR PLOTTING
# ============================================================
# This section reshapes the dataset from wide format into long format
# to prepare the data for plotting.
# pivot_longer() combines the columns containing the number of species
# for the four groups — Vent molluscs, This study, Decapoda, and
# Combined total — into two new columns.
# The Gruppe column identifies the respective group, while Anzahl
# contains the corresponding number of species.
#
# The factor() function defines the order in which the groups will
# appear in the plot: Vent molluscs, This study, Decapoda, and
# Combined total.

daten_plot <- daten %>%
  pivot_longer(
    cols = c(
      `Vent molluscs`,
      `This study`,
      Decapoda,
      `Combined total`
    ),
    names_to = "Gruppe",
    values_to = "Anzahl"
  )


# Define order of groups in the plot
daten_plot$Gruppe <- factor(
  daten_plot$Gruppe,
  levels = c(
    "Vent molluscs",
    "This study",
    "Decapoda",
    "Combined total"
  )
)


# ============================================================
# 8. PLOT - FIGURE 6A
# ============================================================
# This section creates a grouped bar plot showing the absolute
# number of species in each IUCN category.
# The IUCN category is displayed on the x-axis, while the number
# of species is shown on the y-axis. The fill aesthetic separates
# the four groups: Vent molluscs, This study, Decapoda, and
# Combined total.
#
# geom_col() creates the bars, while position_dodge() places the
# groups side by side within each IUCN category.
# scale_y_continuous() sets the y-axis from 0 to 100, with
# intervals of 10.
# scale_fill_manual() assigns specific colours and display labels
# to the four groups.
# Finally, labs() defines the axis and legend labels, while
# theme_classic() and theme() control the overall appearance
# of the figure and place the legend at the top.

ggplot(
  daten_plot,
  aes(
    x = category,
    y = Anzahl,
    fill = Gruppe
  )
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
      "Vent molluscs" = "#0072B2",
      "This study" = "#D55E00",
      "Decapoda" = "#009E73",
      "Combined total" = "grey50"
    ),
    labels = c(
      "Vent molluscs" = "Vent mollusks",
      "This study" = "This study",
      "Decapoda" = "Decapoda",
      "Combined total" = "Combined total"
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
# 9. PLOT - FIGURE 6B
# ============================================================
# This section creates a grouped bar plot showing the percentage
# of species in each IUCN category for the four groups.
# The IUCN category is displayed on the x-axis, while the percentage
# of species is shown on the y-axis. The fill aesthetic separates
# the four groups: Vent molluscs, This study, Decapoda, and
# Combined total.
#
# geom_col() creates the bars, while position_dodge() places the
# four groups side by side within each IUCN category.
# scale_y_continuous() sets the y-axis from 0 to 100%, with
# intervals of 10 percentage points.
# scale_fill_manual() assigns specific colours and display labels
# to the four groups.
# Finally, labs() defines the axis and legend labels, while
# theme_classic() and theme() control the overall appearance
# of the figure and place the legend at the top.

ggplot(
  daten_prozent_plot,
  aes(
    x = category,
    y = Prozent,
    fill = Gruppe
  )
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
      "Vent molluscs" = "#0072B2",
      "This study" = "#D55E00",
      "Decapoda" = "#009E73",
      "Combined total" = "grey50"
    ),
    labels = c(
      "Vent molluscs" = "Vent mollusks",
      "This study" = "This study",
      "Decapoda" = "Decapoda",
      "Combined total" = "Combined total"
    )
  ) +
  labs(
    x = "IUCN category",
    y = "Percentage of species (%)",
    fill = ""
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    legend.position = "top"
  )
# ============================================================
# 10. CALCULATE PERCENTAGES
# ============================================================
# This section converts the absolute species counts into percentages
# for each dataset.
# For each group, the number of species in each IUCN category is
# divided by the total number of species in that group and multiplied
# by 100.
# Thus, the values represent the percentage of species within each
# dataset that belong to each IUCN category. The percentages for
# each dataset add up to 100%.

daten_prozent <- daten %>%
  mutate(
    `Vent molluscs` =
      `Vent molluscs` / sum(`Vent molluscs`) * 100,
    
    `This study` =
      `This study` / sum(`This study`) * 100,
    
    Decapoda =
      Decapoda / sum(Decapoda) * 100,
    
    `Combined total` =
      `Combined total` / sum(`Combined total`) * 100
  )
# ============================================================
# 11. RESHAPE DATA FOR PERCENTAGE PLOT
# ============================================================
# This section reshapes the percentage data from wide format into
# long format for plotting.
# pivot_longer() combines the four group columns into two new
# columns: Gruppe, which identifies the dataset, and Prozent,
# which contains the corresponding percentage of species.
# The factor() function defines the order in which the four groups
# appear in the plot.

daten_prozent_plot <- daten_prozent %>%
  pivot_longer(
    cols = c(
      `Vent molluscs`,
      `This study`,
      Decapoda,
      `Combined total`
    ),
    names_to = "Gruppe",
    values_to = "Prozent"
  )


# Define order of groups
daten_prozent_plot$Gruppe <- factor(
  daten_prozent_plot$Gruppe,
  levels = c(
    "Vent molluscs",
    "This study",
    "Decapoda",
    "Combined total"
  )
)


# ============================================================
# 12. PERCENTAGE PLOT
# ============================================================
# This section creates a grouped bar plot showing the percentage
# of species in each IUCN category for the four groups.
# The IUCN category is displayed on the x-axis, while the percentage
# of species is shown on the y-axis.
# geom_col() creates the bars, while position_dodge() places the
# four groups side by side within each IUCN category.
# The y-axis ranges from 0 to 100%, with intervals of 10 percentage
# points. scale_fill_manual() assigns the same colours used in the
# previous plot: blue for Vent molluscs, orange for This study,
# green for Decapoda, and grey for Combined total.
# Finally, labs() defines the axis and legend labels, while
# theme_classic() and theme() control the overall appearance of
# the plot and place the legend at the top.

ggplot(
  daten_prozent_plot,
  aes(
    x = category,
    y = Prozent,
    fill = Gruppe
  )
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
      "Vent molluscs" = "#0072B2",
      "This study" = "#D55E00",
      "Decapoda" = "#009E73",
      "Combined total" = "grey50"
    ),
    labels = c(
      "Vent molluscs" = "Vent mollusks",
      "This study" = "This study",
      "Decapoda" = "Decapoda",
      "Combined total" = "Combined total"
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
# 13. 100% STACKED BAR PLOT
#    IUCN categories
# ============================================================
# This section prepares the percentage data for a 100% stacked
# bar plot showing the relative distribution of IUCN categories
# within each group.
#
# pivot_longer() reshapes the percentage data from wide format
# into long format. The Gruppe column identifies the four groups:
# Vent molluscs, This study, Decapoda, and Combined total, while
# Prozent contains the percentage of species in each IUCN category.
#
# The factor() function defines the order in which the four groups
# appear on the x-axis.

daten_stacked <- daten_prozent %>%
  pivot_longer(
    cols = c(
      `Vent molluscs`,
      `This study`,
      Decapoda,
      `Combined total`
    ),
    names_to = "Gruppe",
    values_to = "Prozent"
  )


# Define order of groups
daten_stacked$Gruppe <- factor(
  daten_stacked$Gruppe,
  levels = c(
    "Vent molluscs",
    "This study",
    "Decapoda",
    "Combined total"
  )
)


# ============================================================
# 14. 100% STACKED BAR PLOT
# ============================================================
# This section creates a 100% stacked bar plot showing the relative
# distribution of IUCN categories within each group.
#
# In the ggplot() function, the four groups are displayed on the
# x-axis and the percentage of species is shown on the y-axis.
# The bars are stacked according to IUCN category using
# fill = category. Each bar therefore represents one group and
# sums to 100%, with the different colours showing the relative
# contribution of LC, NT, VU, EN, and CR species.
#
# scale_fill_manual() assigns specific colours to the five IUCN
# categories, while scale_y_continuous() fixes the y-axis at
# 0–100%.
# scale_x_discrete() defines the display labels for the four groups.
# Finally, labs() defines the axis and legend labels, while
# theme_classic() and theme() control the overall appearance
# of the plot and place the legend at the top.

ggplot(
  daten_stacked,
  aes(
    x = Gruppe,
    y = Prozent,
    fill = category
  )
) +
  geom_col(
    width = 0.7
  ) +
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
      "Vent molluscs" = "Vent mollusks",
      "This study" = "This study",
      "Decapoda" = "Decapoda",
      "Combined total" = "Combined total"
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
#This section loads and combines the occurrence data from all CSV files in the specified folder.
#First, list.files() identifies all CSV files in the folder. The lapply() function then processes each file individually. The code checks for different possible column names for latitude and longitude (Latitude/Longitude or dec_lat/dec_long) and extracts the available coordinates. The scientific name is taken from the sci_name column; if this column is not available, the filename is used as the species name.
#The extracted latitude, longitude, and species information from all files are then combined into a single dataset called all_data. Records without valid geographic coordinates are removed.
#Finally, coords is created as a simplified dataset containing only the longitude, latitude, and species name, which are the variables required for the subsequent spatial analysis.

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
#This section creates a new dataset called coords containing only the three variables needed for the spatial analysis: longitude, latitude, and species.
#The original dataset all_data may contain additional information, but only these three variables are required to assign each species occurrence to a spatial grid cell and calculate species richness.

coords <- all_data[, c(
  "Longitude",
  "Latitude",
  "species"
)]


# ============================================================
# 3. CREATE GLOBAL HEXAGONAL GRID
# ============================================================
#This section creates a global hexagonal spatial grid using the hexagrid() function from the icosa package.
#The argument deg = 5 defines a grid with an average edge length of approximately 5 degrees. The argument sf = TRUE creates the grid as an sf spatial object, which allows it to be used with other spatial analysis functions.
#The resulting object hex represents the global set of hexagonal grid cells that will later be used to assign species occurrence records and calculate species richness for each cell.

hex <- hexagrid(
  deg = 5,
  sf = TRUE
)


# ============================================================
# 4. ASSIGN RECORDS TO GRID CELLS
# ============================================================
#This section assigns each species occurrence record to a specific hexagonal grid cell.
#The locate() function determines which 5° hexagonal cell contains each occurrence based on its longitude and latitude. The resulting cell identifier is stored in a new column called cell in the coords dataset.
#This allows the subsequent analysis to group species occurrences by grid cell and calculate species richness for each cell.

coords$cell <- locate(
  hex,
  coords[, c("Longitude", "Latitude")]
)


# ============================================================
# 5. CALCULATE SPECIES RICHNESS
# ============================================================

#This section calculates the species richness for each hexagonal grid cell.
#The tapply() function groups the species records according to their assigned grid cell (coords$cell). Within each cell, unique(x) identifies the distinct species, and length() counts them.
#Thus, species_richness represents the number of unique species occurring in each 5° hexagonal grid cell. If a species has multiple occurrence records within the same cell, it is counted only once.

species_richness <- tapply(
  coords$species,
  coords$cell,
  function(x) length(unique(x))
)


# ============================================================
# 6. LOAD AND TRANSFORM WORLD MAP
# ============================================================
#This section loads a global land polygon dataset and transforms it to the same coordinate reference system as the hexagonal grid.
#st_read() loads the Natural Earth land boundaries provided with the icosa package. The st_transform() function then transforms the world map to the coordinate reference system of the hexagonal grid (hex).
#This ensures that the world map and the hexagonal grid use the same spatial reference system and can therefore be displayed together correctly.

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
#This section visualises the species richness across the global 5° hexagonal grid.
#The first plot() displays the hexagonal grid and uses species_richness to colour the cells according to the number of unique species recorded in each cell. Cells with higher species richness therefore show higher values in the plot.
#The second plot() adds the global land areas from the Natural Earth dataset in grey, providing geographical context for the grid.
#Finally, points() adds the original species occurrence records as small points based on their longitude and latitude. This allows the distribution of the individual occurrence records to be seen alongside the calculated species richness.

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

