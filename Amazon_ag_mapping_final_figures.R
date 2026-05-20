### Amazon ag project mapping:
rm(list=ls())

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)

###################
#### read in data:
dat <- read.csv("Amazon_Ag_data_for_mapping_final.csv", header=T)
head(dat)

# pad with leading zeros for merge
dat <- dat %>% 
  mutate(Code = ifelse(Country=="Peru", str_pad(Code, 6, side="left", "0"), Code))
dat <- as.data.frame(dat)

########################
### read in shapefiles:
Peru_dist <- read_sf("per_admbnda_adm3_2018_EPSG3857.shp")
head(Peru_dist)

Colombia_dist <- read_sf("MGN_MPIO_POLITICO_2018_EPSG3857.shp")
head(Colombia_dist)

Brazil_dist <- read_sf("municipios_2010_EPSG3857.shp")
head(Brazil_dist)

Amazon_biome <- read_sf("Lim_Raisg.shp")
head(Amazon_biome)

##############################
# merge csv and spatial data:
dat_Peru <- dat[which(dat$Country=="Peru"),]
colnames(dat_Peru)[1] <- "IDDIST"
Peru_dist_dat <- merge(Peru_dist, dat_Peru, by=c("IDDIST"), all.x=T, all.y=T)
head(Peru_dist_dat)
nrow(Peru_dist_dat)

dat_Colombia <- dat[which(dat$Country=="Colombia"),]
colnames(dat_Colombia)[1] <- "MPIO_CCNCT"
Colombia_dist_dat <- merge(Colombia_dist, dat_Colombia, by=c("MPIO_CCNCT"), all.x=T, all.y=T)
head(Colombia_dist_dat)
nrow(Colombia_dist_dat)

dat_Brazil <- dat[which(dat$Country=="Brazil"),]
colnames(dat_Brazil)[1] <- "codigo_ibg"
Brazil_dist_dat <- merge(Brazil_dist, dat_Brazil, by=c("codigo_ibg"), all.x=T, all.y=T)
head(Brazil_dist_dat)
nrow(Brazil_dist_dat)

#merge spatial data:
head(Peru_dist_dat)
ncol(Peru_dist_dat)
Peru_dist_dat_slim <- Peru_dist_dat[,c(1,18:28)]
colnames(Peru_dist_dat_slim)[1] <- "Code"
head(Peru_dist_dat_slim)
head(Colombia_dist_dat)
ncol(Colombia_dist_dat)
Colombia_dist_dat_slim <- Colombia_dist_dat[,c(1,14:24)]
colnames(Colombia_dist_dat_slim)[1] <- "Code"
head(Colombia_dist_dat_slim)
head(Brazil_dist_dat)
ncol(Brazil_dist_dat)
Brazil_dist_dat_slim <- Brazil_dist_dat[,c(1,10:20)]
colnames(Brazil_dist_dat_slim)[1] <- "Code"
head(Brazil_dist_dat_slim)

All_countries_dat <- rbind(Brazil_dist_dat_slim, Colombia_dist_dat_slim, Peru_dist_dat_slim)
head(All_countries_dat)

########################################
# Maps for export and main text figures:
head(All_countries_dat)
nrow(All_countries_dat)
All_countries_dat_nona <- All_countries_dat[complete.cases(All_countries_dat$Forest_Pasture_Mosaic_delta), ]
nrow(All_countries_dat_nona)

# Get South America outline for map background
sa_outline <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sf")
sa_outline_3857 <- st_transform(sa_outline, st_crs(All_countries_dat))

# Define map extent from original shapefiles
brazil_bounds <- st_bbox(Brazil_dist_dat_slim)
colombia_bounds <- st_bbox(Colombia_dist_dat_slim)
peru_bounds <- st_bbox(Peru_dist_dat_slim)

All_countries_bounds <- st_bbox(All_countries_dat_nona)

all_bounds <- st_bbox(c(
  xmin = min(All_countries_bounds["xmin"]),
  xmax = max(All_countries_bounds["xmax"]),
  ymin = min(All_countries_bounds["ymin"]),
  ymax = max(All_countries_bounds["ymax"])
), crs = st_crs(All_countries_dat))

sa_outline_full_extent <- st_crop(sa_outline_3857, all_bounds)

sa_countries <- c("Brazil", "Peru", "Venezuela", "Chile", "Ecuador", 
                  "Bolivia", "Paraguay", "Uruguay", "Guyana", "Suriname", 
                  "France", "Colombia", "Argentina")

continents_map_SA <- ne_countries(scale = "large") %>%
  filter(admin %in% sa_countries) %>%
  st_union()

continents_map_SA_3857 <- st_transform(continents_map_SA, st_crs(All_countries_dat))
continents_map_SA_full_extent <- st_crop(continents_map_SA_3857, all_bounds)

SA_bounds <- st_bbox(c(
  xmin = -9200000,
  xmax = -3500000,
  ymin = -7500000,
  ymax = 1300000
), crs = st_crs(All_countries_dat))

continents_map_SA_3857_SA_extent <- st_crop(continents_map_SA_3857, SA_bounds)

#biome outline:
Amazon_biome_3857 <- st_transform(Amazon_biome, st_crs(All_countries_dat))

##################
# map for inset with study area and South America
amazon_basin_inset_map <- ggplot() +
  geom_sf(data = continents_map_SA_3857_SA_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = Country), 
          #size=0.05
          color=NA
  ) +
  geom_sf(data = Amazon_biome_3857, 
          fill = NA, 
          color = "black", 
          size = 0.3) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_inset_map


######################
# pasture patch size:
min(All_countries_dat_nona$area_mn_pasture_mosaic_delta)
max(All_countries_dat_nona$area_mn_pasture_mosaic_delta)

colour_breaks <- c(-100, -10, 0, 10, 100)
colours <- c("darkgreen", "lightgreen", "gray95", "goldenrod", "goldenrod4")

amazon_basin_pasture_patch_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = area_mn_pasture_mosaic_delta), size=0.05
          ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$area_mn_pasture_mosaic_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$area_mn_pasture_mosaic_delta)), 1),
    name = c(expression(Delta~" pasture patch size (ha)")),
    labels = function(x) ifelse(x %in% c(-10, 10), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(-200, 0, 200)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_pasture_patch_map

##################
# pasture area:
min(All_countries_dat_nona$Pasture_Mosaic_Area_ha_delta)
max(All_countries_dat_nona$Pasture_Mosaic_Area_ha_delta)

colour_breaks <- c(-150000, -7500, 0, 25000, 500000)
colours <- c("darkgreen", "lightgreen", "gray95", "goldenrod", "goldenrod4")

amazon_basin_pasture_area_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = Pasture_Mosaic_Area_ha_delta), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$Pasture_Mosaic_Area_ha_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$Pasture_Mosaic_Area_ha_delta)), 1),
    name = c(expression(Delta~" pasture area (ha)   ")),
    labels = function(x) ifelse(x %in% c(-100000, 300000), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(-200000, 400000)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_pasture_area_map

############################
# pasture forest adjacency:
min(All_countries_dat_nona$Forest_Pasture_Mosaic_delta)
max(All_countries_dat_nona$Forest_Pasture_Mosaic_delta)

colour_breaks <- c(-75000, -25000, 0, 250000, 750000)
colours <- c("darkgreen", "lightgreen", "gray95", "goldenrod", "goldenrod4")

amazon_basin_pasture_forest_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = Forest_Pasture_Mosaic_delta), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$Forest_Pasture_Mosaic_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$Forest_Pasture_Mosaic_delta)), 1),
    name = c(expression(Delta~" forest-pasture adjacency")),
    labels = function(x) ifelse(x %in% c(-50000, 500000), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 600000)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_pasture_forest_map

##################
# ag patch size:
min(All_countries_dat_nona$area_mn_agriculture_delta)
max(All_countries_dat_nona$area_mn_agriculture_delta)

colour_breaks <- c(-50, -5, 0, 50, 300)
colours <- c("darkgreen", "lightgreen", "gray95", "maroon1", "maroon4")

amazon_basin_ag_patch_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = area_mn_agriculture_delta), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$area_mn_agriculture_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$area_mn_agriculture_delta)), 1),
    name = c(expression(Delta~" crop patch size (ha)  ")),
    labels = function(x) ifelse(x %in% c(-75, 350), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(-150, 600)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_ag_patch_map

##################
# ag area:
min(All_countries_dat_nona$Agriculture_Area_ha_delta)
max(All_countries_dat_nona$Agriculture_Area_ha_delta)

colour_breaks <- c(-8000, -4000, 0, 25000, 150000)
colours <- c("darkgreen", "lightgreen", "gray95", "maroon1", "maroon4")

amazon_basin_ag_area_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = Agriculture_Area_ha_delta), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$Agriculture_Area_ha_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$Agriculture_Area_ha_delta)), 1),
    name = c(expression(Delta~" crop area (ha)  ")),
    labels = function(x) ifelse(x %in% c(-4000, 100000), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 200000)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_ag_area_map

#########################
# ag forest adjacency:
min(All_countries_dat_nona$Forest_Agriculture_delta)
max(All_countries_dat_nona$Forest_Agriculture_delta)

colour_breaks <- c(-20000, -10000, 0, 40000, 80000)
colours <- c("darkgreen", "lightgreen", "gray95", "maroon1", "maroon4")

amazon_basin_ag_forest_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = Forest_Agriculture_delta), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$Forest_Agriculture_delta),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$Forest_Agriculture_delta)), 1),
    name = c(expression(Delta~" forest-crop adjacency")),
    labels = function(x) ifelse(x %in% c(-10000, 50000), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 75000)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 22),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_ag_forest_map

##################
# malaria:
All_countries_dat_nona$log.P_falciparum_Malaria_Incidence_Per_10k.sum <- log(All_countries_dat_nona$P_falciparum_Malaria_Incidence_Per_10k.sum + 1)
min(All_countries_dat_nona$log.P_falciparum_Malaria_Incidence_Per_10k.sum)
max(All_countries_dat_nona$log.P_falciparum_Malaria_Incidence_Per_10k.sum)

colour_breaks <- c(0.1, 1, 3, 5, 7, 9)
colours <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")

amazon_basin_malaria_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = log.P_falciparum_Malaria_Incidence_Per_10k.sum), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$log.P_falciparum_Malaria_Incidence_Per_10k.sum),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$log.P_falciparum_Malaria_Incidence_Per_10k.sum)), 1),
    name = "log(Malaria)",
    labels = function(x) ifelse(x %in% c(0.5, 5), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 5, 10)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 24),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_malaria_map

##################
# dengue:
All_countries_dat_nona$log.Dengue_Incidence_Per_10k.sum <- log(All_countries_dat_nona$Dengue_Incidence_Per_10k.sum + 1)
min(All_countries_dat_nona$log.Dengue_Incidence_Per_10k.sum)
max(All_countries_dat_nona$log.Dengue_Incidence_Per_10k.sum)

colour_breaks <- c(0.1, 1, 3, 4.5, 6, 7.5)
colours <- c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#E56B5DFF", "#FDC328FF", "#F0F921FF")

amazon_basin_dengue_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = log.Dengue_Incidence_Per_10k.sum), size=0.05
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$log.Dengue_Incidence_Per_10k.sum),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$log.Dengue_Incidence_Per_10k.sum)), 1),
    name = "log(Dengue)",
    labels = function(x) ifelse(x %in% c(0.5, 5), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 5, 9)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 24),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_dengue_map

##################
# CL:
All_countries_dat_nona$log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum <- log(All_countries_dat_nona$Cutaneous_Leishmaniasis_Incidence_Per_10k.sum + 1)
min(All_countries_dat_nona$log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum)
max(All_countries_dat_nona$log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum)

colour_breaks <- c(0.1, 1, 3, 4.5, 6, 7.5)
colours <- c("#2B1C35FF", "#3B5698FF", "#359EAAFF", "#49C1ADFF", "#96DDB5FF", "#DEF5E5FF")

amazon_basin_CL_map <- ggplot() +
  geom_sf(data = continents_map_SA_full_extent, 
          fill = "gray", 
          color = "black", 
          size = 0.3) +
  geom_sf(data = All_countries_dat_nona, 
          aes(fill = log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum), size=0.5
  ) +
  scale_fill_gradientn(
    limits  = range(All_countries_dat_nona$log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum),
    colours = colours[c(1, seq_along(colours), length(colours))],
    values  = c(0, scales::rescale(colour_breaks, from = range(All_countries_dat_nona$log.Cutaneous_Leishmaniasis_Incidence_Per_10k.sum)), 1),
    name = "log(CL)",
    labels = function(x) ifelse(x %in% c(0.5, 5), sprintf("%.1f", x), as.character(round(x))),
    breaks = c(0, 5, 9)
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 24),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )
amazon_basin_CL_map

