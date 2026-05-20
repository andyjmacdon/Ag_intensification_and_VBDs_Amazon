# Ag_intensification_and_VBDs_Amazon
Repository containing final data and code for agricultural land use change and intensification effects on vector-borne diseases in the Amazon project.

The code file: "ag_intensification_VBD_final_analysis.R" analyzes the effect of pasture and crop land use change, fragmentation and intensification on P. falciparum malaria, dengue virus and cutaneous leishmaniasis incidence across the Brazilian, Peruvian and Colombian Amazon region of South America.
It includes the main model specifications for each of the three diseases as reported in the main manuscript and reproduces the panels of the main results figure (Figure 4).
The data files: "Malaria_final_dataset.csv", “Dengue_final_dataset.csv”, and “CL_final_dataset.csv” are the final cleaned datasets used in the analyses, and include the following columns:
1.	Code_unique: unique municipality codes identifying the panel units and used as fixed effects in models
2.	Year: year variable identifying the time dimension of the panel and used as fixed effects (interacted with country) in models
3.	Country: country identifier, interacted with year as fixed effects in models
4.	log.P_falciparum_Malaria_Incidence_Per_10k: log-transformed P. falciparum malaria incidence (+1)
5.	log.Cutaneous_Leishmaniasis_Incidence_Per_10k: log-transformed CL incidence (+1)
6.	log.Cutaneous_Leishmaniasis_Incidence_Per_10k: log-transformed CL incidence (+1)
7.	log.Dengue_Incidence_Per_10k: log-transformed dengue incidence (+1)
8.	log.Pasture_Mosaic_Area_ha: log-transformed pasture area (ha +1)
9.	log.Forest_Pasture_Mosaic: log-transformed adjacency of forest and pasture pixels (+1)
10.	log.area_mn_pasture_mosaic: log-transformed average pasture patch size (ha +1; calculated using landscapemetrics package, AREA_MN)
11.	log.enn_mn_pasture_mosaic: log-transformed average pasture patch isolation (+1; calculated using landscapemetrics package, ENN_MN)
12.	log.Agriculture_Area_ha: log-transformed cropland area (ha +1)
13.	log.Forest_Agriculture: log-transformed adjacency of forest and cropland pixels (+1)
14.	log.area_mn_agriculture: log-transformed average cropland patch size (ha +1; calculated using landscapemetrics package, AREA_MN)
15.	log.enn_mn_agriculture: log-transformed average cropland patch isolation (+1; calculated using landscapemetrics package, ENN_MN)
16.	log.Urban_Area_ha: log-transformed urban area (ha +1)
17.	log.Forest_Area_ha: log-transformed forest area (ha +1)
18.	log.GFC_Forest_Loss_ha: log-transformed forest area lost (ha +1)
19.	lag.log.AOD_mean: 1 year lagged, log transformed aerosol optical depth used as instrument for forest loss in malaria model
20.	NDVI_std: z-score transformed average annual normalized difference vegetation index
21.	ERA5_mean_temp_std: z-score transformed average annual temperature
22.	Precip_std: z-score transformed cumulative annual precipitation
23.	Population_Density_std: z-score transformed annual population density
    
The “Amazon_ag_mapping_final_figures.R" code file contains code for making the map figures in figures 2 and 3, importing the Peru, Brazil and Colombia shapefiles contained in this repository, and the “Amazon_Ag_data_for_mapping_final.csv” data file. The data file contains columns representing: 

1.	Cumulative disease burden for each of the three diseases (P. falciparum malaria, dengue, cutaneous leishmaniasis)
2.	Change in total a) pasture and b) cropland area (2021 – 2007 for Brazil and Colombia and 2021 – 2010 for Peru)
3.	Change in a) forest-pasture and b) forest-cropland adjacency (2021 – 2007 for Brazil and Colombia and 2021 – 2010 for Peru)
4.	Change in average patch size of a) pasture and b) cropland (2021 – 2007 for Brazil and Colombia and 2021 – 2010 for Peru)
5.	Municipality code and country for merging with shapefiles

