##########################################################
# analysis script for Amazon VBD's and ag intensification

rm(list=ls())
library(tidyverse)
library(sjPlot)
library(regclass)
library(sandwich)
library(fixest)
library(marginaleffects)

########################################################################
# P. falciparum malaria, final model:
########################################################################
Malaria_final_dataset <- read.csv("Malaria_final_dataset.csv", header=T)
head(Malaria_final_dataset)

malaria_feols_FEs_Covs_countryByYear_ag_pasture <- feols(
  log.P_falciparum_Malaria_Incidence_Per_10k ~
    log.Pasture_Mosaic_Area_ha
    + log.Forest_Pasture_Mosaic
    + log.area_mn_pasture_mosaic
    + I((log.area_mn_pasture_mosaic)^2)
    + log.enn_mn_pasture_mosaic
    + I((log.enn_mn_pasture_mosaic)^2)
    + log.Agriculture_Area_ha
  + log.Forest_Agriculture
  + log.area_mn_agriculture
  + I((log.area_mn_agriculture)^2)
  + log.enn_mn_agriculture
  + I((log.enn_mn_agriculture)^2)
  + log.Forest_Area_ha
  + NDVI_std
  + ERA5_mean_temp_std
  + I(ERA5_mean_temp_std^2)
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Code_unique 
  |
    log.GFC_Forest_Loss_ha ~
    lag.log.AOD_mean
  ,
  data=Malaria_final_dataset,
  cluster = c("Code_unique")
)
summary(malaria_feols_FEs_Covs_countryByYear_ag_pasture)
collinearity(malaria_feols_FEs_Covs_countryByYear_ag_pasture)

###################################
# coefficient plots for figure 4:
###################################
Pasture_coefs <- plot_model(malaria_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                                   terms=c("log.Pasture_Mosaic_Area_ha", "log.Forest_Pasture_Mosaic",
                                           "log.area_mn_pasture_mosaic", "I((log.area_mn_pasture_mosaic)^2)",
                                            "log.enn_mn_pasture_mosaic", "I((log.enn_mn_pasture_mosaic)^2)"),
                                   axis.labels = c("Pasture patch isolation sqrd", "Pasture patch isolation",
                                                   "Pasture patch size sqrd", "Pasture patch size", 
                                                   "Forest-pasture", "Pasture area ha"),
                                   show.values = TRUE, value.offset = .3,
                            title="Malaria ~ Pasture", vline.color = "black")
Pasture_coefs + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

Ag_coefs <- plot_model(malaria_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                                   terms=c("log.Agriculture_Area_ha", "log.Forest_Agriculture", 
                                           "log.area_mn_agriculture", "I((log.area_mn_agriculture)^2)", 
                                           "log.enn_mn_agriculture", "I((log.enn_mn_agriculture)^2)"),
                                   axis.labels = c("Crop patch isolation sqrd", "Crop patch isolation",
                                                   "Crop patch size sqrd", "Crop patch size",
                                                   "Forest-crop", "Crop area ha"),
                                   show.values = TRUE, value.offset = .3,
                       title="Malaria ~ Crops", vline.color = "black")
Ag_coefs + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

#####################################################################################################
# calculate and plot marginal effects over the range of quadratic land use variables for figure 4:
#####################################################################################################
# malaria~pasture patch size
malaria_ME_pasture_patch <- slopes(malaria_feols_FEs_Covs_countryByYear_ag_pasture, variables="log.area_mn_pasture_mosaic"
                                   , newdata = datagrid(log.area_mn_pasture_mosaic=seq(min(Malaria_final_dataset$log.area_mn_pasture_mosaic), max(Malaria_final_dataset$log.area_mn_pasture_mosaic), length.out=100))
                                   , vcov = vcovCL)
malaria_ME_pasture_patch_df <- as.data.frame(malaria_ME_pasture_patch)
preds_malaria_pasture_patch <- plot_predictions(malaria_feols_FEs_Covs_countryByYear_ag_pasture, condition = "log.area_mn_pasture_mosaic", vcov=FALSE, draw=FALSE)

Malaria_final_dataset[which(Malaria_final_dataset$log.area_mn_pasture_mosaic >= 1.999 &
                              Malaria_final_dataset$log.area_mn_pasture_mosaic <= 2.001),]

Malaria_final_dataset[which(Malaria_final_dataset$log.area_mn_pasture_mosaic >= 3.999 &
                              Malaria_final_dataset$log.area_mn_pasture_mosaic <= 4.001),]

Malaria_final_dataset[which(Malaria_final_dataset$log.area_mn_pasture_mosaic >= 5.999 &
                              Malaria_final_dataset$log.area_mn_pasture_mosaic <= 6.001),]

x_crosswalk <- c("0" = 0, "6" = 2, "53" = 4, "402" = 6)

ggplot(malaria_ME_pasture_patch_df, aes(x=log.area_mn_pasture_mosaic, y=estimate)) + geom_line(linewidth=1.5, color="goldenrod") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill="goldenrod") +
  scale_x_continuous(
    breaks = as.numeric(x_crosswalk), # Original values for breaks
    labels = names(x_crosswalk)      # Custom labels
  ) +
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  theme_minimal(base_size = 22) + xlab("Pasture patch size, ha (log scale)") +
  geom_line(data=preds_malaria_pasture_patch, aes(x=log.area_mn_pasture_mosaic, y=estimate*0.7), color="gray", linewidth=1.5, linetype=2) +
  scale_y_continuous(name="Marginal Effect, 95% CI\n(solid yellow)", sec.axis=sec_axis(~.*(1/0.7), name="Predicted relationship\n(dashed grey)")) +
  ggtitle("Malaria ~ pasture patch size") + theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.5))


############################
# dengue, final model:
############################
Dengue_final_dataset <- read.csv("Dengue_final_dataset.csv", header=T)
head(Dengue_final_dataset)

dengue_feols_FEs_Covs_countryByYear_ag_pasture <- feols(
  log.Dengue_Incidence_Per_10k ~
    log.Pasture_Mosaic_Area_ha
  + log.Forest_Pasture_Mosaic
  + log.area_mn_pasture_mosaic
  + I((log.area_mn_pasture_mosaic)^2)
  + log.enn_mn_pasture_mosaic
  + I((log.enn_mn_pasture_mosaic)^2)
  + log.Agriculture_Area_ha
  + log.Forest_Agriculture
  + log.area_mn_agriculture
  + I((log.area_mn_agriculture)^2)
  + log.enn_mn_agriculture
  + I((log.enn_mn_agriculture)^2)
  + log.Urban_Area_ha
  + I((log.Urban_Area_ha)^2)
  + NDVI_std
  + ERA5_mean_temp_std
  + I(ERA5_mean_temp_std^2)
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Code_unique 
  ,
  data=Dengue_final_dataset,
  cluster = c("Code_unique")
)
summary(dengue_feols_FEs_Covs_countryByYear_ag_pasture)
collinearity(dengue_feols_FEs_Covs_countryByYear_ag_pasture)

###################################
# coefficient plots for figure 4:
###################################
Pasture_coefs_dengue <- plot_model(dengue_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                            terms=c("log.Pasture_Mosaic_Area_ha", "log.Forest_Pasture_Mosaic",
                                    "log.area_mn_pasture_mosaic", "I((log.area_mn_pasture_mosaic)^2)",
                                    "log.enn_mn_pasture_mosaic", "I((log.enn_mn_pasture_mosaic)^2)"),
                            axis.labels = c("Pasture patch isolation sqrd", "Pasture patch isolation",
                                            "Pasture patch size sqrd", "Pasture patch size", 
                                            "Forest-pasture", "Pasture area ha"),
                            show.values = TRUE, value.offset = .3,
                            title="Dengue ~ Pasture", vline.color = "black")
Pasture_coefs_dengue + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

Ag_coefs_dengue <- plot_model(dengue_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                       terms=c("log.Agriculture_Area_ha", "log.Forest_Agriculture", 
                               "log.area_mn_agriculture", "I((log.area_mn_agriculture)^2)", 
                               "log.enn_mn_agriculture", "I((log.enn_mn_agriculture)^2)"),
                       axis.labels = c("Crop patch isolation sqrd", "Crop patch isolation",
                                       "Crop patch size sqrd", "Crop patch size",
                                       "Forest-crop", "Crop area ha"),
                       show.values = TRUE, value.offset = .3,
                       title="Dengue ~ Crops", vline.color = "black")
Ag_coefs_dengue + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

#####################################################################################################
# calculate and plot marginal effects over the range of quadratic land use variables for figure 4:
#####################################################################################################
# dengue ag patch size
dengue_ME_ag_patch <- slopes(dengue_feols_FEs_Covs_countryByYear_ag_pasture, variables="log.area_mn_agriculture"
                              , newdata = datagrid(log.area_mn_agriculture=seq(min(Dengue_final_dataset$log.area_mn_agriculture), max(Dengue_final_dataset$log.area_mn_agriculture), length.out=100))
                              , vcov = vcovCL)
dengue_ME_ag_patch_df <- as.data.frame(dengue_ME_ag_patch)
preds_dengue_ag_patch <- plot_predictions(dengue_feols_FEs_Covs_countryByYear_ag_pasture, condition = "log.area_mn_agriculture", vcov=FALSE, draw=FALSE)

Dengue_final_dataset[which(Dengue_final_dataset$log.area_mn_agriculture >= 1.999 &
                             Dengue_final_dataset$log.area_mn_agriculture <= 2.001),]

Dengue_final_dataset[which(Dengue_final_dataset$log.area_mn_agriculture >= 3.999 &
                             Dengue_final_dataset$log.area_mn_agriculture <= 4.001),]

Dengue_final_dataset[which(Dengue_final_dataset$log.area_mn_agriculture >= 5.9 &
                             Dengue_final_dataset$log.area_mn_agriculture <= 6.1),]

x_crosswalk <- c("0" = 0, "6" = 2, "54" = 4, "413" = 6)

ggplot(dengue_ME_ag_patch_df, aes(x=log.area_mn_agriculture, y=estimate)) + geom_line(linewidth=1.5, color="maroon") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill="maroon") +
  scale_x_continuous(
    breaks = as.numeric(x_crosswalk), # Original values for breaks
    labels = names(x_crosswalk)      # Custom labels
  ) +
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  theme_minimal(base_size = 22) + xlab("Crop patch size, ha (log scale)") +
  geom_line(data=preds_dengue_ag_patch, aes(x=log.area_mn_agriculture, y=estimate*0.2), color="gray", linewidth=1.5, linetype=2) +
  scale_y_continuous(name="Marginal Effect, 95% CI\n(solid pink)", sec.axis=sec_axis(~.*(1/0.2), name="Predicted relationship\n(dashed grey)")) +
  ggtitle("Dengue ~ crop patch size") + theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.5))


##############################
# CL, final model:
##############################
CL_final_dataset <- read.csv("CL_final_dataset.csv", header=T)
head(CL_final_dataset)

CL_feols_FEs_Covs_countryByYear_ag_pasture <- feols(
  log.Cutaneous_Leishmaniasis_Incidence_Per_10k ~
    log.Pasture_Mosaic_Area_ha
  + log.Forest_Pasture_Mosaic
  + log.area_mn_pasture_mosaic
  + I((log.area_mn_pasture_mosaic)^2)
  + log.enn_mn_pasture_mosaic
  + I((log.enn_mn_pasture_mosaic)^2)
  + log.Agriculture_Area_ha
  + log.Forest_Agriculture
  + log.area_mn_agriculture
  + I((log.area_mn_agriculture)^2)
  + log.enn_mn_agriculture
  + I((log.enn_mn_agriculture)^2)
  + log.Urban_Area_ha
  + log.Forest_Area_ha
  + log.GFC_Forest_Loss_ha 
  + NDVI_std
  + ERA5_mean_temp_std
  + I(ERA5_mean_temp_std^2)
  + Precip_std
  + Population_Density_std
  |
    Year^Country +
    Code_unique 
  ,
  data=CL_final_dataset,
  cluster = c("Code_unique")
)
summary(CL_feols_FEs_Covs_countryByYear_ag_pasture)
collinearity(CL_feols_FEs_Covs_countryByYear_ag_pasture)

###################################
# coefficient plots for figure 4:
###################################
Pasture_coefs_CL <- plot_model(CL_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                                   terms=c("log.Pasture_Mosaic_Area_ha", "log.Forest_Pasture_Mosaic",
                                           "log.area_mn_pasture_mosaic", "I((log.area_mn_pasture_mosaic)^2)",
                                           "log.enn_mn_pasture_mosaic", "I((log.enn_mn_pasture_mosaic)^2)"),
                                   axis.labels = c("Pasture patch isolation sqrd", "Pasture patch isolation",
                                                   "Pasture patch size sqrd", "Pasture patch size", 
                                                   "Forest-pasture", "Pasture area ha"),
                                   show.values = TRUE, value.offset = .3,
                                   title="CL ~ Pasture", vline.color = "black")
Pasture_coefs_CL + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

Ag_coefs_CL <- plot_model(CL_feols_FEs_Covs_countryByYear_ag_pasture, type="est",
                              terms=c("log.Agriculture_Area_ha", "log.Forest_Agriculture", 
                                      "log.area_mn_agriculture", "I((log.area_mn_agriculture)^2)", 
                                      "log.enn_mn_agriculture", "I((log.enn_mn_agriculture)^2)"),
                              axis.labels = c("Crop patch isolation sqrd", "Crop patch isolation",
                                              "Crop patch size sqrd", "Crop patch size",
                                              "Forest-crop", "Crop area ha"),
                              show.values = TRUE, value.offset = .3,
                              title="CL ~ Crops", vline.color = "black")
Ag_coefs_CL + theme_sjplot(base_size = 22) + scale_y_continuous(limits = .8 * c(-1, 1))

#####################################################################################################
# calculate and plot marginal effects over the range of quadratic land use variables for figure 4:
#####################################################################################################
# CL ag patch size
CL_ME_ag_patch <- slopes(CL_feols_FEs_Covs_countryByYear_ag_pasture, variables="log.area_mn_agriculture"
                             , newdata = datagrid(log.area_mn_agriculture=seq(min(CL_final_dataset$log.area_mn_agriculture), max(CL_final_dataset$log.area_mn_agriculture), length.out=100))
                             , vcov = vcovCL)
CL_ME_ag_patch_df <- as.data.frame(CL_ME_ag_patch)
preds_CL_ag_patch <- plot_predictions(CL_feols_FEs_Covs_countryByYear_ag_pasture, condition = "log.area_mn_agriculture", vcov=FALSE, draw=FALSE)

CL_final_dataset[which(CL_final_dataset$log.area_mn_agriculture >= 1.999 &
                         CL_final_dataset$log.area_mn_agriculture <= 2.001),]

CL_final_dataset[which(CL_final_dataset$log.area_mn_agriculture >= 3.999 &
                         CL_final_dataset$log.area_mn_agriculture <= 4.001),]

CL_final_dataset[which(CL_final_dataset$log.area_mn_agriculture >= 5.9 &
                         CL_final_dataset$log.area_mn_agriculture <= 6.1),]

x_crosswalk <- c("0" = 0, "6" = 2, "54" = 4, "413" = 6)

ggplot(CL_ME_ag_patch_df, aes(x=log.area_mn_agriculture, y=estimate)) + geom_line(linewidth=1.5, color="maroon") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2, fill="maroon") +
  scale_x_continuous(
    breaks = as.numeric(x_crosswalk), # Original values for breaks
    labels = names(x_crosswalk)      # Custom labels
  ) +
  geom_hline(yintercept=0, linetype="dashed", color="black") +
  theme_minimal(base_size = 22) + xlab("Crop patch size, ha (log scale)") +
  geom_line(data=preds_CL_ag_patch, aes(x=log.area_mn_agriculture, y=estimate*0.2), color="gray", linewidth=1.5, linetype=2) +
  scale_y_continuous(name="Marginal Effect, 95% CI\n(solid pink)", sec.axis=sec_axis(~.*(1/0.2), name="Predicted relationship\n(dashed grey)")) +
  ggtitle("CL ~ crop patch size") + theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.5))

