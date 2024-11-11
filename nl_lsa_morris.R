###################################################################################################################################################################
###################################################################################################################################################################
######                                                              LOCAL SENSITIVITY ANALYSIS                                                              #######
######                                                          NLRX - MODIFIED MORRIS SCREENING                                                            #######
###################################################################################################################################################################
###################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Initial Requirements
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# JAVA configuration
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-21")
Sys.setenv(JAVA_CPPFLAGS="-I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/win32")
Sys.setenv(JAVA_LD_LIBRARY_PATH=paste0(Sys.getenv("JAVA_HOME"), "/bin/server"))
Sys.setenv(JAVA_LIBS=paste0("-L", Sys.getenv("JAVA_HOME"), "/bin/server -ljvm"))

# Clear all
rm(list=ls())
options(scipen = 999)

# Workplace
setwd("C:/Users/thiago/Desktop/model_final")

# Library
#install.packages("rJava")
#install.packages("dplyr")
#install.packages("future")
#install.packages("sensitivity")
#install.packages("nlrx")
library(rJava)
library(dplyr)
library(future)
library(sensitivity)
library(nlrx)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Build the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Diretory definition
netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
modelpath <- file.path("C:/Users/thiago/Desktop/model_final/model-of-collective-interactions.nlogo")
outpath <- file.path("C:/Users/thiago/Desktop/model_final")

# nl setup
nl_lsa_morris <- nl(nlversion = "6.3.0",
                    nlpath = netlogopath,
                    modelpath = modelpath,
                   jvmmem = 14336) #14Gb alocated

# experiment
nl_lsa_morris@experiment <- experiment(
  expname           =          "outpath_lsa_morris_001",
  outpath           =          outpath,
  repetition        =          1,
  tickmetrics       =          "true",
  idsetup           =          "setup",
  idgo              =          "go",
  runtime           =          216000,
  evalticks         =          216000,
  metrics           =          c("mean-conv-nearest-follow-dist-car-ahead",
                                 "median-conv-nearest-follow-dist-car-ahead",
                                 "sd-conv-nearest-follow-dist-car-ahead",
                                 
                                 "mean-conv-travel-time",
                                 "median-conv-travel-time",
                                 "sd-conv-travel-time",
                                 "sum-conv-travel-time",
                                 
                                 "mean-conv-emission-co2",
                                 "median-conv-emission-co2",
                                 "sd-conv-emission-co2",
                                 "sum-conv-emission-co2",
                                 
                                 "traffic-flow-model",
                                 "traffic-density-model"
                                ),
  variables         =          list('flow'                     = list(min = 600, max = 1400, qfun = "qunif"), 
                                    #Mean = 1000 cars/h               -> 600~1400 cars/h              (±40%)
                                    
                                    'conv-acceleration'        = list(min = 0.75, max = 1.75, qfun = "qunif"),
                                    #eco-driver  = 1.0 m/s²
                                    #conv-driver = 1.5 m/s²
                                    #Mean = (1 + 1.5) / 2 = 1.25 m/s² -> 0.75~1.75 m/s²               (±40%)
                                    
                                    'conv-deceleration'        = list(min = 2.4, max = 5.6, qfun = "qunif"),
                                    #eco-driver  = 3.0 m/s²
                                    #conv-driver = 5.0 m/s²
                                    #Mean = (3 + 5) / 2 = 4 m/s²      -> 2.4~5.6 m/s²                 (±40%)
                                    
                                    'seconds-to-react'         = list(min = 0.408, max = 0.952, qfun = "qunif"),
                                    #Mean = 0.68 s                    -> 0.408~0.952 s                (±40%)
                                    
                                    'conv-following-dist'      = list(min = 1.323, max = 3.087, qfun = "qunif"),
                                    #eco-driver  = 1.57 s
                                    #conv-driver = 2.84 s
                                    #Mean = (2.84 + 1.57)/2 = 2.205 s -> 1.323~3,087 s                (±40%)
                                    
                                    'speed-desired-sd'         = list(min = 0, max = 3.70, qfun = "qunif")
                                    #speed 60~140 km/h (16.67~38.89 m/s)                              (±40%)
                                    #38.89m/s - 27.78m/s = 11.11m/s
                                    #27.78m/s - 16.67m/s = 11.11m/s   -> (I considered this value to be three times the standard deviation)
                                    #11.11 / 3 = 3.70m/s
                                    #0~3.70 m/s
  ),
  constants         =          list('eco-drivers-percent'         = 0,
                                    'eco-acceleration'            = 0,
                                    'eco-deceleration'            = 0,
                                    'eco-following-dist'          = 0,
                                    'speed-desired-mean'          = 27.78
                                    )
)


# Check variables and constants
eval_variables_constants(nl_lsa_morris)

# Design
nl_lsa_morris@simdesign <- simdesign_morris(nl                = nl_lsa_morris,
                                            morristype        = "oat",
                                            morrislevels      = 4,
                                            morrisr           = 60,
                                            morrisgridjump    = 2,
                                            nseeds            = 4
)


# Print the nl
print(nl_lsa_morris)


#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Record initial date.time
start_time      <-      Sys.time()

plan(multisession, gc = TRUE)
lsa_results     <-      run_nl_all(nl_lsa_morris, split = 10)
#lsa_results1 <- run_nl_one(nl_lsa_morris, seed = 1, siminputrow = 1)


# Record final date.time
end_time        <-      Sys.time()

# Calculate the time spent and show
elapsed_time    <-      end_time - start_time
print(elapsed_time)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Results
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Attach the results to the nl object
setsim(nl_lsa_morris, "simoutput") <- lsa_results


# Save the object
#saveRDS(nl_lsa_morris, file = "nl_lsa_morris.rds")
nl_lsa_morris <- readRDS("nl_lsa_morris.rds")
#lsa_results <- read.csv("outpath_lsa_morris_001_morris.csv",check.names = FALSE, header = T, sep = ",")


# Write a file with the results
#write_simoutput(nl_lsa_morris)


# Analysis
eval_output <- eval_simoutput(nl_lsa_morris)
simoutput_morris_analysis <- analyze_nl(nl_lsa_morris)

save(simoutput_morris_analysis, file = "simoutput_morris_analysis.RData")

###################################################################################################################################################################
###################################################################################################################################################################
#######################################################                                                ############################################################
#######################################################            PLOTS - MORRIS SCREENING            ############################################################
#######################################################                                                ############################################################
###################################################################################################################################################################
###################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Initial Requirements
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Library
library(dplyr)
library(ggplot2)
library(lemon)
library(ggpubr)
library(gridExtra)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2. Pre Processing
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Changing the name of parameters to better understanding
new_parameter_name <- c("conv-acceleration" = "Acceleration",
                        "conv-deceleration" = "Deceleration",
                        "conv-following-dist" = "Following distance",
                        "flow" = "Flow",
                        "seconds-to-react" = "Reaction time",
                        "speed-desired-sd" = "Desired speed SD")

simoutput_morris_analysis$parameter <- new_parameter_name[simoutput_morris_analysis$parameter]


# Create a new df with the mean of different seeds
simoutput_morris_analysis2 <- simoutput_morris_analysis %>%
  group_by(metric, parameter, index) %>%
  summarize(media_value = mean(value, na.rm = TRUE))


simoutput_morris_analysis2 <- simoutput_morris_analysis2 %>%
  mutate(mu_value = ifelse(index == "mu", media_value, NA),
         mustar_value = ifelse(index == "mustar", media_value, NA),
         sigma_value = ifelse(index == "sigma", media_value, NA))

simoutput_morris_analysis2 <- simoutput_morris_analysis2 %>%
  select(metric, parameter, mu_value, mustar_value, sigma_value) %>% 
  distinct(metric, parameter, mu_value, mustar_value, sigma_value, .keep_all = TRUE)



simoutput_morris_analysis2 <- simoutput_morris_analysis2 %>%
  group_by(metric, parameter) %>%
  summarize(mu_value = mu_value[which.min(is.na(mu_value))],
            mustar_value = mustar_value[which.min(is.na(mustar_value))],
            sigma_value = sigma_value[which.min(is.na(sigma_value))])

# A data frame for every output
## Mean
df_mean_conv_emission_co2 <- simoutput_morris_analysis2 %>%
  filter(metric == "mean-conv-emission-co2_mean")

df_mean_conv_nearest_follow_dist_car_ahead <- simoutput_morris_analysis2 %>%
  filter(metric == "mean-conv-nearest-follow-dist-car-ahead_mean")

df_mean_conv_travel_time <- simoutput_morris_analysis2 %>%
  filter(metric == "mean-conv-travel-time_mean")
## Median
df_median_conv_emission_co2 <- simoutput_morris_analysis2 %>%
  filter(metric == "median-conv-emission-co2_mean")

df_median_conv_nearest_follow_dist_car_ahead <- simoutput_morris_analysis2 %>%
  filter(metric == "median-conv-nearest-follow-dist-car-ahead_mean")

df_median_conv_travel_time <- simoutput_morris_analysis2 %>%
  filter(metric == "median-conv-travel-time_mean")
## Standard-deviation
df_sd_conv_emission_co2 <- simoutput_morris_analysis2 %>%
  filter(metric == "sd-conv-emission-co2_mean")

df_sd_conv_nearest_follow_dist_car_ahead <- simoutput_morris_analysis2 %>%
  filter(metric == "sd-conv-nearest-follow-dist-car-ahead_mean")

df_sd_conv_travel_time <- simoutput_morris_analysis2 %>%
  filter(metric == "sd-conv-travel-time_mean")
## Sum
df_sum_conv_emission_co2 <- simoutput_morris_analysis2 %>%
  filter(metric == "sum-conv-emission-co2_mean")

df_sum_conv_travel_time <- simoutput_morris_analysis2 %>%
  filter(metric == "sum-conv-travel-time_mean")
## Traffic density
df_traffic_density <- simoutput_morris_analysis2 %>%
  filter(metric == "traffic-density-model_mean")
## Traffic flow
df_traffic_flow <- simoutput_morris_analysis2 %>%
  filter(metric == "traffic-flow-model_mean")

#******************************************************************************************************************************************************************
# 3. PLOTS
#******************************************************************************************************************************************************************
#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3.1 - TRAVEL TIME
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (A) mean-conv-travel-time_mean
plotA1 <- ggplot(df_mean_conv_travel_time,
                aes(
                  x = mustar_value,
                  y = mu_value,
                  colour = parameter,
                  shape = parameter,
                  size = 3
                )) +
  geom_point() +
  xlim(-1000,4000) +
  ylim(-1000,4000) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(A)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotA1)

plotA2 <- ggplot(df_mean_conv_travel_time,
                aes(
                  x = mustar_value,
                  y = sigma_value,
                  colour = parameter,
                  shape = parameter,
                  size = 3
                )) +
  geom_point() +
  xlim(-1000,4000) +
  ylim(-1000,4000) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotA2)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (B) sd-conv-travel-time_mean
plotB1 <- ggplot(df_sd_conv_travel_time,
                 aes(
                   x = mustar_value,
                   y = mu_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(0,1000) +
  ylim(-200,800) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(B)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotB1)

plotB2 <- ggplot(df_sd_conv_travel_time,
                 aes(
                   x = mustar_value,
                   y = sigma_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(0,1000) +
  ylim(-200,800) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotB2)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (C) sum-conv-travel-time_mean
plotC1 <- ggplot(df_sum_conv_travel_time,
                 aes(
                   x = mustar_value,
                   y = mu_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(-5000000,50000000) +
  ylim(-5000000,50000000) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(C)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotC1)

plotC2 <- ggplot(df_sum_conv_travel_time,
                 aes(
                   x = mustar_value,
                   y = sigma_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(-5000000,50000000) +
  ylim(-5000000,50000000) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotC2)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3.2 - CO2 EMISSIONS
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (D) mean-conv-emission-co2_mean
plotD1 <- ggplot(df_mean_conv_emission_co2,
                aes(
                  x = mustar_value,
                  y = mu_value,
                  colour = parameter,
                  shape = parameter,
                  size = 3
                )) +
  geom_point() +
  xlim(0,0.6) +
  ylim(-0.3,0.3) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(D)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  geom_abline(slope = 0, intercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0.3, linetype = "dotted") +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotD1)

plotD2 <- ggplot(df_mean_conv_emission_co2,
                aes(
                  x = mustar_value,
                  y = sigma_value,
                  colour = parameter,
                  shape = parameter,
                  size = 3
                )) +
  geom_point() +
  xlim(0,0.6) +
  ylim(0,0.6) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  geom_abline(slope = 0, intercept = 0.3, linetype = "dotted") +
  geom_vline(xintercept = 0.3, linetype = "dotted") +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotD2)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (E) sd-conv-emission-co2_mean
plotE1 <- ggplot(df_sd_conv_emission_co2,
                 aes(
                   x = mustar_value,
                   y = mu_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(0,0.6) +
  ylim(-0.3,0.3) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(E)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotE1)

plotE2 <- ggplot(df_sd_conv_emission_co2,
                 aes(
                   x = mustar_value,
                   y = sigma_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(0,0.6) +
  ylim(-0.3,0.3) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotE2)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

## (F) sum-conv-emission-co2_mean
plotF1 <- ggplot(df_sum_conv_emission_co2,
                 aes(
                   x = mustar_value,
                   y = mu_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(-5000,15000) +
  ylim(-5000,15000) +
  labs(x = expression(paste(mu^"*")), y = expression(mu), tag = "(F)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotF1)

plotF2 <- ggplot(df_sum_conv_emission_co2,
                 aes(
                   x = mustar_value,
                   y = sigma_value,
                   colour = parameter,
                   shape = parameter,
                   size = 3
                 )) +
  geom_point() +
  xlim(-5000,15000) +
  ylim(-5000,15000) +
  labs(x = expression(paste(mu^"*")), y = expression(sigma), tag = "") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  #theme(axis.title.x = element_blank()) +
  scale_size(range = 3) + guides(size = "none")
print(plotF2)
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

plot_travel_time <- ggarrange(plotA1, plotA2, plotB1, plotB2, plotC1, plotC2,
                              ncol = 2,
                              nrow = 3,
                              common.legend = TRUE,
                              legend = "bottom",
                              font.label = list(size = 18, face = "bold")
)

print(plot_travel_time)






plot_emission_co2 <- ggarrange(plot1, plot2, plot13, plot14, plot19, plot20,
                               ncol = 2,
                               nrow = 3,
                               common.legend = TRUE,
                               #equal.widths = TRUE, 
                               #equal.heights = TRUE,
                               legend = "bottom",
                               font.label = list(size = 18, face = "bold")
)  

print(plot_emission_co2)




