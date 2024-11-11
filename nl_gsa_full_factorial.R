###################################################################################################################################################################
###################################################################################################################################################################
######                                                            GLOBAL SENSITIVITY ANALYSIS                                                               #######
######                                                               NLRX - FULL FACTORIAL                                                                  #######
###################################################################################################################################################################
###################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Initial Requirements
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#JAVA configuration
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-21")
Sys.setenv(JAVA_CPPFLAGS="-I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/win32")
Sys.setenv(JAVA_LD_LIBRARY_PATH=paste0(Sys.getenv("JAVA_HOME"), "/bin/server"))
Sys.setenv(JAVA_LIBS=paste0("-L", Sys.getenv("JAVA_HOME"), "/bin/server -ljvm"))

#Clear all
rm(list=ls())
options(scipen = 999)

#workplace
setwd("C:/Users/thiago/Desktop/model_final")

#Library
library(rJava)
library(dplyr)
library(future)
library(sensitivity)
library(nlrx)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Build the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Diretory definition
netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
modelpath <- file.path("C:/Users/thiago/Desktop/model_final/model-of-collective-interactions.nlogo")
outpath <- file.path("C:/Users/thiago/Desktop/model_final")

#nl setup
nl_gsa_ff <- nl(nlversion = "6.3.0",
                nlpath = netlogopath,
                modelpath = modelpath,
                jvmmem = 6144) #6Gb alocated

#experiment
nl_gsa_ff@experiment <- experiment(
  expname           =          "outpath_gsa_full_factorial_001",
  outpath           =          outpath,
  repetition        =          1,
  tickmetrics       =          "true",
  idsetup           =          "setup",
  idgo              =          "go",
  runtime           =          216000,
  evalticks         =          216000,
  metrics           =          c("mean-conv-travel-time",
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
  variables         =          list(# making a global sensitivity analysis with two variables: following distance and speed desired standard deviation
                                    'conv-following-dist'         = list(min = 1.4994, max = 2.9106, step = 0.1764, qfun="qunif"),
                                                                        #eco-driver  = 1.57 s
                                                                        #conv-driver = 2.84 s
                                                                        #Mean = (2.84 + 1.57)/2 = 2.205 s -> 1.323~3,087s  (±40%)
                                                                        #3.087 - 1.323 = 1.764
                                                                        # 1.764 / 10 = 0.1764 -> [step] 
                                                                        #(1.323, 1.4994, 1.6758, 1.8522, 2.0286, 2.205, 2.3814, 2.5578, 2.7342, 2.9106, 3.087)
                                    
                                                                        # (±40%) -> (±32%)
                                                                        #(1.4994, 1.6758, 1.8522, 2.0286, 2.205, 2.3814, 2.5578, 2.7342, 2.9106)
                                                                        
                                    'speed-desired-sd'            = list(min = 0.37, max = 3.33, step = 0.37, qfun="qunif")
                                                                        #speed 68~132 km/h (16.67~38.89 m/s)  (±40%)
                                                                        #38.89m/s - 27.78m/s = 11.11m/s
                                                                        #27.78m/s - 16.67m/s = 11.11m/s
                                                                        #11.11 / 3 = 3.70m/s
                                                                        #0~3.70 m/s
                                                                        #3.70 / 10 = 0.37 -> [step]
                                                                        #(0.0, 0.37, 0.74, 1.11, 1.48, 1.85, 2.22, 2.59, 2.96, 3.33, 3.70)
                                    
                                                                        # (±40%) -> (±32%)
                                                                        #(0.37, 0.74, 1.11, 1.48, 1.85, 2.22, 2.59, 2.96, 3.33)
  ),
  constants         =          list(
                                    # eco-drivers
                                    'eco-drivers-percent'         = 0,
                                    'eco-acceleration'            = 0,
                                    'eco-deceleration'            = 0,
                                    'eco-following-dist'          = 0,
                                    
                                    # global parameters
                                    'flow'                        = 1000,
                                    'seconds-to-react'            = 0.68,
                                    'speed-desired-mean'          = 27.78,

                                    # conv-drivers
                                    'conv-acceleration'           = 1.50,
                                    'conv-deceleration'           = 5.00
                                    )
)

#Check variables and constants
eval_variables_constants(nl_gsa_ff)

#Design
nl_gsa_ff@simdesign <- simdesign_ff( nl = nl_gsa_ff, nseeds = 4)

#Print the nl
print(nl_gsa_ff)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Record initial date.time
start_time      <-      Sys.time()

plan(multisession, gc = TRUE)
gsa_results     <-      run_nl_all(nl_gsa_ff, split = 11)
#gsa_results     <-      run_nl_one(nl_gsa_ff, seed = 1, siminputrow = 1)

#Record final date.time
end_time        <-      Sys.time()

#Calculate the time spent and show
elapsed_time    <-      end_time - start_time
print(elapsed_time)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Results
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Attach the results to the nl object
setsim(nl_gsa_ff, "simoutput") <- gsa_results

#Save the object
#saveRDS(nl_gsa_ff, nl_gsa_ff@experiment@outpath, "nl_gsa_ff_results.rds")

#Open the object saved
nl_gsa_ff <- readRDS("C:/Users/thiago/Desktop/model_final/nl_gsa_ff_results.rds")

#Write a file with the results
#write_simoutput(nl_gsa_ff)

#Analysis
simoutput_gsa_ff_analysis <- analyze_nl(nl_gsa_ff)


###################################################################################################################################################################
#######################################################           PLOTS - GSA FULL FACTORIAL           ############################################################
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(lemon)
library(ggpubr)
library(gridExtra)
library(viridis)

colors <- gray.colors(9, start = 0.6, end = 0)
#colors <- viridis(9)

#Removing the ±40%, passing by to ±32%
simoutput_gsa_ff_analysis <- subset(simoutput_gsa_ff_analysis, `speed-desired-sd` != 0 & `speed-desired-sd` != 3.7)
simoutput_gsa_ff_analysis <- subset(simoutput_gsa_ff_analysis, `conv-following-dist` != 1.323 & `conv-following-dist` != 3.087)

#Setting following distance as string with 2 decimals
simoutput_gsa_ff_analysis$`conv-following-dist` <- round(simoutput_gsa_ff_analysis$`conv-following-dist`, 2)
simoutput_gsa_ff_analysis$`conv-following-dist` <- sprintf("%.2f", simoutput_gsa_ff_analysis$`conv-following-dist`)




#MEAN TRAVEL TIME
#G
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`mean-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = "Mean travel time (ticks)",
                 legend = TRUE,
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#H
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`mean-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = "Mean travel time (ticks)",
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

## MEAN EMISSION C02
#I
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`mean-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = expression(Mean~CO[2]~emissions~(kg)),
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#J
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`mean-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = expression(Mean~CO[2]~emissions~(kg)),
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
                 )

#SD TRAVEL TIME
#L
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`sd-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = "SD travel time (ticks)",
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#M
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`sd-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = "SD travel time (ticks)",
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

## SD EMISSION C02
#N
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`sd-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = expression(SD~CO[2]~emissions~(kg)),
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#O
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`sd-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = expression(SD~CO[2]~emissions~(kg)),
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

## SUM TRAVEL TIME
#P
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`sum-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = "Sum travel time (ticks)",
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#Q
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`sum-conv-travel-time_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = "Sum travel time (ticks)",
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

## SUM EMISSION C02
#R
interaction.plot(simoutput_gsa_ff_analysis$`speed-desired-sd`, 
                 simoutput_gsa_ff_analysis$`conv-following-dist`,
                 simoutput_gsa_ff_analysis$`sum-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Desired speed SD (m/s)",
                 ylab = expression(Sum~CO[2]~emissions~(kg)),
                 trace.label = "Following distance\n   (s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#S
interaction.plot(simoutput_gsa_ff_analysis$`conv-following-dist`, 
                 simoutput_gsa_ff_analysis$`speed-desired-sd`,
                 simoutput_gsa_ff_analysis$`sum-conv-emission-co2_mean`,
                 
                 type = "l",
                 xlab = "Following distance (s)",
                 ylab = expression(Sum~CO[2]~emissions~(kg)),
                 trace.label = "Desired speed SD \n   (m/s)",
                 fixed = TRUE,
                 col = colors,
                 lty = 1,
                 lwd = 3,
                 xpd = TRUE
)

#*******************************************************************************************************************************************************************
#*******************************************************************************************************************************************************************
#**********************************    END OF GLOBAL SENSITIVITY ANALYSIS WITH FULL FACTORIAL DESIGN AND CORRESPONDING PLOTS    ************************************
#*******************************************************************************************************************************************************************
#*******************************************************************************************************************************************************************
#*******************************************************************************************************************************************************************