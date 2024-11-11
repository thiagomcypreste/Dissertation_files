###################################################################################################################################################################
###################################################################################################################################################################
######                                                          FLOW AND ECO-DIVER PERCENT EXPERIMENT                                                       #######
###################################################################################################################################################################
###################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Initial Requirements
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#JAVA configuration
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-21.0.1")
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
library(tidyr)
library(future)
library(sensitivity)
library(nlrx)
library(ggplot2)
library(patchwork)
library(nortest)
library(purrr)
library(openxlsx)
library(car)
library(stats)
library(broom)
library(writexl)
library(rstatix)
library(multcomp)
library(FSA)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Build the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Diretory definition
netlogopath <- file.path("C:/Program Files/NetLogo 6.3.0")
modelpath <- file.path("C:/Users/thiago/Desktop/model_final/model-of-collective-interactions.nlogo")
outpath <- file.path("C:/Users/thiago/Desktop/model_final")

#nl setup
nl_exp_ff <- nl(nlversion = "6.3.0",
                nlpath = netlogopath,
                modelpath = modelpath,
                jvmmem = 6144) #6Gb alocated

#experiment
flow_values <- c(1, seq(200, 2000, by = 200)) # Create a custom sequence for the 'flow' variable starting with a special value of 1, followed by a regular sequence from 200 to 2000 in increments of 200.

nl_exp_ff@experiment <- experiment(
  expname           =          "outpath_flow_and_eco-drivers",
  outpath           =          outpath,
  repetition        =          1,
  tickmetrics       =          "true",
  idsetup           =          "setup",
  idgo              =          "go",
  runtime           =          216000,
  evalticks         =          216000,
  metrics           =          c("mean-eco-travel-time",
                                 "median-eco-travel-time",
                                 "sd-eco-travel-time",
                                 "sum-eco-travel-time",
                                 
                                 "mean-eco-emission-co2",
                                 "median-eco-emission-co2",
                                 "sd-eco-emission-co2",
                                 "sum-eco-emission-co2",
                                 
                                 "mean-conv-travel-time",
                                 "median-conv-travel-time",
                                 "sd-conv-travel-time",
                                 "sum-conv-travel-time",
                                 
                                 "mean-conv-emission-co2",
                                 "median-conv-emission-co2",
                                 "sd-conv-emission-co2",
                                 "sum-conv-emission-co2",
                                 
                                 "traffic-flow-model",
                                 "traffic-density-model",
                                 "number-of-accidents"
                                ),
  variables         =          list('flow'                     = list(values = flow_values), # putting the custom sequence for the 'flow' variable here
                                    'eco-drivers-percent'      = list(min = 0, max = 100, step = 10, qfun="qunif")
  ),
  constants         =          list('seconds-to-react'            = 0.68,
                                    'eco-acceleration'            = 1.00,
                                    'conv-acceleration'           = 1.50,
                                    'eco-deceleration'            = 3.00,
                                    'conv-deceleration'           = 5.00,
                                    'eco-following-dist'          = 2.84,
                                    'conv-following-dist'         = 1.57,
                                    'speed-desired-mean'          = 27.78,
                                    'speed-desired-sd'            = 0.93
                                    )
)

#Check variables and constants
eval_variables_constants(nl_exp_ff)

#Design
nl_exp_ff@simdesign <- simdesign_ff( nl = nl_exp_ff, nseeds = 8)

#Print the nl
print(nl_exp_ff)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Run the Simulation
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Record initial date.time
start_time      <-      Sys.time()

plan(multisession, gc = TRUE)
exp_results     <-      run_nl_all(nl_exp_ff, split = 11)

#Record final date.time
end_time        <-      Sys.time()

#Calculate the time spent and show
elapsed_time    <-      end_time - start_time
print(elapsed_time)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Results
#------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Attach the results to the nl object
setsim(nl_exp_ff, "simoutput") <- exp_results

#Save the object
#saveRDS(nl_exp_ff, "nl_exp_ff_results.rds")

#Open the object saved
nl_exp_ff <- readRDS("C:/Users/thiago/Desktop/model_final/nl_exp_ff_results.rds")

#Write a file with the results
#write_simoutput(nl_exp_ff)

simoutput_raw <- data.frame(nl_exp_ff@simdesign@simoutput)