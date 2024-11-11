# Clear all
rm(list=ls())
options(scipen = 999)

# Library
library(tidyverse)
library(gridExtra)
library(dplyr)
library(tidyr)
library(nlrx)
library(ggplot2)
library(openxlsx)
library(writexl)
library(FSA)



# Open the object saved
nl_exp_ff <- readRDS("nl_exp_ff_results.rds")

# Take raw data from the object and put it into a dataframe
simoutput_raw <- data.frame(nl_exp_ff@simdesign@simoutput)




### Prepare the Data Frame

# Selecting only wished flow
# Filter the data to include only the specified flow values (200, 400, 600, 800, 1000, 1200)
simoutput_200_400_600_800_1000_1200 <- subset(simoutput_raw, 
                                                flow == 200 | 
                                                flow == 400 | 
                                                flow == 600 | 
                                                flow == 800 | 
                                                flow == 1000 | 
                                                flow == 1200)

# Replacing zeros with NA (except the eco-drivers-percent column)
# Use the mutate and across functions to replace zeros with NA in all columns except eco.drivers.percent
simoutput_200_400_600_800_1000_1200 <-  simoutput_200_400_600_800_1000_1200 %>%
                                        mutate(across(
                                          .cols = -eco.drivers.percent,
                                          .fns = ~ na_if(., 0)
                                        ))

# Selecting specific columns
# Select the columns of interest for the analysis
simoutput_200_400_600_800_1000_1200 <-  simoutput_200_400_600_800_1000_1200 %>% 
                                        select(
                                          flow,
                                          eco.drivers.percent,
                                          mean.eco.travel.time,
                                          mean.conv.travel.time,
                                          mean.eco.emission.co2,
                                          mean.conv.emission.co2
                                        )

# Use pivot_longer to combine the columns of travel time and emission CO2
simoutput_organized <- simoutput_200_400_600_800_1000_1200 %>%
                        # Pivot travel time columns into a long format
                        pivot_longer(
                          # Columns to pivot
                          cols = c(mean.eco.travel.time, mean.conv.travel.time),
                          # Name of the new column that stores the original column names
                          names_to = "driver_type", 
                          # Name of the new column that stores the values
                          values_to = "travel_time"                             
                        ) %>%
                        # Pivot emission CO2 columns into a long format
                        pivot_longer(
                          # Columns to pivot
                          cols = c(mean.eco.emission.co2, mean.conv.emission.co2),
                          # Name of the new column that stores the original column names
                          names_to = "emission_type",      
                          # Name of the new column that stores the values
                          values_to = "emission_co2"                              
                        ) %>%
                        # Filter rows to ensure matching 'driver_type' and 'emission_type'
                        #   (eco-driver with eco-emission, etc.)
                        filter(
                          (driver_type == "mean.eco.travel.time" & 
                             emission_type == "mean.eco.emission.co2") | 
                          (driver_type == "mean.conv.travel.time" & 
                             emission_type == "mean.conv.emission.co2")
                        ) %>%
                        # Mutate to rename 'driver_type' for readability
                        # Replace 'mean.eco.travel.time' with 'eco-driver' and
                        #   'mean.conv.travel.time' with 'conv-driver'
                        mutate(
                          driver_type = ifelse(
                            driver_type == "mean.eco.travel.time", "eco-driver", "conv-driver")
                        ) %>%
                        # Select only the final relevant columns
                        select(flow, 
                               eco.drivers.percent, 
                               driver_type, 
                               travel_time, 
                               emission_co2)

# Convert the specified columns to categorical and quantitative types
simoutput_organized <- simoutput_organized %>%
                        mutate(
                          # Convert 'flow' to categorical
                          flow_cat = as.factor(flow),   
                          flow_num = as.numeric(flow),
                          # Convert 'eco.drivers.percent' to categorical
                          eperc_cat = as.factor(eco.drivers.percent),
                          eperc_num = as.numeric(eco.drivers.percent), 
                          # Convert 'driver_type' to categorical
                          driver_type = as.factor(driver_type),     
                          # Convert 'travel_time' to quantitative
                          travel_time = as.numeric(travel_time),  
                          # Convert 'emission_co2' to quantitative
                          emission_co2 = as.numeric(emission_co2)               
                        )

# drop na's
simoutput_new <- simoutput_organized %>% 
                 drop_na()


### MODEL FOR TTIME
# TTIME = f(FLOW, EPERC, DTYPE)
lm_tt <- lm(travel_time ~ 
              I(flow_num^3) + I(flow_num^2) + flow_num +
              eperc_cat + 
              driver_type +
              flow_num : eperc_cat, # +
            # flow_num : driver_type +           # not significant
            # eperc_cat : driver_type,           # not significant
            data = simoutput_new)



anova(lm_tt)
summary(lm_tt)



# homoscedasticidade
library(lmtest)
bptest(lm_tt)

# independência
dwtest(lm_tt, alternative = "two.sided")

# normalidade
shapiro.test(lm_tt$resid)




# Realiza procedimento EMMeans
library(emmeans)

em_tt_1 <- emmeans(lm_tt, ~ driver_type | flow_num,
                   weights = "proportional", cov.reduce = mean,
                   at = list(flow_num = seq(200, 1200, by = 200))); # em_tt_1

cat("===========================================================================")
cat("Estimated Mean TTIME x DTYPE \n(all combinations of FLOW and levels of EPERC)")

em_tt_2 <- emmeans(lm_tt, ~ driver_type | flow_num * eperc_cat,  
                   weights = "proportional", cov.reduce = mean,
                   at = list(flow_num = seq(200, 1200, by = 200))); # em_tt_2

cat("===========================================================================")
cat("Estimated Mean TTIME x FLOW and EPERC \n(results are averaged over levels of DTYPE)")

em_tt_3 <- emmeans(lm_tt, ~ eperc_cat | flow_num,  
                   weights = "proportional", cov.reduce = mean,
                   at = list(flow_num = seq(200, 1200, by = 200))); # em_tt_3

cat("===========================================================================")
cat("Estimated Mean TTIME x EPERC \n(all combinations of FLOW and levels of DTYPE)")

em_tt_4 <- emmeans(lm_tt, ~ eperc_cat | flow_num * driver_type,  
                   weights = "proportional", cov.reduce = mean,
                   at = list(flow_num = seq(200, 1200, by = 200))); # em_tt_4



## mean TTIME by DTYPE and FLOW (averaged over EPERC)
pairs_tt_1 <- pairs(em_tt_1)

## mean TTIME by EPERC and FLOW (averaged over DTYPE)
pairs_tt_3 <- pairs(em_tt_3, reverse = TRUE)







# Function to convert ticks to MM:SS and use in plots
ticks_to_mmss <- function(ticks) {
  total_seconds <- ticks * 0.1 # Every tick is 0.1 seconds
  minutes <- floor(total_seconds / 60)
  seconds <- round(total_seconds %% 60)
  paste0(minutes, ":", sprintf("%02d", seconds))
}


df_em_tt_3 <- as.data.frame(em_tt_3)
library(cowplot)

plot_em_tt_3 <-   ggplot(df_em_tt_3, aes(x = emmean, y = eperc_cat)) +
  geom_tile(aes(width = upper.CL - lower.CL, height = 1), fill = "grey80", color = "grey60") +
  geom_point(size = 1) +
  facet_grid(rows = vars(flow_num)) +
  labs(x = "Estimated Marginal Means (Travel time)",
       y = "Eco-drivers percent"
  ) +
  theme_minimal() +
  scale_y_discrete(
    breaks = c("0", "20", "40", "60", "80", "100"),
    labels = function(x) {
      x_numeric <- as.numeric(x)
      ifelse(x_numeric %% 20 == 0, paste0(x, "%"), "")
    }) +
  scale_x_continuous(
    limits = c(7882, 8406),
    labels = ticks_to_mmss
  ) +
  theme(strip.text.y = element_text(angle = 90),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, margin = margin(t = 0.25)),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
        axis.line.y.left = element_line(color = "black", linewidth = 0.2),
        axis.ticks.length = unit(0.1, "cm"),
        axis.ticks = element_line(color = "black", linewidth = 0.1),
        axis.ticks.x = element_line(color = "black", linewidth = 0.1),
        axis.ticks.y = element_line(color = "black", linewidth = 0.1),
        panel.grid.major = element_line(size = 0.1),
        panel.grid.minor = element_line(size = 0.1) 
  )

plot_em_tt_3



final_plot <- plot_grid(plot_em_tt_3, 
                        ggdraw() + draw_label("Flow (Cars/h)", angle = 90, vjust = 0, hjust = 0.29, size = 10), 
                        ncol = 2, rel_widths = c(0.9, 0.02))  # Ajuste de largura

print(final_plot)

#ggsave("plot_em_tt_3.pdf", plot = final_plot, width = 6, height = 5, units = "in")








### MODEL FOR CO2
# CO2 = f(FLOW, EPERC, DTYPE)
lm_co2 <- lm(emission_co2 ~ 
               I(flow_num^2) + flow_num +
               eperc_cat + 
               driver_type +
               flow_num : eperc_cat +  
               flow_num : driver_type, # +           # not significant
             #eperc_cat : driver_type, # not significant
             data = simoutput_new)

anova(lm_co2)
summary(lm_co2)




library(lmtest)
bptest(lm_co2)

# independência
dwtest(lm_co2, alternative = "two.sided")

# normalidade
shapiro.test(lm_co2$resid)


cat("===========================================================================")
cat("Estimated Mean CO2 x FLOW and DTYPE \n(results are averaged over levels of EPERC)")

em_co2_1 <- emmeans(lm_co2, ~ driver_type | flow_num,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_1

cat("===========================================================================")
cat("Estimated Mean CO2 x DTYPE \n(all combinations of FLOW and levels of EPERC)")

em_co2_2 <- emmeans(lm_co2, ~ driver_type | flow_num * eperc_cat,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_2



cat("===========================================================================")
cat("Estimated Mean CO2 x FLOW and EPERC \n(results are averaged over levels of DTYPE)")

em_co2_3 <- emmeans(lm_co2, ~ eperc_cat | flow_num,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_3
cat("===========================================================================")
cat("Estimated Mean CO2 x EPERC \n(all combinations of FLOW and levels of DTYPE)")

em_co2_4 <- emmeans(lm_co2, ~ eperc_cat | flow_num * driver_type,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_4






pairs_co2_1 <- pairs(em_co2_1)


pwpm(em_co2_1, means = TRUE, flip = TRUE,      # args for pwpm()
     reverse = TRUE,                           # args for pairs()
     side = "=", adjust = "tukey")             # args for test()


##############################################################################################################################################################



# Create a data frame for plotting
df <- data.frame(travel_time = simoutput_new$travel_time, 
                 emission_co2 = simoutput_new$emission_co2, 
                 tt_fitted_values = lm_tt$fitted.values,
                 co2_fitted_values = lm_co2$fitted.values,
                 flow = simoutput_new$flow_num,
                 driver_type = simoutput_new$driver_type)









### TTIME

# 1) Fitted vs Observed TTIME
plot1 <- ggplot(df,
                aes(x = travel_time,
                    y = tt_fitted_values, 
                    color = as.factor(flow),
                    shape = as.factor(flow)
                    )
                ) +
         geom_abline(intercept = 0,
                     slope = 1,
                     linetype = "solid",
                     linewidth = 0.7,
                     color = "black"
                     ) +
         geom_point(size = 1.3,
                    stroke = 0.3,
                    alpha = 0.9
                    ) +
         scale_color_viridis_d(option = "viridis", direction = -1) +
         scale_shape_manual(values = c(9, 6, 2, 5, 1, 0)) +
         labs(x = "Simulated travel time\n(Minutes : seconds)",
              y = "Fitted Values\n(Minutes : seconds)",
              color = "Flow\n(Cars/h)",
              shape = "Flow\n(Cars/h)",
              tag = "(A)"
              ) +
         theme_minimal() +
         scale_y_continuous(
          limits = c(7882, 8406),
          labels = ticks_to_mmss
         ) +
         scale_x_continuous(
          limits = c(7882, 8406),
          labels = ticks_to_mmss
         ) +
         theme(axis.title.x = element_text(margin = margin(t = 10)),
          axis.line.x.bottom = element_line(color = "black", size = 0.2),
          axis.line.y.left = element_line(color = "black", size = 0.2),
          axis.ticks.length = unit(0.1, "cm"),
          axis.ticks = element_line(color = "black", linewidth = 0.1),
          axis.ticks.x = element_line(color = "black", linewidth = 0.1),
          axis.ticks.y = element_line(color = "black", linewidth = 0.1),
          panel.grid.major = element_line(size = 0.1),
          panel.grid.minor = element_line(size = 0.1),
          legend.position = "bottom"
         ) +
         guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1, stroke = 1.1)))
plot1
#ggsave("03-travel_time_fitted_obeserved.pdf", plot = plot1, width = 6, height = 5, units = "in")



residual_plot_co2_2_1 <- ggplot(df, 
                                 aes(x = emission_co2, 
                                     y = co2_fitted_values,
                                     color = as.factor(flow),
                                     shape = as.factor(flow)
                                     )
                                 ) +
  
                         geom_abline(intercept = 0,
                                     slope = 1,
                                     linetype = "solid",
                                     linewidth = 0.7,
                                     color = "black"
                         ) +
                         geom_point(size = 1.3,
                                    stroke = 0.3,
                                    alpha = 0.9
                         ) +
                         scale_color_viridis_d(option = "viridis", direction = -1) +
                         scale_shape_manual(values = c(9, 6, 2, 5, 1, 0)) +
                         labs(#title = expression("Emissions of CO"[2] * ": Fitted x Observed"), 
                              x = expression("Simulated CO"[2] * " emissions (kg)"),
                              y = "\nFitted values (kg)",
                              color = "Flow\n(Cars/h)",
                              shape = "Flow\n(Cars/h)",
                              tag = "(B)"
                          ) +
                          theme_minimal() +
                          scale_y_continuous(
                            limits = c(3.0, 3.24)
                             ) +
                          scale_x_continuous(
                            limits = c(3.0, 3.24)
                          ) +
                          theme(axis.title.x = element_text(margin = margin(t = 10)),
                              axis.line.x.bottom = element_line(color = "black", size = 0.2),
                              axis.line.y.left = element_line(color = "black", size = 0.2),
                              axis.ticks.length = unit(0.1, "cm"),
                              axis.ticks = element_line(color = "black", linewidth = 0.1),
                              axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                              axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                              panel.grid.major = element_line(size = 0.1),
                              panel.grid.minor = element_line(size = 0.1),
                              legend.position = "bottom"
                          ) +
                          guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1, stroke = 1.1)))
residual_plot_co2_2_1
#ggsave("residual_plot_co2_2_1.pdf", plot = residual_plot_co2_2_1, width = 6, height = 5, units = "in")





library(ggpubr)

plot1 <- plot1 + theme(plot.margin = margin(10, 1, 10, 10))
residual_plot_co2_2_1 <- residual_plot_co2_2_1 + theme(plot.margin = margin(10, 10, 10, 1))


combined_plot <- ggarrange(
                           plot1, residual_plot_co2_2_1,
                           ncol = 2,
                           common.legend = TRUE,
                           legend = "bottom",
                           align = "hv"
                          )

# Exibir o plot combinado
print(combined_plot)

ggsave("03-simulated_vs_fitted_tt_co2.pdf", plot = combined_plot, width = 8, height = 4, units = "in")









# 2) Comparison of estimated mean TTIME between different levels of % Eco-driver considering driver type, under different flow conditions 
plot2 <-  emmip(lm_tt, eperc_cat ~ driver_type | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                          eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(title = "Flow (Cars/h)",
               x = "Driver type", 
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Eco-drivers") +
          facet_wrap(~flow_num, nrow = 1) + 
          theme(legend.position="bottom", legend.box = "vertical") +
          scale_color_manual(values = gray.colors(length(unique(simoutput_new$eco.drivers.percent)), start = 0.8, end = 0.0), 
                             labels = function(x) paste0(x, "%")) +
          scale_y_continuous(labels = ticks_to_mmss) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(hjust = 0.5, size = 10),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1)
                )
plot2
#ggsave("03-travel_time_eperc_dtype.pdf", plot = plot2, width = 6, height = 5, units = "in")




# pairwise p-value matrix
pwpm(em_tt_1, means = TRUE, flip = TRUE,         # args for pwpm()
     reverse = TRUE,                             # args for pairs()
     side = "=", adjust = "tukey")               # args for test()

pwpm(em_tt_3, means = TRUE, flip = TRUE,         # args for pwpm()
     reverse = TRUE,                             # args for pairs()
     side = "=", adjust = "tukey")               # args for test()



# pairwise p-value plot
my.aes <- list(segment = list(linewidth = 0.1, color = "black"))

plot3 <-  pwpp(em_tt_1, type = "response", aes = my.aes) +
          facet_wrap(~flow_num, nrow = 2) +
          theme_minimal() +
          labs(title = "Flow (Cars/h)",
               x = "P-value", 
               y = "Driver type",
               color = "Eco-drivers") +
          theme(axis.text.x = element_text(angle = 0, hjust = 0.75),
                plot.title = element_text(hjust = 0.5, size = 10),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1)
          ) +
          scale_color_grey(start = 0.5, end = 0.2)
plot3
#ggsave("03-p_value_plot_pairwise.pdf", plot = plot3, width = 6, height = 5, units = "in")


plot4 <-  pwpp(em_tt_3, type = "response", aes = my.aes) +
          facet_wrap(~flow_num, nrow = 2) +
          theme_minimal() +
          labs(title = "Flow (Cars/h)",
               x = "Tukey adjusted P-value", 
               y = "Eco-drivers percent",
               color = "Eco-drivers percent") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25),
                plot.title = element_text(hjust = 0.5, size = 10),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1)
          ) +
          scale_color_grey(start = 0.6, end = 0) +
          scale_y_discrete(labels = function(x) paste0(x, "%"))
plot4
#ggsave("03-tukey_adj_p_value_plot_pairwise_eperc.pdf", plot = plot4, width = 6, height = 5, units = "in")




# plot estimated means and CIs
plot(em_tt_1, comparisons = FALSE)

df_em_tt_1 <- as.data.frame(em_tt_1)
library(cowplot)


plot_em_tt_1 <-   ggplot(df_em_tt_1, aes(x = emmean, y = driver_type)) +
                  geom_tile(aes(width = upper.CL - lower.CL, height = 0.5), fill = "grey80", color = "grey60") +
                  geom_point(size = 1) +
                  facet_grid(rows = vars(flow_num)) +
                  labs(x = "Estimated Marginal Means (Travel time)",
                       y = "Driver type"
                  ) +
                  theme_minimal() +
                  scale_x_continuous(
                    limits = c(7882, 8406),
                    labels = ticks_to_mmss
                  ) +
                  theme(strip.text.y = element_text(angle = 90),
                        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, margin = margin(t = 0.25)),
                        axis.text.y = element_text(size = 8),
                        axis.title = element_text(size = 10),
                        axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                        axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                        axis.ticks.length = unit(0.1, "cm"),
                        axis.ticks = element_line(color = "black", linewidth = 0.1),
                        axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                        axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                        panel.grid.major = element_line(size = 0.1),
                        panel.grid.minor = element_line(size = 0.1) 
                  )
plot_em_tt_1

plot5 <- plot_grid(plot_em_tt_1, 
                   ggdraw() + draw_label("Flow (Cars/h)", angle = 90, vjust = 0, hjust = 0.29, size = 10), 
                   ncol = 2, rel_widths = c(0.9, 0.02))  # Ajuste de largura

print(plot5)
#ggsave("03-plot_em_tt_1.pdf", plot = plot5, width = 6, height = 5, units = "in")









plot6 <-  emmip(lm_tt,  ~ eperc_cat | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2, alpha = 0.5),
              dotarg = list(size = 1, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(0, 100, by = 10))))  +
  
          facet_wrap(~flow_num, nrow = 1) +
          geom_point(aes(x = eperc_cat, 
                         y = travel_time, 
                         color = driver_type,
                         shape = driver_type
                         ), 
                     data = simoutput_new,
                     size = 1,
                     alpha = 0.3,
                     stroke = 0.8
                     ) +
  
          labs(x = "Eco-drivers percent",
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Driver type",
               shape = "Driver type",
               tag = "(C)"
          ) +
          theme_minimal() +
          scale_y_continuous(labels = ticks_to_mmss) +
          scale_shape_manual(values = c("eco-driver" = 5, "conv-driver" = 0)) +
          scale_x_discrete(breaks = c("10", "30", "50", "70", "90")) +
          theme(strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 10),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical",
                plot.margin = margin(10, 5, 10, 10)
          ) +
          annotate("text", x = 2.8, y = Inf, label = "(Cars/h)", hjust = 0, vjust = 1, size = 2.5) +
          guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1, stroke = 1.1)))
plot6


plot13 <-   emmip(lm_co2,  ~ eperc_cat | flow_num, CIs = TRUE,
                weights = "proportional", cov.reduce = mean,
                CIarg = list(lwd = 2, alpha = 0.5),
                dotarg = list(size = 1, color = "black"),
                at = list(flow_num = seq(200, 1200, by = 200),
                          eperc_cat = as.factor(seq(0, 100, by = 10))))  +
    
            facet_wrap(~flow_num, nrow = 1) +
            geom_point(aes(x = eperc_cat, 
                           y = emission_co2, 
                           color = driver_type,
                           shape = driver_type
                           ), 
                       data = simoutput_new,
                       size = 1,
                       alpha = 0.3,
                       stroke = 0.8
                       ) +
    
            labs(x = "Eco-drivers percent",
                 y = expression("Estimated CO"[2]~"emissions (kg)"),
                 color = "Driver type",
                 shape = "Driver type",
                 tag = "(D)"
            ) +
            theme_minimal() +
            scale_shape_manual(values = c("eco-driver" = 5, "conv-driver" = 0)) +
            scale_x_discrete(breaks = c("10", "30", "50", "70", "90")) +
            theme(strip.text.y = element_text(angle = 90),
                  axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10, margin = margin(t = 0.25)),
                  axis.text.y = element_text(size = 10),
                  axis.title = element_text(size = 10),
                  axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                  axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                  axis.ticks.length = unit(0.1, "cm"),
                  axis.ticks = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  legend.position = "bottom",
                  legend.box = "vertical",
                  plot.margin = margin(10, 5, 10, 10)
            ) +
            annotate("text", x = 2.8, y = Inf, label = "(Cars/h)", hjust = 0, vjust = 1, size = 2.5) +
            guides(shape = guide_legend(override.aes = list(size = 2, alpha = 1, stroke = 1.1)))
plot13




combined_plot_2 <-  ggarrange(
                      plot6, plot13,
                      ncol = 2,
                      common.legend = TRUE,
                      legend = "bottom",
                      align = "hv"
                    )

# Exibir o plot combinado
print(combined_plot_2)


#ggsave("03-estimated_tt_co2_eperc.pdf", plot = combined_plot_2, width = 10, height = 5, units = "in")


# expected variation in mean travel time by Driver Type and Flow
plot6.1 <-  emmip(lm_tt, driver_type ~ eperc_cat | flow_num, CIs = TRUE,
                weights = "proportional", cov.reduce = mean,
                CIarg = list(lwd = 2, alpha = 0.5),
                dotarg = list(size = 0.5, color = "black"),
                at = list(flow_num = seq(200, 1200, by = 200),
                          eperc_cat = as.factor(seq(10, 90, by = 10))))  +
            labs(x = "Eco-drivers percent",
                 y = "Estimated travel time\n(Minutes : seconds)",
                 color = "Driver type",
                 shape = "Driver type",
                 tag = "(C)"
                   ) +
            theme(legend.position="bottom", legend.box = "vertical") +
            facet_wrap(~flow_num, nrow = 1) +
            theme_minimal() +
            scale_y_continuous(labels = ticks_to_mmss) +
            scale_shape_manual(values = c("eco-driver" = 5, "conv-driver" = 0)) +
            scale_x_discrete(breaks = c("10", "30", "50", "70", "90"),
                             #labels = function(x) paste0(x, "%")
                             ) +
            theme(axis.title.x = element_text(margin = margin(t = 10)),
                  strip.text.y = element_text(angle = 90),
                  axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 9, margin = margin(t = 0.25)),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 10),
                  axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                  axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                  axis.ticks.length = unit(0.1, "cm"),
                  axis.ticks = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  legend.position = "bottom",
                  legend.box = "vertical",
                  plot.margin = margin(3, 3, 3, 3)
            ) +
            annotate("text", x = 2.8, y = Inf, label = "(Cars/h)", hjust = -0.25, vjust = 1, size = 2.5) +
            guides(shape = guide_legend(override.aes = list(size = 1.2, alpha = 1)))
plot6.1

# expected variation in mean co2 emission by Driver Type and Flow
plot13.1 <- emmip(lm_co2, driver_type ~ eperc_cat | flow_num, CIs = TRUE,
                weights = "proportional", cov.reduce = mean,
                CIarg = list(lwd = 2.5, alpha = 0.5),
                dotarg = list(size = 0.5, color = "black"),
                at = list(flow_num = seq(200, 1200, by = 200),
                          eperc_cat = as.factor(seq(10, 90, by = 10))))  +
            labs(x = "Eco-drivers percent",
                 y = expression("Estimated CO"[2]~"emissions (kg)"),
                 color = "Driver type",
                 shape = "Driver type",
                 tag = "(D)"
                 ) +
            theme(legend.position="bottom", legend.box = "vertical") +
            facet_wrap(~flow_num, nrow = 1) +
            theme_minimal() +
            scale_shape_manual(values = c("eco-driver" = 5, "conv-driver" = 0)) +
            scale_x_discrete(breaks = c("10", "30", "50", "70", "90"),
                             #labels = function(x) paste0(x, "%")
                             ) +
            theme(axis.title.x = element_text(margin = margin(t = 10)),
                  strip.text.y = element_text(angle = 90),
                  axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 9, margin = margin(t = 0.25)),
                  axis.text.y = element_text(size = 9),
                  axis.title = element_text(size = 10),
                  axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                  axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                  axis.ticks.length = unit(0.1, "cm"),
                  axis.ticks = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                  axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  legend.position = "bottom",
                  legend.box = "vertical",
                  plot.margin = margin(3, 3, 3, 3)
            ) +
            annotate("text", x = 2.8, y = Inf, label = "(Cars/h)", hjust = -0.25, vjust = 1, size = 2.5) +
            guides(shape = guide_legend(override.aes = list(size = 1.2, alpha = 1)))
plot13.1

combined_plot_3 <-  ggarrange(
                      plot6.1, plot13.1,
                      ncol = 2,
                      common.legend = TRUE,
                      legend = "bottom",
                      align = "hv"
                    )

# Exibir o plot combinado
print(combined_plot_3)
#ggsave("03-estimated_tt_co2_eperc_by_flow.pdf", plot = combined_plot_3, width = 10, height = 5, units = "in")


combined_plot_4 <-  ggarrange(
                      plot6.1, plot13.1,
                      ncol = 1,
                      common.legend = TRUE,
                      legend = "bottom",
                      align = "hv"
                    )

# Exibir o plot combinado
print(combined_plot_4)
#ggsave("03-estimated_tt_co2_eperc_by_flow_2.pdf", plot = combined_plot_4, width = 8, height = 6, units = "in")

# averaging over driver_type
plot7 <-  emmip(lm_tt,  ~ eperc_cat | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(title = "Flow (Cars/h)",
               x = "Eco-drivers percent",
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Driver type") +
          facet_wrap(~flow_num, nrow = 1) +
          theme_minimal() +
          scale_color_grey(start = 0.5, end = 0.2) +
          scale_y_continuous(
            #limits = c(7882, 8406),
            labels = ticks_to_mmss
          ) +
          scale_x_discrete(labels = function(x) paste0(x, "%")) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot7
#ggsave("03-travel_time_avg_dtype.pdf", plot = plot7, width = 6, height = 5, units = "in")

# 4) Comparison of estimated mean TTIME between driver types considering the level of % Eco-driver, under flows 600 and 800 
plot8 <-  emmip(lm_tt, driver_type ~ eperc_cat | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(600, 800, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(title = "Flow (Cars/h)",
               x = "Eco-drivers percent",
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Driver type: ") +
          theme(legend.position="bottom", legend.box = "vertical") +
          facet_wrap(~flow_num, nrow = 1) +
          theme_minimal() +
          scale_color_grey(start = 0.5, end = 0.2) +
          scale_y_continuous(
            #limits = c(7882, 8406),
            labels = ticks_to_mmss
          ) + 
          scale_x_discrete(labels = function(x) paste0(x, "%")) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot8
#ggsave("03-travel_time_avg_600_800.pdf", plot = plot8, width = 6, height = 5, units = "in")



# averaging over driver_type
plot9 <-  emmip(lm_tt,  ~ eperc_cat | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(600, 800, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(title = "Flow (Cars/h)",
               x = "Eco-drivers percent",
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Driver type: ") +
          theme(legend.position="bottom", legend.box = "vertical") +
          facet_wrap(~flow_num, nrow = 1) +
          theme_minimal() +
          scale_color_grey(start = 0.5, end = 0.2) +
          scale_y_continuous(
            #limits = c(7882, 8406),
            labels = ticks_to_mmss
          ) + 
          scale_x_discrete(labels = function(x) paste0(x, "%")) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot9
#ggsave("03-travel_time_avg_600_800_driver.pdf", plot = plot9, width = 6, height = 5, units = "in")



# 5) Comparison of estimated mean Travel Time between different levels of % Eco-drivers considering varying flow conditions, for each driver type
plot10 <- emmip(lm_tt, eperc_cat ~ flow_num | driver_type, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(x = "Flow (Cars/h)",
               y = "Estimated mean travel time\n(Minutes : seconds)",
               color = "Eco-drivers") +
          theme(legend.position="bottom", legend.box = "horizontal") +
          scale_x_continuous(breaks=seq(200, 1200, by = 200)) +
          theme_minimal() +
          scale_color_manual(values = gray.colors(length(unique(simoutput_new$eco.drivers.percent)), start = 0.6, end = 0),
                             labels = function(x) paste0(x, "%")
          ) +
          scale_y_continuous(
            #limits = c(7882, 8406),
            labels = ticks_to_mmss
          ) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot10
#ggsave("03-travel_time_per_driver_flow.pdf", plot = plot10, width = 6, height = 5, units = "in")



# 6) Differences in Estimated Mean TTIME between Driver types
plot11 <- plot(pairs_tt_1) +
          geom_point(size = 0.5) +
          geom_vline(xintercept = 0, linewidth = 0.3, linetype = 5) +
          facet_wrap(~flow_num, ncol = 1) +
          #labs(title = "Differences in Estimated Mean Travel Time between Driver types")
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot11
#ggsave("03-travel_time_em_between_drivers.pdf", plot = plot11, width = 6, height = 5, units = "in")


# 7) Differences in Estimated mean TTIME for pairs % Eco-drivers levels
new_names <- gsub("eperc_cat(\\d+) - eperc_cat(\\d+)", "\\1% - \\2%", pairs_tt_3@levels[["contrast"]])
pairs_tt_3@levels[["contrast"]] <- new_names
new_grid_names <- gsub("eperc_cat(\\d+) - eperc_cat(\\d+)", "\\1% - \\2%", pairs_tt_3@grid[["contrast"]])
pairs_tt_3@grid[["contrast"]] <- new_grid_names

plot12 <- plot(pairs_tt_3, cex = 0.1) + 
          geom_point(size = 0.1) +
          geom_vline(xintercept = 0, linewidth = 0.3, linetype = 5) +
          facet_wrap(~flow_num, nrow = 1) +
          labs(title = "Flow (Cars/h)",
               x = "Estimate",
               y = "Contrast") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 6),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot12
#ggsave("03-travel_time_estimate_contrast.pdf", plot = plot12, width = 7.2, height = 6, units = "in")





### CO2


# CO2 = f(FLOW, EPERC, DTYPE)
lm_co2 <- lm(emission_co2 ~ 
               I(flow_num^2) + flow_num +
               eperc_cat + 
               driver_type +
               flow_num : eperc_cat +  
               flow_num : driver_type, # +           # not significant
             #eperc_cat : driver_type, # not significant
             data = simoutput_new)

anova(lm_co2)
summary(lm_co2)


# homoscedasticidade
library(lmtest)
bptest(lm_co2)

# independência
dwtest(lm_co2, alternative = "two.sided")

# normalidade
shapiro.test(lm_co2$resid)


# Realiza procedimento EMMeans
library(emmeans)

# THIS GRAPH IS AT THE LAST SECTION
# "Graphs for the Paper"
# Estimated marginal means
# # 1) estimated mean TTIME x FLOW and DTYPE
# emmip(lm_co2, eperc_cat ~ driver_type | flow_num, 
#       weights = "proportional", cov.reduce = mean,
#       at = list(flow_num = seq(200, 1200, by = 200)))

cat("===========================================================================")
cat("Estimated Mean CO2 x FLOW and DTYPE \n(results are averaged over levels of EPERC)")

em_co2_1 <- emmeans(lm_co2, ~ driver_type | flow_num,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_1

cat("===========================================================================")
cat("Estimated Mean CO2 x DTYPE \n(all combinations of FLOW and levels of EPERC)")

em_co2_2 <- emmeans(lm_co2, ~ driver_type | flow_num * eperc_cat,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_2

cat("===========================================================================")
cat("Estimated Mean CO2 x FLOW and EPERC \n(results are averaged over levels of DTYPE)")

em_co2_3 <- emmeans(lm_co2, ~ eperc_cat | flow_num,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_3
cat("===========================================================================")
cat("Estimated Mean CO2 x EPERC \n(all combinations of FLOW and levels of DTYPE)")

em_co2_4 <- emmeans(lm_co2, ~ eperc_cat | flow_num * driver_type,  
                    weights = "proportional", cov.reduce = mean,
                    at = list(flow_num = seq(200, 1200, by = 200))); em_co2_4



pairs_co2_1 <- pairs(em_co2_1)

pairs_co2_3 <- pairs(em_co2_3, reverse = TRUE)

# pairwise p-value matrix
pwpm(em_co2_1, means = TRUE, flip = TRUE,      # args for pwpm()
     reverse = TRUE,                           # args for pairs()
     side = "=", adjust = "tukey")             # args for test()


# pairwise p-value plot
pwpp(em_co2_1)




# 1) Fitted vs Observed CO2
ggplot(df, aes(x = emission_co2, y = co2_fitted_values, color = as.factor(flow))) +
  geom_abline(intercept = 0, slope = 1, linetype = "solid", size = 0.8) +
  geom_point(alpha = 0.25, size = 2.5) +
  labs(title = "Fitted Values vs Simulated CO2 emission", 
       x = "Simulated CO2 emission", 
       y = "Fitted Values", 
       color = "Flow") + 
  theme(legend.position="bottom", legend.box = "vertical")


# 2) Comparison of estimated mean CO2 emission between different levels of % Eco-driver considering driver type, under different flow conditions 
emmip(lm_co2, eperc_cat ~ driver_type | flow_num, CIs = TRUE,
      weights = "proportional", cov.reduce = mean,
      CIarg = list(lwd = 2.5, alpha = 0.5),
      dotarg = list(shape = "circle", size = 0.5, color = "black"),
      at = list(flow_num = seq(200, 1200, by = 200),
                eperc_cat = as.factor(seq(10, 90, by = 10))))  +
  labs(x = "Driver type", 
       y = "Estimated CO2 emission",
       color = "% Eco-drivers") +
  facet_wrap(~flow_num, nrow = 1) + 
  theme(legend.position="bottom", legend.box = "vertical")

# pairwise p-value matrix
pwpm(em_co2_1, means = TRUE, flip = TRUE,         # args for pwpm()
     reverse = TRUE,                             # args for pairs()
     side = "=", adjust = "tukey")               # args for test()

pwpm(em_co2_3, means = TRUE, flip = TRUE,         # args for pwpm()
     reverse = TRUE,                             # args for pairs()
     side = "=", adjust = "tukey")               # args for test()

# pairwise p-value plot
my.aes <- list(segment = list(linewidth = 0.1, color = "black"))

pwpp(em_co2_1, type = "response", aes = my.aes) +
  facet_wrap(~flow_num, nrow = 2)

pwpp(em_co2_3, type = "response", aes = my.aes) +
  facet_wrap(~flow_num, nrow = 2)


# 3) Comparison of estimated mean CO2 emission between driver types considering the level of % Eco-driver, under different flow conditions 
plot13 <- emmip(lm_co2, driver_type ~ eperc_cat | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(title = "Flow (Cars/h)",
               x = "Eco-drivers percent",
               y = expression("Estimated CO"[2]~"emissions (kg)"),
               color = "Driver type") +
          theme_minimal() +
          facet_wrap(~flow_num, nrow = 1) +
          scale_color_grey(start = 0.5, end = 0.2) +
          scale_x_discrete(labels = function(x) paste0(x, "%")) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot13
#ggsave("03-co2_emissions_eperc_flow_dtype.pdf", plot = plot13, width = 6, height = 5, units = "in")


# 5) Comparison of estimated mean CO2 emission between different levels of % Eco-drivers considering varying flow conditions, for each driver type
plot14 <- emmip(lm_co2, eperc_cat ~ flow_num | driver_type, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(x = "Flow (Cars/h)",
               y = expression("Estimated CO"[2]~"emissions (kg)"),
               color = "Eco-drivers percent") +
          theme(legend.position="bottom", legend.box = "vertical") +
          scale_x_continuous(breaks=seq(200, 1200, by = 200)) +
          theme_minimal() +
          scale_color_manual(values = gray.colors(length(unique(simoutput_new$eco.drivers.percent)), start = 0.6, end = 0),
                             labels = function(x) paste0(x, "%")
          ) +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 8),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot14
#ggsave("03-co2_per_driver_flow.pdf", plot = plot14, width = 6, height = 5, units = "in")


# 2) Comparison of estimated mean CO2 emission between different levels of % Eco-driver considering driver type, under different flow conditions 
plot15 <- emmip(lm_co2, eperc_cat ~ driver_type | flow_num, CIs = TRUE,
              weights = "proportional", cov.reduce = mean,
              CIarg = list(lwd = 2.5, alpha = 0.5),
              dotarg = list(shape = "circle", size = 0.5, color = "black"),
              at = list(flow_num = seq(200, 1200, by = 200),
                        eperc_cat = as.factor(seq(10, 90, by = 10))))  +
          labs(x = "Driver type", 
               y = expression("Estimated CO"[2]~"emissions (kg)"),
               color = "Eco-drivers") +
          facet_wrap(~flow_num, nrow = 1) +
          scale_color_manual(values = gray.colors(length(unique(simoutput_new$eco.drivers.percent)), start = 0.8, end = 0.0), 
                                                             labels = function(x) paste0(x, "%")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                plot.title = element_text(hjust = 0.5, size = 10),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1)
          )
plot15
#ggsave("SI-co2_eperc_dtype.pdf", plot = plot15, width = 6, height = 5, units = "in")

# 7) Differences in Estimated mean CO2 emission for pairs % Eco-drivers levels

co2_new_names <- gsub("eperc_cat(\\d+) - eperc_cat(\\d+)", "\\1% - \\2%", pairs_co2_3@levels[["contrast"]])
pairs_co2_3@grid[["contrast"]] <- co2_new_names


plot16 <- plot(pairs_co2_3, cex = 0.1) + 
          geom_point(size = 0.1) +
          geom_vline(xintercept = 0, linewidth = 0.3, linetype = 5) +
          facet_wrap(~flow_num, nrow = 1) +
          labs(title = "Flow (Cars/h)",
               x = "Estimate",
               y = "Contrast") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, size = 10),
                strip.text.y = element_text(angle = 90),
                axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 6, margin = margin(t = 0.25)),
                axis.text.y = element_text(size = 6),
                axis.title = element_text(size = 10),
                axis.line.x.bottom = element_line(color = "black", linewidth = 0.2),
                axis.line.y.left = element_line(color = "black", linewidth = 0.2),
                axis.ticks.length = unit(0.1, "cm"),
                axis.ticks = element_line(color = "black", linewidth = 0.1),
                axis.ticks.x = element_line(color = "black", linewidth = 0.1),
                axis.ticks.y = element_line(color = "black", linewidth = 0.1),
                panel.grid.major = element_line(size = 0.1),
                panel.grid.minor = element_line(size = 0.1),
                legend.position = "bottom",
                legend.box = "vertical"
          )
plot16
#ggsave("SI-co2_estimate_contrast.pdf", plot = plot16, width = 7.2, height = 6, units = "in")



## TABLES

df <- simoutput_organized %>%
  group_by(flow, eco.drivers.percent) %>%
  summarise(
    travel_time = mean(travel_time, na.rm = TRUE),
    emission_co2 = mean(emission_co2, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculating percentage changes and creating the new dataframe
df_changes <- df %>%
  arrange(flow, eco.drivers.percent) %>%
  group_by(flow) %>%
  mutate(
    # Calculates the ranges of eco.drivers.percent between consecutive values
    eco_drivers_range = paste0(lag(eco.drivers.percent, default = first(eco.drivers.percent)), 
                               "% - ", eco.drivers.percent, "%"),
    # Calculates the percentage change for travel_time and emission_co2
    travel_time_change = ((travel_time - lag(travel_time)) / lag(travel_time)),
    emission_co2_change = ((emission_co2 - lag(emission_co2)) / lag(emission_co2))
  ) %>%
  # Remove the first rows where the variance cannot be calculated (NA)
  filter(!is.na(travel_time_change) & !is.na(emission_co2_change)) %>%
  # Select and rename the columns to the desired format
  select(
    flow,
    eco_drivers_range,
    travel_time_change = travel_time_change,
    emission_co2_change = emission_co2_change
  )

df_reorganized_tt <- df_changes %>%
  pivot_wider(
    names_from = flow,                          # Coloca `flow` como cabeçalhos de coluna
    values_from = travel_time_change,           # Valores das colunas a preencher
    id_cols = eco_drivers_range,
    names_glue = "{flow}",                      # Define os nomes das colunas como valores de `flow`
    values_fill = list(travel_time_change = NA) # Preenche valores ausentes com NA
  )

df_reorganized_co2 <- df_changes %>%
  pivot_wider(
    names_from = flow,                          # Coloca `flow` como cabeçalhos de coluna
    values_from = emission_co2_change,           # Valores das colunas a preencher
    id_cols = eco_drivers_range,
    names_glue = "{flow}",                      # Define os nomes das colunas como valores de `flow`
    values_fill = list(travel_time_change = NA) # Preenche valores ausentes com NA
  )

df_changes_from_zero <- df %>%
  arrange(flow, eco.drivers.percent) %>%
  group_by(flow) %>%
  mutate(
    # Fixa a referência para o cálculo como sendo o valor de eco.drivers.percent = 0
    reference_travel_time = first(travel_time[eco.drivers.percent == 0]),
    reference_emission_co2 = first(emission_co2[eco.drivers.percent == 0]),
    # Cria as faixas de eco_drivers_range em relação a 0%
    eco_drivers_range = paste0("0% - ", eco.drivers.percent, "%"),
    # Calcula a variação percentual em relação ao valor de 0%
    travel_time_change = ((travel_time - reference_travel_time) / reference_travel_time),
    emission_co2_change = ((emission_co2 - reference_emission_co2) / reference_emission_co2)
  ) %>%
  # Remove a linha onde eco.drivers.percent é 0%, pois não há variação em relação a si mesmo
  filter(eco.drivers.percent != 0) %>%
  # Seleciona e renomeia as colunas para o formato desejado
  select(
    flow,
    eco_drivers_range,
    travel_time_change,
    emission_co2_change
  )

df_reorganized_tt_zero <- df_changes_from_zero %>%
  pivot_wider(
    names_from = flow,                          # Coloca `flow` como cabeçalhos de coluna
    values_from = travel_time_change,           # Valores das colunas a preencher
    id_cols = eco_drivers_range,
    names_glue = "{flow}",                      # Define os nomes das colunas como valores de `flow`
    values_fill = list(travel_time_change = NA) # Preenche valores ausentes com NA
  )

df_reorganized_co2_zero <- df_changes_from_zero %>%
  pivot_wider(
    names_from = flow,                          # Coloca `flow` como cabeçalhos de coluna
    values_from = emission_co2_change,           # Valores das colunas a preencher
    id_cols = eco_drivers_range,
    names_glue = "{flow}"                      # Define os nomes das colunas como valores de `flow`
  )
