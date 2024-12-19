#Final Project - National Risk Index (New Jersey)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
#Reads in the file for New Jersey Hazards
#Checks the names of each column
nj_hazards <- read.csv("ENVST-325-CENSUSTRACTS-NJ-2.csv", skip=1)
colnames(nj_hazards) 

#Data frame for Mercer, Monmouth, and Middlesex counties in New Jersey
mercer <- nj_hazards[nj_hazards$COUNTY == "Mercer",]
monmouth <- nj_hazards[nj_hazards$COUNTY == "Monmouth",]
middlesex <- nj_hazards[nj_hazards$COUNTY == "Middlesex",]

# Used colorblind friendly colors to distinguish variables
custom_colors <- c(
  "CFLD_RISKS" = "#1b9e77",  
  "CWAV_RISKS" = "#80b1d3",  
  "DRGT_RISKS" = "#fdb462",  
  "HRCN_RISKS" = "#8dd3c7",  
  "HAIL_RISKS" = "#bebada",  
  "HWAV_RISKS" = "#ffed6f",  
  "ISTM_RISKS" = "#b3de69", 
  "LNDS_RISKS" = "#fb8072",  
  "RFLD_RISKS" = "#d9d9d9",  
  "TRND_RISKS" = "#bc80bd",  
  "WFIR_RISKS" = "#ccebc5"   
)
#Renamed hazard variable names to the appropriate environmental hazards
hazard_labels <- c(
  "CFLD_RISKS" = "Coastal Flooding",
  "CWAV_RISKS" = "Coastal Waves",
  "DRGT_RISKS" = "Drought",
  "HRCN_RISKS" = "Hurricane",
  "HAIL_RISKS" = "Hail",
  "HWAV_RISKS" = "Heat Wave",
  "ISTM_RISKS" = "Ice Storm",
  "LNDS_RISKS" = "Landslide",
  "RFLD_RISKS" = "River Flooding",
  "TRND_RISKS" = "Tornado",
  "WFIR_RISKS" = "Wildfire"
)

mercer_hazard_long <- mercer %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer(                       # Extends number of rows the data frame can take
    cols = -COUNTY,                   # Reshapes the columns except county
    names_to = "Hazard_Type",         # Designates a column for hazard types
    values_to = "Risk_Score"          # Designates a for risk scores
  )
print(mercer_hazard_long)
#Plots the hazard risk scores in Mercer County
ggplot(mercer_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels   # Replace hazard variable names labels with actual label names 
  ) +
  labs(
    title = "Hazard Risk Scores by Type in Mercer County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"         
  ) +
  theme_minimal() + #Makes the background clear to see
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Ensures the label names are correctly placed

#---------
monmouth_hazard_long <- monmouth %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer(                       #Extends number of rows the data frame can take
    cols = -COUNTY,                   # Reshapes the columns except county
    names_to = "Hazard_Type",         # Designates a column for hazard types
    values_to = "Risk_Score"          # Designates a for risk scores
  )

# View the reshaped data
print(monmouth_hazard_long)
#Plots the hazard risk scores in Monmouth County
ggplot(monmouth_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels   # Replace hazard variable names labels with actual label names 
  ) +
  labs(
    title = "Hazard Risk Scores in Monmouth County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"         
  ) +
  theme_minimal() + #Makes the background clear to see
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Ensures the label names are correctly placed

#---------------
middlesex_hazard_long <- middlesex %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer( #Extends number of rows the data frame can take
    cols = -COUNTY,                   # Reshapes the columns except county
    names_to = "Hazard_Type",         # Designates a column for hazard types
    values_to = "Risk_Score"          # Designates a for risk scores
  )

# View the reshaped data
print(middlesex_hazard_long)
#Plots the hazard risk scores in Middlesex County
ggplot(middlesex_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels   # Replace hazard variable names labels with actual label names 
  ) +
  labs(
    title = "Hazard Risk Scores in Middlesex County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"        
  ) +
  theme_minimal() + #Makes the background clear to see
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Ensures the label names are correctly placed
#---------------
# Calculates the average risk scores by hazard type excluding zeros for Mercer County
mercer_avg_risks <- mercer_hazard_long %>%
  filter(Risk_Score != 0) %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))

# Calculates the average risk scores by hazard type excluding zeros for Monmouth County
monmouth_avg_risks <- monmouth_hazard_long %>%
  filter(Risk_Score != 0) %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))

# Calculates the average risk scores by hazard type excluding zeros for Middlesex County
middlesex_avg_risks <- middlesex_hazard_long %>%
  filter(Risk_Score != 0) %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))

# Prints the results to view the most prominent environmental hazards in each county
print(mercer_avg_risks)
print(monmouth_avg_risks)
print(middlesex_avg_risks)

# Binds the data frames of the three counties to combine their average risk scores
combined_risks <- bind_rows(
  mutate(mercer_avg_risks, County = "Mercer"),
  mutate(monmouth_avg_risks, County = "Monmouth"),
  mutate(middlesex_avg_risks, County = "Middlesex")
)

# Plots the data of all the risk scores
ggplot(combined_risks, aes(x = Hazard_Type, y = Avg_Risk, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  facet_wrap(~County) +
  scale_fill_manual(values = custom_colors, labels = hazard_labels) +  # Update legend labels
  scale_x_discrete(labels = hazard_labels) +                          # Update x-axis labels
  labs(
    title = "Average Hazard Risk Scores by County",
    x = "Hazard Type",
    y = "Average Risk Score",
    fill = "Hazard Type"
  ) +
  theme_minimal() +                                        # Makes the background clear to see
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ensures the label names are correctly placed


