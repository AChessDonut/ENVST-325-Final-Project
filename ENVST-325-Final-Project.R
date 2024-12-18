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

#Merges data frame for all three counties
Middle_NJ <- nj_hazards[nj_hazards$COUNTY  == "Mercer" |
                   nj_hazards$COUNTY  == "Monmouth" |
                   nj_hazards$COUNTY  == "Middlesex", ]

#Data frame for coastal flooding in New Jersey
ggplot(data = mercer, # data for plot
       aes(x=CFLD_EXP_AREA, y=CFLD_RISKS, color="Coastal Flooding Damage Costs" ) )+ #Construct aesthetic mappings 
  geom_point()+ 
  geom_line()+ # Connects points to construct a scatterplot
  labs(x="Expected Annual Loss - Building Value", y="Risk Score")+ # make axis labels
  theme_classic()

#data frame for the middle counties in New Jersey 
Middle_NJ_wildfire <- nj_hazards[nj_hazards$COUNTY  == "Mercer" |
                          nj_hazards$COUNTY  == "Monmouth" |
                          nj_hazards$COUNTY  == "Middlesex", ]

Middle_NJ <- nj_hazards %>%
  filter(COUNTY %in% c("Mercer", "Monmouth", "Middlesex")) %>%
  mutate(CFLD_EALB = as.numeric(CFLD_EALB),
         CFLD_RISKS = as.numeric(CFLD_RISKS))

ggplot(data = Middle_NJ, aes(x = CFLD_EALB, ymin = 0, ymax = CFLD_RISKS, fill = COUNTY)) +
  geom_ribbon(alpha = 0.5) +  
  geom_line(aes(y = CFLD_RISKS, color = COUNTY), size = 1) +  
  labs(
    title = "Risk Score by Expected Annual Loss (Building Value)",
    x = "Expected Annual Loss - Building Value",
    y = "Risk Score",
    fill = "County",
    color = "County"
  ) +
  theme_minimal()

# Summarize risk scores for hazards in Mercer 
hazard_summary_mercer <- mercer %>%
  summarize(
    Coastal_Flooding = mean(CFLD_RISKS, na.rm = TRUE), 
    Cold_Wave = mean(CWAV_RISKS, na.rm = TRUE), 
    Drought = mean(DRGT_RISKS, na.rm = TRUE), Earthquake = mean(ERQK_RISKS, na.rm = TRUE), 
    Hail = mean(HAIL_RISKS, na.rm = TRUE), Heat_Wave = mean(HWAV_RISKS, na.rm = TRUE),
    Hurricane = mean(HRCN_RISKS, na.rm = TRUE), Ice_Storm = mean(ISTM_RISKS, na.rm = TRUE), 
    Landslide = mean(LNDS_RISKS, na.rm = TRUE), Lightning = mean(LTNG_RISKS, na.rm = TRUE), 
    Riverine_Flood = mean(RFLD_RISKS, na.rm = TRUE), Tornado = mean(TRND_RISKS, na.rm = TRUE), 
    Wildfire = mean(WFIR_RISKS, na.rm = TRUE)
  )
print(hazard_summary_mercer)
# Summarize risk scores for hazards in Middlesex
hazard_summary_middlesex <- middlesex %>%
  summarize(
    Coastal_Flooding = mean(CFLD_RISKS, na.rm = TRUE), 
    Cold_Wave = mean(CWAV_RISKS, na.rm = TRUE), 
    Drought = mean(DRGT_RISKS, na.rm = TRUE), Earthquake = mean(ERQK_RISKS, na.rm = TRUE), 
    Hail = mean(HAIL_RISKS, na.rm = TRUE), Heat_Wave = mean(HWAV_RISKS, na.rm = TRUE),
    Hurricane = mean(HRCN_RISKS, na.rm = TRUE), Ice_Storm = mean(ISTM_RISKS, na.rm = TRUE), 
    Landslide = mean(LNDS_RISKS, na.rm = TRUE), Lightning = mean(LTNG_RISKS, na.rm = TRUE), 
    Riverine_Flood = mean(RFLD_RISKS, na.rm = TRUE), Tornado = mean(TRND_RISKS, na.rm = TRUE), 
    Wildfire = mean(WFIR_RISKS, na.rm = TRUE)
  )
print(hazard_summary_middlesex)

# Summarize risk scores for hazards in Monmouth
hazard_summary_monmouth <- monmouth %>%
  summarize(
    Coastal_Flooding = mean(CFLD_RISKS, na.rm = TRUE), 
    Cold_Wave = mean(CWAV_RISKS, na.rm = TRUE), 
    Drought = mean(DRGT_RISKS, na.rm = TRUE), Earthquake = mean(ERQK_RISKS, na.rm = TRUE), 
    Hail = mean(HAIL_RISKS, na.rm = TRUE), Heat_Wave = mean(HWAV_RISKS, na.rm = TRUE),
    Hurricane = mean(HRCN_RISKS, na.rm = TRUE), Ice_Storm = mean(ISTM_RISKS, na.rm = TRUE), 
    Landslide = mean(LNDS_RISKS, na.rm = TRUE), Lightning = mean(LTNG_RISKS, na.rm = TRUE), 
    Riverine_Flood = mean(RFLD_RISKS, na.rm = TRUE), Tornado = mean(TRND_RISKS, na.rm = TRUE), 
    Wildfire = mean(WFIR_RISKS, na.rm = TRUE)
  )
print(hazard_summary_monmouth)
colnames(hazard_summary_mercer)

custom_colors <- c(
  "CFLD_RISKS" = "#1b9e77",  # Dark Green
  "CWAV_RISKS" = "#80b1d3",  # Sky Blue
  "DRGT_RISKS" = "#fdb462",  # Tan/Orange
  "HRCN_RISKS" = "#8dd3c7",  # Mint Green
  "HAIL_RISKS" = "#bebada",  # Purple
  "HWAV_RISKS" = "#ffed6f",  # Yellow
  "ISTM_RISKS" = "#b3de69",  # Light Green
  "LNDS_RISKS" = "#fb8072",  # Coral Red
  "RFLD_RISKS" = "#d9d9d9",  # Gray
  "TRND_RISKS" = "#bc80bd",  # Light Purple
  "WFIR_RISKS" = "#ccebc5"   # Pale Green
)
mercer_hazard_long <- mercer %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer(
    cols = -COUNTY,                   # Reshape all columns except COUNTY
    names_to = "Hazard_Type",         # New column for hazard types
    values_to = "Risk_Score"          # New column for risk scores
  )

# View the reshaped data
print(mercer_hazard_long)

ggplot(mercer_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Hazard Risk Scores by Type in Mercer County",
    x = "Hazard Type", y = "Risk Score", fill = "Hazard Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)

#---------
monmouth_hazard_long <- monmouth %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer(
    cols = -COUNTY,                   # Reshape all columns except COUNTY
    names_to = "Hazard_Type",         # New column for hazard types
    values_to = "Risk_Score"          # New column for risk scores
  )

# View the reshaped data
print(monmouth_hazard_long)

ggplot(monmouth_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Hazard Risk Scores by Type in Monmouth County",
    x = "Hazard Type", y = "Risk Score", fill = "Hazard Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)

#---------------
middlesex_hazard_long <- middlesex %>%
  select(COUNTY, CFLD_RISKS, DRGT_RISKS, WFIR_RISKS, HWAV_RISKS, HRCN_RISKS, 
         LNDS_RISKS, RFLD_RISKS, TRND_RISKS, ISTM_RISKS) %>%  # Select hazard columns
  pivot_longer(
    cols = -COUNTY,                   # Reshape all columns except COUNTY
    names_to = "Hazard_Type",         # New column for hazard types
    values_to = "Risk_Score"          # New column for risk scores
  )

# View the reshaped data
print(middlesex_hazard_long)

ggplot(middlesex_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "Hazard Risk Scores by Type in Middlesex County",
    x = "Hazard Type", y = "Risk Score", fill = "Hazard Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)
#---------------
#Checked for statistical significance between variables to determine relationship with risk score
mercer_multi_model <- lm(CFLD_RISKS ~ CFLD_AFREQ + CFLD_EXP_AREA + CFLD_EXPB + CFLD_EALP, data = mercer)
summary(mercer_multi_model)

middlesex_multi_model <- lm(CFLD_RISKS ~ CFLD_AFREQ + CFLD_EXP_AREA + CFLD_EXPB + CFLD_EALP, data = middlesex)
summary(middlesex_multi_model)

monmouth_multi_model <- lm(CFLD_RISKS ~ CFLD_AFREQ + CFLD_EXP_AREA + CFLD_EXPB + CFLD_EALP, data = monmouth)
summary(monmouth_multi_model)



