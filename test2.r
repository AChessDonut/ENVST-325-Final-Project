#Final Project - National Risk Index (New Jersey)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
#Reads in the file for New Jersey Hazards
#Checks the names of each column
# Calculate Risk Index using Expected Annual Loss and Frequency
nj_hazards <- nj_hazards %>%
  mutate(
    CFLD_Risk_Index = CFLD_EALB * CFLD_AFREQ * SOVI_SCORE / RESL_SCORE,
    DRGT_Risk_Index = DRGT_EALA * DRGT_AFREQ * SOVI_SCORE / RESL_SCORE,
    ERQK_Risk_Index = ERQK_EALB * ERQK_AFREQ * SOVI_SCORE / RESL_SCORE,
    HAIL_Risk_Index = HAIL_EALB * HAIL_AFREQ * SOVI_SCORE / RESL_SCORE,
    HWAV_Risk_Index = HWAV_EALB * HWAV_AFREQ * SOVI_SCORE / RESL_SCORE,
    HRCN_Risk_Index = HRCN_EALB * HRCN_AFREQ * SOVI_SCORE / RESL_SCORE,
    ISTM_Risk_Index = ISTM_EALB * ISTM_AFREQ * SOVI_SCORE / RESL_SCORE,
    LNDS_Risk_Index = LNDS_EALB * LNDS_AFREQ * SOVI_SCORE / RESL_SCORE,
    LTNG_Risk_Index = LTNG_EALB * LTNG_AFREQ * SOVI_SCORE / RESL_SCORE,
    RFLD_Risk_Index = RFLD_EALB * RFLD_AFREQ * SOVI_SCORE / RESL_SCORE,
    SWND_Risk_Index = SWND_EALB * SWND_AFREQ * SOVI_SCORE / RESL_SCORE,
    TRND_Risk_Index = TRND_EALB * TRND_AFREQ * SOVI_SCORE / RESL_SCORE,
    WFIR_Risk_Index = WFIR_EALB * WFIR_AFREQ * SOVI_SCORE / RESL_SCORE
)


#Data frame for Mercer, Monmouth, and Middlesex counties in New Jersey
mercer <- nj_hazards[nj_hazards$COUNTY == "Mercer",]
monmouth <- nj_hazards[nj_hazards$COUNTY == "Monmouth",]
middlesex <- nj_hazards[nj_hazards$COUNTY == "Middlesex",]

# Find maximum risk scores by hazard type for Mercer County
mercer_max_risks <- mercer_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Max_Risk = max(Risk_Score, na.rm = TRUE))

print(mercer_max_risks)

# Repeat for Monmouth and Middlesex counties
monmouth_max_risks <- monmouth_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Max_Risk = max(Risk_Score, na.rm = TRUE))

print(monmouth_max_risks)

middlesex_max_risks <- middlesex_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Max_Risk = max(Risk_Score, na.rm = TRUE))
print(middlesex_max_risks)

# Calculate average risk scores by hazard type for Mercer County
mercer_avg_risks <- mercer_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))

print(mercer_avg_risks)

monmouth_avg_risks <- monmouth_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))

middlesex_avg_risks <- middlesex_hazard_long %>%
  group_by(Hazard_Type) %>%
  summarize(Avg_Risk = mean(Risk_Score, na.rm = TRUE))


# Summarize risk scores for hazards in Mercer 
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
  select(COUNTY, CFLD_Risk_Index, DRGT_Risk_Index, ERQK_Risk_Index, HAIL_Risk_Index,
         HWAV_Risk_Index, HRCN_Risk_Index, ISTM_Risk_Index, LNDS_Risk_Index, LTNG_Risk_Index,
         RFLD_Risk_Index, SWND_Risk_Index, TRND_Risk_Index,WFIR_Risk_Index
  ) %>%
  pivot_longer(
    cols = -COUNTY,
    names_to = "Hazard_Type",
    values_to = "Risk_Index"
  )
# View the reshaped data
print(mercer_hazard_long)

ggplot(mercer_hazard_long, aes(x = Hazard_Type, y = Risk_Score, fill = Hazard_Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels      # Replace legend labels with friendly names
  ) +
  labs(
    title = "Hazard Risk Scores by Type in Mercer County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"         # Rename legend title
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels      # Replace legend labels with friendly names
  ) +
  labs(
    title = "Hazard Risk Scores by Type in Monmouth County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"         # Rename legend title
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  scale_fill_manual(
    values = custom_colors,  # Use the updated colorblind-friendly palette
    labels = hazard_labels      # Replace legend labels with friendly names
  ) +
  labs(
    title = "Hazard Risk Scores by Type in Middlesex County",
    x = "Hazard Type",
    y = "Risk Score",
    fill = "Hazard Type"         # Rename legend title
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
#---------------


