library(ggplot2)
library(dplyr)

# Load Data
nj_hazards <- read.csv("ENVST-325-CENSUSTRACTS-NJ-2.csv", skip = 1)
colnames(nj_hazards)

# Filter by County
target_counties <- c("Mercer", "Monmouth", "Middlesex")
mercer <- filter(nj_hazards, COUNTY == "Mercer")
Middle_NJ <- filter(nj_hazards, COUNTY %in% target_counties)

# Plot: Coastal Flooding
mercer <- filter(mercer, !is.na(CFLD_EXP_AREA) & !is.na(CFLD_RISKS))
ggplot(data = mercer, aes(x = CFLD_EXP_AREA, y = CFLD_RISKS)) +
  geom_point(color = "red") +
  geom_line(color = "blue") +
  labs(x = "Expected Annual Loss - Building Value", y = "Risk Score") +
  theme_classic()

# Example: Area Plot (if `Year`, `CO2`, `Entity` exist)
if ("Year" %in% colnames(mercer)) {
  ggplot(data = mercer, aes(x = Year, y = CO2, fill = Entity)) +
    geom_area()
}
summary(mercer$CFLD_EXP_AREA)
summary(mercer$CFLD_RISKS)
