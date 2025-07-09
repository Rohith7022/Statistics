# Load libraries
library(readxl)
library(dplyr)

# Step 1: Read the Sikkim Excel file
sikkim_data <- read_excel("~/Downloads/Sikkim_Data.xlsx")

# Step 2: Replace all NAs with 0
sikkim_data[is.na(sikkim_data)] <- 0

district_map_sikkim <- data.frame(
  state = rep(11, 4),
  district = c(1, 2, 3, 4),
  district_name = c("North", "West", "South", "East"),
  state_name = rep("Sikkim", 4)
)

sikkim_data <- merge(
  sikkim_data,
  district_map_sikkim,
  by.x = c("state", "District"),
  by.y = c("state", "district"),
  all.x = TRUE
)

# Step 3: Create total_cons from selected quantity columns (replace/add columns as needed)
sikkim_data$total_cons <- rowSums(sikkim_data[, c(
  "ricepds_q", "riceos_q", "ricetotal_q", "chira_q", "khoi_q", "muri_q",
  "ricepro_q", "riceGT_q", "Wheatpds_q", "wheatos_q", "wheattotal_q",
  "maida_q", "suji_q", "sewai_q", "bread_q", "wheatp_q", "wheatGT_q",
  "jowarp_q", "bajrap_q", "maizep_q", "barleyp_q", "milletp_q", "ragip_q",
  "cerealot_q", "cerealtot_q", "cerealsub_q", "cerealstt_q", "arhar_q",
  "gramdal_q", "gramwholep_q", "gramGT_q", "moong_q", "masur_q", "urd_q",
  "peasdal_q", "khesari_q", "otpulse_q", "gramp_q", "besan_q", "pulsep_q",
  "pulsestot_q", "pulsestt_q", "milk_q", "curd_q", "ghee_q", "butter_q",
  "eggsno_q", "fishprawn_q", "goatmeat_q", "chicken_q", "othrbirds_q", 
  "nonvegtotal_q", "potato_q", "onion_q", "tamato_q", "brinjal_q"
)], na.rm = TRUE)

# Step 4: Check unique districts
unique(sikkim_data$district_name)

# Step 5: Histogram of total household consumption
hist(
  sikkim_data$total_cons,
  main = "Distribution of Total Consumption in Sikkim",
  xlab = "Total Consumption (kg or litres per household)",
  col = "lightblue",
  border = "black"
)

# Step 6: Barplot of average total consumption by district
avg_consumption <- sikkim_data %>%
  group_by(district_name) %>%
  summarise(avg_cons = mean(total_cons, na.rm = TRUE))

barplot(
  avg_consumption$avg_cons,
  names.arg = avg_consumption$district_name,
  las = 2,
  col = "orange",
  main = "Average Total Consumption by District in Sikkim",
  xlab = "District",
  ylab = "Average Household Consumption"
)
