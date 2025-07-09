getwd()
options(repos = c(CRAN = "https://cloud.r-project.org"))

options(repos = c(CRAN = "https://cloud.r-project.org"))
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)   # For thematic maps



# Step 1: Load NSSO data from Excel
nsso_data <- read_excel("~/Downloads/NSSO68.xlsx")

# Step 2: Filter for Karnataka (State Code 29)
karnataka_data <- nsso_data %>% 
  filter(state == 29)

# Step 3: Replace NA with 0 in entire dataset
karnataka_data[is.na(karnataka_data)] <- 0

karnataka_data <- karnataka_data[, colSums(is.na(karnataka_data)) == 0]

sum(is.na(karnataka_data))  # Should return 0



names(karnataka_data)



# Step 5: Load Karnataka district GeoJSON
karnataka_geo <- st_read("~/Downloads/KARNATAKA_DISTRICTS.geojson")  # adjust path if needed

names(karnataka_geo)  # assuming this is your geojson spatial file loaded with `sf::st_read()`


district_map <- data.frame(
  District = as.character(1:30),  # NSSO district codes (example: 1 to 30)
  district_name = c(
    "bagalkote", "ballari", "bangalore", "belagavi", "bengaluru rural", "bidar",
    "chamarajanagara", "chikkaballapura", "chikkamagaluru", "chitradurga", "dakshina kannada",
    "davanagere", "dharwad", "gadag", "hassan", "haveri", "kalaburagi", "kodagu", "kolar", "koppal",
    "mandya", "mysuru", "raichur", "ramanagara", "shivamogga", "tumakuru", "udupi", "uttara kannada",
    "vijayapura", "yadgir"
  )
)

names(karnataka_geo)


library(sf)


#
# Step 4: Summarise average milk consumption per district code
district_consumption <- karnataka_data %>%
  group_by(District) %>%
  summarise(avg_milk = mean(milk_q, na.rm = TRUE)) %>%
  mutate(District = as.integer(District))


# Step 6: Ensure district codes match (assuming 'dtcode11' is district code)
karnataka_geo$dtcode11 <- as.integer(karnataka_geo$dtcode11)

# Step 7: Join on district code (not name!)
karnataka_merged <- left_join(karnataka_geo, district_consumption, by = c("dtcode11" = "District"))

# Step 8: Plot using ggplot2
ggplot(karnataka_merged) +
  geom_sf(aes(fill = avg_milk), color = "black", size = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "lightgrey") +
  labs(
    title = "Average Milk Consumption per District in Karnataka",
    fill = "Milk (litres)"
  ) +
  theme_minimal()
