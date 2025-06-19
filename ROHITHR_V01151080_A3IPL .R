getwd()
options(repos = c(CRAN = "https://cloud.r-project.org"))

options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("fitdistrplus")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggrepel")



# Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(ggpubr)

# Loading the data 

ipl_data <- read.csv("~/Downloads/IPL_ball_by_ball_updated till 2024.csv", stringsAsFactors = FALSE)

library(dplyr)

ipl_recent <- ipl_data %>% filter(Season %in% c(2022, 2023, 2024))

performance_summary <- ipl_recent %>%
  group_by(Striker) %>%
  summarise(
    Total_Runs = sum(runs_scored, na.rm = TRUE),
    Matches = n_distinct(Match.id)
  ) %>%
  rename(Player = Striker)

library(readxl)

# Read the Excel file (default reads first sheet)
salaries_2024 <- read_excel("~/Downloads/IPL SALARIES 2024-2.xlsx")
library(readxl)
library(dplyr)
library(stringr)


salaries_2024 <- salaries_2024 %>%
  mutate(Salary_Lakhs = suppressWarnings(case_when(
    str_detect(tolower(Salary), "crore") ~ as.numeric(str_extract(Salary, "[0-9.]+")) * 100,
    str_detect(tolower(Salary), "lakh") ~ as.numeric(str_extract(Salary, "[0-9.]+")),
    TRUE ~ as.numeric(Salary)  # for already numeric values
  )))


player_data <- left_join(performance_summary, salaries_2024, by = "Player")

# Remove missing or 0 salary cases
player_data <- player_data %>%
  filter(!is.na(Salary_Lakhs), Salary_Lakhs > 0)

# Regression: Salary ~ Performance
model_ipl <- lm(Salary_Lakhs ~ Total_Runs + Matches, data = player_data)
summary(model_ipl)

library(ggplot2)
ggplot(player_data, aes(x = Total_Runs, y = Salary_Lakhs)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Player Salary vs Total Runs (2022–2024)")


cor(player_data$Total_Runs, player_data$Salary_Lakhs, method = "pearson")
