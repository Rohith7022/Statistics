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

install.packages("readxl")  

library(readxl)
library(dplyr)

# Read the Excel file (default reads first sheet)
salaries_2024 <- read_excel("~/Downloads/IPL SALARIES 2024.xlsx")

# Example: If your data is already read into 'salaries_2024'
library(dplyr)
library(stringr)




# Convert season to factor

ipl_data$Season <- as.factor(ipl_data$Season)


#nCheck how many NA values are in each column

colSums(is.na(ipl_data))

#  View rows with any NA values (optional)
ipl_data[!complete.cases(ipl_data), ]

# Remove rows that are fully NA (all columns are NA)
ipl_data <- ipl_data[rowSums(is.na(ipl_data)) != ncol(ipl_data), ]

#  Optionally remove rows with too many NAs (e.g., more than half the columns)
ipl_data <- ipl_data[rowSums(is.na(ipl_data)) < ncol(ipl_data)/2, ]

#  Confirm cleaning
cat("Remaining rows:", nrow(ipl_data), "\n")
cat("Remaining columns:", ncol(ipl_data), "\n")
sum(is.na(ipl_data))  


# Replace all NA in numeric columns with 0
ipl_data[sapply(ipl_data, is.numeric)] <- lapply(ipl_data[sapply(ipl_data, is.numeric)], function(x) ifelse(is.na(x), 0, x))

# Load required package
library(dplyr)

# Step 1: Standardize column names for easier coding
names(ipl_data) <- tolower(gsub("\\.", "_", names(ipl_data)))
# Now you have: match_id, strikers, bowlers, runs_scored, wicket_confirmation, etc.

# Step 2: Summary of Runs Scored per Batsman (striker) per Match
runs_summary <- ipl_data %>%
  group_by(match_id, striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE), .groups = "drop") %>%
  rename(player = striker, match = match_id)

# Step 3: Summary of Wickets Taken per Bowler per Match
# We assume a wicket is counted where wicket_confirmation == "Yes"
wickets_summary <- ipl_data %>%
  filter(tolower(wicket_confirmation) == "yes") %>%
  group_by(match_id, bowler) %>%
  summarise(total_wickets = n(), .groups = "drop") %>%
  rename(player = bowler, match = match_id)

# Step 4: Merge the two summaries
player_match_summary <- full_join(runs_summary, wickets_summary, by = c("match", "player"))

# Step 5: Replace NAs with 0 (for players who only batted or only bowled)
player_match_summary[is.na(player_match_summary)] <- 0

# Step 6: View the result
head(player_match_summary)




# Top 3 Batsmen
top_batsmen <- ipl_data %>%
  group_by(season, striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE), .groups = 'drop') %>%
  group_by(season) %>%
  top_n(3, total_runs)
top_batsmen

# Top 3 Bowlers
top_bowlers <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(season, bowler) %>%
  summarise(total_wickets = n(), .groups = 'drop') %>%
  group_by(season) %>%
  top_n(3, total_wickets)
top_bowlers

# Runs distribution of top batsmen
top_batsmen_data <- ipl_data %>%
  filter(striker %in% top_batsmen$striker)


library(fitdistrplus)


fit_runs <- fitdist(top_batsmen_data$runs_scored, "pois")
plot(fit_runs)

# Wickets distribution of top bowlers
top_bowlers_data <- ipl_data %>%
  filter(bowler %in% top_bowlers$bowler & wicket_confirmation == 1)

fit_wickets <- fitdist(rep(1, nrow(top_bowlers_data)), "pois")
plot(fit_wickets)

# Pull his runs conceded per ball
pathirana_bowling <- ipl_data %>%
  filter(str_detect(tolower(bowler), "pathirana")) %>%
  pull(runs_scored)

# Check if there's data
length(pathirana_bowling)
summary(pathirana_bowling)



# Fit distribution for Matheesha Pathirana (M Pathirana)


library(fitdistrplus)

# Fit Poisson
fit_pois <- fitdist(pathirana_bowling, "pois")

# Fit Negative Binomial
fit_nb <- fitdist(pathirana_bowling, "nbinom")

# Fit Geometric
fit_geom <- fitdist(pathirana_bowling, "geom")

par(mfrow = c(2, 2))  # 2x2 grid for plots

plot(fit_pois)
plot(fit_nb)
plot(fit_geom)

gofstat(list(fit_pois, fit_nb, fit_geom),
        fitnames = c("Poisson", "NegBinomial", "Geometric"))



library(dplyr)
library(fitdistrplus)
library(ggplot2)

# Filter Pathirana's bowling data (you’ve already done this)
pathirana_bowling <- ipl_data %>%
  filter(str_detect(tolower(bowler), "pathirana")) %>%
  pull(runs_scored)

ggplot(data.frame(Runs = pathirana_bowling), aes(x = Runs)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", boundary = 0) +
  ggtitle("Histogram: Runs Conceded Per Ball by Matheesha Pathirana") +
  xlab("Runs per Ball") +
  ylab("Frequency") +
  theme_minimal()

# Fit candidate distributions
fit_pois <- fitdist(pathirana_bowling, "pois")
fit_nb <- fitdist(pathirana_bowling, "nbinom")
fit_geom <- fitdist(pathirana_bowling, "geom")

par(mfrow = c(2, 2))  # 2x2 grid
plot(fit_pois)
plot(fit_nb)
plot(fit_geom)

#Finding the relationship between a player’s performance and the salary he gets in your data (Correlation).

library(dplyr)

# Total performance by player (example: total runs)
performance_summary <- ipl_data %>%
  group_by(striker) %>%
  summarise(Performance = sum(runs_scored, na.rm = TRUE)) %>%
  rename(Player = striker)

library(readxl)
library(stringr)

# Load and clean salary data

salaries_2024 <- salaries_2024 %>%
  mutate(Salary_Cleaned = case_when(
    str_detect(tolower(Salary), "crore") ~ as.numeric(str_extract(Salary, "[0-9.]+")) * 100,
    str_detect(tolower(Salary), "lakh") ~ as.numeric(str_extract(Salary, "[0-9.]+")),
    TRUE ~ as.numeric(Salary)
  ))


# Merge performance and salary
performance_salary <- left_join(performance_summary, salaries_2024, by = "Player")

# Remove NAs if any
performance_salary <- performance_salary %>%
  filter(!is.na(Performance), !is.na(Salary_Cleaned))

# Pearson correlation (default)
correlation <- cor(performance_salary$Performance, performance_salary$Salary_Cleaned)

print(paste("Correlation between performance and salary:", round(correlation, 3)))


library(ggplot2)
library(ggrepel)

ggplot(performance_salary, aes(x = Performance, y = Salary_Cleaned, label = Player)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_text_repel(size = 3, max.overlaps = 12) +
  theme_minimal() +
  xlab("Total Performance (Runs)") +
  ylab("Salary (in Lakhs)") +
  ggtitle(paste("Performance vs Salary\nCorrelation: ", round(correlation, 3)))





