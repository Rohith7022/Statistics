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

library(readxl)


# Reading the file into R

data <- read_excel("~/Downloads/NSSO68.xlsx")


model_nsso <- lm(state ~ Age + Education + Sex + Marital_Status + Land_Total_possessed + Meals_Payment
                , data = data)
summary(model_nsso)

par(mfrow = c(2, 2))
plot(model_nsso)




library(car)

ncvTest(model_nsso) # Breusch-Pagan test

# Fix: Use robust standard errors
library(sandwich)
library(lmtest)

coeftest(model_nsso, vcov = vcovHC(model_nsso, type = "HC1"))

influencePlot(model_nsso)


# Get Cook's distance
cooksd <- cooks.distance(model_nsso)

# Plot
plot(cooksd, type = "h", main = "Cook's Distance")
abline(h = 4/length(cooksd), col = "red")  # Common threshold

# Flag outliers
outlier_ids <- which(cooksd > 4/length(cooksd))

# Influence plot
library(car)
influencePlot(model_nsso)

# Refit model after removing influential observations
model_nsso_refined <- lm(state ~ Age + Education + Sex + Marital_Status + Land_Total_possessed + Meals_Payment , data = data[-outlier_ids, ])
summary(model_nsso_refined)

model_nsso_refined <- update(model_nsso, subset = -c(outlier_ids))
summary(model_nsso_refined)


