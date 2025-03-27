# 1
options(scipen=999)
Model <- lm(Price ~ Sqft + Beds + Baths + Colonial, data = myData)
summary(Model)
predict(Model, data.frame(Sqft=2500, Beds=3, Baths=2, Colonial=1), level=.95, interval="prediction")
# 2
Model2 <- lm(Return ~ PE + PS, data = myData)
summary(Model2)
# 3
Model3 <- glm(STEM ~ GPA + SAT + White + Female + Asian, 
              data = myData, 
              family = binomial)
summary(Model3)

newdata_asian <- data.frame(GPA = 3.4, 
                            SAT = 1400, 
                            White = 0, 
                            Female = 0, 
                            Asian = 1)
pred_prob_asian <- predict(Model3, newdata = newdata_asian, type = "response")
percent_asian <- pred_prob_asian * 100
cat("Predicted probability for an Asian male student (in %):", percent_asian, "\n")
newdata_other <- data.frame(GPA = 3.4, SAT = 1400, White = 0, Female = 0, Asian = 0)

pred_prob_other <- predict(Model3, newdata = newdata_other, type = "response")

percent_other <- pred_prob_other * 100

cat("Predicted probability for a male student (neither White nor Asian) (in %):", percent_other, "\n")


newdata_female <- data.frame(GPA = 3.4, SAT = 1400, White = 1, Female = 1, Asian = 0)

pred_prob_other <- predict(Model3, newdata = newdata_female, type = "response")

percent_other <- pred_prob_other * 100

cat("Predicted probability for a female student (White) (in %):", percent_other, "\n")


asian_female <- data.frame(GPA = 3.4, SAT = 1400, White = 0, Female = 1, Asian = 1)

pred_prob_other <- predict(Model3, newdata = asian_female, type = "response")

percent_other <- pred_prob_other * 100

cat("Predicted probability for a female student (Asian) (in %):", percent_other, "\n")

other_female <- data.frame(GPA = 3.4, SAT = 1400, White = 0, Female = 1, Asian = 0)

pred_prob_other <- predict(Model3, newdata = other_female, type = "response")

percent_other <- pred_prob_other * 100

cat("Predicted probability for a female student (Not white or Asian) (in %):", percent_other, "\n")
# 4

Model4 <- glm(Complication ~ Age + Weight + Diabetes, data = myData)
summary(Model4)
parta <- data.frame(Age=60, Weight=180, Diabetes=1)
pp4a <- predict(Model4, newdata = parta, type = "response")
pp4a
partb <- data.frame(Age=60, Weight=180, Diabetes=0)
pp4b <- predict(Model4, newdata = partb, type = "response")
pp4b
# Fit the logistic regression model with the binomial family
Model4 <- glm(Complication ~ Age + Weight + Diabetes, 
              data = myData, 
              family = binomial)
summary(Model4)

# For a 60-year-old diabetic patient (Diabetes = 1)
parta <- data.frame(Age = 60, Weight = 180, Diabetes = 1)
# Use type = "response" to get the predicted probability
p_diabetic <- predict(Model4, newdata = parta, type = "response")
# Convert probability to odds: odds = p / (1 - p)
odds_diabetic <- p_diabetic / (1 - p_diabetic)
# Round to 2 decimal places
odds_diabetic_rounded <- round(odds_diabetic, 2)
cat("Odds of complication for a diabetic patient:", odds_diabetic_rounded, "\n")

# For a 60-year-old nondiabetic patient (Diabetes = 0)
partb <- data.frame(Age = 60, Weight = 180, Diabetes = 0)
p_nondiabetic <- predict(Model4, newdata = partb, type = "response")
odds_nondiabetic <- p_nondiabetic / (1 - p_nondiabetic)
odds_nondiabetic_rounded <- round(odds_nondiabetic, 2)
cat("Odds of complication for a nondiabetic patient:", odds_nondiabetic_rounded, "\n")

# Compute the percentage difference in the odds
# Percentage difference = ((odds_diabetic - odds_nondiabetic) / odds_nondiabetic) * 100
perc_diff <- ((odds_diabetic - odds_nondiabetic) / odds_nondiabetic) * 100
perc_diff_rounded <- round(perc_diff, 2)
cat("Percentage difference in the odds (diabetic vs. nondiabetic):", perc_diff_rounded, "%\n")


