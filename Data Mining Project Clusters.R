library(dplyr)

myData_clean <- myData %>%
  # Create a pet ownership indicator
  mutate(PetOwner = if_else(Cats > 0 | Dogs > 0, 1, 0)) %>%
  
  # Create a custom marital status variable
  mutate(MaritalStatus = case_when(
    F_Rel > 0 & M_Rel > 0 ~ "Married",
    F_Rel > 0 & M_Rel == 0 ~ "FemaleOnly",
    M_Rel > 0 & F_Rel == 0 ~ "MaleOnly",
    TRUE ~ "Other"
  )) %>%
  
  # Convert residence type and status to factors with descriptive labels
  mutate(ResidenceType = factor(ResType, levels = 1:6,
                                labels = c("Apartment", "Condo", "SingleFamily",
                                           "MultipleFamily", "Mobile", "Other")),
         ResidenceStatus = factor(ResStatus, levels = 1:3,
                                  labels = c("Owned", "Rented", "Other"))) %>%
  
  # Calculate total work hours and determine number of adults and kids
  mutate(TotalWorkHrs = FWrkHrs + MWrkHrs,
         Adults = case_when(
           MaritalStatus == "Married" ~ 2,
           MaritalStatus %in% c("FemaleOnly", "MaleOnly") ~ 1,
           TRUE ~ NA_real_
         ),
         Kids = HHNbr - Adults) %>%
  
  # Create an alternative Age variable based on available birth years
  mutate(Age = case_when(
    MaritalStatus == "Married" ~ 1985 - round((FBirth + MBirth) / 2),
    MaritalStatus == "FemaleOnly" ~ 1985 - FBirth,
    MaritalStatus == "MaleOnly" ~ 1985 - MBirth,
    TRUE ~ NA_real_
  )) %>%
  
  # Instead of AvgEdu, compute the highest household education level
  # For married households, take the maximum of FEdu and MEdu;
  # for single households, use the available education.
  mutate(HighestEduc = case_when(
    MaritalStatus == "Married" ~ pmax(FEdu, MEdu, na.rm = TRUE),
    MaritalStatus == "FemaleOnly" ~ FEdu,
    MaritalStatus == "MaleOnly" ~ MEdu,
    TRUE ~ NA_real_
  )) %>%
  
  # Create a binary variable: 1 if highest education is college or above (>=9),
  # 0 if highest education corresponds to high school.
  mutate(CollegePlus = if_else(HighestEduc >= 9, 1, 0)) %>%
  
  # Optionally, create an indicator for high income relative to the median
  mutate(HighIncome = if_else(HHInc > median(HHInc, na.rm = TRUE), 1, 0)) %>%
  
  # Select only the variables we want to keep for analysis
  select(HH_ID, HHInc, Kids, Adults, Age, TotalWorkHrs, CollegePlus,
         PetOwner, ResidenceType, ResidenceStatus, MaritalStatus, HighIncome,
         YogExp, DinExp)

# View the cleaned data
head(myData_clean)
myData_clean





# Load required libraries
library(dplyr)
library(ggplot2)

# ----- Step 1: Extract a subset of numeric variables for clustering -----
# Choose variables you consider important for clustering. Here we use:
# - HHInc: Household Income
# - Kids: Number of kids
# - Age: Age of the (average) adult(s)
# - TotalWorkHrs: Total work hours of the household
# - YogExp: Yogurt expenditure
# - DinExp: Frozen dinner expenditure
num_vars <- myData_clean %>% 
  select(HHInc, Kids, Age, TotalWorkHrs, YogExp, DinExp)

# ----- Step 2: Scale the numeric variables -----
num_vars_scaled <- scale(num_vars)
# Clear the current plotting device(s)
while(dev.cur() > 1) dev.off()

# Open a new device (uncomment the one that applies to your OS)
# windows()   # For Windows
# quartz()    # For Mac
# x11()       # For Linux

# Set smaller margins
par(mar = c(1, 1, 1, 1))

# Now plot your elbow method chart
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Choosing k")

# After inspecting the plot, suppose we choose k = 3 (adjust as needed)

# ----- Step 4: Run k-means clustering -----
set.seed(1)
kmeans_result <- kmeans(num_vars_scaled, centers = 2, nstart = 20)
myData_clean$Cluster <- kmeans_result$cluster

# ----- Step 5: Summarize the clusters -----
cluster_summary <- myData_clean %>%
  group_by(Cluster) %>%
  summarise(Count = n(),
            Mean_HHInc = mean(HHInc, na.rm = TRUE),
            Mean_Age = mean(Age, na.rm = TRUE),
            Mean_TotalWorkHrs = mean(TotalWorkHrs, na.rm = TRUE),
            Mean_YogExp = mean(YogExp, na.rm = TRUE),
            Mean_DinExp = mean(DinExp, na.rm = TRUE))
print(cluster_summary)

# ----- Optional: Visualize clusters with a scatterplot of Yogurt vs Dinner Expenditures -----
ggplot(myData_clean, aes(x = YogExp, y = DinExp, color = factor(Cluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(color = "Cluster",
       title = "Clusters Based on Yogurt and Frozen Dinner Expenditures",
       x = "Yogurt Expenditure", y = "Frozen Dinner Expenditure") +
  theme_minimal()



library(dplyr)

# Generate a summary table that computes the mean for every numeric variable by cluster
cluster_summary_all <- myData_clean %>%
  group_by(Cluster) %>%
  summarise_if(is.numeric, ~ mean(., na.rm = TRUE))

print(cluster_summary_all)
