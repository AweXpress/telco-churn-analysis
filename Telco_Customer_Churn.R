# Use your already loaded file - rename to churn_df
churn_df <- Telco_Customer_Churn

# Data Cleaning but without %>%
# Convert blank spaces to numeric (trimws = removes blank space)
churn_df$TotalCharges <- as.numeric(trimws(churn_df$TotalCharges))

# Binary Encoding - 1 for Yes, 0 for No
churn_df$ChurnBinary <- ifelse(churn_df$Churn == "Yes", 1, 0)

# Remove the 11 NA rows (tenure = 0 customers)
churn_df <- churn_df[!is.na(churn_df$TotalCharges), ]

# Proof - should be 7043 and 26.54%
dim(churn_df)
mean(churn_df$ChurnBinary) * 100
table(churn_df$Churn)

# 5. Save for STATA
write.csv(churn_df, "C:/Users/Public/churn_clean.csv", row.names=FALSE)


# Advanced CLV at Risk (Final)

# Load clean data
churn_df <- read.csv("C:/Users/Public/churn_clean.csv")

# Logistic model for churn probability
model <- glm(ChurnBinary ~ tenure + MonthlyCharges + TotalCharges + SeniorCitizen, data=churn_df, family=binomial)
churn_df$predicted_churn <- predict(model, type="response")

# Clear old models (model1, etc. that cause the 215257 error)
rm(list=ls())

Reload your clean file (the 7032 row file you proved in STATA)
churn_df <- read.csv("C:/Users/Public/churn_clean.csv")


#Rebuild model fresh - ONLY with 7032 rows
model <- glm(ChurnBinary ~ tenure + MonthlyCharges + TotalCharges + SeniorCitizen, data=churn_df, family=binomial)

# Predict WITH newdata = churn_df (this forces 7032 rows)
pred <- predict(model, newdata = churn_df, type = "response")

# Check length - must be 7032
length(pred)

# Now add it - will work because both are 7032
churn_df$predicted_churn <- pred


# Finish CLV
churn_df$CLV <- (churn_df$MonthlyCharges * 0.70) / 0.265
churn_df$CLV_at_Risk <- churn_df$predicted_churn * churn_df$CLV


# Proof
head(churn_df$predicted_churn)
sum(churn_df$CLV_at_Risk)


# Save
write.csv(churn_df, "C:/Users/Public/churn_final_CLV.csv", row.names=FALSE)

# CLV at Risk - Advanced calculation
churn_df$CLV <- (churn_df$MonthlyCharges * 0.70) / 0.265
churn_df$CLV_at_Risk <- churn_df$predicted_churn * churn_df$CLV


# Risk Tier
churn_df$Risk_Tier <- ifelse(churn_df$predicted_churn >= 0.6, "High Risk",
                             ifelse(churn_df$predicted_churn >= 0.3, "Medium Risk", "Low Risk"))

# Executive Summary Numbers
table(churn_df$Risk_Tier)
sum(churn_df$CLV_at_Risk)
mean(churn_df$CLV_at_Risk)


# Save final
write.csv(churn_df, "C:/Users/Public/churn_final_CLV.csv", row.names=FALSE)


install.packages("openxlsx")
library(openxlsx)

# Load your final scored file
churn_df <- read.csv("C:/Users/Public/churn_final_CLV.csv")

# === 1. KPI CARDS FOR EXECUTIVE DASHBOARD ===
Total_Active_Revenue <- sum(churn_df$MonthlyCharges)
Total_CLV_at_Risk <- sum(churn_df$CLV_at_Risk)
Overall_Churn_Rate <- mean(churn_df$ChurnBinary) * 100
High_Risk_Count <- sum(churn_df$Risk_Tier == "High Risk")

KPI <- data.frame(
  Metric = c("Total Active Monthly Revenue ($)", "Total CLV at Risk ($)", "Overall Churn Rate (%)", "High-Risk Customer Count"),
  Value = c(Total_Active_Revenue, Total_CLV_at_Risk, Overall_Churn_Rate, High_Risk_Count)
)


# Data for Charts
chart1_contract <- aggregate(ChurnBinary ~ Contract, data=churn_df, mean) 
# Churn Rate by Contract Type
chart2_internet <- aggregate(CLV_at_Risk ~ InternetService, data=churn_df, sum)
# Revenue at Risk by Internet
chart3_tenure <- aggregate(ChurnBinary ~ tenure, data=churn_df, mean)
# Tenure vs Churn

# === 2. TOP 5 CHURN DRIVERS (From STATA) ===
# For this you need to run in STATA: logistic ChurnBinary i.Contract i.InternetService tenure MonthlyCharges SeniorCitizen
# Then take Odds Ratios
Drivers <- data.frame(
  Rank = 1:3,
  Churn_Driver = c("Month-to-Month Contract", "Fiber Optic Internet Service", "Low Tenure (< 12 months)"),
  Metric_Odds_Ratio = c("High Odds Ratio (OR > 3.0)", "High Odds Ratio", "Inverse Relationship (OR < 1)"),
  Business_Insight = c("Customers without long-term commitment have significantly higher churn rates.",
                       "Higher price combined with service issues leads to elevated churn.",
                       "The first year is critical retention window - tenure reduces churn.")
)

# === 3. CREATE EXCEL WORKBOOK ===
wb <- createWorkbook()

# Sheet 1 - Executive Dashboard (Data behind dashboard)
addWorksheet(wb, "Executive_Dashboard_KPIs")
writeData(wb, "Executive_Dashboard_KPIs", KPI)
writeData(wb, "Executive_Dashboard_KPIs", data.frame(Note=""), startRow=7)
writeData(wb, "Executive_Dashboard_KPIs", chart1_contract, startRow=8)
writeData(wb, "Executive_Dashboard_KPIs", chart2_internet, startRow=15)
writeData(wb, "Executive_Dashboard_KPIs", chart3_tenure, startRow=22)


# Sheet 2 - Top 5 Drivers
addWorksheet(wb, "Top 5 Churn Drivers")
writeData(wb, "Top 5 Churn Drivers", Drivers)


# Sheet 3 - Full Scored Dataset
addWorksheet(wb, "Scored_Dataset")
writeData(wb, "Scored_Dataset", churn_df)


# Save
saveWorkbook(wb, "C:/Users/Public/Executive_Excel_Report.xlsx", overwrite=TRUE)

print(KPI)


churn_df <- read.csv("C:/Users/Public/churn_final_CLV.csv")

churn_df$Tenure_Group <- cut(churn_df$tenure,
                             breaks=c(0,12,24,36,48,60,72),
                             labels=c("0-12","13-24","25-36","37-48","49-60","61-72"))

write.csv(churn_df, "C:/Users/Public/churn_final_CLV_grouped.csv", row.names=FALSE)
