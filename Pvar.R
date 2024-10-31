setwd("C:\Users\DELL\Downloads\8516077\Pvar.R")
# Load necessary libraries
install.packages(c("tidyverse", "plm", 'lmtest", "broom", "vars", "stargazer", "panelvar", "psych", "writexl"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(lmtest)
library(plm)
library(stargazer)
library(panelvar)
library(psych)
library(writexl)

# Assuming your dataset is named DATA_1
df <- DATA_1
summary(DATA_1)
str(DATA_1)
View(df)
# Convert columns to appropriate data types
DATA_1$Year <- as.integer(data$Year)
df$NCF_OA <- as.numeric(data$NCF_OA)
data$NCF_IA <- as.numeric(data$NCF_IA)
data$NCF_FA <- as.numeric(data$NCF_FA)
data$TOT_ASSET <- as.numeric(data$TOT_ASSET)
data$CUS_DEP <- as.numeric(data$CUS_DEP)
data$OP_CF <- as.numeric(data$OP_CF)
data$PHASE.NO <- as.factor(data$PHASE.NO)

# Plot 1: Total Assets by Firms Over the Years
ggplot(data, aes(x = Year, y = TOT_ASSET, color = Firms)) +
  geom_line() +
  labs(title = "Total Assets by Firms Over the Years",
       x = "Year",
       y = "Total Assets") +
  theme_minimal()

# Plot 2: Customer Deposits by Firms
ggplot(data, aes(x = Firms, y = CUS_DEP)) +
  geom_boxplot() +
  labs(title = "Customer Deposits by Firms",
       x = "Firms",
       y = "Customer Deposits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Heatmap of Net Cash Flow from Investing Activities (NCF_IA)
data_long <- data %>%
  pivot_longer(cols = starts_with("NCF"), names_to = "NCF_Type", values_to = "Value")

ggplot(data_long %>% filter(NCF_Type == "NCF_IA"), aes(x = Year, y = Firms, fill = Value)) +
  geom_tile() +
  labs(title = "Heatmap of Net Cash Flow from Investing Activities (NCF_IA)",
       x = "Year",
       y = "Firms",
       fill = "Net Cash Flow") +
  theme_minimal()

# Plot 4: Facet Plot of Operating Cash Flow (OP_CF) by Phase Number
# Remove the Timestamp column if it exists
if("Timestamp" %in% colnames(df)) {
  df <- df %>% select(-Timestamp)
}

# Clean non-numeric values and convert to numeric
df <- df %>%
  mutate(across(c(NCF_OA, NCF_IA, NCF_FA, TOTLIAB, TA, OP_CF, `Ait-1`, `OCFit-1`, `OCFit+1`), 
                ~as.numeric(gsub(",", "", .))))

# Check for remaining NAs
print(colSums(is.na(df)))

# Optionally, remove rows with NA values in specific columns
df <- df %>%
  drop_na(NCF_OA, OP_CF, `Ait-1`, `OCFit-1`, `OCFit+1`)

# Calculate Total Accruals and Scaled Total Accruals
df <- df %>%
  mutate(
    Total_Accruals = NCF_OA - OP_CF,
    Scaled_TA = Total_Accruals / `Ait-1`
  )

# Estimate the residuals using a linear model
model <- lm(Scaled_TA ~ `OCFit-1`, data = df)

# Extract residuals and calculate FRQ
df <- df %>%
  mutate(FRQ_residuals = residuals(model),
         FRQ = -FRQ_residuals)

# Apply the FRQ calculation function to each bank and year combination
df_result <- df %>%
  group_by(Firms, Year) %>%
  group_modify(~ {
    data <- .x
    model <- lm(Scaled_TA ~ `OCFit-1`, data = data)
    data <- data %>%
      mutate(FRQ_residuals = residuals(model),
             FRQ = -FRQ_residuals)
    return(data)
  }) %>%
  ungroup()

View(df_result)

# Calculate market share and HHI
data_result <- df %>%
  group_by(Year) %>% 
  mutate(market_share = TOTLIAB / sum(TOTLIAB, na.rm = TRUE)) %>%
  summarise(HHI = sum(market_share^2, na.rm = TRUE)) %>%
  right_join(df %>% select(Firms, Year) %>% distinct(), by = "Year") %>%
  arrange(Firms, Year)

View(data_result)

# Save the results to an Excel file
write_xlsx(df_result, path = "df_Results.xlsx")

# Check for the required columns in df_result and data_result
required_columns_frq <- c("Firms", "Year", "FRQ")
required_columns_hhi <- c("Firms", "Year", "HHI")

# Extract FRQ from df_result
if(all(required_columns_frq %in% colnames(df_result))) {
  df_frq <- df_result %>%
    select(Firms, Year, FRQ)
} else {
  stop("One or more required columns are missing in df_result: Firms, Year, FRQ")
}

# Extract HHI from data_result
if(all(required_columns_hhi %in% colnames(data_result))) {
  df_hhi <- data_result %>%
    select(Firms, Year, HHI)
} else {
  stop("One or more required columns are missing in data_result: Firms, Year, HHI")
}

# Rename columns for consistency
df_frq <- df_frq %>%
  rename(firm_id = Firms, year = Year)

df_hhi <- df_hhi %>%
  rename(firm_id = Firms, year = Year)

# Merge df_frq and df_hhi to ensure they contain the necessary data
merged_df <- merge(df_frq, df_hhi, by = c("firm_id", "year"))

# Print the first few rows of merged_df to debug
print("merged_df:")
print(head(merged_df))
View(merged_df)

# Ensure the correct data types
merged_df <- merged_df %>%
  mutate(
    firm_id = as.factor(firm_id),  # Convert firm_id to factor
    year = as.integer(year)        # Convert year to integer
  )

# Define the variables for the PVAR model
# Replace 'Firm_Life_Cycle' with the actual variable representing the firm life cycle
dependent_vars <- c("firm_id", "HHI", "FRQ")
colnames(merged_df)
str(merged_df)
# Check for missing values
colSums(is.na(merged_df))
# Load necessary libraries
if (!require(panelvar)) {
  install.packages("panelvar")
  library(panelvar)
}

# Define the variables for the PVAR model
dependent_vars <- c("FRQ", "HHI")

# Run the PVAR model
pvar_model <- pvargmm(
  dependent_vars = dependent_vars,
  lags = 1, 
  transformation = "fod",  # Forward orthogonal deviations transformation
  data = merged_df,
  panel_identifier = c("firm_id", "year"),
  steps = c("twostep"),  # Two-step GMM estimation
  system_instruments = FALSE,  # System GMM instruments set to FALSE
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE
)

# Display the summary of the model
summary(pvar_model)
install.packages("car")
library(car)
vif(lm(FRQ ~ HHI + firm_id + year, data = merged_df))
pvar_model <- pvargmm(
  dependent_vars = dependent_vars,
  lags = 1,
  transformation = "fod",
  data = merged_df,
  panel_identifier = c("firm_id", "year"),
  steps = c("twostep"),
  system_instruments = FALSE,
  max_instr_dependent_vars = 99,
  max_instr_predet_vars = 99,
  min_instr_dependent_vars = 2L,
  min_instr_predet_vars = 1L,
  collapse = FALSE,
  effect = "individual"
)
summary(merged_df$HHI)
summary(merged_df$year)
summary(merged_df$firm_id)


table(merged_df$firm_id, merged_df$year)
panel_summary <- merged_df %>%
  group_by(firm_id) %>%
  summarize(n_years = n_distinct(year))
View(panel_summary)

