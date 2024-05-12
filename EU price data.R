library(dplyr)
library("lubridate")
library(openxlsx)
library(ggplot2)
library(zoo)
library(tidyr)
install.packages("zoo")

observations_by_date <- combined_data_EU1 %>%
  group_by(date) %>%
  summarise(observation_count = n())

### THE PRICE DATASETS ####
prices_EU_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2023.csv")
prices_EU_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2022.csv")
prices_EU_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2021.csv")
prices_EU_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2020.csv")
prices_EU_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2019.csv")
prices_EU_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2018.csv")
prices_EU_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2017.csv")
prices_EU_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2016.csv")
prices_EU_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2015.csv")
prices_EU_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2014.csv")
prices_EU_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_emea_2013.csv")

##### MAKE SURE ITS AS DATE ####
prices_EU_2023$date <- as.Date(prices_EU_2023$date)  # Converting to Date type
prices_EU_2022$date <- as.Date(prices_EU_2022$date)
prices_EU_2021$date <- as.Date(prices_EU_2021$date)
prices_EU_2020$date <- as.Date(prices_EU_2020$date)
prices_EU_2019$date <- as.Date(prices_EU_2019$date)
prices_EU_2018$date <- as.Date(prices_EU_2018$date)
prices_EU_2017$date <- as.Date(prices_EU_2017$date)
prices_EU_2016$date <- as.Date(prices_EU_2016$date)
prices_EU_2015$date <- as.Date(prices_EU_2015$date)
prices_EU_2014$date <- as.Date(prices_EU_2014$date)
prices_EU_2013$date <- as.Date(prices_EU_2013$date)

#### CONVERT TO MONTHLY DATA FOR EACH YEAR ####
prices_EU_2023_monthly <- prices_EU_2023 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2022_monthly <- prices_EU_2022 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2021_monthly <- prices_EU_2021 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2020_monthly <- prices_EU_2020 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2019_monthly <- prices_EU_2019 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary


prices_EU_2018_monthly <- prices_EU_2018 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2017_monthly <- prices_EU_2017 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2016_monthly <- prices_EU_2016 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2015_monthly <- prices_EU_2015 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2014_monthly <- prices_EU_2014 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_EU_2013_monthly <- prices_EU_2013 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

#### SAVE AS EXCEL ####
write.csv(prices_EU_2023_monthly, file = "prices_EU_2023_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2022_monthly, file = "prices_EU_2022_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2021_monthly, file = "prices_EU_2021_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2020_monthly, file = "prices_EU_2020_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2019_monthly, file = "prices_EU_2019_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2018_monthly, file = "prices_EU_2018_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2017_monthly, file = "prices_EU_2017_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2016_monthly, file = "prices_EU_2016_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2015_monthly, file = "prices_EU_2015_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2014_monthly, file = "prices_EU_2014_monthly.csv", row.names = FALSE)
write.csv(prices_EU_2013_monthly, file = "prices_EU_2013_monthly.csv", row.names = FALSE)

adj_prices_EU_2023_monthly <- select(prices_EU_2023_monthly, -"close_dkk", -"tot_ret_dkk")

master_sec <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/sec_master.csv")

Combined_prices_EU_montly <- rbind(adj_prices_EU_2023_monthly, prices_EU_2022_monthly, 
                                   prices_EU_2021_monthly, prices_EU_2020_monthly, 
                                   prices_EU_2019_monthly, prices_EU_2018_monthly,
                                   prices_EU_2017_monthly, prices_EU_2016_monthly,
                                   prices_EU_2015_monthly, prices_EU_2014_monthly,
                                   prices_EU_2013_monthly)

final_prices_EU_monthly <- left_join(Combined_prices_EU_montly, master_sec, by = "sec_id")
final_prices_EU_monthly <- final_prices_EU_monthly %>%
  mutate(
    date = as.Date(date, format = "%Y-%m-%d"),  # Adjust format as needed
    date = ceiling_date(date, "month") - days(1)
  )

mktcap_EU_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2023.csv")
mktcap_EU_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2022.csv")
mktcap_EU_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2021.csv")
mktcap_EU_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2020.csv")
mktcap_EU_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2019.csv")
mktcap_EU_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2018.csv")
mktcap_EU_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2017.csv")
mktcap_EU_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2016.csv")
mktcap_EU_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2015.csv")
mktcap_EU_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2014.csv")
mktcap_EU_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_emea_2013.csv")

Combined_mktcap_EU <- rbind(mktcap_EU_2023, mktcap_EU_2022, 
                            mktcap_EU_2021, mktcap_EU_2020, 
                            mktcap_EU_2019, mktcap_EU_2018,
                            mktcap_EU_2017, mktcap_EU_2016,
                            mktcap_EU_2015, mktcap_EU_2014,
                            mktcap_EU_2013)
Combined_mktcap_EU$date <- as.Date(Combined_mktcap_EU$date)
Combined_mktcap_EU <- Combined_mktcap_EU %>%
  mutate(date = ceiling_date(date, "month") - days(1))
final_EU <- left_join(final_prices_EU_monthly, Combined_mktcap_EU, by = c("sec_id", "date"))

esg_EU_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2023.csv")
esg_EU_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2022.csv")
esg_EU_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2021.csv")
esg_EU_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2020.csv")
esg_EU_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2019.csv")
esg_EU_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2018.csv")
esg_EU_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2017.csv")
esg_EU_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2016.csv")
esg_EU_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2015.csv")
esg_EU_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2014.csv")
esg_EU_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_emea_2013.csv")

Combined_esg_EU <- rbind(esg_EU_2023, esg_EU_2022, 
                            esg_EU_2021, esg_EU_2020, 
                            esg_EU_2019, esg_EU_2018,
                            esg_EU_2017, esg_EU_2016,
                            esg_EU_2015, esg_EU_2014,
                            esg_EU_2013)

Combined_esg_EU$date <- as.Date(Combined_esg_EU$date)
Combined_esg_EU <- Combined_esg_EU %>%
  mutate(date = ceiling_date(date, "month") - days(1))
nowfinal_EU <- left_join(final_EU, Combined_esg_EU, by = c("sec_id", "date"))

summary(nowfinal_EU_Filtered)

Final_EU_lessColumns <- select(nowfinal_EU,"sec_id", "date", "close_usd", "tot_ret_usd", "name", "region_name", "sector_1.x", "sector_2.x", "sector_3.x", "sector_4.x", "MSCI.ESG..Weighted.Average.Score.", "MSCI.Environmental.Pillar.Score", "MSCI.Social.Pillar.Score", "MSCI.Governance.Pillar.Score", "Marked.Cap.USD" ,"ROE", "ROA", "BookRatio", "EarningsYield", "EPS.Growth.5Y.Trailing", "beta")

summary(Final_EU_lessColumns)

momentum_EU_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2023.csv")
momentum_EU_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2022.csv")
momentum_EU_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2021.csv")
momentum_EU_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2020.csv")
momentum_EU_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2019.csv")
momentum_EU_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2018.csv")
momentum_EU_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2017.csv")
momentum_EU_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2016.csv")
momentum_EU_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2015.csv")
momentum_EU_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2014.csv")
momentum_EU_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_EMEA_2013.csv")

Combined_momentum_EU <- rbind(momentum_EU_2023, momentum_EU_2022, 
                              momentum_EU_2021, momentum_EU_2020, 
                              momentum_EU_2019, momentum_EU_2018,
                              momentum_EU_2017, momentum_EU_2016,
                              momentum_EU_2015, momentum_EU_2014,
                              momentum_EU_2013)

Combined_momentum_EU_selected <- Combined_momentum_EU %>%
  select(sec_id, date, Momentum.1m, Momentum.12m)
Combined_momentum_EU_selected$date <- as.Date(Combined_momentum_EU_selected$date, format = "%Y-%m-%d")

# Now that 'date' is a Date object, adjust all dates to the last day of their respective month
Combined_momentum_EU_selected <- Combined_momentum_EU_selected %>%
  mutate(date = ceiling_date(date, "month") - days(1))

combined_data <- merge(Final_EU_lessColumns, Combined_momentum_EU_selected, by = c("sec_id", "date"))

unique_sec_id_count <- combined_data %>%
  summarise(Unique_sec_ids = n_distinct(sec_id))

# Print the number of unique 'sec_id'
print(unique_sec_id_count)

write.csv(combined_data, file = "Combined_data_EU.csv", row.names = FALSE)

combined_data <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master thesis - Private/Data/Combined_data_EU.csv")
summary(combined_data)

cleaned_combined_data <- combined_data %>%
  filter(!is.na(MSCI.ESG..Weighted.Average.Score.))

industry_mapping <- data.frame(
  sector_code = c(1010, 1510, 2010, 2020, 2030, 2510, 2520, 2530, 2550, 3010, 3020, 3030, 
                  3510, 3520, 4010, 4020, 4030, 4510, 4520, 4530, 5010, 5020, 5510, 6010, 6020),
  sector_name = c("Energy", "Materials", "Capital Goods", "Commercial & Professional Services",
                  "Transportation", "Automobiles & Components", "Consumer Durables & Apparel",
                  "Consumer Services", "Consumer Discretionary Distribution & Retail",
                  "Consumer Staples Distribution & Retail", "Food, Beverage & Tobacco",
                  "Household & Personal Products", "Health Care Equipment & Services",
                  "Pharmaceuticals, Biotechnology & Life Sciences", "Banks", "Financial Services",
                  "Insurance", "Software & Services", "Technology Hardware & Equipment",
                  "Semiconductors & Semiconductor Equipment", "Telecommunication Services",
                  "Media & Entertainment", "Utilities", "Equity Real Estate Investment Trusts",
                  "Real Estate Management & Development")
)

# Convert the date column to Date type if it's not already
cleaned_combined_data$date <- as.Date(cleaned_combined_data$date, format = "%Y-%m-%d")
cleaned_combined_data <- cleaned_combined_data %>%
  filter(sec_id != 111865, sec_id !=43735)
annual_sec_id_counts <- cleaned_combined_data %>%
  mutate(Year = year(date)) %>%  # Extract the year from the date
  group_by(Year) %>%  # Group data by year
  summarise(Unique_sec_ids = n_distinct(sec_id))  # Count unique sec_id for each year
print(annual_sec_id_counts)
# Filter data for December 31, 2023
data_2023 <- cleaned_combined_data %>%
  filter(date == as.Date("2023-12-31"))

# Merge industry names
data_2023 <- data_2023 %>%
  left_join(industry_mapping, by = c("sector_2.x" = "sector_code"))

industry_summary <- data_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Unique_Securities = n_distinct(sec_id),
    Total_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Market_Share = sum(Marked.Cap.USD, na.rm = TRUE) / sum(data_2023$Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # This ensures no grouping is left over after summarise
  )

industry_table <- industry_summary %>%
  rename(
    Industry = sector_name,
    `Number of Companies` = Unique_Securities,
    `Market Share` = Value_Weighted_Market_Share
  ) %>%
  mutate(`Market Share` = scales::percent(`Market Share`, accuracy = 0.01))  # Convert market share to percentage format
industry_table <- industry_table %>%
  select(-Total_Market_Cap)

write.csv(industry_summary, "industry_summary_2023.csv", row.names = FALSE)



#### ESG split####
esg_percentiles <- cleaned_combined_data %>%
  group_by(date) %>%
  summarise(
    esg_10_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.1, na.rm = TRUE),
    esg_90_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.9, na.rm = TRUE)
  )

cleaned_combined_data <- cleaned_combined_data %>%
  left_join(esg_percentiles, by = "date")

# Print the calculated percentiles
print(esg_percentiles)
cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_ESG_Score = lag(MSCI.ESG..Weighted.Average.Score., 1)) %>%
  ungroup()
cleaned_combined_data <- cleaned_combined_data %>%
  mutate(
    ESG_Category = case_when(
      Lagged_ESG_Score <= esg_10_percentile ~ "Low",
      Lagged_ESG_Score >= esg_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )


esg_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(ESG_Category == "High", na.rm = TRUE),
    Low_Count = sum(ESG_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(esg_category_counts)
summary(cleaned_combined_data)
### E SPLIT ###
e_percentiles <- cleaned_combined_data %>%
  group_by(date) %>%
  summarise(
    e_10_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.1, na.rm = TRUE),
    e_90_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.9, na.rm = TRUE)
  )

cleaned_combined_data <- cleaned_combined_data %>%
  left_join(e_percentiles, by = "date")

cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_E_Score = lag(MSCI.Environmental.Pillar.Score, 1)) %>%
  ungroup()
# Print the calculated percentiles
print(e_percentiles)
cleaned_combined_data <- cleaned_combined_data %>%
  mutate(
    E_Category = case_when(
      Lagged_E_Score <= e_10_percentile ~ "Low",
      Lagged_E_Score >= e_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

e_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(E_Category == "High", na.rm = TRUE),
    Low_Count = sum(E_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(e_category_counts)


### S SPLIT ###
s_percentiles <- cleaned_combined_data %>%
  group_by(date) %>%
  summarise(
    s_10_percentile = quantile(MSCI.Social.Pillar.Score, 0.1, na.rm = TRUE),
    s_90_percentile = quantile(MSCI.Social.Pillar.Score, 0.9, na.rm = TRUE)
  )
print(s_percentiles)
cleaned_combined_data <- cleaned_combined_data %>%
  left_join(s_percentiles, by = "date")

cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_S_Score = lag(MSCI.Social.Pillar.Score, 1)) %>%
  ungroup()

cleaned_combined_data <- cleaned_combined_data %>%
  mutate(
    S_Category = case_when(
      Lagged_S_Score <= s_10_percentile ~ "Low",
      Lagged_S_Score >= s_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

s_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(S_Category == "High", na.rm = TRUE),
    Low_Count = sum(S_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(s_category_counts)

### G SPLIT ###
g_percentiles <- cleaned_combined_data %>%
  group_by(date) %>%
  summarise(
    g_10_percentile = quantile(MSCI.Governance.Pillar.Score, 0.1, na.rm = TRUE),
    g_90_percentile = quantile(MSCI.Governance.Pillar.Score, 0.9, na.rm = TRUE)
  )
print(g_percentiles)
cleaned_combined_data <- cleaned_combined_data %>%
  left_join(g_percentiles, by = "date")

cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_G_Score = lag(MSCI.Governance.Pillar.Score, 1)) %>%
  ungroup()

cleaned_combined_data <- cleaned_combined_data %>%
  mutate(
    G_Category = case_when(
      Lagged_G_Score <= g_10_percentile ~ "Low",
      Lagged_G_Score >= g_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

g_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(G_Category == "High", na.rm = TRUE),
    Low_Count = sum(G_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(g_category_counts)


cleaned_combined_data <- cleaned_combined_data %>%
  left_join(industry_mapping, by = c("sector_2.x" = "sector_code"))
high_esg_data <- cleaned_combined_data %>%
  filter(ESG_Category == "High")

low_esg_data <- cleaned_combined_data %>%
  filter(ESG_Category == "Low")

# Calculate market capitalization by industry, date, and ESG category
calculate_market_shares <- function(data) {
  data %>%
    group_by(sector_name, date) %>%
    summarise(Total_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE), .groups = 'drop') %>%
    left_join(data %>%
                group_by(date) %>%
                summarise(Total_Market_Cap_All = sum(Marked.Cap.USD, na.rm = TRUE), .groups = 'drop'), by = "date") %>%
    mutate(Market_Share = Total_Market_Cap / Total_Market_Cap_All)
}

# Apply the function to both High and Low ESG data
high_esg_market_shares <- calculate_market_shares(high_esg_data)
low_esg_market_shares <- calculate_market_shares(low_esg_data)


high_esg_data_dec <- high_esg_market_shares %>%
  filter(date == as.Date("2023-12-31"))

low_esg_data_dec <- low_esg_market_shares %>%
  filter(date == as.Date("2023-12-31"))

# Function to create a treemap for given data
create_treemap <- function(data, esg_category) {
  treemap(data,
          index = "sector_name",  # Industry names as labels
          vSize = "Total_Market_Cap",  # Total market cap as the size of the blocks
          vColor = "Market_Share",  # Market share as the color scale
          title = paste("Market Share by Industry for", esg_category, "ESG Portfolio"),
          palette = "Spectral",  # Color palette
          border.col = "white",  # Border color
          fontcolor.labels = "black",  # Font color for labels
          is.colored = TRUE,  # Enable coloring
          draw.labels = TRUE,  # Enable labels
          fontsize.labels = 12,  # Font size for labels
          fontface.labels = 2)  # Font face for labels
}

# Visualize High ESG market shares for December 2023
create_treemap(high_esg_data_dec, "High")

# Visualize Low ESG market shares for December 2023
create_treemap(low_esg_data_dec, "Low")

december_2023_data <- cleaned_combined_data %>%
  filter(date == as.Date("2023-12-31"))

total_market_cap_dec_2023 <- sum(december_2023_data$Marked.Cap.USD, na.rm = TRUE)

december_2023_data <- december_2023_data %>%
  mutate(Weight = Marked.Cap.USD / total_market_cap_dec_2023)


### FACTORS ###
cleaned_combined_data <- cleaned_combined_data %>%
  group_by(date) %>%
  mutate(  
    Size_Category = case_when(
      Marked.Cap.USD <= quantile(Marked.Cap.USD, 0.3, na.rm = TRUE) ~ "Small",
      Marked.Cap.USD <= quantile(Marked.Cap.USD, 0.7, na.rm = TRUE) ~ "Neutral",
      TRUE ~ "Big"
    ),
    Profitability_Category = case_when(
      ROA <= quantile(ROA, 0.3, na.rm = TRUE) ~ "Weak",
      ROA <= quantile(ROA, 0.7, na.rm = TRUE) ~ "Neutral",
      TRUE ~ "Robust"
    ),
    Value_Category = case_when(
      BookRatio <= quantile(BookRatio, 0.3, na.rm = TRUE) ~ "Growth",
      BookRatio <= quantile(BookRatio, 0.7, na.rm = TRUE) ~ "Neutral",
      TRUE ~ "Value"
  )) %>%
  ungroup()

profit_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(Profitability_Category == "Robust", na.rm = TRUE),
    Low_Count = sum(Profitability_Category == "Weak", na.rm = TRUE)
  )

# Print the results
print(profit_category_counts)


Size_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(Size_Category == "Big", na.rm = TRUE),
    Low_Count = sum(Size_Category == "Small", na.rm = TRUE)
  )

# Print the results
print(Size_category_counts)

Value_category_counts <- cleaned_combined_data %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(Value_Category == "Growth", na.rm = TRUE),
    Low_Count = sum(Value_Category == "Value", na.rm = TRUE)
  )

# Print the results
print(Value_category_counts)

cleaned_combined_data <- cleaned_combined_data %>%
  group_by(date) %>%
  mutate(  
    Mom_Category = case_when(
      Momentum.12m <= quantile(Momentum.12m, 0.3, na.rm = TRUE) ~ "Low",
      Momentum.12m > quantile(Momentum.12m, 0.7, na.rm = TRUE) ~ "High",
      TRUE ~ NA_character_
    )) %>%
  ungroup()

### Calculating the factors ###
riskfree <- Risikofrie.rente

# Assuming riskfree data is initially in 'Risikofrie.rente' with dates as "dd.mm.yyyy"
riskfree <- Risikofrie.rente %>%
  mutate(DATE = dmy(DATE),  # Convert from "dd.mm.yyyy" to Date object
         DATE = ceiling_date(DATE, "month") - days(1))  # Adjust to the last day of the month
riskfree <- riskfree %>%
  mutate(
    TB4WK = as.numeric(gsub(",", ".", TB4WK))  # Replace comma with dot and convert to numeric
  )
# Ensure the riskfree dataset's DATE is formatted as "YYYY-MM-DD" if necessary
riskfree <- riskfree %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))

riskfree <- riskfree %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    Monthly_Rate = ((1 + TB4WK)^(1/12) - 1)/100
  )

cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%
  group_by(sec_id) %>%
  mutate(Return = (tot_ret_usd - lag(tot_ret_usd)) / lag(tot_ret_usd))

cleaned_combined_data <- cleaned_combined_data %>%
  filter(Return <= 2)
cleaned_combined_data <- cleaned_combined_data %>%
  mutate(Weighted_Return = Return * Marked.Cap.USD)

# Summarize to get the value-weighted return by date
value_weighted_returns_by_date <- cleaned_combined_data %>%
  group_by(date) %>%
  summarise(
    Total_Weighted_Return = sum(Weighted_Return, na.rm = TRUE),
    Total_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Market_Cap,
    .groups = 'drop'  # Ensures the data is no longer grouped after summarisation
  )


value_weighted_returns_by_date <- value_weighted_returns_by_date %>%
  left_join(riskfree, by = c("date" = "DATE"))

value_weighted_returns_by_date <- value_weighted_returns_by_date %>%
  mutate(excess_ret = Value_Weighted_Return - Monthly_Rate)

portfolio_returns_smb <- cleaned_combined_data %>%
  group_by(date, Size_Category, Value_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop')

smb <- portfolio_returns_smb %>%
  group_by(date) %>%
  summarise(
    Small_Value = sum(Average_Return[Size_Category == "Small" & Value_Category == "Value"], na.rm = TRUE),
    Small_Neutral = sum(Average_Return[Size_Category == "Small" & Value_Category == "Neutral"], na.rm = TRUE),
    Small_Growth = sum(Average_Return[Size_Category == "Small" & Value_Category == "Growth"], na.rm = TRUE),
    Big_Value = sum(Average_Return[Size_Category == "Big" & Value_Category == "Value"], na.rm = TRUE),
    Big_Neutral = sum(Average_Return[Size_Category == "Big" & Value_Category == "Neutral"], na.rm = TRUE),
    Big_Growth = sum(Average_Return[Size_Category == "Big" & Value_Category == "Growth"], na.rm = TRUE),
    SMB = (1/3) * (Small_Value + Small_Neutral + Small_Growth) - (1/3) * (Big_Value + Big_Neutral + Big_Growth)
  )

smb <- smb %>%
  filter(date != as.Date("2014-01-31"))

factor_combined <- smb  %>%
  select(date, SMB)

factor_combined <- factor_combined %>%
  left_join(value_weighted_returns_by_date %>% select(date, excess_ret), by = "date")

portfolio_returns_HML <- cleaned_combined_data %>%
  group_by(date, Size_Category, Value_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(Value_Category %in% c("Value", "Growth"))

hml <- portfolio_returns_HML %>%
  group_by(date) %>%
  summarise(
    Small_Value = sum(Average_Return[Size_Category == "Small" & Value_Category == "Value"], na.rm = TRUE),
    Big_Value = sum(Average_Return[Size_Category == "Big" & Value_Category == "Value"], na.rm = TRUE),
    Small_Growth = sum(Average_Return[Size_Category == "Small" & Value_Category == "Growth"], na.rm = TRUE),
    Big_Growth = sum(Average_Return[Size_Category == "Big" & Value_Category == "Growth"], na.rm = TRUE),
    HML = (1/2) * (Small_Value + Big_Value) - (1/2) * (Small_Growth + Big_Growth)
  )

hml <- hml %>%
  filter(date != as.Date("2014-01-31"))

factor_combined <- left_join(factor_combined, hml, by = "date")
factor_combined <- factor_combined %>%
  select(-Small_Value, -Small_Growth, -Big_Value, -Big_Growth)

portfolio_returns_RMW <- cleaned_combined_data %>%
  group_by(date, Size_Category, Profitability_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(Profitability_Category %in% c("Robust", "Weak"))

rmw <- portfolio_returns_RMW %>%
  group_by(date) %>%
  summarise(
    Small_Robust = sum(Average_Return[Size_Category == "Small" & Profitability_Category == "Robust"], na.rm = TRUE),
    Big_Robust = sum(Average_Return[Size_Category == "Big" & Profitability_Category == "Robust"], na.rm = TRUE),
    Small_Weak = sum(Average_Return[Size_Category == "Small" & Profitability_Category == "Weak"], na.rm = TRUE),
    Big_Weak = sum(Average_Return[Size_Category == "Big" & Profitability_Category == "Weak"], na.rm = TRUE),
    RMW = (1/2) * (Small_Robust + Big_Robust) - (1/2) * (Small_Weak + Big_Weak)
  )

rmw <- rmw %>%
  filter(date != as.Date("2014-01-31"))

factor_combined <- left_join(factor_combined, rmw, by = "date")
factor_combined <- factor_combined %>%
  select(-Small_Robust, -Small_Weak, -Big_Robust, -Big_Weak)

portfolio_returns_Mom <- cleaned_combined_data %>%
  group_by(date, Size_Category, Mom_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(Mom_Category %in% c("High", "Low"))

mom <- portfolio_returns_Mom %>%
  group_by(date) %>%
  summarise(
    Small_High = sum(Average_Return[Size_Category == "Small" & Mom_Category == "High"], na.rm = TRUE),
    Big_High = sum(Average_Return[Size_Category == "Big" & Mom_Category == "High"], na.rm = TRUE),
    Small_Low = sum(Average_Return[Size_Category == "Small" & Mom_Category == "Low"], na.rm = TRUE),
    Big_Low = sum(Average_Return[Size_Category == "Big" & Mom_Category == "Low"], na.rm = TRUE),
    MOM = (1/2) * (Small_High + Big_High) - (1/2) * (Small_Low + Big_Low)
  )

mom <- mom %>%
  filter(date != as.Date("2014-01-31"))

factor_combined <- left_join(factor_combined, mom, by = "date")
factor_combined <- factor_combined %>%
  select(-Small_High, -Big_High, -Small_Low, -Big_Low)


value_weighted_returns_ESG <- cleaned_combined_data %>%
  group_by(date, ESG_Category) %>%  # Grouping by both date and ESG category
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),  # Summing weighted returns, ignoring NA
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,  # Calculating value-weighted return
    .groups = 'drop'  # Prevents the result from being grouped
  )

value_weighted_returns_E <- cleaned_combined_data %>%
  group_by(date, E_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for S Category
value_weighted_returns_S <- cleaned_combined_data %>%
  group_by(date, S_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for G Category
value_weighted_returns_G <- cleaned_combined_data %>%
  group_by(date, G_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

high_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "High")

# Step 3: Merge the filtered ESG data with the factors data on Date
factor_combined <- left_join(factor_combined, high_esg_data %>% 
                            select(date, ESG_High = Value_Weighted_Return), 
                          by = "date")

low_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "Low")

factor_combined <- left_join(factor_combined, low_esg_data %>% 
                            select(date, ESG_Low = Value_Weighted_Return), 
                          by = "date")

high_e_data <- filter(value_weighted_returns_E, E_Category == "High")

factor_combined <- left_join(factor_combined, high_e_data %>% 
                            select(date, E_High = Value_Weighted_Return), 
                          by = "date")

low_e_data <- filter(value_weighted_returns_E, E_Category == "Low")

factor_combined <- left_join(factor_combined, low_e_data %>% 
                            select(date, E_Low = Value_Weighted_Return), 
                          by = "date")

high_s_data <- filter(value_weighted_returns_S, S_Category == "High")

factor_combined <- left_join(factor_combined, high_s_data %>% 
                            select(date, S_High = Value_Weighted_Return), 
                          by = "date")

low_s_data <- filter(value_weighted_returns_S, S_Category == "Low")

factor_combined <- left_join(factor_combined, low_s_data %>% 
                            select(date, S_Low = Value_Weighted_Return), 
                          by = "date")

high_g_data <- filter(value_weighted_returns_G, G_Category == "High")

factor_combined <- left_join(factor_combined, high_g_data %>% 
                            select(date, G_High = Value_Weighted_Return), 
                          by = "date")

low_g_data <- filter(value_weighted_returns_G, G_Category == "Low")

factor_combined <- left_join(factor_combined, low_g_data %>% 
                            select(date, G_Low = Value_Weighted_Return), 
                          by = "date")

factor_combined <- factor_combined %>%
  mutate(ESG = ESG_High - ESG_Low)

factor_combined <- factor_combined %>%
  mutate(E = E_High - E_Low)

factor_combined <- factor_combined %>%
  mutate(S = S_High - S_Low)

factor_combined <- factor_combined %>%
  mutate(G = G_High - G_Low)

factor_combined <- factor_combined %>%
  select(-ESG_High, -ESG_Low, -E_High, -E_Low, -S_High, -S_Low, -G_High, -G_Low)

factor_combined <- factor_combined %>%
  left_join(select(value_weighted_returns_by_date, date, Monthly_Rate), by = "date")
### Test of assumptions ###
correlation_data <- select(factor_combined, excess_ret, SMB, HML, RMW, MOM)

correlation_matrix <- cor(correlation_data)

print(correlation_matrix)

modelESG <- lm(ESG ~  excess_ret + SMB + HML + RMW + MOM, data = factor_combined)
summary(modelESG)

modelE <- lm(E ~ excess_ret + SMB + HML + RMW + MOM, data = factor_combined)
summary(modelE)

modelS <- lm(S ~ excess_ret + SMB + HML + RMW + MOM, data = factor_combined)
summary(modelS)

modelG <- lm(G ~ excess_ret + SMB + HML + RMW + MOM, data = factor_combined)
summary(modelG)

portfolio_columns <- c("ESG", "E", "S", "G", "excess_ret")

portfolio_stats <- data.frame()
library(moments)
for (col in portfolio_columns) {
  temp_stats <- factor_combined %>%
    summarise(
      Mean = mean(get(col), na.rm = TRUE),
      Median = median(get(col), na.rm = TRUE),
      SD = sd(get(col), na.rm = TRUE),
      Sharpe = mean(get(col), na.rm = TRUE) / sd(get(col), na.rm = TRUE),
      Kurtosis = kurtosis(get(col), na.rm = TRUE),
      Skewness = skewness(get(col), na.rm = TRUE)
    ) %>%
    mutate(Portfolio = col)
  
  portfolio_stats <- rbind(portfolio_stats, temp_stats)
}

# View the calculated statistics
print(portfolio_stats)


library(car)

# Calculate VIF
vif_values <- vif(modelESG)
print(vif_values)

library(lmtest)
bptest_result <- bptest(modelESG)

# Print the test results
print(bptest_result)

acf(residuals, main = "ACF of Residuals", lag.max = 30)
acf(residuals(modelESG))

dw_result <- dwtest(modelESG)

# Print the results
print(dw_result)

bg_result <- bgtest(modelESG)
print(bg_result)

residuals_data <- data.frame(
  Residuals = residuals(modelESG),
  Fitted = fitted.values(modelESG),
  MarketFactor = factor_combined$excess_ret,
  SMB = factor_combined$SMB,
  HML = factor_combined$HML,
  RMW = factor_combined$RMW,
  MOM = factor_combined$MOM
)



library(ggplot2)
library(patchwork)

# Plot ESG_High vs Market Factor
plot1 <- ggplot(factor_combined, aes(x = excess_ret, y = ESG)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG vs Market Factor", x = "Market Factor", y = "ESG Returns") +
  theme_minimal()

# Plot ESG_High vs SMB
plot2 <- ggplot(factor_combined, aes(x = SMB, y = ESG)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG vs SMB", x = "SMB", y = "ESG Returns") +
  theme_minimal()

# Plot ESG_High vs HML
plot3 <- ggplot(factor_combined, aes(x = HML, y = ESG)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG vs HML", x = "HML", y = "ESG Returns") +
  theme_minimal()

# Plot ESG_High vs RMW
plot4 <- ggplot(factor_combined, aes(x = RMW, y = ESG)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG vs RMW", x = "RMW", y = "ESG Returns") +
  theme_minimal()

# Plot ESG_High vs MOM
plot5 <- ggplot(factor_combined, aes(x = MOM, y = ESG)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG vs MOM", x = "MOM", y = "ESG Returns") +
  theme_minimal()

combined_plots <- plot1 + plot2 + plot3 + plot4 + plot5 +
  plot_layout(nrow = 3, ncol = 2)

# Print the combined plot
print(combined_plots)

# Market Factor vs Residuals
# Plot Residuals vs. Market Factor
plot1 <- ggplot(residuals_data, aes(x = MarketFactor, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Market Factor", x = "Market Factor", y = "Residuals") +
  theme_minimal()

# Plot Residuals vs. SMB
plot2 <- ggplot(residuals_data, aes(x = SMB, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. SMB", x = "SMB", y = "Residuals") +
  theme_minimal()

# Plot Residuals vs. HML
plot3 <- ggplot(residuals_data, aes(x = HML, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. HML", x = "HML", y = "Residuals") +
  theme_minimal()

# Plot Residuals vs. RMW
plot4 <- ggplot(residuals_data, aes(x = RMW, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. RMW", x = "RMW", y = "Residuals") +
  theme_minimal()

# Plot Residuals vs. MOM
plot5 <- ggplot(residuals_data, aes(x = MOM, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. MOM", x = "MOM", y = "Residuals") +
  theme_minimal()

combined_residual_plots <- plot1 + plot2 + plot3 + plot4 + plot5 +
  plot_layout(nrow = 3, ncol = 2)

# Print the combined plot
print(combined_residual_plots)

residuals <- residuals(modelESG)

# Calculate the mean of residuals
mean_residuals <- mean(residuals, na.rm = TRUE) 
print(mean_residuals)

residuals_data <- data.frame(
  Fitted = fitted(modelESG),
  Residuals = residuals(modelESG)
)

ggplot(residuals_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +  # Semi-transparent points to handle overplotting
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Add a horizontal line at zero
  labs(
    title = "Residuals vs Fitted",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal() +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title
plot(fitted(modelESG), residuals(modelESG), main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

bptest_result <- bptest(modelESG)
print(bptest_result)

acf(residuals, main = "ACF of Residuals", lag.max = 30)
acf(residuals(modelESG))

residuals <- residuals(modelESG)

# Increase margins to prevent labels from being cut off
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust margins (bottom, left, top, right)

# Create ACF plot with increased number of lags
acf(residuals, main = "ACF of Residuals", lag.max = 30, col = "blue")

dw_result <- dwtest(modelESG)

# Print the results
print(dw_result)

ggplot(factor_combined, aes(x = excess_ret, y = ESG)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG vs Market Factor", x = "Market Factor", y = "ESG High Returns") +
  theme_minimal()

ggplot(factor_combined, aes(x = SMB, y = ESG)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG vs SMB", x = "SMB", y = "ESG Returns") +
  theme_minimal()

ggplot(factor_combined, aes(x = HML, y = ESG)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG vs HML", x = "HML", y = "ESG Returns") +
  theme_minimal()

ggplot(factor_combined, aes(x = RMW, y = ESG)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG vs RMW", x = "RMW", y = "ESG Returns") +
  theme_minimal()

ggplot(factor_combined, aes(x = MOM, y = ESG)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG vs MOM", x = "MOM", y = "ESG Returns") +
  theme_minimal()

### Industry specific (ENERGY) ###
industry_group_Energy <- cleaned_combined_data %>%
  filter(sector_2.x %in% c(1010))
industry_group_Energy <- industry_group_Energy %>%
  select(-esg_10_percentile.x, -esg_90_percentile.x, -esg_10_percentile.y, -esg_90_percentile.y)

esg_percentiles_industry <- industry_group_Energy %>%
  group_by(date) %>%
  summarise(
    esg_10_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.2, na.rm = TRUE),
    esg_90_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.8, na.rm = TRUE)
  )

industry_group_Energy <- industry_group_Energy %>%
  left_join(esg_percentiles_industry, by = "date")

industry_group_Energy <- industry_group_Energy %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_ESG_Score = lag(MSCI.ESG..Weighted.Average.Score., 1)) %>%
  ungroup()
industry_group_Energy <- industry_group_Energy %>%
  mutate(
    ESG_Category = case_when(
      Lagged_ESG_Score <= esg_10_percentile ~ "Low",
      Lagged_ESG_Score >= esg_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

esg_category_counts <- industry_group_Energy %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(ESG_Category == "High", na.rm = TRUE),
    Low_Count = sum(ESG_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(esg_category_counts)

industry_group_Energy <- industry_group_Energy %>%
  select(-e_10_percentile.x, -e_90_percentile.x, -e_10_percentile.y, -e_90_percentile.y)

e_percentiles_industry <- industry_group_Energy %>%
  group_by(date) %>%
  summarise(
    e_10_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.2, na.rm = TRUE),
    e_90_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.8, na.rm = TRUE)
  )

industry_group_Energy <- industry_group_Energy %>%
  left_join(e_percentiles_industry, by = "date")

industry_group_Energy <- industry_group_Energy %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_E_Score = lag(MSCI.Environmental.Pillar.Score, 1)) %>%
  ungroup()
industry_group_Energy <- industry_group_Energy %>%
  mutate(
    E_Category = case_when(
      Lagged_E_Score <= e_10_percentile ~ "Low",
      Lagged_E_Score >= e_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

e_category_counts <- industry_group_Energy %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(E_Category == "High", na.rm = TRUE),
    Low_Count = sum(E_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(e_category_counts)
summary(industry_group_Energy)

industry_group_Energy <- industry_group_Energy %>%
  select(-s_10_percentile.x, -s_90_percentile.x, -s_10_percentile.y, -s_90_percentile.y)


s_percentiles_industry <- industry_group_Energy %>%
  group_by(date) %>%
  summarise(
    s_10_percentile = quantile(MSCI.Social.Pillar.Score, 0.2, na.rm = TRUE),
    s_90_percentile = quantile(MSCI.Social.Pillar.Score, 0.8, na.rm = TRUE)
  )

industry_group_Energy <- industry_group_Energy %>%
  left_join(s_percentiles_industry, by = "date")

industry_group_Energy <- industry_group_Energy %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_S_Score = lag(MSCI.Social.Pillar.Score, 1)) %>%
  ungroup()
industry_group_Energy <- industry_group_Energy %>%
  mutate(
    S_Category = case_when(
      Lagged_S_Score <= s_10_percentile ~ "Low",
      Lagged_S_Score >= s_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

s_category_counts <- industry_group_Energy %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(S_Category == "High", na.rm = TRUE),
    Low_Count = sum(S_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(s_category_counts)
industry_group_Energy <- industry_group_Energy %>%
  select(-g_10_percentile, -g_90_percentile)

g_percentiles_industry <- industry_group_Energy %>%
  group_by(date) %>%
  summarise(
    g_10_percentile = quantile(MSCI.Governance.Pillar.Score, 0.2, na.rm = TRUE),
    g_90_percentile = quantile(MSCI.Governance.Pillar.Score, 0.8, na.rm = TRUE)
  )

industry_group_Energy <- industry_group_Energy %>%
  left_join(g_percentiles_industry, by = "date")

industry_group_Energy <- industry_group_Energy %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_G_Score = lag(MSCI.Governance.Pillar.Score, 1)) %>%
  ungroup()
industry_group_Energy <- industry_group_Energy %>%
  mutate(
    G_Category = case_when(
      Lagged_G_Score <= g_10_percentile ~ "Low",
      Lagged_G_Score >= g_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )
industry_group_Energy_filt <- industry_group_Energy %>%
  filter(date == as.Date("2023-12-31"))
g_category_counts <- industry_group_Energy_filt %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(G_Category == "High", na.rm = TRUE),
    Low_Count = sum(G_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(g_category_counts)

value_weighted_returns_ESG <- industry_group_Energy %>%
  group_by(date, ESG_Category) %>%  # Grouping by both date and ESG category
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),  # Summing weighted returns, ignoring NA
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,  # Calculating value-weighted return
    .groups = 'drop'  # Prevents the result from being grouped
  )

value_weighted_returns_E <- industry_group_Energy %>%
  group_by(date, E_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for S Category
value_weighted_returns_S <- industry_group_Energy %>%
  group_by(date, S_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for G Category
value_weighted_returns_G <- industry_group_Energy %>%
  group_by(date, G_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

high_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "High")

# Step 3: Merge the filtered ESG data with the factors data on Date
industry_group_Energy <- left_join(industry_group_Energy, high_esg_data %>% 
                               select(date, ESG_High = Value_Weighted_Return), 
                             by = "date")

low_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "Low")

industry_group_Energy <- left_join(industry_group_Energy, low_esg_data %>% 
                               select(date, ESG_Low = Value_Weighted_Return), 
                             by = "date")

high_e_data <- filter(value_weighted_returns_E, E_Category == "High")

industry_group_Energy <- left_join(industry_group_Energy, high_e_data %>% 
                               select(date, E_High = Value_Weighted_Return), 
                             by = "date")

low_e_data <- filter(value_weighted_returns_E, E_Category == "Low")

industry_group_Energy <- left_join(industry_group_Energy, low_e_data %>% 
                               select(date, E_Low = Value_Weighted_Return), 
                             by = "date")

high_s_data <- filter(value_weighted_returns_S, S_Category == "High")

industry_group_Energy <- left_join(industry_group_Energy, high_s_data %>% 
                               select(date, S_High = Value_Weighted_Return), 
                             by = "date")

low_s_data <- filter(value_weighted_returns_S, S_Category == "Low")

industry_group_Energy <- left_join(industry_group_Energy, low_s_data %>% 
                               select(date, S_Low = Value_Weighted_Return), 
                             by = "date")

high_g_data <- filter(value_weighted_returns_G, G_Category == "High")

industry_group_Energy <- left_join(industry_group_Energy, high_g_data %>% 
                               select(date, G_High = Value_Weighted_Return), 
                             by = "date")

low_g_data <- filter(value_weighted_returns_G, G_Category == "Low")

industry_group_Energy <- left_join(industry_group_Energy, low_g_data %>% 
                               select(date, G_Low = Value_Weighted_Return), 
                             by = "date")

industry_group_Energy <- industry_group_Energy %>%
  mutate(ESG = ESG_High - ESG_Low)

industry_group_Energy <- industry_group_Energy %>%
  mutate(E = E_High - E_Low)

industry_group_Energy <- industry_group_Energy %>%
  mutate(S = S_High - S_Low)

industry_group_Energy <- industry_group_Energy %>%
  mutate(G = G_High - G_Low)

industry_group_Energy <- industry_group_Energy %>%
  select(-ESG_High, -ESG_Low, -E_High, -E_Low, -S_High, -S_Low, -G_High, -G_Low)

industry_factor <- industry_group_Energy %>%
  select(date, ESG, E, S, G)  # Select only the specified columns

industry_factor <- industry_factor %>%
  left_join(factor_combined %>% select(date, excess_ret, SMB, HML, RMW, MOM), by = "date")
industry_factor <- industry_factor %>%
  distinct()

industry_factor <- industry_factor %>%
  filter(date != as.Date("2014-01-31"))
modelESG <- lm(ESG ~  excess_ret + SMB + HML + RMW + MOM, data = industry_factor)
summary(modelESG)

modelE <- lm(E ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor)
summary(modelE)

modelS <- lm(S ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor)
summary(modelS)

modelG <- lm(G ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor)
summary(modelG)

portfolio_stats_energy <- data.frame()

for (col in portfolio_columns) {
  temp_stats_energy <- industry_factor %>%
    summarise(
      Mean = mean(get(col), na.rm = TRUE),
      Median = median(get(col), na.rm = TRUE),
      SD = sd(get(col), na.rm = TRUE),
      Sharpe = mean(get(col), na.rm = TRUE) / sd(get(col), na.rm = TRUE),
      Kurtosis = kurtosis(get(col), na.rm = TRUE),
      Skewness = skewness(get(col), na.rm = TRUE)
    ) %>%
    mutate(Portfolio = col)
  
  portfolio_stats_energy <- rbind(portfolio_stats_energy, temp_stats_energy)
}

# View the calculated statistics
print(portfolio_stats_energy)

# Histogram for the 'ESG' column
ggplot(industry_factor, aes(x = ESG)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  ggtitle("Histogram of ESG Scores") +
  xlab("ESG Scores") +
  ylab("Frequency")

# Histogram for the 'E' column
ggplot(industry_factor, aes(x = E)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  ggtitle("Histogram of E Scores") +
  xlab("E Scores") +
  ylab("Frequency")

# Histogram for the 'S' column
ggplot(industry_factor, aes(x = S)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  ggtitle("Histogram of S Scores") +
  xlab("S Scores") +
  ylab("Frequency")

# Histogram for the 'G' column
ggplot(industry_factor, aes(x = G)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black") +
  ggtitle("Histogram of G Scores") +
  xlab("G Scores") +
  ylab("Frequency")


### Industry specific (Software & Services) ###
industry_group_software <- cleaned_combined_data %>%
  filter(sector_2.x %in% c(4510))

industry_group_software <- industry_group_software %>%
  select(-esg_10_percentile, -esg_90_percentile)

esg_percentiles_software <- industry_group_software %>%
  group_by(date) %>%
  summarise(
    esg_10_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.2, na.rm = TRUE),
    esg_90_percentile = quantile(MSCI.ESG..Weighted.Average.Score., 0.8, na.rm = TRUE)
  )

industry_group_software <- industry_group_software %>%
  left_join(esg_percentiles_software, by = "date")

industry_group_software <- industry_group_software %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_ESG_Score = lag(MSCI.ESG..Weighted.Average.Score., 1)) %>%
  ungroup()
industry_group_software <- industry_group_software %>%
  mutate(
    ESG_Category = case_when(
      Lagged_ESG_Score <= esg_10_percentile ~ "Low",
      Lagged_ESG_Score >= esg_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

esg_category_counts <- industry_group_software %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(ESG_Category == "High", na.rm = TRUE),
    Low_Count = sum(ESG_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(esg_category_counts)
industry_group_software <- industry_group_software %>%
  select(-e_10_percentile.x, -e_90_percentile.x, -e_10_percentile.y, -e_90_percentile.y)


e_percentiles_software <- industry_group_software %>%
  group_by(date) %>%
  summarise(
    e_10_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.2, na.rm = TRUE),
    e_90_percentile = quantile(MSCI.Environmental.Pillar.Score, 0.8, na.rm = TRUE),
    .groups = 'drop'  # Drop grouping for future operations
  )

# Ensure you are joining the correct dataframe
industry_group_software <- industry_group_software %>%
  left_join(e_percentiles_software, by = "date")

# Arrange and create a lagged score column
industry_group_software <- industry_group_software %>%
  arrange(sec_id, date) %>%
  group_by(sec_id) %>%
  mutate(Lagged_E_Score = lag(MSCI.Environmental.Pillar.Score, 1)) %>%
  ungroup()

# Categorize based on lagged scores and percentiles
industry_group_software <- industry_group_software %>%
  mutate(
    E_Category = case_when(
      Lagged_E_Score <= e_10_percentile ~ "Low",
      Lagged_E_Score >= e_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

# Filter for a specific date and count the categories
industry_group_software_filt <- industry_group_software %>%
  filter(date == as.Date("2023-10-31"))

e_category_counts <- industry_group_software %>%
  summarise(
    High_Count = sum(E_Category == "High", na.rm = TRUE),
    Low_Count = sum(E_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(e_category_counts)

industry_group_software <- industry_group_software %>%
  select(-s_10_percentile, -s_90_percentile)

s_percentiles_industry <- industry_group_software %>%
  group_by(date) %>%
  summarise(
    s_10_percentile = quantile(MSCI.Social.Pillar.Score, 0.2, na.rm = TRUE),
    s_90_percentile = quantile(MSCI.Social.Pillar.Score, 0.8, na.rm = TRUE)
  )

industry_group_software <- industry_group_software %>%
  left_join(s_percentiles_industry, by = "date")

industry_group_software <- industry_group_software %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_S_Score = lag(MSCI.Social.Pillar.Score, 1)) %>%
  ungroup()
industry_group_software <- industry_group_software %>%
  mutate(
    S_Category = case_when(
      Lagged_S_Score <= s_10_percentile ~ "Low",
      Lagged_S_Score >= s_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

s_category_counts <- industry_group_software %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(S_Category == "High", na.rm = TRUE),
    Low_Count = sum(S_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(s_category_counts)

industry_group_software <- industry_group_software %>%
  select(-g_10_percentile, -g_90_percentile)

g_percentiles_industry <- industry_group_software %>%
  group_by(date) %>%
  summarise(
    g_10_percentile = quantile(MSCI.Governance.Pillar.Score, 0.2, na.rm = TRUE),
    g_90_percentile = quantile(MSCI.Governance.Pillar.Score, 0.8, na.rm = TRUE)
  )

industry_group_software <- industry_group_software %>%
  left_join(g_percentiles_industry, by = "date")

industry_group_software <- industry_group_software %>%
  arrange(sec_id, date) %>%  # Ensure data is in order
  group_by(sec_id) %>%
  mutate(Lagged_G_Score = lag(MSCI.Governance.Pillar.Score, 1)) %>%
  ungroup()
industry_group_software <- industry_group_software %>%
  mutate(
    G_Category = case_when(
      Lagged_G_Score <= g_10_percentile ~ "Low",
      Lagged_G_Score >= g_90_percentile ~ "High",
      TRUE ~ "Neutral"
    )
  )

industry_group_software_filt <- industry_group_software %>%
  filter(date == as.Date("2016-12-31"))
g_category_counts <- industry_group_software_filt %>%
  ungroup() %>%  # Ensure no prior grouping influences the result
  summarise(
    High_Count = sum(G_Category == "High", na.rm = TRUE),
    Low_Count = sum(G_Category == "Low", na.rm = TRUE)
  )

# Print the results
print(g_category_counts)

ESG_software_return <- industry_group_software %>%
  group_by(date, ESG_Category) %>%  # Grouping by both date and ESG category
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),  # Summing weighted returns, ignoring NA
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,  # Calculating value-weighted return
    .groups = 'drop'  # Prevents the result from being grouped
  )

E_software_return <- industry_group_software %>%
  group_by(date, E_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for S Category
S_software_return <- industry_group_software %>%
  group_by(date, S_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for G Category
G_software_return <- industry_group_software %>%
  group_by(date, G_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

high_esg_software <- filter(ESG_software_return, ESG_Category == "High")

# Step 3: Merge the filtered ESG data with the factors data on Date
industry_group_software <- left_join(industry_group_software, high_esg_software %>% 
                                     select(date, ESG_High = Value_Weighted_Return), 
                                   by = "date")

low_esg_software <- filter(ESG_software_return, ESG_Category == "Low")

industry_group_software <- left_join(industry_group_software, low_esg_software %>% 
                                     select(date, ESG_Low = Value_Weighted_Return), 
                                   by = "date")

high_e_software <- filter(E_software_return, E_Category == "High")

industry_group_software <- left_join(industry_group_software, high_e_software %>% 
                                     select(date, E_High = Value_Weighted_Return), 
                                   by = "date")

low_e_software <- filter(E_software_return, E_Category == "Low")

industry_group_software <- left_join(industry_group_software, low_e_software %>% 
                                     select(date, E_Low = Value_Weighted_Return), 
                                   by = "date")

high_s_software <- filter(S_software_return, S_Category == "High")

industry_group_software <- left_join(industry_group_software, high_s_software %>% 
                                     select(date, S_High = Value_Weighted_Return), 
                                   by = "date")

low_s_software <- filter(S_software_return, S_Category == "Low")

industry_group_software <- left_join(industry_group_software, low_s_software %>% 
                                     select(date, S_Low = Value_Weighted_Return), 
                                   by = "date")

high_g_software <- filter(G_software_return, G_Category == "High")

industry_group_software <- left_join(industry_group_software, high_g_software %>% 
                                     select(date, G_High = Value_Weighted_Return), 
                                   by = "date")

low_g_software <- filter(G_software_return, G_Category == "Low")

industry_group_software <- left_join(industry_group_software, low_g_software %>% 
                                     select(date, G_Low = Value_Weighted_Return), 
                                   by = "date")

industry_group_software <- industry_group_software %>%
  mutate(ESG = ESG_High - ESG_Low)

industry_group_software <- industry_group_software %>%
  mutate(E = E_High - E_Low)

industry_group_software <- industry_group_software %>%
  mutate(S = S_High - S_Low)

industry_group_software <- industry_group_software %>%
  mutate(G = G_High - G_Low)

industry_group_software <- industry_group_software %>%
  select(-ESG_High, -ESG_Low, -E_High, -E_Low, -S_High, -S_Low, -G_High, -G_Low)

industry_factor_software <- industry_group_software %>%
  select(date, ESG, E, S, G)  # Select only the specified columns

industry_factor_software <- industry_factor_software %>%
  distinct(date, ESG, E, S, G, .keep_all = TRUE)
industry_factor_software <- industry_factor_software %>%
  left_join(factor_combined %>% select(date, excess_ret, SMB, HML, RMW, MOM), by = "date")

industry_factor_software <- industry_factor_software %>%
  filter(date != as.Date("2014-01-31"))
modelESG <- lm(ESG ~  excess_ret + SMB + HML + RMW + MOM, data = industry_factor_software)
summary(modelESG)

modelE <- lm(E ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor_software)
summary(modelE)

modelS <- lm(S ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor_software)
summary(modelS)

modelG <- lm(G ~ excess_ret + SMB + HML + RMW + MOM, data = industry_factor_software)
summary(modelG)

portfolio_stats_software <- data.frame()

for (col in portfolio_columns) {
  temp_stats_energy <- industry_factor_software %>%
    summarise(
      Mean = mean(get(col), na.rm = TRUE),
      Median = median(get(col), na.rm = TRUE),
      SD = sd(get(col), na.rm = TRUE),
      Sharpe = mean(get(col), na.rm = TRUE) / sd(get(col), na.rm = TRUE),
      Kurtosis = kurtosis(get(col), na.rm = TRUE),
      Skewness = skewness(get(col), na.rm = TRUE)
    ) %>%
    mutate(Portfolio = col)
  
  portfolio_stats_software <- rbind(portfolio_stats_software, temp_stats_energy)
}

# View the calculated statistics
print(portfolio_stats_software)


### Graphs and tables ###

filter_table <- cleaned_combined_data %>%
  filter(date == as.Date("2023-12-31"))

filter_table <- filter_table %>%
  left_join(industry_mapping, by = c("sector_2.x" = "sector_code"))

ESG_High_2023 <- filter_table %>%
  filter(ESG_Category == "High")

industry_market_caps <- ESG_High_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping for further operations
  )

total_high_esg_market_cap <- sum(ESG_High_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry
industry_market_caps <- industry_market_caps %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_high_esg_market_cap
  )

# Filter for E Category being 'High'
E_High_2023 <- filter_table %>%
  filter(E_Category == "High")

# Calculate total market cap per industry
industry_market_caps_E <- E_High_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping for further operations
  )

# Calculate the total market cap across E High
total_high_e_market_cap <- sum(E_High_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in E High
industry_market_caps_E <- industry_market_caps_E %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_high_e_market_cap
  )

# Filter for S Category being 'High'
S_High_2023 <- filter_table %>%
  filter(S_Category == "High")

# Calculate total market cap per industry
industry_market_caps_S <- S_High_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping for further operations
  )

# Calculate the total market cap across S High
total_high_s_market_cap <- sum(S_High_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in S High
industry_market_caps_S <- industry_market_caps_S %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_high_s_market_cap
  )

G_High_2023 <- filter_table %>%
  filter(G_Category == "High")

# Calculate total market cap per industry
industry_market_caps_G <- G_High_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping for further operations
  )

# Calculate the total market cap across G High
total_high_g_market_cap <- sum(G_High_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in G High
industry_market_caps_G <- industry_market_caps_G %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_high_g_market_cap
  )
write.csv(industry_market_caps, "ESG_High_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_E, "E_High_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_S, "S_High_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_G, "G_High_Market_Share_2023.csv", row.names = FALSE)



# Filter for ESG Category being 'Low'
ESG_Low_2023 <- filter_table %>%
  filter(ESG_Category == "Low")

# Calculate total market cap per industry
industry_market_caps_ESG_Low <- ESG_Low_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'  # Drop the grouping for further operations
  )

# Calculate the total market cap across ESG Low
total_low_esg_market_cap <- sum(ESG_Low_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in ESG Low
industry_market_caps_ESG_Low <- industry_market_caps_ESG_Low %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_low_esg_market_cap
  )

# Filter for E Category being 'Low'
E_Low_2023 <- filter_table %>%
  filter(E_Category == "Low")

# Calculate total market cap per industry
industry_market_caps_E_Low <- E_Low_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the total market cap across E Low
total_low_e_market_cap <- sum(E_Low_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in E Low
industry_market_caps_E_Low <- industry_market_caps_E_Low %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_low_e_market_cap
  )

# Filter for S Category being 'Low'
S_Low_2023 <- filter_table %>%
  filter(S_Category == "Low")

# Calculate total market cap per industry
industry_market_caps_S_Low <- S_Low_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the total market cap across S Low
total_low_s_market_cap <- sum(S_Low_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in S Low
industry_market_caps_S_Low <- industry_market_caps_S_Low %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_low_s_market_cap
  )

# Filter for G Category being 'Low'
G_Low_2023 <- filter_table %>%
  filter(G_Category == "Low")

# Calculate total market cap per industry
industry_market_caps_G_Low <- G_Low_2023 %>%
  group_by(sector_name) %>%
  summarise(
    Total_Industry_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate the total market cap across G Low
total_low_g_market_cap <- sum(G_Low_2023$Marked.Cap.USD, na.rm = TRUE)

# Calculate market share for each industry in G Low
industry_market_caps_G_Low <- industry_market_caps_G_Low %>%
  mutate(
    Market_Share = Total_Industry_Market_Cap / total_low_g_market_cap
  )

write.csv(industry_market_caps_ESG_Low, "ESG_Low_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_E_Low, "E_Low_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_S_Low, "S_Low_Market_Share_2023.csv", row.names = FALSE)
write.csv(industry_market_caps_G_Low, "G_Low_Market_Share_2023.csv", row.names = FALSE)

smb$date <- as.Date(smb$date)
hml$date <- as.Date(hml$date)
rmw$date <- as.Date(rmw$date)

factors_data <- left_join(smb, hml, by = "date")

# Now merge with RMW
factors_data <- left_join(factors_data, rmw, by = "date")

factors_data <- factors_data %>%
  select(date, SMB, HML, RMW)

riskfree <- Risikofrie.rente

# Assuming riskfree data is initially in 'Risikofrie.rente' with dates as "dd.mm.yyyy"
riskfree <- Risikofrie.rente %>%
  mutate(DATE = dmy(DATE),  # Convert from "dd.mm.yyyy" to Date object
         DATE = ceiling_date(DATE, "month") - days(1))  # Adjust to the last day of the month

# Ensure the 'date' in 'combined_factor_data' is a Date object, assuming it's already in "YYYY-MM-DD" format
combined_factor_data <- combined_factor_data %>%
  mutate(date = as.Date(date))

# Ensure the riskfree dataset's DATE is formatted as "YYYY-MM-DD" if necessary
riskfree <- riskfree %>%
  mutate(DATE = as.Date(DATE, format = "%Y-%m-%d"))

riskfree <- riskfree %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    Monthly_Rate = (1 + TB4WK)^(1/12) - 1
  )
# Join the riskfree rates to the factors_data by aligning dates
factors_data <- factors_data %>%
  left_join(riskfree, by = c("date" = "DATE"))

# Calculate the weighted returns
combined_factor_data <- combined_factor_data %>%
  filter(Return <= 2)
combined_factor_data <- combined_factor_data %>%
  mutate(Weighted_Return = Return * Marked.Cap.USD)

# Summarize to get the value-weighted return by date
value_weighted_returns_by_date <- combined_factor_data %>%
  group_by(date) %>%
  summarise(
    Total_Weighted_Return = sum(Weighted_Return, na.rm = TRUE),
    Total_Market_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Market_Cap,
    .groups = 'drop'  # Ensures the data is no longer grouped after summarisation
  )
factors_data <- factors_data %>%
  select(-Value_Weighted_Return)

factors_data <- factors_data %>%
  left_join(value_weighted_returns_by_date %>% select(date, Value_Weighted_Return), by = "date")

factors_data <- factors_data %>%
  mutate(TB4WK = as.character(TB4WK),  # Ensure TB4WK is treated as a character string for replacement
         TB4WK = gsub(",", ".", TB4WK),  # Replace commas with periods
         TB4WK = as.numeric(TB4WK))  # Convert to numeric
factors_data <- factors_data %>%
  mutate(
    DATE = as.Date(date, format = "%Y-%m-%d"),
    Monthly_Rate = (1 + TB4WK)^(1/12) - 1
  )

factors_data <- factors_data %>%
  mutate(TB4WK = as.numeric(TB4WK))
factors_data <- factors_data %>%
  mutate(TB4WK = TB4WK / 100)  # Convert from percentage to decimal

factors_data <- factors_data %>%
  mutate(excess_ret = Value_Weighted_Return - Monthly_Rate)

summary(factors_data)

# Apply the categorization based on the calculated percentiles
combined_factor_data <- combined_factor_data %>%
  mutate(
    ESG_10 = quantile(MSCI.ESG..Weighted.Average.Score., 0.1, na.rm = TRUE),
    ESG_90 = quantile(MSCI.ESG..Weighted.Average.Score., 0.9, na.rm = TRUE),
    E_10 = quantile(MSCI.Environmental.Pillar.Score, 0.1, na.rm = TRUE),
    E_90 = quantile(MSCI.Environmental.Pillar.Score, 0.9, na.rm = TRUE),
    S_10 = quantile(MSCI.Social.Pillar.Score, 0.1, na.rm = TRUE),
    S_90 = quantile(MSCI.Social.Pillar.Score, 0.9, na.rm = TRUE),
    G_10 = quantile(MSCI.Governance.Pillar.Score, 0.1, na.rm = TRUE),
    G_90 = quantile(MSCI.Governance.Pillar.Score, 0.9, na.rm = TRUE),
    ESG_Category = case_when(
      MSCI.ESG..Weighted.Average.Score. <= ESG_10 ~ "Low",
      MSCI.ESG..Weighted.Average.Score. >= ESG_90 ~ "High",
      TRUE ~ "Neutral"
    ),
    E_Category = case_when(
      MSCI.Environmental.Pillar.Score <= E_10 ~ "Low",
      MSCI.Environmental.Pillar.Score >= E_90 ~ "High",
      TRUE ~ "Neutral"
    ),
    S_Category = case_when(
      MSCI.Social.Pillar.Score <= S_10 ~ "Low",
      MSCI.Social.Pillar.Score >= S_90 ~ "High",
      TRUE ~ "Neutral"
    ),
    G_Category = case_when(
      MSCI.Governance.Pillar.Score <= G_10 ~ "Low",
      MSCI.Governance.Pillar.Score >= G_90 ~ "High",
      TRUE ~ "Neutral"
    )
  ) %>%
  select(-c(E_10, E_90, S_10, S_90, G_10, G_90))

# Assuming 'combined_factor_data' is your dataframe
value_weighted_returns_ESG <- combined_factor_data %>%
  group_by(date, ESG_Category) %>%  # Grouping by both date and ESG category
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),  # Summing weighted returns, ignoring NA
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,  # Calculating value-weighted return
    .groups = 'drop'  # Prevents the result from being grouped
  )

value_weighted_returns_E <- combined_factor_data %>%
  group_by(date, E_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for S Category
value_weighted_returns_S <- combined_factor_data %>%
  group_by(date, S_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

# Value Weighted Returns for G Category
value_weighted_returns_G <- combined_factor_data %>%
  group_by(date, G_Category) %>%
  summarise(
    Total_Marked_Cap = sum(Marked.Cap.USD, na.rm = TRUE),
    Total_Weighted_Return = sum(Return * Marked.Cap.USD, na.rm = TRUE),
    Value_Weighted_Return = Total_Weighted_Return / Total_Marked_Cap,
    .groups = 'drop'
  )

high_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "High")

# Step 3: Merge the filtered ESG data with the factors data on Date
factors_data <- left_join(factors_data, high_esg_data %>% 
                           select(date, ESG_High = Value_Weighted_Return), 
                         by = "date")

low_esg_data <- filter(value_weighted_returns_ESG, ESG_Category == "Low")

factors_data <- left_join(factors_data, low_esg_data %>% 
                            select(date, ESG_Low = Value_Weighted_Return), 
                          by = "date")

high_e_data <- filter(value_weighted_returns_E, E_Category == "High")

factors_data <- left_join(factors_data, high_e_data %>% 
                            select(date, E_High = Value_Weighted_Return), 
                          by = "date")

low_e_data <- filter(value_weighted_returns_E, E_Category == "Low")

factors_data <- left_join(factors_data, low_e_data %>% 
                            select(date, E_Low = Value_Weighted_Return), 
                          by = "date")

high_s_data <- filter(value_weighted_returns_S, S_Category == "High")

factors_data <- left_join(factors_data, high_s_data %>% 
                            select(date, S_High = Value_Weighted_Return), 
                          by = "date")

low_s_data <- filter(value_weighted_returns_S, S_Category == "Low")

factors_data <- left_join(factors_data, low_s_data %>% 
                            select(date, S_Low = Value_Weighted_Return), 
                          by = "date")

high_g_data <- filter(value_weighted_returns_G, G_Category == "High")

factors_data <- left_join(factors_data, high_g_data %>% 
                            select(date, G_High = Value_Weighted_Return), 
                          by = "date")

low_g_data <- filter(value_weighted_returns_G, G_Category == "Low")

factors_data <- left_join(factors_data, low_g_data %>% 
                            select(date, G_Low = Value_Weighted_Return), 
                          by = "date")

factors_data <- factors_data %>%
  mutate(
    Excess_ESG_High = ESG_High - Monthly_Rate,
    Excess_ESG_Low = ESG_Low - Monthly_Rate,
    Excess_E_High = E_High - Monthly_Rate,
    Excess_E_low = E_Low - Monthly_Rate,
    Excess_S_High = S_High - Monthly_Rate,
    Excess_S_Low = S_Low - Monthly_Rate,
    Excess_G_High = G_High - Monthly_Rate,
    Excess_G_low = G_Low - Monthly_Rate
    # Add more portfolios here as needed
  )

correlation_data <- select(factors_data, excess_ret, SMB, HML, RMW)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

print(correlation_matrix)

model_E_High <- lm(Excess_E_High ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_E_High)

model_E_Low <- lm(Excess_E_low ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_E_Low)

model_S_High <- lm(Excess_S_High ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_S_High)

model_S_Low <- lm(Excess_S_Low ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_S_Low)

model_G_High <- lm(Excess_G_High ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_G_High)

model_G_Low <- lm(Excess_G_low ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_G_Low)

model_ESG_High <- lm(Excess_ESG_High ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_ESG_High)

model_ESG_Low <- lm(Excess_ESG_Low ~ excess_ret + SMB + HML + RMW, data = factors_data)
summary(model_ESG_Low)

summary(factors_data)

library(car)

# Calculate VIF
vif_values <- vif(model_ESG_High)
print(vif_values)

vif_values_ESG_Low <- vif(model_ESG_Low)
print(vif_values_ESG_Low)

vif_values_E_High <- vif(model_E_High)
print(vif_values_E_High)


correlation_data1 <- select(factors_data, excess_ret, Excess_ESG_High, Excess_ESG_Low, Excess_E_High, Excess_E_low, Excess_S_High, Excess_S_Low, Excess_G_High,Excess_G_low)

# Calculate the correlation matrix
correlation_matrix1 <- cor(correlation_data1)
print(correlation_matrix1)
### CHECK ESG_Low portfolio###

selected_data <- combined_factor_data %>%
  filter(date == as.Date("2022-03-31"))

ggplot(selected_data, aes(x = Return, y = Marked.Cap.USD)) +
  geom_point() +  # This adds points to the plot
  labs(x = "Return", y = "Market Cap in USD", title = "Scatter Plot of Market Cap vs Return") +
  theme_minimal()  # This applies a minimal theme to the plot
# Step 1: Filter the data for ESG_Category == "Low"
s_high <- combined_factor_data %>%
  filter(S_Category == "High")

# Step 2: Add the sum of Marked.Cap.USD by date
s_high <- s_high %>%
  group_by(date) %>%
  mutate(total_market_cap = sum(Marked.Cap.USD))

# Step 3: Calculate the weight for each stock
weight_data <- s_high %>%
  mutate(weight = Marked.Cap.USD / total_market_cap)

summed_weights_by_date <- weight_data %>%
  group_by(date) %>%
  summarise(total_weight = sum(weight, na.rm = TRUE))  # na.rm = TRUE removes NA values before summing

# Viewing the resulting summed weights by date
print(summed_weights_by_date)

factors_data$Date <- as.Date(factors_data$Date, format = "%Y-%m-%d")

# Plot using ggplot2
ggplot(data = factors_data, aes(x = date)) +
  geom_line(aes(y = excess_ret, colour = "Excess Return"), size = 1) +
  geom_line(aes(y = Excess_ESG_High, colour = "Excess ESG High"), size = 1) +
  scale_y_continuous(
    name = "Excess Return",
    sec.axis = sec_axis(~ ., name = "Excess ESG High")
  ) +
  labs(title = "Monthly Excess Return and Excess ESG High from 2014-2023",
       x = "Date", y = "Value") +
  theme_minimal() +
  scale_colour_manual(
    name = "Legend", 
    values = c("Excess Return" = "blue", "Excess ESG High" = "red")
  )

combined_factor_data %>%
  ungroup() %>%  # Ensure there is no existing grouping
  summarise(
    ESG_High = sum(ESG_Category == "High", na.rm = TRUE),
    ESG_Low = sum(ESG_Category == "Low", na.rm = TRUE),
    E_High = sum(E_Category == "High", na.rm = TRUE),
    E_Low = sum(E_Category == "Low", na.rm = TRUE),
    S_High = sum(S_Category == "High", na.rm = TRUE),
    S_Low = sum(S_Category == "Low", na.rm = TRUE),
    G_High = sum(G_Category == "High", na.rm = TRUE),
    G_Low = sum(G_Category == "Low", na.rm = TRUE)
  ) -> count_scores
print(count_scores)

unique_sec_id_count <- combined_factor_data %>%
  summarise(count = n_distinct(sec_id))

print(unique_sec_id_count)

unique_sec_id_count_base <- length(unique(combined_factor_data$sec_id))

# Print the result
print(unique_sec_id_count_base)

portfolio_columns <- c("Excess_ESG_High", "Excess_ESG_Low", "Excess_E_High", "Excess_E_low", "Excess_S_High", "Excess_S_Low", "Excess_G_High", "Excess_G_low", "excess_ret")

# Prepare a data frame to hold all statistics
portfolio_stats <- data.frame()

for (col in portfolio_columns) {
  temp_stats <- factors_data %>%
    summarise(
      Mean = mean(get(col), na.rm = TRUE),
      Median = median(get(col), na.rm = TRUE),
      SD = sd(get(col), na.rm = TRUE),
      Sharpe = mean(get(col), na.rm = TRUE) / sd(get(col), na.rm = TRUE),
      Kurtosis = kurtosis(get(col), na.rm = TRUE),
      Skewness = skewness(get(col), na.rm = TRUE)
    ) %>%
    mutate(Portfolio = col)
  
  portfolio_stats <- rbind(portfolio_stats, temp_stats)
}

# View the calculated statistics
print(portfolio_stats)

factors_data$date <- as.Date(factors_data$date, format = "%Y-%m-%d")  # Adjust format as needed

factors_data <- factors_data %>%
  mutate(
    Log_High_ESG = log(1 + Excess_ESG_High),
    Log_Low_ESG = log(1 + Excess_ESG_Low),
    Log_Value_Weighted = log(1 + excess_ret)
  )

factors_data <- factors_data %>%
  arrange(date) %>%  # Make sure data is sorted by date
  mutate(
    Cumulative_High_ESG = cumsum(Log_High_ESG),
    Cumulative_Low_ESG = cumsum(Log_Low_ESG),
    Cumulative_Value_Weighted = cumsum(Log_Value_Weighted)
  )

data_long <- factors_data %>%
  select(date, Cumulative_High_ESG, Cumulative_Low_ESG, Cumulative_Value_Weighted) %>%
  pivot_longer(
    cols = starts_with("Cumulative"),
    names_to = "Portfolio",
    values_to = "Cumulative_Return"
  )

library(tidyr)
data_long <- data_long %>%
  mutate(Portfolio = case_when(
    Portfolio == "Cumulative_High_ESG" ~ "High ESG",
    Portfolio == "Cumulative_Low_ESG" ~ "Low ESG",
    Portfolio == "Cumulative_Value_Weighted" ~ "Value Weighted Benchmark",
    TRUE ~ Portfolio  # Keeps the original value if none of the conditions are met
  ))

ggplot(data_long, aes(x = date, y = Cumulative_Return, color = Portfolio)) +
  geom_line() +
  labs(title = "Cumulative Excess Returns of ESG Portfolios and Benchmark",
       x = "Date",
       y = "Cumulative Logarithmic Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(legend.position = "bottom")

factors_data <- factors_data %>%
  mutate(
    Log_E_High = log(1 + Excess_E_High),
    Log_E_Low = log(1 + Excess_E_low),
    Log_S_High = log(1 + Excess_S_High),
    Log_S_Low = log(1 + Excess_S_Low),
    Log_G_High = log(1 + Excess_G_High),
    Log_G_Low = log(1 + Excess_G_low),
  )

factors_data <- factors_data %>%
  arrange(date) %>%
  mutate(
    Cumulative_E_High = cumsum(Log_E_High),
    Cumulative_E_Low = cumsum(Log_E_Low),
    Cumulative_S_High = cumsum(Log_S_High),
    Cumulative_S_Low = cumsum(Log_S_Low),
    Cumulative_G_High = cumsum(Log_G_High),
    Cumulative_G_Low = cumsum(Log_G_Low),
    Cumulative_Value_Weighted = cumsum(Log_Value_Weighted)
  )

data_long_E <- factors_data %>%
  select(date, Cumulative_E_High, Cumulative_E_Low, Cumulative_Value_Weighted) %>%
  pivot_longer(
    cols = -date,
    names_to = "Portfolio",
    values_to = "Cumulative_Return"
  ) %>%
  mutate(Portfolio = case_when(
    Portfolio == "Cumulative_E_High" ~ "High E",
    Portfolio == "Cumulative_E_Low" ~ "Low E",
    Portfolio == "Cumulative_Value_Weighted" ~ "Value Weighted Benchmark"
  ))

# Plot for E
ggplot(data_long_E, aes(x = date, y = Cumulative_Return, color = Portfolio)) +
  geom_line() +
  labs(title = "Cumulative Returns for E Portfolios and Benchmark",
       x = "Date",
       y = "Cumulative Log Return",
       color = "Portfolio") +
  theme_minimal() +
  theme(legend.position = "bottom")

write.csv(factors_data, "factors_data.csv", row.names = FALSE)


#### SMU ####
combined_factor_data <- combined_factor_data %>%
  mutate(
    ESG_30 = quantile(MSCI.ESG..Weighted.Average.Score., 0.3, na.rm = TRUE),
    ESG_70 = quantile(MSCI.ESG..Weighted.Average.Score., 0.7, na.rm = TRUE),
    E_30 = quantile(MSCI.Environmental.Pillar.Score, 0.3, na.rm = TRUE),
    E_70 = quantile(MSCI.Environmental.Pillar.Score, 0.7, na.rm = TRUE),
    S_30 = quantile(MSCI.Social.Pillar.Score, 0.3, na.rm = TRUE),
    S_70 = quantile(MSCI.Social.Pillar.Score, 0.7, na.rm = TRUE),
    G_30 = quantile(MSCI.Governance.Pillar.Score, 0.3, na.rm = TRUE),
    G_70 = quantile(MSCI.Governance.Pillar.Score, 0.7, na.rm = TRUE),
    ESG_Category = case_when(
      MSCI.ESG..Weighted.Average.Score. <= ESG_30 ~ "Low",
      MSCI.ESG..Weighted.Average.Score. >= ESG_70 ~ "High",
      TRUE ~ "Neutral"
    ),
    E_Category = case_when(
      MSCI.Environmental.Pillar.Score <= E_30 ~ "Low",
      MSCI.Environmental.Pillar.Score >= E_70 ~ "High",
      TRUE ~ "Neutral"
    ),
    S_Category = case_when(
      MSCI.Social.Pillar.Score <= S_30 ~ "Low",
      MSCI.Social.Pillar.Score >= S_70 ~ "High",
      TRUE ~ "Neutral"
    ),
    G_Category = case_when(
      MSCI.Governance.Pillar.Score <= G_30 ~ "Low",
      MSCI.Governance.Pillar.Score >= G_70 ~ "High",
      TRUE ~ "Neutral"
    )
  ) %>%
  select(-c(E_30, E_70, S_30, S_70, G_30, G_70))


total_counts <- combined_factor_data %>%
  summarise(
    Total_High_ESG = sum(ESG_Category == "High", na.rm = TRUE),
    Total_Low_ESG = sum(ESG_Category == "Low", na.rm = TRUE),
    Total_High_E = sum(E_Category == "High", na.rm = TRUE),
    Total_Low_E = sum(E_Category == "Low", na.rm = TRUE),
    Total_High_S = sum(S_Category == "High", na.rm = TRUE),
    Total_Low_S = sum(S_Category == "Low", na.rm = TRUE),
    Total_High_G = sum(G_Category == "High", na.rm = TRUE),
    Total_Low_G = sum(G_Category == "Low", na.rm = TRUE)
  )

# Write the data frame to a CSV file
write.csv(total_counts, "total_counts.csv", row.names = FALSE)

portfolio_returns_SMU_ESG <- combined_factor_data %>%
  group_by(date, Size_Category, ESG_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(ESG_Category %in% c("High", "Low"))

SMU_ESG <- portfolio_returns_SMU_ESG %>%
  group_by(date) %>%
  summarise(
    Small_High = sum(Average_Return[Size_Category == "Small" & ESG_Category == "High"], na.rm = TRUE),
    Big_High = sum(Average_Return[Size_Category == "Big" & ESG_Category == "High"], na.rm = TRUE),
    Small_Low = sum(Average_Return[Size_Category == "Small" & ESG_Category == "Low"], na.rm = TRUE),
    Big_Low = sum(Average_Return[Size_Category == "Big" & ESG_Category == "Low"], na.rm = TRUE),
    SMU_ESG = (1/2) * (Small_High + Big_High) - (1/2) * (Small_Low + Big_Low)
  )
summary(factors_data)
factors_data <- factors_data %>%
  select(-Small_High.x, -Big_High.x, -Small_Low.x, -Big_Low.x, -SMU_ESG.x, -Small_High.y, -Big_High.y, -Small_Low.y, -Big_Low.y, -SMU_ESG.y, -Small_High.x.x, -Big_High.x.x, -Small_Low.x.x, -Big_Low.x.x, -SMU_S, -Small_High.y.y, -Big_High.y.y, -Small_Low.y.y, -Big_Low.y.y, -SMU_G, -SMU_ESG)
factors_data <- left_join(factors_data, SMU_ESG %>% select(date, SMU_ESG), by = "date")

portfolio_returns_SMU_E <- combined_factor_data %>%
  group_by(date, Size_Category, E_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(E_Category %in% c("High", "Low"))

SMU_E <- portfolio_returns_SMU_E %>%
  group_by(date) %>%
  summarise(
    Small_High = sum(Average_Return[Size_Category == "Small" & E_Category == "High"], na.rm = TRUE),
    Big_High = sum(Average_Return[Size_Category == "Big" & E_Category == "High"], na.rm = TRUE),
    Small_Low = sum(Average_Return[Size_Category == "Small" & E_Category == "Low"], na.rm = TRUE),
    Big_Low = sum(Average_Return[Size_Category == "Big" & E_Category == "Low"], na.rm = TRUE),
    SMU_E = (1/2) * (Small_High + Big_High) - (1/2) * (Small_Low + Big_Low)
  )

factors_data <- left_join(factors_data, SMU_E %>% select(date, SMU_E), by = "date")

portfolio_returns_SMU_S <- combined_factor_data %>%
  group_by(date, Size_Category, S_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(S_Category %in% c("High", "Low"))

SMU_S <- portfolio_returns_SMU_S %>%
  group_by(date) %>%
  summarise(
    Small_High = sum(Average_Return[Size_Category == "Small" & S_Category == "High"], na.rm = TRUE),
    Big_High = sum(Average_Return[Size_Category == "Big" & S_Category == "High"], na.rm = TRUE),
    Small_Low = sum(Average_Return[Size_Category == "Small" & S_Category == "Low"], na.rm = TRUE),
    Big_Low = sum(Average_Return[Size_Category == "Big" & S_Category == "Low"], na.rm = TRUE),
    SMU_S = (1/2) * (Small_High + Big_High) - (1/2) * (Small_Low + Big_Low)
  )

factors_data <- left_join(factors_data, SMU_S %>% select(date, SMU_S), by = "date")

# Step 1: Calculate average returns grouped by G_Category
portfolio_returns_SMU_G <- combined_factor_data %>%
  group_by(date, Size_Category, G_Category) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE), .groups = 'drop') %>%
  filter(G_Category %in% c("High", "Low"))

# Step 2: Calculate SMU values for G
SMU_G <- portfolio_returns_SMU_G %>%
  group_by(date) %>%
  summarise(
    Small_High = sum(Average_Return[Size_Category == "Small" & G_Category == "High"], na.rm = TRUE),
    Big_High = sum(Average_Return[Size_Category == "Big" & G_Category == "High"], na.rm = TRUE),
    Small_Low = sum(Average_Return[Size_Category == "Small" & G_Category == "Low"], na.rm = TRUE),
    Big_Low = sum(Average_Return[Size_Category == "Big" & G_Category == "Low"], na.rm = TRUE),
    SMU_G = (1/2) * (Small_High + Big_High) - (1/2) * (Small_Low + Big_Low)
  )

factors_data <- left_join(factors_data, SMU_G %>% select(date, SMU_G), by = "date")

correlation_data <- select(factors_data, excess_ret, SMB, HML, RMW, SMU_E)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data)

print

model_SMU_esg <- lm(Excess_ESG_High ~ excess_ret + SMB + HML + RMW + SMU_ESG, data = factors_data)
summary(model_SMU_esg)

model_SMU_esg_l <- lm(Excess_ESG_Low ~ excess_ret + SMB + HML + RMW + SMU_ESG, data = factors_data)
summary(model_SMU_esg_l)

model_SMU_e <- lm(Excess_E_High ~ excess_ret + SMB + HML + RMW + SMU_E, data = factors_data)
summary(model_SMU_e)

model_SMU_e_l <- lm(Excess_E_low ~ excess_ret + SMB + HML + RMW + SMU_E, data = factors_data)
summary(model_SMU_e_l)

model_SMU_s <- lm(Excess_S_High ~ excess_ret + SMB + HML + RMW + SMU_S, data = factors_data)
summary(model_SMU_s)

model_SMU_s_l <- lm(Excess_S_Low ~ excess_ret + SMB + HML + RMW + SMU_S, data = factors_data)
summary(model_SMU_s_l)

model_SMU_g <- lm(Excess_G_High ~ excess_ret + SMB + HML + RMW + SMU_G, data = factors_data)
summary(model_SMU_g)

model_SMU_g_l <- lm(Excess_G_low ~ excess_ret + SMB + HML + RMW + SMU_G, data = factors_data)
summary(model_SMU_g_l)

ggplot(factors_data, aes(x = excess_ret, y = ESG_High)) +
  geom_point(alpha = 0.5) +  # Adds points with slight transparency
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Adds a linear regression line without confidence band
  labs(title = "Line Fit: ESG_High vs Market Factor", x = "Market Factor", y = "ESG High Returns") +
  theme_minimal()

# Plot ESG_High vs SMB
ggplot(factors_data, aes(x = SMB, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Line Fit: ESG_High vs SMB", x = "SMB", y = "ESG High Returns") +
  theme_minimal()

# Plot ESG_High vs HML
ggplot(factors_data, aes(x = HML, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Line Fit: ESG_High vs HML", x = "HML", y = "ESG High Returns") +
  theme_minimal()

# Plot ESG_High vs RMW
ggplot(factors_data, aes(x = RMW, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Line Fit: ESG_High vs RMW", x = "RMW", y = "ESG High Returns") +
  theme_minimal()

install.packages("patchwork")
library(patchwork)

# Create each plot and store them
plot1 <- ggplot(factors_data, aes(x = excess_ret, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG_High vs Market Factor") +
  theme_minimal()

plot2 <- ggplot(factors_data, aes(x = SMB, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG_High vs SMB") +
  theme_minimal()

plot3 <- ggplot(factors_data, aes(x = HML, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG_High vs HML") +
  theme_minimal()

plot4 <- ggplot(factors_data, aes(x = RMW, y = ESG_High)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "ESG_High vs RMW") +
  theme_minimal()

# Combine the plots into a 2x2 grid
plot_grid <- plot1 + plot2 + plot3 + plot4 +
  plot_layout(ncol = 2, nrow = 2)

# Print the combined plot
print(plot_grid)

# Create a data frame of residuals and fitted values
residuals_data <- data.frame(
  Residuals = residuals(model_ESG_High),
  Fitted = fitted.values(model_ESG_High),
  MarketFactor = factors_data$excess_ret,
  SMB = factors_data$SMB,
  HML = factors_data$HML,
  RMW = factors_data$RMW
)


# Market Factor vs Residuals
p1 <- ggplot(residuals_data, aes(x = MarketFactor, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Market Factor", x = "Market Factor", y = "Residuals") +
  theme_minimal()

# SMB vs Residuals
p2 <- ggplot(residuals_data, aes(x = SMB, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. SMB", x = "SMB", y = "Residuals") +
  theme_minimal()

# HML vs Residuals
p3 <- ggplot(residuals_data, aes(x = HML, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. HML", x = "HML", y = "Residuals") +
  theme_minimal()

# RMW vs Residuals
p4 <- ggplot(residuals_data, aes(x = RMW, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Residuals vs. RMW", x = "RMW", y = "Residuals") +
  theme_minimal()

# Optional: Use the patchwork package to display all plots in a single layout
library(patchwork)
p1 + p2 + p3 + p4 + plot_layout(ncol = 2)

residuals <- residuals(model_ESG_High)

# Calculate the mean of residuals
mean_residuals <- mean(residuals, na.rm = TRUE) 
print(mean_residuals)

plot(fitted(model_ESG_High), residuals(model_ESG_High), main="Residuals vs Fitted", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")

install.packages("lmtest")
library(lmtest)
bptest_result <- bptest(model_ESG_High)

# Print the test results
print(bptest_result)

acf(residuals, main = "ACF of Residuals", lag.max = 30)
acf(residuals(model_ESG_High))

dw_result <- dwtest(model_ESG_High)

# Print the results
print(dw_result)