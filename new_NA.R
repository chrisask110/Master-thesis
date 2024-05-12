library(dplyr)
library("lubridate")
library(ggplot2)
library(zoo)
library(tidyr)



### THE PRICE DATASETS ####
prices_NA_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2023.csv")
prices_NA_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2022.csv")
prices_NA_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2021.csv")
prices_NA_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2020.csv")
prices_NA_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2019.csv")
prices_NA_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2018.csv")
prices_NA_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2017.csv")
prices_NA_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2016.csv")
prices_NA_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2015.csv")
prices_NA_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2014.csv")
prices_NA_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2013.csv")
prices_NA_2012 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2012.csv")
prices_NA_2011 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2011.csv")
prices_NA_2010 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2010.csv")
prices_NA_2009 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2009.csv")
prices_NA_2008 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/prices_north_america_2008.csv")






##### MAKE SURE ITS AS DATE ####
prices_NA_2023$date <- as.Date(prices_NA_2023$date)  # Converting to Date type
prices_NA_2022$date <- as.Date(prices_NA_2022$date)
prices_NA_2021$date <- as.Date(prices_NA_2021$date)
prices_NA_2020$date <- as.Date(prices_NA_2020$date)
prices_NA_2019$date <- as.Date(prices_NA_2019$date)
prices_NA_2018$date <- as.Date(prices_NA_2018$date)
prices_NA_2017$date <- as.Date(prices_NA_2017$date)
prices_NA_2016$date <- as.Date(prices_NA_2016$date)
prices_NA_2015$date <- as.Date(prices_NA_2015$date)
prices_NA_2014$date <- as.Date(prices_NA_2014$date)
prices_NA_2013$date <- as.Date(prices_NA_2013$date)

#### CONVERT TO MONTHLY DATA FOR EACH YEAR ####
prices_NA_2023_monthly <- prices_NA_2023 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2022_monthly <- prices_NA_2022 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2021_monthly <- prices_NA_2021 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2020_monthly <- prices_NA_2020 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2019_monthly <- prices_NA_2019 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary


prices_NA_2018_monthly <- prices_NA_2018 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2017_monthly <- prices_NA_2017 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2016_monthly <- prices_NA_2016 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2015_monthly <- prices_NA_2015 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2014_monthly <- prices_NA_2014 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2013_monthly <- prices_NA_2013 %>%
  mutate(year_month = floor_date(date, "month")) %>%  # Create a year-month column
  group_by(sec_id, year_month) %>%
  slice(n()) %>%  # Select the last observation in each month for each sec_id
  ungroup()  # Ungroup for further manipulation if necessary

prices_NA_2023_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2023_monthly.csv")
prices_NA_2022_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2022_monthly.csv")
prices_NA_2021_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2021_monthly.csv")
prices_NA_2020_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2020_monthly.csv")
prices_NA_2019_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2019_monthly.csv")
prices_NA_2018_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2018_monthly.csv")
prices_NA_2017_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2017_monthly.csv")
prices_NA_2016_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2016_monthly.csv")
prices_NA_2015_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2015_monthly.csv")
prices_NA_2014_monthly <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/Master Thesis - Private/Data/prices_NA_2014_monthly.csv")
adj_prices_NA_2023_monthly <- select(prices_NA_2023_monthly, -"close_dkk", -"tot_ret_dkk")

master_sec <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/price_data/sec_master.csv")

Combined_prices_NA_montly <- rbind(adj_prices_NA_2023_monthly, prices_NA_2022_monthly, 
                                   prices_NA_2021_monthly, prices_NA_2020_monthly, 
                                   prices_NA_2019_monthly, prices_NA_2018_monthly,
                                   prices_NA_2017_monthly, prices_NA_2016_monthly,
                                   prices_NA_2015_monthly, prices_NA_2014_monthly)

Combined_prices_NA_montly$date <- as.Date(Combined_prices_NA_montly$date)  # Converting to Date type

final_prices_NA_monthly <- left_join(Combined_prices_NA_montly, master_sec, by = "sec_id")
final_prices_NA_monthly <- final_prices_NA_monthly %>%
  mutate(date = ceiling_date(date, "month") - days(1))

mktcap_NA_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2023.csv")
mktcap_NA_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2022.csv")
mktcap_NA_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2021.csv")
mktcap_NA_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2020.csv")
mktcap_NA_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2019.csv")
mktcap_NA_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2018.csv")
mktcap_NA_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2017.csv")
mktcap_NA_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2016.csv")
mktcap_NA_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2015.csv")
mktcap_NA_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2014.csv")
mktcap_NA_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2013.csv")
mktcap_NA_2012 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2012.csv")
mktcap_NA_2011 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2011.csv")
mktcap_NA_2010 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2010.csv")
mktcap_NA_2009 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2009.csv")
mktcap_NA_2008 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/factor_values_north_america_2008.csv")
Combined_mktcap_NA <- rbind(mktcap_NA_2023, mktcap_NA_2022, 
                            mktcap_NA_2021, mktcap_NA_2020, 
                            mktcap_NA_2019, mktcap_NA_2018,
                            mktcap_NA_2017, mktcap_NA_2016,
                            mktcap_NA_2015, mktcap_NA_2014)
Combined_mktcap_NA$date <- as.Date(Combined_mktcap_NA$date)
Combined_mktcap_NA <- Combined_mktcap_NA %>%
  mutate(date = ceiling_date(date, "month") - days(1))
final_NA <- left_join(final_prices_NA_monthly, Combined_mktcap_NA, by = c("sec_id", "date"))

test_mkt <- rbind(mktcap_NA_2013, mktcap_NA_2012, 
                  mktcap_NA_2011, mktcap_NA_2010, 
                  mktcap_NA_2009, mktcap_NA_2008)
test_mkt$date <- as.Date(test_mkt$date)
test_mkt <- test_mkt %>%
  mutate(date = ceiling_date(date, "month") - days(1))
esg_NA_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2023.csv")
esg_NA_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2022.csv")
esg_NA_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2021.csv")
esg_NA_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2020.csv")
esg_NA_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2019.csv")
esg_NA_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2018.csv")
esg_NA_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2017.csv")
esg_NA_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2016.csv")
esg_NA_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2015.csv")
esg_NA_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2014.csv")
esg_NA_2013 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2013.csv")
esg_NA_2012 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2012.csv")
esg_NA_2011 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2011.csv")
esg_NA_2010 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2010.csv")
esg_NA_2009 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2009.csv")
esg_NA_2008 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors_esg/factor_esg_values_north_america_2008.csv")
Combined_esg_NA <- rbind(esg_NA_2023, esg_NA_2022, 
                         esg_NA_2021, esg_NA_2020, 
                         esg_NA_2019, esg_NA_2018,
                         esg_NA_2017, esg_NA_2016,
                         esg_NA_2015, esg_NA_2014)

test_esg <- rbind(esg_NA_2013, esg_NA_2012, 
                 esg_NA_2011, esg_NA_2010, 
                 esg_NA_2009, esg_NA_2008)
test_esg$date <- as.Date(test_esg$date)
test_esg <- test_esg %>%
  mutate(date = ceiling_date(date, "month") - days(1))
combined_test <- left_join(test_mkt, test_esg, by = c("sec_id", "date"))
combined_test_1 <- select(combined_test, "sec_id", "date","MSCI.ESG..Weighted.Average.Score.", "MSCI.Environmental.Pillar.Score", "MSCI.Social.Pillar.Score", "MSCI.Governance.Pillar.Score", "Marked.Cap.USD")
write.csv(combined_test_1, file = "datatest.csv", row.names = FALSE)
Combined_esg_NA$date <- as.Date(Combined_esg_NA$date)
Combined_esg_NA <- Combined_esg_NA %>%
  mutate(date = ceiling_date(date, "month") - days(1))
nowfinal_NA <- left_join(final_NA, Combined_esg_NA, by = c("sec_id", "date"))

Final_NA_lessColumns <- select(nowfinal_NA,"sec_id", "date", "close_usd", "tot_ret_usd", "name", "region_name", "sector_1.x", "sector_2.x", "sector_3.x", "sector_4.x", "MSCI.ESG..Weighted.Average.Score.", "MSCI.Environmental.Pillar.Score", "MSCI.Social.Pillar.Score", "MSCI.Governance.Pillar.Score", "Marked.Cap.USD" ,"ROE", "ROA", "BookRatio", "EarningsYield", "EPS.Growth.5Y.Trailing", "beta")

summary(Final_NA_lessColumns)

momentum_NA_2023 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2023.csv")
momentum_NA_2022 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2022.csv")
momentum_NA_2021 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2021.csv")
momentum_NA_2020 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2020.csv")
momentum_NA_2019 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2019.csv")
momentum_NA_2018 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2018.csv")
momentum_NA_2017 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2017.csv")
momentum_NA_2016 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2016.csv")
momentum_NA_2015 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2015.csv")
momentum_NA_2014 <- read.csv("C:/Users/1600a/OneDrive - CBS - Copenhagen Business School/factors/momentum_factors/momentum_factors_north_america_2014.csv")

Combined_momentum_NA <- rbind(momentum_NA_2023, momentum_NA_2022, 
                              momentum_NA_2021, momentum_NA_2020, 
                              momentum_NA_2019, momentum_NA_2018,
                              momentum_NA_2017, momentum_NA_2016,
                              momentum_NA_2015, momentum_NA_2014)

Combined_momentum_NA_selected <- Combined_momentum_NA %>%
  select(sec_id, date, Momentum.12m)
Combined_momentum_NA_selected$date <- as.Date(Combined_momentum_NA_selected$date, format = "%Y-%m-%d")

# Now that 'date' is a Date object, adjust all dates to the last day of their respective month
Combined_momentum_NA_selected <- Combined_momentum_NA_selected %>%
  mutate(date = ceiling_date(date, "month") - days(1))

combined_data <- merge(Final_NA_lessColumns, Combined_momentum_NA_selected, by = c("sec_id", "date"))

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

write.csv(industry_summary, "industry_summary_2023_NA.csv", row.names = FALSE)
cleaned_combined_data <- cleaned_combined_data %>%
  arrange(sec_id, date) %>%
  group_by(sec_id) %>%
  mutate(Return = (tot_ret_usd - lag(tot_ret_usd)) / lag(tot_ret_usd))


cleaned_combined_data <- cleaned_combined_data %>%
  filter(Return <= 2)
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

category_counts <- cleaned_combined_data %>%
  group_by(G_Category) %>%
  summarise(count = n(), .groups = 'drop')

# Print the counts
print(category_counts)

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

write.csv(factor_combined, "factors", row.names = FALSE)

library(zoo)  # for as.yearmon
library(lubridate)  # for handling date calculations

X5_factor$date <- as.Date(as.yearmon(as.character(X5_factor$date), "%Y%m"), frac = 1)
X5_factor$date <- ceiling_date(X5_factor$date, "month") - days(1)

# Adjust the factor columns by dividing by 100
X5_factor <- X5_factor %>%
  mutate(across(c(`Mkt-RF`, SMB, HML, RMW, MOM), ~ . * 100))

X5_factor <- X5_factor %>%
  rename(
    fama_Mkt = "Mkt-RF",
    fama_SMB = SMB,
    fama_HML = HML,
    fama_RMW = RMW,
    fama_MOM = MOM
  )

factor_correlation <- factor_combined %>%
  left_join(X5_factor, by = "date")
mkt_correlation <- cor(factor_correlation$excess_ret, factor_correlation$fama_Mkt, use = "complete.obs")

# Print the correlation
print(mkt_correlation)

smb_correlation <- cor(factor_correlation$SMB, factor_correlation$fama_SMB, use = "complete.obs")

# Print the correlation
print(smb_correlation)
HML_correlation <- cor(factor_correlation$HML, factor_correlation$fama_HML, use = "complete.obs")

# Print the correlation
print(HML_correlation)

RMW_correlation <- cor(factor_correlation$RMW, factor_correlation$fama_RMW, use = "complete.obs")

# Print the correlation
print(RMW_correlation)

MOM_correlation <- cor(factor_correlation$MOM, factor_correlation$fama_MOM, use = "complete.obs")

# Print the correlation
print(MOM_correlation)

factors_correlation <- factor_combined %>%
  select(excess_ret, SMB, HML, RMW, MOM)

correlation_matrix <- cor(factors_correlation, use = "complete.obs")

print(correlation_matrix)

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

write.csv(value_weighted_returns_by_date, "Value_weighted_index.csv", row.names = FALSE)

library(openxlsx)
write.xlsx(value_weighted_returns_by_date, "Value_weighted_index.xl")


filter_table <- cleaned_combined_data %>%
  filter(date == as.Date("2023-12-31"))

filter_table <- filter_table %>%
  left_join(industry_mapping, by = c("sector_2.x" = "sector_code"))

ESG_High_2023 <- filter_table %>%
  filter(ESG_Category == "High")

industry_market_caps <- ESG_High_2023 %>%
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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
write.csv(industry_market_caps, "ESG_High_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_E, "E_High_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_S, "S_High_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_G, "G_High_Market_Share_2023_NA.csv", row.names = FALSE)



# Filter for ESG Category being 'Low'
ESG_Low_2023 <- filter_table %>%
  filter(ESG_Category == "Low")

# Calculate total market cap per industry
industry_market_caps_ESG_Low <- ESG_Low_2023 %>%
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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
  group_by(sector_name.x) %>%
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

write.csv(industry_market_caps_ESG_Low, "ESG_Low_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_E_Low, "E_Low_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_S_Low, "S_Low_Market_Share_2023_NA.csv", row.names = FALSE)
write.csv(industry_market_caps_G_Low, "G_Low_Market_Share_2023_NA.csv", row.names = FALSE)
