# First need to import the energy demand data per day
library(dplyr)
setwd("~/Coding Practice/Energy-Prediction-For-TNEI")

# Loading in the demand data
# Specify the path to your CSV file
file_path <- "EnergyDemandData.csv"

# Load the CSV data into a dataframe
demand_data <- read.csv(file_path)

demand_data <- demand_data %>%
  # Need year, month and day columns
  dplyr::mutate(year = as.numeric(substr(Day, 1, 4)),
                month = as.numeric(substr(Day, 6, 7)),
                day = as.numeric(substr(Day, 9, 10)))

# Loading in the temperature data
# Specify the path to the text file
file_path <- "TemperatureData.txt"

# Read the data into a dataframe
temp_data <- read.table(
  file_path,
  header = FALSE,         # We don't have an official header row in the data, but we can assign headers later
  skip = 7,               # Skip the first 7 lines to avoid the metadata section
  fill = TRUE,            # Fill in missing columns with NAs
  na.strings = c("---"),  # Treat '---' as NA
  col.names = c("year", "month", "tmax", "tmin", "af", "rain", "sun")) %>% 
  # Select only required columns
  dplyr::select(year, month, tmax, tmin)

# Join the two tables together on year and month to get one dataset
data <- dplyr::left_join(demand_data, temp_data, by=c('year','month')) %>%
  # Want a column time to plot with
  dplyr::arrange(year,month,day) %>%
  dplyr::mutate(time = row_number())

# Set the start date as the 1st of July, 2021
start_date <- as.Date("2021-07-01")

# Convert integer days to dates
data$date <- start_date + data$time - 1  # Adjust by subtracting 1 for accurate day alignment

# Normal linear regression model
model1 <- lm(Demand ~ time + tmax, data=data)
summary(model1)

# All coefficients are very significant, but R^2 is fairly low
# Check residual assumptions
par(mfrow = c(1,2))
plot(model1)

# Plotting the model
par(mfrow = c(1,1))
plot(data$date, data$Demand/1000000, type="p", ylab='UK Energy Demand (Wh)', xlab='Date')
lines(data$date, model1$fitted/1000000, col='red', lwd=3)
# Model not too bad, but very clunky and doesn't capture the full highs

# Check if tmax and tmin are highly correlated - may not require both in the model
correlation <- cor(data$tmax, data$tmin, method = "pearson")
print(correlation)




# Normal linear regression model 2
model2 <- lm(Demand ~ I(cos(2*pi*time/365)) + I(sin(2*pi*time/365)) + tmax, data=data)
summary(model2)
# Now tmax is insignificant, but R^2 is still fairly low
# Check residual assumptions
par(mfrow = c(2,2))
plot(model2)

# Plotting the model
par(mfrow = c(1,1))
plot(data$date, data$Demand/1000000, type="p", ylab='UK Energy Demand (Wh)', xlab='Date')
lines(data$date, model2$fitted/1000000, col='red', lwd=2)

