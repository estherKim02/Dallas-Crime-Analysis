df <- read.csv("~/Downloads/Dallas_-_Crimes_20250429.csv", stringsAsFactors = FALSE, header = TRUE)
names(df) <- trimws(names(df))
print(names(df))
df$hour <- as.numeric(substr(df$offensestarttime, 1, 2))

regression_df <- df[, c("offensezip", "offensedescription", "hour")]
regression_df <- na.omit(regression_df)
head(regression_df)

# Question 1 (what time)
hourly_crimes <- table(regression_df$hour)

sorted_crimes <- sort(hourly_crimes, decreasing = TRUE)
print(sorted_crimes)



# Question 2 (high crime rate ZIP code)
library(dplyr)

crime_counts <- regression_df %>%
  group_by(offensezip, offensedescription) %>%
  summarise(count = n(), .groups = "drop")

crime_counts <- crime_counts %>% filter(offensezip >= 10000)

zip_total <- crime_counts %>%
  group_by(offensezip) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total))

top_zips <- head(zip_total$offensezip, 10) # for top 10

top_crime_by_zip <- crime_counts %>%
  filter(offensezip %in% top_zips) %>%
  group_by(offensezip) %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup()

top_crime_by_zip <- top_crime_by_zip %>%
  arrange(desc(count))

top_crime_by_zip #results

# Question 3
clean_df <- regression_df %>%
  filter(!is.na(offensezip), offensezip >= 10000)

zip_crime_counts <- clean_df %>%
  group_by(offensezip) %>%
  summarise(crime_counts = n(), .groups = "drop") %>%
  arrange(desc(crime_counts))

head(zip_crime_counts)
# modeled crime counts using both ZIP code and time of day. 
# The results show that location(ZIP code) has a statistically
# significant influence on the number of crimes reported.


# Question 4
top_zips <- regression_df %>%
  count(offensezip) %>%
  filter(offensezip >= 10000) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  pull(offensezip)

top_crimes <- regression_df %>%
  count(offensedescription) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  pull(offensedescription)

crime_model_df <- regression_df %>%
  filter(offensezip %in% top_zips,
         offensedescription %in% top_crimes) %>%
  group_by(offensezip, hour, offensedescription) %>%
  summarise(crime_counts = n(), .groups = "drop")

crime_model_df$offensezip <- as.factor(crime_model_df$offensezip)  
crime_model_df$offensedescription <- as.factor(crime_model_df$offensedescription)

model <- glm(crime_counts ~ hour + offensezip + offensedescription,
             data = crime_model_df,
             family = poisson())

summary(model)

# Residual Analysis
res <- residuals(model, type = "deviance")

fitted_vals <- fitted(model)

plot(fitted_vals, res,
     xlab = "Fitted values",
     ylab = "Deviance residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

plot(res, main = "Deviance Residuals", ylab = "Residuals")
abline(h = 0, col = "red")
