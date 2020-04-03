library(moderndive)
#Convert sum_year_sale to a rate per 100,000


vvg_fbi <- vvg_fbi %>%
  mutate(sum_year_rate = (sum_year_sale * 100000)/population)

#initial plot
ggplot(data = vvg_fbi, aes(x = sum_year_sale, y = violent_crime_rate)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

#Final plot
ggplot(data = vvg_fbi, aes(x = sum_year_rate, y = violent_crime_rate)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

violent_crime %>% ggplot(aes(x = year, y = violent_crime_rate)) +
  geom_point()

#Correlation
cor(vvg_fbi$sum_year_rate, vvg_fbi$violent_crime_rate)

#Regression
model <- lm(violent_crime_rate ~ sum_year_rate, vvg_fbi)
summary(model)
#Check for normality of residuals
regression_points <- get_regression_points(model)

ggplot(regression_points, aes(x = residual)) +
  geom_histogram(binwidth = 8) +
  labs(x = "Residual")

#Check for equality of variance
ggplot(regression_points, aes(x = sum_year_rate, y = residual)) +
  geom_point() +
  labs(x = "Video game sale ", y = "Residual") +
  geom_hline(yintercept = 0, col = "blue", size = 1)
