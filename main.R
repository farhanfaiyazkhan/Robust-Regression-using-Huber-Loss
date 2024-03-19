library(MASS)    
library(ggplot2) 

set.seed(123)
n <- 100
x <- seq(1, n)
y <- 2*x + 3 + rnorm(n, sd = 5)
y[c(10, 20, 30, 40, 50, 60, 70, 80, 90)] <- y[c(10, 20, 30, 40, 50, 60, 70, 80, 90)] + 100 

lm_model <- lm(y ~ x)

huber_model <- MASS::rlm(y ~ x, method = "MM")

pred_lm <- predict(lm_model)
pred_huber <- predict(huber_model)

data_df <- data.frame(x = x, y = y)
lm_df <- data.frame(x = x, y = pred_lm)
huber_df <- data.frame(x = x, y = pred_huber)

p <- ggplot(data_df, aes(x, y)) +
  geom_point() +
  geom_line(data = lm_df, aes(x, y, color = "Linear Regression")) +
  geom_line(data = huber_df, aes(x, y, color = "Huber Regression")) +
  labs(title = "Comparison of Traditional and Robust Regression",
       x = "X", y = "Y") +
  scale_color_manual(values = c("Linear Regression" = "blue", "Huber Regression" = "red")) +
  theme_minimal()

print(p)

lm_residuals <- resid(lm_model)
huber_residuals <- resid(huber_model)

lm_rmse <- sqrt(mean(lm_residuals^2))
huber_rmse <- sqrt(mean(huber_residuals^2))

lm_r_squared <- summary(lm_model)$r.squared
huber_r_squared <- summary(huber_model)$r.squared

cat("\nLinear Regression RMSE:", lm_rmse)
cat("\nHuber Regression RMSE:", huber_rmse)

cat("\nLinear Regression R-squared:", lm_r_squared)

