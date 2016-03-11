# Building the linear relationship model between the grey kangaroo's nose width (mm) and nose length (mm).
# Data downloaded from:
# http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/slr/excel/slr07.xls
# and saved locally as csv file for easy import into R.

kang_nose <- read.csv("slr07.csv", sep = ";")
colnames(kang_nose) <- c("nose_length", "nose_width")
kang_nose <- kang_nose[c(2, 1)]

# Plot the default data:
plot(kang_nose)

# Describe the linear relationship between the two variables in lm_kang:
lm_kang <- lm(nose_length ~ nose_width, data = kang_nose)

# Print the coefficients of lm_kang:
coef(lm_kang)

# Predict and print the nose length of the new kangoroo's nose_width:
predict(lm_kang, newdata = data.frame(nose_width = 250))

# Build model and make plot:
lm_kang <- lm(nose_length ~ nose_width, data = kang_nose)
plot(kang_nose, xlab = "nose width", ylab = "nose length")
abline(lm_kang$coefficients, col = "red")

# Apply predict() to lm_kang: nose_length_est:
nose_length_est <- predict(lm_kang)

# Calculate difference between the predicted and the true values:
res <- kang_nose$nose_length - nose_length_est
res

# Calculate RMSE, assign it to rmse:
rmse <- sqrt(1/length(res) * sum(res^2))
rmse

# Calculate the residual sum of squares: ss_res:
ss_res <- sum(res^2)

# Determine the total sum of squares: ss_tot:
ss_tot <- sum((kang_nose$nose_length - mean(kang_nose$nose_length))^2)

# Calculate R-squared and assign it to r_sq:
r_sq <- 1 - ss_res / ss_tot
r_sq

# Apply summary() to lm_kang:
summary(lm_kang)
