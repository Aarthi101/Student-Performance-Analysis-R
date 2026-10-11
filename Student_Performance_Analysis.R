# Student Performance Analysis and Prediction Using R
# Run this in RStudio with student-mat.csv in the working directory.
# It reproduces the numbers shown in the presentation.

student_data <- read.csv("student-mat.csv", sep = ";")
head(student_data); str(student_data); summary(student_data)

colSums(is.na(student_data))          # missing values
sum(duplicated(student_data))         # duplicate rows

hist(student_data$G3, breaks = 0:20, main = "Distribution of Student Final Grades", xlab = "Final Grade (G3)")
boxplot(G3 ~ studytime, data = student_data, main = "Final Grade by Study Time", xlab = "Study Time Category", ylab = "Final Grade (G3)")
plot(student_data$absences, student_data$G3, main = "Absences vs Final Grade", xlab = "Number of Absences", ylab = "Final Grade (G3)")

round(cor(student_data[c("G1", "G2", "G3", "studytime", "absences")], use = "complete.obs"), 3)

set.seed(123)
train_index <- sample(seq_len(nrow(student_data)), size = floor(0.8 * nrow(student_data)))
train_data <- student_data[train_index, ]
test_data  <- student_data[-train_index, ]
cat("Train rows:", nrow(train_data), " Test rows:", nrow(test_data), "\n")

model <- lm(G3 ~ G1 + G2 + studytime + absences, data = train_data)
summary(model)                        # coefficients, p-values, training R-squared

predictions <- predict(model, newdata = test_data)
actual <- test_data$G3
rmse <- sqrt(mean((actual - predictions)^2))
mae  <- mean(abs(actual - predictions))
r2   <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
cat("Test R-squared:", round(r2, 4), " RMSE:", round(rmse, 4), " MAE:", round(mae, 4), "\n")

plot(actual, predictions, main = "Actual vs Predicted Student Grades", xlab = "Actual Final Grade", ylab = "Predicted Final Grade")
abline(0, 1, col = "red", lty = 2)
head(data.frame(Actual = actual, Predicted = round(predictions, 2)), 8)
