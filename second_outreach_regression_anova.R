library(readxl)

# File path (the data is in my working directory)
file <- "Training_Datasets.xlsx" 

#### 1. Simple Linear Regression

# Read Simple Linear Regression data
slr_data <- read_excel(file, sheet = "Simple_Linear_Regression")

print(slr_data)

summary(slr_data)

# Fit model
slr_model <- lm(y ~ x, data = slr_data)

# Summary of results
summary(slr_model)

# Scatterplot with regression line
plot(slr_data$x, slr_data$y, main="Simple Linear Regression",
     xlab="X", ylab="Y", pch=19)
abline(slr_model, col="blue", lwd=2)

# Normality test of residuals
shapiro.test(residuals(slr_model))

# Visual check
qqnorm(residuals(slr_model)); qqline(residuals(slr_model))
hist(residuals(slr_model), main = "Residuals Histogram", col = "lightblue")


  
#### 2. Multiple Linear Regression

# Read Multiple Linear Regression data
mlr_data <- read_excel(file, sheet = "Multiple_Linear_Regression")

# Fit model
mlr_model <- lm(y2 ~ x1 + x2, data = mlr_data)

# Summary of results
summary(mlr_model)

# Diagnostic plots
par(mfrow=c(2,2))
plot(mlr_model)
par(mfrow=c(1,1))

# Normality test
shapiro.test(residuals(mlr_model))

# Q-Q Plot
qqnorm(residuals(mlr_model)); qqline(residuals(mlr_model))



#### 3. One-Way ANOVA + Post-hoc

# Read One-Way ANOVA data
anova1_data <- read_excel(file, sheet = "One_Way_ANOVA")

# Fit ANOVA model
library(agricolae) # for Duncan multiple range
anova1_model <- aov(scores ~ group, data = anova1_data)

# Summary of results
summary(anova1_model)

# Plot group means
boxplot(scores ~ group, data = anova1_data,
        main="One-way ANOVA",
        xlab="Group", ylab="Scores", col="lightblue")

# Post-hoc test (Tukey)
TukeyHSD(anova1_model)

# Post-hoc test (Duncan’s Multiple Range Test)
duncan.test(anova1_model, "group", console = TRUE)

# Normality of residuals
shapiro.test(residuals(anova1_model))
qqnorm(residuals(anova1_model)); qqline(residuals(anova1_model))


  
#### 4. Two-Way ANOVA + Post-hoc

# Read Two-Way ANOVA data
anova2_data <- read_excel(file, sheet = "Two_Way_ANOVA")

# Fit two-way ANOVA model
anova2_model <- aov(response ~ factor1 * factor2, data = anova2_data)

# Summary of results
summary(anova2_model)

# Interaction plot
interaction.plot(anova2_data$factor1, anova2_data$factor2,
                 anova2_data$response,
                 main="Interaction Plot",
                 xlab="Factor1", trace.label="Factor2", col=c("red","blue"))

# Post-hoc test (Tukey)
TukeyHSD(anova2_model)

# Normality check
shapiro.test(residuals(anova2_model))
qqnorm(residuals(anova2_model)); qqline(residuals(anova2_model))
