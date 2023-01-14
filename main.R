#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("dplyr")
#install.packages("GGally")
#install.packages("caret")
#install.packages("ggcorrplot")
#install.packages("glmnet")
# install.packages("olsrr")
# install.packages("MASS")
# install.packages("jtools")
library("caret")
library("ggplot2")
library("corrplot")
library("dplyr")
library("GGally")
library("ggcorrplot")
library("glmnet")
library("olsrr")
library("MASS")
library("jtools")
library("tidyverse")
library("broom")

df <- as.data.frame(read.csv("heart.csv"))
attach(df)
names(df)
head(df)
summary(df)

data <- df
set.seed(1223)
# Numerical matrix for corrplot representation
data <- model.matrix( ~ . , data = data) %>% 
  as_tibble() %>% 
  dplyr::select(-"(Intercept)")

sum(is.na(df)) # There are no missing values :)
df$ChestPainType[df$ChestPainType == "TA"] <- "Typical Angina"
df$ChestPainType[df$ChestPainType == "ATA"] <- "Atypical Angina"
df$ChestPainType[df$ChestPainType == "NAP"] <- "Non-Anginal Pain"
df$ChestPainType[df$ChestPainType == "ASY"] <- "Asymptomatic"

## EDA ##

## BOXPLOTS

ggplot(df, aes(x=Age)) + 
  geom_boxplot(fill="white") +
  theme_minimal() + ggtitle('Age Distribution')

ggplot(df, aes(x=RestingBP)) +
  geom_boxplot(fill="white") +
  theme_minimal() + ggtitle('Resting Blood Pressure Distribution (mm Hg)')

ggplot(df, aes(x=Cholesterol)) +
  geom_boxplot(fill="white") +
  theme_minimal() + ggtitle('Serum Cholesterol Distribution (mm/dl)')

ggplot(df, aes(x=MaxHR)) +
  geom_boxplot(fill="white") +
  theme_minimal() + ggtitle('Maximum Heart Rate Achieved Distribution')

ggplot(df, aes(x=Oldpeak)) +
  geom_boxplot(fill="white") +
  theme_minimal() + ggtitle('ST Depression Distribution')

pie(c(male_percentage,fem_percentage),
    labels = c('Male Patients', 'Female Patients'),
    col = c('Steelblue1','Palevioletred1'))

## OUTLIERS ADJUSTMENTS

# Since some variables have values = 0 (that is impossible), we set them equal to the mean 
df$Cholesterol[df$Cholesterol == 0] <- NA
chol_m <- round(mean(df$Cholesterol, na.rm = TRUE))
df$Cholesterol[is.na(df$Cholesterol)] <- chol_m

df$RestingBP[df$RestingBP == 0] <- NA
bp_m <- round(mean(df$RestingBP, na.rm = TRUE))
df$RestingBP[is.na(df$RestingBP)] <- bp_m

## COMPARISON GRAPHS

ggplot(df, aes(factor(HeartDisease), fill = Sex)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_bw() +
  labs(title = "Heart Disease across Gender",
       x = "Heart Disease", y = "Count") +
  scale_fill_manual(values = c("Palevioletred1", "Steelblue1")) +
  theme(legend.position = "bottom") # Non so come mettere la caption meglio, o due scritte sotto ogni coppia

ggplot(df, aes(factor(HeartDisease), fill = ExerciseAngina)) +
  geom_histogram(stat = "count", position = "dodge") + theme_bw() +
  labs(title = "Heart Disease across Exercise-induced Angina",
       x = "Heart Disease", y = "Count") +
  scale_fill_discrete() +
  theme(legend.position = "bottom")

ggplot(df, aes(factor(HeartDisease), fill = ChestPainType)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_bw() +
  labs(title = "Heart Disease by Chest Pain Type",
       x = "Heart Disease", y = "Count") +
  scale_fill_discrete() +
  theme(legend.position = "bottom")

ggplot(df, aes(factor(HeartDisease), fill = RestingECG)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_bw() +
  labs(title = "Heart Disease by ECG at rest",
       x = "Heart Disease", y = "Count") +
  scale_fill_discrete() +
  theme(legend.position = "bottom")

ggplot(df, aes(factor(HeartDisease), fill = ST_Slope)) +
  geom_histogram(stat = "count", position = "dodge") +
  theme_bw() +
  labs(title = "Heart Disease by EST slope",
       x = "Heart Disease", y = "Count") +
  scale_fill_discrete() +
  theme(legend.position = "bottom")


ggcorrplot(cor(data), hc.order = TRUE, lab = TRUE, lab_size = 2.5,
           type = 'lower', method = 'circle',
           colors = c('orangered', 'white', 'seagreen4'))


## MODEL: LOGISTIC REGRESSION ##

## ALL COVARIATES USED

# We split the dataset in two, one part for training (80%) and one for testing (20%)
sample <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[sample, ]
test <- data[!sample, ]

# We fit the model using the gml function
model <- glm(HeartDisease~., family="binomial", data=train)


# Importance of each variable
varImp(model)
vif(model)

# Check if multicolinearity is a problem (it is for values over 5, as a rule of thumb)

# This function predicts if the patients in our database have a probability of 
# having an hearth failure.
predicted <- predict(model, test, type="response")

probs <- ifelse(predicted > 0.5, 1, 0)

mean(probs == test$HeartDisease) # Gives the accuracy


model_data <- augment(model) %>% 
  mutate(index = 1:n())

model_data %>% top_n(3, .cooksd)
ggplot(model_data, aes(index, .std.resid)) + 
  geom_point(aes(color = HeartDisease), alpha = .5) +
  theme_bw()

model_data %>% 
  filter(abs(.std.resid) > 3)


confusionMatrix(as.factor(unname(test$HeartDisease)), as.factor(unname(probs))) # Accuracy confirmed


## STEPWISE FITTING

step_model <- glm(HeartDisease~., family = "binomial", data = train) %>%
  stepAIC(trace = FALSE)

step_model$null.deviance - step_model$deviance -> chi
step_model$df.null - step_model$df.residual -> chi_df
cat("F-statistic = " ,1  - pchisq(chi, chi_df) )

step_predicted <- predict(step_model, test, type = "response")
step_probs <- ifelse(step_predicted > 0.5, 1, 0)

mean(step_probs == test$HeartDisease)

step_model_data <- augment(step_model) %>% 
  mutate(index = 1:n())

step_model_data %>% top_n(3, .cooksd)

ggplot(model_data, aes(index, .std.resid)) + 
  geom_point(aes(color = HeartDisease), alpha = .5) +
  theme_bw() + 
  scale_color_gradientn(colours = rainbow(5))

step_model_data %>% 
  filter(abs(.std.resid) > 3)

vif(step_model)
varImp(step_model)

## LASSO FITTING

# Find the best lambda through cross-validation

x_train <- model.matrix(HeartDisease~., train)[,-1]

cv_lasso <- cv.glmnet(x_train, train$HeartDisease, alpha = 1, family = "binomial")

# Fit the model (one with min lambda and one with 1se)

lasso_model_min <- glmnet(x_train, train$HeartDisease, alpha = 1, family = "binomial",
                          lambda = cv_lasso$lambda.min)
lasso_model_se <- glmnet(x_train, train$HeartDisease, alpha = 1, family = "binomial",
                         lambda = cv_lasso$lambda.1se)

# Test the model

x_test <- model.matrix(HeartDisease~., test)[,-1]

lassomin_predicted <- predict(lasso_model_min, x_test, type = "response")
lassose_predicted <- predict(lasso_model_se, x_test, type = "response")

lassomin_probs <- ifelse(lassomin_predicted > 0.5, 1, 0)
lassose_probs <- ifelse(lassose_predicted > 0.5, 1, 0)

mean(lassomin_probs == test$HeartDisease)
mean(lassose_probs == test$HeartDisease)

## ASSESSING

summ(model)
summ(step_model)


plot(predicted, step_predicted, col = c("steelblue1", "orangered"))
plot(predicted, lassomin_predicted, col = c("steelblue1", "orangered"))
plot(predicted, lassose_predicted, col = c("steelblue1", "orangered"))
plot(step_predicted, lassomin_predicted, col = c("steelblue1", "orangered"))
plot(step_predicted, lassose_predicted, col = c("steelblue1", "orangered"))
plot(lassose_predicted, lassomin_predicted, col = c("steelblue1", "orangered"))


