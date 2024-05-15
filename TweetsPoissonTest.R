install.packages("caret")
install.packages("lmtest")
library(caret)
library(lmtest)
library(MASS)
tweetsData<-read.csv("sampleData.csv")
#10-cross validation
train_control <- trainControl(method = "cv", number = 10)
model <- train(Retweet_x ~ WC + Analytic + Clout + Authentic + Tone + BigWords + Dic + Linguistic + function. + pronoun + ppron, 
               data = tweetsData, 
               method = "glm", 
               family = "poisson", 
               trControl = train_control)
print(model)

# Residual test
residuals <- residuals(model)

# Plot residuals vs. predicted values
plot(fitted(model), residuals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residual Plot")

# BPtest
bp_test <- bptest(model$finalModel) 
print(bp_test) 

#negative binomial regression
model <- train(Retweet_x ~ WC + Analytic + Clout + Authentic + Tone + BigWords + Dic + Linguistic + function. + pronoun + ppron, 
               data = tweetsData, 
               method = "glm.nb", 
               trControl = train_control)

print(model)

residuals <- residuals(model)

# Residuals test
plot(fitted(model), residuals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residual Plot")

# BPtest
bp_test <- bptest(model$finalModel) 
print(bp_test)

#no 10-cross
model <- train(Retweet_x ~ WC + Analytic + Clout + Authentic + Tone + BigWords + Dic + Linguistic + function. + pronoun + ppron, 
               data = tweetsData, 
               method = "glm", 
               family = "poisson")
print(model)