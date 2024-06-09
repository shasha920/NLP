install.packages("caret")
install.packages("lmtest")
install.packages("pscl")
library(pscl)
library(caret)
library(lmtest)
library(MASS)
library(stats)
library(dplyr)
tweetsData<-read.csv("sampleData.csv")

#Log transformation
tweetsData <- tweetsData %>%
  mutate(
    log_Authentic = log(Authentic + 1),
    log_Tone = log(Tone + 1),
    log_WPS = log(WPS + 1),
    log_BigWords = log(BigWords + 1),
    log_pronoun = log(pronoun + 1),
    log_ppron = log(ppron + 1)
  )
#Linear Regression include all variable
model <- lm(Retweet_x ~ WC + Analytic + Clout + log_Authentic + log_Tone + log_WPS + log_BigWords + Dic + Linguistic + function. + log_pronoun + log_ppron, 
            data = tweetsData)
summary(model)

#Linear Regression 7 variable
model <- lm(Retweet_x ~ WC + Analytic + Clout + log_Authentic + log_Tone + log_WPS + log_BigWords, 
            data = tweetsData)
summary(model)

#Linear Regression 4 variable
model <- lm(Retweet_x ~ Analytic + Clout + log_Authentic + log_Tone, 
            data = tweetsData)
summary(model)

#Log with Possion
model <- glm(Retweet_x ~ WC + Analytic + Clout + log_Authentic + log_Tone + log_WPS + log_BigWords + Dic + Linguistic + function. + log_pronoun + log_ppron, 
             data = tweetsData, 
             family = poisson(link = "log"))
summary(model)

#Log with NB
model_nb <- glm.nb(Retweet_x ~ WC + Analytic + Clout + log_Authentic + log_Tone + log_WPS + log_BigWords + Dic + Linguistic + function. + log_pronoun + log_ppron, 
                   data = tweetsData, 
                   link = log)
summary(model_nb)

#Possion with log link
model <- glm(Retweet_x ~ WC + Analytic + Clout + Authentic + Tone + BigWords + Dic + Linguistic + function. + pronoun + ppron, 
             data = tweetsData, 
             family = poisson(link = "log"))
summary(model)


#negative binomial regression
model_nb <- glm.nb(Retweet_x ~ WC + Analytic + Clout + Authentic + Tone + BigWords + Dic + Linguistic + function. + pronoun + ppron, 
                   data = tweetsData)
print(summary(model_nb))

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
