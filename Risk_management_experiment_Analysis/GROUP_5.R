#################################
### STATISTICAL DATA ANALYSIS ###
###   TEL-AVIV UNIVERSITY     ###
###     MAXIM,ALON,OLGA       ###
###         GROUP 5           ###
#################################


data <- read.csv("data-processed-no_outliers.csv")
factor_levels <- read.csv("factor_levels.csv")


# DATA EXPLORATION
data
head(data)

dim(data)
nrow(data)
ncol(data)
summary(data)

class(data$Mean.Pumps)
class(data$Mean.time)
class(data$Risk)


hist(data$Risk_Initial, breaks = 10, col = "red", main = "Histogram Plot", xlab = "Risk_initial")
#To see the distribution of our level so that we can
#know if our factor ranges were good enough


#density test for the Risk
d_1 <- density(data$Risk)
plot(d_1)

#correlation matrix
res <- cor(data)
round(res, 2)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


########Model assumptions validation######### 

#FIRST FACTOR - PUMPS 

#Validate that the Residuals have constant variance with a Box plot:
boxplot(Mean.Pumps ~ Risk, data=data)
title("Mean.Pumps ~ Risk Box Plot")


#risk 0 - Levels 1-4
risk0 <- lm(Mean.Pumps.risk0~risk0, data=factor_levels)

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk0$residuals,ylim=c(-2,5))
qqline(risk0$residuals)

#Validate that the Residuals are independent of each other:
plot(risk0$residuals, ylim = c(-4,5))
lines(na.omit(risk0$residuals))

# risk1 - Levels 5-7
risk1 <- lm(Mean.Pumps.risk1~risk1, data=factor_levels)

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk1$residuals,ylim=c(-2,5))
qqline(risk1$residuals)

#Validate that the Residuals are independent of each other:
plot(risk1$residuals, ylim = c(-4,5))
lines(na.omit(risk1$residuals))

# risk2 - Levels 8-10
risk2 <- lm(Mean.Pumps.risk2~risk2, data=factor_levels)

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk2$residuals,ylim=c(-2,5))
qqline(risk2$residuals)

#Validate that the Residuals are independent of each other:
plot(risk2$residuals, ylim = c(-4,5))
lines(na.omit(risk2$residuals))


#SECOND FACTOR - TIME


#Validate that the Residuals have constant variance with a Box plot:
boxplot (Mean.time ~ Risk, data=data)
title("Mean.time ~ Risk Box Plot")

#risk 0 - Levels 1-4
risk0 <- lm(Mean.time.risk0~risk0, data=factor_levels)

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk0$residuals,ylim=c(-1,2))
qqline(risk0$residuals)

#Validate that the Residuals are independent of each other:
plot(risk0$residuals, ylim = c(-1,5))
lines(na.omit(risk0$residuals))

# risk1 - Levels 5-7
risk1 <- lm(Mean.time.risk1~risk1, data=factor_levels)# look how to do all graph in the same skala

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk1$residuals,ylim=c(-1,2))
qqline(risk1$residuals)

#Validate that the Residuals are independent of each other:
plot(risk1$residuals, ylim = c(-1,5))
lines(na.omit(risk1$residuals))

# risk2 - Levels 8-10
risk2 <- lm(Mean.time.risk2~risk2, data=factor_levels)

#Validate that the Residuals are normally distributed with a Q-Q plot:
qqnorm(risk2$residuals,ylim=c(-1,2))
qqline(risk2$residuals)

#Validate that the Residuals are independent of each other:
plot(risk2$residuals, ylim = c(-1,5))
lines(na.omit(risk2$residuals))


#######ANOVA#######

data$Risk <- as.numeric(data$Risk) # change data type from integer to numeric

# anova for PUMPS
lm.model_1 <- lm(Mean.Pumps ~ Risk, data = data)
anova(lm.model_1)


# anova for TIME
lm.model_2 <- lm(Mean.time ~ Risk, data = data)
anova(lm.model_2)


####Logistic regression####
binary.raw <- read.csv("data-processed-binary.csv", header = TRUE)

summary(binary.raw)

# only take relevant columns for our analysis 
binary1 <- subset(binary.raw, select = c(1,2))
binary2 <- subset(binary.raw, select = c(1, 3))

summary(binary1)
summary(binary2)
# change survived data type to categorical
binary1$binary.pumps <- as.factor(binary1$binary.pumps)
binary2$binary.time <- as.factor(binary2$binary.time)
# build a model based on Logistic Regression 
model.1 <- glm(binary.pumps ~ ., family = binomial, data = binary1)
summary(model.1)

model.2 <- glm(binary.time ~ ., family = binomial, data = binary2)
summary(model.2)

# predict results for new records using our model:
new.data1 <- data.frame(Risk = 0)
new.data2 <- data.frame(Risk = 1)
new.data3 <- data.frame(Risk = 2)

#predictions for model 1
predict(object = model.1, newdata = new.data1, type = 'response')
predict(object = model.1, newdata = new.data2, type = 'response')
predict(object = model.1, newdata = new.data3, type = 'response')

#predictions for model 2
predict(object = model.2, newdata = new.data1, type = 'response')
predict(object = model.2, newdata = new.data2, type = 'response')
predict(object = model.2, newdata = new.data3, type = 'response')


#Goodness of Fit for the logistic regression model
library(pscl) 
pR2(model.1)
pR2(model.2)
#The McFadden ranges from 0 to just under 1 
#Low McFadden value indicates that the model has no predictive power



