#Importing libraries
library(tidyverse)
library(skimr)
library(ggplot2)
library(ggpubr)
library(gam)
library(splines)
library(boot)
library(tree)
library(gbm)
library(randomForest)


#Importing dataset
fifa = read.csv("C:/Users/HP/Documents/SU - MSBA/MSBA Lectures/Fall 20/PAI 793 - PA/Project/data.csv")
attach(fifa)
glimpse(fifa)
skim(fifa)
view(fifa)

#Dropping unnecessary columns
fifa = subset(fifa, select = -c(Club,ID,Special,International.Reputation,Work.Rate,Photo,Flag,Club.Logo, Loaned.From, Body.Type, Real.Face, Jersey.Number, Weak.Foot, Skill.Moves))
fifa = fifa[,-c(15:40)]

#Modyfying 'Value' column
fifa$Value = str_remove_all(fifa$Value, "â,¬" )
fifa$Value = str_replace_all(fifa$Value,"K", "000")
fifa$Value = str_replace_all(fifa$Value,"M", "")
fifa$Value = as.numeric(fifa$Value)
fifa <- fifa  %>% mutate(Value = if_else(fifa$Value < 1000 , Value * 1000000, Value))

#Modyfying 'Wage' column
fifa$Wage = str_remove_all(fifa$Wage, "â,¬" )
fifa$Wage = str_replace_all(fifa$Wage,"K", "000")
fifa$Wage = as.numeric(fifa$Wage)

#Modyfying 'Release.Clause' column
fifa$Release.Clause = str_remove_all(fifa$Release.Clause, "â,¬")
fifa$Release.Clause = str_replace_all(fifa$Release.Clause,"K", "000")
fifa$Release.Clause = str_replace_all(fifa$Release.Clause,"M", "")
fifa$Release.Clause = as.numeric(fifa$Release.Clause)
fifa = fifa  %>% mutate(Release.Clause = if_else(fifa$Release.Clause < 1000 , Release.Clause * 1000000, Release.Clause))

#Converting columns to numeric
fifa$Contract.Valid.Until = as.numeric(fifa$Contract.Valid.Until)
fifa$Age = as.numeric(fifa$Age)

#Removing NA values
fifa = na.omit(fifa)

#Creating a new variable 'Position.Class' using data from 'Position' variable
forward = c("ST", "LW", "RW", "LF", "RF", "RS","LS", "CF")
midfielder = c("CM","RCM","LCM", "CDM","RDM","LDM", "CAM", "LAM", "RAM", "RM", "LM")

fifa = fifa %>% mutate(Position.Class = as.factor( if_else(Position %in% forward, "Forward", 
                                                            if_else(Position %in% midfielder, "Midfielder", 
                                                                    if_else(Position == "GK", "Goal Keeper", "Defender")))))
rm(forward, midfielder)


#Modifying Height and Weight variable
fifa$Weight = as.numeric(str_remove_all(fifa$Weight, "lbs"))
fifa$Height = as.numeric(str_replace_all(fifa$Height,"'","."))
fifa$Height = fifa$Height*12

#Modifying Joined variable
fifa$Joined = as.numeric(str_sub(fifa$Joined, start = -4))
head(fifa$Joined)

fifa = subset(fifa, select = -c(Name, Position))

#Data Analysis
av = ggplot(fifa, aes(Age, Value)) + geom_point() + geom_smooth()
wv = ggplot(fifa, aes(Wage, Value)) + geom_point()+ geom_smooth()
ov = ggplot(fifa, aes(Overall, Value)) + geom_point() + geom_smooth()
pv = ggplot(fifa, aes(Potential, Value)) + geom_point() + geom_smooth()
cv = ggplot(fifa, aes(Contract.Valid.Until, Value)) + geom_point() + theme(axis.text.x = element_text(angle = -45))
rv = ggplot(fifa, aes(Release.Clause, Value)) + geom_point() + geom_smooth() + theme(axis.text.x = element_text(angle = -45))
pvc = ggplot(fifa, aes(Position.Class, Value)) + geom_point() + theme(axis.text.x = element_text(angle = -45))

#Combining all plots
plot1 = ggarrange(av,wv,ov,pv,cv,rv,pvc, nrow = 4, ncol = 2)
plot1

#Setting seed to 50
set.seed(50)

#Splitting the database into test and train datasets
train = sample(nrow(fifa), floor(nrow(fifa)*.7 ))
traindf = fifa[train,]
testdf = fifa[-train,]


## Running Multiple Linear Regression Model ##

set.seed(50)
lr = lm(Value ~.,
        data = fifa, subset = train)
summary(lr)

par(mfrow=c(2,2))
plot(lr)

#Calculating test MSE
lr.predict = predict(lr, newdata = fifa[-train,])
lr.test = data.frame(testdf, lr.predict)

(mean(((lr.test$Value)-(lr.test$lr.predict))^2))^0.5


## Adding Polynomial function to our model ##
set.seed(50)
poly_lr = lm(Value ~ Age+Wage+Overall+Potential+poly(Contract.Valid.Until,2)+Release.Clause+Position.Class, 
             data = fifa, subset = train)
summary(poly_lr)

#Calculating test MSE
poly.predict = predict(poly_lr, newdata = fifa[-train,])
poly.test = data.frame(testdf, poly.predict)

(mean(((poly.test$Value)-(poly.test$poly.predict))^2))^0.5


## Adding interaction variable to regression model ##
set.seed(50)
lr2 = lm(Value ~ Age*Wage+Overall+Potential+Contract.Valid.Until+Release.Clause+Position.Class, 
         data = fifa, subset = train)
summary(lr2)

par(mfrow=c(2,2))
plot(lr2)

#Calculating test MSE
lr2.predict = predict(lr2, newdata = fifa[-train,])
lr2.test = data.frame(testdf, lr2.predict)

(mean(((lr2.test$Value)-(lr2.test$lr2.predict))^2))^0.5


## GAM Model ##

set.seed(50)
lr4 = gam(Value ~ Age+poly(Contract.Valid.Until,2)+Wage+ns(Overall,2)+Potential+Release.Clause+Position.Class,
          data = fifa, subset = train)
summary(lr4)
coefficients(lr4)

#Calculating test MSE
lr4.predict = predict.Gam(lr4, newdata = fifa[-train,])
lr4.test = data.frame(testdf, lr4.predict)

(mean(((lr4.test$Value)-(lr4.test$lr4.predict))^2))^0.5


## Decision Tree Model ##

set.seed(50)
tree.fifa = tree(Value ~ Age+Wage+Overall+Potential+Contract.Valid.Until+Release.Clause+Position.Class,
                 data = fifa, subset = train)
summary(tree.fifa)
par(mfrow = c(1,1))
plot(tree.fifa)
text(tree.fifa,pretty=0)

yhat.tree=predict(tree.fifa, newdata=fifa[-train,])
fifa.test=data.frame(testdf, yhat.tree)

(mean(((fifa.test$Value)-(fifa.test$yhat.tree))^2))^0.5


## Generalized Boosted Model ##

set.seed(50)

gbm1 <- gbm(Value~Age+Wage+Overall+Potential+
              Contract.Valid.Until+Release.Clause+Position.Class,
            data = fifa[train,], n.trees = 500,
            distribution = "gaussian", 
            interaction.depth = 2)

summary(gbm1)

boost.predict = predict.gbm(gbm1, newdata = fifa[-train,], n.trees = 500)
boost.test = data.frame(testdf, boost.predict)

(mean(((boost.test$Value)-(boost.test$boost.predict))^2))^0.5


## Random Forest Model##

set.seed(50)

rand1 = randomForest(Value~Age+Wage+Overall+Potential+Contract.Valid.Until+Release.Clause+Position.Class,
                     data = fifa, subset=train, mtry=3, importance=TRUE)

rand1

#Calculating test MSE
rand.predict = predict(rand1, newdata = fifa[-train,])
rand.test = data.frame(testdf, rand.predict)

(mean(((rand.test$Value)-(rand.test$rand.predict))^2))^0.5

#Variable importance plot
varImpPlot(rand1)

#Calculating R2 value for this model
R2 = 1-(mean(abs(rand.test$Value-rand.test$rand.predict)/rand.test$Value))
R2



