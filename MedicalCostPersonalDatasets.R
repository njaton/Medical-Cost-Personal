#Insurance Data Project
#Written By: Nick Jaton 
#Data From: Medical Cost Personal Datasets by Miri Choi
#Data Source URL: https://www.kaggle.com/mirichoi0218/insurance/home 

#Goal: To see how having children effects insurance cost 

library(plyr)
library(ggplot2)
library(plotly)
library(flexdashboard)
library(DAAG)

insurance <- read.csv("~/Downloads/insurance.csv")

insuranceFrame <- as.data.frame(insurance)

#-----------------------------------------------------
#Get the average costs for individuals without children. 
insuranceNoChildren <- insuranceFrame[grepl(0, insuranceFrame$children),]
mean(insuranceNoChildren$charges) # $12365.98
sd(insuranceNoChildren$charges) # 12023.29 

#----------------------------------------------------
#Get the average costs for individuals with children. 
insuranceChildrenTotal <- insuranceFrame[!grepl(0, insuranceFrame$children),]
mean(insuranceChildrenTotal$charges) # $13949.94
sd(insuranceChildrenTotal$charges) # 12138.31

#---------------------------------------------------
#Comparisons 
CostCompare <- ddply(insuranceFrame,~children,summarise,CostAvg=mean((charges)))
sd(CostCompare$CostAvg) #2399.923

plot1 <- ggplot(CostCompare, aes(x=children, y=CostAvg)) +
  geom_bar(position="dodge", stat="identity", fill = "red4") + ggtitle("Average Charges by Child Number") + 
  ylab("Charges Applied") + xlab("# Children") + 
  geom_text(aes(label=round(CostAvg,1)), position = position_dodge(width=1), vjust=-.25) + theme_bw()
plot1 + theme(plot.title = element_text(hjust = .5), legend.position = "none")
#Generate Deviation Plot
CostCompare$Norm <- round((CostCompare$CostAvg - 
                            mean(CostCompare$CostAvg)) / sd(CostCompare$CostAvg), 2)
mean(CostCompare$CostAvg)
#Check to see if the values are below or above 0
CostCompare$aboveBelow <- ifelse(CostCompare$Norm < 0, "below", "above")
CostCompare <- CostCompare[order(CostCompare$Norm),]
CostCompare$children <- factor(CostCompare$children, levels = CostCompare$children)

plot2 <-ggplot(CostCompare, aes(x=children, y=Norm, label=Norm)) +
  geom_bar(stat='identity', aes(fill=aboveBelow), width=.5)  +
  scale_fill_manual(name="Deviation of Charges", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="cyan3", "below"="orangered1")) + 
  ylab("Deviation From Mean") + xlab("Number of Children") +
  labs(title= "Normalized Charges by Number of Children") + coord_flip() + theme_bw()
plot2 + theme(plot.title = element_text(hjust = .5),
              axis.text.x = element_text(angle=35, vjust=0.6))
#--------------------------------------------------
#Model testing
M1 <- lm(insurance$charges ~ insurance$age + insurance$children + insurance$bmi, insurance)
summary(M1) #R^2 = .1201
BIC(M1) # 28820.07
AIC(M1) # 28794.07

M2 <- lm(insurance$charges ~ insurance$age + insurance$children + insurance$bmi + 
             insurance$smoker, insurance)
print(M2)
summary(M2) #R^2 = 0.7497
BIC(M2) # 27145.23
AIC(M2) # 27114.04          <- This seems to be the best model choice 

M3 <- lm(insurance$charges ~ insurance$age + insurance$smoker, insurance)
summary(M3) #R^2 =  0.7214
BIC(M3) #27274.12
AIC(M3) #27253.32

M4 <- lm(insurance$charges ~ insurance$age + insurance$smoker +
           insurance$sex + insurance$children + insurance$bmi + insurance$region, insurance)
summary(M4) # R^2 = .7509
BIC(M4) #27167.5
AIC(M4) #27115.51


#From this data it can be seen that the region and sex aren't going to be as effective at 
#building our model but, smoker on the other hand is a huge factor for this data set. 
#-------------------------------------------------
#Regression graph on total using M2
plot(M2)

#M2 over scatterplot
ggplot(insurance, aes(x=age, y=charges)) +
  geom_point(aes(col=children)) + ggtitle("Charges by Age & Number of Children") + 
  ylab("Charges Applied") + xlab("age") + theme_bw() + 
  geom_abline(slope = 473.5, intercept = -12102.8 , color = "red", 
              show.legend = TRUE) 

#Assumed plot using stat_smooth
plot3 <- ggplot(insurance, aes(x=age, y=charges)) +
  geom_point(aes(col=children)) + ggtitle("Charges by Age") + 
  stat_smooth(method="lm", formula = y~poly(x,2)) +
  ylab("Charges Applied") + xlab("Age") + theme_bw()
plot3 + theme(plot.title = element_text(hjust = .5))
#-------------------------------------------------
#Seperate data to be used with linear regression
insuranceChildren1 <- insuranceFrame[grepl(1, insuranceFrame$children),]
insuranceChildren2 <- insuranceFrame[grepl(2, insuranceFrame$children),]
insuranceChildren3 <- insuranceFrame[grepl(3, insuranceFrame$children),]
insuranceChildren4 <- insuranceFrame[grepl(4, insuranceFrame$children),]
insuranceChildren5 <- insuranceFrame[grepl(5, insuranceFrame$children),]

#find correlation 
cor(insuranceNoChildren$charges, insuranceNoChildren$ag) #.325
cor(insuranceChildren1$charges, insuranceChildren1$age) #.309
cor(insuranceChildren2$charges, insuranceChildren2$age) #.217
cor(insuranceChildren3$charges, insuranceChildren3$age) #.285
cor(insuranceChildren4$charges, insuranceChildren4$age) #.190
cor(insuranceChildren5$charges, insuranceChildren5$age) #.724

#Perform a linear regression on each set 

#Assuming the indiviual is a smoker
lm(insuranceNoChildren$charges ~ insuranceNoChildren$age + insuranceNoChildren$smoker +
     insuranceNoChildren$bmi)
lm(insuranceChildren1$charges ~ insuranceChildren1$age + insuranceChildren1$smoker +
     insuranceChildren1$bmi)
lm(insuranceChildren2$charges ~ insuranceChildren2$age + insuranceChildren2$smoker + 
     insuranceChildren2$bmi)
lm(insuranceChildren3$charges ~ insuranceChildren3$age + insuranceChildren3$smoker +
     insuranceChildren3$bmi)
lm(insuranceChildren4$charges ~ insuranceChildren4$age + insuranceChildren4$smoker + 
     insuranceChildren4$bmi)
lm(insuranceChildren5$charges ~ insuranceChildren5$age + insuranceChildren5$smoker + 
     insuranceChildren5$bmi)
#--------------------------------------------------
#plot to show how child amount and age effect charges value.
# 0 = Red
# 1 = Blue
# 2 = Green 
# 3 = Grey
# 4 = Orange
# 5 = Purple

# Still working on combining all elements.  9/25/2018
plot4 <- ggplot(insurance, aes(x=age, y=charges)) +
  geom_point(aes(col=children)) + ggtitle("Charges by Age & Number of Children") + 
  ylab("Charges Applied") + xlab("Age") + theme_bw() + 
  geom_abline(slope = 258.3, intercept = -11165.1 , color = "red", 
              show.legend = TRUE) +
  geom_abline(slope = 278.5, intercept = -11533.4  , color = "blue", 
              show.legend = TRUE) + 
  geom_abline(slope =  207.6, intercept = -10810.3, color = "green", 
              show.legend = TRUE) +
  geom_abline(slope = 282.1, intercept = -14457.8, color = "grey", 
              show.legend = TRUE) + 
  geom_abline(slope =  242.8, intercept =  -10026.7, color = "orange", 
              show.legend = TRUE) +
  geom_abline(slope = 254.32, intercept = -373.26, color = "purple", 
              show.legend = TRUE) 
plot4 + theme(plot.title = element_text(hjust = .5))
#---------------------------------------------------
#Get into some testing 
child0Sum <- summary(lm(insuranceNoChildren$charges ~ insuranceNoChildren$age + 
                          insuranceNoChildren$smoker +  insuranceNoChildren$bmi))
print(child0Sum) #p-value: 2e-16, Current R^2 =  0.7792
#plot(lm(insuranceNoChildren$charges ~ insuranceNoChildren$age))

child1Sum <- summary(lm(insuranceChildren1$charges ~ insuranceChildren1$age +
                          insuranceChildren1$smoker +  insuranceChildren1$bmi))
print(child1Sum) #p-value: 2e-16, Current R^2 =  0.7179
#plot(lm(insuranceChildren1$charges ~ insuranceChildren1$age))

child2Sum <- summary(lm(insuranceChildren2$charges ~ insuranceChildren2$age +
                          insuranceChildren2$smoker +  insuranceChildren2$bmi))
print(child2Sum) #p-value: 2.17e-07, Current R^2 = 0.7173
#plot(lm(insuranceChildren2$charges ~ insuranceChildren2$age))

child3Sum <- summary(lm(insuranceChildren3$charges ~ insuranceChildren3$age +
                          insuranceChildren3$smoker +  insuranceChildren3$bmi))
print(child3Sum) #p-value: 6.58e-12, Current R^2 =  0.7927
#plot(lm(insuranceChildren3$charges ~ insuranceChildren3$age))

child4Sum <- summary(lm(insuranceChildren4$charges ~ insuranceChildren4$age +
                          insuranceChildren4$smoker +  insuranceChildren4$bmi))
print(child4Sum) #p-value: 0.07969, Current R^2 = 0.4328
#plot(lm(insuranceChildren4$charges ~ insuranceChildren4$age))

child5Sum <- summary(lm(insuranceChildren5$charges ~ insuranceChildren5$age + 
                          insuranceChildren5$smoker +  insuranceChildren5$bmi))
print(child5Sum) #p-value 1.49e-068, Current R^2 =  0.9006
#plot(lm(insuranceChildren4$charges ~ insuranceChildren4$age))
#-------------------------------------------------
#Go Back and train on partial datasets 
# No Children
Child0TrainingVal <- sample(1:nrow(insuranceNoChildren), 0.8*nrow(insuranceNoChildren))
child0trainSet <- insuranceNoChildren[Child0TrainingVal,]
child0Test <- insuranceNoChildren[-Child0TrainingVal,]                       

LRChild0 <- lm(charges ~ age + smoker + bmi, data = child0trainSet)
LRChild0Predict <- predict(LRChild0, child0Test)
accuracyCheck0 <- data.frame(cbind(actuals = child0Test$charges, pred = LRChild0Predict))
child0Acc <- cor(accuracyCheck0)
summary(accuracyCheck0) 

#One Child

Child1TrainingVal <- sample(1:nrow(insuranceChildren1), 0.8*nrow(insuranceChildren1))
child1trainSet <- insuranceChildren1[Child1TrainingVal,]
child1Test <- insuranceChildren1[-Child1TrainingVal,]                       

LRChild1 <- lm(charges ~ age + smoker + bmi, data = child1trainSet)
LRChild1Predict <- predict(LRChild1, child1Test)
accuracyCheck1 <- data.frame(cbind(actuals = child1Test$charges, pred = LRChild1Predict))
child1Acc <- cor(accuracyCheck1)
summary(accuracyCheck1) 

#Two Children

Child2TrainingVal <- sample(1:nrow(insuranceChildren2), 0.8*nrow(insuranceChildren2))
child2trainSet <- insuranceChildren2[Child2TrainingVal,]
child2Test <- insuranceChildren2[-Child2TrainingVal,]                       

LRChild2 <- lm(charges ~ age + smoker + bmi, data = child2trainSet)
LRChild2Predict <- predict(LRChild2, child2Test)
accuracyCheck2 <- data.frame(cbind(actuals = child2Test$charges, pred = LRChild2Predict))
child2Acc <- cor(accuracyCheck2)
summary(accuracyCheck2) 

#Three Children

Child3TrainingVal <- sample(1:nrow(insuranceChildren3), 0.8*nrow(insuranceChildren3))
child3trainSet <- insuranceChildren3[Child3TrainingVal,]
child3Test <- insuranceChildren3[-Child3TrainingVal,]                       

LRChild3 <- lm(charges ~ age + smoker + bmi, data = child3trainSet)
LRChild3Predict <- predict(LRChild3, child3Test)
accuracyCheck3 <- data.frame(cbind(actuals = child3Test$charges, pred = LRChild3Predict))
child3Acc <- cor(accuracyCheck3)
summary(accuracyCheck3) 

#Four Children 

Child4TrainingVal <- sample(1:nrow(insuranceChildren4), 0.8*nrow(insuranceChildren4))
child4trainSet <- insuranceChildren4[Child4TrainingVal,]
child4Test <- insuranceChildren4[-Child4TrainingVal,]                       

LRChild4 <- lm(charges ~ age + smoker + bmi, data = child4trainSet)
LRChild4Predict <- predict(LRChild4, child4Test)
accuracyCheck4 <- data.frame(cbind(actuals = child4Test$charges, pred = LRChild4Predict))
child4Acc <- cor(accuracyCheck4)
summary(accuracyCheck4) 

#Five Children

Child5TrainingVal <- sample(1:nrow(insuranceChildren5), 0.8*nrow(insuranceChildren5))
child5trainSet <- insuranceChildren5[Child5TrainingVal,]
child5Test <- insuranceChildren5[-Child5TrainingVal,]                       

LRChild5 <- lm(charges ~ age + smoker + bmi, data = child5trainSet)
LRChild5Predict <- predict(LRChild5, child5Test)
accuracyCheck5 <- data.frame(cbind(actuals = child5Test$charges, pred = LRChild5Predict))
child5Acc <- cor(accuracyCheck5)
summary(accuracyCheck5)
#------------------------------------------------------
