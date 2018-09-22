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
lm(insuranceNoChildren$charges ~ insuranceNoChildren$age)
lm(insuranceChildren1$charges ~ insuranceChildren1$age)
lm(insuranceChildren2$charges ~ insuranceChildren2$age)
lm(insuranceChildren3$charges ~ insuranceChildren3$age)
lm(insuranceChildren4$charges ~ insuranceChildren4$age)
lm(insuranceChildren5$charges ~ insuranceChildren5$age)
#--------------------------------------------------
#plot to show how child amount and age effect charges value.
#Regression colors are a little strange ATM. The graph should be updated by 9/22/2018
# 0 = Red
# 1 = Blue
# 2 = Green 
# 3 = Grey
# 4 = Orange
# 5 = Purple
plot3 <- ggplot(insurance, aes(x=age, y=charges)) +
  geom_point(aes(col=children)) + ggtitle("Charges by Age & Number of Children") + 
  ylab("Charges Applied") + xlab("Age") + theme_bw() + 
  geom_abline(slope = 237.9, intercept = 3219.0, color = "red", 
              show.legend = TRUE) +
  geom_abline(slope = 303.1, intercept = 771.3, color = "blue", 
              show.legend = TRUE) + 
  geom_abline(slope = 241.5, intercept = 5546.3, color = "green", 
              show.legend = TRUE) +
  geom_abline(slope = 287.4, intercept = 3407.3, color = "grey", 
              show.legend = TRUE) + 
  geom_abline(slope = 143.4, intercept = 8258.4, color = "orange", 
              show.legend = TRUE) +
  geom_abline(slope = 272.5, intercept = -919.0, color = "purple", 
              show.legend = TRUE) 
plot3 + theme(plot.title = element_text(hjust = .5))
#---------------------------------------------------
#Get into some testing 
child0Sum <- summary(lm(insuranceNoChildren$charges ~ insuranceNoChildren$age))
print(child0Sum) #p-value: 1.307e-15

child1Sum <- summary(lm(insuranceChildren1$charges ~ insuranceChildren1$age))
print(child1Sum) #p-value: 1.288e-08

child2Sum <- summary(lm(insuranceChildren2$charges ~ insuranceChildren2$age))
print(child2Sum) #p-value: 0.0007077

child3Sum <- summary(lm(insuranceChildren3$charges ~ insuranceChildren3$age))
print(child3Sum) #p-value: 0.0003042

child4Sum <- summary(lm(insuranceChildren4$charges ~ insuranceChildren4$age))
print(child4Sum) #p-value: 0.0003042

child5Sum <- summary(lm(insuranceChildren5$charges ~ insuranceChildren5$age))
print(child5Sum) #p-value .0006734
#-------------------------------------------------
#Go Back and train on partial datasets 
# No Children
Child0TrainingVal <- sample(1:nrow(insuranceNoChildren), 0.8*nrow(insuranceNoChildren))
child0trainSet <- insuranceNoChildren[Child0TrainingVal,]
child0Test <- insuranceNoChildren[-Child0TrainingVal,]                       

LRChild0 <- lm(charges ~ age, data = child0trainSet)
LRChild0Predict <- predict(LRChild0, child0Test)
accuracyCheck0 <- data.frame(cbind(actuals = child0Test$charges, pred = LRChild0Predict))
child0Acc <- cor(accuracyCheck0)
summary(accuracyCheck0) 

#One Child

Child1TrainingVal <- sample(1:nrow(insuranceChildren1), 0.8*nrow(insuranceChildren1))
child1trainSet <- insuranceChildren1[Child1TrainingVal,]
child1Test <- insuranceChildren1[-Child1TrainingVal,]                       

LRChild1 <- lm(charges ~ age, data = child1trainSet)
LRChild1Predict <- predict(LRChild1, child1Test)
accuracyCheck1 <- data.frame(cbind(actuals = child1Test$charges, pred = LRChild1Predict))
child1Acc <- cor(accuracyCheck1)
summary(accuracyCheck1) 

#Two Children

Child2TrainingVal <- sample(1:nrow(insuranceChildren2), 0.8*nrow(insuranceChildren2))
child2trainSet <- insuranceChildren2[Child2TrainingVal,]
child2Test <- insuranceChildren2[-Child2TrainingVal,]                       

LRChild2 <- lm(charges ~ age, data = child2trainSet)
LRChild2Predict <- predict(LRChild2, child2Test)
accuracyCheck2 <- data.frame(cbind(actuals = child2Test$charges, pred = LRChild2Predict))
child2Acc <- cor(accuracyCheck2)
summary(accuracyCheck2) 

#Three Children

Child3TrainingVal <- sample(1:nrow(insuranceChildren3), 0.8*nrow(insuranceChildren3))
child3trainSet <- insuranceChildren3[Child3TrainingVal,]
child3Test <- insuranceChildren3[-Child3TrainingVal,]                       

LRChild3 <- lm(charges ~ age, data = child3trainSet)
LRChild3Predict <- predict(LRChild3, child3Test)
accuracyCheck3 <- data.frame(cbind(actuals = child3Test$charges, pred = LRChild3Predict))
child3Acc <- cor(accuracyCheck3)
summary(accuracyCheck3) 

#Four Children

Child4TrainingVal <- sample(1:nrow(insuranceChildren4), 0.8*nrow(insuranceChildren4))
child4trainSet <- insuranceChildren4[Child4TrainingVal,]
child4Test <- insuranceChildren4[-Child4TrainingVal,]                       

LRChild4 <- lm(charges ~ age, data = child4trainSet)
LRChild4Predict <- predict(LRChild4, child4Test)
accuracyCheck4 <- data.frame(cbind(actuals = child4Test$charges, pred = LRChild4Predict))
child4Acc <- cor(accuracyCheck4)
summary(accuracyCheck4) 

#Five Children

Child5TrainingVal <- sample(1:nrow(insuranceChildren5), 0.8*nrow(insuranceChildren5))
child5trainSet <- insuranceChildren5[Child5TrainingVal,]
child5Test <- insuranceChildren5[-Child5TrainingVal,]                       

LRChild5 <- lm(charges ~ age, data = child5trainSet)
LRChild5Predict <- predict(LRChild5, child5Test)
accuracyCheck5 <- data.frame(cbind(actuals = child5Test$charges, pred = LRChild5Predict))
child5Acc <- cor(accuracyCheck5)
summary(accuracyCheck5) 
#-------------------------------------------------