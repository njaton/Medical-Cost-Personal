#Insurance Data Project
#Written By: Nick Jaton 
#Data From: Medical Cost Personal Datasets by Miri Choi
#Data Source URL: https://www.kaggle.com/mirichoi0218/insurance/home 

#Goal: To see how having children effects insurance cost 

library(plyr)
library(ggplot2)
library(plotly)
library(flexdashboard)

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
#plot to show how child amount and age effect charges value.
plot1 <- ggplot(insurance, aes(x=age, y=charges)) +
  geom_point(aes(col=children)) + ggtitle("Charges by Age & Number of Children") + 
  ylab("Charges Applied") + xlab("Age") + theme_bw() + 
  geom_smooth(method="loess", se=F) 
plot1 + theme(plot.title = element_text(hjust = .5))

#--------------------------------------------------

