setwd("D:/HR Attrition Model")
hr<-read.csv("HR Attrition.csv")
head(names(hr))
colnames(hr)[1]<-"Age"
str(hr)

library(ggplot2)
ggplot(hr,aes(Attrition,fill=Attrition))+geom_bar()

#to find the propotion of attrition
prop.table(table(hr$Attrition))

#to check each variable infulence
library(grid)
library(gridExtra)

ageplot<-ggplot(hr,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
travelplot<-ggplot(hr,aes(BusinessTravel,fill=Attrition))+geom_bar()
rateplot<-ggplot(hr,aes(DailyRate,fill=Attrition))+geom_dotplot()
deptplot<-ggplot(hr,aes(JobRole,fill=Attrition))+geom_bar()
distanceplot<-ggplot(hr,aes(DistanceFromHome,fill=Attrition))+geom_bar()

grid.arrange(ageplot,travelplot,rateplot,deptplot,ncol=2,top = "Fig 1")

eduPlot <- ggplot(hr,aes(Education,fill=Attrition))+geom_bar()
edufieldPlot <- ggplot(hr,aes(EducationField,fill=Attrition))+geom_bar()
envPlot <- ggplot(hr,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()
genPlot <- ggplot(hr,aes(Gender,fill=Attrition))+geom_bar()
grid.arrange(distanceplot,eduPlot,edufieldPlot,envPlot,genPlot,ncol=2,top = "Fig 2")

hourlyPlot <- ggplot(hr,aes(HourlyRate,fill=Attrition))+geom_bar()
jobInvPlot <- ggplot(hr,aes(JobInvolvement,fill=Attrition))+geom_bar()
jobLevelPlot <- ggplot(hr,aes(JobLevel,fill=Attrition))+geom_bar()
jobSatPlot <- ggplot(hr,aes(JobSatisfaction,fill=Attrition))+geom_bar()
grid.arrange(hourlyPlot,jobInvPlot,jobLevelPlot,jobSatPlot,ncol=2,top = "Fig 3")

marPlot <- ggplot(hr,aes(MaritalStatus,fill=Attrition))+geom_bar()
monthlyIncPlot <- ggplot(hr,aes(MonthlyIncome,fill=Attrition))+geom_density()
monthlyRatePlot <- ggplot(hr,aes(MonthlyRate,fill=Attrition))+geom_density()
numCompPlot <- ggplot(hr,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
grid.arrange(marPlot,monthlyIncPlot,monthlyRatePlot,numCompPlot,ncol=2,top = "Fig 4")

overTimePlot <- ggplot(hr,aes(OverTime,fill=Attrition))+geom_bar()
hikePlot <- ggplot(hr,aes(PercentSalaryHike,Attrition))+geom_boxplot()
perfPlot <- ggplot(hr,aes(PerformanceRating,fill = Attrition))+geom_bar()
RelSatPlot <- ggplot(hr,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()
grid.arrange(overTimePlot,hikePlot,perfPlot,RelSatPlot,ncol=2,top = "Fig 5")

StockPlot <- ggplot(hr,aes(StockOptionLevel,fill = Attrition))+geom_bar()
workingYearsPlot <- ggplot(hr,aes(TotalWorkingYears,fill = Attrition))+geom_bar()
TrainTimesPlot <- ggplot(hr,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()
WLBPlot <- ggplot(hr,aes(WorkLifeBalance,fill = Attrition))+geom_bar()
grid.arrange(StockPlot,workingYearsPlot,TrainTimesPlot,WLBPlot,ncol=2,top = "Fig 6")

YearAtComPlot <- ggplot(hr,aes(YearsAtCompany,fill = Attrition))+geom_bar()
YearInCurrPlot <- ggplot(hr,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()
YearsSinceProm <- ggplot(hr,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
YearsCurrManPlot <- ggplot(hr,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()
grid.arrange(YearAtComPlot,YearInCurrPlot,YearsSinceProm,YearsCurrManPlot,ncol=2,top = "Fig 7")




hr$Attrition<-as.factor(hr$Attrition)
hr$Education<-as.factor(hr$Education)
hr$EnvironmentSatisfaction<-as.factor(hr$EnvironmentSatisfaction)
hr$JobInvolvement<-as.factor(hr$JobInvolvement)
hr$JobSatisfaction<-as.factor(hr$JobSatisfaction)
hr$PerformanceRating<-as.factor(hr$PerformanceRating)
hr$RelationshipSatisfaction<-as.factor(hr$RelationshipSatisfaction)
hr$WorkLifeBalance<-as.factor(hr$WorkLifeBalance)



#full model
hr_full_mod<-glm(Attrition~.,data = hr,family = "binomial")
glm(Attrition~.,data = hr,family = "binomial")
summary(hr_full_mod)

#Model Selection
#Here we have used the backward selection model to select one of the best fit model with significant coefficients.

#BACKWARD SELECTION
backwd <- step(hr_full_mod, trace=0)

summary(backwd)

#CROSS-VALIDATION
#We use 10-fold cross validation to validate our models.

f#Cross Validation for full model
library(boot)
cost <- function(Attrition,pi=0)
  
{
  
  mean(abs(Attrition-pi)>0.5)
  
}

HR.glm<-glm(Attrition~.,binomial,data=HR)

(cv.err<-cv.glm(HR,hrglm,cost,K=10)$delta[1])



backwd.glm<-glm(Attrition~.,binomial,data=HR)

(cv.err<-cv.glm(HR,backwd.glm,cost,K=10)$delta[1])



#MAKING PREDICTION
#This gives us the probability of attrition for each of the employer.
fpred<-predict(fullmod,type="response")
pred[1:5] 


#Estimating servival
#some of the employee had left the company but some of them are still working. 
#We will estimate the survival probabilities for each time point t (in years).
#loading the survival package

library(survival)
#attaching the variables

attach(HR)

#fitting the survival model using survfit taking Attrition == 1 (KM survival probabilities)

HR.surv <- survfit(Surv(YearsAtCompany, Attrition) ~ 1, conf.type="none")
summary(HR.surv) #summary of the HR.surv



