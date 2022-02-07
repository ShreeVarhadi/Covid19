Data analysis on Covid 19 data 

rm(list=ls())   #for the removing all previously stored variables
install.packages("Hmisc")
library(Hmisc)    #importing Hmisc library
Covid_data<- read.csv("C:/Users/hp/Desktop/covid_R/COVID19_line_list_data.csv")
summary(Covid_data) #to get the idea about the data

#cleaning data
#coercing variables into integer format and creating dummy variable of death where person is alive i.e value=0
Covid_data$death_dummy<-as.integer(Covid_data$death !=0)
unique(Covid_data$death_dummy)

#death rate of covid19
DR=sum(Covid_data$death_dummy)/nrow(Covid_data);DR
#death rate in to percent
DRpercent<-DR*100;DRpercent


#Claim1 :- people died due to covid are older than who survived

dead=subset(Covid_data,death_dummy==1)
alive=subset(Covid_data,death_dummy==0)

mean(dead$age, na.rm = TRUE) #to remove the missing values  
mean(alive$age,na.rm = TRUE) #to remove the missing values  


#statistical significance testing
t.test(dead$age,dead$age,alternative = "two.sided", conf.level = 0.95)
# normally we reject null hypothesis when P value<0.05
# here P value~0, we reject null hypothesis
#here the claim is statistically significant that people who died due to covid 
#older than survived.


#claim2 :- Male have higher chance of dying than female 


Male=subset(Covid_data,gender=="male")
Female=subset(Covid_data,gender=="female")

mean(Male$death_dummy, na.rm = TRUE)
mean(Female$death_dummy, na.rm = TRUE)

t.test(Male$death_dummy,Female$death_dummy, alternative = "two.sided",conf.level = 0.99)

# here P value(0.02)< 0.05 , we reject null hypothesis
#here the claim is statistically significant that males have the higher chance of dying than females.
