#Project 1 - Reducing selection competencies using Machine Learning

#Performed Different Analysis
# 1 - Linear Regression for RC Growth
# 2 - Linear Regression for Math Level
# 3 - Linear Regression for Top 25% 
# 4 - Multinomial Logistic Regression for Quartiles
# 5 - Proportional Odd Logistic Regression for Quartiles
# 6 - Linear Regression for Pune Fellows (RC and Math)
# 7 - K Means Cluster Analysis
# 8 - Expectation Maximization Cluster Analysis 
# 9 - K Means Cluster Analysis for Continuous variables
# 10 - Linear Regression for Program Managers
# 11 - Linear Regression for Mid Year RC Growth
# 12 - Linear Regression for AMCAT Scores
# 13 - Principal Component Analysis

#Regression Analysis
#--------------------------------------------------------------------------#
#Regression Analysis for RC Growth
#Importing data sheet
`EoYMergedTrn&Sel.(FINAL_NoFES)` <- read.csv("C:/Users/Divyansh/Downloads/EoYMergedTrn&Sel (FINAL_NoFES).csv", stringsAsFactors=FALSE)
View(`EoYMergedTrn&Sel.(FINAL_NoFES)`)
z <- `EoYMergedTrn&Sel.(FINAL_NoFES)`
new_z <- NULL
data1 <- z$Resp2014BOY.EOYRCGrowth
#Removing Duplicating values
dupligrow <- which(table(data1)>1)
duplival <- as.double(names(dupligrow))
dupliloc=list()
for(i in 1:length(duplival))
{
  dupliloc[[i]] <- which(data1==duplival[i])
}
unlisted <- unlist(dupliloc)
for(i in 1:60)
{
  data2[i] <- dupliloc[[i]][1]
}
loc <- union(setdiff(l,unlisted),data2)
new_z <- z[loc,]

#Renaming column names from the excel sheet
tRCGrowth <- new_z$Resp2014BOY.EOYRCGrowth
tAge <- new_z$Age
tGrade <- as.factor(new_z$Grade)
tYOI <- new_z$Yearofintervention
tStudents <- new_z$TotalStudents
tInstitute <- new_z$InstituteAttending
tCity <- new_z$City
tLeadSelf <- new_z$LeadSelftoAchieveFinalScore
tLeadOthers <- new_z$LeadOtherstoAchieveFinalScore
tGrit <- new_z$GritFinalScore
tFit <- new_z$FitFinalScore
tContinuousLearning <- new_z$ContinuousLearningFinalScore
tTeachesMath <- new_z$TeachesMath
tTeachesMath <- as.integer(tTeachesMath)
tTeachesWriting <- new_z$TeachesWriting
tTeachesWriting <- as.integer(tTeachesWriting)
tSchool <- new_z$Schooltype
tSchool <- tSchool[-nula]
tBOYMathMean <- new_z$X2014BOYOverallMathMastery.Mean.
tTeachesRC <- as.numeric(new_z$TeachesRC)

new_z$CoA1 <- as.numeric(new_z$CoA)
new_z$Rigour1 <- as.numeric(new_z$Rigour)
new_z$V.M1 <- as.numeric(new_z$V.M)
new_z$A.E1 <- as.numeric(new_z$A.E)
new_z$PT1 <- as.numeric(new_z$PT)
new_z$EE1 <- as.numeric(new_z$EE)
new_z$CA1 <- as.numeric(new_z$CA)

#Data Cleaning Started

#Changing decimal values of LDJs to range values for better interpretation
new_z$CoA2[which(new_z$CoA1==1)] <- '1-2'    
new_z$CoA2[which(new_z$CoA1==1.5)] <- '1-2'
new_z$CoA2[which(new_z$CoA1==2)] <- '2-3'    
new_z$CoA2[which(new_z$CoA1==2.5)] <- '2-3'
new_z$CoA2[which(new_z$CoA1==3)] <- '3-4'    
new_z$CoA2[which(new_z$CoA1==3.5)] <- '3-4'
new_z$CoA2[which(new_z$CoA1==4)] <- '4-5'    
new_z$CoA2[which(new_z$CoA1==4.5)] <- '4-5'

new_z$Rigour2[which(new_z$Rigour1==1)] <- '1-2'    
new_z$Rigour2[which(new_z$Rigour1==1.5)] <- '1-2'
new_z$Rigour2[which(new_z$Rigour1==2)] <- '2-3'    
new_z$Rigour2[which(new_z$Rigour1==2.5)] <- '2-3'
new_z$Rigour2[which(new_z$Rigour1==3)] <- '3-4'    
new_z$Rigour2[which(new_z$Rigour1==3.5)] <- '3-4'
new_z$Rigour2[which(new_z$Rigour1==4)] <- '4-5'    
new_z$Rigour2[which(new_z$Rigour1==4.5)] <- '4-5'

new_z$V.M2[which(new_z$V.M1==1)] <- '1-2'    
new_z$V.M2[which(new_z$V.M1==1.5)] <- '1-2'
new_z$V.M2[which(new_z$V.M1==2)] <- '2-3'    
new_z$V.M2[which(new_z$V.M1==2.5)] <- '2-3'
new_z$V.M2[which(new_z$V.M1==3)] <- '3-4'    
new_z$V.M2[which(new_z$V.M1==3.5)] <- '3-4'
new_z$V.M2[which(new_z$V.M1==4)] <- '4-5'    
new_z$V.M2[which(new_z$V.M1==4.5)] <- '4-5'

new_z$A.E2[which(new_z$A.E1==1)] <- '1-2'    
new_z$A.E2[which(new_z$A.E1==1.5)] <- '1-2'
new_z$A.E2[which(new_z$A.E1==2)] <- '2-3'    
new_z$A.E2[which(new_z$A.E1==2.5)] <- '2-3'
new_z$A.E2[which(new_z$A.E1==3)] <- '3-4'    
new_z$A.E2[which(new_z$A.E1==3.5)] <- '3-4'
new_z$A.E2[which(new_z$A.E1==4)] <- '4-5'    
new_z$A.E2[which(new_z$A.E1==4.5)] <- '4-5'

new_z$PT2[which(new_z$PT1==1)] <- '1-2'    
new_z$PT2[which(new_z$PT1==1.5)] <- '1-2'
new_z$PT2[which(new_z$PT1==2)] <- '2-3'    
new_z$PT2[which(new_z$PT1==2.5)] <- '2-3'
new_z$PT2[which(new_z$PT1==3)] <- '3-4'    
new_z$PT2[which(new_z$PT1==3.5)] <- '3-4'
new_z$PT2[which(new_z$PT1==4)] <- '4-5'    
new_z$PT2[which(new_z$PT1==4.5)] <- '4-5'

new_z$EE2[which(new_z$EE1==1)] <- '1-2'    
new_z$EE2[which(new_z$EE1==1.5)] <- '1-2'
new_z$EE2[which(new_z$EE1==2)] <- '2-3'    
new_z$EE2[which(new_z$EE1==2.5)] <- '2-3'
new_z$EE2[which(new_z$EE1==3)] <- '3-4'    
new_z$EE2[which(new_z$EE1==3.5)] <- '3-4'
new_z$EE2[which(new_z$EE1==4)] <- '4-5'    
new_z$EE2[which(new_z$EE1==4.5)] <- '4-5'

new_z$CA2[which(new_z$CA1==1)] <- '1-2'    
new_z$CA2[which(new_z$CA1==1.5)] <- '1-2'
new_z$CA2[which(new_z$CA1==2)] <- '2-3'    
new_z$CA2[which(new_z$CA1==2.5)] <- '2-3'
new_z$CA2[which(new_z$CA1==3)] <- '3-4'    
new_z$CA2[which(new_z$CA1==3.5)] <- '3-4'
new_z$CA2[which(new_z$CA1==4)] <- '4-5'    
new_z$CA2[which(new_z$CA1==4.5)] <- '4-5'

#Converting LDJs to factor (Factor is a data type used for regression algorithms)
new_z$CoA2 <- as.factor(new_z$CoA2)
new_z$Rigour2 <- as.factor(new_z$Rigour2)
new_z$V.M2 <- as.factor(new_z$V.M2)
new_z$A.E2 <- as.factor(new_z$A.E2)
new_z$PT2 <- as.factor(new_z$PT2)
new_z$EE2 <- as.factor(new_z$EE2)
new_z$CA2 <- as.factor(new_z$CA2)

tCoA <- new_z$CoA2
tRigour <- new_z$Rigour2
tVM <- new_z$V.M2
tAE <- new_z$A.E2
tPT <- new_z$PT2
tEE <- new_z$EE2
tCA <- new_z$CA2
tEOYMathMean <- new_z$X2014EOYOverallMathMastery.Mean.
tBOYRCLevel <- new_z$Resp2014BOYRClevel

#Making final data set
t3 <- cbind.data.frame(tRCGrowth,tAge,tGrade,tYOI,tStudents,tCity,tInstitute,tLeadSelf,tLeadOthers,tGrit,tFit,tContinuousLearning,tTeachesRC,tCoA,tCA,tPT,tAE,tVM,tEE,tBOYRCLevel)
nula <- which(is.na(tYOI))
t3 <- t3[-nula,]
t3 <- t3[-c(297,298,299),]
t3$tYOI <- as.character(t3$tYOI)
for(i in 2:length(t3$tYOI))
{
  if(t3$tYOI[i]=='1st YOI')
    t3$tYOI[i] <- 1
  if(t3$tYOI[i]=='5th YOI')
    t3$tYOI[i] <- 5
  if(t3$tYOI[i]=='2nd YOI')
    t3$tYOI[i] <- 2
  if(t3$tYOI[i]=='3rd YOI')
    t3$tYOI[i] <- 3
  if(t3$tYOI[i]=='6th YOI')
    t3$tYOI[i] <- 6
  if(t3$tYOI[i]=='4th YOI')
    t3$tYOI[i] <- 4
}
t3$tYOI <- as.factor(t3$tYOI)

t3 <- t3[-1,]
t3$tInstitute <- as.character(t3$tInstitute)
for(i in 1:length(t3$tInstitute))
{
  if(t3$tInstitute[i]=='Institute 1')
    t3$tInstitute[i] <- 0
  if(t3$tInstitute[i]=='Institute 2')
    t3$tInstitute[i] <- 1
}

t3$tInstitute <- as.factor(t3$tInstitute)
#Changing tSchool
tSchool <- tSchool[-c(1,297,298,299)]
t3 <- cbind(t3,tSchool)
t3$tSchool <- as.character(t3$tSchool)
t3 <- t3[-1,]
for(i in 1:length(t3$tSchool))
{
  if(t3$tSchool[i]=='Private School')
    t3$tSchool[i] <- 0
  if(t3$tSchool[i]=='Government School')
    t3$tSchool[i] <- 1
}
t3$tSchool <- as.factor(t3$tSchool)

#Changing tCity to numerical values
t3$tCity <- as.character(t3$tCity)
for(i in 1:length(t3$tCity))
{
  if(t3$tCity[i]=='Pune')
    t3$tCity[i] <- 1
  if(t3$tCity[i]=='Chennai')
    t3$tCity[i] <- 2
  if(t3$tCity[i]=='Mumbai')
    t3$tCity[i] <- 3
  if(t3$tCity[i]=='Hyderabad')
    t3$tCity[i] <- 4
  if(t3$tCity[i]=='Delhi')
    t3$tCity[i] <- 5
  if(t3$tCity[i]=='Ahmedabad')
    t3$tCity[i] <- 6
}
t3$tCity <- as.factor(t3$tCity)
#Removing N/As (empty rows) from the data frame
t3_new <- t3[complete.cases(t3),]
t3_new <- t3_new[complete.cases(t3_new),]
loca1 <- which(t3_new$tTeachesRC==1)
t3_new <- t3_new[loca1,]

#using Linear Model with RC Growth as dependent variable
fit1 <- lm(t3_new$tRCGrowth~t3_new$tAge+t3_new$tGrade+t3_new$tYOI+t3_new$tStudents+t3_new$tCity+t3_new$tInstitute+t3_new$tLeadSelf+t3_new$tLeadOthers+t3_new$tGrit+t3_new$tFit+t3_new$tContinuousLearning+t3_new$tCoA+t3_new$tCA+t3_new$tPT+t3_new$tAE+t3_new$tVM+t3_new$tEE+t3_new$tBOYRCLevel,data = t3_new)
#----------------------------------------------------------------------#

#Regression Analysis of Math Mean

#Making a new data set 
t30 <- cbind.data.frame(tRCGrowth,tAge,tGrade,tYOI,tStudents,tCity,tInstitute,tLeadSelf,tLeadOthers,tGrit,tFit,tContinuousLearning,tTeachesRC,tTeachesWriting,tCoA,tCA,tPT,tAE,tVM,tEE,tTeachesMath,tEOYMathMean)
t30_new <- as.data.frame(t30)
#Removing N/As from the data frame
t30_new <- t30_new[complete.cases(t30_new),]
t30_new <- t30_new[complete.cases(t30_new),]
loca2 <- which(t30_new$tTeachesMath==1)
t30_new <- t30_new[loca2,]
View(t30_new)
#Using Linear model for Math Mean
fit2 <- lm(t30_new$tEOYMathMean~t30_new$tAge+t30_new$tGrade+t30_new$tYOI+t30_new$tStudents+t30_new$tCity+t30_new$tInstitute+t30_new$tLeadSelf+t30_new$tLeadOthers+t30_new$tGrit+t30_new$tFit+t30_new$tContinuousLearning+t30_new$tCoA+t30_new$tCA+t30_new$tPT+t30_new$tAE+t30_new$tVM+t30_new$tEE+t30_new$tTeachesRC+t30_new$tTeachesWriting,data = t30_new)
t3 <- cbind.data.frame(tRCGrowth,tAge,tGrade,tYOI,tStudents,tCity,tInstitute,tLeadSelf,tLeadOthers,tGrit,tFit,tContinuousLearning,tTeachesRC,tCoA,tCA,tPT,tAE,tVM,tEE,tBOYRCLevel)
t4_new <- as.data.frame(t3)

#-------------------------------------------------#
#Performing regression analysis of RC Growth for TeachesRC = 1

#Removing NAs from the data frame
t4_new <- t4_new[complete.cases(t4_new),]
t4_new <- t4_new[complete.cases(t4_new),]
loca1 <- which(t4_new$tTeachesRC==1)
t4_new <- t4_new[loca1,]
View(t4_new)
#Fitting linear model
fit1 <- lm(t4_new$tRCGrowth~t4_new$tAge+t4_new$tGrade+t4_new$tYOI+t4_new$tStudents+t4_new$tCity+t4_new$tInstitute+t4_new$tLeadSelf+t4_new$tLeadOthers+t4_new$tGrit+t4_new$tFit+t4_new$tContinuousLearning+t4_new$tCoA+t4_new$tCA+t4_new$tPT+t4_new$tAE+t4_new$tVM+t4_new$tEE+t4_new$tBOYRCLevel,data = t4_new)


#----------------------------------------------------------------------------------#

#Top 25%tile Analysis

#Sorting data in decreasing order in terms of RC Growth
t5_new <- t3_new
t5_new.sorted <- t5_new[order(t5_new$tRCGrowth),]
t5_new.sorted <- t5_new.sorted[-1,]
dim(t5_new.sorted)
t5_new.sorted <- t5_new.sorted[-c(1,2),]

#Creating a new column named 'Quartile'
t5_new.sorted$quartile <- rep(1:4, each=248/4)

#Selecting top 25% 
t5_new.top <- t5_new.sorted[which(t5_new.sorted$quartile==4),]
t5_new.top
View(t5_new.top)

#Fitting Linear Model 
fit3 <- lm(t5_new.top$tRCGrowth~t5_new.top$tAge+t5_new.top$tGrade+t5_new.top$tYOI+t5_new.top$tStudents+t5_new.top$tCity+t5_new.top$tInstitute+t5_new.top$tLeadSelf+t5_new.top$tLeadOthers+t5_new.top$tGrit+t5_new.top$tFit+t5_new.top$tContinuousLearning+t5_new.top$tCoA+t5_new.top$tCA+t5_new.top$tPT+t5_new.top$tAE+t5_new.top$tVM+t5_new.top$tEE+t5_new.top$tBOYRCLevel,data = t5_new.top)
class(t5_new.top$tCity)

#Cleaning data
t5_new.top$tGrade <- as.factor(t5_new.top$tGrade)
t5_new.top$tYOI <- as.factor(t5_new.top$tYOI)
t5_new.top$tCity <- as.factor(t5_new.top$tCity)
t5_new.top$tInstitute <- as.factor(t5_new.top$tInstitute)
t5_new.top$tCoA <- as.factor(t5_new.top$tCoA)
t5_new.top$tCA <- as.factor(t5_new.top$tCA)
t5_new.top$tEE <- as.factor(t5_new.top$tEE)
t5_new.top$tPT <- as.factor(t5_new.top$tPT)
t5_new.top$tVM <- as.factor(t5_new.top$tVM)
t5_new.top$tAE <- as.factor(t5_new.top$tAE)
fit3 <- lm(t5_new.top$tRCGrowth~t5_new.top$tAge+t5_new.top$tGrade+t5_new.top$tYOI+t5_new.top$tStudents+t5_new.top$tCity+t5_new.top$tInstitute+t5_new.top$tLeadSelf+t5_new.top$tLeadOthers+t5_new.top$tGrit+t5_new.top$tFit+t5_new.top$tContinuousLearning+t5_new.top$tCoA+t5_new.top$tCA+t5_new.top$tPT+t5_new.top$tAE+t5_new.top$tVM+t5_new.top$tEE+t5_new.top$tBOYRCLevel,data = t5_new.top)

#---------------------------------------------------------------------------#
#Implementing Regression Analysis for different quartiles as ordinal variables

#We need to use Multinomial Logistic Regression

#Converting all variables to factor type
t5_new.sorted$tGrade <- as.factor(t5_new.sorted$tGrade)
t5_new.sorted$tYOI <- as.factor(t5_new.sorted$tYOI)
t5_new.sorted$tCity <- as.factor(t5_new.sorted$tCity)
t5_new.sorted$tInstitute <- as.factor(t5_new.sorted$tInstitute)
t5_new.sorted$tCoA <- as.factor(t5_new.sorted$tCoA)
t5_new.sorted$tCA <- as.factor(t5_new.sorted$tCA)
t5_new.sorted$tEE <- as.factor(t5_new.sorted$tEE)
t5_new.sorted$tPT <- as.factor(t5_new.sorted$tPT)
t5_new.sorted$tVM <- as.factor(t5_new.sorted$tVM)
t5_new.sorted$tAE <- as.factor(t5_new.sorted$tAE)
t5_new.sorted$quartile <- as.factor(t5_new.sorted$quartile)
View(t5_new.sorted)

#Importing neural network package
library(nnet)

#Fitting multinomial model
mod <- multinom(t5_new.sorted$quartile ~t5_new.sorted$tAge+t5_new.sorted$tGrade+t5_new.sorted$tYOI+t5_new.sorted$tStudents+t5_new.sorted$tCity+t5_new.sorted$tInstitute+t5_new.sorted$tLeadSelf+t5_new.sorted$tLeadOthers+t5_new.sorted$tGrit+t5_new.sorted$tFit+t5_new.sorted$tContinuousLearning+t5_new.sorted$tCoA+t5_new.sorted$tCA+t5_new.sorted$tPT+t5_new.sorted$tAE+t5_new.sorted$tVM+t5_new.sorted$tEE+t5_new.sorted$tBOYRCLevel,data = t5_new.sorted)

#Summary of the regression
z <- summary(mod)$coefficients/summary(mod)$standard.errors

# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
View(exp(coef(mod)))

#---------------------------------------------------------------------------#
#Implementing another algorithm for better interpretation

#Ordinal Logistic Regression

#Importing required package
library(MASS)

#Removing Grade 1 as its population is very less
which(t5_new.sorted$tGrade==1)
x12 <- which(t5_new.sorted$tGrade==1)
t5_new.sorted <- t5_new.sorted[-x12,]

#Using Proportional Odds Regression Model
op <- polr(t5_new.sorted$quartile ~t5_new.sorted$tAge+t5_new.sorted$tGrade+t5_new.sorted$tYOI+t5_new.sorted$tStudents+t5_new.sorted$tCity+t5_new.sorted$tInstitute+t5_new.sorted$tLeadSelf+t5_new.sorted$tLeadOthers+t5_new.sorted$tGrit+t5_new.sorted$tFit+t5_new.sorted$tContinuousLearning+t5_new.sorted$tCoA+t5_new.sorted$tCA+t5_new.sorted$tPT+t5_new.sorted$tAE+t5_new.sorted$tVM+t5_new.sorted$tEE+t5_new.sorted$tBOYRCLevel,data = t5_new.sorted, Hess=TRUE,na.action=na.omit)
summary(op)
ctable1 <- coef(summary(op))

#Adding p - value to the table along with t - value for better interpretation
p <- 0
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p)
print(op)

#Creating a separate table for variables with some significance
ctable1[which(ctable1[,4]<0.05),]
ctable1[which(ctable1[,4]<0.001),]

#Performing this regression for Pune fellows alone
#-----------------------------------------------------------------------#

#Selecting Pune Fellows by creating a booloen variable
#Data Cleaning
for(i in 1:245)
{
  if(t5_new.sorted1$tCity[i] == 1)
    t5_new.sorted1$Pune[i] <- 1
  else 
    t5_new.sorted1$Pune[i] <- 0
}
t5_new.sorted1$Pune = as.factor(t5_new.sorted1$Pune)
#Fitting POLR Model
op1 <- polr(t5_new.sorted1$quartile ~t5_new.sorted1$tAge+t5_new.sorted1$tGrade+t5_new.sorted1$tYOI+t5_new.sorted1$tStudents+t5_new.sorted1$Pune+t5_new.sorted1$tInstitute+t5_new.sorted1$tLeadSelf+t5_new.sorted1$tLeadOthers+t5_new.sorted1$tGrit+t5_new.sorted1$tFit+t5_new.sorted1$tContinuousLearning+t5_new.sorted1$tCoA+t5_new.sorted1$tCA+t5_new.sorted1$tPT+t5_new.sorted1$tAE+t5_new.sorted1$tVM+t5_new.sorted1$tEE+t5_new.sorted1$tBOYRCLevel,data = t5_new.sorted1, Hess=TRUE,na.action=na.omit)
summary(op1)
ctable1 <- coef(summary(op1))
#Calculating P - value
p <- 0
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p)
print(op1)
#Displaying significant variables from the table created above.
ctable1[which(ctable1[,4]<0.05),]
xyz <- ctable1[which(ctable1[,4]<0.05),]
cbind(exp(xyz[,1]))

#--------------------------------------------------------#
#Pune Analysis RC Growth

#Data Cleaning
t6_new <- t3_new
t6_new$tCity
t6_new.pune <- t6_new[which(t6_new$tCity==1),]
View(t6_new.pune)
t6_new.pune$tGrade <- as.factor(t6_new.pune$tGrade)
t6_new.pune$tYOI <- as.factor(t6_new.pune$tYOI)
t6_new.pune$tInstitute <- as.factor(t6_new.pune$tInstitute)
t6_new.pune$tCoA <- as.factor(t6_new.pune$tCoA)
t6_new.pune$tCA <- as.factor(t6_new.pune$tCA)
t6_new.pune$tEE <- as.factor(t6_new.pune$tEE)
t6_new.pune$tPT <- as.factor(t6_new.pune$tPT)
t6_new.pune$tVM <- as.factor(t6_new.pune$tVM)
t6_new.pune$tAE <- as.factor(t6_new.pune$tAE)

#Fitting Model
fit4 <- felm(t6_new.pune$tRCGrowth~t6_new.pune$tAge+t6_new.pune$tGrade+t6_new.pune$tYOI+t6_new.pune$tStudents+t6_new.pune$tInstitute+t6_new.pune$tLeadSelf+t6_new.pune$tLeadOthers+t6_new.pune$tGrit+t6_new.pune$tFit+t6_new.pune$tContinuousLearning+t6_new.pune$tCoA+t6_new.pune$tCA+t6_new.pune$tPT+t6_new.pune$tAE+t6_new.pune$tVM+t6_new.pune$tBOYRCLevel,data = t6_new.pune)
summary(fit4)
#-------------------------------------------------------------------#
#Pune Math Analysis

#Data Cleaning
#Extacting Math scores from the original data set
loza <- as.numeric(row.names(t3_new))
tEOYMath[loza]
t4_new <- cbind.data.frame(t3_new,tEOYMath[loza],tBOYMath[loza])
t7_new <- t4_new
t7_new$tCity
t7_new.pune <- t7_new[which(t7_new$tCity==1),]
View(t7_new.pune)

#Fitting Model
fit5 <- felm(t7_new.pune$`tEOYMath[loza]`~t7_new.pune$tAge+t7_new.pune$tStudents+t7_new.pune$tInstitute+t7_new.pune$tLeadSelf+t7_new.pune$tLeadOthers+t7_new.pune$tGrit+t7_new.pune$tFit+t7_new.pune$tContinuousLearning+t7_new.pune$tCoA+t7_new.pune$tCA+t7_new.pune$tPT+t7_new.pune$tAE+t7_new.pune$tVM+t7_new.pune$`tBOYMath[loza]`,data = t7_new.pune)

#-------------------------------------------------------#
#Performing clustering analysis for better interpretation

#KMeans Cluster Analysis

#Data Cleaning - Converting data into matrix format
library(NbClust)
t4_new <- t4_new[complete.cases(t4_new),]
t4_new  <- t4_new[,-13]
for(i in 1:22)
{
  t4_new[,i]<- as.numeric(t4_new[,i])
}
t4_new <- as.matrix(t4_new)
#Implementing the clustering algorithm
nc <- NbClust(t4_new, min.nc = 2, max.nc = 15, method = 'kmeans')
barplot(table(nc$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km <- kmeans(t4_new, 2, nstart=25)                           #3
fit.km$size
plot(t4_new[c(1,5)], col=fit.km$cluster)
points(fit.km$centers[,c(1,5)], col=1:2, pch=8, cex=2)
class(fit.km$cluster)

#Creating two different data sets for 2 clusters
cluster1loc <- which(fit.km$cluster==1)
cluster2loc <- which(fit.km$cluster==2)
cluster1 <- t4_new[cluster1loc,]
cluster2 <- t4_new[cluster2loc,]
#-------------------------------------------------------------------#
#Performing Expectation Maximization (EM) Algorithm

#EM Cluster Analysis
library(mclust)
fit <- Mclust(t4_new, G = 1)
plot(fit) # plot results 
summary(fit)

library(mclust)
mc <- Mclust(t4_new[,c(1,5)], 2)
#Display results 
plot(mc, what=c('classification'))
table(t4_new$tYOI, mc$classification)

mc <- Mclust(t7_new.pune[,c(1,5)], 2)
#Display results 
plot(mc, what=c('classification'))
table(t7_new.pune$tYOI, mc$classification)

#KMeans Clustering without categorical analysis

#Data Cleaning
t7_new <- t4_new
t7_new <- t7_new[c(1,2,5,20)]
t7_new <- as.matrix(t7_new)
library(NbClust)
nc <- NbClust(t7_new, min.nc = 2, max.nc = 15, method = 'kmeans')
barplot(table(nc$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km <- kmeans(t7_new,2, nstart=25) #3
fit.km$size
#Display results
plot(t7_new, col=fit.km$cluster)
plot(t7_new[,c(1,4)], col=fit.km$cluster) #BOY RC Level
plot(t7_new[,c(1,3)], col=fit.km$cluster) #Total Student
plot(t7_new[,c(1,2)], col=fit.km$cluster) #Age

#--------------------------------------------------------------------------#
#Proportional Odd Logisitic Regression (POLR)
t5_new.sorted <- t5_new.sorted[-which(t5_new.sorted$tGrade==1),]
class(op$zeta)
xc <- ctable1[which(ctable1[,4]<0.05),]
class(xc[,1])
xc1 <- xc[,1]

#Calculating odd ratio for all variables
xc2 <-  op$zeta
xc3 <- xc2[1]-xc1
probquartile1_2 <- exp(xc3)/(1+exp(xc3))
xc4 <- xc2[2]-xc1
probquartile2_3 <- exp(xc4)/(1+exp(xc4))
probquartile2_3 <- probquartile2_3-probquartile1_2
xc5 <- xc2[3]-xc1
probquartile3_4 <- exp(xc5)/(1+exp(xc5))
probquartile3_4 <- probquartile3_4 - probquartile2_3 - probquartile1_2
options(scipen = 999)
yo <- cbind(probquartile1_2,probquartile2_3,probquartile3_4)

#Data Cleaning
t19_new <- cbind(t4_new$tRCGrowth,t4_new$tCity)
View(t19_new)
t19_new <- as.data.frame(t19_new)
names(t19_new)=c('RC Growth','City')
t19_new$newcity[which(t19_new$City > 1)]=2
t19_new$newcity[which(is.na(t19_new$newcity))]=1
#Display Results
plot(t19_new$`RC Growth`,col=t19_new$newcity,ylim = c(0,2.5))
#-----------------------------------------------------------------------------#
#Program Managers Analysis

#Importing necessary package
library(lfe)

#Data Cleaning
pm <- new_z$PM.Name
unipm <- unique(pm)
pm <- as.factor(pm)
pm <- as.numeric(pm)
label1 <- sort(unipm)
label1 <- cbind('PM Name' = label1, 'ID' = 1:49)
row1 <- row.names(t4_new)
row1 <- as.numeric(row1)
pm <- pm[row1]
pm <- as.factor(pm)
t4_new <- cbind(t4_new,'PM' = pm)

#Fitting Model
fit10 <- felm(t4_new$tRCGrowth~t4_new$tAge+t4_new$tGrade+t4_new$tYOI+t4_new$tStudents+t4_new$tCity+t4_new$tInstitute+t4_new$tLeadSelf+t4_new$tLeadOthers+t4_new$tGrit+t4_new$tFit+t4_new$tContinuousLearning+t4_new$tCoA+t4_new$tCA+t4_new$tPT+t4_new$tAE+t4_new$tVM+t4_new$tEE+t4_new$tBOYRCLevel+as.factor(t4_new$PM),data = t4_new)
#-----------------------------------------------------------------------#
#Logistic Regression of Top25 Percentile Fellows

#Dataset creation
t6_new.sorted <- t5_new.sorted

#Data Cleaning
for(i in 1:245)
{
  if(t6_new.sorted$quartile[i]==4)
    t6_new.sorted$top25[i] <- 1
  else
    t6_new.sorted$top25[i] <- 0
}
t6_new.sorted <- t6_new.sorted[,-19]
#Fitting model
fit11 <- glm(t6_new.sorted$top25~t6_new.sorted$tAge+t6_new.sorted$tGrade+t6_new.sorted$tYOI+t6_new.sorted$tStudents+t6_new.sorted$tCity+t6_new.sorted$tInstitute+t6_new.sorted$tLeadSelf+t6_new.sorted$tLeadOthers+t6_new.sorted$tGrit+t6_new.sorted$tFit+t6_new.sorted$tContinuousLearning+t6_new.sorted$tCoA+t6_new.sorted$tCA+t6_new.sorted$tPT+t6_new.sorted$tAE+t6_new.sorted$tVM+t6_new.sorted$tEE+t6_new.sorted$tBOYRCLevel,data = t6_new.sorted)

#--------------------------------------------------------------------------#
#Mid Year RC Growth Analysis
MidRC <- new_z$BOY...MY.RC.growth
MidRC <- MidRC[as.numeric(row.names(t4_new))]
MidRC <- as.numeric(MidRC)
t4_new <- t4_new[,-21]
t4_new <- cbind(t4_new,MidRC)
t4_new <- t4_new[complete.cases(t4_new),]

#Fitting Model
fit19 <- lm(t4_new$MidRC~t4_new$tAge+t4_new$tGrade+t4_new$tYOI+t4_new$tStudents+t4_new$tCity+t4_new$tInstitute+t4_new$tLeadSelf+t4_new$tLeadOthers+t4_new$tGrit+t4_new$tFit+t4_new$tContinuousLearning+t4_new$tCoA+t4_new$tCA+t4_new$tPT+t4_new$tAE+t4_new$tVM+t4_new$tEE+t4_new$tBOYRCLevel,data = t4_new)
View(t4_new)
t4_new <- t4_new[-which(t4_new$tGrade==1),]

#------------------------------------------------------------------------------------#
#Amcat Analysis

#Importing AMCAT scores into the data sheet
Amcat12...Sheet1 <- read.csv("C:/Users/Divyansh/Downloads/Amcat12 - Sheet1.csv", stringsAsFactors=FALSE)
amcat <- Amcat12...Sheet1
new_z12 <- new_z
contactID...Sheet1 <- read.csv("C:/Users/Divyansh/Downloads/contactID - Sheet1.csv", stringsAsFactors=FALSE)
contactID <- contactID...Sheet1
row23 <- row.names(new_z12)

#Data Cleaning 
contactID <- contactID[row23,]
new_z12 <- cbind(new_z12,contactID$Contact.ID)
j <- NA
for(i in 1:368)
{
  j <- cbind(j,which(new_z12$Contact.ID[i] == amcat$Contact.ID))
}
j <- j[-1]
for(i in 1:368)
{
new_z12$amcatE[i] <-amcat$English.Score[j[i]]
new_z12$amcatL[i] <-amcat$Logical.Ability.Score[j[i]]
}
View(new_z12)
new_z13 <- cbind('RC Growth' = new_z12$X2014.BOY.EOY.RC.Growth,'Age' = new_z12$Age,'Grade' = new_z12$Grade,'YOI' = new_z12$Year.of.intervention,'Total Students' = new_z12$Total.Students,'City' = new_z12$City,'Institute' = new_z12$Institute.Attending,'Lead Self' = new_z12$Lead.Self.to.Achieve...Final.Score,'Lead Others' = new_z12$Lead.Others.to.Achieve..Final.Score,'Grit' = new_z12$Grit...Final.Score,'Fit' = new_z12$Fit...Final.Score,'Continuous Learning' = new_z12$Continuous.Learning...Final.Score,'CoA' = new_z12$CoA,'AE' = new_z12$A.E,'VM' = new_z12$V.M,'PT' = new_z12$PT,'CA' = new_z12$CA,'EE' = new_z12$EE,'Rigour' = new_z12$Rigour,'AMCAT English' = new_z12$amcatE,'AMCAT Logical' = new_z12$amcatL,'BOY Level' = new_z12$X2014.BOY.RC.level)
new_z13 <- new_z13[complete.cases(new_z13),]
new_z13 <- as.data.frame(new_z13)
xca <- which(new_z13$CoA =='#N/A')
new_z13 <- new_z13[-xca,]

#Factoring LDJ Components
new_z13$CoA2[which(new_z13$CoA==1)] <- '1-2'    
new_z13$CoA2[which(new_z13$CoA==1.5)] <- '1-2'
new_z13$CoA2[which(new_z13$CoA==2)] <- '2-3'    
new_z13$CoA2[which(new_z13$CoA==2.5)] <- '2-3'
new_z13$CoA2[which(new_z13$CoA==3)] <- '3-4'    
new_z13$CoA2[which(new_z13$CoA==3.5)] <- '3-4'
new_z13$CoA2[which(new_z13$CoA==4)] <- '4-5'    
new_z13$CoA2[which(new_z13$CoA==4.5)] <- '4-5'

new_z13$Rigour2[which(new_z13$Rigour==1)] <- '1-2'    
new_z13$Rigour2[which(new_z13$Rigour==1.5)] <- '1-2'
new_z13$Rigour2[which(new_z13$Rigour==2)] <- '2-3'    
new_z13$Rigour2[which(new_z13$Rigour==2.5)] <- '2-3'
new_z13$Rigour2[which(new_z13$Rigour==3)] <- '3-4'    
new_z13$Rigour2[which(new_z13$Rigour==3.5)] <- '3-4'
new_z13$Rigour2[which(new_z13$Rigour==4)] <- '4-5'    
new_z13$Rigour2[which(new_z13$Rigour==4.5)] <- '4-5'

new_z13$VM2[which(new_z13$VM==1)] <- '1-2'    
new_z13$VM2[which(new_z13$VM==1.5)] <- '1-2'
new_z13$VM2[which(new_z13$VM==2)] <- '2-3'    
new_z13$VM2[which(new_z13$VM==2.5)] <- '2-3'
new_z13$VM2[which(new_z13$VM==3)] <- '3-4'    
new_z13$VM2[which(new_z13$VM==3.5)] <- '3-4'
new_z13$VM2[which(new_z13$VM==4)] <- '4-5'    
new_z13$VM2[which(new_z13$VM==4.5)] <- '4-5'

new_z13$AE2[which(new_z13$AE==1)] <- '1-2'    
new_z13$AE2[which(new_z13$AE==1.5)] <- '1-2'
new_z13$AE2[which(new_z13$AE==2)] <- '2-3'    
new_z13$AE2[which(new_z13$AE==2.5)] <- '2-3'
new_z13$AE2[which(new_z13$AE==3)] <- '3-4'    
new_z13$AE2[which(new_z13$AE==3.5)] <- '3-4'
new_z13$AE2[which(new_z13$AE==4)] <- '4-5'    
new_z13$AE2[which(new_z13$AE==4.5)] <- '4-5'

new_z13$PT2[which(new_z13$PT==1)] <- '1-2'    
new_z13$PT2[which(new_z13$PT==1.5)] <- '1-2'
new_z13$PT2[which(new_z13$PT==2)] <- '2-3'    
new_z13$PT2[which(new_z13$PT==2.5)] <- '2-3'
new_z13$PT2[which(new_z13$PT==3)] <- '3-4'    
new_z13$PT2[which(new_z13$PT==3.5)] <- '3-4'
new_z13$PT2[which(new_z13$PT==4)] <- '4-5'    
new_z13$PT2[which(new_z13$PT==4.5)] <- '4-5'

new_z13$EE2[which(new_z13$EE==1)] <- '1-2'    
new_z13$EE2[which(new_z13$EE==1.5)] <- '1-2'
new_z13$EE2[which(new_z13$EE==2)] <- '2-3'    
new_z13$EE2[which(new_z13$EE==2.5)] <- '2-3'
new_z13$EE2[which(new_z13$EE==3)] <- '3-4'    
new_z13$EE2[which(new_z13$EE==3.5)] <- '3-4'
new_z13$EE2[which(new_z13$EE==4)] <- '4-5'    
new_z13$EE2[which(new_z13$EE==4.5)] <- '4-5'

new_z13$CA2[which(new_z13$CA==1)] <- '1-2'    
new_z13$CA2[which(new_z13$CA==1.5)] <- '1-2'
new_z13$CA2[which(new_z13$CA==2)] <- '2-3'    
new_z13$CA2[which(new_z13$CA==2.5)] <- '2-3'
new_z13$CA2[which(new_z13$CA==3)] <- '3-4'    
new_z13$CA2[which(new_z13$CA==3.5)] <- '3-4'
new_z13$CA2[which(new_z13$CA==4)] <- '4-5'    
new_z13$CA2[which(new_z13$CA==4.5)] <- '4-5'

new_z13 <- new_z13[,-(13:18)]
new_z13 <- new_z13[,-13]

new_z13$CoA2 <- as.factor(new_z13$CoA2)
new_z13$Rigour2 <- as.factor(new_z13$Rigour2)
new_z13$VM2 <- as.factor(new_z13$VM2)
new_z13$AE2 <- as.factor(new_z13$AE2)
new_z13$PT2 <- as.factor(new_z13$PT2)
new_z13$EE2 <- as.factor(new_z13$EE2)
new_z13$CA2 <- as.factor(new_z13$CA2)

#Removing Grade 1 Fellows
xcaa <- which(new_z13$Grade==1)
new_z13 <- new_z13[-xcaa,]

#Renaming Pune City for base value
new_z13$City <- as.character(new_z13$City)
new_z13$City[which(new_z13$City=='Pune')] <- '1Pune'   
new_z13$City <- as.factor(new_z13$City)

#Fitting Model
amcatfit <- lm(as.numeric(new_z13$`RC Growth`)~as.numeric(new_z13$Age)+as.factor(new_z13$Grade)+as.factor(new_z13$YOI)+as.numeric(new_z13$`Total Students`)+as.factor(new_z13$City)+as.factor(new_z13$Institute)+as.numeric(new_z13$`Lead Self`)+as.numeric(new_z13$`Lead Others`)+as.numeric(new_z13$Grit)+as.numeric(new_z13$Fit)+as.numeric(new_z13$`Continuous Learning`)+as.factor(new_z13$CoA2)+as.factor(new_z13$AE2)+as.factor(new_z13$VM2)+as.factor(new_z13$PT2)+as.factor(new_z13$CA2)+as.factor(new_z13$EE2)+as.factor(new_z13$Rigour2)+as.numeric(new_z13$`AMCAT English`)+as.numeric(new_z13$`AMCAT Logical`)+as.numeric(new_z13$`BOY Level`))
amcatfitwosel <- lm(as.numeric(new_z13$`RC Growth`)~as.numeric(new_z13$Age)+as.factor(new_z13$Grade)+as.factor(new_z13$YOI)+as.numeric(new_z13$`Total Students`)+as.factor(new_z13$City)+as.factor(new_z13$Institute)+as.factor(new_z13$CoA2)+as.factor(new_z13$AE2)+as.factor(new_z13$VM2)+as.factor(new_z13$PT2)+as.factor(new_z13$CA2)+as.factor(new_z13$EE2)+as.factor(new_z13$Rigour2)+as.numeric(new_z13$`AMCAT English`)+as.numeric(new_z13$`AMCAT Logical`)+as.numeric(new_z13$`BOY Level`))
summary(amcatfit)
#-----------------------------------------------------------------#
#Principal Component Analysis (PCA)
View(t3_new)
selectioncomp <- t3_new[,c(8,9,10,11,12)]
View(selectioncomp)
pca <- prcomp(selectioncomp)
pca
ldjcomp1 <- t3_new[,c(15,16,19)]
View(ldjcomp1)
ldjcomp1$CA[which(ldjcomp1$tCA=='2-3')] <- 2
ldjcomp1$CA[which(ldjcomp1$tCA=='3-4')] <- 3
ldjcomp1$CA[which(ldjcomp1$tCA=='4-5')] <- 4

ldjcomp1$EE[which(ldjcomp1$tEE=='2-3')] <- 2
ldjcomp1$EE[which(ldjcomp1$tEE=='3-4')] <- 3
ldjcomp1$EE[which(ldjcomp1$tEE=='4-5')] <- 4

ldjcomp1$PT[which(ldjcomp1$tPT=='2-3')] <- 2
ldjcomp1$PT[which(ldjcomp1$tPT=='3-4')] <- 3
ldjcomp1$PT[which(ldjcomp1$tPT=='4-5')] <- 4
ldjcomp1 <- ldjcomp1[,-c(1,2,3)]

pca1 <- prcomp(ldjcomp1)

ldjcomp2 <- t3_new[,c(14,17,18)]
ldjcomp2$CoA[which(ldjcomp2$tCoA=='2-3')] <- 2
ldjcomp2$CoA[which(ldjcomp2$tCoA=='3-4')] <- 3
ldjcomp2$CoA[which(ldjcomp2$tCoA=='4-5')] <- 4
ldjcomp2$CoA[which(ldjcomp2$tCoA=='1-2')] <- 1

ldjcomp2$AE[which(ldjcomp2$tAE=='2-3')] <- 2
ldjcomp2$AE[which(ldjcomp2$tAE=='3-4')] <- 3
ldjcomp2$AE[which(ldjcomp2$tAE=='4-5')] <- 4
ldjcomp2$AE[which(ldjcomp2$tAE=='1-2')] <- 1

ldjcomp2$VM[which(ldjcomp2$tVM=='2-3')] <- 2
ldjcomp2$VM[which(ldjcomp2$tVM=='3-4')] <- 3
ldjcomp2$VM[which(ldjcomp2$tVM=='4-5')] <- 4
ldjcomp2$VM[which(ldjcomp2$tVM=='1-2')] <- 1
ldjcomp2 <- ldjcomp2[,-c(1,2,3)]

#Implementing PCA algorithm
pca2 <- prcomp(ldjcomp2,scale. = TRUE)
#-------------------------------------------------------------------#
#Principal Component Analysis
#PCA using FactoMineR
library(FactoMineR)
#Implementing Algorithm
pca0.1 <- PCA(selectioncomp) 
print(pca0.1)
pca0.1$eig[,1:2]
pca1.1 <- PCA(ldjcomp1)
pca1.1$eig[,1:2]
pca2.1 <- PCA(ldjcomp2)
pca2.1$eig[,1:2]
selectpca <- pca$x
selectpca <- selectpca[,c(1,2,3,4)]
selectpca <- as.data.frame(selectpca)
selectpca$selectPC1 <- selectpca$PC1
selectpca$selectPC2 <- selectpca$PC2
selectpca$selectPC3 <- selectpca$PC3
selectpca$selectPC4 <- selectpca$PC4
selectpca <- selectpca[,-c(1,2,3,4)]
fcspca <- pca1$x
fcspca <- fcspca[,c(1,2)]
fcspca <- as.data.frame(fcspca)
fcspca$fcsPC1 <- fcspca$PC1
fcspca$fcsPC2 <- fcspca$PC2
fcspca$fcsPC3 <- fcspca$PC3
fcspca$fcsPC4 <- fcspca$PC4
fcspca <- fcspca[,-c(1,2)]
svspca <- pca2$x
svspca <- svspca[,c(1,2)]
svspca <- as.data.frame(svspca)
svspca$svsPC1 <- svspca$PC1
svspca$svsPC2 <- svspca$PC2
svspca$svsPC3 <- svspca$PC3
svspca$svsPC4 <- svspca$PC4
svspca <- svspca[,-c(1,2)]
pcadata1 <- cbind(t3_new,pcadata)
pcadata1 <- pcadata1[,-c(8,9,10,11,12,14,15,16,17,18,19)]
#----------------------------------------------------------------------------------#
#Kmeans Algorithm
library(NbClust)
pcadata2 <- pcadata1
for(i in 1:17)
{
  pcadata2[,i] <- as.numeric(pcadata2[,i])
}
pcadata2 <- pcadata2[,-8]
kmpca <- NbClust(pcadata2,method = 'kmeans')
barplot(table(kmpca$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km1 <- kmeans(pcadata2, 2, nstart=25)                           #3
fit.km1$size
#Display Results
plot(pcadata2, col=fit.km1$cluster)
points(fit.km1$centers, col=1:2, pch=8, cex=2)
pcadata3 <- arrange(pcadata2,pcadata2$tRCGrowth)
dumhigh <- rnorm(125,mean = 3, sd = 1)
dumlow <- rnorm(125, mean = 1.5, sd = 1)
dum <- c(dumlow,dumhigh)
for(i in 1:250)
{
  pcadata3$dum[i] <- dum[i]
}
#----------------------------------------------------------------------------#
#EM Analysis
kmpca1 <- NbClust(pcadata3,method = 'kmeans')
barplot(table(kmpca1$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km2 <- kmeans(pcadata3, 2, nstart=25)                           #3
fit.km2$size
#Display Results
plot(pcadata3[c(1,17)], col=fit.km2$cluster)
points(fit.km2$centers[,c(1,17)], col=1:2, pch=8, cex=2)

library(mclust)
mc <- Mclust(pcadata3[,c(1,17)], 4)
plot(mc, what=c('classification'))

mc <- Mclust(pcadata3[,c(1,17)], 3)
plot(mc, what=c('classification'))
library(NbClust)
pcadata2 <- pcadata1
for(i in 1:17)
{
  pcadata2[,i] <- as.numeric(pcadata2[,i])
}
pcadata2 <- pcadata2[,-8]
kmpca <- NbClust(pcadata2,method = 'kmeans')
barplot(table(kmpca$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km1 <- kmeans(pcadata2, 2, nstart=25)                           #3
fit.km1$size

#Display Results
plot(pcadata2, col=fit.km1$cluster)
points(fit.km1$centers, col=1:2, pch=8, cex=2)
pcadata3 <- arrange(pcadata2,pcadata2$tRCGrowth)
dumhigh <- rnorm(43,mean = 3, sd = 1)
dumlow <- rnorm(207, mean = 1.5, sd = 1)
dum <- c(dumlow,dumhigh)
for(i in 1:250)
{
  pcadata3$dum[i] <- dum[i]
}
#-----------------------------------------------------------------------------------------#
#Finding Top Performers
#Lead Self, Lead Others, Grit, Fit, CL (Continuous Learning)

#Extract dataset from original data sheet
as <- t5_new.sorted1
#Creating new data set with required fields and their values
topp <- as[which(as$tLeadSelf>3 & as$tLeadOthers>3 & as$tGrit>2 & as$tFit>2 & as$tContinuousLearning>2),]
rownames(topp)
toppRC <- topp$tRCGrowth
toppn <- 0
for(i in 1:length(toppRC))
{
  toppn[i] <- which(new_z$X2014.BOY.EOY.RC.Growth == toppRC[i])
}
topname <- new_z[toppn,]$Name
topperf <- cbind('Name' = new_z[toppn,]$Name,'RC Growth' = new_z[toppn,]$X2014.BOY.EOY.RC.Growth,'Grit' = new_z[toppn,]$Grit...Final.Score,'Fit' = new_z[toppn,]$Fit...Final.Score,'LeadSelf' = new_z[toppn,]$Lead.Self.to.Achieve...Final.Score,'LeadOthers' = new_z[toppn,]$Lead.Others.to.Achieve..Final.Score,'Continuous Learning' = new_z[toppn,]$Continuous.Learning...Final.Score,'City' = new_z[toppn,]$City)
#Export sheet into CSV file
write.csv(topperf,'Name.csv')
#---------------------------------------------------------------------------------#
#Get Top Grade Fellows
#Grade 3 and 8
t5_new.sorted$tGrade
grade38 <- t5_new.sorted[which(t5_new.sorted$tGrade==8 | t5_new.sorted$tGrade==3 ),]
same1 <- which(grade38$tLeadSelf > 2 & grade38$tLeadOthers > 2 & grade38$tGrit > 2 & grade38$tFit > 2 & grade38$tContinuousLearning>2)
sameRC <- grade38$tRCGrowth[same1]
samen <- 0
for(i in 1:length(sameRC))
{
  samen[i] <- which(new_z$X2014.BOY.EOY.RC.Growth == sameRC[i])
}
samengrade <- cbind('Name' = new_z[samen,]$Name,'RC Growth' = new_z[samen,]$X2014.BOY.EOY.RC.Growth,'Grit' = new_z[samen,]$Grit...Final.Score,'Fit' = new_z[samen,]$Fit...Final.Score,'LeadSelf' = new_z[samen,]$Lead.Self.to.Achieve...Final.Score,'LeadOthers' = new_z[samen,]$Lead.Others.to.Achieve..Final.Score,'Continuous Learning' = new_z[samen,]$Continuous.Learning...Final.Score,'City' = new_z[samen,]$City,'Grade' = new_z[samen,]$Grade)
write.csv(samengrade,'Grade.xlsx')
#-------------------------------------------------------------------------------#
#Grade 4 and 7
grade47 <- t5_new.sorted[which(t5_new.sorted$tGrade==4 | t5_new.sorted$tGrade==7 ),]
same2 <- which(grade47$tLeadSelf > 2 & grade47$tLeadOthers > 2 & grade47$tGrit > 2 & grade47$tFit > 2 & grade47$tContinuousLearning>2)
sameRC1 <- grade47$tRCGrowth[same2]
samen1 <- 0
for(i in 1:length(sameRC1))
{
  samen1[i] <- which(new_z$X2014.BOY.EOY.RC.Growth == sameRC1[i])
}
samengrade1 <- cbind('Name' = new_z[samen1,]$Name,'RC Growth' = new_z[samen1,]$X2014.BOY.EOY.RC.Growth,'Grit' = new_z[samen1,]$Grit...Final.Score,'Fit' = new_z[samen1,]$Fit...Final.Score,'LeadSelf' = new_z[samen1,]$Lead.Self.to.Achieve...Final.Score,'LeadOthers' = new_z[samen1,]$Lead.Others.to.Achieve..Final.Score,'Continuous Learning' = new_z[samen1,]$Continuous.Learning...Final.Score,'City' = new_z[samen1,]$City,'Grade' = new_z[samen1,]$Grade)
#Exporting result to CSV File
write.table(samengrade1,'Grade1.xls')

#--------------------------------------END OF PROJECT 1---------------------------------------#
#Summary
#This project helped to filter out only few selection competencies that are relatively more significant
#to the RC Growth.
