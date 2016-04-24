new_z <- NULL
data1 <- z$X2014.BOY.EOY.RC.Growth
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

t1 <- new_z$X2014.BOY.EOY.RC.Growth
t2 <- new_z$Age
t4 <- new_z$Grade
t5 <- new_z$Year.of.intervention
t6 <- new_z$Total.Students
t7 <- new_z$Institute.Attending
t8 <- new_z$City
t9 <- new_z$Lead.Self.to.Achieve...Final.Score
t10 <- new_z$Lead.Others.to.Achieve..Final.Score
t11 <- new_z$Grit...Final.Score
t12 <- new_z$Fit...Final.Score
t13 <- new_z$Continuous.Learning...Final.Score
t14 <- new_z$Teaches.Math
t14 <- as.integer(t14)
t15 <- new_z$Teaches.Writing
t15 <- as.integer(t15)
t16 <- new_z$School.type
t25 <- new_z$X2014.BOY.Overall.Math.Mastery..Mean.

new_z$CoA1 <- as.numeric(new_z$CoA)
new_z$Rigour1 <- as.numeric(new_z$Rigour)
new_z$V.M1 <- as.numeric(new_z$V.M)
new_z$A.E1 <- as.numeric(new_z$A.E)
new_z$PT1 <- as.numeric(new_z$PT)
new_z$EE1 <- as.numeric(new_z$EE)
new_z$CA1 <- as.numeric(new_z$CA)


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

new_z$CoA2 <- as.factor(new_z$CoA2)
new_z$Rigour2 <- as.factor(new_z$Rigour2)
new_z$V.M2 <- as.factor(new_z$V.M2)
new_z$A.E2 <- as.factor(new_z$A.E2)
new_z$PT2 <- as.factor(new_z$PT2)
new_z$EE2 <- as.factor(new_z$EE2)
new_z$CA2 <- as.factor(new_z$CA2)

t17 <- new_z$CoA2
t18 <- new_z$Rigour2
t19 <- new_z$V.M2
t20 <- new_z$A.E2
t21 <- new_z$PT2
t22 <- new_z$EE2
t23 <- new_z$CA2
t101 <- new_z$X2014.EOY.Overall.Math.Mastery..Mean.
t24 <- new_z$X2014.BOY.RC.level

#change t5,t7,t8,t16 from text to integer
for(i in 1:length(t5))
{
  if(t5[i]=='1st YOI')
    t5[i] <- 1
  if(t5[i]=='5th YOI')
    t5[i] <- 5
  if(t5[i]=='2nd YOI')
    t5[i] <- 2
  if(t5[i]=='3rd YOI')
    t5[i] <- 3
  if(t5[i]=='6th YOI')
    t5[i] <- 6
  if(t5[i]=='4th YOI')
    t5[i] <- 4
}
t5 <- as.integer(t5)
for(i in 1:length(t7))
{
  if(t7[i]=='Institute 1')
    t7[i] <- 0
  if(t7[i]=='Institute 2')
    t7[i] <- 1
}
t7 <- as.integer(t7)
#Changing t16
for(i in 1:length(t16))
{
  if(t16[i]=='Private School')
    t16[i] <- 0
  if(t16[i]=='Government School')
    t16[i] <- 1
}
t16 <- as.integer(t16)
#Changing t8
for(i in 1:length(t8))
{
  if(t8[i]=='Pune')
    t8[i] <- 1
  if(t8[i]=='Chennai')
    t8[i] <- 2
  if(t8[i]=='Mumbai')
    t8[i] <- 3
  if(t8[i]=='Hyderabad')
    t8[i] <- 4
  if(t8[i]=='Delhi')
    t8[i] <- 5
  if(t8[i]=='Ahmedabad')
    t8[i] <- 6
}
t8 <- as.integer(t8)
t3 <- cbind.data.frame(t1,t2,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t101,t24,t25)
t3_new <- as.data.frame(t3)
#Removing NAs from the data frame
t3_new <- t3_new[complete.cases(t3_new),]
t3_new <- t3_new[complete.cases(t3_new),]
fit1 <- lm(t3_new$t101~t3_new$t2+t3_new$t8+t3_new$t4+t3_new$t6+t3_new$t9+t3_new$t10+t3_new$t16+t3_new$t7+t3_new$t13+t3_new$t14+t3_new$t15+t3_new$t11+t3_new$t12+t3_new$t5+t3_new$t17+t3_new$t18+t3_new$t19+t3_new$t20+t3_new$t21+t3_new$t22+t3_new$t23,data = t3_new)
# Bonferonni p-value for most extreme obs
outlierTest(fit1) 
# Draws four plots to check the assuptions of regression analysis
plot(fit1)
# Check multicollinearity
vif(fit1) 

#Year of Intervention
for(i in 1:length(t5))
{
  if(t3_new$t5[i]==1)
    t3_new$YOI_1[i] <- 1
  else
    t3_new$YOI_1[i] <- 0
}
for(i in 1:length(t5))
{
  if(t3_new$t5[i]==2)
    t3_new$YOI_2[i] <- 1
  else
    t3_new$YOI_2[i] <- 0
}
for(i in 1:length(t5))
{
  if(t3_new$t5[i]==3)
    t3_new$YOI_3[i] <- 1
  else
    t3_new$YOI_3[i] <- 0
}
for(i in 1:length(t5))
{
  if(t3_new$t5[i]==4)
    t3_new$YOI_4[i] <- 1
  else
    t3_new$YOI_4[i] <- 0
}
for(i in 1:length(t5))
{
  if(t3_new$t5[i]==5)
    t3_new$YOI_5[i] <- 1
  else
    t3_new$YOI_5[i] <- 0
}

for(i in 1:length(t5))
{
  if(t3_new$t5[i]==6)
    t3_new$YOI_6[i] <- 1
  else
    t3_new$YOI_6[i] <- 0
}


#Checking variance
(pca$sdev)^2 #only first four components have variance > 1. Thus retain these components only.

#White Test for checking heteroscedasticity
model1 <- VAR(t3_new)
whites.htest(model1)

#Breusch Pagan Test
bptest(fit1)

#Creating separate columns for each value of Ordinal Variables

#CoA
View(t3_new)
for(i in 1:length(t3_new$t17))
{
  if(t3_new$t17[i]=='1-2')
    t3_new$t17_1to2[i] <- 1
  else 
    t3_new$t17_1to2[i] <- 0
}
for(i in 1:length(t3_new$t17))
{
  if(t3_new$t17[i]=='2-3')
    t3_new$t17_2to3[i] <- 1
  else 
    t3_new$t17_2to3[i] <- 0
}
for(i in 1:length(t3_new$t17))
{
  if(t3_new$t17[i]=='3-4')
    t3_new$t17_3to4[i] <- 1
  else 
    t3_new$t17_3to4[i] <- 0
}
for(i in 1:length(t3_new$t17))
{
  if(t3_new$t17[i]=='4-5')
    t3_new$t17_4to5[i] <- 1
  else 
    t3_new$t17_4to5[i] <- 0
}
#Rigour
for(i in 1:length(t3_new$t18))
{
  if(t3_new$t18[i]=='2-3')
    t3_new$t18_2to3[i] <- 1
  else 
    t3_new$t18_2to3[i] <- 0
}
for(i in 1:length(t3_new$t18))
{
  if(t3_new$t18[i]=='3-4')
    t3_new$t18_3to4[i] <- 1
  else 
    t3_new$t18_3to4[i] <- 0
}
for(i in 1:length(t3_new$t18))
{
  if(t3_new$t18[i]=='4-5')
    t3_new$t18_4to5[i] <- 1
  else 
    t3_new$t18_4to5[i] <- 0
}
#V.M
for(i in 1:length(t3_new$t19))
{
  if(t3_new$t19[i]=='1-2')
    t3_new$t19_1to2[i] <- 1
  else 
    t3_new$t19_1to2[i] <- 0
}
for(i in 1:length(t3_new$t19))
{
  if(t3_new$t19[i]=='2-3')
    t3_new$t19_2to3[i] <- 1
  else 
    t3_new$t19_2to3[i] <- 0
}
for(i in 1:length(t3_new$t19))
{
  if(t3_new$t19[i]=='3-4')
    t3_new$t19_3to4[i] <- 1
  else 
    t3_new$t19_3to4[i] <- 0
}
for(i in 1:length(t3_new$t19))
{
  if(t3_new$t19[i]=='4-5')
    t3_new$t19_4to5[i] <- 1
  else 
    t3_new$t19_4to5[i] <- 0
}


#A.E
for(i in 1:length(t3_new$t20))
{
  if(t3_new$t20[i]=='1-2')
    t3_new$t20_1to2[i] <- 1
  else 
    t3_new$t20_1to2[i] <- 0
}
for(i in 1:length(t3_new$t20))
{
  if(t3_new$t20[i]=='2-3')
    t3_new$t20_2to3[i] <- 1
  else 
    t3_new$t20_2to3[i] <- 0
}
for(i in 1:length(t3_new$t20))
{
  if(t3_new$t20[i]=='3-4')
    t3_new$t20_3to4[i] <- 1
  else 
    t3_new$t20_3to4[i] <- 0
}
for(i in 1:length(t3_new$t20))
{
  if(t3_new$t20[i]=='4-5')
    t3_new$t20_4to5[i] <- 1
  else 
    t3_new$t20_4to5[i] <- 0
}


#PT
for(i in 1:length(t3_new$t21))
{
  if(t3_new$t21[i]=='2-3')
    t3_new$t21_2to3[i] <- 1
  else 
    t3_new$t21_2to3[i] <- 0
}
for(i in 1:length(t3_new$t21))
{
  if(t3_new$t21[i]=='3-4')
    t3_new$t21_3to4[i] <- 1
  else 
    t3_new$t21_3to4[i] <- 0
}
for(i in 1:length(t3_new$t21))
{
  if(t3_new$t21[i]=='4-5')
    t3_new$t21_4to5[i] <- 1
  else 
    t3_new$t21_4to5[i] <- 0
}

#EE
for(i in 1:length(t3_new$t22))
{
  if(t3_new$t22[i]=='2-3')
    t3_new$t22_2to3[i] <- 1
  else 
    t3_new$t22_2to3[i] <- 0
}
for(i in 1:length(t3_new$t22))
{
  if(t3_new$t22[i]=='3-4')
    t3_new$t22_3to4[i] <- 1
  else 
    t3_new$t22_3to4[i] <- 0
}
for(i in 1:length(t3_new$t22))
{
  if(t3_new$t22[i]=='4-5')
    t3_new$t22_4to5[i] <- 1
  else 
    t3_new$t22_4to5[i] <- 0
}

#CA
for(i in 1:length(t3_new$t23))
{
  if(t3_new$t23[i]=='2-3')
    t3_new$t23_2to3[i] <- 1
  else 
    t3_new$t23_2to3[i] <- 0
}
for(i in 1:length(t3_new$t23))
{
  if(t3_new$t23[i]=='3-4')
    t3_new$t23_3to4[i] <- 1
  else 
    t3_new$t23_3to4[i] <- 0
}
for(i in 1:length(t3_new$t23))
{
  if(t3_new$t23[i]=='4-5')
    t3_new$t23_4to5[i] <- 1
  else 
    t3_new$t23_4to5[i] <- 0
}

#Creating binary variables for cities
for(i in 1:368)
{
  if(t3_new$t8[i]==1)
    t3_new$Pune[i] <- 1
  else
    t3_new$Pune[i] <- 0
}
for(i in 1:368)
{
  if(t3_new$t8[i]==2)
    t3_new$Chennai[i] <- 1
  else
    t3_new$Chennai[i] <- 0
}
for(i in 1:368)
{
  if(t3_new$t8[i]==3)
    t3_new$Mumbai[i] <- 1
  else
    t3_new$Mumbai[i] <- 0
}
for(i in 1:368)
{
  if(t3_new$t8[i]==4)
    t3_new$Hyderabad[i] <- 1
  else
    t3_new$Hyderabad[i] <- 0
}
for(i in 1:368)
{
  if(t3_new$t8[i]==5)
    t3_new$Delhi[i] <- 1
  else
    t3_new$Delhi[i] <- 0
}
for(i in 1:368)
{
  if(t3_new$t8[i]==6)
    t3_new$Ahmedabad[i] <- 1
  else
    t3_new$Ahmedabad[i] <- 0
}



#Creating new data frame
t3_new2 <- t3_new
t3_new2$t17 <- NULL
t3_new2$t18 <- NULL
t3_new2$t19 <- NULL
t3_new2$t20 <- NULL
t3_new2$t21 <- NULL
t3_new2$t22 <- NULL
t3_new2$t23 <- NULL
t3_new2$t101 <- new_z$X2014.EOY.Overall.Math.Mastery..Mean.

fitnew <- felm(t3_new2$t1~t3_new2$t2+t3_new2$t4+t3_new2$YOI_2+t3_new2$YOI_3+t3_new2$YOI_4+t3_new2$YOI_5+t3_new2$YOI_6+t3_new2$Grade_1+t3_new2$Grade_2+t3_new2$Grade_3+t3_new2$Grade_4+t3_new2$Grade_5+t3_new2$Grade_6+t3_new2$Grade_7+t3_new2$Grade_1 t3_new2$t6+t3_new2$t7+t3_new2$t9+t3_new2$t10+t3_new2$t11+t3_new2$t12+t3_new2$t13+t3_new2$t14+t3_new2$t15+t3_new2$t16+t3_new2$t17_1to2+t3_new2$t17_3to4+t3_new2$t17_4to5+t3_new2$t18_3to4+t3_new2$t18_4to5+t3_new2$t19_1to2+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t20_1to2+t3_new2$t20_3to4+t3_new2$t20_4to5+t3_new2$t21_3to4+t3_new2$t21_4to5+t3_new2$t23_3to4+t3_new2$t23_4to5+t3_new2$t22_3to4+t3_new2$t22_4to5+t3_new2$t24+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad)
plot(fitnew)

#Eliminating Ordinal Parameters
summa <- summary(fitnew)
summa$adj.r.squared #0.12398
sort(summa$coefficients[,4],decreasing = TRUE)[1]
summa <- summary(felm(t3_new2$t1~t3_new2$t4+t3_new2$t7+t3_new2$t9+t3_new2$t10+t3_new2$t11+t3_new2$t13+t3_new2$t14+t3_new2$t15+t3_new2$t16+t3_new2$t17_3to4+t3_new2$t18_4to5+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t20_4to5+t3_new2$t21_3to4+t3_new2$t23_3to4+t3_new2$t22_3to4+t3_new2$t24+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad))
summa$adj.r.squared
sort(summa$coefficients[,4],decreasing = TRUE)[1]

#Running new regression model after removing parameters with zero coefficients
fittest <- felm(t3_new2$t1~t3_new2$t4+t3_new2$t7+t3_new2$t9+t3_new2$t10+t3_new2$t11+t3_new2$t13+t3_new2$t14+t3_new2$t15+t3_new2$t16+t3_new2$t17_3to4+t3_new2$t18_4to5+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t20_4to5+t3_new2$t21_3to4+t3_new2$t23_3to4+t3_new2$t22_3to4+t3_new2$t24+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad)
summary(fittest)

#Analysis of Math 
mathfit <- felm(t3_new2$t101~t3_new2$t2+t3_new2$t4+t3_new2$t6+t3_new2$t7+t3_new2$t9+t3_new2$t10+t3_new2$t11+t3_new2$t12+t3_new2$t13+t3_new2$t14+t3_new2$t15+t3_new2$t16+t3_new2$t17_3to4+t3_new2$t18_3to4+t3_new2$t18_4to5+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t20_1to2+t3_new2$t20_3to4+t3_new2$t21_3to4+t3_new2$t21_4to5+t3_new2$t23_3to4+t3_new2$t22_3to4+t3_new2$t25+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad)
summam <- summary(mathfit)
summam$adj.r.squared #0.12398
sort(summam$coefficients[,4],decreasing = TRUE)[1]
summam<- summary(felm(t3_new2$t101~t3_new2$t4+t3_new2$t10+t3_new2$t16+t3_new2$t17_3to4+t3_new2$t18_3to4+t3_new2$t18_4to5+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t21_3to4+t3_new2$t21_4to5+t3_new2$t23_3to4+t3_new2$t22_3to4+t3_new2$t25+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad))
summam$adj.r.squared #0.12398
sort(summam$coefficients[,4],decreasing = TRUE)[1]

fittestmath <- felm(t3_new2$t101~t3_new2$t4+t3_new2$t8+t3_new2$t10+t3_new2$t16+t3_new2$t17_3to4+t3_new2$t18_3to4+t3_new2$t18_4to5+t3_new2$t19_3to4+t3_new2$t19_4to5+t3_new2$t21_3to4+t3_new2$t21_4to5+t3_new2$t23_3to4+t3_new2$t22_3to4+t3_new2$t25+t3_new2$Pune+t3_new2$Chennai+t3_new2$Mumbai+t3_new2$Hyderabad+t3_new2$Ahmedabad)

#Implement Principal Component Analysis
ir.pca <- prcomp(t3_new2, center = TRUE, scale. = TRUE)
print(ir.pca)
plot(ir.pca,type = 'l')
summary(ir.pca)

#Analysing t17
analyset17 <- sum(which(t3_new2$t17_1to2==1)>0)
analyset17 <- c(analyset17,sum(which(t3_new2$t17_2to3==1)>0),sum(which(t3_new2$t17_3to4==1)>0),sum(which(t3_new2$t17_4to5==1)>0))
barplot(analyset17)

#Analysing t18
analyset18 <- sum(which(t3_new2$t18_1to2==1)>0)
analyset18 <- c(analyset18,sum(which(t3_new2$t18_2to3==1)>0),sum(which(t3_new2$t18_3to4==1)>0),sum(which(t3_new2$t18_4to5==1)>0))
barplot(analyset18)

#Analysing t19
analyset19 <- sum(which(t3_new2$t19_1to2==1)>0)
analyset19 <- c(analyset19,sum(which(t3_new2$t19_2to3==1)>0),sum(which(t3_new2$t19_3to4==1)>0),sum(which(t3_new2$t19_4to5==1)>0))
barplot(analyset19)

#Analysing t20
analyset20 <- sum(which(t3_new2$t20_1to2==1)>0)
analyset20 <- c(analyset20,sum(which(t3_new2$t20_2to3==1)>0),sum(which(t3_new2$t20_3to4==1)>0),sum(which(t3_new2$t20_4to5==1)>0))
barplot(analyset20)

#Analysing t21
analyset21 <- sum(which(t3_new2$t21_1to2==1)>0)
analyset21 <- c(analyset21,sum(which(t3_new2$t21_2to3==1)>0),sum(which(t3_new2$t21_3to4==1)>0),sum(which(t3_new2$t21_4to5==1)>0))
barplot(analyset21)

#Analysing t22
analyset22 <- sum(which(t3_new2$t22_1to2==1)>0)
analyset22 <- c(analyset22,sum(which(t3_new2$t22_2to3==1)>0),sum(which(t3_new2$t22_3to4==1)>0),sum(which(t3_new2$t22_4to5==1)>0))
barplot(analyset22)

#Analysing t23
analyset23 <- sum(which(t3_new2$t23_1to2==1)>0)
analyset23 <- c(analyset23,sum(which(t3_new2$t23_2to3==1)>0),sum(which(t3_new2$t23_3to4==1)>0),sum(which(t3_new2$t23_4to5==1)>0))
barplot(analyset23)
t3_new3<- NULL
#Creating new model
t3_new3 <- t3_new2
t3_new3$Growth <- t3_new2$t1
t3_new3$Age <- t3_new2$t2
t3_new3$Grade <- t3_new2$t4
t3_new3$YearOfIntervention <- t3_new2$t5
t3_new3$TotalStudents <- t3_new2$t6
t3_new3$InstituteAttending <- t3_new2$t7
t3_new3$City <- t3_new2$t8
t3_new3$LeadSelfFinalScore <- t3_new2$t9
t3_new3$LeadOthersFinalScore <- t3_new2$t10
t3_new3$GritFinalScore <- t3_new2$t11
t3_new3$FitFinalScore <- t3_new2$t12
t3_new3$ContinuousLearningFinalScore <- t3_new2$t13
t3_new3$TeachesMath <- t3_new2$t14
t3_new3$TeachesWriting <- t3_new2$t15
t3_new3$SchoolType <- t3_new2$t16
t3_new3$BOYMean <- NULL
t3_new3$CoA1to2 <- t3_new2$t17_1to2
t3_new3$CoA1to2 <- t3_new2$t17_1to2
t3_new3$CoA2to3 <- t3_new2$t17_2to3
t3_new3$CoA3to4 <- t3_new2$t17_3to4
t3_new3$CoA4to5 <- t3_new2$t17_4to5
t3_new3$Rigour1to2 <- t3_new2$t18_1to2
t3_new3$Rigour2to3 <- t3_new2$t18_2to3
t3_new3$Rigour3to4 <- t3_new2$t18_3to4
t3_new3$Rigour4to5 <- t3_new2$t18_4to5
t3_new3$VM1to2 <- t3_new2$t19_1to2
t3_new3$VM2to3 <- t3_new2$t19_2to3
t3_new3$VM3to4 <- t3_new2$t19_3to4
t3_new3$VM4to5 <- t3_new2$t19_4to5
t3_new3$AE1to2 <- t3_new2$t20_1to2
t3_new3$AE2to3 <- t3_new2$t20_2to3
t3_new3$AE3to4 <- t3_new2$t20_3to4
t3_new3$AE4to5 <- t3_new2$t20_4to5
t3_new3$PT1to2 <- t3_new2$t21_1to2
t3_new3$PT2to3 <- t3_new2$t21_2to3
t3_new3$PT3to4 <- t3_new2$t21_3to4
t3_new3$PT4to5 <- t3_new2$t21_4to5
t3_new3$EE1to2 <- t3_new2$t22_1to2
t3_new3$EE2to3 <- t3_new2$t22_2to3
t3_new3$EE3to4 <- t3_new2$t22_3to4
t3_new3$EE4to5 <- t3_new2$t22_4to5
t3_new3$CA1to2 <- t3_new2$t23_1to2
t3_new3$CA2to3 <- t3_new2$t23_2to3
t3_new3$CA3to4 <- t3_new2$t23_3to4
t3_new3$CA4to5 <- t3_new2$t23_4to5
t3_new3$BOYGrowth <- t3_new2$t24
View(t3_new3)
t3_new3$Pune <- t3_new2$Pune
t3_new3$Chennai <- t3_new2$Chennai
t3_new3$Mumbai <- t3_new2$Mumbai
t3_new3$Hyderabad <- t3_new2$Hyderabad
t3_new3$Delhi <- t3_new2$Delhi
t3_new3$Ahmedabad <- t3_new2$Ahmedabad
View(t3_new3)
t3_new3$t1<- NULL
t3_new3$t2<- NULL
t3_new3$t3<- NULL
t3_new3$t4<- NULL
t3_new3$t5<- NULL
t3_new3$t6<- NULL
t3_new3$t7<- NULL
t3_new3$t8 <- NULL
t3_new3$t9 <- NULL
t3_new3$t10 <- NULL
t3_new3$t11 <- NULL
t3_new3$t12 <- NULL
t3_new3$t13 <- NULL
t3_new3$t14 <- NULL
t3_new3$t15 <- NULL
t3_new3$t16 <- NULL
t3_new3$MathEOY <- t3_new2$t101
t3_new3$MathBOY <- t3_new2$t25
t3_new3$t8 <- NULL
t3_new3$t9 <- NULL
t3_new3$t10 <- NULL
t3_new3$t11 <- NULL
t3_new3$t12 <- NULL
t3_new3$t13 <- NULL
t3_new3$t14 <- NULL
t3_new3$t15 <- NULL
t3_new3$t16 <- NULL
t3_new3$t101 <- NULL
t3_new3$t24 <- NULL
t3_new3$t25 <- NULL
t3_new3$t17_1to2 <- NULL
t3_new3$t17_2to3 <- NULL
t3_new3$t17_3to4 <- NULL
t3_new3$t17_4to5 <- NULL
t3_new3$t18_2to3 <- NULL
t3_new3$t18_3to4 <- NULL
t3_new3$t18_4to5 <- NULL
t3_new3$t19_1to2 <- NULL
t3_new3$t19_2to3 <- NULL
t3_new3$t19_3to4 <- NULL
t3_new3$t19_4to5 <- NULL
t3_new3$t20_1to2 <- NULL
t3_new3$t20_2to3 <- NULL
t3_new3$t20_3to4 <- NULL
t3_new3$t20_4to5 <- NULL
t3_new3$t21_2to3 <- NULL
t3_new3$t21_3to4 <- NULL
t3_new3$t21_4to5 <- NULL
t3_new3$t22_2to3 <- NULL
t3_new3$t22_3to4 <- NULL
t3_new3$t22_4to5 <- NULL
t3_new3$t23_2to3 <- NULL
t3_new3$t23_3to4 <- NULL
t3_new3$t23_4to5 <- NULL

fittest <- felm(t3_new3$Growth~t3_new3$Grade+t3_new3$InstituteAttending+t3_new3$LeadSelfFinalScore+t3_new3$LeadOthersFinalScore+t3_new3$GritFinalScore+t3_new3$TeachesMath+t3_new3$TeachesWriting+t3_new3$SchoolType+t3_new3$CoA3to4+t3_new3$Rigour4to5+t3_new3$VM3to4+t3_new3$VM4to5+t3_new3$AE4to5+t3_new3$BOYGrowth+t3_new3$Pune+t3_new3$Chennai+t3_new3$Mumbai+t3_new3$Hyderabad+t3_new3$Ahmedabad)
fittestmath <- felm(t3_new3$MathEOY~t3_new3$Grade+t3_new3$InstituteAttending+t3_new3$LeadSelfFinalScore+t3_new3$LeadOthersFinalScore+t3_new3$GritFinalScore+t3_new3$ContinuousLearningFinalScore+t3_new3$TeachesWriting+t3_new3$TeachesMath+t3_new3$SchoolType+t3_new3$CoA3to4+t3_new3$Rigour4to5+t3_new3$VM3to4+t3_new3$VM4to5+t3_new3$AE4to5+t3_new3$PT3to4+t3_new3$CA3to4+t3_new3$EE3to4+t3_new3$MathBOY+t3_new3$Pune+t3_new3$Chennai+t3_new3$Mumbai+t3_new3$Hyderabad+t3_new3$Ahmedabad)
fittest1 <- felm(t3_new3$Growth~t3_new3$Grade+t3_new3$SchoolType+t3_new3$Rigour4to5+t3_new3$VM3to4+t3_new3$VM4to5+t3_new3$AE4to5+t3_new3$BOYGrowth+t3_new3$Pune+t3_new3$Chennai+t3_new3$Mumbai+t3_new3$Hyderabad+t3_new3$Ahmedabad)
summary(fittest1)
fittestmath1 <- felm(t3_new3$MathEOY~t3_new3$Grade+t3_new3$VM3to4+t3_new3$VM4to5+t3_new3$MathBOY+t3_new3$Pune+t3_new3$Chennai+t3_new3$Mumbai+t3_new3$Hyderabad+t3_new3$Ahmedabad)
summary(fittestmath1)

#Machine Learning
t3 <- cbind.data.frame(tRCGrowth,tAge,tGrade,tYOI,tStudents,tCity,tInstitute,tLeadSelf,tLeadOthers,tGrit,tFit,tContinuousLearning,tTeachesRC,tCoA,tCA,tPT,tAE,tVM,tEE,tBOYRCLevel)
t4_new <- as.data.frame(t3)
#Removing NAs from the data frame
t4_new <- t4_new[complete.cases(t4_new),]
t4_new <- t4_new[complete.cases(t4_new),]
loca1 <- which(t4_new$tTeachesRC==1)
t4_new <- t4_new[loca1,]
View(t4_new)
fit1 <- lm(t4_new$tRCGrowth~t4_new$tAge+t4_new$tGrade+t4_new$tYOI+t4_new$tStudents+t4_new$tCity+t4_new$tInstitute+t4_new$tLeadSelf+t4_new$tLeadOthers+t4_new$tGrit+t4_new$tFit+t4_new$tContinuousLearning+t4_new$tCoA+t4_new$tCA+t4_new$tPT+t4_new$tAE+t4_new$tVM+t4_new$tEE+t4_new$tBOYRCLevel,data = t4_new)


#Top 25%tile Analysis

t5_new <- t4_new
t5_new.sorted <- t5_new[order(t5_new$tRCGrowth),]
t5_new.sorted <- t5_new.sorted[-1,]
t5_new.sorted <- t5_new.sorted[-2,]
dim(t5_new.sorted)
t5_new.sorted$quartile <- rep(1:4, each=248/4)
t5_new.top <- t5_new.sorted[which(t5_new.sorted$quartile==4),]
t5_new.top
View(t5_new.top)
fit3 <- lm(t5_new.top$tRCGrowth~t5_new.top$tAge+t5_new.top$tGrade+t5_new.top$tYOI+t5_new.top$tStudents+t5_new.top$tCity+t5_new.top$tInstitute+t5_new.top$tLeadSelf+t5_new.top$tLeadOthers+t5_new.top$tGrit+t5_new.top$tFit+t5_new.top$tContinuousLearning+t5_new.top$tCoA+t5_new.top$tCA+t5_new.top$tPT+t5_new.top$tAE+t5_new.top$tVM+t5_new.top$tEE+t5_new.top$tBOYRCLevel,data = t5_new.top)
class(t5_new.top$tCity)
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

#Multinomial Logistic Regression
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
library(nnet)
mod <- multinom(t5_new.sorted$quartile ~t5_new.sorted$tAge+t5_new.sorted$tGrade+t5_new.sorted$tYOI+t5_new.sorted$tStudents+t5_new.sorted$tCity+t5_new.sorted$tInstitute+t5_new.sorted$tLeadSelf+t5_new.sorted$tLeadOthers+t5_new.sorted$tGrit+t5_new.sorted$tFit+t5_new.sorted$tContinuousLearning+t5_new.sorted$tCoA+t5_new.sorted$tCA+t5_new.sorted$tPT+t5_new.sorted$tAE+t5_new.sorted$tVM+t5_new.sorted$tEE+t5_new.sorted$tBOYRCLevel,data = t5_new.sorted)
z <- summary(mod)$coefficients/summary(mod)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

#Ordinal Logistic Regression
library(MASS)
op <- polr(t5_new.sorted$quartile ~t5_new.sorted$tAge+t5_new.sorted$tGrade+t5_new.sorted$tYOI+t5_new.sorted$tStudents+t5_new.sorted$tCity+t5_new.sorted$tInstitute+t5_new.sorted$tLeadSelf+t5_new.sorted$tLeadOthers+t5_new.sorted$tGrit+t5_new.sorted$tFit+t5_new.sorted$tContinuousLearning+t5_new.sorted$tCoA+t5_new.sorted$tCA+t5_new.sorted$tPT+t5_new.sorted$tAE+t5_new.sorted$tVM+t5_new.sorted$tEE+t5_new.sorted$tBOYRCLevel,data = t5_new.sorted, Hess=TRUE,na.action=na.omit)
summary(op)
ctable1 <- coef(summary(op))
p <- 0
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE) * 2
ctable1 <- cbind(ctable1, "p value" = p)
print(op)
ctable1[which(ctable1[,4]<0.05),]
ctable1[which(ctable1[,4]<0.001),]

#Pune Analysis
t6_new <- t4_new
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

fit4 <- felm(t6_new.pune$tRCGrowth~t6_new.pune$tAge+t6_new.pune$tGrade+t6_new.pune$tYOI+t6_new.pune$tStudents+t6_new.pune$tInstitute+t6_new.pune$tLeadSelf+t6_new.pune$tLeadOthers+t6_new.pune$tGrit+t6_new.pune$tFit+t6_new.pune$tContinuousLearning+t6_new.pune$tCoA+t6_new.pune$tCA+t6_new.pune$tPT+t6_new.pune$tAE+t6_new.pune$tVM+t6_new.pune$tBOYRCLevel,data = t6_new.pune)
summary(fit4)


#KMeans Cluster Analysis
library(NbClust)
t4_new <- as.matrix(t4_new)
nc <- NbClust(t4_new, min.nc = 2, max.nc = 15, method = 'kmeans')
barplot(table(nc$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km <- kmeans(t4_new, 2, nstart=25)                           #3
fit.km$size
plot(t4_new[c(1,5)], col=fit.km$cluster)
points(fit.km$centers[,c(1,5)], col=1:2, pch=8, cex=2)
class(fit.km$cluster)
cluster1loc <- which(fit.km$cluster==1)
cluster2loc <- which(fit.km$cluster==2)
cluster1 <- t4_new[cluster1loc,]
cluster2 <- t4_new[cluster2loc,]

#EM Cluster Analysis
library(mclust)
fit <- Mclust(t4_new, G = 1)
plot(fit) # plot results 
summary(fit)

library(mclust)
mc <- Mclust(t4_new[,c(1,5)], 3)
plot(mc, what=c('classification'))
table(t4_new$tYOI, mc$classification)

mc <- Mclust(t6_new.pune[,c(1,5)], 4)
plot(mc, what=c('classification'))
table(t6_new.pune$tYOI, mc$classification)

#KMeans Clustering without categorical analysis
t7_new <- t4_new
t7_new <- t7_new[c(1,2,5,20)]
t7_new <- as.matrix(t7_new)
library(NbClust)
nc <- NbClust(t7_new, min.nc = 2, max.nc = 15, method = 'kmeans')
barplot(table(nc$Best.n[1,]),  xlab="Numer of Clusters", ylab="Number of Criteria", main="Number of Clusters Chosen by 26 Criteria")
set.seed(1234)
fit.km <- kmeans(t7_new, 2, nstart=25)                           #3
fit.km$size
plot(t7_new, col=fit.km$cluster)
plot(t7_new[,c(1,4)], col=fit.km$cluster) #BOY RC Level
plot(t7_new[,c(1,3)], col=fit.km$cluster) #Total Student
plot(t7_new[,c(1,2)], col=fit.km$cluster) #Age
