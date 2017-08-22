#CA Project Big Data Analyitics Project Humphrey Murphy
library(readxl)
library(matrixStats)
library(xlsx)
library(plyr)
library(pastecs)
#install.packages("car")
library(car)
#install.packages("pwr", repos = "http://cran.r-project.org")
library(pwr)
Health_Extract1 <- read_excel("Health_Extract.xlsx") # transfers the data from a specific sheet into Health
Indust_Extract1 <- read_excel("Indust_Extract.xlsx")
excel_sheets("Health_Extract.xlsx") #gives names of sheet
excel_sheets("Indust_Extract.xlsx") #gives names of sheet
head(Health_Extract1, n = 10)
head(Indust_Extract1, n = 10)

#Occupation Data Cleaning
## Deleting all of the occupational data except the Agricultural and Professional working elements
UNIQ_Occup = unique(Indust_Extract1$Occupation) #Identifying all of the different/unique occupations
head(UNIQ_Occup)
Indust_Cleared = subset(Indust_Extract1, Occupation == UNIQ_Occup[1] | Occupation == UNIQ_Occup[7]) #Indust_cleared 
#isolates just the 1st (Agr), and 7th (Pro) occupations
head(Indust_Cleared, n = 30)


#Cleaning Health Data, removing unessecary gender data Health
Health_Extract2 <- subset(Health_Extract1, Health.Status != "all", select = c(ED_Name, Number, Health.Status, Gender)) #selects only the elements in the Health Extract1 that are not "all" 
#as their health status and then parcels up the relevant information from these into the variable Health Extract 2
head(Health_Extract2)

Health_Extract3 <- subset(Health_Extract2, Health.Status != "not stated", select = c(ED_Name, Number, Health.Status, Gender))
#removes 'not stated' elements from the data base.
head(Health_Extract3)

Health_Extract4 <- subset(Health_Extract3, Gender != "male", select = c(ED_Name, Number, Health.Status, Gender))
#removes 'male' from the data base.
head(Health_Extract4)

Health_Cleared <- subset(Health_Extract4, Gender != "female", select = c(ED_Name, Number, Health.Status, Gender)) # removes' female'
head(Health_Cleared) #now only have the health status for 'both' male and female.

#Capturing the total number of people for each ED
#Health_Extract5 <- subset(Health_Extract1, Health.Status == "all" & Gender == "both", select = c(ED_Name, Number, Health.Status, Gender))
#head(Health_Extract5)  #excludes duplication of male and female by selecting only all of the health status for the 'both' category.

#Assigning values to the health status.http://stackoverflow.com/questions/19503266/r-replace-all-particular-values-in-a-data-frame 

#Assigning numerical values to the reported health 
UNIQ_Health = unique(Health_Cleared$Health.Status);
#identifies the different unique status of health.
head(UNIQ_Health)
#focuses on the EDName rows and sets up a unique sequence which facilitates a loop based on the ED_Name.  ref Code Institute.
Health_Cleared[Health_Cleared == "Good"] <- 4 #assigns a value of 4 to the elements which are categorised as 'Good'
Health_Cleared[Health_Cleared == "Fair"] <- 3
Health_Cleared[Health_Cleared == "Bad"] <- 2
Health_Cleared[Health_Cleared == "Very Bad"] <- 1
Health_Cleared[Health_Cleared == "Very Good"] <- 5
head(Health_Cleared)
z = Health_Cleared$Number * as.numeric(Health_Cleared$Health.Status);

UNIQ_Health = unique(Health_Cleared$ED_Name);
#takes the 'number' of total members of the edname and multiplies them by 
#the number listed in the "Health Status".  z is now the numeric value of the Health Status multipled by the number of individuals in the ed.
#head(z, n = 10)
#is.numeric(z)
#limit the table to unique ED Names.
#Identify the health score per ED_Name
Score_H = c() #empty variable awaiting the vectors from the if loop, provides the sum of the health score for each ED_Name.
Score_Agr = c() #empty variable awaiting the vectors from the if loop, provides the sum of the agricutlure participants  for each ED_Name.
Score_Prof = c() #empty variable awaiting the vectors from the if loop, provides the sum of the professional participants for each ED_Name.#empty variable awaiting the vectors from the if loop, provides the sum of the agricutlure participant score for each ED_Name.
Number = c() #number in each ED

#for loop to calculate the health status across each ED-Name.  I attempted this as a nested if loop but I was unable to do so, 
#hence I created a series of if statements in sequence.
for (i in 1:length(UNIQ_Health)) {
    #for loop i = automatic variable name 1:length(UNIQ) = sequence established 
    #earlier by ED-Name in line 36.  This sets up the range of the sequence starting with 1 and running for the length of the 
    #UNIQ sequence.  Gives access to loop infromation Ref DataCamp https://campus.datacamp.com/courses/intermediate-r/chapter-2-loops?ex=6
    Score_H[i] = sum(z[Health_Cleared$ED_Name == UNIQ_Health[i]]) #Score_H is populated by i which is then calculated by 
}
head(Score_H, n = 100) #sample of health scores.
stat.desc(Score_H, basic = TRUE, desc = TRUE, norm = FALSE, p = 0.95)

#for loop to calculate the health score across the agriculture sector per ED_Name
for (i in 1:length(UNIQ_Health)) {
    Score_Agr[i] = sum(Indust_Cleared$Number[Indust_Cleared$ED_Name == UNIQ_Health[i] & Indust_Cleared$Occupation == UNIQ_Occup[1]])
    #limits the loop to the unique ED-name and the agriculture profession.
}
head(UNIQ_Health, n = 10)
head(Score_Agr, n = 100)

mean(Score_Agr)

#for loop to calculate the health score across the Professional sector per ED_Name
for (i in 1:length(UNIQ_Health)) {
    Score_Prof[i] = sum(Indust_Cleared$Number[Indust_Cleared$ED_Name == UNIQ_Health[i] & Indust_Cleared$Occupation == UNIQ_Occup[7]])
    #limits the loop to the unique ED-name and the agriculture profession.
}
head(Score_Prof, n = 10)
head(Score_H)
head(UNIQ_Health)
stat.desc(Score_Agr)
stat.desc(Score_Prof)


#Regression Analysis 
Result_Reg = data.frame(UNIQ_Health, Score_H, Score_Agr, Score_Prof)
head(Result_Reg)
#Regression Occupations
plot(Score_Agr ~ Score_Prof, Result_Reg, pch = 10, cex = .6, col = "blue", main = "Occupations, Agriculture and Professional", xlab = "Professional", ylab = "Agriculture", abline(0, 1))
#Practical Regression and Anova using R, Julian J. Faraway, July 2002
#Regression Health Score and Occupations
plot( Score_Prof ~ Score_H, Result_Reg, pch = 1, cex = .8, col = "red", main = "Health and Professional Occupation", xlab = "Health", ylab = "Professional", abline(0, 1))
abline(0, 1)
plot(Score_Agr ~ Score_H, Result_Reg, pch = 1, cex = .8, col = "blue", main = "Health and Agriculture Occupation", xlab = "Health", ylab = "Agriculture", abline(0, 1))
abline(0, 1)
 plot(Score_Prof ~ Score_Agr, Result_Reg)
g <- lm(Score_Prof ~ Score_Agr, Result_Reg)#, pch = 1)#, cex = .8, col = "red", main = "Health and Professional Occupation", xlab = "Health", ylab = "Professional")
abline(g$coef, lty = 5)

#Prepare Contingency Table

Avr_Agr = 0 #average values of the Agriculture profession.
Avr_Prof = 0 #average value of the Professional  profession.  These average values are required as the basis of the contingency or data table 

#to enact the Chi Square test.
#for loop to calculate the average health scores for Agriculture.
for (i in 1:length(UNIQ_Health)) {
    #to avoid N/A and empty data
    if (Score_Agr[i] > 0)
        Avr_Agr[i] = Score_H[i] / Score_Agr[i]
    }
Avr_Agr[is.na(Avr_Agr)] = 0 # removes the N/A data and allow the chisqure to function.  Ref https://stat.ethz.ch/R-manual/R-devel/library/base/html/NA.html
head(Avr_Agr, n = 1000)
mean(Avr_Agr)


#Avr_Agr <- Avr_Agr[is.finite(rowSums(Avr_Agr))
#head(Avr_Agr,n=1000)
#for loop to calculate the average health scores for Professional.
for (i in 1:length(UNIQ_Health)) {
    #to avoid N/A and empty data 
    #if (Avr_Prof[i] > 0)
    Avr_Prof[i] = Score_H[i] / Score_Prof[i]
}
head(Avr_Prof, n = 1000)
mean(Avr_Prof)



#to remove the N/A data and allow the chisquare to function.  Ref https://stat.ethz.ch/R-manual/R-devel/library/base/html/NA.html
#Avr_Agr[is.na(Avr_Agr)] = 0
Avr_Prof[is.na(Avr_Prof)] = 0
#Avr_Agr[is.infinite(Avr_Agr)] = 0# no longer an issue
#Avr_Prof[is.infinite(Avr_Prof)] = 0# no longer aan issue
head(Avr_Prof, n = 40)

Result = data.frame(Score_H, Avr_Agr, Avr_Prof)
head(Result, 20)

#Regression2
Result_Reg2 = data.frame( Score_H, Score_Prof)
head(Result_Reg2)
Result_Reg3 = data.frame(Score_H, Score_Agr)
head(Result_Reg3)
#Regression Occupations
plot(Score_H ~ Score_Prof, Result_Reg, pch = 10, cex = .6, col = "green", main = "Occupations, Agriculture and Professional", xlab = "Professional", ylab = "Agriculture", abline(0, 1))
#Practical Regression and Anova using R, Julian J. Faraway, July 2002
#Regression Health Score and Occupations
plot(Score_H ~ Score_Agr, Result_Reg2, pch = 1, cex = .8, col = "red", main = "Health and Professional Occupation", xlab = "Health", ylab = "Professional", abline(0, 1))
abline(0, 1)
head(Avr_Agr, n = 10)

#Reg2
Result_Reg2[sample(nrow(Result_Reg2), 1497),] #generating a random sample Ref:https://stat.ethz.ch/pipermail/r-help/2007-February/125860.html 
head(Result_Reg2)
is.numeric(Result_Reg2)
Result_Reg2 <- Result_Reg2[, 1:2] #extra comma to define as numeric, 
is.numeric(Result) #Sample Selection
y <- (head(Result_Reg2, n = 9))
chisq.test(y[, 1:2])
Result_Reg3 <- Result_Reg3[, 1:2] #extra comma to define as numeric, 
is.numeric(Result_Reg3) #Sample Selection
q <- (head(Result_Reg3, n = 9))
chisq.test(q[, 1:2])

chisq.test(y[, 1:2])

#plot of chi square
curve(dchisq(y, df = 28), col = 'red', main = "Chi-Square Density Graph",
          from = 0, to = 60)
xvec <- seq(7.5, 60, length = 101)
pvec <- dchisq(xvec, df = 28)
polygon(c(xvec, rev(xvec)), c(pvec, rep(0, length(pvec))),
col = adjustcolor("black", alpha = 0.3))

 y <- y[, 1:2]
w <- chisq.test(y)
is.numeric(y) #Sample Selectionw
hist(y, prob = TRUE)
curve(dchisq(w, df = 5), col = 'green', add = TRUE)



#Result propper
Result[sample(nrow(Result), 1497),] #generating a random sample Ref:https://stat.ethz.ch/pipermail/r-help/2007-February/125860.html 
head(Result)
is.numeric(Result)
Result <- Result[,1:3] #extra comma to define as numeric, 
is.numeric(Result)
t<-(head(Result, n=9))
chisq.test(t[, 1:3])
chisq.test(Result[, 4:5]) ##
pwr.chisq.test(w = .5, df = 7938, sig.level = .05, power = .9)



mean(Result)
head(t)
Result1<-c(Avr_Agr*Score_H)
Result2 <- c(Avr_Prof * Score_H)
Result3 <-(Result1[1:10], Result2)
head(Result3)
Result <- data.frame(scale(Result$Score_H))
plot(Score_H ~ Avr_Agr, Result)
 abline(0, 1)


mean(Score_Prof)
stat.desc(Score_Prof, basic = TRUE, desc = TRUE, norm = FALSE, p = 0.95)
stat.desc(Score_Agr, basic = TRUE, desc = TRUE, norm = FALSE, p = 0.95)
stat.desc(Score_H, basic = TRUE, desc = TRUE, norm = FALSE, p = 0.95)
wilcox.test(Score__H ~ Score_Prof, data)

x <- Result[, 2]
x <- Result[]
head(x, n=10)
is.numeric(x)
class(x)
y <- Result[, 3] #extra comma to define as numeric, 
# https://stackoverflow.com/questions/7652185/x-must-be-a-numeric-vector-error-from-data-frame-of-numbers
is.numeric(y)
list <- c(x, y)
class(y)
list <- c(x, y)

head(list[y])
cor(Result[sapply(Result, is.numeric)])
#cor.test(list[x], list[y])
plot(Result[3])

plot(Result)
abline(plot(Avr_Agr ~ Avr_Prof, data = Result, col = "white")
abline(lsfit(Result, Avr_Agr),)

cor.test(x, y, method = "spearman")


cor.test(~Avr_Agr, Avr_Prof, method = "spearman"
         data = Result,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)

Result = data.frame(Uniq_Health, Score_Agr, Score_Prof, Avr_Agr, Avr_Prof) #
head(Result)

pwr.chisq.test()
ES.h(Avr_Agr, Avr_Prof)


plot(Result)
plot(Avr_Agr ~ Score_Agr, data = Result) #, col = "white")
sapply(Result[c("Score_Agr", "Score_Prof")], median)


with(Result, wilcox.test(Score_Agr, Score_H, paired = TRUE))
sapply(Result[c("Avr_Agr", "Avg_Prof")], function(x)(c(mean = mean(x), sd = sd(x))))

leveneTest(Avr_Agr ~ Avr_Prof, data = Result) #failed test due to quntitative data

fligner.test(Avr_Agr ~ Avr_Prof, data = Result)
fligner.test(Avr_Agr ~ Score_Agr, data = Result)
fligner.test(Avr_Agr ~ Avr_Prof, data = Result)
fligner.test(Avr_Agr ~ Score_H, data = Result)
fligner.test(Avr_Prof ~ Score_H, data = Result)
fligner.test(Avr_Agr ~ Score_H, data = Result)

#Calculating the standard deviation.  Ref: https://stackoverflow.com/questions/21495649/how-to-calculate-standard-deviation-in-r
n <- length(Avr_Agr) # number of values
std <- sd(Avr_Agr) # sample standard deviation
sqrt((std ^ 2) * ((n - 1) / n))
mean(Avr_Agr)
n <- length(Avr_Prof) # number of values
std <- sd(Avr_Prof) # sample standard deviation
sqrt((std ^ 2) * ((n - 1) / n))
mean(Avr_Prof)
head(Result)


