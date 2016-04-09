#Homework 1

#Problem 1(A)
setwd("C:/Users/Andy/Desktop")
load("earthquakes-2014.rda")

#Want events of magnitude at least 2
mag2 = subset(dat, magnitude >= 2)

#Create vector to store observation for each month
months.obs = vector(mode = "numeric", length = 12)

#Store observations for each month into corresponding vector space
for(i in 1:12)
{
  months.obs[i] = length(mag2[which(mag2$month == i),]$month)
}

#Create a table of counts
table = as.data.frame(months.obs)
colnames(table) = "Observations"
table

#Draw a relevant plot for the counts of events
plot(months.obs, xlab = "Months", ylab = "Observations", main = "Observations in Each Month", col = "blue")
axis(side = 1, at = seq(1, 12, 1))
lines(months.obs)
#It appears that March and July are the two months with the most earthquakes

#Problem 1(B)
#Pearson chi-squared goodness-of-fit test
#Null = Earthquakes follow a uniform distribution (Not more prevalent in some months)
#Alternative = Earthquakes do not follow a uniform distribution (More prevalent in some months)
#Significance level = 0.05, and 12 - 1 = 11 degrees of freedom

#The expected frequency under null hypothesis
Uniform.Exp = 1379/12

#Chi-square goodness of fit test
testStatistic = months.obs - (Uniform.Exp)
testStatistic = (testStatistic)^2 / (Uniform.Exp)
testStatistic = sum(testStatistic)
testStatistic
#Large test statistics indicates that the observed counts do not match with the expected counts
#The p-value is extremely small and is less than the significance level (0.05). So the null hypothesis cannot 
#be accepted and this means that earthquakes are indeed more prevalent in some months.

#Problem 2(A)
#The data can be used to create a 2x2 contingency table to perform chi-square test for independence, and the 
#independence or dependence will determine if the applicant's gender influences the admission decisions.

#Problem 2(B)
#Null = Admission decisions are not associated with applicant's gender(Independent)
#Alternative = Admission decisions are associated with applicant's gender(Dependent)

#The expected admission ratio for both male and female is about 41% based on the total admission ratio
#Chi-square test of independence will be used for the hypothesis testing
#Create a 2x2 contingency table first
tableRow = c("Accepted", "Rejected")
tableCol = c("Male", "Female")
table2 = table(tableCol,tableRow)
maleVect = c(3738, 4704)
femaleVect = c(1494, 2827)
table2[1,] = femaleVect
table2[2,] = maleVect
table2

#Now table2 is a 2x2 contingency table, chisq.test function can be applied to test independence
#df = (2-1) * (2-1) = 1
chisq.test(table2)
#The test returns a large value of test statistic (110.85), so the observations do not match with the expected.
#The extreme small p-value is less than the significance level (0.05) in this case. So the null hypothesis cannot 
#be accepted and admission decisions are associated with the applicant's gender.

#Problem 3(A)
#Same hypotheses as Problem 2(B)
#Null = Admission decisions are not associated with applicant's gender(Independent)
#Alternative = Admission decisions are associated with applicant's gender(Dependent)

#Load the dataset and create a 2x2 contingency table based on the 6 tables in the dataset
dat2 = UCBAdmissions
deptA = dat2[,,1]
deptB = dat2[,,2]
deptC = dat2[,,3]
deptD = dat2[,,4]
deptE = dat2[,,5]
deptF = dat2[,,6]
table3 = deptA + deptB + deptC + deptD + deptE + deptF
table3

#table3 is a matrix, convert to table
table3 = as.table(table3)

#Perform another chi-square independence test on the contingency table
chisq.test(table3)
#Same with Problem 2, the chi-square test returns a very large test statistic (91.61) and the p-value is obviously
#less than the significance level of 0.05. So null hypothesis is rejected and there is an association between 
#the applicant's gender and the admission (Dependent).

#Problem 3(B)
plot(table3, main = "UCB admission (6 departments total)")
#Taking a look at the plot of table of summing up all 6 departments, the proportions should be the same 
#if the two variables (male and female) are indepedent. This means that the boxes in the table should line up. 
#Obviously the boxes from the table are not lined up and there exists a gender biasness. 

#Take a look at the plots of admission of individual departments.
deptA = as.table(deptA)
deptB = as.table(deptB)
deptC = as.table(deptC)
deptD = as.table(deptD)
deptE = as.table(deptE)
deptF = as.table(deptF)
plot(deptA, main = "UCB admission")
plot(deptB, main = "UCB admission")
plot(deptC, main = "UCB admission")
plot(deptD, main = "UCB admission")
plot(deptE, main = "UCB admission")
plot(deptF, main = "UCB admission")
#It is important to realize that department A and B have relatively low number of female applicants, so the ratio of 
#admitted female applicants will be higher than male. Other 4 departments do not seem to have gender biasness and
#the numbers of applicants from both genders are fairly close.

#The explanation for the biasness when we sum up all 6 departments is primarily due to department A. It is due to
#the fact that female applicants have very high admitted rate in this department, while male applicant population 
#size is much larger than female applicant population.