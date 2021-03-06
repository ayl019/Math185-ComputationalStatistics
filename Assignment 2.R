#Problem 1(A)
#The 1/2^n factor was there because there are 2^n possible sign vectors (Each entry can take -1 or 1 and there are n entries)
#The result is the proportion of permutations that returns a value of the statistic that is at least as extreme as the statistic 
#on the orginal sample.

#Problem 1(B)
#One-sided
flipSignTest1 = function(x, B = 999)
{
  n = length(x) ##Sample size
  yStar = mean(x) #Original sample mean
  yEps = vector(mode = "numeric") 
  
  #Monto Carlo sampling on sign vector
  for(b in 1 : B)
  {
    e = 2 * rbinom(n, size = 1, prob = 0.5) - 1 #Generate random sign vector
    x.new = x * e
    yEps[b] = mean(x.new)
  }
  
  #Computing p-value
  counts = length(which(yEps >= yStar))
  pVal = (counts + 1) / (B + 1)
  cat("p-value =", pVal)
}

#Problem 1(C)
#Two-sided
flipSignTest2 = function(x, B = 999)
{
  n = length(x) ##Sample size
  yStar = abs(mean(x)) #Original sample mean
  yEps = vector(mode = "numeric") 
  
  #Monto Carlo sampling on sign vector
  for(b in 1 : B)
  {
    e = 2 * rbinom(n, size = 1, prob = 0.5) - 1 #Generate random sign vector
    x.new = x * e
    yEps[b] = abs(mean(x.new))
  }
  
  #Computing p-value
  counts = length(which(yEps >= yStar))
  pVal = (counts + 1) / (B + 1)
  cat("p-value =", pVal)
}

#Problem 2(A)
install.packages("Hmisc")
install.packages("UsingR")
require(UsingR)
dat = father.son
head(dat)
father = dat$fheight
son = dat$sheight
heightDiff = son - father
which(son == father)
#No identical vector elements

#Difference greater than 0 means son is taller, less than 0 means son is shorter
hist(heightDiff, main = "Histogram of Height Difference (Son - Father)", xlab = "Height Difference", col = "Yellow"
     , xlim = c(-10,10), breaks = 20, sub = "Less than 0 = father is shorter; greater than 0 = father is taller")
#It can be seen that there is an asymmetry towards the right, indicating that the son's height tends to exceed the father's height.
#The decision to make the test of hypothesis one-sided should be made before looking at the plot to get a valid p-value.

plot(dat, xlab = "Father's Height", ylab = "Son's Height", main = "Heights of Fathers vs Sons")
abline(a = 0, b = 1, lwd = 3)
#All points above the straight line drawn (Slope 1 and intercept 0) represent the cases that the son is taller than
#his father in the dataset, while points below the line represent the son is shorter. It can be easily seen
#that there are more points above the line. These two visualizations provide an informal solution to the question, and 
#it might be true that a son tends to be taller than his father.

#Problem 2(B)
#Significance level alpha 0.05
#Null hypothesis: The distribution of Z = Y - X is symmetric about 0
#Alternative hypothesis: The distribution of Z is not symmetric about 0
flipSignTest1(heightDiff, 999)
#The p-value is as small as possible, so there is strong evidence against the null hypothesis. So the distribution of Z 
#(Height difference) is not symmetric about 0. This means that it is not true that son's height tends to be shorter
#than his father.

#It is a permutation test because under the null X - Y has the same distribution as Y - X, so if we 
#randomly permute Xi with Yi, we still have the same joint distribution as the original data. Therefore, the p-value of Problem 1 
#is valid.

#Problem 3
#90% bootstrap pivotal confidence interval (One-sided)
B = 5000
alpha = 0.1
x = heightDiff
n = length(x)
avgx = mean(x)
simavg = vector(mode = "numeric", length = n)

for (b in 1 : B)
{
  simx = sample(x, size = n, replace = TRUE)  # bootstrap sample
  simavg[b] = mean(simx)  # the statistic applied to the bootstrap sample
}

CI = c(2*avgx - quantile(simavg, probs = 1 - alpha), Inf)

#The resulting bootstrap confidence interval on the mean difference of son and father heights (Z = Y - X) excludes 0,
#this means that the distribution of Z is very unlikely to be symmetric about 0 and the null hypothesis in Problem 2 is not true. 
#In Problem 2 and 3, the inference is on the population which the sample was taken from, the inference is only valid if the 
#sampling of the population is appropriate.


