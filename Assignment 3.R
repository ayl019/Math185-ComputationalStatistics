#Problem 1(A)
#Testing about the means
m = 1000
n = 3000
B = 10000
X = rnorm(m, mean = 0, sd = 1)
Y = rnorm(n, mean = 0, sd = sqrt(5))
meanDiff = mean(X) - mean(Y)
Z = c(X,Y)
meanDiff.sim = vector(mode = "numeric", length = B)

#Value from one permutation
Z.permu = sample(Z)
mean(Z.permu[1:m]) - mean(Z.permu[(m+1):(m+n)])

#Monte Carlo 10000 times
for (b in 1:B) 
{
  Z.permu = sample(Z)
  meanDiff.sim[b] = mean(Z.permu[1:m]) - mean(Z.permu[(m+1):(m+n)])
}
p.val = (sum(meanDiff.sim >= meanDiff) + 1) / (B + 1)
p.val
t.test(Z)
originalSample = rnorm(B, 0, ((1/m)+(5/n))) # should use sqrt((1/m)+(5/n))
plot(density(originalSample))
plot(density(meanDiff.sim))
#The distribution for the Monte Carlo sample and the original sample appears to be symmetic about 0

# explaination is incorrect

#Problem 1(B)
m = 2000
n = 2000
X = rnorm(m, mean = 0, sd = 1)
Y = rnorm(n, mean = 0, sd = sqrt(5))
meanDiff = mean(X) - mean(Y)
Z = c(X,Y)
meanDiff.sim = vector(mode = "numeric", length = B)

#Value from one permutation
Z.permu = sample(Z)
mean(Z.permu[1:m]) - mean(Z.permu[(m+1):(m+n)])

#Monte Carlo 10000 times
for (b in 1:B) 
{
  Z.permu = sample(Z)
  meanDiff.sim[b] = mean(Z.permu[1:m]) - mean(Z.permu[(m+1):(m+n)])
}
p.val = (sum(meanDiff.sim >= meanDiff) + 1) / (B + 1)
p.val
t.test(Z)
plot(density(originalSample))
plot(density(meanDiff.sim))
#The distribution for the Monte Carlo sample and the original sample appears to be symmetic about 0

#Problem 2(A)
#The null hypothesis to test is whether or not the values in the sample are random

#Problem 2(B)
#Load data and install packages
setwd("C:/Users/Andy/Desktop")
load("cloudseeding.rda")
dat = cloudseeding
install.packages("randtest")
library(randtests)
x = dat$seeded
y = dat$unseeded
wilcox.test(x,y)

#Problem 2(C)
nb.runs.test = function(x, y, B = 999)
{
  m = length(x)
  n = length(y)
  D = mean(x) - mean(y)
  Z = c(x, y)
  B = 999
  D.sim = vector(mode = "numeric", length = B)
  for (b in 1:B) 
  {
    Zperm = sample(Z)
    D.sim[b] = mean(Zperm[1:m]) - mean(Zperm[(m+1):(m+n)])
  }
  p.val = (sum(D.sim >= D)+1)/(B+1)
  p.val
}
nb.runs.test(x, y , B=999)
#The p-value is obtained from permutation test and the p-value is exact

#Problem 2(D)
#For one-sided version, the absolute value of the sample means for x and y should be taken.

#Problem 3
nb.runs.sym.test = function(x, B=999)
{
  X = x[which(x > 0)]
  Y = x[which(x < 0)]
  m = length(X)
  n = length(Y)
  B = 999
  D.sim = vector(mode = "numeric", length = B)
  for (b in 1:B) 
  {
    x.perm = sample(x)
    D.sim[b] = mean(x.perm[1:m]) - mean(x.perm[(m+1):(m+n)])
  }
  p.val = (sum(D.sim >= 0)+1)/(B+1)
  p.val
}
W = rnorm(500)
nb.runs.sym.test(W, B=999)
wilcox.test(W)
