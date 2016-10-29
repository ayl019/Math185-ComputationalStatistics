#Problem 1
#Location model, Wilcoxon rank-sum test
#H0: Distribution of X and Y is symmetric about 0
#H1: Distribution of X and Y is not symmetric about 0
B = 200
n = 20
mu = seq(0.1, 1, 0.2)
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = mu[i], sd = 1)
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B
#The values of mu used here are 0.1, 0.3, 0.5, 0.7, and 0.9. As the value of mu increases, the proportion of rejected 
#null hypotheses also increases.

n = 50
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = mu[i], sd = 1)
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B

n = 100
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = mu[i], sd = 1)
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B

#Part 2
#Scale model
B = 200
n = 20
sigma = seq(1.5, 3.5, 0.5)
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = 0, sd = sigma[i])
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B

n = 50
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = 0, sd = sigma[i])
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B

n = 100
rejectNull = vector(mode = "numeric", length = length(mu))
for(i in 1:5)
{
  count = 0
  for(j in 1:B)
  {
    X = rnorm(n, mean = 0, sd = 1)
    Y = rnorm(n, mean = 0, sd = sigma[i])
    pval = wilcox.test(X,Y)$p.value
    if(pval < 0.05)
    {
      count = count + 1
    }
  }
  rejectNull[i] = count
}
rejectNull/B

#Problem 2
boot.combined.test = function(dat, B)
{
  count = 0
  columnNum = ncol(dat)
  blocks = nrow(dat)
  columnMean.obs = vector(mode = "numeric", length = columnNum)
  meandiff.square.obs = vector(mode = "numeric", length = columnNum)
  columnMean.boot = vector(mode = "numeric", length = columnNum)
  meandiff.square.boot = vector(mode = "numeric", length = columnNum)
  SST.bootStat = vector(mode = "numeric", length = B)
  combined = unlist(dat, use.names = FALSE)
  for(i in 1:columnNum)
  {
    columnMean.obs[i] = mean(dat[, i])
  }
  grandMean.obs = mean(combined)
  meandiff.square.obs = (columnMean.obs - grandMean.obs)^2
  SST.obs = sum((columnNum * meandiff.square.obs))

  #Bootstrap sampling
  for(i in 1:B)
  {
    for(j in 1:columnNum)
    {
      dat[, i] = sample(combined, blocks, replace = TRUE)
      columnMean.boot[i] = mean(dat[, i])
    }
    grandMean.boot = mean(unlist(dat, use.names = FALSE))
    meandiff.square.boot = (columnMean.boot - grandMean.boot)^2
    SST.boot = sum((columnNum * meandiff.square.boot))
    if(SST.boot >= SST.obs)
      count = count + 1
  }
  pval = (count + 1) / (B + 1)
  return (pval)
}
setwd("C:/Users/Andy/Desktop")
load("smokers.rda")
boot.combined.test(smokers, 999)
#The null hypothesis in this case tests for whether or not the treatment effect equals 0

#Problem 3
#This function takes in samples containing values j = 1,2,3,...,k. Then it creates 2x2 contingency table for each
#of the j's to perform a chi-squared test for independence. The p-value can be obtained when the sample size is
#large enough and the p-values for each of the j's will be recorded. A p-value less than 0.05 indicates potential
#association on how likely value j to apepar in sample X and Y, so it might be true that P(X = j) = P(Y = j). This
#is opposite of what we want, so the function displays p-values greater than or equal to 0.05.
multiple.categorical.test = function(x, y, ...)
{
k = length(unique(x))
Xi = vector(mode = 'numeric', length = k)
Yi = vector(mode = "numeric", length = k)
pval = vector(mode = "numeric", length = k)
  for(i in 1:k)
  {
    X = (x == i)
    Y = (y == i)
    Xi[i] = length(X[X == TRUE])
    Yi[i] = length(X[X == TRUE])
    table = matrix(c(c(Xi[i], Yi[i]), c(m - Xi[i], n - Yi[i])), nrow = 2, ncol = 2)
    colnames(table) = c("Equal i","Not equal i")#name the columns
    rownames(table) = c("X","Y")#name the rows
    pval[i] = chisq.test(table)$p.value
  }
  return(which(pval >= 0.05))
}
k = 60
n = 2000
m = 3000
x = sample(1:k, m, replace = TRUE)
y = sample(1:k, n, replace = TRUE)
multiple.categorical.test(x,y)

#Problem 4
smooth.spline.CV = function(x, y, K)
{
  m = (1 - (1/K)) * length(x)
  predictError = vector(mode = "numeric", length = K)
  train.X = sample(1:length(x), m, replace = FALSE)
  train.Y = sample(1:length(y), m, replace = FALSE)
  train.X = x[1:900]
  train.Y = y[1:900]
  test.X = x[(m+1):length(x)]
  test.Y = y[(m+1):length(y)]
  for(i in 1:K)
  {
    fit = smooth.spline(train.X, train.Y, df = i+1)
    yhat = predict(fit, test.X)$y
    predictError[i] = sum((yhat - test.Y)^2)/length(x)
  }
  plot(predictError, type = 'o', col = "red", xlab = "Theta", main = "Prediction Error and Theta")
}
install.packages("splines")
require(splines)
f = function(x) (1 + 10*x - 5*x^2)*sin(10*x)
n = 1e3
x = runif(n)
x = sort(x)
y = f(x) + rnorm(n)
smooth.spline.CV(x,y,10)