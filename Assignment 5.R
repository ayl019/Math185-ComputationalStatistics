#Problem 1
#Sample size was changed to 1000 because my computer could not run 10000.
n = 1000
B = 999
sample.X = runif(n, min = -1, max = 1)
sample.Y =  (sample.X)^2
Pearson.cortest = cor.test(sample.X, sample.Y, method = "pearson", alternative = "two.sided")
Spearman.cortest = cor.test(sample.X, sample.Y, method = "spearman", alternative = "two.sided")
Kendall.cortest = cor.test(sample.X, sample.Y, method = "kendall", alternative = "two.sided")

Pearson.Pval = Pearson.cortest$p.value
Spearman.Pval = Spearman.cortest$p.value
Kendall.cortest = Kendall.cortest$p.value

Pearson.permute.pval = vector(mode = "numeric", length = B)
Spearman.permute.pval = vector(mode = "numeric", length = B)
Kendall.permute.pval = vector(mode = "numeric", length = B)

for(i in 1:B)
{
  Permute.Y = sample(sample.Y, size = n, replace = FALSE)
  Pearson.permute.cortest = cor.test(sample.X, Permute.Y, method = "pearson", alternative = "two.sided")
  Spearman.permute.cortest = cor.test(sample.X, Permute.Y, method = "spearman", alternative = "two.sided")
  Kendall.permute.cortest = cor.test(sample.X, Permute.Y, method = "kendall", alternative = "two.sided")
  
  Pearson.permute.pval[i] = Pearson.permute.cortest$p.value
  Spearman.permute.pval[i] = Spearman.permute.cortest$p.value
  Kendall.permute.pval[i] = Kendall.permute.cortest$p.value
} 
boxplot(Pearson.permute.pval, Spearman.permute.pval, Kendall.permute.pval, ylab = "P-value", 
        names = c("Pearson", "Spearman", "Kendall"), xlab = "Test performed")
#The boxplot indicates that the p-values generated from each of the tests are pretty close.
#Since the null hypothesis is whether or not the correlation between X and Y is zero, we always fail to reject the
#null hypothesis due to the high p-values from the tests. But this does not mean that X and Y are not associated.

#Problem 2
install.packages("Hmisc")
library(Hmisc)
hoeff.test = function(z, B)
{
  D.Statistic.permute.vect = vector(mode = "numeric", length = B)
  sample.X = z[,-2]
  sample.Y = z[,-1]
  sample.Hoeffd = hoeffd(sample.X, sample.Y)
  sample.DStatistic = sample.Hoeffd$D
  sample.DStatistic = sample.DStatistic[1,2] #The higher value means more correlation between X and Y
  for(i in 1:B)
  {
    Permute.Y = sample(sample.Y, size = length(sample.Y), replace = FALSE)
    Permute.Hoeffd = hoeffd(sample.X, Permute.Y)
    Permute.DStatistic = Permute.Hoeffd$D
    D.Statistic.permute.vect[i] = Permute.DStatistic[1,2]
  }
  pval = (length(which(D.Statistic.permute.vect >= sample.DStatistic)) + 1) / (B + 1)
  pval
}

#H0: X and Y are independent
#H1: X and Y are dependent
#Use the same X and Y from Problem 1 with sample size of 1000 to test the function
n = 1000
sample.X = runif(n, min = -1, max = 1)
sample.Y = sample.X^2
Z = cbind(sample.X, sample.Y)
hoeff.test(Z, 999)
#The extreme small p-value suggests that the null hypothesis is rejected, which means that X and Y are indeed dependent,
#or associated. This is the result that should be expected because the X and Y samples used in this case are perfectly
#associated according to lecture notes.

#Problem 3(a)
#Manually import the dataset using Import Dataset button
#The imported dataframe is called sea_ice_data
dat = sea_ice_data[-1,]
X = as.numeric(paste(dat$V1))
Y = as.numeric(paste(dat$V2))
fit.affine = lm(Y ~ X)
plot(X, Y, xlab = "Year", ylab = "Arctic Sea Ice Extent (1,000,000 sq km)", main = "Linear Model Fit")
abline(fit.affine, col = "red", lwd = 2)

#Problem 3(b)
#Quadratic fit
fit.quad = lm(Y ~ poly(X, 12, raw = TRUE))
val.quad = predict(fit.quad)
plot(X, Y, xlab = "Year", ylab = "Arctic Sea Ice Extent (1,000,000 sq km)", main = "Quadratic Model Fit")
lines(X, val.quad ,col = "red", lwd = 2)
S = summary(fit.quad)

#Degree 3 and above
B = 10
R = vector(mode = "numeric", length = B)
for(i in 1:B)
{
  fit = lm(Y ~ poly(X, i, raw = TRUE))
  S = summary(fit)
  R[i] = S$r.squared
}
plot(1:10, R, xlab = "Degree", ylab = "R-squared", main = "R-squared for polynomial models", lwd = 2, col = "red", pch = 5)
lines(1:10, R, type='l', lwd = 4, col = "red")
#Improvement is minimal beyond degree 4

#Problem 3(c)
Knots = quantile(X, seq(0, 1, len = 6), type = 1)
require(splines)
plot(X, Y, xlab = "Year", ylab = "Arctic Sea Ice Extent (1,000,000 sq km)", main = "Spline Model Fit")
fit.spline = lm(Y ~ bs(X, degree=1, knots=Knots))
val.spline = predict(fit.spline)
lines(X, val.spline, col = "red", lwd = 2)

#Problem 3(d)
plot(X, Y, xlab = "Year", ylab = "Arctic Sea Ice Extent (1,000,000 sq km)", main = "Overlaying All Models")
abline(fit.affine, col = "yellow", lwd = 2)
lines(X, val.quad ,col = "red", lwd = 2)
lines(X, val.spline, col = "blue", lwd = 2)
legend(2005, 7.5, c("Linear", "Quadratic", "Spline"), lty = c(1,1), lwd = 2, col = c("yellow", "red", "blue"))

#Problem 4
boot.regression = function(x, y, conf, residual, B)
{
  alpha = 1 - conf
  if(residual == TRUE)
  {
    Slope = vector(mode = "numeric", length = B)
    Intercept = vector(mode = "numeric", length = B)
    boot.y = vector(mode = "numeric", length = length(y))
    for(i in 1:B)
    {    
      LS = lm(y ~ x) 
      resid = as.vector(LS$residuals)
      resid.boot = sample(resid, length(resid), replace = TRUE)
      for(j in 1:length(resid.boot))
      {
        boot.y[j] = y[j] + resid.boot[j]
      }
      LS = lm(boot.y ~ x)
      Intercept[i] = as.numeric(LS$coefficients[1])
      Slope[i] = as.numeric(LS$coefficients[2]) 
    }
    Intercept = sort(Intercept)
    Slope = sort(Slope)
    Intercept.CI = c(Intercept[B * (alpha / 2)], Intercept[B * ((1 - alpha) / 2)])
    Slope.CI = c(Slope[B * (alpha / 2)], Slope[B * ((1 - alpha) / 2)])
    Intercept.CI
    Slope.CI
  }
  else
  {
    Slope = vector(mode = "numeric", length = B)
    Intercept = vector(mode = "numeric", length = B)
    for(i in 1:B)
    {    
      boot.x = sample(x, length(x), replace = TRUE)
      boot.y = sample(y, length(y), replace = TRUE)
      LS = lm(boot.y ~ boot.x) 
      Intercept[i] = as.numeric(LS$coefficients[1])
      Slope[i] = as.numeric(LS$coefficients[2]) 
    }
    Intercept = sort(Intercept)
    Slope = sort(Slope)
    Intercept.CI = c(Intercept[B * (alpha / 2)], Intercept[B * ((1 - alpha) / 2)])
    Slope.CI = c(Slope[B * (alpha / 2)], Slope[B * ((1 - alpha) / 2)])
    Intercept.CI
    Slope.CI
  }
}

#did not do studentized pivotal