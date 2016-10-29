#Problem 1
twowayPermTest = function(dat, B)
{
  #Treatment sum of squares of observation
  Ybar.obs = (1/6) * mean(dat$len)
  OJ.obs = dat[which(dat$supp == "OJ"),]
  VC.obs = dat[which(dat$supp == "VC"),]
  Ybar.i.obs = (1/3) * c(mean(OJ.obs$len), mean(VC.obs$len))
  SST.obs = 3 * ((Ybar.i.obs[1]-Ybar.obs)^2 + (Ybar.i.obs[2]-Ybar.obs)^2)
  testStat = vector(mode = "numeric", length = B)
  perm.dat = dat
  for(i in 1:B)
  {
    perm.dat.B1 = perm.dat[which(perm.dat$dose == 0.5),] #Block1
    perm.dat.B2 = perm.dat[which(perm.dat$dose == 1),]   #Block2
    perm.dat.B3 = perm.dat[which(perm.dat$dose == 2),]   #Block3
    perm.dat.B1$supp = sample(perm.dat.B1$supp)
    perm.dat.B2$supp = sample(perm.dat.B2$supp)
    perm.dat.B3$supp = sample(perm.dat.B3$supp)
    perm.dat = rbind(perm.dat.B1, perm.dat.B2, perm.dat.B3)
    
    #Treatment sum of squares of permutated data
    Ybar.perm = (1/6) * mean(perm.dat$len)
    OJ.perm = dat[which(perm.dat$supp == "OJ"),]
    VC.perm = dat[which(perm.dat$supp == "VC"),]
    Ybar.i.perm = (1/3) * c(mean(OJ.perm$len), mean(VC.perm$len))
    SST.perm = 3 * ((Ybar.i.perm[1]-Ybar.perm)^2 + (Ybar.i.perm[2]-Ybar.perm)^2)
    testStat[i] = SST.perm
  }
  pval = (length(which(testStat >= SST.obs)) + 1) / (B + 1)
  pval
}
dat = ToothGrowth
twowayPermTest(dat, 999)

#Problem 2
#Two-sided t-test
setwd("C:/Users/Andy/Desktop")
load("alon.rData")
dat = alon
u = dat$x
R = nrow(dat$x)   #Number of genes
pval = vector(mode = "numeric", length = R) #p-value for each gene

for (i in 1:R) 
{
  testResult = t.test(u[i, 1:5], u[i, -(1:5)])   # should use normal vs. tumor to perform t-test
  pval[i] = testResult$p.value
}
pval

# should sort pval and utilize 4 methods of FWER and FDR


#Problem 3
#Permutation t-test
B = 999
tTest.obs = testResult$statistic #Observed statistic
testStat = vector(mode = "numeric", length = B) #Vector to store statistics
u.perm = u
for(j in 1:B)
{
  u.perm = u.perm[,sample(ncol(u.perm))] #Permute the subjects
  for (i in 1:R) 
  {
    test.perm = t.test(u.perm[i, 1:5], u.perm[i, -(1:5)])
  }
  testStat[j] = test.perm$statistic
}
pval = (length(which(testStat >= tTest.obs)) + 1) / (B + 1) 
pval
