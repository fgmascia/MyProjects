#MM915_project, year2004

#install.packages("R2OpenBUGS")
install.packages("spDataLarge")
library(Matrix)
library(spData)
library(sp)
library(sf)
library(spdep)
library(MASS)
library(Rcpp)
library(R2OpenBUGS)
library(CARBayes)

names(london_2)
head(london_2@data)

#1a
london_2.2004 <- london_2
london_2.2004@data <- subset(london_2.2004@data, year=="2004")

#1b The standardised incidence ratio (SIR) is calculated as the 
#ratio of the observed number of cases to the expected number of cases
attach(london_2.2004@data)
london_2.2004@data[["SIR"]] <- Observed/Expected
london_2.2004[["SIR"]] <- Observed/Expected

nrow(london_2.2)
     
mean(london_2.2004[["SIR"]])
max(london_2.2004[["SIR"]])
min(london_2.2004[["SIR"]])
#1c
spplot(london_2.2004, "SIR", main="SIR Spatial Plot in Greater London")

#1d
par(mfrow=c(1,3))
plot(SIR, PM25, main="PM25~SIR", 
     xlab="SIR", ylab="PM25", pch=1, col=1)
plot(SIR, JSA, main="JSA~SIR", 
     xlab="SIR", ylab="JSA", pch=1, col=2)
plot(SIR, Price, main="Price~SIR", 
     xlab="SIR", ylab="Price", pch=1, col=3)


#2a
attach(london_2.2004@data)
#Lecture 7
Data  <- list(Y = london_2.2004@data$Observed, E = london_2.2004@data$Expected, 
              weight1 = london_2.2004@data$PM25, weight2 = london_2.2004@data$JSA, 
              weight3 = london_2.2004@data$Price, N = nrow(london_2.2004@data))

Inits <- list(list(beta0 = 0, beta1 = 0, beta2 = 0, beta3 = 0))

pois.sim <- bugs(data=Data, inits = rep(Inits, 3),
                parameters.to.save = c("beta0", "beta1", "beta2", "beta3"),
                model.file = "PoissonModel.txt",
                n.iter = 22000,
                n.burnin = 8000,
                n.thin = 1,
                n.chains = 3)

par(mfrow = c(2,2))
plot(pois.sim$sims.list$beta0, type = "l", ylab = "Samples beta0", main="Traceplot beta0")
plot(pois.sim$sims.list$beta1, type = "l", ylab = "Samples beta1",main="Traceplot beta1")
plot(pois.sim$sims.list$beta2, type = "l", ylab = "Samples beta1",main="Traceplot beta2")
plot(pois.sim$sims.list$beta3, type = "l", ylab = "Samples beta1",main="Traceplot beta3")



#2b Gelman-Rubin and Geweke
library(coda)
z <- pois.sim$summary
head(z)

# look at the Rhat values, and plot in a histogram
z[,8]
hist(z[,8], main="Gelman-Rubin Diagnostic", xlab="Rhat", col=3)

par(mfrow=c(2,2))
geweke.plot(as.mcmc(as.data.frame(pois.sim$sims.list$beta0)))
geweke.plot(as.mcmc(as.data.frame(pois.sim$sims.list$beta1)))
geweke.plot(as.mcmc(as.data.frame(pois.sim$sims.list$beta2)))
geweke.plot(as.mcmc(as.data.frame(pois.sim$sims.list$beta3)))
par(mfrow=c(1,1))

plot(pois.sim$sims.list$beta0, type = "l", ylab = "Samples beta0")
plot(pois.sim$sims.list$beta1, type = "l", ylab = "Samples beta1")
plot(pois.sim$sims.list$beta1, type = "l", ylab = "Samples beta2")
plot(pois.sim$sims.list$beta1, type = "l", ylab = "Samples beta3")


#2c Pearson
#use posterior mean of slope and intercept to calculate the log rate
log_rate <- mean(pois.sim$sims.list$beta0) +
 mean(pois.sim$sims.list$beta1) * london_2.2004@data$PM25 +
 mean(pois.sim$sims.list$beta2) * london_2.2004@data$JSA +
 mean(pois.sim$sims.list$beta3) * london_2.2004@data$Price
  
# exponentiate and multiply by expected cases
mu_preg <- london_2.2004@data$Expected * exp(log_rate)
pearson_preg <- (london_2.2004@data$Observed - mu_preg) / sqrt(mu_preg)
plot(pearson_preg ~ mu_preg, main="Pearson Residuals Diagnostic", xlab = "Fitted values", ylab = "Pearson residual")
#look for funnel shapes, if there are residuals are not random

#3a
install.packages("car") 
library(car)
# Produce the adjacency matrix, and various pieces of associated info BUGS needs
W <- nb2mat(poly2nb(london_2.2004), style = "B")
inds <- lapply(1:nrow(W), function(i) which(W[i, ] == 1))
Adj <- Reduce("c", inds)
Num.Adj <- rowSums(W)
SumNumNeigh <- sum(Num.Adj)
# combine all of the data, and constants into a single list
Data2 <- list(observed = london_2.2004@data$Observed, expected = london_2.2004@data$Expected,
             N = nrow(london_2.2004@data), weight1 = london_2.2004@data$PM25, weight2 = london_2.2004@data$JSA, 
             weight3 = london_2.2004@data$Price, Adj = Adj, Num = Num.Adj,
             SumNumNeigh = SumNumNeigh)
# run OpenBUGs, save the spatial random effect parameters and variances
pCAR.bugs <- bugs(data = Data2,
                  model.file = "CARmodel.txt",
                  parameters.to.save = c("beta0", "beta1", "beta2", "beta3", "phi"),
                  inits = rep(list(list(beta0=0, beta1=0, beta2=0, beta3=0)), times=3),
                  n.iter = 22000,
                  n.burnin = 8000,
                  n.thin = 1,
                  n.chains = 3)

#3b)
z2 <- pCAR.bugs$summary
head(z2)
z2[,8]
hist(z2[,8], main="Gelman-Rubin Diagnostic", xlab="Rhat", col=3)

par(mfrow = c(2,2))
plot(pCAR.bugs$sims.list$beta0, type = "l", ylab = "Samples beta0", main="Traceplot beta0")
plot(pCAR.bugs$sims.list$beta1, type = "l", ylab = "Samples beta1",main="Traceplot beta1")
plot(pCAR.bugs$sims.list$beta2, type = "l", ylab = "Samples beta1",main="Traceplot beta2")
plot(pCAR.bugs$sims.list$beta3, type = "l", ylab = "Samples beta1",main="Traceplot beta3")

par(mfrow=c(1,1))
geweke.plot(as.mcmc(as.data.frame(pCAR.bugs$sims.list$beta0)))
geweke.plot(as.mcmc(as.data.frame(pCAR.bugs$sims.list$beta1)))
geweke.plot(as.mcmc(as.data.frame(pCAR.bugs$sims.list$beta2)))
geweke.plot(as.mcmc(as.data.frame(pCAR.bugs$sims.list$beta3)))

#3c)
log_rate2 <- mean(pCAR.bugs$sims.list$beta0) +
  mean(pCAR.bugs$sims.list$beta1) * london_2.2004@data$PM25 +
  mean(pCAR.bugs$sims.list$beta2) * london_2.2004@data$JSA +
  mean(pCAR.bugs$sims.list$beta3) * london_2.2004@data$Price + 
  mean(pCAR.bugs$sims.list$phi)

# exponentiate and multiply by expected cases
mu_preg2 <- london_2.2004@data$Expected * exp(log_rate2)
pearson_preg2 <- (london_2.2004@data$Observed - mu_preg2) / sqrt(mu_preg2)
plot(pearson_preg2 ~ mu_preg2, main="Pearson Residuals Diagnostic", xlab = "Fitted values", ylab = "Pearson residual")

#4)
a1=pois.sim$DIC
a2=pCAR.bugs$DIC

b1=var(pearson_preg)
b2=var(pearson_preg2)

moran.test(pearson_preg, nb2listw(poly2nb(london_2.2004)), alternative = "two.sided")
moran.test(pearson_preg2, nb2listw(poly2nb(london_2.2004)), alternative = "two.sided")

d1=z[2,1]
d2=z2[2,1]

e1=z[2,3]
e2=z2[2,3]

f1=z[2,7]
f2=z2[2,7]

models <- matrix(c(a1,b1,0.15,d1,e1,f1,a2,b2,0.16,d2,e2,f2),ncol=6,byrow=TRUE)
colnames(models) <- c("DIC","PearsonVar","MoransI","PM25effect","Lower2.5","Upper97.5")
rownames(models) <- c("Poisson","CAR")
models2 <- as.table(models)
round(models2, digits = 3)


