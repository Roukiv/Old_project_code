setwd("C:/Users/Ivor Kuan/Desktop/Exeter/MTHM017/assignment/A")
londonor <- read.csv("London_Pollution.csv")
require(tidyverse)
require(TSA)
library(lubridate)
# 1
# figure 1
summary(londonor[,c(3,4)]) 

london.na <- londonor[rowSums(is.na(londonor)) > 0,]

bexley.na <- london.na[,c(1,2,3)]
bexley.na <- bexley.na[rowSums(is.na(bexley.na)) > 0,]
bexley.na <- bexley.na[,2]
bexley.na <- data.frame(year = year(bexley.na),
                        month = month(bexley.na),
                        day = day(bexley.na))

hounslow.na <- london.na[,c(1,2,4)]
hounslow.na <- hounslow.na[rowSums(is.na(hounslow.na)) > 0,]
hounslow.na <- hounslow.na[,2]
hounslow.na <- data.frame(year = year(hounslow.na),
                          month = month(hounslow.na),
                          day = day(hounslow.na))
# table 1
table(bexley.na$year)
table(hounslow.na$year)

# figure 2
ggplot(londonor, aes(x=Bexley)) + 
  geom_histogram(binwidth=5, fill="lightgreen",colour="black")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of Bexley PM10 concentration",
       x = "Bexley PM10 concentration in microgram per cubic meter", 
       y = "Frequency")+
  xlim(0, 100) +
  ylim(0, 550)

ggplot(londonor, aes(x=Hounslow)) + 
  geom_histogram(binwidth=5, fill="lightgreen",colour="black")+
  geom_density(alpha=.2, fill="#FF6666") +
  labs(title="Histogram of Hounslow PM10 concentration",
       x = "Hounslow PM10 concentration in microgram per cubic meter", 
       y = "Frequency") +
  xlim(0, 100) +
  ylim(0, 550)

bt <-c()
ht <-c()
for (i in 1:5){
  for (j in 1:12){
    bt <- c(bt,unlist(count(bexley.na[bexley.na$year==1999+i&bexley.na$month==j,])))
    ht <- c(ht,unlist(count(hounslow.na[hounslow.na$year==1999+i&hounslow.na$month==j,])))
  }
}
bt <- ts(bt,start=c(2000,1),end=c(2004,12),frequency=12)
ht <- ts(ht,start=c(2000,1),end=c(2004,12),frequency=12)

# figure 3
plot.ts(bt, main = "Bexley missing data count", 
        xlab = "Time (Year)", ylab = "Amount per month")
plot.ts(ht, main = "Hounslow missing data count", 
        xlab = "Time (Year)", ylab = "Amount per month")



# 2

londonor[is.na(londonor)] <- 0

# figure 4
ggplot(londonor,aes(x=as.Date(Date),y=Bexley, colour=Bexley)) + geom_line()+ 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  labs(title="Bexley PM10 concentration in microgram per cubic meter",
       x = "Date (Year)", y = "Concentration")

ggplot(londonor,aes(x=as.Date(Date),y=Hounslow, colour=Hounslow)) + geom_line()+ 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  labs(title="Hounslow PM10 concentration in microgram per cubic meter",
       x = "Date (Year)", y = "Concentration")


# as the minimum value of the Bexley and Hounslow is 3 and 6.4 respectively, 
#     so horizontal line y=2 in red is there. This shows the zeros that replace NAs.


# 3
library(CARBayes)
library(spdep)
library(CARBayes)
library(rgdal)
library(rgeos)
library(RColorBrewer)



# Reading in Scotland shapefiles 
London <- readOGR(dsn = '.',
                    layer = 'London')

London <- st_as_sf(London)

# figure 5
ggplot(London, aes(fill = NONLD_AREA)) +
  geom_sf() + 
  # Black and white theme
  theme_bw() + 
  labs(x = 'Longitude', y = 'Latitude') +
  geom_text(x=514070, y=176480, label="Hounslow",color="red") +
  geom_text(x=548862, y=175380, label="Bexley",color="red")


# 4,5

library(R2jags)
library(MCMCvis)
library(lattice)

jags.mod <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  for (i in 2 : 1461) {
    Bexley[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(Y[i-1], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
}

N <- 1461 # end of 2003


Bexley <- londonor$Bexley
jags.data <- list("Bexley")


# 16.5 median, 18.89 mean
inb <- londonor
inb[is.na(inb)==0] <- 0
inb <- inb$Bexley
inb[is.na(inb)] <- 16.5
inb[inb==0] <- NA
inb1 <- inb
inb2 <- inb+2.39


jags.inits1 <- c(tau.w = 0.005, tau.v = 0.02, Bexley = inb1, Y = c(runif(1461,15,25)))
jags.inits2 <-c(tau.w = 0.01, tau.v = 0.01, Bexley = inb2, Y = c(runif(1461,15,25)))
jags.inits <- list(jags.inits1,jags.inits2)



jags.param <- c("tau.w","tau.v","Y")



jags.mod.fit <- jags(data = jags.data, inits = jags.inits,
                           parameters.to.save = jags.param, n.chains = 2,
                           n.burnin = 4000, n.iter = 8000, n.thin = 1,
                           model.file = jags.mod)


# trace plot, figure 6
jagsfit.mcmc <- as.mcmc(jags.mod.fit)
MCMCtrace(jagsfit.mcmc, params = c("Y[13]","Y[1054]","Y[40]","Y[596]","Y[1286]",
                                   "Y[701]","deviance","tau.v","tau.w"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)
# not miss 13 1054, missing but not missing in 5 previous obs 40 1286, 
#       missing with a lot before 596 701

# table 2
MCMCsummary(jagsfit.mcmc, ISB = F,
            params = c("deviance","tau.v","tau.w","Y[13]","Y[1054]",
                       "Y[40]","Y[596]","Y[1286]","Y[701]"))


# Rhat check
test <- MCMCsummary(jagsfit.mcmc)
test <- filter(test, Rhat > 1.05)

bexley.na <- london.na[,c(1,2,3)]
bexley.na <- bexley.na[rowSums(is.na(bexley.na)) > 0,]
bexley.na <- bexley.na[,c(1,2)]

# Gelman-Rubin statistic
test2 <- as.data.frame(gelman.diag(jagsfit.mcmc)$psrf)
names(test2)[1:2]<-paste(c("pest","upci"))
test2 <- filter(test2, upci > 1.05) # 144 obs

# not all chain converge as there are Rhat and upper CI value of the parameter > 1.05

# material may need
# https://mc-stan.org/rstan/reference/Rhat.html



# 6

mci <- MCMCsummary(jagsfit.mcmc)
mci <- mci[-c(1:3),c(1,3,5)]

a <- c()
for (i in 1:1461){
  z <- paste(c("Y[",i,"]"),collapse="")
  a <- rbind(a,mci[z,])
}
mci <- cbind(a,londonor$Date[1:1461],londonor$Bexley[1:1461])
names(mci)[2:5]<-paste(c("q25","q975","date","Bexley"))

# figure 7
ggplot(mci, aes(x=as.Date(date))) + 
  geom_line(aes(y = mean), color = "darkred") + 
  geom_line(aes(y = q25), color="steelblue") +
  geom_line(aes(y = q975), color="green") +
  geom_line(aes(y = Bexley), color="orange") +
  labs(title="Posterior mean",
       x = "Date (Year)", y = "Y_hat")

bexley.na # by compare with this

# shoet like few day of missing doesn't make much different, but the gap of 
#    second half of 2001 there is like around 5~6 months of missing, so
#    very uncertain. Turns out large CI.




# 7


jags.mod2 <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  Y[2] ~ dnorm(0, 1.0E-3)
  for (i in 3 : 1461) {
    Bexley[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(2*Y[i-1]-Y[i-2], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
}

jags.mod.fit2 <- jags(data = jags.data, inits = jags.inits,
                     parameters.to.save = jags.param, n.chains = 2,
                     n.burnin = 4000, n.iter = 80000, n.thin = 1,
                     model.file = jags.mod2)


# trace plot, figure 8
jagsfit.mcmc2 <- as.mcmc(jags.mod.fit2)
MCMCtrace(jagsfit.mcmc2, params = c("Y[13]","Y[1054]","Y[40]","Y[596]",
                                    "Y[1286]","Y[701]","deviance","tau.v","tau.w"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)
# not miss 13 1054, missing but not missing in 5 previous obs 40 1286, 
#       missing with a lot before 596 701

# table 3
MCMCsummary(jagsfit.mcmc2, ISB = F,
            params = c("deviance","tau.v","tau.w","Y[13]","Y[1054]",
                       "Y[40]","Y[596]","Y[1286]","Y[701]"))
# definition of smoothing
# https://ec.europa.eu/eurostat/statistics-explained/index.php?title=Glossary:Smoothing


# Gelman-Rubin statistic
test22 <- as.data.frame(gelman.diag(jagsfit.mcmc2)$psrf)
names(test22)[1:2]<-paste(c("pest","upci"))
test22 <- filter(test22, upci > 1.05) # 1228 obs
# a lot more than before

# lack of converge explain by covariance 
# https://stats.stackexchange.com/questions/99796/how-can-i-show-that-a-random-walk-is-not-covariance-stationary








# 8

jags.mod.p <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  for (i in 2 : 1468) {
    Bexley[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(Y[i-1], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Bexley)^2)/1468)^0.5
}


Bexley <- c(londonor$Bexley[1:1461],rep(NA,7))
jags.data <- list("Bexley")



# 16.5 median, 18.89 mean
inb <- londonor
inb[is.na(inb)==0] <- 0
inb <- c(inb$Bexley[1:1461],rep(NA,7))
inb[is.na(inb)] <- 16.5
inb[inb==0] <- NA
inb1 <- inb
inb2 <- inb+2.39



jags.inits1 <- c(tau.w = 0.005, tau.v = 0.02, Bexley = inb1, 
                 Y = c(runif(1468,15,25)))
jags.inits2 <-c(tau.w = 0.01, tau.v = 0.01, Bexley = inb2, 
                Y = c(runif(1468,15,25)))
jags.inits <- list(jags.inits1,jags.inits2)

jags.param <- c("tau.w","tau.v","Y","rmspr")




jags.mod.fit.p <- jags(data = jags.data, inits = jags.inits,
                     parameters.to.save = jags.param, n.chains = 2,
                     n.burnin = 4000, n.iter = 8000, n.thin = 1,
                     model.file = jags.mod.p)


jagsfit.mcmcp <- as.mcmc(jags.mod.fit.p)
MCMCsummary(jagsfit.mcmcp, ISB = F, params = c("rmspr"))
# mean 3.893743 




jags.mod2.p <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  Y[2] ~ dnorm(0, 1.0E-3)
  for (i in 3 : 1468) {
    Bexley[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(2*Y[i-1]-Y[i-2], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Bexley)^2)/1468)^0.5
}

jags.mod.fit.p2 <- jags(data = jags.data, inits = jags.inits,
                       parameters.to.save = jags.param, n.chains = 2,
                       n.burnin = 4000, n.iter = 8000, n.thin = 1,
                       model.file = jags.mod2.p)


jagsfit.mcmcp2 <- as.mcmc(jags.mod.fit.p2)
MCMCsummary(jagsfit.mcmcp2, ISB = F, params = c("rmspr"))
# mean 7.022862 



pcm <- cbind(MCMCsummary(jagsfit.mcmcp)[-c(1:4),c(1,3,5)],
             MCMCsummary(jagsfit.mcmcp2)[-c(1:4),c(1,3,5)])

a <- c()
for (i in 1:1468){
  z <- paste(c("Y[",i,"]"),collapse="")
  a <- rbind(a,pcm[z,])
}


pcm <- cbind(a,londonor$Date[1:1468],londonor$Bexley[1:1468])
names(pcm)[1:8]<-paste(c("mm1","lbm1","ubm1","mm2","lbm2","ubm2",
                         "date","act"))
ppcm <- pcm[c(1433:1468),]

# figure 9
ggplot(ppcm, aes(x=as.Date(date))) + 
  geom_line(aes(y = mm1), color = "red") + 
  geom_line(aes(y = lbm1), color="darkred") +
  geom_line(aes(y = ubm1), color="darkred") +
  geom_line(aes(y = mm2), color = "blue") + 
  geom_line(aes(y = lbm2), color="steelblue") +
  geom_line(aes(y = ubm2), color="steelblue") +
  geom_line(aes(y = act), color="green") +
  geom_vline(xintercept = as.Date("2004-01-01"), color = "black") +
  labs(title="Posterior mean",
       x = "Date", y = "Prediction value")




# 10

jags.mod.hvp <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  for (i in 2 : 1461) {
    Hounslow[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(Y[i-1], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Hounslow)^2)/1461)^0.5
}


Hounslow <- c(londonor$Hounslow[1:1461])
jags.data <- list("Hounslow")



# 22.30 median, 24.52 mean
inb <- londonor
inb[is.na(inb)==0] <- 0
inb <- c(inb$Hounslow[1:1461])
inb[is.na(inb)] <- 22.3
inb[inb==0] <- NA
inb1 <- inb
inb2 <- inb+2.22



jags.inits1 <- c(tau.w = 0.005, tau.v = 0.02, Hounslow = inb1, 
                 Y = c(runif(1461,15,25)))
jags.inits2 <-c(tau.w = 0.01, tau.v = 0.01, Hounslow = inb2, 
                Y = c(runif(1461,15,25)))
jags.inits <- list(jags.inits1,jags.inits2)
jags.param <- c("tau.w","tau.v","Y","rmspr")




jags.mod.fit.hvp <- jags(data = jags.data, inits = jags.inits,
                       parameters.to.save = jags.param, n.chains = 2,
                       n.burnin = 4000, n.iter = 8000, n.thin = 1,
                       model.file = jags.mod.hvp)


jagsfit.mcmchvp <- as.mcmc(jags.mod.fit.hvp)

# table 4
MCMCsummary(jagsfit.mcmchvp, ISB = F, 
            params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                       "Y[40]","Y[596]","Y[1286]","Y[701]"))
# figure 10
MCMCtrace(jags.mod.fit.hvp, params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                                       "Y[40]","Y[596]","Y[1286]","Y[701]"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)




# mean 5.174294 




jags.mod.hvp2 <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  Y[2] ~ dnorm(0, 1.0E-3)
  for (i in 3 : 1461) {
    Hounslow[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(2*Y[i-1]-Y[i-2], tau.w)
  }
  #  priors
  tau.w ~ dgamma(1,0.01)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(1,0.01)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Hounslow)^2)/1461)^0.5
}

jags.mod.fit.hvp2 <- jags(data = jags.data, inits = jags.inits,
                        parameters.to.save = jags.param, n.chains = 2,
                        n.burnin = 4000, n.iter = 8000, n.thin = 1,
                        model.file = jags.mod.hvp2)


jagsfit.mcmchvp2 <- as.mcmc(jags.mod.fit.hvp2)

# table 5
MCMCsummary(jagsfit.mcmchvp2, ISB = F, 
            params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                       "Y[40]","Y[596]","Y[1286]","Y[701]"))
# figure 11
MCMCtrace(jags.mod.fit.hvp2, params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                                        "Y[40]","Y[596]","Y[1286]","Y[701]"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)

# mean 7.890078 by the way also fail to converge



# count not converge
testh <- as.data.frame(gelman.diag(jagsfit.mcmchvp)$psrf)
names(testh)[1:2]<-paste(c("pest","upci"))
testh <- filter(testh, upci > 1.05) # 148 obs

testh2 <- as.data.frame(gelman.diag(jagsfit.mcmchvp2)$psrf)
names(testh2)[1:2]<-paste(c("pest","upci"))
testh2 <- filter(testh2, upci > 1.05) # 1348 obs



# 11

# get prior by using mean/variance

# for m1
# tau.v r = 28.43067 lambda = 413.9913
# tau.w r = 67.47498 lambda = 2269.671

# for m2
# tau.v r = 94.78448 lambda = 4531.329
# tau.w r = 11.04833 lambda = 8.330585 

jags.mod.hvp <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  for (i in 2 : 1461) {
    Hounslow[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(Y[i-1], tau.w)
  }
  #  priors
  tau.w ~ dgamma(67.47498,2269.671)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(28.43067,413.9913)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Hounslow)^2)/1461)^0.5
}

jags.inits1 <- c(tau.w = 0.025, tau.v = 0.09, Hounslow = inb1, 
                 Y = c(runif(1461,15,25)))
jags.inits2 <-c(tau.w = 0.035, tau.v = 0.05, Hounslow = inb2, 
                Y = c(runif(1461,15,25)))
jags.inits <- list(jags.inits1,jags.inits2)
jags.param <- c("tau.w","tau.v","Y","rmspr")




jags.mod.fit.hvp <- jags(data = jags.data, inits = jags.inits,
                         parameters.to.save = jags.param, n.chains = 2,
                         n.burnin = 4000, n.iter = 8000, n.thin = 1,
                         model.file = jags.mod.hvp)


jagsfit.mcmchvp <- as.mcmc(jags.mod.fit.hvp)
# table 7
MCMCsummary(jagsfit.mcmchvp, ISB = F, 
            params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                                                 "Y[40]","Y[596]","Y[1286]","Y[701]"))
# figure 12
MCMCtrace(jags.mod.fit.hvp, params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                                       "Y[40]","Y[596]","Y[1286]","Y[701]"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)






jags.mod.hvp2 <- function(){
  Y[1] ~ dnorm(0, 1.0E-3)
  Y[2] ~ dnorm(0, 1.0E-3)
  for (i in 3 : 1461) {
    Hounslow[i] ~ dnorm(Y[i],tau.v)
    Y[i] ~ dnorm(2*Y[i-1]-Y[i-2], tau.w)
  }
  #  priors
  tau.w ~ dgamma(11.04833,8.330585)
  sigma.w2 <- 1/tau.w
  tau.v ~ dgamma(94.78448,4531.329)
  sigma.v2 <- 1/tau.v
  
  rmspr <- (sum((Y-Hounslow)^2)/1461)^0.5
}

jags.inits1 <- c(tau.w = 0.75, tau.v = 0.0245, Hounslow = inb1, 
                 Y = c(runif(1461,15,25)))
jags.inits2 <-c(tau.w = 1.75, tau.v = 0.0185, Hounslow = inb2, 
                Y = c(runif(1461,15,25)))
jags.inits <- list(jags.inits1,jags.inits2)
jags.param <- c("tau.w","tau.v","Y","rmspr")

jags.mod.fit.hvp2 <- jags(data = jags.data, inits = jags.inits,
                          parameters.to.save = jags.param, n.chains = 2,
                          n.burnin = 4000, n.iter = 8000, n.thin = 1,
                          model.file = jags.mod.hvp2)

jagsfit.mcmchvp2 <- as.mcmc(jags.mod.fit.hvp2)
# table 8
MCMCsummary(jagsfit.mcmchvp2, ISB = F, 
            params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                       "Y[40]","Y[596]","Y[1286]","Y[701]"))
# figure 13
MCMCtrace(jags.mod.fit.hvp2, params = c("rmspr","deviance","tau.v","tau.w","Y[13]","Y[1054]",
                                       "Y[40]","Y[596]","Y[1286]","Y[701]"), 
          type = "trace",ind = TRUE, pdf = FALSE, ISB = F)




# count not converge
testh <- as.data.frame(gelman.diag(jagsfit.mcmchvp)$psrf)
names(testh)[1:2]<-paste(c("pest","upci"))
testh <- filter(testh, upci > 1.05) # 147 obs

testh2 <- as.data.frame(gelman.diag(jagsfit.mcmchvp2)$psrf)
names(testh2)[1:2]<-paste(c("pest","upci"))
testh2 <- filter(testh2, upci > 1.05) # 1380 obs































