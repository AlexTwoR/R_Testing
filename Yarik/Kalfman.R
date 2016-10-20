data<- c( 0.0012, 0.0042, 0.0008, 
          0.0042, 0.0165, 0.0111, 
          0.0008, 0.0111, 0.1010,
          0.0045, 0.0362, 0.0108, 
          0.0042, 0.0165, 0.0111, 
          0.0008, 0.0011, 0.1010,
          
          0.0011, 0.0043, 0.0008, 
          0.0043, 0.0164, 0.0112, 
          0.0008, 0.0112, 0.1011 ) 

data<-as.matrix(data) 
data<-matrix(data, nrow=3) 
summary(data) 

a0<-data[,1:3] 
a1<-data[,4:6] 
a2<-data[,7:9]

at<-t(a0)



library(mvtnorm)

dat <-rmvnorm(1000, sigma = a0)

persp(dat)
pairs(dat)






library(FKF)




y <- Nile
y[c(3:4, 10)] <- NA  # NA values can be handled

plot(y,type="l")

## Set constant parameters:
dt <- ct <- matrix(0) 
Zt <- Tt <- matrix(1)
a0 <- y[1]            # Estimation of the first year flow 
P0 <- matrix(100)     # Variance of 'a0'


## Estimate parameters:
fit.fkf <- optim(c(HHt = var(y, na.rm = TRUE) * .5,
                   GGt = var(y, na.rm = TRUE) * .5),
                 fn = function(par, ...)-fkf(HHt = matrix(par[1]), 
                        GGt = matrix(par[2]), ...)$logLik,
                 yt = rbind(y), a0 = a0, P0 = P0, dt = dt, ct = ct,
                 Zt = Zt, Tt = Tt, check.input = FALSE)

## Filter Nile data with estimated parameters:
fkf.obj <- ?fkf(a0, P0, dt, ct, Tt, Zt, HHt = matrix(fit.fkf$par[1]),
               GGt = matrix(fit.fkf$par[2]), yt = rbind(y))

a0
fkf.obj<-fkf(c(y[1]), P0, dt, ct, Tt, Zt, matrix(1), matrix(1123), matrix(1411),yt = rbind(dat))

## Compare with the stats' structural time series implementation:
fit.stats <- StructTS(y, type = "level")

fit.fkf$par
fit.stats$coef

## Plot the flow data together with fitted local levels:
plot(y, main = "Nile flow")
lines(fitted(fit.stats), col = "green")
plot(ts(fkf.obj$att[1, ], start = start(y), frequency = frequency(y)), col = "blue")
legend("top", c("Nile flow data", "Local level (StructTS)", "Local level (fkf)"),
       col = c("black", "green", "blue"), lty = 1)