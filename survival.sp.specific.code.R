library(rstan)
library(rethinking)
# species specific survival

setwd("~/Desktop/Biotic.Impacts/Data")
survival.Dec.Jan=read.csv("survival.Dec.Jan.csv", header=T)

mod <-map2stan(
  alist(
    # likelihood
    survival ~ dbinom(1,p),
    
    #linear models
    logit(p) <- a + a_plot[plot_id] + a_spj[sp_id]
    + a_station[station_id]+ b1*(log_ini_size) + b2*(log_ConNeigh) + b3*(log_HetNeigh)
    + b4*(Treatment) + b5*(Elevation) + b6j[sp_id]*(log_ConNeigh)*(Treatment) + b7j[sp_id]*(log_HetNeigh)*(Treatment),
    
    a ~ dnorm(0,100),
    a_plot[plot_id] ~ dnorm(z, sigma_plot),
    z ~ dnorm(0,1),
    
    a_station[station_id] ~ dnorm(z1, sigma_station),
    z1 ~ dnorm(0,1),
    
    c(a_spj, b6j, b7j)[sp_id] ~ dmvnorm2(c(a_sp, b6, b7), sigma_sp, Rho),
    
    sigma_plot ~ dcauchy(0,2),
    sigma_station ~ dcauchy(0,2),
    sigma_sp ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2),
    
    c(a_sp,b1,b2,b3,b4,b5,b6,b7) ~ dnorm(0, 10)
  ),  
  
  data=survival.Dec.Jan, warmup=4000, iter=40000, chains=4, cores=4)

saveRDS(mod, "survival.Dec.Jan.sp.RData")
precis(mod)
output=as.data.frame(precis(mod,depth=3, prob=0.95))
write.csv(output, file="survival.Dec.Jan.sp.output.csv")

