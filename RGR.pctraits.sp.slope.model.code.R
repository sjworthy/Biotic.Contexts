library(rstan)
library(rethinking)
setwd("~/Desktop/Biotic.Impacts/Data")
rgr.pca.model=read.csv("rgr.pca.model.csv", header=T)

mod_pca.sp <-map2stan(
  alist(
    # likelihood
    log_RGR_plus_1 ~ dnorm(mu, sigma),
    
    #linear models
    mu <- a + a_plot[plot_id] + a_spj[sp_id] + a_station[Station_id] + b1*(log_init_height) + b2*(pc_1) + b3*(pc_2)
    + b4*(pc_3) + b5*(log_ConNeigh) + b6*(log_HetNeigh)
    + b7*(Treatment_2) + b8*(Elevation)
    + b9j[sp_id]*(pc_1)*(Treatment_2) + b10j[sp_id]*(pc_2)*(Treatment_2) + b11j[sp_id]*(pc_3)*(Treatment_2)
    + b12j[sp_id]*(pc_1)*(log_ConNeigh) + b13j[sp_id]*(pc_2)*(log_ConNeigh) + b14j[sp_id]*(pc_3)*(log_ConNeigh)
    + b15j[sp_id]*(pc_1)*(log_HetNeigh) + b16j[sp_id]*(pc_2)*(log_HetNeigh) + b17j[sp_id]*(pc_3)*(log_HetNeigh)
    + b18j[sp_id]*(pc_1)*(log_ConNeigh)*(Treatment_2) + b19j[sp_id]*(pc_2)*(log_ConNeigh)*(Treatment_2) 
    + b20j[sp_id]*(pc_3)*(log_ConNeigh)*(Treatment_2)
    + b21j[sp_id]*(pc_1)*(log_HetNeigh)*(Treatment_2) + b22j[sp_id]*(pc_2)*(log_HetNeigh)*(Treatment_2)
    + b23j[sp_id]*(pc_3)*(log_HetNeigh)*(Treatment_2) + b24*(log_ConNeigh)*(Treatment_2) + b25*(log_HetNeigh)*(Treatment_2), 
    
    a ~ dnorm(0,100),
    a_plot[plot_id] ~ dnorm(z, sigma_plot),
    z ~ dnorm(0,1),
    a_station[Station_id] ~ dnorm(z2, sigma_station),
    z2 ~ dnorm(0,1),
    
    c(a_sp, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11,  b12,  b13,  b14,  b15,  b16,  b17,  b18,  b19,  b20,
      b21,  b22,  b23, b24, b25) ~ dnorm(0,10),
    
    c(a_spj, b9j, b10j, b11j,  b12j,  b13j,  b14j,  b15j,  b16j,  b17j,  b18j,  b19j,  b20j,
      b21j,  b22j,  b23j)[sp_id] ~ dmvnorm2(c(a_sp, b9, b10, b11,  b12,  b13,  b14,  b15,  b16,  b17,  b18,  b19,  b20,
                                             b21,  b22,  b23), sigma_sp, Rho),
    
    sigma_plot ~ dcauchy(0,2),
    sigma_sp ~ dcauchy(0,2),
    sigma_station ~ dcauchy(0,2),
    sigma ~ dcauchy(0,2),
    Rho ~ dlkjcorr(2)
  ),
  
  data=rgr.pca.model, warmup=7000, iter=70000, chains=4, cores=4)

saveRDS(mod_pca.sp, "RGR.pca.mod.sp.RData")
all.output=precis(mod_pca.sp, depth=3, prob=0.95) # gives you the mean, sd, CI, n_eff, Rhat
all.output.2=as.data.frame(all.output) # extracts and converts to data frame for output
write.csv(all.output.2, "RGR.mod_pca.sp.df.output.csv")




