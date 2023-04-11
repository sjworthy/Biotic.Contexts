library(vegan)
library(hillR) # not good for hill numbers
install.packages("mFD")
library(diverse)
library(sjPlot)
library(ggplot2)
library(rstanarm)
library(cowplot)

setwd("~/Desktop/Biotic.Impacts/Data")

# Read in dataframes that have all data
# Different dataframes used for trait data so that it has complete cases

G.300.RGR=read.csv("300.Green.RGR.csv", header=T)
R.300.RGR=read.csv("300.Red.RGR.csv", header=T)
G.400.RGR=read.csv("400.Green.RGR.csv", header=T)
R.400.RGR=read.csv("400.Red.RGR.csv", header=T)
G.500.RGR=read.csv("500.Green.RGR.csv", header=T)
R.500.RGR=read.csv("500.Red.RGR.csv", header=T)

#### Calculate RGR (log(h_final-log(h_initial)))/number of months ####
G.300.RGR$log.final.height=log(G.300.RGR$Final.Height)
G.300.RGR$log.init.height=log(G.300.RGR$Init.Height)
G.300.RGR$RGR=(G.300.RGR$log.final.height-G.300.RGR$log.init.height)/G.300.RGR$months.in.census

G.400.RGR$log.final.height=log(G.400.RGR$Final.Height)
G.400.RGR$log.init.height=log(G.400.RGR$Init.Height)
G.400.RGR$RGR=(G.400.RGR$log.final.height-G.400.RGR$log.init.height)/G.400.RGR$months.in.census

G.500.RGR$log.final.height=log(G.500.RGR$Final.Height)
G.500.RGR$log.init.height=log(G.500.RGR$Init.Height)
G.500.RGR$RGR=(G.500.RGR$log.final.height-G.500.RGR$log.init.height)/G.500.RGR$months.in.census

R.300.RGR$log.final.height=log(R.300.RGR$Final.Height)
R.300.RGR$log.init.height=log(R.300.RGR$Init.Height)
R.300.RGR$RGR=(R.300.RGR$log.final.height-R.300.RGR$log.init.height)/R.300.RGR$months.in.census

R.400.RGR$log.final.height=log(R.400.RGR$Final.Height)
R.400.RGR$log.init.height=log(R.400.RGR$Init.Height)
R.400.RGR$RGR=(R.400.RGR$log.final.height-R.400.RGR$log.init.height)/R.400.RGR$months.in.census

R.500.RGR$log.final.height=log(R.500.RGR$Final.Height)
R.500.RGR$log.init.height=log(R.500.RGR$Init.Height)
R.500.RGR$RGR=(R.500.RGR$log.final.height-R.500.RGR$log.init.height)/R.500.RGR$months.in.census

write.csv(G.300.RGR, file="300.Green.RGR.csv")
write.csv(G.400.RGR, file="400.Green.RGR.csv")
write.csv(G.500.RGR, file="500.Green.RGR.csv")
write.csv(R.300.RGR, file="300.Red.RGR.csv")
write.csv(R.400.RGR, file="400.Red.RGR.csv")
write.csv(R.500.RGR, file="500.Red.RGR.csv")

#### Creating species-specific RGR file ####
# abundance of each species in each elevation/treatment
# average RGR of each species in each elevation/treatment

table(G.300.RGR$Sp.Code)
table(G.400.RGR$Sp.Code)
table(G.500.RGR$Sp.Code)
table(R.300.RGR$Sp.Code)
table(R.400.RGR$Sp.Code)
table(R.500.RGR$Sp.Code)

#### mean RGR per species per treatment ####

g300.df=data.frame(G.300.RGR$Sp.Code, G.300.RGR$RGR)
aggregate(.~G.300.RGR.Sp.Code, data=g300.df, mean)

r300.df=data.frame(R.300.RGR$Sp.Code, R.300.RGR$RGR)
aggregate(.~R.300.RGR.Sp.Code, data=r300.df, mean)

g400.df=data.frame(G.400.RGR$Sp.Code, G.400.RGR$RGR)
aggregate(.~G.400.RGR.Sp.Code, data=g400.df, mean)

r400.df=data.frame(R.400.RGR$Sp.Code, R.400.RGR$RGR)
aggregate(.~R.400.RGR.Sp.Code, data=r400.df, mean)

g500.df=data.frame(G.500.RGR$Sp.Code, G.500.RGR$RGR)
aggregate(.~G.500.RGR.Sp.Code, data=g500.df, mean)

r500.df=data.frame(R.500.RGR$Sp.Code, R.500.RGR$RGR)
aggregate(.~R.500.RGR.Sp.Code, data=r500.df, mean)

# Species-specific RGR created outside of R
spp.sp.rgr=read.csv("sp.specific.RGR.csv", header=T, row.names=1)
spp.sp.rgr=spp.sp.rgr[1:65,]

#### Making community data matrices ####

abund.cdm = matrix(data=NA, nrow = 6, ncol = 65)
row.names(abund.cdm) = c("p.300.G", "p.300.R", "p.400.G", "p.400.R", "p.500.G", "p.500.R")
colnames(abund.cdm) = row.names(spp.sp.rgr)

abund.cdm[1,]=spp.sp.rgr$X300.G.Abund
abund.cdm[2,]=spp.sp.rgr$X300.R.Abund
abund.cdm[3,]=spp.sp.rgr$X400.G.Abund
abund.cdm[4,]=spp.sp.rgr$X400.R.Abund
abund.cdm[5,]=spp.sp.rgr$X500.G.Abund
abund.cdm[6,]=spp.sp.rgr$X500.R.Abund

abund.cdm[is.na(abund.cdm)]=0

abund.cdm=write.csv(abund.cdm, file="abund.cdm.csv")

all.abund.cdm=read.csv("all.abund.cdm.csv", header=T, row.names = 1)

#### Information in Table 1 ####

all.abund.cdm=read.csv("all.abund.cdm.csv", header=T, row.names = 1)
abund.cdm=read.csv("abund.cdm.csv", header = T, row.names = 1)

diverse.output=diversity(as.matrix(abund.cdm), type = "all", q=1) # same as HillR
diverse.output.all=diversity(as.matrix(all.abund.cdm), type = "all", q=1)

diverse.output.invsimp=diversity(as.matrix(abund.cdm), type = "all", q=2) # same as HillR
diverse.output.all.invsimp=diversity(as.matrix(all.abund.cdm), type = "all", q=2)

# Calculate species richness
richness=specnumber(abund.cdm)
all.richness=specnumber(all.abund.cdm)

# Calculating Shannon's entropy (Hill number = 1)
shan.entro=hill_taxa(abund.cdm, q=1)
shan.entro.all=hill_taxa(all.abund.cdm, q=1)

# Calculating Inverse Simpson (Hill number = 2)
invsimp=hill_taxa(abund.cdm, q=2)
invsimp.all=hill_taxa(all.abund.cdm, q=2)

# Evenness, not correlated with Shannon's entropy
even=invsimp/richness
all.even=invsimp.all/all.richness

#### Determining number of conspecific and heterospecific neighbors of each individual ####
# Repeat code below for each Station 1-25

# read in dataframes that have complete cases of traits
G.300.RGR.traits=read.csv("300.Green.RGR.traits.csv", header=T)
R.300.RGR.traits=read.csv("300.Red.RGR.traits.csv", header=T)
G.400.RGR.traits=read.csv("400.Green.RGR.traits.csv", header=T)
R.400.RGR.traits=read.csv("400.Red.RGR.traits.csv", header=T)
G.500.RGR.traits=read.csv("500.Green.RGR.traits.csv", header=T)
R.500.RGR.traits=read.csv("500.Red.RGR.traits.csv", header=T)

table(G.300.RGR.traits[G.300.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(G.300.RGR.traits[G.300.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

table(R.300.RGR.traits[R.300.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(R.300.RGR.traits[R.300.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

# No plot 4
table(G.400.RGR.traits[G.400.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(G.400.RGR.traits[G.400.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

# No plot 4, plot 16
table(R.400.RGR.traits[R.400.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(R.400.RGR.traits[R.400.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

table(G.500.RGR.traits[G.500.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(G.500.RGR.traits[G.500.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

# No plot 11 and 13
table(R.500.RGR.traits[R.500.RGR.traits$Station==25,]$Sp.Code)
# determine conspecific neighbors, 1 minus total for the species
sum(table(R.500.RGR.traits[R.500.RGR.traits$Station==25,]$Sp.Code))
# determine heterospecific neighbors, total minus number of the species from above

#### Determining number of conspecific and heterospecific neighbors of each individual for census survival models ####

dec_jan=read.csv("survival.Dec.Jan.csv", header=T)
dec_jan_t0=subset(dec_jan, dec_jan$Treatment==0)
output=dec_jan_t0[dec_jan_t0$plot_id==123,]
table(output$species)
# determine conspecific neighbors, 1 minus total for the species
sum(table(output$species))
# determine heterospecific neighbors, total minus number of the species from above
# total from both columns should always be 1 less than total to account for self

dec_jan_t1=subset(dec_jan, dec_jan$Treatment==1)
output=dec_jan_t1[dec_jan_t1$plot_id==146,]
table(output$species)
# determine conspecific neighbors, 1 minus total for the species
sum(table(output$species))
# determine heterospecific neighbors, total minus number of the species from above
# total from both columns should always be 1 less than total to account for self


#### Evaluating model effects ####
# Read in data used to fit the model
setwd("/Users/samanthaworthy/Desktop/Biotic.Impacts/Data")
rgr.indiv.trait.model=read.csv("rgr.indiv.trait.model.csv", row.names = 1)
rgr.pca.model=read.csv("rgr.pca.model.csv", row.names = 1)
setwd("/Users/samanthaworthy/Desktop/Biotic.Impacts/Data/Results")
# read in the parameter output file from the fitted model
rgr.sp.traits.parm=read.csv("RGR.mod.sp.traits.df.output.csv")
rgr.sp.pca.parm=read.csv("RGR.mod_pca.sp.df.output.csv")

### Make sure of significant variables ####

sig.neg.traits=rgr.sp.traits.parm[rgr.sp.traits.parm$X2.50. < 0 & rgr.sp.traits.parm$X97.50. < 0,]
sig.pos.traits=rgr.sp.traits.parm[rgr.sp.traits.parm$X2.50. > 0 & rgr.sp.traits.parm$X97.50. > 0,]

sig.neg.pca=rgr.sp.pca.parm[rgr.sp.pca.parm$X2.50. < 0 & rgr.sp.pca.parm$X97.50. < 0,]
sig.pos.pca=rgr.sp.pca.parm[rgr.sp.pca.parm$X2.50. > 0 & rgr.sp.pca.parm$X97.50. > 0,]


#### Three-Way interactions ####
# Interaction: PC1*HetNeigh*Treatment

# run subset of model with rstanarms so easier to plot. Coefficients are the same.

rgr.pca.model$Treatment_factor=as.factor(rgr.pca.model$Treatment_factor)
rgr.indiv.trait.model$Treatment_factor=as.factor(rgr.indiv.trait.model$Treatment_factor)

pc1.hetneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) + (1|sp_id) + (1|Station_id) +
      pc_1*log_HetNeigh*Treatment_factor,
    data = rgr.pca.model,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
pc1.hetneigh.trt.plot=plot_model(pc1.hetneigh.trt.mod, type="int", 
                  axis.title = c("PC1","Log RGR"), 
                  legend.title = "Log Heterospecifc\nNeighbor Density", title = "")

pc1.hetneigh.trt.plot[[4]]

ggsave("Figure2.pdf", height=10, width=12)

#### Species-Specific Models

# Interaction: PC1*ConNeigh(-) and LA*ConNeigh(-)
alcflo=rgr.pca.model[rgr.pca.model$sp_id==1,]
hibros=rgr.pca.model[rgr.pca.model$sp_id==22,]
schmor=rgr.pca.model[rgr.pca.model$sp_id==51,]
spomom=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==56,]

alclfo.pc1.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_ConNeigh,
    data = alcflo,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
alclfo.pc1.conneigh.mod.plot=plot_model(alclfo.pc1.conneigh.mod, type="int", 
           axis.title = c("PC1","Log RGR"), 
           legend.title = "Log Conspecific\nNeighbor Density", title = "A. floribunda")

ggsave("alcflo.pc1.conneigh.pdf", height=10, width=12)

hibros.pc1.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_ConNeigh,
    data = hibros,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
hibros.pc1.conneigh.mod.plot=plot_model(hibros.pc1.conneigh.mod, type="int", 
                        axis.title = c("PC1","Log RGR"), 
                        legend.title = "Log Conspecific\nNeighbor Density", title = "H. rosa-sinensis")
ggsave("hibros.pc1.conneigh.pdf", height=10, width=12)

schmor.pc1.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 +
      pc_1*log_ConNeigh,
    data = schmor, prior = R2(location=0.5),
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
schmor.pc1.conneigh.mod.plot=plot_model(schmor.pc1.conneigh.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Conspecific\nNeighbor Density", title = "S. morototoni")
ggsave("schmor.pc1.conneigh.pdf", height=10, width=12)

spomom.LA.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*log_ConNeigh,
    data = spomom,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
spomom.LA.conneigh.mod.plot=plot_model(spomom.LA.conneigh.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Log Conspecific\nNeighbor Density", title = "S. mombin")
ggsave("spmom.la.conneigh.pdf", height=10, width=12)


# Interaction: SMF*ConNeigh(-)

alcflo=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==1,]
hibros=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==22,]
schmor=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==51,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
drygla=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==14,]

alclfo.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = alcflo,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
alclfo.smf.conneigh.mod.plot=plot_model(alclfo.smf.conneigh.mod, type="int", 
                        axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                        legend.title = "Log Conspecific\nNeighbor Density", title = "A. floribunda")
ggsave("alclfo.smf.conneigh.pdf", height=10, width=12)

hibros.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = hibros,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
hibros.smf.conneigh.mod.plot=plot_model(hibros.smf.conneigh.mod, type="int", 
                        axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                        legend.title = "Log Conspecific\nNeighbor Density", title = "H. rosa-sinensis")
ggsave("hibros.smf.conneigh.pdf", height=10, width=12)

schmor.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = schmor,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
schmor.smf.conneigh.mod.plot=plot_model(schmor.smf.conneigh.mod, type="int", 
                       axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                       legend.title = "Log Conspecific\nNeighbor Density", title = "S. morototoni")
ggsave("schmor.smf.conneigh.pdf", height=10, width=12)

guagui.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = guagui,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
guagui.smf.conneigh.mod.plot=plot_model(guagui.smf.conneigh.mod, type="int", 
                       axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                       legend.title = "Log Conspecific\nNeighbor Density", title = "G. guidonia")
ggsave("guagui.smf.conneigh.pdf", height=10, width=12)


drygla.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = drygla,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
drygla.smf.conneigh.mod.plot=plot_model(drygla.smf.conneigh.mod, type="int", 
                       axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                       legend.title = "Log Conspecific\nNeighbor Density", title = "D. glauca")
ggsave("drygla.smf.conneigh.pdf", height=10, width=12)

# Interaction: SMF*ConNeigh(+)
micrac=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==30,]

# model won't compute because ConNeigh value is the same for all individuals despite being in different plots

micrac.smf.conneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh,
    data = micrac,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

micrac.plot=plot_model(micrac.smf.conneigh.mod, type="int", 
                       axis.title = c("log SMF","Log RGR"), 
                       legend.title = "log ConNeigh", title = "")

# Interaction: PC1*HetNeigh(+) and LA*HetNeigh(+)
alclat=rgr.pca.model[rgr.pca.model$sp_id==2,]
alclat.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==2,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
psyber=rgr.pca.model[rgr.pca.model$sp_id==43,]
swimac=rgr.pca.model[rgr.pca.model$sp_id==57,]
swimac.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==57,]


alclat.pc1.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
     pc_1*log_HetNeigh,
    data = alclat,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
alclat.pc1.hetneigh.mod.plot=plot_model(alclat.pc1.hetneigh.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "A. latifolia")
ggsave("alclat.pc1.hetneigh.pdf", height=10, width=12)

alclat.LA.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*log_HetNeigh,
    data = alclat.2,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
alclat.LA.hetneigh.mod.plot=plot_model(alclat.LA.hetneigh.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "A. latifolia")
ggsave("alclat.LA.hetneigh.pdf", height=10, width=12)

guagui.LA.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*log_HetNeigh,
    data = guagui,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
guagui.LA.hetneigh.mod.plot=plot_model(guagui.LA.hetneigh.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "G. guidonia")
ggsave("guagui.LA.hetneigh.pdf", height=10, width=12)

psyber.pc1.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
     pc_1*log_HetNeigh,
    data = psyber,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
psyber.pc1.hetneigh.mod.plot=plot_model(psyber.pc1.hetneigh.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "P. berteroana")
ggsave("psyber.PC1.hetneigh.pdf", height=10, width=12)

swimac.pc1.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_HetNeigh,
    data = swimac,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
swimac.pc1.hetneigh.mod.plot=plot_model(swimac.pc1.hetneigh.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "S. macrophylla")
ggsave("swimac.PC1.hetneigh.pdf", height=10, width=12)

swimac.LA.hetneigh.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*log_HetNeigh,
    data = swimac.2,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
swimac.LA.hetneigh.mod.plot=plot_model(swimac.LA.hetneigh.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Heterospecific\nNeighbor Density", title = "S. macrophylla")
ggsave("swimac.LA.hetneigh.pdf", height=10, width=12)

# Interaction: PC1 x Treatment(+) and LA x Treatment(-)
cecsch=rgr.pca.model[rgr.pca.model$sp_id==6,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
swimac=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==57,]

cecsch.PC1.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*Treatment_factor,
    data = cecsch,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
cecsch.PC1.trt.mod.plot=plot_model(cecsch.PC1.trt.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Plots", title = "C. schreberiana")
ggsave("cecsch.PC1.treatment.pdf", height=10, width=12)

guagui.LA.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*Treatment_factor,
    data = guagui,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
guagui.LA.trt.mod.plot=plot_model(guagui.LA.trt.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Plots", title = "G. guidonia")
ggsave("guagui.LA.treatment.pdf", height=10, width=12)

swimac.LA.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*Treatment_factor,
    data = swimac,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
swimac.LA.trt.mod.plot=plot_model(swimac.LA.trt.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Plots", title = "S. macrophylla")
ggsave("swimac.LA.treatment.pdf", height=10, width=12)

# Interaction: PC1/LA*hetneigh*Treatment(-)
cecsch=rgr.pca.model[rgr.pca.model$sp_id==6,]
cecsch.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==6,]

cecsch.PC1.hetneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_HetNeigh*Treatment_factor,
    data = cecsch,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
cecsch.PC1.hetneigh.trt.mod.plot=plot_model(cecsch.PC1.hetneigh.trt.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Heterospecifc\nNeighbor Density", title = "C. schreberiana")

ggsave("cecsch.pc1.hetneigh.treatment.pdf", height=10, width=12)

cecsch.LA.hetneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_LA*log_HetNeigh*Treatment_factor,
    data = cecsch.2,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
cecsch.LA.hetneigh.trt.mod.plot=plot_model(cecsch.LA.hetneigh.trt.mod, type="int", 
                       axis.title = c("Log Leaf Area (LA)","Log RGR"), 
                       legend.title = "Log Heterospecifc\nNeighbor Density", title = "C. schreberiana")
ggsave("cecsch.LA.hetneigh.treatment.pdf", height=10, width=12)


# Interaction PC1*ConNeigh*Treatment(+)
psyber=rgr.pca.model[rgr.pca.model$sp_id==43,]
hibros=rgr.pca.model[rgr.pca.model$sp_id==22,]

psyber.PC1.conneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_ConNeigh*Treatment_factor,
    data = psyber,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
psyber.PC1.conneigh.trt.mod.plot=plot_model(psyber.PC1.conneigh.trt.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Conspecifc\nNeighbor Density", title = "P. berteroana")
ggsave("psyber.PC1.conneigh.treatment.pdf", height=10, width=12)

hibros.PC1.conneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      pc_1*log_ConNeigh*Treatment_factor,
    data = hibros,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
hibros.PC1.conneigh.trt.mod.plot=plot_model(hibros.PC1.conneigh.trt.mod, type="int", 
                       axis.title = c("PC1","Log RGR"), 
                       legend.title = "Log Conspecifc\nNeighbor Density", title = "H. rosa-sinensis")
ggsave("hibros.PC1.conneigh.treatment.pdf", height=10, width=12)

# Interaction: SMF*Conneight*Trt (+) and SRL*ConNeigh*Trt(-)
psyber=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==43,]

psyber.SMF.conneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SMF*log_ConNeigh*Treatment_factor,
    data = psyber,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
psyber.SMF.conneigh.trt.mod.plot=plot_model(psyber.SMF.conneigh.trt.mod, type="int", 
                       axis.title = c("Log Stem Mass Fraction (SMF)","Log RGR"), 
                       legend.title = "Log Conspecifc\nNeighbor Density", title = "P. berteroana")
ggsave("psyber.SMF.conneigh.treatment.pdf", height=10, width=12)

psyber.SRL.conneigh.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SRL*log_ConNeigh*Treatment_factor,
    data = psyber,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
psyber.SRL.conneigh.trt.mod.plot=plot_model(psyber.SRL.conneigh.trt.mod, type="int", 
                       axis.title = c("Log Specific Root Length (SRL)","Log RGR"), 
                       legend.title = "Log Conspecifc\nNeighbor Density", title = "P. berteroana")

ggsave("psyber.SRL.conneigh.treatment.pdf", height=10, width=12)

# Interaction: SRL x Treatment(-)
cecsch=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==6,]

cecsch.SRL.trt.mod <- 
  rstanarm::stan_lmer(
    log_RGR_plus_1 ~ 1 + (1 | plot_id) +(1|Station_id)+
      log_SRL*Treatment_factor,
    data = cecsch,
    iter = 4000, warmup = 1000, chains = 4, cores = 4)

set_theme(base = theme_classic(base_size = 15), axis.textcolor = "black",legend.title.size = .8,
          axis.title.color = "black", legend.title.color = "black")
cecsch.SRL.trt.mod.plot=plot_model(cecsch.SRL.trt.mod, type="int", 
                       axis.title = c("Log Specific Root Length (SRL)","Log RGR"), 
                       legend.title = "Plots", title = "C. schreberiana")
ggsave("cecsch.SRL.treatment.pdf", height=10, width=12)

#### Correlate plot-level RGR with plot-level SR for each SR census ####

cor.df=read.csv("RGR.SR.correlation.csv", header=T, row.names=1)
test=cor.df[c(1:28,30:40,42:62,64:75),]

# Control Plots
cor.test(cor.df[,1], cor.df[,2]) # r = 0.19 p = 0.12
cor.test(cor.df[,1], cor.df[,3]) # r = -0.17 p = 0.14
cor.test(cor.df[,1], cor.df[,4])# r = -0.32 p = 0.006
cor.test(cor.df[,1], cor.df[,5])# r = -0.04 p = 0.72
cor.test(cor.df[,1], cor.df[,6])# r = -0.07 p = 0.57
cor.test(cor.df[,1], cor.df[,7])# r = -0.02 p = 0.84
cor.test(cor.df[,1], cor.df[,8])# r = -0.24 p = 0.05
cor.test(cor.df[,1], cor.df[,9])# r = 0.13 p = 0.27
cor.test(cor.df[,1], cor.df[,10])# r = 0.03 p = 0.83
cor.test(cor.df[,1], cor.df[,11])# r = -0.42 p = 0.0002
cor.test(cor.df[,1], cor.df[,12])# r = -0.02 p = 0.89
cor.test(cor.df[,1], cor.df[,13])# r = 0.01 p = 0.92
cor.test(cor.df[,1], cor.df[,14])# r = -0.32 p = 0.007

# with logged values
cor.test(log(cor.df[,1]), log(cor.df[,2])) # r = 0.15 p = 0.22
cor.test(log(cor.df[,1]+1), log(cor.df[,3]+1)) # r = -0.18 p = 0.13
cor.test(log(cor.df[,1]), log(cor.df[,4]))# r = -0.26 p = 0.03
cor.test(log(cor.df[,1]), log(cor.df[,5]))# r = -0.06 p = 0.62
cor.test(log(cor.df[,1]), log(cor.df[,6]))# r = -0.08 p = 0.51
cor.test(log(cor.df[,1]), log(cor.df[,7]))# r = -0.04 p = 0.76
cor.test(log(cor.df[,1]), log(cor.df[,8]))# r = -0.21 p = 0.08
cor.test(log(cor.df[,1]), log(cor.df[,9]))# r = 0.19 p = 0.11
cor.test(log(cor.df[,1]), log(cor.df[,10]))# r = 0.02 p = 0.87
cor.test(log(cor.df[,1]), log(cor.df[,11]))# r = -0.36 p = 0.002
cor.test(log(cor.df[,1]), log(cor.df[,12]))# r = -0.00002 p = 0.99
cor.test(log(cor.df[,1]), log(cor.df[,13]))# r = -0.04 p = 0.76
cor.test(log(cor.df[,1]), log(cor.df[,14]))# r = -0.30 p = 0.01


# Treatment Plots
cor.test(cor.df[,15], cor.df[,16]) # r = -0.36 p = 0.002
cor.test(cor.df[,15], cor.df[,17]) # r = -0.32 p = 0.006
cor.test(cor.df[,15], cor.df[,18])# r = -0.27 p = 0.02
cor.test(cor.df[,15], cor.df[,19])# r = -0.17 p = 0.17
cor.test(cor.df[,15], cor.df[,20])# r = 0.03 p = 0.78
cor.test(cor.df[,15], cor.df[,21])# r = -0.01 p = 0.93
cor.test(cor.df[,15], cor.df[,22])# r = -0.19 p = 0.12
cor.test(cor.df[,15], cor.df[,23])# r = -0.06 p = 0.61
cor.test(cor.df[,15], cor.df[,24])# r = 0.07 p = 0.57
cor.test(cor.df[,15], cor.df[,25])# r = 0.23 p = 0.05
cor.test(cor.df[,15], cor.df[,26])# r = 0.12 p = 0.31
cor.test(cor.df[,15], cor.df[,27])# r = 0.009 p = 0.94
cor.test(cor.df[,15], cor.df[,28])# r = -0.22 p = 0.06

# with logged values
cor.test(log(cor.df[,15]), log(cor.df[,16])) # r = -0.29 p = 0.02
cor.test(log(cor.df[,15]+1), log(cor.df[,17]+1)) # r = -0.32  p = 0.005
cor.test(log(cor.df[,15]), log(cor.df[,18]))# r = -0.25 p = 0.04
cor.test(log(cor.df[,15]), log(cor.df[,19]))# r = -0.18 p = 0.13
cor.test(log(cor.df[,15]), log(cor.df[,20]))# r = -0.0006 p = 0.99
cor.test(log(cor.df[,15+1]), log(cor.df[,21+1]))# r = -0.05 p = 0.69
cor.test(log(cor.df[,15]), log(cor.df[,22]))# r = -0.15 p = 0.21
cor.test(log(cor.df[,15]), log(cor.df[,23]))# r = -0.08 p = 0.48
cor.test(log(cor.df[,15]), log(cor.df[,24]))# r = 0.05 p = 0.64
cor.test(log(cor.df[,15]), log(cor.df[,25]))# r = 0.25 p = 0.03
cor.test(log(cor.df[,15]), log(cor.df[,26]))# r = 0.09 p = 0.48
cor.test(log(cor.df[,15]), log(cor.df[,27]))# r = -0.03 p = 0.80
cor.test(log(cor.df[,15]), log(cor.df[,28]))# r = -0.18 p = 0.14

#### Figures ####

Figure.3 = plot_grid(hibros.pc1.conneigh.mod.plot,spomom.LA.conneigh.mod.plot,hibros.smf.conneigh.mod.plot,
                     schmor.smf.conneigh.mod.plot,alclat.pc1.hetneigh.mod.plot,psyber.pc1.hetneigh.mod.plot,
                     ncol=2, nrow=3, labels = c("A.","B.","C.","D.","E.","F."))

ggsave("Figure3.pdf", height = 10, width = 12)

Figure.4 = plot_grid(cecsch.PC1.trt.mod.plot, cecsch.PC1.hetneigh.trt.mod.plot[[4]],hibros.PC1.conneigh.trt.mod.plot[[4]],
                     psyber.SMF.conneigh.trt.mod.plot[[4]],cecsch.SRL.trt.mod.plot,labels = c("A.","B.","C.","D.","E."),
                     ncol=2, nrow=3)

ggsave("Figure4.pdf", height = 10, width = 12)

Figure.S1 = plot_grid(schmor.pc1.conneigh.mod.plot,alclfo.pc1.conneigh.mod.plot,alclfo.smf.conneigh.mod.plot,
                      guagui.smf.conneigh.mod.plot,alclat.LA.hetneigh.mod.plot,guagui.LA.hetneigh.mod.plot,
                      swimac.LA.hetneigh.mod.plot,labels = c("A.","B.","C.","D.","E.","F.","G."),ncol=2, nrow=4)

ggsave("Figure.S1.pdf", height = 10, width = 12)

Figure.S2 = plot_grid(swimac.LA.trt.mod.plot,guagui.LA.trt.mod.plot,psyber.PC1.conneigh.trt.mod.plot[[4]],
                      psyber.SRL.conneigh.trt.mod.plot[[4]],labels = c("A.","B.","C.","D."),ncol=2, nrow=2)

ggsave("Figure.S2.pdf", height = 10, width = 12)

#### Determining mean growth rates per plot ####

G.300.RGR=read.csv("300.Green.RGR.csv", header=T)
R.300.RGR=read.csv("300.Red.RGR.csv", header=T)
G.400.RGR=read.csv("400.Green.RGR.csv", header=T)
R.400.RGR=read.csv("400.Red.RGR.csv", header=T)
G.500.RGR=read.csv("500.Green.RGR.csv", header=T)
R.500.RGR=read.csv("500.Red.RGR.csv", header=T)

# repeat for each plot
mean((subset(G.300.RGR, G.300.RGR$Station == 1,))$RGR)
mean((subset(R.300.RGR, R.300.RGR$Station == 1,))$RGR)
mean((subset(G.400.RGR, G.400.RGR$Station == 1,))$RGR) # no station 4
mean((subset(R.400.RGR, R.400.RGR$Station == 1,))$RGR) # no station 4, eliminate station 16 because only have Green data
mean((subset(G.500.RGR, G.500.RGR$Station == 1,))$RGR)
mean((subset(R.500.RGR, R.500.RGR$Station == 1,))$RGR) # no station 13


#### T.test for RGR ####

all.rgr=read.csv("All.RGR.csv",header=T)
# plots 400.4, 400.16, 500.13 removed because no individuals survived so RGR is NaN

# standard t.test of all plot means for all elevations
# running t.test with rgr not logged and log()

test.1=all.rgr[c(1:72),c(1:3)] # subset to get columns needed

t.test(log(test.1$Green.all), log(test.1$Red.all)) # logged RGR
# t = -0.31811, df = 140.91, p = 0.7509
# mean Green = -2.400922
# mean Red = -2.371441

t.test(test.1$Green.all, test.1$Red.all)
# t = -0.12687, df = 141.97, p = 0.8992
# mean Green = 0.1055484
# mean Red = 0.1067318

# t.test for each elevation separately
#300
t.test(all.rgr[1:25,2], all.rgr[1:25,3])
# t = -1.4251, df = 42.304, p = 0.1615
# mean Green = 0.06238131
# mean Red = 0.07561301

t.test(log(all.rgr[1:25,2]), log(all.rgr[1:25,3]))
# t = -1.2625, df = 46.208, p = 0.2131
# mean Green = -2.849520
# mean Red = -2.692993

#400
t.test(all.rgr[26:48,2], all.rgr[26:48,3])
# t = 1.2156, df = 42.93, p = 0.2308
# mean Green = 0.11243465
# mean Red = 0.09631597

t.test(log(all.rgr[26:48,2]), log(all.rgr[26:48,3]))
# t = 1.2434, df = 43.889, p = 0.2203
# mean Green = -2.268793
# mean Red = -2.420914

#500
t.test(all.rgr[49:72,2], all.rgr[49:72,3])
# t = -0.31504, df = 45.996, p = 0.7542
# mean Green = 0.1439147
# mean Red = 0.1491290

t.test(log(all.rgr[49:72,2]), log(all.rgr[49:72,3]))
# t = -0.4685, df = 43.229, p = 0.6418
# mean Green = -2.060256
# mean Red = -1.989079

# Plot of RGR values for treatments overall and by elevations

all.rgr.2=all.rgr[,c(2,3,10,11,14,15,18,19)]
all.rgr.3=melt(all.rgr.2)
rgr.boxplot=ggplot(all.rgr.3, aes(y=value, x=variable))+
  geom_boxplot(aes(fill=variable),show.legend = FALSE)+
  geom_point()+
  labs(title="",x="", y = "Mean Plot Relative Growth Rate")+
  scale_x_discrete(labels=c("All:C","All:T","300m:C","300m:T","400m:C","400m:T","500m:C","500m:T"))+
  theme_classic(base_size = 15)+
  scale_fill_manual(values=c("#548F01","#DB4743","#548F01","#DB4743","#548F01","#DB4743","#548F01","#DB4743"))




rgr.boxplot=boxplot(all.rgr[,c(2,3,10,11,14,15,18,19)], ylab="Mean Plot Relative Growth Rate", cex.axis = 1.50,cex.lab=1.5,
        names=c("All:C","All:T","300m:C","300m:T","400m:C","400m:T","500m:C","500m:C"))
rgr.recorded <- recordPlot()

# Plot SR overall and each elevation by treatment
all.SR=read.csv("Elev.SR.csv", header=T, row.names=1)
all.SR.melt=melt(all.SR)

all.SR.melt$value.2=log(all.SR.melt$value)

sr.boxplot=ggplot(all.SR.melt, aes(y=value, x=variable))+
  geom_boxplot(aes(fill=variable),show.legend = FALSE)+
  geom_point()+
  labs(title="",x="", y = "Mean Plot Survival Rate")+
  scale_x_discrete(labels=c("All:C","All:T","300m:C","300m:T","400m:C","400m:T","500m:C","500m:T"))+
  theme_classic(base_size = 15)+
  scale_fill_manual(values=c("#548F01","#DB4743","#548F01","#DB4743","#548F01","#DB4743","#548F01","#DB4743"))

# Figure 1

Figure1=plot_grid(rgr.boxplot,sr.boxplot, labels = c("A.","B."))


# Plot of RGR at the individual level for each elevation and treatment

indiv.rgr=read.csv("All.Indiv.RGR.csv", header=T)
boxplot(indiv.rgr, ylab="Individual Relative Growth Rate")


