library(vegan)
library(hillR)

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

# plot marginal effects of interactions
# Have to make model in lm first
library(sjPlot)
library(ggplot2)

rgr.pca.model$Treatment_factor=as.factor(rgr.pca.model$Treatment_factor)
rgr.indiv.trait.model$Treatment_factor=as.factor(rgr.indiv.trait.model$Treatment_factor)


pc1.hetneigh.trt.mod=lm(log_RGR_plus_1~pc_1*log_HetNeigh*Treatment_factor, rgr.pca.model)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(pc1.hetneigh.trt.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log HetNeigh", title = "")

output[[4]]

#### Species-Specific Models

# Interaction: PC1*ConNeigh(-) and LA*ConNeigh(-)
alcflo=rgr.pca.model[rgr.pca.model$sp_id==1,]
hibros=rgr.pca.model[rgr.pca.model$sp_id==22,]
schmor=rgr.pca.model[rgr.pca.model$sp_id==51,]
spomom=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==56,]

alclfo.pc1.conneigh.mod=lm(log_RGR_plus_1~pc_1*log_ConNeigh, alcflo)
hibros.pc1.conneigh.mod=lm(log_RGR_plus_1~pc_1*log_ConNeigh, hibros)
schmor.pc1.conneigh.mod=lm(log_RGR_plus_1~pc_1*log_ConNeigh, schmor)
spomom.LA.conneigh.mod=lm(log_RGR_plus_1~log_LA*log_ConNeigh, spomom)


set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(alclfo.pc1.conneigh.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

# Interaction: SMF*ConNeigh(-)

alcflo=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==1,]
hibros=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==22,]
schmor=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==51,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
drygla=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==14,]

alclfo.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, alcflo) # -SMF
hibros.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, hibros) # -ConNeigh
schmor.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, schmor) # -ConNeigh
guagui.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, guagui) # SMF
drygla.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, drygla) # -SMF and -ConNeigh


set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(schmor.smf.conneigh.mod, type="int", 
                  axis.title = c("Stem Mass Fraction (SMF)","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

# Interaction: SMF*ConNeigh(+)
micrac=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==30,]

# model won't compute because ConNeigh value is the same for all individuals despite being in different plots
micrac.smf.conneigh.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh, micrac)

# Interaction: PC1*HetNeigh(+) and LA*HetNeigh(+)
alclat=rgr.pca.model[rgr.pca.model$sp_id==2,]
alclat.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==2,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
psyber=rgr.pca.model[rgr.pca.model$sp_id==43,]
swimac=rgr.pca.model[rgr.pca.model$sp_id==57,]
swimac.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==57,]

alclat.pc1.hetneigh.mod=lm(log_RGR_plus_1~pc_1*log_HetNeigh, alclat)
alclat.LA.hetneigh.mod=lm(log_RGR_plus_1~log_LA*log_HetNeigh, alclat.2)
guagui.LA.hetneigh.mod=lm(log_RGR_plus_1~log_LA*log_HetNeigh, guagui)
psyber.pc1.hetneigh.mod=lm(log_RGR_plus_1~pc_1*log_HetNeigh, psyber)
swimac.pc1.hetneigh.mod=lm(log_RGR_plus_1~pc_1*log_HetNeigh, swimac)
swimac.LA.hetneigh.mod=lm(log_RGR_plus_1~log_LA*log_HetNeigh, swimac.2)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(psyber.pc1.hetneigh.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log HetNeigh", title = "")

# Interaction: SRL x Treatment(-)
cecsch=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==6,]

cecsch.SRL.trt.mod=lm(log_RGR_plus_1~log_SRL*Treatment_factor, cecsch)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(cecsch.SRL.trt.mod, type="int", 
                  axis.title = c("Specific Root Length (SRL)","Log Relative Growth Rate (RGR)"), 
                  legend.title = "Treatment", title = "")

# Interaction: PC1 x Treatment(+) and LA x Treatment(-)
cecsch=rgr.pca.model[rgr.pca.model$sp_id==6,]
guagui=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==19,]
swimac=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==57,]

cecsch.PC1.trt.mod=lm(log_RGR_plus_1~pc_1*Treatment_factor, cecsch)
guagui.LA.trt.mod=lm(log_RGR_plus_1~log_LA*Treatment_factor, guagui)
swimac.LA.trt.mod=lm(log_RGR_plus_1~log_LA*Treatment_factor, swimac)


set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(cecsch.PC1.trt.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "Treatment", title = "")

# Interaction: PC1/LA*hetneigh*Treatment(-)
cecsch=rgr.pca.model[rgr.pca.model$sp_id==6,]
cecsch.2=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==6,]

cecsch.PC1.hetneigh.trt.mod=lm(log_RGR_plus_1~pc_1*log_HetNeigh*Treatment_factor, cecsch)
cecsch.LA.hetneigh.trt.mod=lm(log_RGR_plus_1~log_LA*log_HetNeigh*Treatment_factor, cecsch.2)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(cecsch.PC1.hetneigh.trt.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log HetNeigh", title = "")

output[[4]]

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(cecsch.LA.hetneigh.trt.mod, type="int", 
                  axis.title = c("Leaf Area (LA)","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log HetNeigh", title = "")

output[[4]]

# Interaction PC1*ConNeigh*Treatment(+)
psyber=rgr.pca.model[rgr.pca.model$sp_id==43,]
hibros=rgr.pca.model[rgr.pca.model$sp_id==22,]

psyber.PC1.conneigh.trt.mod=lm(log_RGR_plus_1~pc_1*log_ConNeigh*Treatment_factor, psyber)
hibros.PC1.conneigh.trt.mod=lm(log_RGR_plus_1~pc_1*log_ConNeigh*Treatment_factor, hibros)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(psyber.PC1.conneigh.trt.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

output[[4]]

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(hibros.PC1.conneigh.trt.mod, type="int", 
                  axis.title = c("PC1","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

output[[4]]

# Interaction: SMF*Conneight*Trt (+) and SRL*ConNeigh*Trt(-)
psyber=rgr.indiv.trait.model[rgr.indiv.trait.model$sp_id==43,]

psyber.SMF.conneigh.trt.mod=lm(log_RGR_plus_1~log_SMF*log_ConNeigh*Treatment_factor, psyber)
psyber.SRL.conneigh.trt.mod=lm(log_RGR_plus_1~log_SRL*log_ConNeigh*Treatment_factor, psyber)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(psyber.SMF.conneigh.trt.mod, type="int", 
                  axis.title = c("Stem Mass Fraction (SMF)","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

output[[4]]

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(psyber.SRL.conneigh.trt.mod, type="int", 
                  axis.title = c("Specific Root Length (SRL)","Log Relative Growth Rate (RGR)"), 
                  legend.title = "log ConNeigh", title = "")

output[[4]]

test=allEffects(psyber.SMF.conneigh.trt.mod)
plot(test)












# Another way to plot interactions
# find the range of the second term in the interaction
range(rgr.pca.model$log_HetNeigh)
# -1.473140, 2.993264

# Calculate the intercept of the min and max values
# first term is global intercept of model
# second term is coefficient of second term in interaction
# multiply by max and min value of second term in interaction

intercept.1=


pred <- expand.grid(pc_1=c(min(rgr.pca.model$pc_1),0,max(rgr.pca.model$pc_1)),
                    log_HetNeigh=c(min(rgr.pca.model$log_HetNeigh),0,max(rgr.pca.model$log_HetNeigh)),
                    Treatment_2=factor(0:1))
pred$log_RGR_plus_1 <- predict(pc1.hetneigh.trt.mod,pred)


ggplot(rgr.pca.model,aes(x=pc_1,y=log_RGR_plus_1,color=log_HetNeigh))+geom_point()+facet_grid(~Treatment_2)+
  geom_line(data=pred,aes(group=log_HetNeigh))


slope.1=0.191717161
slope.2=0.191717161+-0.008911660
slope.3=0.191717161+(0.026413307*-1.473140)
slope.4=0.191717161+(0.026413307*2.993264)+-0.008911660+(-0.033098700*2.993264)

# Calculate the slope of the min and max values
# first term is coefficient of first term in interaction
# second term is coefficient of interaction term 
# multiply by max and min value of second term in interaction

srl.conneigh.high=rgr.sp.traits.parm[11,2]+rgr.sp.traits.parm[163,2]*3.7325813
0.1133334
srl.conneigh.low=rgr.sp.traits.parm[11,2]+rgr.sp.traits.parm[162,2]*-0.7612897
-0.0285517

# Calculate the intercept of the min and max values
# first term is global intercept of model
# second term is coefficient of second term in interaction
# multiply by max and min value of second term in interaction

srl.conneigh.high.int=rgr.sp.traits.parm[1353,2]+rgr.sp.traits.parm[24,2]*3.7325813
-3.982554
srl.conneigh.low.int=rgr.sp.traits.parm[1353,2]+rgr.sp.traits.parm[24,2]*-0.7612897
-3.848533

# plotting slopes and intercepts
plot(rgr.indiv.trait.model$log_SRL, rgr.indiv.trait.model$log_RGR_plus_1, type="n", 
     xlab="Specific Root Length", ylab="Relative Growth Rate")
abline(-3.982554,0.1133334, col="black", lwd=4)
abline(-3.848533,-0.0285517, col="blue", lwd=4)
legend("topright", legend=c("ConNeigh","High", "Low"),
       col=c("NA","black", "blue"), lty=1, lwd=4)

# another way to plot interactions
library(effects)
# have to make lm of each model first and then feed it to allEffects
rgr.srl.cn=lm(log_RGR_plus_1~log_SRL*log_ConNeigh, rgr.indiv.trait.model)
rgr.srl.cn.effects=allEffects(rgr.srl.cn)
plot(rgr.srl.cn.effects, xlab="Log Specific Root Length (SRL)", ylab="Log Relative Growth Rate (RGR)", 
     main="RGR ~ SRL*Conspecific Neighbor Density")

# another plotting method 
library(emmeans)
mean(rgr.indiv.trait.model$log_ConNeigh)
range(rgr.indiv.trait.model$log_ConNeigh)
# -0.7612897, 3.7325813
mylist <- list(log_ConNeigh=c(-0.7612897, -2.504319e-11 ,3.7325813)) 
emtrends(rgr.srl.cn, ~log_ConNeigh, var="log_SRL",at=mylist)

mylist.2 <- list(log_SRL=seq(-6,4, by=1),log_ConNeigh=c(-0.7612897, -2.504319e-11 ,3.7325813)) 
emmip(rgr.srl.cn,log_ConNeigh~log_SRL,at=mylist.2, CIs=TRUE)

#### Correlate plot-level RGR with plot-level SR for each SR census ####

cor.df=read.csv("RGR.SR.correlation.csv", header=T, row.names=1)

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

