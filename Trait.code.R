setwd("~/Desktop/Biotic.Impacts/Data")

# Read in dataframes that have all data

G.300.RGR=read.csv("300.Green.RGR.csv", header=T, row.names = 1)
R.300.RGR=read.csv("300.Red.RGR.csv", header=T, row.names = 1)
G.400.RGR=read.csv("400.Green.RGR.csv", header=T, row.names = 1)
R.400.RGR=read.csv("400.Red.RGR.csv", header=T, row.names = 1)
G.500.RGR=read.csv("500.Green.RGR.csv", header=T, row.names = 1)
R.500.RGR=read.csv("500.Red.RGR.csv", header=T, row.names = 1)

#### Calculate traits ####
# total plant biomass
G.300.RGR$total.biomass=G.300.RGR$total.leaf.mass+G.300.RGR$stem.mass+G.300.RGR$total.root.mass
G.400.RGR$total.biomass=G.400.RGR$total.leaf.mass+G.400.RGR$stem.mass+G.400.RGR$total.root.mass
G.500.RGR$total.biomass=G.500.RGR$total.leaf.mass+G.500.RGR$stem.mass+G.500.RGR$total.root.mass
R.300.RGR$total.biomass=R.300.RGR$total.leaf.mass+R.300.RGR$stem.mass+R.300.RGR$total.root.mass
R.400.RGR$total.biomass=R.400.RGR$total.leaf.mass+R.400.RGR$stem.mass+R.400.RGR$total.root.mass
R.500.RGR$total.biomass=R.500.RGR$total.leaf.mass+R.500.RGR$stem.mass+R.500.RGR$total.root.mass

# leaf mass per area
G.300.RGR$LMA=G.300.RGR$total.leaf.mass/G.300.RGR$total.leaf.area
G.400.RGR$LMA=G.400.RGR$total.leaf.mass/G.400.RGR$total.leaf.area
G.500.RGR$LMA=G.500.RGR$total.leaf.mass/G.500.RGR$total.leaf.area
R.300.RGR$LMA=R.300.RGR$total.leaf.mass/R.300.RGR$total.leaf.area
R.400.RGR$LMA=R.400.RGR$total.leaf.mass/R.400.RGR$total.leaf.area
R.500.RGR$LMA=R.500.RGR$total.leaf.mass/R.500.RGR$total.leaf.area

# leaf area ratio
G.300.RGR$LAR=G.300.RGR$total.leaf.area/G.300.RGR$total.biomass
G.400.RGR$LAR=G.400.RGR$total.leaf.area/G.400.RGR$total.biomass
G.500.RGR$LAR=G.500.RGR$total.leaf.area/G.500.RGR$total.biomass
R.300.RGR$LAR=R.300.RGR$total.leaf.area/R.300.RGR$total.biomass
R.400.RGR$LAR=R.400.RGR$total.leaf.area/R.400.RGR$total.biomass
R.500.RGR$LAR=R.500.RGR$total.leaf.area/R.500.RGR$total.biomass

# leaf mass fraction
G.300.RGR$LMF=G.300.RGR$total.leaf.mass/G.300.RGR$total.biomass
G.400.RGR$LMF=G.400.RGR$total.leaf.mass/G.400.RGR$total.biomass
G.500.RGR$LMF=G.500.RGR$total.leaf.mass/G.500.RGR$total.biomass
R.300.RGR$LMF=R.300.RGR$total.leaf.mass/R.300.RGR$total.biomass
R.400.RGR$LMF=R.400.RGR$total.leaf.mass/R.400.RGR$total.biomass
R.500.RGR$LMF=R.500.RGR$total.leaf.mass/R.500.RGR$total.biomass

# Stem mass fraction
G.300.RGR$SMF=G.300.RGR$stem.mass/G.300.RGR$total.biomass
G.400.RGR$SMF=G.400.RGR$stem.mass/G.400.RGR$total.biomass
G.500.RGR$SMF=G.500.RGR$stem.mass/G.500.RGR$total.biomass
R.300.RGR$SMF=R.300.RGR$stem.mass/R.300.RGR$total.biomass
R.400.RGR$SMF=R.400.RGR$stem.mass/R.400.RGR$total.biomass
R.500.RGR$SMF=R.500.RGR$stem.mass/R.500.RGR$total.biomass

# Specific stem length
G.300.RGR$SSL=G.300.RGR$stem.length/G.300.RGR$stem.mass
G.400.RGR$SSL=G.400.RGR$stem.length/G.400.RGR$stem.mass
G.500.RGR$SSL=G.500.RGR$stem.length/G.500.RGR$stem.mass
R.300.RGR$SSL=R.300.RGR$stem.length/R.300.RGR$stem.mass
R.400.RGR$SSL=R.400.RGR$stem.length/R.400.RGR$stem.mass
R.500.RGR$SSL=R.500.RGR$stem.length/R.500.RGR$stem.mass

# Root mass fraction
G.300.RGR$RMF=G.300.RGR$total.root.mass/G.300.RGR$total.biomass
G.400.RGR$RMF=G.400.RGR$total.root.mass/G.400.RGR$total.biomass
G.500.RGR$RMF=G.500.RGR$total.root.mass/G.500.RGR$total.biomass
R.300.RGR$RMF=R.300.RGR$total.root.mass/R.300.RGR$total.biomass
R.400.RGR$RMF=R.400.RGR$total.root.mass/R.400.RGR$total.biomass
R.500.RGR$RMF=R.500.RGR$total.root.mass/R.500.RGR$total.biomass

# Shoot to Root Ratio
G.300.RGR$SRR=(G.300.RGR$total.leaf.mass+G.300.RGR$stem.mass)/G.300.RGR$total.root.mass
G.400.RGR$SRR=(G.400.RGR$total.leaf.mass+G.400.RGR$stem.mass)/G.400.RGR$total.root.mass
G.500.RGR$SRR=(G.500.RGR$total.leaf.mass+G.500.RGR$stem.mass)/G.500.RGR$total.root.mass
R.300.RGR$SRR=(R.300.RGR$total.leaf.mass+R.300.RGR$stem.mass)/R.300.RGR$total.root.mass
R.400.RGR$SRR=(R.400.RGR$total.leaf.mass+R.400.RGR$stem.mass)/R.400.RGR$total.root.mass
R.500.RGR$SRR=(R.500.RGR$total.leaf.mass+R.500.RGR$stem.mass)/R.500.RGR$total.root.mass

# Specific root length
G.300.RGR$SRL=G.300.RGR$total.Length.cm./G.300.RGR$fine.root.mass
G.400.RGR$SRL=G.400.RGR$total.Length.cm./G.400.RGR$fine.root.mass
G.500.RGR$SRL=G.500.RGR$total.Length.cm./G.500.RGR$fine.root.mass
R.300.RGR$SRL=R.300.RGR$total.Length.cm./R.300.RGR$fine.root.mass
R.400.RGR$SRL=R.400.RGR$total.Length.cm./R.400.RGR$fine.root.mass
R.500.RGR$SRL=R.500.RGR$total.Length.cm./R.500.RGR$fine.root.mass

# Root length ratio
G.300.RGR$RLR=G.300.RGR$total.Length.cm./G.300.RGR$total.biomass
G.400.RGR$RLR=G.400.RGR$total.Length.cm./G.400.RGR$total.biomass
G.500.RGR$RLR=G.500.RGR$total.Length.cm./G.500.RGR$total.biomass
R.300.RGR$RLR=R.300.RGR$total.Length.cm./R.300.RGR$total.biomass
R.400.RGR$RLR=R.400.RGR$total.Length.cm./R.400.RGR$total.biomass
R.500.RGR$RLR=R.500.RGR$total.Length.cm./R.500.RGR$total.biomass

# Specific root area
G.300.RGR$SRA=G.300.RGR$SurfArea.cm2./G.300.RGR$fine.root.mass
G.400.RGR$SRA=G.400.RGR$SurfArea.cm2./G.400.RGR$fine.root.mass
G.500.RGR$SRA=G.500.RGR$SurfArea.cm2./G.500.RGR$fine.root.mass
R.300.RGR$SRA=R.300.RGR$SurfArea.cm2./R.300.RGR$fine.root.mass
R.400.RGR$SRA=R.400.RGR$SurfArea.cm2./R.400.RGR$fine.root.mass
R.500.RGR$SRA=R.500.RGR$SurfArea.cm2./R.500.RGR$fine.root.mass

# Root tissue density
G.300.RGR$RTD=G.300.RGR$fine.root.mass/G.300.RGR$RootVolume.cm3.
G.400.RGR$RTD=G.400.RGR$fine.root.mass/G.400.RGR$RootVolume.cm3.
G.500.RGR$RTD=G.500.RGR$fine.root.mass/G.500.RGR$RootVolume.cm3.
R.300.RGR$RTD=R.300.RGR$fine.root.mass/R.300.RGR$RootVolume.cm3.
R.400.RGR$RTD=R.400.RGR$fine.root.mass/R.400.RGR$RootVolume.cm3.
R.500.RGR$RTD=R.500.RGR$fine.root.mass/R.500.RGR$RootVolume.cm3.

write.csv(G.300.RGR, file="300.Green.RGR.csv")
write.csv(G.400.RGR, file="400.Green.RGR.csv")
write.csv(G.500.RGR, file="500.Green.RGR.csv")
write.csv(R.300.RGR, file="300.Red.RGR.csv")
write.csv(R.400.RGR, file="400.Red.RGR.csv")
write.csv(R.500.RGR, file="500.Red.RGR.csv")

# Only individuals with complete cases for all traits were used in RGR models
# These are from data frames 300.Green.RGR.traits, 300.Red.RGR.traits, 400.Green.RGR.traits,
# 400.Red.RGR.traits, 500.Green.RGR.traits, 500.Red.RGR.traits
# These data frames were combined into All.RGR.Data.models.csv file

#### Testing for normal distribution of trait ####

rgr.model.data=read.csv("All.RGR.Data.models.csv", header=T)

shapiro.test(rgr.model.data$Init.Height)
shapiro.test(rgr.model.data$RGR) 
shapiro.test(rgr.model.data$LA)
shapiro.test(rgr.model.data$LMA)
shapiro.test(rgr.model.data$LAR)
shapiro.test(rgr.model.data$LMF)
shapiro.test(rgr.model.data$SMF)
shapiro.test(rgr.model.data$SSL)
shapiro.test(rgr.model.data$RMF)
shapiro.test(rgr.model.data$SRR)
shapiro.test(rgr.model.data$SRL)
shapiro.test(rgr.model.data$SRA)
shapiro.test(rgr.model.data$RTD)
shapiro.test(rgr.model.data$RLR)
shapiro.test(rgr.model.data$ConNeigh)
shapiro.test(rgr.model.data$HetNeigh)
# no value is normally distributed

# log and scale variables
rgr.model.data$log.init.height=scale(log(rgr.model.data$Init.Height))
rgr.model.data$log.LA=scale(log(rgr.model.data$LA))
rgr.model.data$log.LMA=scale(log(rgr.model.data$LMA))
rgr.model.data$log.LAR=scale(log(rgr.model.data$LAR))
rgr.model.data$log.LMF=scale(log(rgr.model.data$LMF))
rgr.model.data$log.SMF=scale(log(rgr.model.data$SMF))
rgr.model.data$log.SSL=scale(log(rgr.model.data$SSL))
rgr.model.data$log.RMF=scale(log(rgr.model.data$RMF))
rgr.model.data$log.SRR=scale(log(rgr.model.data$SRR))
rgr.model.data$log.SRL=scale(log(rgr.model.data$SRL))
rgr.model.data$log.SRA=scale(log(rgr.model.data$SRA))
rgr.model.data$log.RTD=scale(log(rgr.model.data$RTD))
rgr.model.data$log.RLR=scale(log(rgr.model.data$RLR))
rgr.model.data$log.ConNeigh=scale(rgr.model.data$ConNeigh) # only scaled because of 0 values
rgr.model.data$log.HetNeigh=scale(rgr.model.data$HetNeigh) # only scaled because of 0 values

# add 1 to all values to get rid of negative values and 0 values so data can be logged
# difference between the range of values is essentially the same after adding 1
rgr.model.data$RGR.plus.1=rgr.model.data$RGR+1
rgr.model.data$log.RGR.plus.1=scale(log(rgr.model.data$RGR.plus.1))

#### determine correlation between traits ####
cor.test(rgr.model.data$log.LA,rgr.model.data$log.LMA) # r = 0.23 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.LAR) # r = -0.22 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.LMF) # r = -0.09 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.SMF) # r = 0.17 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.SSL) # r = -0.83 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.RMF) # r = -0.10 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.SRR) # r = 0.11 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.SRL) # r = -0.01 p = 0.66, not significant
cor.test(rgr.model.data$log.LA,rgr.model.data$log.SRA) # r = -0.15 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.RTD) # r = 0.30 p<0.0001
cor.test(rgr.model.data$log.LA,rgr.model.data$log.RLR) # r = -0.68 p<0.0001

cor.test(rgr.model.data$log.LMA,rgr.model.data$log.LAR) # r = -0.54 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.LMF) # r = 0.10 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.SMF) # r = -0.24 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.SSL) # r = -0.41 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.RMF) # r = -0.13 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.SRR) # r = 0.11 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.SRL) # r = -0.24 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.SRA) # r = -0.25 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.RTD) # r = 0.13 p<0.0001
cor.test(rgr.model.data$log.LMA,rgr.model.data$log.RLR) # r = -0.41 p<0.0001

cor.test(rgr.model.data$log.LAR,rgr.model.data$log.LMF) # r = 0.78 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.SMF) # r = -0.39 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.SSL) # r = 0.66 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.RMF) # r = -0.14 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.SRR) # r = 0.17 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.SRL) # r = 0.22 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.SRA) # r = 0.29 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.RTD) # r = -0.25 p<0.0001
cor.test(rgr.model.data$log.LAR,rgr.model.data$log.RLR) # r = 0.55 p<0.0001

cor.test(rgr.model.data$log.LMF,rgr.model.data$log.SMF) # r = -0.64 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.SSL) # r = -0.48 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.RMF) # r = -0.27 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.SRR) # r = 0.29 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.SRL) # r = 0.08 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.SRA) # r = 0.16 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.RTD) # r = -0.21 p<0.0001
cor.test(rgr.model.data$log.LMF,rgr.model.data$log.RLR) # r = 0.35 p<0.0001

cor.test(rgr.model.data$log.SMF,rgr.model.data$log.SSL) # r = -0.45 p<0.0001
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.RMF) # r = 0.01 p = 0.61, not significant
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.SRR) # r = 0.04 p = 0.051, not significant
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.SRL) # r = -0.02 p = 0.24, not significant
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.SRA) # r = -0.07 p<0.001
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.RTD) # r = 0.10 p<0.0001
cor.test(rgr.model.data$log.SMF,rgr.model.data$log.RLR) # r = -0.32 p<0.0001

cor.test(rgr.model.data$log.SSL,rgr.model.data$log.RMF) # r = 0.003 p = 0.88, not significant
cor.test(rgr.model.data$log.SSL,rgr.model.data$log.SRR) # r = -0.01 p = 0.56, not significant
cor.test(rgr.model.data$log.SSL,rgr.model.data$log.SRL) # r = 0.15 p<0.0001
cor.test(rgr.model.data$log.SSL,rgr.model.data$log.SRA) # r = 0.27 p<0.0001
cor.test(rgr.model.data$log.SSL,rgr.model.data$log.RTD) # r = -0.33 p<0.0001
cor.test(rgr.model.data$log.SSL,rgr.model.data$log.RLR) # r = 0.79 p<0.0001

cor.test(rgr.model.data$log.RMF,rgr.model.data$log.SRR) # r = -0.99 p<0.0001
cor.test(rgr.model.data$log.RMF,rgr.model.data$log.SRL) # r = -0.13 p<0.0001
cor.test(rgr.model.data$log.RMF,rgr.model.data$log.SRA) # r = -0.12 p<0.0001
cor.test(rgr.model.data$log.RMF,rgr.model.data$log.RTD) # r = 0.05 p<0.05
cor.test(rgr.model.data$log.RMF,rgr.model.data$log.RLR) # r = 0.20 p<0.0001

cor.test(rgr.model.data$log.SRR,rgr.model.data$log.SRL) # r = 0.15 p<0.0001
cor.test(rgr.model.data$log.SRR,rgr.model.data$log.SRA) # r = 0.14 p<0.0001
cor.test(rgr.model.data$log.SRR,rgr.model.data$log.RTD) # r = -0.06 p<0.05
cor.test(rgr.model.data$log.SRR,rgr.model.data$log.RLR) # r = -0.19 p<0.0001

cor.test(rgr.model.data$log.SRL,rgr.model.data$log.SRA) # r = 0.88 p<0.0001
cor.test(rgr.model.data$log.SRL,rgr.model.data$log.RTD) # r = -0.24 p<0.0001
cor.test(rgr.model.data$log.SRL,rgr.model.data$log.RLR) # r = 0.40 p<0.0001

cor.test(rgr.model.data$log.SRA,rgr.model.data$log.RTD) # r = -0.66 p<0.0001
cor.test(rgr.model.data$log.SRA,rgr.model.data$log.RLR) # r = 0.44 p<0.0001

cor.test(rgr.model.data$log.RTD,rgr.model.data$log.RLR) # r = -0.27 p<0.0001

#### Determining which three individual traits to use ####
# Best 3 trait option: LA, SMF, SRL
# captures leaf, stem, root trait
# LA ~ SRL, r = 0.01, not significant
# LA ~ SMF, r = 0.16, significant
# SRL ~ SMF, r = -0.02, not significant

# other trait option is SMF, RMF, SRL
# SMF ~ RMF, r = 0.01, not significant
# SMF ~ SRL, r = -0.02, not significant
# RMF ~ SRL, r = -0.13, significant

write.csv(rgr.model.data, file="rgr.model.data.csv")

#### dataframe used for individual trait models ####
# read df back in after changing treatment to 0 and 1 instead of green and red
rgr.model.data=read.csv("rgr.model.data.csv", header=T, row.names = 1)
rgr.indiv.trait.model=rgr.model.data[,c(1,2,4,7,22,25,26,30,34,39,43,44)]
# change column names to have underscores instead of periods
colnames(rgr.indiv.trait.model)[6]="log_init_height"
colnames(rgr.indiv.trait.model)[7]="log_LA"
colnames(rgr.indiv.trait.model)[8]="log_SMF"
colnames(rgr.indiv.trait.model)[9]="log_SRL"
colnames(rgr.indiv.trait.model)[10]="log_RGR_plus_1"
colnames(rgr.indiv.trait.model)[11]="log_ConNeigh"
colnames(rgr.indiv.trait.model)[12]="log_HetNeigh"
write.csv(rgr.indiv.trait.model, file="rgr.indiv.trait.model.csv")

#### Also calculate PC axes of traits instead of individual traits (12 total) ####
pc.traits=princomp(rgr.model.data[,c(25:36)])
summary(pc.traits) 
# first 3 axes explain 70% of variation
# first 4 axes explain 81% of variation
pc.trait.scores=pc.traits$scores[,1:3]
loadings.pc=with(pc.traits, unclass(loadings))
write.csv(loadings.pc, file="pc.loadings.csv")
sqrt(1/ncol(rgr.model.data[,c(25:36)])) # 0.2886751, cutoff for important loadings, value if all variables contributed equally
# PC 1 +LA(0.30), -LAR(0.39), -LMF(0.30), -SSL(0.43), -SRA(0.30), -RLR(0.41)
# PC 2 -RMF(0.60), SRR(0.60),
# PC 3 -LMF(0.38), SMF(0.43), SRL(0.49), SRA(0.50)
biplot(pc.traits$scores[, 1:3], pc.traits$loadings[, 1:3], cex=0.7)
rgr.model.data$pc_1=pc.trait.scores[,1]
rgr.model.data$pc_2=pc.trait.scores[,2]
rgr.model.data$pc_3=pc.trait.scores[,3]
write.csv(rgr.model.data, file="rgr.model.data.csv")

#### Dataframe used for pc models ####
rgr.pca.model=rgr.model.data[,c(1,2,4,7,22,25,39,40,41,42,43,44)]
colnames(rgr.pca.model)[6]="log_init_height"
colnames(rgr.pca.model)[7]="log_RGR_plus_1"
colnames(rgr.pca.model)[11]="log_ConNeigh"
colnames(rgr.pca.model)[12]="log_HetNeigh"
write.csv(rgr.pca.model, file="rgr.pca.model.csv")

# Determining range of trait data for Table S1 ####

trait.data=read.csv("rgr.model.data.csv", row.names = 1)
range(trait.data$LA)
range(trait.data$LMA)
range(trait.data$LMF)
range(trait.data$LAR)
range(trait.data$SMF)
range(trait.data$SSL)
range(trait.data$RMF)
range(trait.data$SRR)
range(trait.data$SRL)
range(trait.data$SRA)
range(trait.data$RTD)
range(trait.data$RLR)





