#### determining mean growth rates per plot ####

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


#### read in plot level mean RGR for treatments ####

all.rgr=read.csv("All.RGR.csv",header=T)
# plots 400.4, 400.16, 500.13 removed because no individuals survived so RGR is NaN

# standard t.test of all plot means for all elevations

t.test(all.rgr$Green.all, all.rgr$Red.all)
# t = -0.12687, df = 141.97, p = 0.8992
# mean Green = 0.1055484
# mean Red = 0.1067318

# t.test for each elevation separately
#300
t.test(all.rgr[1:25,2], all.rgr[1:25,3])
# t = -1.4251, df = 42.304, p = 0.1615
# mean Green = 0.06238131
# mean Red = 0.07561301

#400
t.test(all.rgr[26:48,2], all.rgr[26:48,3])
# t = 1.2156, df = 42.93, p = 0.2308
# mean Green = 0.11243465
# mean Red = 0.09631597

#500
t.test(all.rgr[49:72,2], all.rgr[49:72,3])
# t = -0.31504, df = 45.996, p = 0.7542
# mean Green = 0.1439147
# mean Red = 0.1491290

# Plot of RGR values for treatments overall and by elevations
boxplot(all.rgr[,c(2,3,10,11,14,15,18,19)], ylab="Mean Plot Relative Growth Rate", cex.axis = 1.50,cex.lab=1.5,
        names=c("All:C","All:T","300m:C","300m:T","400m:C","400m:T","500m:C","500m:C"))

# Plot of RGR at the individual level for each elevation and treatment

indiv.rgr=read.csv("All.Indiv.RGR.csv", header=T)
boxplot(indiv.rgr, ylab="Individual Relative Growth Rate")

# Species specific mean RGR between treatments

# Getting the phylogenetic tree

install.packages("brranching")
library(brranching)
library(ape)
library(phytools)
install.packages("phylocomr")
library(phylocomr)

# 300 m tree
taxa.300=read.csv("spp.300m.csv", header=FALSE)
tree.300=brranching::phylomatic(taxa=taxa.300, storedtree="zanne2014")

# 400 m tree
taxa.400=read.csv("spp.400m.csv", header=FALSE)
tree.400=brranching::phylomatic(taxa=taxa.400, storedtree="zanne2014")

# 500 m tree
taxa.500=read.csv("spp.500m.csv", header=FALSE)
tree.500=brranching::phylomatic(taxa=taxa.500, storedtree="zanne2014")

# all species from all elevations
all.taxa=read.csv("all.ssp.csv", header=FALSE)
all.tree=brranching::phylomatic(taxa=all.taxa, storedtree="zanne2014")

tip.rgr=read.csv("all.tree.tips.2.csv", header = T, row.names = 1)
phylosig(all.tree, tip.rgr$RGR, method = "K", test = TRUE, nsim=1000)


all.taxa.green=read.csv("all.ssp.green.csv", header = FALSE)
all.tree.green=brranching::phylomatic(taxa=all.taxa.green, storedtree="zanne2014")
tip.green=read.csv("all.tree.green.tips.csv", header=T, row.names = 1)
phylosig(all.tree.green, tip.green$RGR, method = "K", test = TRUE, nsim=1000)

all.taxa.red=read.csv("all.ssp.red.csv", header = FALSE)
all.tree.red=brranching::phylomatic(taxa=all.taxa.red, storedtree="zanne2014")
tip.red=read.csv("all.tree.red.tips.csv", header=T, row.names = 1)
phylosig(all.tree.red, tip.red$X.1, method = "K", test = TRUE, nsim=1000)

library(phytools)

phylANOVA(tree, Treatment, RGR, nsim=1000, posthoc=TRUE,)

tree<-pbtree(n=100)
Q<-matrix(c(-2,1,1,
            1,-2,1,
            1,1,-2),3,3)
rownames(Q)<-colnames(Q)<-letters[1:3]
x<-as.factor(sim.history(tree,Q)$states)
