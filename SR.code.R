#### Determining total number of individuals censused ####

SR.G300=read.csv("300.Green.SR.csv")
SR.R300=read.csv("300.Red.SR.csv")
SR.G400=read.csv("400.Green.SR.csv")
SR.R400=read.csv("400.Red.SR.csv")
SR.G500=read.csv("500.Green.SR.csv")
SR.R500=read.csv("500.Red.SR.csv")

866+498+445+744+559+414
# 3526 total individuals censused

#### Determining Monthly Survival Rate ####

# Read in Survival Rate data sheet

SR.300.Green=read.csv("300.Green.SR.2.csv", header=T)
SR.300.Red=read.csv("300.Red.SR.2.csv", header=T)
SR.400.Green=read.csv("400.Green.SR.2.csv", header=T)
SR.400.Red=read.csv("400.Red.SR.2.csv", header=T)
SR.500.Green=read.csv("500.Green.SR.2.csv", header=T)
SR.500.Red=read.csv("500.Red.SR.2.csv", header=T)

# Take the means of the SR columns and put them into Elev.SR.csv
colMeans(SR.300.Green)
colMeans(SR.400.Green)
colMeans(SR.500.Green)
colMeans(SR.300.Red)
colMeans(SR.400.Red)
colMeans(SR.500.Red)

# Plot SR overall and each elevation by treatment
all.SR=read.csv("Elev.SR.csv", header=T, row.names=1)
# Plot for Figure 1B
boxplot(all.SR, ylab="Mean Survival Rate", cex.axis = 1.5, cex.lab=1.5,names=c("All:C",
"All:T","300m:C","300m:T","400m:C","400m:T","500m:C","500m:C"))

t.test(all.SR$Green.All, all.SR$Red.All)
# t = 0.013183, df = 21.964, p = 0.9896
# mean Green = 9650677
# mean Red = 0.9649270 

# t.test for each elevation separately
#300
t.test(all.SR$Green.300, all.SR$Red.300)
# t = -0.77917, df = 19.734, p = 0.4451
# mean Green = 0.9687378
# mean Red = 0.9782805

#400
t.test(all.SR$Green.400, all.SR$Red.400)
# t = -0.0047536, df = 21.963, p = 0.9963
# mean Green = 0.9536385
# mean Red = 0.9537038

#500
t.test(all.SR$Green.500, all.SR$Red.500)
# t = 0.94366, df = 21.66, p = 0.3558
# mean Green = 0.9728269
# mean Red = 0.9627966

### Making SR df ready for model ####

dec_jan=read.csv("survival.Dec.Jan.csv", header=T)
dec_jan$log_ini_size=scale(log(dec_jan$ini_size))
dec_jan$log_ConNeigh=scale(dec_jan$ConNeigh) # only scaled because of 0 values
dec_jan$log_HetNeigh=scale(dec_jan$HetNeigh) # only scaled because of 0 values
write.csv(dec_jan, file="survival.Dec.Jan.csv")

# Plot mean SR overtime for each treatment overall and by elevation

time=row.names(all.SR)
library(RColorBrewer)
brewer.pal(8, "Dark2")

plot(all.SR$Green.All, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#1B9E77",
     xaxt="n", type="l", ylim=c(0.85,1), lwd=1.5)
par(new=T)
plot(all.SR$Red.All, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#D95F02",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Green.300, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#7570B3",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Red.300, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#E7298A",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Green.400, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#66A61E",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Red.400, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#E6AB02",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Green.500, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#A6761D",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
par(new=T)
plot(all.SR$Red.500, xlab = "Monthly Intervals", ylab = "Mean Survival Rate", col="#666666",
     xaxt="n", type="l",ylim=c(0.85,1),lwd=1.5)
xtick=seq(1,12, by=1)
axis(side=1, at=xtick, labels=time, cex.axis=0.75)
legend("bottomleft", legend=c("All.Green", "All.Red", "300.Green", "300.Red",
                              "400.Green", "400.Red", "500.Green", "500.Red"),
       col=c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E", "#E6AB02",
             "#A6761D", "#666666"), lty=1, lwd=1.5, cex = 0.75)

### Make sure of significant variables ####

setwd("/Users/samanthaworthy/Desktop/Biotic.Impacts/Data/Results")
# read in the parameter output file from the fitted model
jan.feb.parm=read.csv("survival.Jan.Feb.sp.output.csv")
feb.mar.parm=read.csv("survival.Feb.Mar.sp.output.csv")
mar.apr.parm=read.csv("survival.Mar.Apr.sp.output.csv")
apr.may.parm=read.csv("survival.Apr.May.sp.output.csv")
may.june.parm=read.csv("survival.May.June.sp.output.csv")
june.july.parm=read.csv("survival.June.July.sp.output.csv")
juky.aug.parm=read.csv("survival.July.Aug.sp.output.csv")
aug.sept.parm=read.csv("survival.Aug.Sept.sp.output.csv")
sept.oct.parm=read.csv("survival.Sept.Oct.sp.output.csv")
oct.nov.parm=read.csv("survival.Oct.Nov.sp.output.csv")
nov.dec.parm=read.csv("survival.Nov.Dec.sp.output.csv")
dec.jan.parm=read.csv("survival.Dec.Jan.sp.output.csv")

sig.neg.=dec.jan.parm[dec.jan.parm$X2.50. < 0 & dec.jan.parm$X97.50. < 0,]
sig.pos.=dec.jan.parm[dec.jan.parm$X2.50. > 0 & dec.jan.parm$X97.50. > 0,]

#### Species-Specific Models

setwd("/Users/samanthaworthy/Desktop/Biotic.Impacts/Data")
feb_mar.data=read.csv("survival.Feb.Mar.csv", row.names = 1)
feb_mar.data$Treatment_factor=as.factor(feb_mar.data$Treatment_factor)
drygla=subset(feb_mar.data, feb_mar.data$species=="DRYGLA")
slober=subset(feb_mar.data, feb_mar.data$species=="SLOBER")
swimac=subset(feb_mar.data, feb_mar.data$species=="SWIMAC")
may_june.data=read.csv("survival.May.June.csv", row.names = 1)
may_june.data$Treatment_factor=as.factor(may_june.data$Treatment_factor)
sept_oct.data=read.csv("survival.Sept.Oct.csv", row.names = 1)
sept_oct.data$Treatment_factor=as.factor(sept_oct.data$Treatment_factor)
nov_dec.data=read.csv("survival.Nov.Dec.csv", row.names = 1)
nov_dec.data$Treatment_factor=as.factor(nov_dec.data$Treatment_factor)

# Interaction: ConNeigh*Treatment

feb.mar.conneigh.trt.mod=glm(survival~log_ConNeigh*Treatment_factor, feb_mar.data, family = binomial)
drygla.conneigh.trt.mod=glm(survival~log_ConNeigh*Treatment_factor, drygla,family = binomial)
slober.conneigh.trt.mod=glm(survival~log_ConNeigh*Treatment_factor, slober,family = binomial)
swimac.conneigh.trt.mod=glm(survival~log_ConNeigh*Treatment_factor, swimac,family = binomial)

set_theme(base = theme_classic(), axis.title.size = 1.5, axis.textcolor = "black")
output=plot_model(drygla.conneigh.trt.mod, type="int",
                  axis.title = c("Log Conspecific Neighbor Density","Survival Probability"), 
                  legend.title = "Treatment", title = "")

# Model: survival probability~treatment

may_june.trt=glm(survival~Treatment_factor, may_june.data, family = binomial)
sept_oct.trt=glm(survival~Treatment_factor, sept_oct.data, family = binomial)
nov_dec.trt=glm(survival~Treatment_factor, nov_dec.data, family = binomial)

output=plot_model(may_june.trt, type = "slope")

plot(allEffects(nov_dec.trt))
