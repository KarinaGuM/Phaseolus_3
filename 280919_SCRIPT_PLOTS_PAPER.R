# karina.gutierrez@cinvestav.mx
# Karina Gutierrez Moreno
# PhD Student on Integrative Biology (Cinvestav Irapuato Mexico)
# Supervisor: Martin Heil
# Statistical analysis of 2019 field experiments
# August 6th 2019 
# Edited: September 6th 2019

# Set on your working directory
setwd("~/R/Invernaderotodos_2018")

# Create a dataset

# Reading the table for this experiment
field <- read.table("220119_allsetskarina.txt", sep="\t", header=T)
head(field)

# WORKING FIRST WITH SPORES GERMINATION

# New table (data.frame) for Spores germination (Spores)
SPORES <- field[,c(2,3,4,6)]
head(SPORES)
SPORES2 <- na.omit(SPORES)
head(SPORES2)

# Library
# Install if necessary
#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}
if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(ggplot2)
}

if (!require("Rmisc")) {
  install.packages("Rmisc", dependencies = TRUE)
  library(Rmisc)
}

if (!require("plyr")) {
  install.packages("plyr", dependencies = TRUE)
  library(plyr)
}

if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library(dplyr)
}

# PLAYING WITH DATA TO MAKE BOXPLOTS AND KNOW THE EFFECT OF FACTORS (AS PREVIOSLY KNOWN
# WITH ANOVA TESTS)

p <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill= Treatment)) + 
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette="Greys") +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", x = "Bean genotypes", fill="Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold")) # checar nombres para cada set
p

p <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill= Treatment)) + 
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", x = "Bean genotypes", fill="Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold")) # checar nombres para cada set
p

############################ 16-Junio-2020 ############################
p <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill= Treatment)) + 
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", x = "Bean genotypes", fill="Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold")) # checar nombres para cada set
p




p2 <- ggplot(SPORES2, aes(x=Treatment, y=Spores, fill= Cultivar)) + 
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette="Greys") +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", x = "Trichoderma treatment", fill="Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p2

####################### THIS IS THE GOOD ONE ##############################

p3 <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill=Cultivar)) + 
  geom_boxplot() +
  theme_light() +
  scale_fill_brewer(palette="BuGn") +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3

####################### Aquí 17-junio-2020 ######################
p3 <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill=Cultivar)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  scale_fill_brewer(palette="Pastel1") +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3


p3 <- ggplot(SPORES2, aes(x=Cultivar, y=Spores, fill=Cultivar)) + 
  geom_boxplot() +
  theme_bw() +
  geom_jitter(width=0.25, alpha=0.5) +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3

#########################################################################

p4 <- ggplot(SPORES2, aes(x=Trichoderma, y=Spores, fill=Trichoderma)) + 
  geom_boxplot() +
  theme_classic() +
  scale_fill_brewer(palette="Greys") +
  facet_wrap(~Set) +
  labs(y = "Percentage of germinated spores", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p4

# PLAYING WITH DATA, NOW FOR DISEASE SEVERITY (DAMAGED AREA; AREA)

# New table (data.frame) for Spores germination (Area)
DAREA <- field[,c(2,3,4,7)]
head(DAREA)
DAREA2 <- na.omit(DAREA)
head(DAREA2)

p3 <- ggplot(DAREA2, aes(x=Cultivar, y=Area, fill=Cultivar)) + 
  geom_boxplot() +
  theme_light() +
  scale_fill_brewer(palette="BuGn") +
  facet_wrap(~Set) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3


########################### 17-junio-2020 #############################
p3 <- ggplot(DAREA2, aes(x=Cultivar, y=Area, fill=Cultivar)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  scale_fill_brewer(palette="Dark2") +
  facet_wrap(~Set) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3



# WORKING NOW WITH INDIVIDUALL SETS FOR MAKE NEW INDIVIDUAL PLOTS WITH ERROR BARS
# Working first with SET 1 (Spring 2018 natural soil) AND SPORES
setwd("~/R/Invernadero2018_Suelo1")
set1 <- read.table("18082018_SOILone.txt", sep="\t", header=T)
head(set1)

# Working first with spores' germination
GSPORES <- set1[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

Trichoderma<-GSPORES2$Treatment
Gspores<-GSPORES2$Spores
Cultivar<-GSPORES2$Cultivar
Spores.new<-data.frame(Cultivar, Trichoderma, Gspores)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Spores.new, measurevar="Gspores", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Gspores, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Infection level") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of germinated spores") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p

########################### 17-JUNIO-2020 #############################
p1 <- ggplot(GSPORES2, aes(x=Cultivar, y=Spores, fill=Treatment)) +
  geom_boxplot() +
  geom_point() +
  theme_bw() +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p1


p2 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p2

p2+stat_compare_means()


######################## 19-junio-2020 ###################################
## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Gspores, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  geom_point() +
  ggtitle("Infection level") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of germinated conidia") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


############### ESTA ES LA BUENASSSS ########## 19062020 ################

p5 <- ggplot(tgc, aes(y=Gspores, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 50)) +
  ylab("Percentage of germinated conidia") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


###################################################################


# Working then with disease severity (percentage of damaged leaf area)
AREA <- set1[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

Trichoderma<-AREA2$Treatment
Area<-AREA2$Area
Cultivar<-AREA2$Cultivar
Area.new<-data.frame(Cultivar, Trichoderma, Area)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Area.new, measurevar="Area", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Area, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Disease severity") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


########## BarPlot Face Wrap ##############
p5 <- ggplot(tgc, aes(y=Area, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 100)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


#################### Boxplot facewrap ####################
p6 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6




# Working first with SET 2 (Autumn 2018 natural soil) AND SPORES
setwd("~/R/Invernadero2018_Suelo2")
set2 <- read.table("311218Allresults_Soiltwo.txt", sep="\t", header=T)
head(set2)

# Working first with spores' germination
GSPORES <- set2[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

Trichoderma<-GSPORES2$Treatment
Gspores<-GSPORES2$Spores
Cultivar<-GSPORES2$Cultivar
Spores.new<-data.frame(Cultivar, Trichoderma, Gspores)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Spores.new, measurevar="Gspores", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Gspores, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Infection level") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of germinated spores") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p

##################### Barplot 24062020 ######################
p5 <- ggplot(tgc, aes(y=Gspores, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 50)) +
  ylab("Percentage of germinated conidia") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6



# Working then with disease severity (percentage of damaged leaf area)
AREA <- set2[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

Trichoderma<-AREA2$Treatment
Area<-AREA2$Area
Cultivar<-AREA2$Cultivar
Area.new<-data.frame(Cultivar, Trichoderma, Area)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Area.new, measurevar="Area", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Area, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Disease severity") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


##################### Barplot 24062020 ######################
p5 <- ggplot(tgc, aes(y=Area, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 60)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6



# Working first with SET 3 (Autumn 2018 sterile greenhouse mix) AND SPORES
setwd("~/R/Invernadero2018_Sustrato")
set3 <- read.table("AllResults_Mix.txt", sep="\t", header=T)
head(set3)

# Working first with spores' germination
GSPORES <- set3[,c(2,3,5)]
head(GSPORES)
GSPORES2 <- na.omit(GSPORES)
head(GSPORES2)

Trichoderma<-GSPORES2$Treatment
Gspores<-GSPORES2$Spores
Cultivar<-GSPORES2$Cultivar
Spores.new<-data.frame(Cultivar, Trichoderma, Gspores)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Spores.new, measurevar="Gspores", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Gspores, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Infection level") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of germinated spores") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


##################### Barplot 24062020 ######################
p5 <- ggplot(tgc, aes(y=Gspores, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 50)) +
  ylab("Percentage of germinated conidia") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Gspores-se, ymax=Gspores+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(GSPORES2, aes(x=Treatment, y=Spores, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of germinated conidia", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6



# Working then with disease severity (percentage of damaged leaf area)
AREA <- set3[,c(2,3,6)]
head(AREA)
AREA2 <- na.omit(AREA)
head(AREA2)

Trichoderma<-AREA2$Treatment
Area<-AREA2$Area
Cultivar<-AREA2$Cultivar
Area.new<-data.frame(Cultivar, Trichoderma, Area)

## How to add standard error bars to the plot?
## VISIT: http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(Area.new, measurevar="Area", groupvars=c("Cultivar","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then -->
p <- ggplot(tgc, aes(fill=Trichoderma, y=Area, x=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Disease severity") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 60)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV","Pv4")) +
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=0, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


##################### Barplot 24062020 ######################
p5 <- ggplot(tgc, aes(y=Area, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Cultivar, ncol = 5) +
  scale_y_continuous(limits = c(0, 60)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Area-se, ymax=Area+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(AREA2, aes(x=Treatment, y=Area, fill=Treatment)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Cultivar, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6



# Working now with Field Experiments (Spring 2019)
setwd("~/R/FIELD_2019")
field<-read.table("150919_FIELD_noPv4.txt", sep="\t", header=T)
head(field)

# Working first with boxplots to show the effects of Condition: With and without Colletotrichum
# on disease severity, according to ANOVA test.
DAREA <- field[,c(1,2,3,6)]
head(DAREA)
DAREA2 <- na.omit(DAREA)
head(DAREA2)

p <- ggplot(DAREA2, aes(x=Genotype, y=D.AREA, fill=Genotype)) + 
  geom_boxplot() +
  theme_light() +
  scale_fill_brewer(palette="BuGn") +
  facet_wrap(~Colletotrichum) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(angle = 0, hjust = 1, size=14),
        axis.text.x = element_text(angle = 0, hjust = 1, size=14))
p

######################## 24JUNIO2020 #################################
p3 <- ggplot(DAREA2, aes(x=Genotype, y=D.AREA, fill=Genotype)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  scale_fill_brewer(palette="Dark2") +
  facet_wrap(~Colletotrichum) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3


# Working now with herbivory and evaluating the effect of Colletotrichum inoculation
HAREA <- field[,c(1,2,3,7)]
head(HAREA)
HAREA2 <- na.omit(HAREA)
head(HAREA2)

p <- ggplot(HAREA2, aes(x=Genotype, y=H.AREA, fill=Genotype)) + 
  geom_boxplot() +
  theme_light() +
  scale_fill_brewer(palette="BuGn") +
  facet_wrap(~Colletotrichum) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.y = element_text(angle = 0, hjust = 1, size=14),
        axis.text.x = element_text(angle = 0, hjust = 1, size=14))
p

##################### 24JUNIO2020 ######################################
p3 <- ggplot(HAREA2, aes(x=Genotype, y=H.AREA, fill=Genotype)) + 
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  scale_fill_brewer(palette="Pastel2") +
  facet_wrap(~Colletotrichum) +
  labs(y = "Percentage of damaged leaf area", fill= "Bean genotype") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
p3


# Segregating each condition into different data frames.
field.C1 <- field[as.integer(field$Colletotrichum)==1,] # Only Condition 1: With Colletotrichum
field.C2 <- field[as.integer(field$Colletotrichum)==2,] # Only Condition 2: No Colletotrichum

C1<-na.omit(field.C1)
field1<-C1[,c(2,3,4,5,6,7)]
head(field1)

C2<-na.omit(field.C2)
field2<-C2[,c(2,3,4,5,6,7)]
head(field2)

# We will work first with field1 and LOG CFU
# Then, you can substitute with Area or Dweight, D.AREA, H.AREA, LOG.CFU, Etc.
Trichoderma<-field1$Trichoderma
CFU<-field1$LOG.CFU
Darea<-field1$D.AREA
Harea<-field1$H.AREA
Genotype<-field1$Genotype
field.new <- data.frame(Genotype, Trichoderma, Darea, Harea, CFU)

# summarySE provides the standard deviation, standard error of the mean, 
# and a (default 95%) confidence interval
tgc <- summarySE(field.new, measurevar="CFU", groupvars=c("Genotype","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then --> 
p <- ggplot(tgc, aes(fill=Trichoderma, y=CFU, x=Genotype)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Infection level") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV")) + #In the same order as the table
  ylab("Log [CFU/leaf fresh weight (g)]") +
  xlab("Common bean genotype") +
  theme(text = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14), # For Colletotrichum italic
        axis.title=element_text(size=14),
        axis.text.y = element_text(angle = 0, hjust = 1, size=14),
        axis.text.x = element_text(angle = 0, hjust = 1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=CFU-se, ymax=CFU+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p

######################################################################
####################### BARPLOT 24JUNIO20 ###########################
p5 <- ggplot(tgc, aes(y=CFU, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Genotype, ncol=4) +
  scale_y_continuous(limits = c(0, 1.5)) +
  ylab("LOG[CFU/leaf fresh weight (g)]") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=CFU-se, ymax=CFU+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(field1, aes(x=Trichoderma, y=LOG.CFU, fill=Trichoderma)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 5) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "LOG[CFU/fresh leaf weight (g)]", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6


# Working now with damaged area due to disease 
tgc <- summarySE(field.new, measurevar="Darea", groupvars=c("Genotype","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then --> 
p <- ggplot(tgc, aes(fill=Trichoderma, y=Darea, x=Genotype)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Disease severity") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV")) + #In the same order as the table
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14), # For Colletotrichum italic
        axis.title=element_text(size=14),
        axis.text.y = element_text(angle = 0, hjust = 1, size=14),
        axis.text.x = element_text(angle = 0, hjust = 1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Darea-se, ymax=Darea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


####################### BARPLOT 24JUNIO20 ###########################
p5 <- ggplot(tgc, aes(y=Darea, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Genotype, ncol=4) +
  scale_y_continuous(limits = c(0, 30)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Darea-se, ymax=Darea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(field1, aes(x=Trichoderma, y=D.AREA, fill=Trichoderma)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6


# Finally we will work for the Herbivory damage for Colletotrichum condition
tgc <- summarySE(field.new, measurevar="Harea", groupvars=c("Genotype","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then --> 
p <- ggplot(tgc, aes(fill=Trichoderma, y=Harea, x=Genotype)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Herbivory") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 7)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV")) + #In the same order as the table
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14), # For Colletotrichum italic
        axis.title=element_text(size=14),
        axis.text.y = element_text(angle = 0, hjust = 1, size=22),
        axis.text.x = element_text(angle = 0, hjust = 1, size=18)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Harea-se, ymax=Harea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


####################### BARPLOT 24JUNIO20 ###########################
p5 <- ggplot(tgc, aes(y=Harea, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Genotype, ncol=4) +
  scale_y_continuous(limits = c(0, 4)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Harea-se, ymax=Harea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(field1, aes(x=Trichoderma, y=H.AREA, fill=Trichoderma)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6



# Since I only had interesting results for Herbivory in the experimental set with and 
# without Colletotrichum I will use the second condition to plot these results
Trichoderma<-field2$Trichoderma
Darea<-field2$D.AREA
Harea<-field2$H.AREA
Genotype<-field2$Genotype
field.new <- data.frame(Genotype, Trichoderma, Darea, Harea)

tgc <- summarySE(field.new, measurevar="Harea", groupvars=c("Genotype","Trichoderma"), na.rm=TRUE)
tgc

## na.rm=TRUE argument to the  end of the argument list should clear it if theres NAs**

## Then --> 
p <- ggplot(tgc, aes(fill=Trichoderma, y=Harea, x=Genotype)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Herbivory") +
  scale_fill_brewer(palette="RdBu", direction=-1) +
  theme_classic() + #white theme (withot squared theme)
  scale_y_continuous(limits = c(0, 7)) +
  scale_x_discrete(limit = c("FJM", "FMA", "NSL","PV")) + #In the same order as the table
  ylab("Percentage of damaged leaf area") +
  xlab("Common bean genotype") +
  theme(text = element_text(size=14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14), # For Colletotrichum italic
        axis.title=element_text(size=14),
        axis.text.y = element_text(angle = 0, hjust = 1, size=22),
        axis.text.x = element_text(angle = 0, hjust = 1, size=18)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Harea-se, ymax=Harea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p


####################### BARPLOT 24JUNIO20 ###########################
p5 <- ggplot(tgc, aes(y=Harea, x=Trichoderma, fill=Trichoderma)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer(palette="Pastel2") +
  theme_bw() + 
  facet_wrap(~Genotype, ncol=4) +
  scale_y_continuous(limits = c(0, 7)) +
  ylab("Percentage of damaged leaf area") +
  theme(text=element_text(size=14),
        legend.title=element_text(size = 14),
        legend.text=element_text(size = 14),
        plot.title=element_text(size=14),
        axis.title=element_text(size=14),
        axis.text.y=element_text(angle=0, hjust=1, size=14), 
        axis.text.x=element_text(angle=45, hjust=1, size=14)) +
  labs(fill = "Trichoderma treatment") +
  geom_errorbar(aes(ymin=Harea-se, ymax=Harea+se), # dont forget to change variable here
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
p5


####################### BOxPLOT 24062020 ####################
p6 <- ggplot(field2, aes(x=Trichoderma, y=H.AREA, fill=Trichoderma)) +
  geom_boxplot() +
  theme_bw() +
  geom_point() +
  facet_wrap(~Genotype, ncol = 4) +
  scale_fill_brewer(palette="Pastel2") +
  labs(y = "Percentage of damaged leaf area", fill= "Trichoderma treatment") +
  theme(text = element_text(size=16),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
p6
