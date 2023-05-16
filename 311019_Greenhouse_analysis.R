# karina.gutierrez@cinvestav.mx
# Karina Gutierrez Moreno
# PhD Student on Integrative Biology (Cinvestav Irapuato Mexico)
# Supervisor: Martin Heil
# Statistical analysis of 2018 greenhouse experiments
# Edited: October 2019

# Set on your working directory
setwd("~/R/Invernaderotodos_2018")

# Create a dataset
# Reading the table for this experiment
pots<-read.table("220119_allsetskarina.txt", sep="\t", header=T)
head(pots)
summary(pots)

pots$Set <- as.factor(pots$Set)
summary(pots$Set)

# Dependent variables are "Spores", "Area", "Dweight".
# Spores: Percentage of Colletotrichum germinated spores on leaves surface
# Area: Percentage of damaged leaf area (Disease severity)
# Dweight: Plant shoot dry weight (g)
# Design is (complete factorial) Set * Cultivar * Treatment
# Where:
# Set: Greenhouse experimental set and where:
# Set 1: Natural field soil Spring 2018
# Set 2: Natural field soil autumn 2018
# Set 3: Autoclaved greenhouse mix autumn 2018

# Cultivar: Bean genotype
# Treatment: Trichoderma treatment or control with no Trichoderma

# Analyses of "Spores"
spores.aov <- aov(Spores ~ Set * Cultivar * Treatment, data=pots)
anova(spores.aov)

shapiro.test(spores.aov$residuals)
hist(spores.aov$residuals)
# Interpretation: The non-normality is possibly due to a few residuals larger than 30 units.

model.tables(spores.aov, type="means", se=T)

# Performing Tukey test for Spores within all experimental sets
TukeyHSD(spores.aov, which="Set")
TukeyHSD(spores.aov, which="Cultivar")
TukeyHSD(spores.aov, which="Treatment")

# Non-parametric alternative to ANOVA (one main factor at the time)
kruskal.test(Spores ~ Set, data=field)
kruskal.test(Spores ~ Cultivar, data=field)
kruskal.test(Spores ~ Treatment, data=field)

# Segregating each condition into different data frames.
pots.s1 <- pots[as.integer(pots$Set)==1,] # Only Set 1: Soil spring 2018
pots.s2 <- pots[as.integer(pots$Set)==2,] # Only Set 2: Soil autumn 2018
pots.s3 <- pots[as.integer(pots$Set)==3,] # Only Set 3: Mix autumn 2018

# Performing ANOVA for Spores at each one of the sets
spores.aov1 <- aov(Spores ~ Cultivar * Treatment, data=pots.s1)
spores.aov2 <- aov(Spores ~ Cultivar * Treatment, data=pots.s2)
spores.aov3 <- aov(Spores ~ Cultivar * Treatment, data=pots.s3)
anova(spores.aov1)
anova(spores.aov2)
anova(spores.aov3)

# Tukey-test
# Set 1
TukeyHSD(spores.aov1, which="Cultivar")
TukeyHSD(spores.aov1, which="Treatment")

# Set 2
TukeyHSD(spores.aov2, which="Cultivar")
TukeyHSD(spores.aov2, which="Treatment")

# Set 3
TukeyHSD(spores.aov3, which="Cultivar")
TukeyHSD(spores.aov3, which="Treatment")


# Analyses of "Area" for all experimental sets
summary(pots$Area)
darea.aov <- aov(Area ~ Set * Cultivar * Treatment, data=pots)
summary(darea.aov)

#Tukey-test
TukeyHSD(darea.aov, which="Set")
TukeyHSD(darea.aov, which="Cultivar")
TukeyHSD(darea.aov, which="Treatment")

# Tukey-test for individual Experimental Set
area.aov1 <- aov(Area ~ Cultivar * Treatment, data=pots.s1)
area.aov2 <- aov(Area ~ Cultivar * Treatment, data=pots.s2)
area.aov3 <- aov(Area ~ Cultivar * Treatment, data=pots.s3)
anova(area.aov1)
anova(area.aov2)
anova(area.aov3)

# Tukey-test
# Set 1
TukeyHSD(area.aov1, which="Cultivar")
TukeyHSD(area.aov1, which="Treatment")

# Set 2
TukeyHSD(area.aov2, which="Cultivar")
TukeyHSD(area.aov2, which="Treatment")

# Set 3
TukeyHSD(area.aov3, which="Cultivar")
TukeyHSD(area.aov3, which="Treatment")


# Non-parametric alternative
kruskal.test(Area ~ Cultivar, data=pots.s1)
kruskal.test(Area ~ Cultivar, data=pots.s2)
kruskal.test(Area ~ Cultivar, data=pots.s3)
kruskal.test(Area ~ Treatment, data=pots.s1)
kruskal.test(Area ~ Treatment, data=pots.s2)
kruskal.test(Area ~ Treatment, data=pots.s2)
