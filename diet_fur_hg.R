########################################################################
# Environmental Hg ~ Gut Hg ~ Fur Hg
# Written in R version 3.5.0
########################################################################
# Load Data
data = read.csv("mink_ro_gut_bw.csv")

# Load Libraries
library(car)
library(corrplot)
library(AICcmodavg)
library(lmtest)
library(heplots) # partial r2
library(weights) # weighted correlation

# Plotting Libraries
library(ggplot2)
library(ggpubr)
library(sjPlot)

########################################################################
#### Summarize Data ####
# Add logged variables to dataset
data$log_fur=log10(data$FurHg)
data$log_gut=log10(data$GutHg)

# Subset and summarize mink data
mink  = subset(data, data$Species == "Mink")
summary(mink)
sd(mink$FurHg)
sd(mink$GutHg)

#Subset and summarize river otter data
otter  = subset(data, data$Species == "Otter")
summary(otter)
sd(otter$FurHg)
sd(otter$GutHg)

########################################################################
#### Relationship between Fur and Gut ####
# Test normality of variables
shapiro.test(otter$FurHg)
shapiro.test(otter$GutHg)
shapiro.test(mink$FurHg)
shapiro.test(mink$GutHg)
# Use logged variables due to the non-normal distrubtion
shapiro.test(otter$log_fur)
shapiro.test(otter$log_gut) 
shapiro.test(mink$log_fur) 
shapiro.test(mink$log_gut) 

# Test Variance
leveneTest(data$log_fur~data$Species)
leveneTest(data$log_gut~data$Species)
# Equal variance among groups

##### T Test for difference in Hg between species ####
# Fur Hg
t.test(data$log_fur~data$Species)
wilcox.test(data$log_fur~data$Species)

# Gut Hg
t.test(data$log_gut~data$Species)
wilcox.test(data$log_gut~data$Species)

# Correlation Between Fur and Gut
# Pearson Poduct Moment Correlation
# River Otter
cor.test(otter$log_fur,otter$log_gut)
plot(otter$log_fur,otter$log_gut)

# Mink
cor.test(mink$log_fur,mink$log_gut)
plot(mink$log_fur,mink$log_gut)

########################################################################
#### Regression Model: Gut ~ Fur ####
# Otter
# Null
lm1=lm(otter$log_fur~1)
AICc(lm1) # 13.72451

# Unweighted
lm1.1=lm(otter$log_fur~otter$log_gut)
summary(lm1.1)
AICc(lm1.1) # 10.50199

# Weighted
lm1.2=lm(otter$log_fur~otter$log_gut, weight=otter$bw_gut_ratio)
summary(lm1.2)
AICc(lm1.2) # 26.92151

# Mink
# Null
lm2=lm(mink$log_fur~1)
AICc(lm2) #24.31118

# Unweighted
lm2.1=lm(mink$log_fur~mink$log_gut)
summary(lm2.1)
AICc(lm2.1) # 13.83017

# Weighted
lm2.2=lm(mink$log_fur~mink$log_gut, weight=mink$bw_gut_ratio)
summary(lm2.2)
AICc(lm2.2) #37.1412

# Plot Models 
plot_otter= ggplot(otter, aes(x = log_fur, y = log_gut)) + 
  geom_point(shape = 20, size=3) +
  geom_smooth(method = "lm", mapping = aes(weight = bw_gut_ratio), 
              color = "black", show.legend = FALSE)+
  theme_minimal(base_size=14)+
  labs(x="Log Fur Total Mercury (ppm)", y="Log Stomach Content Total Mercury (ppm)")
  
plot_mink = ggplot(mink, aes(x = log_fur, y = log_gut)) + 
  geom_point(shape = 20, size=3) +
  geom_smooth(method = "lm", mapping = aes(weight = bw_gut_ratio), 
              color = "black", show.legend = FALSE)+
  theme_minimal(base_size = 14)+
  labs(x="Log Fur Total Mercury (ppm)", y="Log Stomach Content Total Mercury (ppm)")

plot1=ggarrange(plot_otter, plot_mink,
                labels = c("A", "B"),
                ncol = 2, nrow = 1, 
                font.label = list(size = 20))
#Plot figures with dpi=300
save_plot("mink_otter_gut_hg.tif", plot1, width = 30, height = 20, dpi = 300)

########################################################################
#### Test for interactions between site and species ####
# Boxplot All Species
ggplot(data, aes(x=as.factor(data$Lat), y=FurHg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  theme_minimal()+
  labs(x="Latitude", y="Fur Hg (ppm)")

#Box plot Mink and river otter seperated
ggplot(data, aes(x=as.factor(data$Lat), y=FurHg, fill=Species)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  theme_minimal()+
  labs(x="Latitude", y="Fur Hg (ppm)")

# ANOVA All interactons
lm1=lm(data$FurHg~data$Species*as.factor(data$Lat))
anova(lm1) # No interaction
TukeyHSD(aov(data$FurHg~data$Species*as.factor(data$Lat)))

#Sites
lm1=lm(data$FurHg~as.factor(data$Lat))
anova(lm1) # No Interaction between site
t1 = TukeyHSD(aov(data$FurHg~as.factor(data$Lat)))
plot(t1)
