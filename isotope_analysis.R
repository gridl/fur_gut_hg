########################################################################
# Fur ~Gut: Isotopes
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
# Regression Gut and stable isotopes
lm3=lm(log_fur~Delta_13C+Delta_15N, data=data)
summary(lm3)
bptest(lm3)
dwtest(lm3)
resettest(lm3)
shapiro.test(resid(lm3))
# Meets all model assumptions

lm4=lm(log_gut~Delta_13C+Delta_15N, data=data, weight=bw_gut_ratio)
summary(lm4)

lm4.1=lm(log_gut~Delta_15N, data=data, weight=bw_gut_ratio)
summary(lm4.1)

bptest(lm4.1)
ncvTest(lm4.1) # Volated
resettest(lm4.1) # Violated
shapiro.test(resid(lm4.1)) # Violated

cor(data$Mass_Fraction_N,data$Delta_15N, use="complete.obs")

########################################################################

# Plot
# delta 13C~ delta 15N
plot_fur=ggplot(data, aes(x=data$Delta_13C, y=data$Delta_15N)) +
  geom_point(aes(colour = factor(Species), size = data$FurHg))+
  labs(size = "Fur Hg (ppm)")+
  scale_color_manual(name="Species", values=c("#C0C0C0","#696969"))+
  labs(x = "Delta 13C", y= "Delta 15N")+
  theme_minimal(base_size=16)

plot_gut=ggplot(data, aes(x=data$Delta_13C, y=data$Delta_15N)) +
  geom_point(aes(colour = factor(Species), size = data$GutHg))+
  labs(size = "Gut Hg (ppm)")+
  scale_color_manual(name="Species", values=c("#C0C0C0","#696969"))+
  labs(x = "Delta 13C", y= "Delta 15N")+
  theme_minimal(base_size=16)

plot2=ggarrange(plot_fur, plot_gut,
                labels = c("A", "B"),
                ncol = 2, nrow = 1, 
                font.label = list(size = 20))

# Plot figures with dpi=300
save_plot("isotopes_gut_hg.tif", plot2, width = 30, height = 15, dpi = 300)

# Difference in Isotopes between groups
t.test(data$Delta_13C~data$Species)
t.test(data$Delta_15N~data$Species)

