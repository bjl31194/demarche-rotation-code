install.packages("ggpmisc")
install.packages("car")
library(car)
library(ggpmisc)
library(ggplot2)
library(tidyverse)

arabia_data <- read.csv("~/Documents/UGA/UGA_Fall 2021/DeMarche-rotation/Ben-arabia-data.csv")

#melt data
arabia_data_melted <- pivot_longer(arabia_data, cols = ends_with("cover"), names_to = "Species", values_to = "Percent.Cover")

#determine occupancy
for(i in 1:nrow(arabia_data_melted)) {
  if(arabia_data_melted[i,"Percent.Cover"] > 0) {
    arabia_data_melted[i,"Occupation"] <- 1
  }
  else {
    arabia_data_melted[i,"Occupation"] <- 0
  }
}

#clean up names
arabia_data_melted[arabia_data_melted == "endemic.percent.cover"] <- "Endemic"
arabia_data_melted[arabia_data_melted == "non.endemic.percent.cover"] <- "Non-endemic"

#fit linear models relating pool occupation and percent cover to distance to edge for both species
mod1 <- glm(Occupation ~ distance.to.edge * Species, data = arabia_data_melted, family = "binomial")

mod1_stats <- summary(mod1)

mod1_anova <- Anova(mod1)

arabia_data_occupied <- subset(arabia_data_melted, arabia_data_melted$Occupation != 0)

mod2 <- lm(Percent.Cover ~ distance.to.edge * Species, data = arabia_data_occupied)

mod2_stats <- summary(mod2)

mod2_anova <- Anova(mod2)

#plot w/ linear regression
ggplot(arabia_data_occupied, aes(x = distance.to.edge, y = Percent.Cover, Fill = Species)) +
  geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(aes(color = Species, fill = Species), method = "lm", formula = y~x) +
  stat_poly_eq(formula = y~x)

ggplot(arabia_data_melted, aes(x = distance.to.edge, y = Occupation, Fill = Species)) +
  geom_point(aes(color = Species, shape = Species)) +
  geom_smooth(aes(color = Species, fill = Species), method = "glm", formula = y~x, method.args = list(family = "binomial")) +
  stat_poly_eq(formula = y~x)
 
#plot(mod2)
  







