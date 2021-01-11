
library(readr)
mammalsleep <- read_csv("DataScience/data/mammalsleep.csv")
View(mammalsleep)


#1 - Summary about dataframe

summary(mammalsleep)

#2 - Change character to FACTOR (space issue + many functions get only factor as param)
mammalsleep$species <- factor(mammalsleep$species)


#3 - corellation between two variables (watch p-value in test response)

cor(mammalsleep$bw, mammalsleep$brw, method = "spearman")
cor.test(mammalsleep$bw, mammalsleep$brw, method = "spearman") 


#4 - Basic cprellation matrix 
library(Hmisc)
rcorr(as.matrix(mammalsleep[,3:9]),type = "spearman")


#5 - show distribution of a factor : every value with frequency (one vector ot two vectors)
table(mammalsleep$pi)
table(odi=mammalsleep$odi, sei=mammalsleep$sei)

#6 - ggplot library, plots and correlation matrix visual

library(ggplot2)

ggplot(data=mammalsleep) + geom_bar(aes(x=higrisk))

ggplot(data=mammalsleep) +
  geom_bar(aes(x=odi, group=factor(higrisk), color=factor(higrisk)))

plot(mammalsleep$bw ~ mammalsleep$brw, xlim=c(0,1000), ylim=c(0,700))
plot(log(mammalsleep$bw+1) ~ log(mammalsleep$brw+1))

hist(mammalsleep$bw,breaks = 40)
boxplot(mammalsleep$bw)

hist(log(mammalsleep$bw))
boxplot(log(mammalsleep$bw))

boxplot(log(mammalsleep$bw) ~ mammalsleep$odi)


ggplot(data=mammalsleep) +
  geom_density(aes(x=log(bw), group=odi,color=odi))

pairs(mammalsleep[,3:13])




