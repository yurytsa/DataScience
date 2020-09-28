#########################
### ABO Blood Groups
#########################

# In this notebook we will analyze the world distribution of the different blood groups.
# 
# On the surface of the red blood cells (also known as erythrocytes) are expressed molecules 
# that are determined on the genetic code of individuals. Among those molecules are two 
# important ones, that determinate the blood type. The implication of those types is that 
# individuals needing a blood transfussion can not ever receive blood from any other 
# individual. Many people have to receive only the same type they have. Also, pregnant 
# women that their babies have an incompatible blood type with their mothers are at risk of 
# abortion or can generate a dangerous reaction on the mother, that may even cause the risk 
# of death on the mother. Thus, the importance of testing the blood type on all the 
# population.
# 
# There are four groups that are responsible of the blood compatibility:
# 
# -Group O: Is the most common blood group. People with this blood group can recieve blood 
# only from the same type.
# -Group A: Is the second most frequently found group. People with this group can recieve 
# from A and O groups.
# -Group B: Is the third type in frequency. People with this group can recieve from B and O 
# groups.
# -Group AB: Is the least type in frequency. People with this group can recieve blood from
# any type (A,B, and O).
# 
# Another molecule present in the red blood cells and which determines the compatibility of 
# the blood is known as the Rhesus D factor . This factor was discovered in monkies (from 
# here the name). When this factor is present we say that the blood type is Rh-positive (Rh+). \
# When it is absent is considered Rh-negative (Rh-).
# 
# In summary, the combination of the ABO types and the Rhesus D factor results in Eight 
# different possible blood types: O+, A+, B+, AB+, O-, A-, B-, AB-.
# 
# In the present dataset we have the distribution of the eight blood types by country. 
# We will analyze this data using the statistical knowledge we have learned.


if(!require(ggfortify)) install.packages("ggfortify")
library(dplyr)

path <- "../Data/"  ### update this to reflect your local paht !!!!
abo <- read.csv(paste(path,"blood_groups_world_distribution.csv",sep=""))
names(abo) <- c("Country","Population","O+","A+","B+","AB+","O-","A-","B-","AB-")

head(abo)

summary(abo)


abo$O <- abo$`O+` + abo$`O-`
abo$A <- abo$`A+` + abo$`A-`
abo$B <- abo$`B+` + abo$`B-`
abo$AB <- abo$`AB+` + abo$`AB-`

##############################################
# Please Answer to the following questions:
##############################################

#  1. Which is the most common blood type: O+ or O-?
o_plus_avg <- sum(abo$`O+` * abo$Population) / sum(abo$Population)
o_minus_avg <- sum(abo$`O-` * abo$Population) / sum(abo$Population)

# Answer: O+

#  2. On how many countries the most common blood type is A?

abo[, "most_common_pct"] <- apply(abo[, 11:14], 1, max)
library(dplyr)

countries_with_most_common_A <- filter(abo, A == most_common_pct)
# Answer: 34


#  3. Show the five countries with the higher percentage of AB types.

countries_with_most_common_AB <- arrange(abo, desc(AB))
countries_with_most_common_AB <- slice_head(countries_with_most_common_AB, n = 5) 
countries_with_most_common_AB$Country

# Answer: "North Korea" "South Korea" "Pakistan" "Japan"  "Bangladesh"


#  4. How many people (and which percent of the total world population) has a negative Rh?

abo$'RH-' <- abo$`O-` + abo$`A-` + abo$`B-` + abo$`AB-`
abo$'RH-ABS' <-  abo$Population * (abo$`O-` + abo$`A-` + abo$`B-` + abo$`AB-`) 
total_VH_negative = sum(abo$`RH-ABS`)
total_VH_negative_pct = round((total_VH_negative / sum(abo$Population)),3)

# Answer: 6%

#  5. Is there a correlation between the distribution of Israel and the US?

israel_us <- abo %>% filter(Country == 'Israel' | Country == 'United States') 
israel_us <- israel_us[,1:10]
israel_us <- subset(israel_us, select = -c(Population))

library(data.table)
library("ggplot2")
groups_names = names(israel_us)
israel_us_trans <- transpose(israel_us)
israel_us_trans$group = groups_names
israel_us_trans <- israel_us_trans[2:9,]

ggplot(data=israel_us_trans, aes(x=group, y=V1)) + geom_bar(stat="identity")
ggplot(data=israel_us_trans, aes(x=group, y=V2)) + geom_bar(stat="identity")

cor.test(as.numeric(israel_us_trans$V1, israel_us_trans$V2), as.numeric(us), method = "spearman",na.rm=T)

# Answer: yes


#  6. Is there a statistical significant difference between the distribution of Israel and Russia?

israel_rus <- abo %>% filter(Country == 'Israel' | Country == 'Russia')
israel_rus <- israel_rus[,1:10]
israel_rus <- subset(israel_rus, select = -c(Population))
groups_names <- names(israel_rus)
israel_rus_trans <- transpose(israel_rus)
israel_rus_trans <- israel_rus_trans[2:9,]
cor.test(as.numeric(israel_rus_trans$V1, israel_rus_trans$V2), as.numeric(us), method = "spearman",na.rm=T)
# Answer: no


#  7. Use different clustering techniques to classify the distribution of the eight blood cell types. Which cluster give a more logical segmentation?

#### Note:
#  Use the NbClust package to calculate the optimal number of clusters. Also use plots coloring the groups with the resulting clusters. You can use the autoplot function (ggfortify package) to see the cluster distribution. Pass the Country values as rownames so you can see each country on the graphs.


library(countrycode)
abo$continent <- countrycode(sourcevar = as.vector(abo$Country),
                            origin = "country.name",
                            destination = "continent")


kmod <- kmeans(as.matrix(abo[,3:10]),centers=5)
table(kmod$cluster)
kmod$centers
kmod$tot.withinss

plot(abo$`O+`, abo$`O-`)
plot(abo$`O+`, abo$`O-`, col=kmod$cluster)

library(plyr)
count(abo, "continent")
abo %>% group_by(continent) %>% select(continent, count())

library(NbClust)
res <- NbClust(abo[,3:10], distance = "euclidean", min.nc=2, max.nc=8, method = "complete")
kmod <- kmeans(as.matrix(abo[,3:10]),centers=3)

table(kmod$cluster)
kmod$centers
kmod$tot.withinss

plot(abo$`O`, abo$`A`, col=kmod$cluster)    



#  8. Analyze the dataset using Principal Component Analysis (PCA).


