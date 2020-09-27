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



##############################################
# Please Answer to the following questions:
##############################################

#  1. Which is the most common blood type: O+ or O-?


#  2. On how many countries the most common blood type is A?


#  3. Show the five countries with the higher percentage of AB types.

#  4. How many people (and which percent of the total world population) has a negative Rh?

#  5. Is there a correlation between the distribution of Israel and the US?

#  6. Is there a statistical significant difference between the distribution of Israel and Russia?

#  7. Use different clustering techniques to classify the distribution of the eight blood cell types. Which cluster give a more logical segmentation?

#### Note:
#  Use the NbClust package to calculate the optimal number of clusters. Also use plots coloring the groups with the resulting clusters. You can use the autoplot function (ggfortify package) to see the cluster distribution. Pass the Country values as rownames so you can see each country on the graphs.


#  8. Analyze the dataset using Principal Component Analysis (PCA).











