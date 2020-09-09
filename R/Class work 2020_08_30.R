library(dplyr)
library(car)

df <- read.csv("~\\DataScience\\data\\SomervilleHappinessSurvey2015.csv", header= TRUE)

mod1 <- glm(D ~ ., data=df, family = "binomial")
summary(mod1)

mod1a = step(mod1, direction="forward")
summary(mod1a)

mod1b = step(mod1, direction="backward")
summary(mod1b)


pred1 <- predict(mod1,type="response")
hist(pred1)
summary(pred1)



coronads <- read.csv("~\\DataScience\\data\\corona_positives.csv", header= TRUE)
coronads$ï..test_date <- as.Date(coronads$ï..test_date)

options(repr.plot.width = 8, repr.plot.height = 8)
plot(coronads)

dts <- ts(coronads$positives, min(coronads$ï..test_date), max(coronads$ï..test_date), 7)
