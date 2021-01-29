library(readr)
library(dplyr)


df <- read_csv(file = "creditcard.csv")

summary(df)

table(df$Class)

### divide into train-test
mechkar::train_test(data=df, train_name="train", test_name="test", prop=0.7, seed=2, tableone=T)

table(train$Class)/nrow(train)
table(test$Class)/nrow(test)

#### k-means with 1000 clusters
kmod <- kmeans(train[,1:31],centers = 1000)
ktab <- data.frame(table(clust=kmod$cluster, class=train$Class))

#### select clusters having at least one minority class (1)
ktab %>% filter(class==1 & Freq >0) %>% select(clust) %>% tally()

clust <- ktab %>% filter(class==1 & Freq >0) %>% select(clust) 

train$cluster <- kmod$cluster

train2 <- train %>% filter(cluster %in% as.numeric(clust$clust))
summary(train2)
summary(kmod$centers)

#### run logistic regression model
mod4 <- glm(Class ~., data=train2, family="binomial")
summary(mod4)
pred4 <- predict(mod4,newdata=test,type="response")
hist(pred4)
table(obs=test$Class, pred=ifelse(pred4>0.5,1,0))

#### run random forest model
mod5 <- ranger::ranger(Class ~., data=train2)
mod5
pred5 <- predict(mod5,data=test)
hist(pred5$predictions)
table(obs=test$Class, pred=ifelse(pred5$predictions>0.5,1,0))
(85304+110)/(85304+110+18+11)

### AUC
roc1 <- pROC::roc(test$Class, pred5$predictions)
roc1

### Predict on whole dataset
pred5a <- predict(mod5,data=df)
hist(pred5a$predictions)
table(obs=df$Class, pred=ifelse(pred5a$predictions>0.5,1,0))
(284284+453)/(284284+453+39+31)

roc1 <- pROC::roc(df$Class, pred5a$predictions)
roc1

acc <- ROSE::accuracy.meas(df$Class, pred5a$predictions)
acc$threshold
acc$precision
acc$recall
acc$F
