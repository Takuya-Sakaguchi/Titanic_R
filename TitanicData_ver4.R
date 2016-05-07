
library(ggplot2)
library(randomForest)

test<-read.csv("test.csv", header = TRUE)
train<-read.csv("train.csv", header = TRUE)

test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

data.combined <- rbind(train, test.survived)


str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)


ggplot(train, aes(x=Pclass, fill=factor(Survived))) +
  geom_histogram(binwidth= 0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")


summary(train$Age)

table(train$SibSp)
table(train$Parch)


temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch<-c(train$Parch, test$Parch)
data.combined$Family.size <- c(temp.SibSp + temp.Parch +1)


n <- aggregate(Survived ~ Family.size, data = slice.train, 
               function(v){c(mean(v), length(v))})
n <- cbind(n[, -2, drop = FALSE], n[, 2])
colnames(n)[2:3] <- c("Survived", "Count")
n[order(n[, "Survived"], decreasing = TRUE), ]


slice.train <- data.combined[1:891, ]
slice.train$Survived <- as.integer(as.character(slice.train$Survived))




titanic.rf <- randomForest(Survived ~ Family.size + Pclass + Sex,
                           data=slice.train, importance=TRUE,proximity=TRUE)


predict(titanic.rf, data.combined[892:nrow(data.combined), ])


round(predict(titanic.rf, data.combined[892:nrow(data.combined), ]), 0)

write.csv(data.frame(data.combined[892:nrow(data.combined), "PassengerId"],
                     round(predict(titanic.rf, data.combined[892:nrow(data.combined), ]), 0)),
          "output_R.csv", row.names = FALSE)



