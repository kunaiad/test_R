zoo <- read.csv("zoo2.csv", fileEncoding="euc-kr",header = TRUE,stringsAsFactors = F)
zoo

table(zoo)

normalize<-function(x){ return ((x-min(x))/ (max(x)-min(x))) }


zoo_n <- as.data.frame(lapply(zoo[-1], normalize ))
zoo_n <- as.data.frame(lapply(zoo_n[-17],normalize))
zoo_n


set.seed(1)
train <- round(0.7*dim(zoo)[1])
zoo_index = sample(1:dim(zoo)[1], train, replace =F)
zoo_train <- zoo_n[train_index,]
zoo_test <- zoo_n[-train_index,]
zoo_test
zoo_train_label <- zoo[zoo_index,5]
zoo_test_label <- zoo[-zoo_index,5]
prop.table(table(zoo_train_label))
prop.table(table(zoo_test_label))
zoo_test_pred <- knn(train=zoo_train, test=zoo_test, cl=zoo_train_label, k=3)


mapply(median,zoo[-1],na.rm=TRUE)



Zoo_scaled <- cbind(as.data.frame(scale(Zoo[,-17])), type = Zoo[,17])
knnFit <- train(type ~ ., method = "knn", data = Zoo_scaled,
                tuneLength = 5,  tuneGrid=data.frame(k=1:10),
                trControl = trainControl(
                  method = "cv", indexOut = train))




#실습예제1
x1 <-read.csv("email1.csv ",header=T,stringsAsFactors=FALSE)
str(x1)


x1$type <- factor(x1$type,order=TRUE, levels=c("spam","ham"))
x1$viagra <-factor(x1$viagra,order=TRUE, levels=c("yes","no"))
str(x1)

#library(gmodels)
CrossTable(x=x1$type, y=x1$viagra)

#실습예제2
x2 <-read.csv("email2.csv ",header=T,stringsAsFactors=FALSE)
str(x2)

CrossTable(x=x2$type, y=x2$viagra)
CrossTable(x=x2$type, y=x2$coupon)


x2

length(grep('yes',x2[,3]))


movie <-read.csv("movie (1).csv ",header=T)

install.packages("e1071")
library(e1071)
movie
nm <- naiveBayes(movie[1:5],movie$장르,laplace=0)
nm

res <- predict(nm,movie[1:5])
res
cbind(movie,res)


zoo2 <- read.csv("zoo2.csv ",stringsAsFactors = F,header=FALSE)
zoo22
str(zoo2)
zoo2 <- as.data.frame(lapply(zoo2,as.factor))

zoo2$X1 <- as.factor(zoo2$X1)
zoo2$X0 <- as.factor(zoo2$X0)
zoo2$X0.1 <- as.factor(zoo2$X0.1)
zoo2$X1.1 <- as.factor(zoo2$X1.1)
zoo2$X0.2 <- as.factor(zoo2$X0.2)
zoo2$X0.3 <- as.factor(zoo2$X0.3)
zoo2$X1.2 <- as.factor(zoo2$X1.2)
zoo2$X1.3 <- as.factor(zoo2$X1.3)
zoo2$X1.4 <- as.factor(zoo2$X1.4)
zoo2$X1.5 <- as.factor(zoo2$X1.5)
zoo2$X0.4 <- as.factor(zoo2$X0.4)
zoo2$X0.5 <- as.factor(zoo2$X0.5)
zoo2$X0.6 <- as.factor(zoo2$X0.6)
zoo2$X0.7 <- as.factor(zoo2$X0.7)
zoo2$X1.6 <- as.factor(zoo2$X1.6)
zoo2$X1.7 <- as.factor(zoo2$X1.7)

nm <- naiveBayes(zoo2[1:17],zoo2$X1.7,laplace=0)
nm
res <- predict(nm,zoo2[1:17])
cbind(zoo2,res)

zoo2[100,]
str(zoo)
m <- naiveBayes(zoo2[-1],zoo2$V18,laplace = 0)
m
p<-predict(m,zoo2,type='class')
p

str(zoo2)
