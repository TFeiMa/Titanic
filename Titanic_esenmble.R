Titanic_esenmble <- function(){
  library(dplyr)
  library(e1071)
  library(randomForest)
  library(stringr)
  library(mice)#deal with the imputation
  train <- read.csv(file = 'train.csv') #read data
  test <- read.csv(file = 'test.csv')
  full <- bind_rows(train,test)  #combine two data
  full <- select(full,- Cabin) #delete colum for Cabin
  full[1044,'Fare'] <- 27.5
  full$Fare <- scale(full$Fare,center = TRUE,scale = TRUE)
  full$Embarked <- as.factor(full$Embarked)
  
  #提取特征title
  full$title <- sapply(full$Name,function(x){strsplit(x,split = '[,.]')}[[1]][2])
  full$title <- sub(' ','',full$title)
  full$title[full$title %in% c('Capt', 'Don', 'Col','Dr','Rev','Major', 'Sir')] <- 'Mater'
  full$title[full$title %in% c('Dona', 'Lady', 'the Countess','Ms','Mme','Mlle', 'Jonkheer')] <- 'Miss'
  full$title <- as.factor(full$title)
  # 姓氏特征
  full$second_name <- sapply(full$Name,function(x){strsplit(x,split = '[,.]')}[[1]][3])
  full$second_name <- sapply(full$second_name,function(x){sub(' ','',x)})
  full$second_name <- sapply(full$second_name,function(x){gsub("[(*)]","",x)})
  full$second_name <- sapply(full$second_name,function(x){gsub("[\"*\"]","",x)})
  full$second_name <- sapply(full$second_name,function(x){strsplit(x,split = ' ')}[[1]][1])
  
  full$C <- 0
  full$C[full$Embarked == 'C'] <- 1
  full$Q <- 0
  full$Q[full$Embarked == 'Q' ]<- 1
  full$S <- 0
  full$S[full$Embarked == 'S'] <- 1
  
  # Age缺失值的补全
  mice_out <- mice(select(full,Age,Fare,Sex,title),method = 'rf')
  mice_output <- complete(mice_out)
  full$Age <- mice_output$Age
  full$Age <- scale(full$Age,center = TRUE,scale = F)
  #family Size
  full$Family.Num <- full$Parch + full$SibSp + 1
  #family 
  full$FirstName <- sapply(full$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
  full$FamilyId1 <- paste(full$Family.Num,full$FirstName,sep = '')
  full$FamilyId1[full$Family.Num <= 2] <- 'samll'
  full$FamilyId1 <- as.factor(full$FamilyId1)
  
  full$FamilyId2 <- paste(full$Family.Num,full$FirstName,sep = '')
  full$FamilyId2[full$Family.Num <= 3] <- 'samll'
  full$FamilyId2 <- as.factor(full$FamilyId2)
  # ticket特征提取
  full$Tic <- as.factor(str_sub(full$Ticket,-4))
  
  train1 <- full[1:891,]
  test1<- full[892:1309,]
  
  # model 训练
  #SVM
  svm_model<-svm(factor(Survived)~Pclass+Sex+Age+title+Family.Num
                 +Fare+C+Q+S,data = train1)
  svm_prediction <- predict(svm_model,test1)
  svm_Survived <- as.numeric(as.character(svm_prediction))
  #RandomForest
  rf_model <- randomForest(factor(Survived)~Pclass+Sex+title+SibSp+Parch+Family.Num+FamilyId2
                           +C+Q+S+Fare,data = train1,nTree = 1000)
  rf_prediction <- predict(rf_model,test1)
  rf_Survived <- as.numeric(as.character(rf_prediction))
  #CForest
  crf_model <- cforest(factor(Survived)~Pclass+Sex+Fare+title+C+Q+S+FamilyId1+Family.Num,
                      data = train1,
                      controls=cforest_unbiased(ntree=2000, mtry=3))
  crf_prediction <- predict(crf_model, test1, OOB=TRUE, type = "response")
  crf_Survived <- as.numeric(as.character(crf_prediction))
  #预测相加
  Survived <- svm_Survived + rf_Survived + crf_Survived
  
  Survived[Survived < 2] <- 0
  Survived[Survived >= 2] <- 1
  Survived <- data.frame(PassengerId = test$PassengerId,Survived = Survived)
  write.csv(Survived,file = 'Survived.csv',row.names = FALSE)
  Survived_sum <- sum(as.numeric(Survived$Survived))
  return(Survived_sum)
}