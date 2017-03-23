Titanic_RF <- function(){
  library(dplyr)
  library(randomForest)
  library(party)
  library(mice)#deal with the imputation
  train <- read.csv(file = 'train.csv') #read data
  test <- read.csv(file = 'test.csv')
  full <- bind_rows(train,test)  #combine two data
  
  #Cabin
  # full$Cabin <- as.character(full$Cabin)
  # full$Cabin[full$Cabin == ''] <- 'N'
  # # full$Cabin <- substr(full$Cabin,1,1)
  # Cabin_s <- train$Cabin[train$Survived == 1] # have Survived
  # Cabin_ns <- train$Cabin[train$Survived == 0] # have UnSurvived
  # Cabin_s_o <- Cabin_s[!Cabin_s %in% Cabin_ns] # only Survived
  # Cabin_ns_o <- Cabin_ns[!Cabin_ns %in% Cabin_s]# only Survived
  # Cabin_unkonw <- train$Cabin[train$Cabin %in% Cabin_ns & train$Cabin %in% Cabin_s] #have S and US
  # full$Cabin[full$Cabin %in% Cabin_s_o] <- 'S'
  # full$Cabin[full$Cabin %in% Cabin_ns_o] <- 'NS'
  # full$Cabin[full$Cabin %in% Cabin_unkonw] <- 'NKnow'
  full$Cabin <- as.factor(full$Cabin)
  # full <- select(full,- Cabin) #delete colum for Cabin
  
  # m <- na.omit(full)%>%       #compute mean of Fare group by Embarked
  #   + group_by(Embarked)%>%
  #   + summarize(mean(Fare))
  full[1044,'Fare'] <- 27.5
  
  # mice_out <- mice(select(full,Age,Fare,Sex,Embarked),method = 'rf')
  # mice_output <- complete(mice_out)
  # full$Age <- mice_output$Age
  # full$Age[is.na(full$Age)] <- 28
  #family Size
  full$FamilySize <- full$Parch + full$SibSp + 1
  # title
  full$title <- sapply(full$Name,function(x){strsplit(x,split = '[,.]')}[[1]][2])
  full$title <- sub(' ','',full$title)
  full$title[full$title %in% c('Capt', 'Don', 'Col','Dr','Rev','Major', 'Sir','Jonkheer')] <- 'Mater'
  full$title[full$title %in% c('Dona', 'Lady', 'the Countess','Ms','Mme','Mlle')] <- 'Miss'
  full$title <- as.factor(full$title)
  #family 
  full$FirstName <- sapply(full$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
  full$Family <- paste(full$FamilySize,full$FirstName,sep = '')
  full$Family[full$FamilySize <= 2] <- 'samll'
  full$Family <- as.factor(full$Family)
  #The Embarked
  full$C <- 0
  full$C[full$Embarked == 'C'] <- 1
  full$Q <- 0
  full$Q[full$Embarked == 'Q' ]<- 1
  full$S <- 0
  full$S[full$Embarked == 'S'] <- 1
  full$Embarked <- as.factor(full$Embarked)
  
  full <- select(full,Survived,Pclass,Sex,Fare,title,Family,title,FamilySize,Cabin,Embarked,Age)
 
  train1 <- full[1:891,]
  test1<- full[892:1309,]
  ###############
  # The model of randomForest
  # rf_model <- randomForest(factor(Survived)~Pclass+Sex+Fare+title+C+Q+S+FamilyId+Family.Num,
  #                          data = train1,nTree = 2000)
  #rf_prediction <- predict(rf_model,test1)
  
  rf_model <- cforest(factor(Survived)~.,
                      data = train1,
                 controls=cforest_unbiased(ntree=2000, mtry=3))
  rf_prediction <- predict(rf_model, test1, OOB=TRUE, type = "response")
  Survived <- data.frame(PassengerId = test$PassengerId,
                          Survived = rf_prediction)
  write.csv(Survived,file = 'Survived.csv',row.names = FALSE)
  return(rf_model)
  ######################3
  
}
  