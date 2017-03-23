Titanic_NB <- function(){
  library(dplyr)
  library(e1071)
  library(mice)#deal with the imputation
  train <- read.csv(file = 'train.csv') #read data
  test <- read.csv(file = 'test.csv')
  full <- bind_rows(train,test)  #combine two data
  full <- select(full,- Cabin) #delete colum for Cabin
  #m <- na.omit(full)%>%       #compute mean of Fare group by Embarked
  # + group_by(Embarked)%>%
  # + summarize(mean(Fare))
  full[1044,'Fare'] <- 27.5 
  
  full$C <- 0
  full$C[full$Embarked == 'C'] <- 1
  full$Q <- 0
  full$Q[full$Embarked == 'Q' ]<- 1
  full$S <- 0
  full$S[full$Embarked == 'S'] <- 1
  
  full$Family.Num <- full$Parch + full$SibSp + 1
  
  full$title <- sapply(full$Name,function(x){strsplit(x,split = '[,.]')}[[1]][2])
  full$title <- sub(' ','',full$title)
  full$title[full$title %in% c('Capt', 'Don', 'Col','Dr','Rev','Major', 'Sir')] <- 'Mater'
  full$title[full$title %in% c('Dona', 'Lady', 'the Countess','Ms','Mme','Mlle', 'Jonkheer')] <- 'Miss'
  
  full$FirstName <- sapply(full$Name,FUN = function(x){strsplit(x,split = '[,.]')[[1]][1]})
  full$FamilyId <- paste(full$Family.Num,full$FirstName,sep = '')
  full$FamilyId[full$Family.Num <= 3] <- 'samll'
  full$FamilyId <- as.factor(full$FamilyId)
  
  #the age
  mice_out <- mice(select(full,Age,Fare,Sex,title),method = 'rf')
  mice_output <- complete(mice_out)
  full$Age <- mice_output$Age
  # full$Embarked <- as.factor(full$Embarked)
  
  train1 <- full[1:891,]
  test1<- full[892:1309,]
  nb_model <- naiveBayes(factor(Survived)~Pclass+Sex+Age+title+C+Q+S+Family.Num+FamilyId
                         +Fare,data = train1)
  nb_prediction <- predict(nb_model,test1)
  Survived <- data.frame(PassengerId = test$PassengerId,
                         Survived = nb_prediction)
  write.csv(Survived,file = 'Survived1.csv',row.names = FALSE)
  
}