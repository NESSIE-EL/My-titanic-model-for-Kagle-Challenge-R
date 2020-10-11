# My-titanic-model-for-Kagle-Challenge-R

---
  title: "Titanic Model"
author: "Nesserine ELMEDIOUNI"
output:
  html_document: 
  number_sections:true
toc:true
---
```{r}
titanic.train<-read.csv("../input/train.csv", na.string=FALSE)
titanic.test<-read.csv("../input/test.csv", na.strings=FALSE)
```
#Data Cleanup
#1 Combine two datasets and clean together


#make flag to indidcate the row is from train or test set
```{r}
titanic.train$IsTrainSet<-TRUE
titanic.test$IsTrainSet<-FALSE
View(titanic.train)
View(titanic.test)
names(titanic.train)
names(titanic.test)
titanic.test$Survived<-NA
titanic.full<-rbind(titanic.train,titanic.test)
titanic.full
View(titanic.full)
table(titanic.full$IsTrainSet)
```

#2.Looking for missing values and replace.Dataviz
```{r}
summary(titanic.full)
```
#Embarked 2 missing 

```{r}
table(titanic.full$Embarked)
```
#C _Cherbourg, S-Southampton, Q=Queenstown
#Replace missing embarked with the mode = S

```{r}
titanic.full[titanic.full$Embarked=="",]
titanic.full[titanic.full$Embarked=="","Emabarked"]<-"S"
titanic.full[titanic.full$PassengerId=="62",titanic.full$PassengerId=="830",]
summary(titanic.full)
```
#Age 263 missing
```{r}
table(is.na(titanic.full$Age))
```
#We will replace the median of titanic.full
```{r}
medianAge<-median(titanic.full$Age, na.rm=TRUE)
medianAge
titanic.full[is.na(titanic.full$Age),"Age"]<-medianAge
titanic.full
table(is.na(titanic.full$Age))
```
#Fare 1 missing 
```{r}
table(is.na(titanic.full$Fare))
titanic.full[is.na(titanic.full$Fare),]
medianFare<-median(titanic.full$Fare,na.rm=TRUE)
medianFare
titanic.full[is.na(titanic.full$Fare),"Fare"]<-medianFare
table(is.na(titanic.full$Fare))
```
#3.change the types of the columns if needed
```{r}
str(titanic.full)
titanic.full$PassengerId<-as.factor(titanic.full$PassengerId)
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
str(titanic.full)
```
#Split the dataset into train and test
```{r}
titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,]
nrow(titanic.train)
nrow(titanic.test)
titanic.train$Survived<-as.factor(titanic.train$Survived)
str(titanic.train)
```
#DAta Visualization Train Set
```{r}
library(ggplot2)
g<-ggplot(data=titanic.train,aes(x=Sex,fill=Survived))+geom_bar()
g

g<-ggplot(data=titanic.train,aes(x=Pclass,fill=Survived))+geom_bar()
g

g<-ggplot(data=titanic.train,aes(x=Embarked,fill=Survived))+geom_bar()
g

g<-ggplot(data=titanic.train,aes(x=SibSp,fill=Survived))+geom_bar()+facet_grid(~Sex)
g

g<-ggplot(data=titanic.train, aes(x=Age))+geom_density()
g

g<-ggplot(data=titanic.train, aes(x=Fare))+geom_density()+facet_grid(~Pclass)
g

g<-ggplot(data=titanic.test, aes(x=Pclass, y=Fare, color=Pclass))+geom_boxplot()+geom_jitter()
g

g<-ggplot(data=titanic.test, aes(x=Pclass, y=Fare, 
                                 color=Survived))+
  geom_boxplot()+geom_jitter()+ggtitle("Titanic Survival by Class")
g
```

#Create Predictive Model
```{r}
library("randomForest")
survived.equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.equation
survived.formula<-as.formula(survived.equation)
survived.formula
titanic.model<-randomForest(formula=survived.formula,
                            data=titanic.train,
                            ntree=500,
                            mtry=3,
                            nodesize=0.01 * nrow(titanic.train))

```
#Do predictions and submit on Kaggle
```{r}
Survived<-predict(titanic.model,newdata=titanic.test)
```
#PassengerID Survived
```{r}
PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived
write.csv(output.df,"titanic_kaggle_submission.csv", row.names=FALSE)
```
