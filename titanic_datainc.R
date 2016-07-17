#read train/test data
train<-read.csv("../input/train.csv",na.strings=c('NA',''),stringsAsFactors=F)
test<-read.csv("../input/test.csv",na.strings=c('NA',''),stringsAsFactors=F)

#checking the missing data
#library(Amelia) #seems Amelia not installed in server
#missmap(train,col=c('yellow','black'),main='Titanic Train Data',legend=F)
check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))

#combine train/test data for pre-processing
train$Cat<-'train'
test$Cat<-'test'
test$Survived<-NA
full<-rbind(train,test)

#Embarked
full$Embarked[is.na(full$Embarked)]<-'S'

#Extract Title from Name
full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
aggregate(Age~Title,full,median)
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Adding FamilySize
full$FamilySize<-full$Parch+full$SibSp+1

#Fare
#simply replace the one missing Fare data with median, due to skewed distribution of Fare
full$Fare[is.na(full$Fare)]<-median(full$Fare,na.rm=T)

#Age
#decision tree (regression) method to predict missing Age data
library(rpart)
fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,data=full[!is.na(full$Age),],method='anova')
full$Age[is.na(full$Age)]<-predict(fit.Age,full[is.na(full$Age),])

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1
#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=18]<-1

#FamilyId2
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$FamilySize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Exact Deck from Cabin number
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])
#deck.fit<-rpart(Deck~Pclass+Fare,data=full[!is.na(full$Deck),])
#full$Deck[is.na(full$Deck)]<-as.character(predict(deck.fit,full[is.na(full$Deck),],type='class'))
#full$Deck[is.na(full$Deck)]<-'UNK'

#Excat Position from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL
#side.train<-full[!is.na(full$Side),]
#side.test<-full[is.na(full$Side),]
#side.fit<-rpart(Side~FamilyId+FamilySize,side.train,method='class')
#full$Side[is.na(full$Side)]<-as.character(predict(side.fit,side.test,type='class'))

#factorize the categorical variables
full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                Deck=factor(Deck)
                )

#split train/test data
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)

#randomForest method, (not support variables with too many levels, e.g. FamilyId here)
library(randomForest)
fit.rf<-randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,data=train,ntree=100,importance=T)
plot(fit.rf,main='randomForest error rate')
imp<-importance(fit.rf,type='1')
imp<-imp[order(imp),]
barplot(imp,las=1,cex.names=0.8,col='blue',horiz=T)

#cforest (conditional inference tree) method, (support variables with more levels and missing values, with unbiased prediction)
library(party)
fit<-cforest(Survived~FamilyId2+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child+Deck,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))

#write submission with cforest method
test$Survived<-predict(fit,test,OOB=TRUE,type='response')
submission<-test[,1:2]
write.csv(submission,'submission.csv',row.names=F)