SQF = read.csv("/Users/boom/Desktop/Data mining/SQF_2016.csv",na.strings=" ",stringsAsFactors = TRUE)

df = SQF[0:(nrow(SQF)-1),]

levels(df$arstoffn) <- c(levels(df$arstoffn),"NOARREST")
df$arstoffn[df$arstmade=='N'] = 'NOARREST' 
df$arstoffn[is.na(df$arstoffn)] = "CPW"

levels(df$sumoffen) <- c(levels(df$sumoffen),"NOSUMMON")
df$sumoffen[df$sumissue=='N'] = 'NOSUMMON'

levels(df$officrid) <- c(levels(df$officrid),"INUNIFORM")
df$officrid[df$offunif=='Y'] = 'INUNIFORM'

levels(df$officrid) <- c(levels(df$offunif),'N')
df$officrid[df$offunif=='N' & is.na(df$officrid)] = 'N'

levels(df$offverb) <- c(levels(df$offverb),"INUNIFORM")
df$offverb[df$offunif=='Y'] = 'INUNIFORM'

levels(df$offverb) <- c(levels(df$offverb),"N")
df$offverb[df$offunif=='N' & is.na(df$offverb)] = 'N'

levels(df$offshld) <- c(levels(df$offshld),"INUNIFORM")
df$offshld[df$offunif=='Y'] = 'INUNIFORM'

levels(df$offshld) <- c(levels(df$offshld),"N")
df$offshld[df$offunif=='N' & is.na(df$offshld)] = 'N'

levels(df$stinter) <- c(levels(df$stinter),"NOTINTERSECTION")
df$stinter[is.na(df$stinter)] = "NOTINTERSECTION"

levels(df$crossst) <- c(levels(df$crossst),"NOTCROSSSTREET")
df$crossst[is.na(df$crossst)] = "NOTCROSSSTREET"

noCoorNull= df[!is.na(df$xcoord),c('xcoord','ycoord','city')]
meanQUEENSx = mean(noCoorNull[noCoorNull$city=='QUEENS',1])
meanQUEENSy =  mean(noCoorNull[noCoorNull$city=='QUEENS',2])
meanSTATENx =  mean(noCoorNull[noCoorNull$city=='STATEN IS',1])
meanSTATENy =  mean(noCoorNull[noCoorNull$city=='STATEN IS',2])
meanBROOKLYNx =  mean(noCoorNull[noCoorNull$city=='BROOKLYN',1])
meanBROOKLYNy =  mean(noCoorNull[noCoorNull$city=='BROOKLYN',2])
meanBRONXx =  mean(noCoorNull[noCoorNull$city=='BRONX',1])
meanBRONXy =  mean(noCoorNull[noCoorNull$city=='BRONX',2])
meanMANHATTANx =  mean(noCoorNull[noCoorNull$city=='MANHATTAN',1])
meanMANHATTANy =  mean(noCoorNull[noCoorNull$city=='MANHATTAN',2])

df$xcoord[is.na(df$xcoord) & df$city=='QUEENS'] = meanQUEENSx 
df$ycoord[is.na(df$ycoord) & df$city=='QUEENS'] = meanQUEENSy
df$xcoord[is.na(df$xcoord) & df$city=='STATEN IS'] = meanSTATENx 
df$ycoord[is.na(df$ycoord) & df$city=='STATEN IS'] = meanSTATENy 
df$xcoord[is.na(df$xcoord) & df$city=='BROOKLYN'] = meanBROOKLYNx 
df$ycoord[is.na(df$ycoord) & df$city=='BROOKLYN'] = meanBROOKLYNy 
df$xcoord[is.na(df$xcoord) & df$city=='BRONX'] = meanBRONXx 
df$ycoord[is.na(df$ycoord) & df$city=='BRONX'] = meanBRONXy 
df$xcoord[is.na(df$xcoord) & df$city=='MANHATTAN'] = meanMANHATTANx 
df$ycoord[is.na(df$ycoord) & df$city=='MANHATTAN'] = meanMANHATTANy

tempForce = (df$pf_baton=='Y' | df$pf_drwep == 'Y' | df$pf_grnd =='Y' | df$pf_hands =='Y' | df$pf_hcuff=='Y' | df$pf_other=='Y'| df$pf_pepsp=='Y' | df$pf_ptwep == 'Y' | df$pf_wall=='Y')
df$isforceuse[tempForce] ='Y'
df$isforceuse[!tempForce] = 'N'
df$isforceuse = factor(df$isforceuse)

levels(df$forceuse) <- c(levels(df$forceuse),"NOFORCE")
df$forceuse[df$isforceuse=='N'] = 'NOFORCE'

colNA = colSums(is.na(df))
df = df[,colNA!=nrow(df)]
df = subset(df, select = -c(stname,addrnum))

modePremname = names(summary(df$premname)[(summary(df$premname)==max(summary(df$premname)))])
df$premname[is.na(df$premname)] = modePremname

modeSector = names(summary(df$sector)[(summary(df$sector)==max(summary(df$sector)))])
df$sector[is.na(df$sector)] = modeSector

modeForceuse = names(summary(df$forceuse[df$isforceuse=='Y'])[(summary(df$forceuse[df$isforceuse=='Y'])==max(summary(df$forceuse[df$isforceuse=='Y'])))])
df$forceuse[is.na(df$forceuse)] = modeForceuse

colNA = colSums(is.na(df))
colNA[colNA>0]

df$weaponfound = df$weaponfound = (df$pistol=='Y' | df$riflshot == 'Y' | df$asltweap =='Y' | df$knifcuti =='Y' | df$machgun=='Y' | df$othrweap=='Y')
df$weaponfound[df$weaponfound] = 'Y'
df$weaponfound[df$weaponfound=='FALSE'] = 'N'
df$weaponfound = factor(df$weaponfound)

df$datestop = as.Date(as.character(df$datestop),format="%m/%d/%Y")
df$day = factor(weekdays(df$datestop),levels = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

df$height = ((df$ht_feet*12) + df$ht_inch) * 2.54

df$weight = df$weight/2.2046
df$bmi = df$weight/((df$height/100)**2)

library(stringr)
minutes = as.numeric(str_sub(as.character(df$timestop),-2,-1))/60
hours = as.numeric(substr(df$timestop,1,nchar(df$timestop)-2))
hours[is.na(hours)] = 0
df$hours = round(hours+minutes,2)

df$age = as.numeric(df$age)
df$height = as.numeric(df$height)

df$age[df$age<=5]  = mean(df$age)

df$age[df$age==6 & df$height >110] = mean(df$age)
df$age[df$age==7 & df$height >135] = mean(df$age)
df$age[df$age==8 & df$height >145] = mean(df$age)
df$age[df$age==9 & df$height >150] = mean(df$age)
df$age[df$age==10 & df$height >155] = mean(df$age)
df$age[df$age==11 & df$height >160] = mean(df$age)
df$age[df$age==12 & df$height >167] = mean(df$age)
df$age[df$age==13 & df$height >175] = mean(df$age)
df$age[df$age==14 & df$height >180] = mean(df$age)
df$age[df$age==15 & df$height >185] = mean(df$age)
df$age = round(df$age)

df$weight[df$weight<30] = mean(df$weight)
df$weight[df$weight>200] = mean(df$weight)
df$height[df$height<30] = mean(df$height)
df$height[df$height>200] = mean(df$height)

#Recalculate BMI
df$bmi = df$weight/((df$height/100)**2)
df$weight[df$bmi>50|df$bmi<15] = mean(df$weight)
df$height[df$bmi>50|df$bmi<15] = mean(df$height)
#Recalculate bmi
df$bmi = df$weight/((df$height/100)**2)

#pf_hands
#pf_wall
#pf_grnd
#pf_drwep
#pf_ptwep
#pf_baton
#pf_hcuff
#pf_pepsp
#pf_other
#NOFORCE
df$forcetype = ""
df$forcetype[df$pf_baton=='Y'] = "Baton"
df$forcetype[df$pf_drwep=='Y'] = paste(df$forcetype[df$pf_drwep=='Y'],"WeaponDrawn")
df$forcetype[df$pf_grnd=='Y'] = paste(df$forcetype[df$pf_grnd=='Y'],"OnGround")
df$forcetype[df$pf_hands=='Y'] = paste(df$forcetype[df$pf_hands=='Y'],"Hand")
df$forcetype[df$pf_hcuff=='Y'] = paste(df$forcetype[df$pf_hcuff=='Y'],"Hancuff")
df$forcetype[df$pf_pepsp=='Y'] = paste(df$forcetype[df$pf_pepsp=='Y'],"PepperSpray")
df$forcetype[df$pf_ptwep=='Y'] = paste(df$forcetype[df$pf_ptwep=='Y'],"WeaponPointed")
df$forcetype[df$pf_wall=='Y'] = paste(df$forcetype[df$pf_wall=='Y'],"Wall")
df$forcetype[df$pf_other=='Y'] = paste(df$forcetype[df$pf_other=='Y'],"Other")
df$forcetype[df$forcetype==""] = "NoForce"

top15=list()
x = 1
for(i in (68:54)){
  top15[x] = names(sort(table(df$forcetype))[i])
  x = x+1
}
df$forcetype[!(df$forcetype %in% top15)] = "Rare"
df$forcetype = factor(df$forcetype)


#
df$forcetype2[df$pf_other=='Y'] = "Other"
df$forcetype2[df$pf_hands=='Y'] = "Hands"
df$forcetype2[df$pf_grnd=='Y'] = "Ground"
df$forcetype2[df$pf_wall=='Y'] = "Wall"
df$forcetype2[df$pf_hcuff=='Y'] = "Hancuff"
df$forcetype2[df$pf_pepsp=='Y'] = "PepperSpray"
df$forcetype2[df$pf_baton=='Y'] = "Baton"
df$forcetype2[df$pf_drwep=='Y'] = "WeaponDraw"
df$forcetype2[df$pf_ptwep=='Y'] = "WeaponPoint"
df$forcetype2[is.na(df$forcetype2)] = "NoForce"
df$forcetype2 = factor(df$forcetype2)




df$hourRound = round(df$hours)





df$suspected[grepl("ROB",df$crimsusp)] = "ROBBERY"
df$suspected[grepl("BU",df$crimsusp)] = "BURGLARY"
df$suspected[grepl("FEL",df$crimsusp)] = "FELONY"
df$suspected[grepl("MI",df$crimsusp)] = "MISD"
#CPW and possension
df$suspected[grepl("CPW",df$crimsusp)|grepl("POS",df$crimsusp)] = "CPW"
df$suspected[grepl("ASS",df$crimsusp)] = "ASSAULT"
df$suspected[is.na(df$suspected)] =df$crimsusp
df$suspected[df$suspected!='FELONY' & df$suspected!= 'MISD' & df$suspected != 'CPW'& df$suspected !='ASSAULT'& df$suspected !='ROBBERY'& df$suspected !='BURGLARY'& df$suspected !='286'& df$suspected !='374'& df$suspected !='266'& df$suspected !='123'] = 'rare' 

df$suspected = as.factor(df$suspected)

df$stopReason[df$cs_bulge=='Y'] = 'bulge'
df$stopReason[df$cs_casng=='Y'] = 'casng'
df$stopReason[df$cs_cloth=='Y'] = 'cloth'
df$stopReason[df$cs_descr=='Y'] = 'descr'
df$stopReason[df$cs_drgtr=='Y'] = 'drgtr'
df$stopReason[df$cs_furtv=='Y'] = 'furtv'
df$stopReason[df$cs_lkout=='Y'] = 'lkout'
df$stopReason[df$cs_objcs=='Y'] = 'objcs'
df$stopReason[df$cs_other=='Y'] = 'other'
df$stopReason[df$cs_vcrim=='Y'] = 'vcrim'


df$rowNumber = 1:nrow(df)
df$pct = as.factor(df$pct)
df$stopReason = as.factor(df$stopReason)
library(e1071)
library(elmNN)
library(randomForest)
library(nnet)
library(rpart)
library(fastAdaboost)
library(caret)
library(C50)
#########################################person is armed#################################################
#select features
fullFeatures1 = df[,c('rowNumber','day','hours','isforceuse','age','sex','arstmade','race','frisked','searched','perobs','perstop','typeofid','weight','height','bmi','stopReason','weaponfound','pct','suspected','forcetype')]
#normalize numbers

fullFeatures1$hours = scale(as.numeric(fullFeatures1$hours))
fullFeatures1$perobs = scale(as.numeric(fullFeatures1$perobs))
fullFeatures1$perstop = scale(as.numeric(fullFeatures1$perstop))
fullFeatures1$age  = scale(fullFeatures1$age)
fullFeatures1$weight = scale(fullFeatures1$weight)
fullFeatures1$height = scale(fullFeatures1$height)
fullFeatures1$bmi = scale(fullFeatures1$bmi)


#create test and train

test = fullFeatures1[fullFeatures1$rowNumber%%10==0,2:ncol(fullFeatures1)]
train = fullFeatures1[fullFeatures1$rowNumber%%10!=0,2:ncol(fullFeatures1)]



bayesModel = naiveBayes(weaponfound~., data = train)
fitBAYES = predict(bayesModel,test)
confusionMatrix(table(fitBAYES,test$weaponfound))

svmModel = svm(weaponfound~., data = train,kernel = 'radial')
fitSVM = predict(svmModel,test)
confusionMatrix(table(fitSVM,test$weaponfound),mode = 'prec_recall')

rfModel = randomForest(weaponfound~., data = train)
fitRF = predict(rfModel,test)
table(fitRF,test$weaponfound)

nnModel = nnet(weaponfound~.,data=train,size = 2,maxit = 200)
fitNN = predict(nnModel,test,type="class")
fitNN = factor(fitNN)
levels(fitNN) = c("N","Y")
confusionMatrix(table(fitNN,test$weaponfound),mode = 'prec_recall')

treeModel = rpart(weaponfound~., data = train)
treeModel<- prune(treeModel, cp=   treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"])
treePercentage = predict(treeModel,test)
fitTREE = ifelse(treePercentage[,1] > treePercentage[,2], "N", "Y")
fitTREE = factor(fitTREE)
levels(fitTREE) = c("N","Y")
confusionMatrix(table(fitTREE,test$weaponfound),mode = 'prec_recall',positive="Y")

##### Stacking f(x) = mode
library(modeest)
vote = data.frame(cbind(fitNN,fitTREE,fitSVM,fitRF,fitBAYES))
rownames(vote) <- seq(length=nrow(vote)) 
modeStack = apply(vote, 1,mfv)
fitStack = ifelse(modeStack==1,"N","Y")
fitStack = factor(fitStack)
confusionMatrix(table(fitStack,test$weaponfound))



#########################################Arrest made#################################################
##Select features
fullFeatures1 = df[,c('rowNumber','day','hours','isforceuse','age','sex','race','frisked','searched','perobs','perstop','typeofid','weight','height','bmi','stopReason','weaponfound','pct','suspected','arstmade','forcetype')]
#Normalize number
fullFeatures1$stopReason = as.factor(fullFeatures1$stopReason)
fullFeatures1$hours = scale(as.numeric(fullFeatures1$hours))
fullFeatures1$perobs = scale(as.numeric(fullFeatures1$perobs))
fullFeatures1$perstop = scale(as.numeric(fullFeatures1$perstop))
fullFeatures1$age  = scale(fullFeatures1$age)
fullFeatures1$weight = scale(fullFeatures1$weight)
fullFeatures1$height = scale(fullFeatures1$height)
fullFeatures1$bmi = scale(fullFeatures1$bmi)


#create test and train
test = fullFeatures1[fullFeatures1$rowNumber%%10==0,2:ncol(fullFeatures1)]
train = fullFeatures1[fullFeatures1$rowNumber%%10!=0,2:ncol(fullFeatures1)]

svmModel = svm(arstmade~., data = train)
fitSVM = predict(svmModel,test)
confusionMatrix(table(fitSVM,test$arstmade))

bayesModel = naiveBayes(arstmade~., data = train)
fitBAYES = predict(bayesModel,test)
confusionMatrix(table(fitBAYES,test$arstmade))

rfModel = randomForest(arstmade~., data = train)
#rfModel2 <- train(arstmade~., data=train, method = "xgbTree")

rfitRF = predict(rfModel,test)
confusionMatrix(table(fitRF,test$arstmade),positive = "Y",mode = 'prec_recall')

nnModel = nnet(arstmade~.,data=train,size = 5,itmax = 200)
fitNN = predict(nnModel,test,type="class")
confusionMatrix(table(fitNN,test$arstmade),positive = "N",mode = 'prec_recall')

treeModel = rpart(arstmade~., data = train)
treeModel<- prune(treeModel, cp=   treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"])
fitTREE = predict(treeModel,test,type="class")
levels(fitTREE) = c("N","Y")
confusionMatrix(table(fitTREE,test$arstmade))


##### Stacking f(x) = mode
library(modeest)
vote = data.frame(cbind(fitNN,fitTREE,fitSVM,fitRF,fitBAYES))
vote$fitNN = as.character(vote$fitNN)
vote$fitNN[vote$fitNN=="N"] = 1
vote$fitNN[vote$fitNN=="Y"] = 2
vote$fitNN = as.numeric(vote$fitNN)
rownames(vote) <- seq(length=nrow(vote)) 
modeStack = apply(vote, 1,mfv)
fitStack = ifelse(modeStack==1,"N","Y")
fitStack = factor(fitStack)
confusionMatrix(table(fitStack,test$arstmade),positive="Y",mode = 'prec_recall')



#########################################Force use type#################################################
##Select features
fullFeatures1 = df[,c('rowNumber','day','hours','age','sex','arstmade','race','frisked','searched','perobs','perstop','pct','typeofid','weight','height','bmi','stopReason','weaponfound','suspected','forcetype')]
#Normalize number
fullFeatures1$stopReason = as.factor(fullFeatures1$stopReason)
fullFeatures1$hours = scale(as.numeric(fullFeatures1$hours))
fullFeatures1$perobs = scale(as.numeric(fullFeatures1$perobs))
fullFeatures1$perstop = scale(as.numeric(fullFeatures1$perstop))
fullFeatures1$age  = scale(fullFeatures1$age)
fullFeatures1$weight = scale(fullFeatures1$weight)
fullFeatures1$height = scale(fullFeatures1$height)
fullFeatures1$bmi = scale(fullFeatures1$bmi)


#create test and train
test = fullFeatures1[fullFeatures1$rowNumber%%10==0,2:ncol(fullFeatures1)]
train = fullFeatures1[fullFeatures1$rowNumber%%10!=0,2:ncol(fullFeatures1)]

svmModel = svm(forcetype~., data = train)
fitSVM = predict(svmModel,test,type="class")
levels(fitSVM) = levels(train$forcetype)
confusionMatrix(table(fitSVM,test$forcetype),mode = 'prec_recall')

bayesModel = naiveBayes(forcetype~., data = train)
fitBAYES = predict(bayesModel,test)
confusionMatrix(table(fitBAYES,test$forcetype),mode = 'prec_recall')

rfModel = randomForest(forcetype~., data = train)
fitRF = predict(rfModel,test)
confusionMatrix(table(fitRF,test$forcetype))

nnModel = nnet(forcetype~.,data=train,size = 5,itmax = 300)
fitNN = predict(nnModel,test,type='class')
fitNN = as.factor(fitNN)
levels(fitNN) = levels(df$forcetype)
confusionMatrix(table(fitNN,test$forcetype),mode = 'prec_recall')

treeModel = rpart(forcetype~., data = train)
treeModel<- prune(treeModel, cp=   treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"])
fitTREE = predict(treeModel,test,type="class")
levels(fitTREE) = c("N","Y")
confusionMatrix(table(fitTREE,test$forcetype))

c50Model = C5.0(forcetype~., data = train)
fitC50 = predict(c50Model,test,type="class")
confusionMatrix(table(fitC50,test$forcetype),mode = 'prec_recall')

 ##### Stacking f(x) = mode
library(modeest)
vote = data.frame(cbind(fitNN,fitTREE,fitSVM,fitRF,fitBAYES))
rownames(vote) <- seq(length=nrow(vote)) 
modeStack = apply(vote, 1,mfv)
fitStack = ifelse(modeStack==1,"N","Y")
fitStack = factor(fitStack)
confusionMatrix(table(fitStack,test$arstmade))



