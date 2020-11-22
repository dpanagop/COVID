library(openxlsx)
library(ggplot2)
# SEt date locale to english
Sys.setlocale("LC_TIME", "US")

### Loading and preprocessing data ####
# Load data
data = read.xlsx(xlsxFile = "deaths_ventilator_20201120.xlsx", sheet = 1, skipEmptyRows = FALSE)
# order data from lattest to newest date
data=data[order(data$timestamp),]
# create a date column
data$date=substr(data$timestamp,1,10)
data$date=as.Date(data$date)

### Plot data
gr=ggplot(data)+
  geom_point(aes(x=date,y=deaths))+
  labs(title="Plot of COVID deaths by date",
       x ="date", y = "deaths")
gr
gr=ggplot(data)+
  geom_point(aes(x=date,y=ventilator))+
  labs(title="Plot of COVID patients in ventilator by date",
       x ="date", y = "pattients")
gr

# Add columns ventilator_1, ventilator_2,..., ventilator_20 with 
# number of patients in ventilator 1, 2,..., 20 days before
rows=nrow(data)
for (i in 1:20){
  name=paste0("ventilator_",i)
  data[[name]]=0
  data[[name]][(i+1):rows]=data$ventilator[1:(rows-i)]
}
# Add columns deaths_1, deaths_2,..., deaths_20 with 
# number of deaths 1, 2,..., 20 days before
for (i in 1:20){
  name=paste0("deaths_",i)
  data[[name]]=0
  data[[name]][(i+1):rows]=data$deaths[1:(rows-i)]
}

# Keep rows with complete columns and
# Drop index column, url, title, timestapm and repsirator 
model_data=data[21:nrow(data),]
model_data=model_data[,-c(1:4,7)]

# Split into 80%-20% train test datasets
set.seed(2020)
idx=sample(1:nrow(model_data), 0.8*nrow(model_data))
train=model_data[idx,]
test=model_data[-idx,]



### Models ####
## Linear model
# train
lm_model_deaths=lm(deaths~.,train[,-1])
lm_model_ventilator=lm(ventilator~.,train[,-2])
# summary
summary(lm_model_deaths)
summary(lm_model_ventilator)
# test
lm_predictions_deaths=predict(lm_model_deaths,test)
lm_predictions_ventilator=predict(lm_model_ventilator,test)
mean((test$deaths-lm_predictions_deaths)^2)
mean((test$ventilator-lm_predictions_ventilator)^2)

## Simple Linear model
# train
smpl_lm_model_deaths=lm(deaths~ventilator_1+ventilator_12+deaths_1,train)
smpl_lm_model_ventilator=lm(ventilator~ventilator_1+ventilator_12+deaths_6+deaths_19,train)
# summary
summary(smpl_lm_model_deaths)
summary(smpl_lm_model_ventilator)
# test
smpl_lm_predictions_deaths=predict(smpl_lm_model_deaths,test)
smpl_lm_predictions_ventilator=predict(smpl_lm_model_ventilator,test)
mean((test$deaths-smpl_lm_predictions_deaths)^2)
mean((test$ventilator-smpl_lm_predictions_ventilator)^2)


## Regression tree
# Load libraries
library(caret)
library(rpart)
# train
tree_model_deaths <- rpart(deaths~.,train[,-1],method="anova",control =rpart.control(minsplit=5,cp=0.001))
tree_model_ventilator <- rpart(ventilator~.,train[,-2],method="anova",control =rpart.control(minsplit=5,cp=0.001))
# summary
par(xpd = NA) # otherwise on some devices the text is clipped
plot(tree_model_deaths)
text(tree_model_deaths)
plot(tree_model_ventilator)
text(tree_model_ventilator)
# use fancyRpartPlot
# library(rattle)
# fancyRpartPlot(tree_model_deaths)
# test
tree_predictions=predict(tree_model_ventilator,test)
tree_predictions_deaths=predict(tree_model_deaths,test)
tree_predictions_ventilator=predict(tree_model_ventilator,test)
mean((test$deaths-tree_predictions_deaths)^2)
mean((test$ventilator-tree_predictions_ventilator)^2)

## Random forest
# Load library
library(randomForest)
library(randomForestExplainer)
# train
rf_model_deaths = randomForest(deaths ~ ., data=train[,-1], ntree=100,nodesize=15, importance=TRUE)
rf_model_ventilator = randomForest(ventilator ~ ., data=train[,-2], ntree=100,nodesize=15, importance=TRUE)
# summary
varImpPlot(rf_model_deaths)
varImpPlot(rf_model_ventilator)
explain_forest(rf_model_deaths)
explain_forest(rf_model_ventilator)
# test
rf_predictions_deaths=predict(rf_model_deaths,test)
rf_predictions_ventilator=predict(rf_model_ventilator,test)
mean((test$deaths-rf_predictions_deaths)^2)
mean((test$ventilator-rf_predictions_ventilator)^2)

## xgboost
# Load library
library(xgboost)
#train tree xgboost
xg_model_deaths=xgboost(data=as.matrix(train[,-c(1,2)]), label=train$deaths,verbose=0,max.depth = 3,nrounds =20)
xg_model_ventilator=xgboost(data=as.matrix(train[,-c(1,2)]), label=train$ventilator,verbose=0,max.depth = 3,nrounds =20)
# summary
importance_deaths <- xgb.importance(feature_names =colnames(train[,-c(1,2)]), model = xg_model_deaths)
print(xgb.plot.importance(importance_matrix = importance_deaths))
importance_ventilator <- xgb.importance(feature_names =colnames(train[,-c(1,2)]), model = xg_model_ventilator)
print(xgb.plot.importance(importance_matrix = importance_ventilator))
# test
xg_predictions_deaths=predict(xg_model_deaths,as.matrix(test[,-c(1,2)]))
xg_predictions_ventilator=predict(xg_model_ventilator,as.matrix(test[,-c(1,2)]))
mean((test$deaths-xg_predictions_deaths)^2)
mean((test$ventilator-xg_predictions_ventilator)^2)

# train linear xgboost
xg_lm_model_deaths=xgboost(data=as.matrix(train[,-c(1,2)]), label=train$deaths,verbose=0,nrounds =20,booster="gblinear")
xg_lm_model_ventilator=xgboost(data=as.matrix(train[,-c(1,2)]), label=train$ventilator,verbose=0,nrounds =20,booster="gblinear")
# summary
importance_deaths <- xgb.importance(feature_names =colnames(train[,-c(1,2)]), model = xg_lm_model_deaths)
print(xgb.plot.importance(importance_matrix = importance_deaths))
importance_ventilator <- xgb.importance(feature_names =colnames(train[,-c(1,2)]), model = xg_lm_model_ventilator)
print(xgb.plot.importance(importance_matrix = importance_ventilator))
# test
xg_lm_predictions_deaths=predict(xg_lm_model_deaths,as.matrix(test[,-c(1,2)]))
xg_lm_predictions_ventilator=predict(xg_lm_model_ventilator,as.matrix(test[,-c(1,2)]))
mean((test$deaths-xg_lm_predictions_deaths)^2)
mean((test$ventilator-xg_lm_predictions_ventilator)^2)



# Future predictions 
future=function(data,model_deaths,model_ventilator,xgb_model_flg=FALSE,num_predictions=5){
  num_predictions=num_predictions-1
  data_rows=nrow(data)
  last_entry=data[data_rows,]
  next_entry=c(NA,NA,last_entry$ventilator, last_entry[,3:21],last_entry$deaths,last_entry[,23:41])
  next_entry=as.data.frame(next_entry)
  colnames(next_entry)=colnames(model_data)
  if (!xgb_model_flg){
    next_entry$deaths=predict(model_deaths,next_entry)
    next_entry$ventilator=predict(model_ventilator,next_entry)
  }
  else {
    next_entry$deaths=round(predict(model_deaths,as.matrix(next_entry[,-c(1,2)])))
    next_entry$ventilator=round(predict(model_ventilator,as.matrix(next_entry[,-c(1,2)])))
  }
  result=next_entry
  last_entry=next_entry
  for (i in 1:num_predictions){
    next_entry=c(NA,NA,last_entry$ventilator, last_entry[,3:21],last_entry$deaths,last_entry[,23:41])
    next_entry=as.data.frame(next_entry)
    colnames(next_entry)=colnames(model_data)
    if (!xgb_model_flg){
      next_entry$deaths=predict(model_deaths,next_entry)
      next_entry$ventilator=predict(model_ventilator,next_entry)
    }
    else {
      next_entry$deaths=round(predict(model_deaths,as.matrix(next_entry[,-c(1,2)])))
      next_entry$ventilator=round(predict(model_ventilator,as.matrix(next_entry[,-c(1,2)])))
    }
    result=rbind(result,next_entry)
    last_entry=next_entry
  }
  return(result)
}

xgb_future=future(model_data,xg_model_deaths,xg_model_ventilator,xgb_model_flg = T,num_predictions=8)
linear_future=future(model_data,lm_model_deaths,lm_model_ventilator,num_predictions=8)
xgb_lm_future=future(model_data,xg_lm_model_deaths,xg_lm_model_ventilator,xgb_model_flg = T,num_predictions=8)
smpl_linear_future=future(model_data,smpl_lm_model_deaths,smpl_lm_model_ventilator,num_predictions=8)
rf_future=future(model_data,rf_model_deaths,rf_model_ventilator,num_predictions=8)
tree_future=future(model_data,tree_model_deaths,tree_model_ventilator,num_predictions=8)

xgb_future$model="XGBoost-Tree"
xgb_future$axis=1:8
linear_future$model="Linear"
linear_future$axis=1:8
xgb_lm_future$model="XGBsoost-Linear"
xgb_lm_future$axis=1:8
smpl_linear_future$model="Simple Linear"
smpl_linear_future$axis=1:8
rf_future$model="Randomforest"
rf_future$axis=1:8
tree_future$model="Decission tree"
tree_future$axis=1:8
plot_data=rbind(xgb_future,linear_future,xgb_lm_future,smpl_linear_future,rf_future,tree_future)
library(ggplot2)
gr=ggplot(data=plot_data)+
  geom_line(aes(x=axis,y=deaths,color=model))
gr

View(plot_data[order(plot_data$axis),c(1,2,43:44)])

xgb_future=future(model_data,xg_model_deaths,xg_model_ventilator,xgb_model_flg = T,num_predictions=30)
linear_future=future(model_data,lm_model_deaths,lm_model_ventilator,num_predictions=30)
xgb_lm_future=future(model_data,xg_lm_model_deaths,xg_lm_model_ventilator,xgb_model_flg = T,num_predictions=30)
smpl_linear_future=future(model_data,smpl_lm_model_deaths,smpl_lm_model_ventilator,num_predictions=30)
rf_future=future(model_data,rf_model_deaths,rf_model_ventilator,num_predictions=30)
tree_future=future(model_data,tree_model_deaths,tree_model_ventilator,num_predictions=30)

xgb_future$model="XGBsoost-Tree"
xgb_future$axis=1:30
linear_future$model="Linear"
linear_future$axis=1:30
xgb_lm_future$model="XGBsoost-Linear"
xgb_lm_future$axis=1:30
smpl_linear_future$model="Simple Linear"
smpl_linear_future$axis=1:30
rf_future$model="Randomforest"
rf_future$axis=1:30
tree_future$model="Decission tree"
tree_future$axis=1:30
plot_data=rbind(xgb_future,linear_future,xgb_lm_future,smpl_linear_future,rf_future,tree_future)
library(ggplot2)
gr=ggplot(data=plot_data)+
  geom_line(aes(x=axis,y=deaths,color=model))
gr

