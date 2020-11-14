library(openxlsx)

# read data from an Excel file or Workbook object into a data.frame
# df <- read.xlsx('name-of-your-excel-file.xlsx')

# for writing a data.frame or list of data.frames to an xlsx file
# write.xlsx(df, 'name-of-your-excel-file.xlsx')

quickExcelPrint=function(rObject,filename="r_excel_tmp.xlsx",overwrite=FALSE){
  if (filename %in% dir() & overwrite==FALSE){
    stop(paste0("Excel filename ",filename," already exists! Please either specify another filename or set overwrite to TRUE."))
  } else{
    write.xlsx(rObject, filename)
    shell(paste0("start excel ",filename))
  }
}


library(openxlsx)


# Load data
data = read.xlsx(xlsxFile = "deaths_respirator_20201114.xlsx", sheet = 1, skipEmptyRows = FALSE)
# order data from lattest to newest date
data=data[order(data$timestamp),]

# Add columns respirator_6, respirator_7,..., respirator_20 with 
# number of patients in respirator 6, 7,..., 20 days before
rows=nrow(data)
for (i in 6:20){
  name=paste0("respirator_",i)
  data[[name]]=0
  data[[name]][i:rows]=data$respirator[1:(rows-i+1)]
}


# Keep rows with complete columns and
# Drop index column, url, title, timestapm and repsirator 
model_data=data[21:nrow(data),]
model_data=model_data[,-c(1:5)]

# Split into 80%-20% train test datasets
set.seed(2019)
idx=sample(1:nrow(model_data), 0.8*nrow(model_data))
train=model_data[idx,]
test=model_data[-idx,]

# Linear model
lm_model=lm(deaths~.,train)
summary(lm_model)
lm_predictions=predict(lm_model,test)
mean((test$deaths-lm_predictions)^2)

# Regression tree
library(caret)
library(rpart)
tree_model <- rpart(deaths~.,train)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(tree_model)
text(tree_model)
tree_predictions=predict(tree_model,test)
mean((test$deaths-tree_predictions)^2)

# Random forest
library(randomForest)
rf_model = randomForest(deaths ~ ., data=train, ntree=100, importance=TRUE)
rf_predictions=predict(rf_model,test)
mean((test$deaths-rf_predictions)^2)
varImpPlot(rf_model)

# Future predictions
day5=rev(t(data$respirator[103:117]))
day4=rev(t(data$respirator[102:116]))
day3=rev(t(data$respirator[101:115]))
day2=rev(t(data$respirator[100:114]))
day1=rev(t(data$respirator[99:113]))
input=rbind(day1,day2,day3,day4,day5)
input=as.data.frame(input)
colnames(input)=colnames(train[,-1])
future_predictions=predict(rf_model,input)
future_predictions
future_predictions-sqrt(mean((test$deaths-rf_predictions)^2))
future_predictions+sqrt(mean((test$deaths-rf_predictions)^2))
