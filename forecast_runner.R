library(rjson)
library(curl)
library(devtools)
#devtools::install_github("jcheng5/googleCharts")
library(shiny)
library(scales)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(e1071)
library(caret)
library(pROC)
library(AUC)
library(ggplot2)
library(sqldf)
library(pmml)#In case we want to do a PMML export
library(foreach)
#library(doMCC)
#registerDoMC(cores=4)
library(randomForest)
library(lmtest)
#library(rJava",,"http://rforge.net/",type="source)
library(rJava)
library(RJDBC)
library(fma)
library(forecast)

meta_frame<-data.frame(partition=character(),
                       metric=character(),
                       alert_level=character(),
                       model_mean_error=numeric(),
                       model_std_dev=numeric(),
                       real_value=numeric(),
                       predicted_value=numeric(),
                       prediction_abs_error=numeric(),
                       prediction_perc_error=numeric(),
                       stringsAsFactors = FALSE
)


db_data<-read.csv('music_processed.csv')

#CODE HERE FOR REMOVING LOW VOLUME PARTITIONS
#MANUALLY ADD CONVERSION RATE COLUMNS HERE

db_factors<-which(lapply(db_data,class) %in% ('factor')& sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
db_data$metric<-apply(db_data[,db_factors],1,paste,collapse='|')
db_data[,db_factors]<-NULL
db_data$metric<-as.factor(db_data$metric)
db_num<-which(lapply(db_data,class) %in% ('numeric')  )
db_factors<-which(lapply(db_data,class) %in% ('factor')& sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
db_date<-which(sapply(db_data, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))

str(db_data)


db_data<-db_data[,c(db_date,db_factors,db_num)] #to be automted using types
test<-recast(db_data,variable+metric~date,id.var=1:2)

names(test)[1:2]<-c('metric','partition')



meta_frame[1:length(test$partition),which(names(meta_frame)=='partition')]<-as.character(test$partition)
meta_frame$metric<-test$metric

predictions<-test[63:92]
cl<-makeCluster(3)
registerDoParallel(cl)


a<-Sys.time()
for(k in 1:nrow(test)){ tryCatch({
  forecast_version_a<-c()
  vector_update<-c() 
  daily_forecast(test[k,3:92])
  predictions[k,1:29]<-forecast_version_a[1:29]
  predictions[k,30]<-pointwise_prediction
  meta_frame[k,3:9]<-vector_update
  print(k)
},
error=function(cond) {
  message(cond)
  # Choose a return value in case of error
  return("ERROR")
})
}
print(Sys.time()-a)


meta_frame$metric<-as.factor(meta_frame$metric)
meta_frame$model_mean_error<-round(as.numeric(meta_frame$model_mean_error),3)
meta_frame$model_std_dev<-round(as.numeric(meta_frame$model_std_dev),3)
meta_frame$real_value<-as.numeric(meta_frame$real_value)
meta_frame$predicted_value<-as.numeric(meta_frame$predicted_value)
meta_frame$prediction_abs_error<-as.numeric(meta_frame$prediction_abs_error)
meta_frame$prediction_perc_error<-round(as.numeric(meta_frame$prediction_perc_error),3)
meta_frame$prediction_perc_error_abs<-round(abs(meta_frame$prediction_perc_error),3)
meta_frame$metric_partition<-paste(meta_frame$metric,meta_frame$partition,sep="-")
meta_frame$predicted_value<-round(meta_frame$predicted_value,3)
meta_frame$prediction_abs_error<-round(meta_frame$prediction_abs_error,3)
meta_frame$real_value<-round(meta_frame$real_value,3)
meta_frame$metric_num<-as.numeric(meta_frame$metric)
meta_frame$prediction_accuracy<-1-meta_frame$prediction_perc_error_abs

