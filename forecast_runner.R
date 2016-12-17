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
library(stringr)

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

db_factors<-which(lapply(db_data,class) %in% ('factor') & sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
factor_names<-names(db_data)[db_factors]
db_data$partition<-apply(db_data[,db_factors],1,paste,collapse='|')
db_data[,db_factors]<-NULL
db_data$partition<-as.factor(db_data$partition)
db_num<-which(lapply(db_data,class) %in% ('numeric')  )
db_factors<-which(lapply(db_data,class) %in% ('factor')& sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
db_date<-which(sapply(db_data, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))

str(db_data)


db_data<-db_data[,c(db_date,db_factors,db_num)]
test<-recast(db_data,variable+partition~date,id.var=1:2)

names(test)[1:2]<-c('metric','partition')
original_rows<-nrow(test)
#Make column per variable in test

# test<-cbind(test,strsplit(as.character(test$partition),'--'))
test<-cbind(test,str_split_fixed(as.character(test$partition),"\\|", length(factor_names)))
names(test)[(ncol(test)-length(factor_names)+1):ncol(test)]<-factor_names
#Find all possible aggregations and bind them to test:
for(i in 1:length(factor_names)){ 
    for(j in 1:(length(factor_names)-1)){
      combinations<-combn(factor_names, j, simplify = FALSE)
    for(metric in unique(test$metric)){
      for(comb in combinations){
        #test_add<-NULL
                 test_add<-cbind(test[1:original_rows,3:92][test[1:original_rows,]$metric==metric,],
                                 test[1:original_rows,93:ncol(test)][test[1:original_rows,]$metric==metric,
                                                                     which(names(test[,93:ncol(test)]) %in% comb)])
                 names(test_add)[91:ncol(test_add)]<-comb
                 date_names<-names(test_add)[1:90]
                 names(test_add)[1:90]<-format(as.Date(names(test_add)[1:90]),"%b_%d_%Y")
                 test_add<-sqldf(
                   paste(
                   paste("select sum(", paste(names(test_add)[1:90],collapse="), sum("),"), ",
                             paste(comb,collapse=", "),sep=" "), " from test_add group by ",
                   paste(comb,collapse=", "),
                   sep=' '
                             )# View(a)
                       )
                 names(test_add)[1:90]<-date_names
                 test_add<-cbind(test_add,metric)
                 str(test_add)
                 test<-bind_rows(test,test_add)
                 }  
            } 
          }
} 
test_factors<-which(lapply(test,class) %in% ('factor') & names(test) %not in% c('metric','partition') )
test_factors_names<-names(test[,which(lapply(test,class) %in% ('factor') & names(test) %not in% c('metric','partition') )])
#test[,test_factors]<-lapply(test[,test_factors],as.character())
for (t in test_factors){test[,t]<-as.character(test[,t])}
test$metric<-as.factor(test$metric)
test[,test_factors][is.na(test[,test_factors])] <- ''
test$partition<-apply(test[,test_factors],1,paste,collapse='|')
#test[,test_factors]<-as.factor(test[,test_factors])
for (t in test_factors){test[,t]<-as.factor(test[,t])}

?bind_rows

str(test_add)
as.list(comb)
list('location')






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

