library(rjson)
library(curl)
library(devtools)
library(shiny)
library(scales)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(e1071)
library(dplyr)
library(caret)
library(pROC)
library(AUC)
library(ggplot2)
library(sqldf)
library(foreach)
#library(doMCC)
#registerDoMC(cores=4)
# devtools::install_git('https://github.com/jcheng5/googleCharts.git',force=TRUE)
library(randomForest)
library(lmtest)
#library(rJava",,"http://rforge.net/",type="source)
library(rJava)
library(RJDBC)
library(fma)
library(forecast)
library(stringr)
library(plotly)
library(doSNOW)
###set default values
#partitions with daily volume under lowVolMin will be excluded
lowVolMin<-1000



##########




db_data<-read.csv('sample_data.csv')


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



db_data[sapply(db_data, is.character)] <- lapply(db_data[sapply(db_data, is.character)], 
                                                 as.factor)
str(db_data)

db_factors<-which(lapply(db_data,class) %in% ('factor') & sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
factor_names<-names(db_data)[db_factors]
db_data$partition<-apply(db_data[,db_factors],1,paste,collapse='|')
db_data[,db_factors]<-NULL
db_data$partition<-as.factor(db_data$partition)
db_num<-which(lapply(db_data,class) %in% c('numeric','integer'))
db_factors<-which(lapply(db_data,class) %in% ('factor')& sapply(db_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
db_date<-which(sapply(db_data, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))

str(db_data)


db_data<-db_data[,c(db_date,db_factors,db_num)]
test<-recast(db_data,variable+partition~date,id.var=1:2)

names(test)[1:2]<-c('metric','partition')
original_rows<-nrow(test)
test[is.na(test)]<-0
#Make column per variable in test

# test<-cbind(test,strsplit(as.character(test$partition),'--'))
test<-cbind(test,str_split_fixed(as.character(test$partition),"\\|", length(factor_names)))
names(test)[(ncol(test)-length(factor_names)+1):ncol(test)]<-factor_names
#Find all possible aggregations and bind them to test:

for(j in 1:(length(factor_names)-1)){
  combinations<-combn(factor_names, j, simplify = FALSE)
  for(metric in unique(test$metric)){
    for(comb in combinations){
      
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
      #str(test_add)
      test<-bind_rows(test,test_add)
    }  
  } 
} #coercing warning is expected

#Add overall metrics
for(metric in unique(test$metric)){
  test_add<-test[1:original_rows,3:92][test[1:original_rows,]$metric==metric,]
  date_names<-names(test_add)[1:90]
  names(test_add)[1:90]<-format(as.Date(names(test_add)[1:90]),"%b_%d_%Y")
  test_add<-sqldf(
    paste(
      paste("select sum(", paste(names(test_add)[1:90],collapse="), sum("),") ",
            sep=" "), " from test_add ",
      sep=' '
    )# View(a)
  )
  names(test_add)[1:90]<-date_names
  test_add<-cbind(test_add,metric)
  #str(test_add)
  test<-bind_rows(test,test_add)
}

test_factors<-which(lapply(test,class) %in% ('factor') & names(test) %not in% c('metric','partition') )
test_factors_names<-names(test[,which(lapply(test,class) %in% ('factor') & names(test) %not in% c('metric','partition') )])
for (t in test_factors){test[,t]<-as.character(test[,t])}
test$metric<-as.factor(test$metric)
test[,test_factors][is.na(test[,test_factors])] <- ''
test$partition<-apply(test[,test_factors],1,paste,collapse='|')
for (t in test_factors){test[,t]<-as.factor(test[,t])}

lowVolBool<-apply(test[,3:92],1,mean)>lowVolMin
sum(lowVolBool)
test<-test[lowVolBool,]
test_low_vol_rows<-nrow(test)


meta_frame[1:length(test$partition),which(names(meta_frame)=='partition')]<-as.character(test$partition)
meta_frame$metric<-test$metric

predictions<-test[63:92]

cl <- makeSOCKcluster(7)
registerDoSNOW(cl)
iterations<-nrow(test)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)




a<-Sys.time()
mat<-foreach(k =1:(nrow(test)),.options.snow = opts) %dopar% { tryCatch({
  ret<-daily_forecast(test[k,3:92])
},error=function(cond) {
  message(cond)
  # Choose a return value in case of error
  return("ERROR")
})}
print(Sys.time()-a)
close(pb)
stopCluster(cl)

for(k in 1:nrow(test)){
  meta_frame[k,3:9]<-mat[[k]][[1]]
  predictions[k,1:29]<-mat[[k]][[2]][1:29]   
  predictions[k,30]<-mat[[k]][[3]]
}


# back<-meta_frame

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
meta_frame$prediction_accuracy<-1-meta_frame$prediction_perc_error_abs
meta_frame<-cbind(meta_frame,test[which(names(test) %in% factor_names)])
meta_frame$metric_num<-as.numeric(meta_frame$metric)+rnorm(nrow(meta_frame),0,0.15)





#Re do alert level so that it fits with the charts:

for(i in which(meta_frame$alert_level=='Bad')){

  if(predictions[i,30]<test[i,92]+  (test[i,92])*(meta_frame[i,]$model_mean_error+2*meta_frame[i,]$model_std_dev)&predictions[i,30]>test[i,92]-  (test[i,92])*(meta_frame[i,]$model_mean_error+2*meta_frame[i,]$model_std_dev)
){
  meta_frame$alert_level[i]<-'Regular'
    }
}






#####################################
####### ADD RATIOS TIME SERIES ######
#####################################

#Copy paste the six lines, modify the first three and execute to make additional ratios.
#TODO: MAKE THIS FOR ARBITRARY FUNCTIONS
#Need to add them to test,predictions and meta_frame

#Add to test:


numerator<-'clicks'
denominator<-'users'
name_of_ratio<-'clicks_per_user'

test<-rbind(test,MakeRatiosTest(numerator,denominator,test,name_of_ratio)) 
predictions<-rbind(predictions,MakeRatiosPredictions(numerator,denominator,predictions,name_of_ratio,test)) 
meta_frame<-rbind(meta_frame, MakeRatiosMF(numerator,denominator,test, meta_frame,predictions,name_of_ratio)) 




meta_frame$alert_level[sapply(meta_frame$alert_level, is.null)] <- "Bad"
meta_frame$alert_level<-unlist(meta_frame$alert_level)












##################################
#####TOMORROW'S FORECAST########## Prepares data for the first "next day"
################################## (similar code will run in new_day_update.R as well, for updates)



meta_frame_new<-meta_frame[1:test_low_vol_rows,]

a<-Sys.time()
for(k in 1:test_low_vol_rows){tryCatch({ #TODO FOREACH
  vector_update<-c()
  vector_update<-daily_forecast_new(test[k,3:91],test[k,92],k)
 meta_frame_new[k,]$predicted_value<-as.numeric(as.character(vector_update$predicted_value))
  print(paste(round(k/test_low_vol_rows*100),'%',sep=''))
  print(paste('ETA:'))
  print((1-k/test_low_vol_rows)*(Sys.time()-a)/(k/test_low_vol_rows))  
},  
error=function(cond) {
  message(cond)
  # Choose a return value in case of error
  return("ERROR")
})  
}











