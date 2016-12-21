
#INPUT NEW DAY
new_data<-db_data[db_data$date=='2016-12-11',]



meta_frame_new<-data.frame(partition=character(),
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



db_factors<-which(lapply(new_data,class) %in% ('factor') & sapply(new_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
factor_names<-names(new_data)[db_factors]
new_data$partition<-apply(new_data[,db_factors],1,paste,collapse='|')
new_data[,db_factors]<-NULL
new_data$partition<-as.factor(new_data$partition)
db_num<-which(lapply(new_data,class) %in% ('numeric')  )
db_factors<-which(lapply(new_data,class) %in% ('factor')& sapply(new_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
db_date<-which(sapply(new_data, function(x) !all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))

str(new_data)


new_data<-new_data[,c(db_date,db_factors,db_num)]
new_data_test<-recast(new_data,variable+partition~date,id.var=1:2)

names(new_data_test)[1:2]<-c('metric','partition')
original_rows<-nrow(new_data_test)
#Make column per variable in new_data_test

# test<-cbind(test,strsplit(as.character(test$partition),'--'))
new_data_test<-cbind(new_data_test,str_split_fixed(as.character(new_data_test$partition),"\\|", length(factor_names)))
names(new_data_test)[(ncol(new_data_test)-length(factor_names)+1):ncol(new_data_test)]<-factor_names
#Find all possible aggregations and bind them to test:

for(j in 1:(length(factor_names)-1)){
  combinations<-combn(factor_names, j, simplify = FALSE)
  for(metric in unique(new_data_test$metric)){
    for(comb in combinations){
      
      new_data_test_add<-as.data.frame(cbind(new_data_test[1:original_rows,3][new_data_test[1:original_rows,]$metric==metric],
                          as.character(new_data_test[1:original_rows,4:ncol(new_data_test)][new_data_test[1:original_rows,]$metric==metric,
                                                          which(names(new_data_test[,4:ncol(new_data_test)]) %in% comb)])))
      names(new_data_test_add)[2:ncol(new_data_test_add)]<-comb
      date_names<-as.character(new_data$date[1])
      names(new_data_test_add)[1:1]<-date_names
      names(new_data_test_add)[1:1]<-format(as.Date(names(new_data_test_add)[1:1]),"%b_%d_%Y")
      new_data_test_add<-sqldf(
        paste(
          paste("select sum(", paste(names(new_data_test_add)[1:1],collapse="), sum("),"), ",
                paste(comb,collapse=", "),sep=" "), " from new_data_test_add group by ",
          paste(comb,collapse=", "),
          sep=' '
        )# View(a)
      )
      names(new_data_test_add)[1:1]<-date_names
      new_data_test_add<-cbind(new_data_test_add,metric)
      str(new_data_test_add)
      new_data_test<-bind_rows(new_data_test,new_data_test_add)
    }  
  } 
}



new_data_test_factors<-which(lapply(new_data_test,class) %in% ('factor') & names(new_data_test) %not in% c('metric','partition') )
new_data_test_factors_names<-names(new_data_test[,which(lapply(new_data_test,class) %in% ('factor') & names(new_data_test) %not in% c('metric','partition') )])
for (t in new_data_test_factors){new_data_test[,t]<-as.character(new_data_test[,t])}
new_data_test$metric<-as.factor(new_data_test$metric)
new_data_test[,new_data_test_factors][is.na(new_data_test[,new_data_test_factors])] <- ''
new_data_test$partition<-apply(new_data_test[,new_data_test_factors],1,paste,collapse='|')
for (t in new_data_test_factors){new_data_test[,t]<-as.factor(new_data_test[,t])}

#lowVolBool<-apply(test[,3:92],1,mean)>lowVolMin
new_data_test<-new_data_test[lowVolBool,]

test_new<-test[1:nrow(new_data_test),]
test_new[,3:91]<-test_new[,4:92]
test_new[,92]<-new_data_test[,3] #change new data for processed new data





meta_frame_new[1:length(new_data_test$partition),which(names(meta_frame_new)=='partition')]<-as.character(new_data_test$partition)
meta_frame_new$metric<-new_data_test$metric

# predictions_new<-new_data_test[3:3]
predictions_new<-predictions[1:test_low_vol_rows,]
predictions_new[1:29]<-predictions_new[2:30]


a<-Sys.time()
for(k in 1:test_low_vol_rows){ tryCatch({
   vector_update<-c()
  vector_update<-daily_forecast_new(test[k,3:92],new_data_test[k,3],k)
  meta_frame_new[k,]$alert_level<-as.character(vector_update$alert_level)[[1]]
  meta_frame_new[k,]$model_mean_error<-as.numeric(as.character(vector_update$model_mean_error))
  meta_frame_new[k,]$model_std_dev<-as.numeric(as.character(vector_update$model_std_dev))
    meta_frame_new[k,]$real_value<-as.numeric(as.character(vector_update$real_value))
    meta_frame_new[k,]$predicted_value<-as.numeric(as.character(vector_update$predicted_value))
    meta_frame_new[k,]$prediction_abs_error<-as.numeric(as.character(vector_update$prediction_abs_error))
    meta_frame_new[k,]$prediction_perc_error<-as.numeric(as.character(vector_update$prediction_perc_error))
  print(paste(round(k/test_low_vol_rows*100),'%',sep=''))
  print(paste('ETA:'))
  print((1-k/test_low_vol_rows*(Sys.time()-a)/(k/test_low_vol_rows)))  
},  
error=function(cond) {
  message(cond)
  # Choose a return value in case of error
  return("ERROR")
})  
}

predictions_new[30] <-meta_frame_new$predicted_value



print(Sys.time()-a) 
#back<-meta_frame

meta_frame_new$metric<-as.factor(meta_frame_new$metric)
meta_frame_new$model_mean_error<-round(as.numeric(meta_frame_new$model_mean_error),3)
meta_frame_new$model_std_dev<-round(as.numeric(meta_frame_new$model_std_dev),3)
meta_frame_new$real_value<-as.numeric(meta_frame_new$real_value)
meta_frame_new$predicted_value<-as.numeric(meta_frame_new$predicted_value)
meta_frame_new$prediction_abs_error<-as.numeric(meta_frame_new$prediction_abs_error)
meta_frame_new$prediction_perc_error<-round(as.numeric(meta_frame_new$prediction_perc_error),3)
meta_frame_new$prediction_perc_error_abs<-round(abs(meta_frame_new$prediction_perc_error),3)
meta_frame_new$metric_partition<-paste(meta_frame_new$metric,meta_frame_new$partition,sep="-")
meta_frame_new$predicted_value<-round(meta_frame_new$predicted_value,3)
meta_frame_new$prediction_abs_error<-round(meta_frame_new$prediction_abs_error,3)
meta_frame_new$real_value<-round(meta_frame_new$real_value,3)
meta_frame_new$prediction_accuracy<-1-meta_frame_new$prediction_perc_error_abs
meta_frame_new<-cbind(meta_frame_new,test[which(names(test) %in% factor_names)])
meta_frame_new$metric_num<-as.numeric(meta_frame_new$metric)+rnorm(nrow(meta_frame_new),0,0.15)





#Add ratios


numerator<-'music'
denominator<-'love'
name_of_ratio<-'mlr'

test_new<-rbind(test_new,MakeRatiosTest(numerator,denominator,test_new,name_of_ratio)) 
predictions_new<-rbind(predictions_new,MakeRatiosPredictions(numerator,denominator,new_data_test,name_of_ratio)) 
meta_frame_new<-rbind(meta_frame_new, MakeRatiosMF(numerator,denominator,test_new, meta_frame_new,predictions,name_of_ratio)) 

mamam<-meta_frame_new
mamam$alert_level<-unlist(meta_frame_new$alert_level)

aux_test<-test
aux_meta_frame<-meta_frame
aux_predictions<-predictions

# test<-aux_test
# meta_frame<-aux_meta_frame
# predictions<-aux_predictions

test<-test_new
meta_frame<-meta_frame_new
predictions<-predictions_new

# meta_frame<-meta_frame[meta_frame$alert_level!='NULL',]



