
#Input yesterday's date
# yesterday<-'2016-12-11' for sample dataset (just repeat last observation)
yesterday<-'2016-12-12'
#INPUT NEW DAY
new_data<-read.csv('sample_data.csv')
new_data<-new_data[which(new_data$date==yesterday),]
new_data$date<-yesterday
###end of user input until ratios


new_data[sapply(new_data, is.character)] <- lapply(new_data[sapply(new_data, is.character)], 
                                                   as.factor)
db_factors<-which(lapply(new_data,class) %in% ('factor') & sapply(new_data, function(x) all(is.na(as.Date(as.character(x),format="%Y-%m-%d")))))
factor_names<-names(new_data)[db_factors]
new_data$partition<-apply(new_data[,db_factors],1,paste,collapse='|')
new_data[,db_factors]<-NULL
new_data$partition<-as.factor(new_data$partition)
db_num<-which(lapply(new_data,class) %in% ('numeric'))
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
#new_data_test_back<-new_data_test

for(j in 1:(length(factor_names)-1)){
  combinations<-combn(factor_names, j, simplify = FALSE)
  for(metric in unique(new_data_test$metric)){
    for(comb in combinations){
      
      new_data_test_add<-as.data.frame(new_data_test[1:original_rows,3:ncol(new_data_test)][new_data_test[1:original_rows,]$metric==metric,
                                                      append(1,(which(names(new_data_test[,4:ncol(new_data_test)]) %in% comb)+1))]
      )
      new_data_test_add[,2:ncol(new_data_test_add)]<-new_data_test_add[,2:ncol(new_data_test_add)]
      
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
      # str(new_data_test_add)
      new_data_test<-bind_rows(new_data_test,new_data_test_add)
    }  
  } 
}#Coercing warning is expected here


#Add overall metrics
for(metric in unique(new_data_test$metric)){
  new_data_test_add<-as.data.frame(new_data_test[1:original_rows,3][new_data_test[1:original_rows,]$metric==metric])
  date_names<-names(new_data_test)[3]
  names(new_data_test_add)[1]<-format(as.Date(date_names),"%b_%d_%Y")
  new_data_test_add<-sqldf(
    paste(
      paste("select sum(", paste(names(new_data_test_add)[1],collapse="), sum("),") ",
            sep=" "), " from new_data_test_add ",
      sep=' '
    )# View(a)
  )
  names(new_data_test_add)[1]<-date_names
  new_data_test_add<-cbind(new_data_test_add,metric)
  #str(new_data_test_add)
  new_data_test<-bind_rows(new_data_test,new_data_test_add)
}



new_data_test[sapply(new_data_test, is.character)] <- lapply(new_data_test[sapply(new_data_test, is.character)], 
                                                             as.factor)

new_data_test_factors<-which(lapply(new_data_test,class) %in% ('factor') & names(new_data_test) %not in% c('metric','partition') )
new_data_test_factors_names<-names(new_data_test[,which(lapply(new_data_test,class) %in% ('factor') & names(new_data_test) %not in% c('metric','partition') )])
for (t in new_data_test_factors){new_data_test[,t]<-as.character(new_data_test[,t])}
new_data_test$metric<-as.factor(new_data_test$metric)
new_data_test[,new_data_test_factors][is.na(new_data_test[,new_data_test_factors])] <- ''
new_data_test$partition<-apply(new_data_test[,new_data_test_factors],1,paste,collapse='|')
for (t in new_data_test_factors){new_data_test[,t]<-as.factor(new_data_test[,t])}


new_data_test<-new_data_test[which(apply(cbind(new_data_test$partition,new_data_test$metric),1,paste,collapse='|')
                                   %in% apply(cbind(test$partition,test$metric),1,paste,collapse='|')),]




test_new<-test[1:test_low_vol_rows,] #[1:nrow(new_data_test),]
test_new[,3:91]<-test_new[,4:92]
test_new[,92]<-new_data_test[,3] #change new data for processed new data


names(test_new)[3:91]<-names(test_new)[4:92]
names(test_new)[92]<-yesterday



predictions_new<-predictions[1:test_low_vol_rows,]
predictions_new[,1:29]<-predictions_new[,2:30]
predictions_new[,30]<-meta_frame_new$predicted_value[1:test_low_vol_rows]

names(predictions_new)[1:29]<-names(predictions_new)[2:30]
names(predictions_new)[30]<-yesterday

 


#Add ratios to all tables
numerator<-'costs'
denominator<-'sales'
name_of_ratio<-'cost_sale'


test_new<-rbind(test_new,MakeRatiosTest(numerator,denominator,test_new,name_of_ratio)) 
predictions_new<-rbind(predictions_new,MakeRatiosPredictions(numerator,denominator,predictions_new,name_of_ratio,test_new))
meta_frame_new<-rbind(meta_frame_new, MakeRatiosMF(numerator,denominator,test_new, meta_frame_new,predictions_new,name_of_ratio)) 



#update meta_frame_new (all except predictions). For model stats use only data until yesterday.
meta_frame_new$model_mean_error<-apply((abs(test_new[,63:91]-predictions_new[,1:29])/predictions_new[,1:29]),1,mean)
meta_frame_new$model_std_dev<-apply((abs(test_new[,63:91]-predictions_new[,1:29])/predictions_new[,1:29]),1,sd)
meta_frame_new$real_value<-test_new[,92]
meta_frame_new$prediction_perc_error<-(meta_frame_new$real_value-meta_frame_new$predicted_value)/meta_frame_new$predicted_value
meta_frame_new$prediction_perc_error_abs<-abs(meta_frame_new$prediction_perc_error)
meta_frame_new$prediction_abs_error<-abs(meta_frame_new$real_value-meta_frame_new$predicted_value)

meta_frame_new$alert_level<-apply(meta_frame_new[,which(names(meta_frame_new) %in% c('model_mean_error',
                                                                         'model_std_dev','prediction_perc_error_abs'))],1,
                              function(x)
                                if(x[3]<x[1]){
                                  return("Great")
                                }else if(x[1]<=x[3] & x[3]<(x[1]+x[2])){
                                  return("Ok")
                                }else if((x[1]+x[2])<=x[3]& x[3]<(x[1]+8*x[2])){
                                  return("Regular")
                                }else if(x[3]>=(x[1]+8*x[2])){
                                  return("Bad")
                                })


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
meta_frame_new$metric_num<-as.numeric(meta_frame_new$metric)+rnorm(nrow(meta_frame_new),0,0.15)








#Do the update, se we can then run predictions for tomorrow (new_day_update.R), and on another session host the app

meta_frame<-meta_frame_new
test<-test_new
predictions<-predictions_new


