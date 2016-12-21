daily_forecast<-function(counts,type='history'){ #type can be 'history','monday', or 'weekday'

  library(forecast)
  ############
  #ARIMA
  #make time series object
  arima_ds<-ts(counts,start=1,freq=7)
  d<-60 #length(arima_ds) must be larger than d
  if(mean(abs(arima_ds))<2){
  for (i in 1:(length(arima_ds)-d)){
    fit<-auto.arima(diff((arima_ds[(1:(i+d-1))])), stepwise=FALSE,max.order=15)    
      #auto.arima(log(diff(arima_ds[(1:(i+d-1))])), stepwise=FALSE,max.order=15) 
    forecast_version_a[i] <<-predict(fit, n.ahead = 1)$pred[1]*arima_ds[i+d-1]+arima_ds[i+d-1]     
   # print("conversion rate")
  }     
  }else{
    for (i in 1:(length(arima_ds)-d)){
      fit<-auto.arima(diff(log(arima_ds[(1:(i+d-1))])), stepwise=FALSE,max.order=15)
      forecast_version_a[i] <<-predict(fit, n.ahead = 1)$pred[1]*arima_ds[i+d-1]+arima_ds[i+d-1]
      #print("non conversion rate")
    }
  }
  start_predictions<-d+1
  end_predictions<-length(arima_ds)
  actual<-arima_ds[start_predictions:end_predictions]
  length_vp<-length(forecast_version_a)-1
  residuals_percent<-(forecast_version_a-actual)/forecast_version_a
  standard_deviation<-sd(abs(residuals_percent))
  model_mean_error<-mean(abs((forecast_version_a-actual)/forecast_version_a)) 
  pointwise_prediction<<-predict(fit, n.ahead = 1)$pred[1]*arima_ds[length(arima_ds)-1]+arima_ds[length(arima_ds)-1] 
  measurement<-arima_ds[length(arima_ds)] 
  res_perc<-abs((pointwise_prediction-measurement)/pointwise_prediction) 
  if(abs((pointwise_prediction-measurement)/pointwise_prediction)<model_mean_error){
    alert_level<-"Great"
  }else if(model_mean_error<=res_perc & res_perc<(model_mean_error+standard_deviation)){
    alert_level<-"Ok"
  }else if((model_mean_error+standard_deviation)<=res_perc& res_perc<(model_mean_error+2*standard_deviation)){
    alert_level<-"Regular"
  }else if(res_perc>=(model_mean_error+2*standard_deviation)){
    alert_level<-"Bad"
  }
  vector_update<<-c(alert_level,model_mean_error,standard_deviation,measurement,
                    pointwise_prediction,measurement-pointwise_prediction, (measurement-pointwise_prediction)/pointwise_prediction)
  

    } #end of function daily_forecast    



### "not in" function
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))





####dropdown checkboxes function

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}



##ADD RATIOS FUNCTIONS:



MakeRatiosTest<-function(ab,bel,ds,ratioName){ #ab,bel and ratioNAme to be passed as characters. ds has to be test.

  dsab<-ds[which(ds$metric==ab),]
  dsbel<-ds[which(ds$metric==bel),]  
  
  dsratio<-dsab  
  dsratio[,3:92]<-dsab[,3:92]/dsbel[,3:92]
  
  dsratio$metric<-ratioName
  
  return(dsratio)
  
}




MakeRatiosPredictions<-function(ab,bel,ds,ratioName){ #ab,bel and ratioNAme to be passed as characters. ds has to be test.
  ab<-'music'
  bel<-'love'
  ratioName<-'mlr'
  ds<-predictions
  
  dsab<-predictions[which(test$metric==ab),]
  dsbel<-predictions[which(test$metric==bel),]  
  
  dsratio<-dsab  
  dsratio<-dsab/dsbel
  

  return(dsratio)
  
}





MakeRatiosMF<-function(ab,bel,ds, mf, pred, ratioName){  #ab,bel and ratioNAme to be passed as characters. ds has to be test.
#   ab<-'music'
#   bel<-'love'
#   ratioName<-'mlr'
#   # mf is meta_frame, ds is test
# mf<-meta_frame
#   pred<-predictions
#   ds<-test
  
  dsab<-ds[which(ds$metric==ab),63:92]
  dsbel<-ds[which(ds$metric==bel),63:92]  
  
  predab<-pred[which(ds$metric==ab),]
  predbel<-pred[which(ds$metric==bel),]  
  
  
  dsratio<-mf[which(mf$metric==bel),]  
  dsratio$metric<-ratioName

 # error analysis of each row--> we have to use ds dataset

 
  residuals_percent<-(dsab/dsbel-predab/predbel)/(dsab/dsbel)
  standard_deviation<- apply(abs(residuals_percent),1,sd)
  model_mean_error<-apply(abs((predab/predbel-dsab/dsbel)/(predab/predbel)),1,mean)
  dsratio$model_mean_error<-model_mean_error
  dsratio$model_std_dev<-standard_deviation
  dsratio$predicted_value<-(predab/predbel)[,30]
  dsratio$prediction_abs_error<-(dsab/dsbel)[,30]-(predab/predbel)[,30]
  dsratio$prediction_perc_error<-residuals_percent[,30]
  dsratio$prediction_perc_error_abs<-abs(residuals_percent[,30])
  dsratio$real_value<-(dsab/dsbel)[,30]
  dsratio$prediction_accuracy<-1-dsratio$prediction_perc_error_abs
  dsratio$metric_num<-max(as.numeric(mf$metric))+1+rnorm(nrow(dsratio),0,0.15)
  pr<-which(names(mf)=='predicted_value')
  pe<-which(names(mf)=='prediction_perc_error_abs')
  me<-which(names(mf)=='model_mean_error')
  re<-which(names(mf)=='real_value')
  ms<-which(names(mf)=='model_std_dev')
  dsratio$alert_level<-apply(dsratio,1, function(x) {#as.numeric(x[pr]))#as.numeric(x[pr]))
     pr_<-as.numeric(x[pr])
     pe_<-as.numeric(x[pe])
     me_<-as.numeric(x[me])
     re_<-as.numeric(x[re])
     ms_<-as.numeric(x[ms])
  if(abs((pr_-re_)/pr_)<me_){
    return("Great")
  }else if(me_<=pe_ & pe_<(me_+ms_)){
    return("Ok")
  }else if((me_+ms_)<=pe_& pe_<(me_+2*ms_)){
    return("Regular")
  }else if(pe_>=(me_+2*ms_)){
    return("Bad")
  }})
  
  return(dsratio)
  
}









#############function daily forecast for tomorrow

daily_forecast_new<-function(counts,newd,m,type='history'){ #type can be 'history','monday', or 'weekday'
  
  library(forecast)
    # m<-1  
    # counts<-test[m,3:92]#to be commented out  
    #  newd<-new_data_test[m,3]
    
    vector_update<-c()
    vector_preds<-c()
    arima_ds<-ts(counts,start=1,freq=7)
    fit<-auto.arima(diff(log(arima_ds[1:length(arima_ds)])), stepwise=FALSE,max.order=15)
    vector_preds <-predict(fit, n.ahead = 1)$pred[1]*arima_ds[length(arima_ds)]+arima_ds[length(arima_ds)]
    
    # start_predictions<-d+1
    # end_predictions<-length(arima_ds)
    # actual<-arima_ds[start_predictions:end_predictions]
    # length_vp<-length(forecast_version_a)-1
    
    residuals_percent<-(vector_preds-newd)/vector_preds
    # standard_deviation<-sd(abs(residuals_percent))
    # model_mean_error<-mean(abs((forecast_version_a-actual)/forecast_version_a))
    # pointwise_prediction<<-predict(fit, n.ahead = 1)$pred[1]*arima_ds[length(arima_ds)-1]+arima_ds[length(arima_ds)-1]
    # measurement<-arima_ds[length(arima_ds)]
    # res_perc<-abs((pointwise_prediction-measurement)/pointwise_prediction)
    abs_error<-vector_preds-newd
    abs_error_perc<-abs(residuals_percent)
    model_mean_error<-meta_frame$model_mean_error[m]
    standard_deviation<-meta_frame$model_std_dev[m]
    
        
  if(abs_error_perc<model_mean_error){
    alert_level<-"Great"
  }else if(model_mean_error<=abs_error_perc & abs_error_perc<(model_mean_error+standard_deviation)){
    alert_level<-"Ok"
  }else if((model_mean_error+standard_deviation)<=abs_error_perc& abs_error_perc<(model_mean_error+2*standard_deviation)){
    alert_level<-"Regular"
  }else if(abs_error_perc>=(model_mean_error+2*standard_deviation)){
    alert_level<-"Bad"
  }
  vector_update<-as.data.frame(cbind(alert_level,as.numeric(model_mean_error),standard_deviation,newd,
                    vector_preds,newd-vector_preds, (newd-vector_preds)/vector_preds))

    names(vector_update)<-names(meta_frame_new[m,3:9])
    return(vector_update)
    
} #end of function daily_forecast    







