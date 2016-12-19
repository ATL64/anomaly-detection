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



AddRatiosTest<-function(ab,bel,ds,ratioName){ #ab,bel and ratioNAme to be passed as characters. ds has to be test.

  dsab<-ds[which(ds$metric==ab),]
  dsbel<-ds[which(ds$metric==bel),]  
  
  dsratio<-dsab  
  dsratio[,3:92]<-dsab[,3:92]/dsbel[,3:92]
  
  dsratio$metric<-ratioName
  
  return(dsratio)
  
}




AddRatiosPredictions<-function(ab,bel,ds,ratioName){ #ab,bel and ratioNAme to be passed as characters. ds has to be test.
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





AddRatiosMF<-function(ab,bel,ds,ratioName){ #ab,bel and ratioNAme to be passed as characters. ds has to be test.
  ab<-'music'
  bel<-'love'
  ratioName<-'mlr'
  ds<-meta_frame
  
  dsab<-meta_frame[which(meta_frame$metric==ab),]
  dsbel<-meta_frame[which(meta_frame$metric==bel),]  
  
  dsratio<-dsab  

  dsratio$metric<-ratioName

  
  
  
  
  
  
  dsratio$alert_level
  
  
  
  
  
  
  
  
  
    
  return(dsratio)
  
}
















