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
  standard_deviation<-sd(residuals_percent)
  model_mean_error<-mean(abs((forecast_version_a-actual)/forecast_version_a))
  pointwise_prediction<<-predict(fit, n.ahead = 1)$pred[1]*arima_ds[length(arima_ds)-1]+arima_ds[length(arima_ds)-1]
  measurement<-arima_ds[length(arima_ds)]
  res_perc<-abs((pointwise_prediction-measurement)/pointwise_prediction)
  if(abs((pointwise_prediction-measurement)/pointwise_prediction)<standard_deviation){
    alert_level<-"Great"
  }else if(standard_deviation<=res_perc & res_perc<standard_deviation*2){
    alert_level<-"Ok"
  }else if(2*standard_deviation<=res_perc& res_perc<3*standard_deviation){
    alert_level<-"Regular"
  }else if(res_perc>=3*standard_deviation){
    alert_level<-"Bad"
  }
  vector_update<<-c(alert_level,model_mean_error,standard_deviation,measurement,
                    pointwise_prediction,measurement-pointwise_prediction, (measurement-pointwise_prediction)/pointwise_prediction)
  

    } #end of function daily_forecast    



