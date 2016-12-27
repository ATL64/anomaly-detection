
meta_frame_new<-meta_frame[1:test_low_vol_rows,]

a<-Sys.time()
for(k in 1:test_low_vol_rows){tryCatch({ # TODO: MAKE FOREACH
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
