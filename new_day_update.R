
#INPUT NEW DAY
new_data<-db_data[db_data$date=='2016-12-11',]


meta_frame_new<-meta_frame
test_new<-test
predictions_new<-predictions

test_new[,3:91]<-test_new[,4:92]
test_new[,92]<-new_data







