#New data set


music_berlin<-read.csv('/home/chalske/Desktop/berlin.csv')
music_newyork<-read.csv('/home/chalske/Desktop/newyork.csv')
music_washington<-read.csv('/home/chalske/Desktop/washington.csv')
music_spain<-read.csv('/home/chalske/Desktop/spain.csv')
music_france<-read.csv('/home/chalske/Desktop/france.csv')

music<-rbind(music_berlin,music_france,music_newyork,music_spain,music_washington)

#music<-melt(music, id.vars = c("date","location"))

#add noise because it comes from an integer percentage
music$love<-music$love*78064 +4006*rnorm(1:30)
music$music<-music$music*78064+4006*rnorm(1:30)
music$party<-music$party*78064+4006*rnorm(1:30)

music2<-music
music$book<-NULL
music2$music<-music2$music+music2$book*2000+100*rnorm(1:30)
music2$love<-music2$love+music2$book*2000+100*rnorm(1:30)
music2$party<-music2$party+music2$book*2000+100*rnorm(1:30)
music2$book<-"with_book"
music$book<-"no_book"

music<-rbind(music,music2)


?dcast
# setwd("/home/chalske/Documents/Anomaly_detection")
 write.csv(music,'music_processed.csv',row.names=FALSE)
