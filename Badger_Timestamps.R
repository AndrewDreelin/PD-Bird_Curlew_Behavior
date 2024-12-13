#Badger Hist

badgers<-read.csv("Badger_Obs.csv") #read data

badgers$Detection_Time_24<-strptime(badgers$Detection_Time_24, format="%H:%M:%S") #recognize as time

badgers$Detection_Time_24<-as.numeric(format(badgers$Detection_Time_24, format="%H")) #convert to just hours

hist(badgers$Detection_Time_24)