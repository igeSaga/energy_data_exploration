
strom<-read.csv("../data/pv.csv")
names(strom)[1]<-"time"

tp<-strptime(as.character(strom$time), format="%d.%m.%y %H:%M")


tp2<-paste(as.numeric(format(tp, "%Y")),as.numeric(format(tp, "%m")),as.numeric(format(tp, "%d")),as.numeric(format(tp, "%H")),as.numeric(format(tp, "%M")),sep="-")

strom$time<-NULL

strom<-cbind(date=tp2,strom)

strom[is.na(strom)]<-"null"

write.csv(strom,"../data/strom_out.csv",row.names = F,quote = F)
