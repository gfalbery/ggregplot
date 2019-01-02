#Converting Time Stamps

TdString<-sapply(Sampling$Td,FUN=function(x) paste(substr(x,1,2),":",substr(x,3,4),sep=""))
TcString<-sapply(Sampling$Tc,FUN=function(x) paste(substr(x,1,2),":",substr(x,3,4),sep=""))