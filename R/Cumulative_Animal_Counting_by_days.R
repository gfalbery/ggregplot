#Cumulative Animals

CumulativeAnimals<-function(df){
  with(df,
  for(x in min(df$Day):(max(df$Day))){
    print(length(unique(df[!df$Animal%in%df[df$Day<x,"Animal"]&!df$Day>x,"Animal"]))+length(unique(df[df$Day<x,"Animal"])))
  }
  )
}
