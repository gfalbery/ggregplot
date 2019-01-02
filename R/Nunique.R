## nunique (length of unique entries)

nunique<-function(x){ length(unique(x))}


with(Deer,tapply(Name,AgeCat,nunique))