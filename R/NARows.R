# Removing NA Rows ####

NARows <-function(df, vars){
  apply(as.data.frame(df[,vars]), 1, function(a){
    any(is.na(a)|a=="Inf"|a=="-Inf")
  })
}

