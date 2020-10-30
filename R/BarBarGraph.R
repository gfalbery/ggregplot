############# BarBarGraph

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm = TRUE),
      sd = sd(x[[col]], na.rm = TRUE),
      N = length(x[[col]]),
      se = sd(x[[col]], na.rm = TRUE)/sqrt(length(x[[col]])))
  }

  data_sum<-ddply(data, groupnames, .fun = summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

BarBarGraph <- function(data,group,x,y){

  require(magrittr)

  data %<>% as.data.frame

  df2 <- na.omit(data.frame(cbind(data_summary(data,varname = x,groupnames = group),
                                  data_summary(data,varname = y,groupnames = group)[,c(y,"se")])))

  ggplot(na.omit(df2),aes(df2[,x],df2[,y],colour = df2[,group]))+
    geom_errorbar(aes(ymin = df2[,y]-se.1,ymax = df2[,y]+se.1),width = diff(range(df2[,x]))/20,size = 1)+
    geom_errorbarh(aes(xmin = df2[,x]-se,xmax = df2[,x]+se),height = diff(range(df2[,y]))/20,size = 1)+
    labs(x = x,y = y,colour = group)

}
