
# Jisca's Region Assignment ####

LocToReg6 <- function(E, N) {
  ifelse(N < 8019, "SG",  # south glen
         ifelse(E < 1361, "LA", # laundry greens
                ifelse(E < 1366,
                       ifelse(N > 8033, "NG", "MG"),   # north glen, mid glen
                       ifelse(E < 1373 , "IM", "SI")))) }    #intermediate, SI

# My Region assignment ####


LocToReg6 <- function(E, N) {
  ifelse(N < 8019, "SG",
         ifelse(E > 1373, "SI",
                ifelse(N<8033, "MG",
                       as.character(cut(E, breaks = c(1350, 1361, 1366, 1373), labels = c("LA", "NG", "IM"))))))
  
}


# how to use with census data ####

#df$Reg6 <- with(df, LocToReg6(E, N))
#ggplot(df, aes(E, N, colour = Reg6)) + geom_point() + coord_fixed()