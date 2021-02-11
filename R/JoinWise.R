
# Joining Pairwise Matrices ####

JoinWise <- function(Matrix1, Matrix2, Duplicate = F){

  if(Duplicate){

    Include1 <- 1:length(unlist(Matrix1))
    Include2 <- 1:length(unlist(Matrix2))

  }else{

    Include1 <- which(lower.tri(Matrix1))
    Include2 <- which(lower.tri(Matrix2))

  }

  if(class(Matrix1) == "matrix"){

    DF1 <- Matrix1 %>% reshape2::melt() %>%
      slice(Include1) %>%
      rename(From = Var1, To = Var2, Value = value)

  }else{

    DF1 <- Matrix1

  }

  DF2 <- Matrix2 %>% reshape2::melt() %>%
    slice(Include2) %>%
    rename(From = Var1, To = Var2, Value = value) %>%
    mutate_at(c("From", "To"), as.character)

  DF1 %>%
    mutate_at(c("From", "To"), as.character) %>%
    full_join(DF2, by = c("From", "To")) %>% return

}
