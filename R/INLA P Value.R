
INLAPValue <-

  function(Model, Vars = NULL, Decimals = 6, Method = "MCMC"){

    if(is.null(Vars)){

      Vars <- names(Model$marginals.fixed)

    }

    if(Method == "MCMC"){

      Vars %>% map(~Model$marginals.fixed[[.x]] %>%

                     inla.rmarginal(marginal = ., 10^Decimals) %>%

                     PCalc %>% return

      ) -> Return

    }else if(Method == "INLA"){

      Vars %>% map(~Model$marginals.fixed[[.x]] %>%

                     (1 - inla.pmarginal(0, marginal = .))*2 %>%

                     return

      ) -> Return

    }

    names(Return) <- Vars

    return(Return)

  }
