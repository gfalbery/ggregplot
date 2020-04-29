INLADICFig <- function(ModelList,
                       ModelNames = NULL,
                       Just = F, Order = F,
                       Delta = F,
                       Legend = F,
                       Responses = NULL,
                       CutOff = 2, OverPlot = F){

  if(!OverPlot){

    if(!class(ModelList)=="list"){

      ModelList <- list(ModelList)

    }

    df <- data.frame(Model = 1:length(ModelList),

                     DIC = sapply(ModelList, function(m) m$dic$dic))

    if(is.null(ModelNames)){

      ModelNames <- 1:length(ModelList)

    }

    df$ModelName <- ModelNames

    df$Competitive <- with(df, ifelse(DIC < min(DIC + CutOff), "Y", "N"))

    if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

    if(Order){

      df %>% arrange(desc(DIC)) %>%
        mutate_at("Model", ~factor(.x, levels = .x)) %>%
        mutate_at("ModelName", ~factor(.x, levels = .x)) -> df

    }

    if(Delta){

      df %>% mutate_at("DIC", ~.x - min(.x)) -> df

    }

    DICPlot <- ggplot(df, aes(as.numeric(as.factor(ModelName)), DIC))

    if(Delta){

      DICPlot <- DICPlot + geom_hline(yintercept = 0, lty = 2, alpha = 0.6)

    }

    DICPlot <- DICPlot +
      geom_point(aes(shape = Competitive)) +
      scale_shape_manual(values = c(1,2)) +
      geom_line() +
      labs(x = "Model") +
      scale_x_continuous(breaks = as.numeric(as.factor(df$ModelName)), labels = df$ModelName) +
      geom_point(data = df[df$Competitive == "Y",]) +
      theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

    if(!Legend){

      DICPlot <- DICPlot + theme(legend.position = "none")

    }

    return(DICPlot)

  }else{

    if(is.null(ModelNames)){

      ModelNames <- 1:length(ModelList[[1]])

    }

    1:length(ModelList) %>%
      map_dfc(~sapply(ModelList[[.x]], MDIC)) %>%
      mutate(Model = 1:n()) ->

      df

    if(is.null(Responses)){

      Responses <- paste0("Resp.", 1:length(ModelList))

    }

    colnames(df)[!(colnames(df) == "Model")] <- Responses

    df$ModelName <- ModelNames

    df %>% gather("Response", "DIC", -c(Model, ModelName)) -> df

    df %>% group_by(Response) %>%
      mutate(Competitive = ifelse(DIC < min(DIC + CutOff), "Y", "N")) ->

      df

    if(Just){ Angle = 45; Hjust = 1 }else{ Angle = 0; Hjust = 0.5}

    if(Order){

      df %>% arrange(Response, desc(DIC)) %>%
        mutate_at("Model", ~factor(.x, levels = .x)) %>%
        mutate_at("ModelName", ~factor(.x, levels = .x)) ->

        df

    }

    if(Delta){

      df %>%
        group_by(Response) %>%
        mutate_at("DIC", ~.x - min(.x)) -> df

    }

    DICPlot <- ggplot(df, aes(as.numeric(as.factor(ModelName)), DIC))

    if(Delta){

      DICPlot <- DICPlot + geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
        labs(y = "DeltaDIC")

    }

    DICPlot <- DICPlot +
      geom_point(aes(shape = Competitive)) +
      scale_shape_manual(values = c(1,2)) +
      geom_line(aes(group = Response, colour = Response)) +
      labs(x = "Model") +
      scale_x_continuous(breaks = as.numeric(as.factor(df$ModelName)), labels = df$ModelName) +
      geom_point(data = df[df$Competitive == "Y",]) +
      theme(axis.text.x = element_text(angle = Angle, hjust = Hjust))

    if(!Legend){

      DICPlot <- DICPlot + theme(legend.position = "none")

    }

    return(DICPlot)

  }
}
