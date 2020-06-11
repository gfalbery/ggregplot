
# Libra Functions ####

# library(MapPalettes)

RandomSlice <- function(a, Max = NULL){

  if(is.null(Max)){

    Max <- nrow(a)

  }

  if((Max>nrow(a))){

    Max <- nrow(a)

  }

  Sample <- sample(1:nrow(a), Max)

  slice(a, Sample)

}

AsBinary <- function(a){as.numeric(a>0)}


CamelConvert <- function(a){

  substr(a, 1, 1) <- substr(a, 1, 1) %>% toupper

  a

}


GetBaseLevels <- function(Covar, TestDF){

  sapply(Covar, function(a){

    if(is.factor(TestDF[,a])){

      paste0(a,levels(TestDF[,a])[1])

    }else{

      paste0(a,unique(TestDF[,a])[1])

    }

  }) %>% unlist
}

GregLog <- function(a, LogFun = log){

  ifelse(abs(a) <= 1, 0,
         ifelse(a > 0,
                LogFun(a),
                - LogFun(abs(a))))

}

GregCube <- function(a, LogFun = function(b) b^(1/3)){

  ifelse(a == 0, 0,
         ifelse(a > 0,
                LogFun(a),
                - LogFun(abs(a))))

}

GregHeader <- function(Data, Rows = 1){

  Names <- Data[Rows, ] %>% unlist %>% as.character

  names(Data) <- Names

  Data <- Data[-c(1:Rows),]

  return(Data)

}


KMConvert <- function(Data, Longitude, Latitude){

  require(sp)

  Data %>% filter({{Longitude}} %in% c(min({{Longitude}}), max({{Longitude}}))) %>%
    select({{Longitude}}, {{Latitude}}) %>% as.matrix %>%
    spDists(longlat = T) %>% c -> LongitudeDistances

  Data %>% filter({{Latitude}} %in% c(min({{Latitude}}), max({{Latitude}}))) %>%
    select({{Longitude}}, {{Latitude}}) %>% as.matrix %>%
    spDists(longlat = T) %>% c -> LatitudeDistances

  LongitudeDistances %>% max -> XStandard
  LatitudeDistances %>% max -> YStandard

  Data %>% mutate(Longitude = {{Longitude}} - min({{Longitude}}),
                  Latitude = {{Latitude}} - min({{Latitude}})) %>%
    mutate(Longitude = {{Longitude}}/max({{Longitude}}),
           Latitude = {{Latitude}}/max({{Latitude}})) %>%
    mutate(Longitude = XStandard*{{Longitude}},
           Latitude = YStandard*{{Latitude}}) %>%

    select(Longitude, Latitude) %>%

    return

}

KMConvert <- function(Data, Longitude, Latitude, Correction = F){

  require(sp)

  Data %>% filter({{Longitude}} %in% c(min({{Longitude}}), max({{Longitude}}))) %>%
    mutate(Latitude = mean({{Latitude}})) %>%
    select({{Longitude}}, Latitude) %>%
    as.matrix %>%
    spDists(longlat = T) %>% c ->

    LongitudeDistances

  Data %>% filter({{Latitude}} %in% c(min({{Latitude}}), max({{Latitude}}))) %>%
    mutate(Longitude = mean({{Longitude}})) %>%
    select(Longitude, {{Latitude}}) %>%
    as.matrix %>%
    spDists(longlat = T) %>% c ->

    LatitudeDistances

  LongitudeDistances %>% max -> XStandard
  LatitudeDistances %>% max -> YStandard

  Data %>% mutate(Longitude = {{Longitude}} - min({{Longitude}}),
                  Latitude = {{Latitude}} - min({{Latitude}})) %>%

    mutate(Longitude = Longitude/max(Longitude),
           Latitude = Latitude/max(Latitude)) %>%

    mutate(Longitude = XStandard*Longitude,
           Latitude = YStandard*Latitude) %>%

    select(Longitude, Latitude) %>%

    return

}

ExtentGet <- function(df, X = NULL, Y = NULL, Return = "Values"){

  if(!is.null(X)){

    df$X <- df[,X]
    df$Y <- df[,Y]

  }else{

    names(df) <- c("X", "Y")

  }

  ymin <- min(df$Y, na.rm = T)
  xmin <- min(df$X, na.rm = T)
  ymax <- max(df$Y, na.rm = T)
  xmax <- max(df$X, na.rm = T)

  if(Return == "Values"){


    data.frame(

      XMin = xmin, XMax = xmax,
      YMin = ymin, YMax = ymax

    ) %>% return

  }else if(Return == "Rectangle"){

    data.frame(

      X = c(xmin, xmax, xmax, xmin),
      Y = c(ymax, ymax, ymin, ymin)

    ) %>% return

  }
}

INLAMarginalPlot <- function(Model, Vars = NULL, Colour = "black"){

  Model$marginals.hyperpar %>% map_dfr(as.data.frame, .id = "Effect") -> MarginalDF

  if(is.null(Vars)){

    Vars <- unique(MarginalDF$Effect)

  }

  Plot <- MarginalDF %>% filter(Effect %in% Vars) %>%
    ggplot(aes(x, y))

  if(length(Vars)>1){

    Plot <- Plot + geom_line(aes(colour = Effect)) + facet_wrap(~Effect, scales = "free")

  }else{

    Plot <- Plot + geom_line(colour = Colour)

  }

  return(Plot)

}

GetCovariateList <- function(Data, Covar){

  NLevels <- Data %>% summarise_at(Covar, nunique)

  SubCovar <- Covar[NLevels>1]

  return(SubCovar)

}

GetEstimates <- function(Model, Variable, Mode = "Character", Round = 3,
                         NDraws = 1, Draws = F){

  if(!Draws){

    Estimates <- Model$summary.fixed[Variable, c("mean", "0.025quant", "0.975quant")]

    Estimates <- round(Estimates, Round)

    if(Mode == "Character"){

      paste0(Estimates[1], " (", Estimates[2], ", ", Estimates[3], ")") %>% return

    }else if(Mode == "Numeric"){

      Estimates %>% return

    }

  }else{

    Model$marginals.fixed[[Variable]] %>% inla.rmarginal(NDraws, .)

  }
}

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])

AlberColours[length(AlberColours)+1:2] <-

  RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]

AlberColours <- append(AlberColours, list(Pink = "#FD6396", Blue = "#3C78D8")) %>% unlist

#FirePalette <-
#  get_colors_from_image(
#    "https://raw.githubusercontent.com/HughSt/mappalettes/master/images/nathan-lindahl-1j18807_ul0-unsplash.jpg",
#    5)

FirePalette <- c("#0C080B", "#142D40", "#425662", "#B3330C", "#F8A14B")

Palette2 <- colorRampPalette(FirePalette)




