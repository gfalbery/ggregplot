# Establishing Themes and Palettes ####

# requireNamespace("ggplot2", quietly = TRUE)
# requireNamespace("RColorBrewer", quietly = TRUE)

ParasitePalettes <- c("PuRd","PuBu","BuGn","Purples","Oranges")
ParasiteColours  <- c("#DD1c77","#2B8CBE","#2CA25F",
                   RColorBrewer::brewer.pal(5,"Purples")[4],
                   RColorBrewer::brewer.pal(5,"Oranges")[4])

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
AlberColours[length(AlberColours)+1:2] <- RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]

AlberTheme <- ggplot2::theme_bw() +
  ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = -0.35,
                                    size = 12,
                                    colour = "black"),
        axis.title.y = ggplot2::element_text(vjust = 1.2,
                                    size = 12,
                                    colour = "black"),
        strip.background = ggplot2::element_rect(fill = "white", colour = "dark grey"))

ggplot2::theme_set(AlberTheme)
