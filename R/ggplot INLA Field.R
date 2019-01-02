# ggplot for INLA fields ####
require(INLA); require(ggplot2)

xlim <- c(xmin=1350, xmax=1390)
ylim <- c(ymin=7995, ymax=8050)

THEME<-theme(axis.text.x=element_text(size=12,colour="black"),
             axis.text.y=element_text(size=12,colour="black"))+
  theme(axis.title.x=element_text(vjust=-0.35),
        axis.title.y=element_text(vjust=1.2))+
  theme_bw()


ggField <- function(Model, Mesh, Groups = 1){#, xlim, ylim){

  require(ggplot2); require(INLA)

  Projection <- inla.mesh.projector(Mesh,
                                    #xlim = xlim,
                                    #ylim = ylim,
                                    dims = c(300, 300))

  Full.Projection <- expand.grid(x = Projection$x, y = Projection$y)

  Dim1 <- dim(Full.Projection)[1]

  #Full.Projection <- Full.Projection[rep(rownames(Full.Projection),Groups),]

  if(Groups ==1){
  Full.Projection$value <- c(inla.mesh.project(Projection, Model$summary.random$w$mean))
  }else{
  Full.Projection[,paste0("Group",1:Groups)]<-apply(
    matrix(Model$summary.random$w$mean,ncol=Groups), 2,
    function(x) c(inla.mesh.project(Projection,x)))

  Full.Projection <- reshape2::melt(Full.Projection, id.vars = c(names(Full.Projection)[-which(names(Full.Projection)%in%paste0("Group",1:Groups))]))
  }

  Full.Projection$Fill <- cut(Full.Projection$value,
                              breaks = quantile(Full.Projection$value, 0:9/9, na.rm = T),
                              labels = c(round(quantile(Full.Projection$value, 0:9/9, na.rm = T), 2)[1:9]))

  Full.Projection$Group <- rep(1:Groups, each = Dim1)


  Full.Projection <- na.omit(Full.Projection)

  return(ggplot(Full.Projection,aes(x, y, fill = Fill))+
           #lims(x = xlim, y = ylim) +
           facet_wrap( ~ Group) +
           geom_tile(colour = "black") +
           geom_tile(colour = NA) + guides(fill = guide_legend(reverse = T)) +
           coord_fixed() + labs(fill = "Mean") +
           THEME + theme(strip.background = element_rect(fill = "white")) +
           #geom_path(data=rbind(boundary,boundary[1,]),inherit.aes=F,aes(Easting,Northing),size=1,colour="black") +
           labs(x = "Easting", y = "Northing")
  )

}
