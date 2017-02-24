#' @title plot of dekadal amount
#' @description plot of dekadal amount and save as a png file
#' @author Adrian Huerta & Carlos Fernandez
#' @param r a raster object
#' @param dir_save_file a character object (directory where is saved)
#' @param name_file a character object
#' @return a png file
#' @importFrom raster reclassify values
#' @importFrom sp spplot sp.polygons sp.points sp.text
#' @importFrom latticeExtra layer
#' @importFrom maptools sp.pointLabel
#' @importFrom ggplot2 qplot theme_bw theme element_blank annotation_raster geom_rect geom_text xlim ylim aes
#' @importFrom grid unit grid.newpage viewport pushViewport upViewport
#' @importFrom grDevices png dev.off
#' @export

plot_dekadal_amount <- function(r = dekadal_cal(),
                                dir_save_file = "C:/Users/usuario 01/Desktop/DEK_files",
                                name_file = make_raster_filename()$raster_dek_png) {

  data("sudamerica.shp", package = "MIDAS")
  data("rios.shp", package = "MIDAS")
  data("oceano.shp", package = "MIDAS")
  data("logo_senamhi", package = "MIDAS")
  data("desierto", package = "MIDAS")
  data("lago.shp", package = "MIDAS")
  data("depart", package = "MIDAS")
  data("capital.shp", package = "MIDAS")

  if( substr(name_file, 12 ,12) == "1"){
    ttt <- "1era Decada"
  } else if ( substr(name_file, 12 ,12) == "2") {
    ttt <- "2da Decada"
  } else {
    ttt <- "3ra Decada"
  }

  main_tittle <- paste("TOTAL ACUMULADO DE PRECIPITACIÓN \n", paste(ttt, " " ,substr(name_file, 9 ,10),"-"  ,substr(name_file, 5 ,8), sep = ""), sep = "")


  # RECLASIFICAR SEGUN LAS SIGUIENTES CATEGORIAS EL SPI
  clase <- matrix(c(-Inf, 5, 1,
                    5, 20, 2,
                    20, 50, 3,
                    50, 100, 4,
                    100, 150, 5,
                    150, 200, 6,
                    200, 300, 7,
                    300, Inf, 8), ncol = 3, byrow = TRUE)

  r <- reclassify(r, clase)
  raster::values(r) <- as.factor(raster::values(r))

  labelat = c(1, 2, 3, 4, 5, 6, 7, 8)
  labeltext = c('0-5', '5-20', "20-50",'50-100', '100-150',"150-200", "200-300", ">300")

  p_drought <- c("white","blue","cyan","chartreuse4","lawngreen","yellow","orange","red")

  ############# MAP

  fig.spi <- spplot(r,margin=FALSE,xlab="",ylab="",col.regions=p_drought,xlim=c(-82, -67.5), ylim=c(-19,0),
                    scales = list(x = list(alternating=1,relation="same",at = seq(-84, -68, by = 4), rot = 0),y=list(relation="same",at = seq(-18, 1, by = 4), rot = 90)),
                    colorkey=list(labels=list(at = labelat, labels = labeltext), title="?ndice",space="bottom",width=1.5,height=1)) +
    #agregando capas de shapefiles
    #layer(sp.polygons(PISCO_CONF, lwd=0.1, col=c("gray60","transparent"),fill = c("gray60","transparent"),alpha=c(1,1)))+
    latticeExtra::layer(sp::sp.polygons(sudamerica.shp, lwd=0.8, col='darkgray',fill="gray")) +
    #layer(sp.polygons(sudamerica_shp, lwd=0.8, col='darkgray',fill="gray")) +
    latticeExtra::layer(sp::sp.polygons(lago.shp, lwd=0.8, col='deepskyblue',fill = "deepskyblue",fisrt=FALSE)) +

    latticeExtra::layer(sp::sp.polygons(oceano.shp, lwd=0.05, col=NA,fill = "deepskyblue",alpha=1))+
    latticeExtra::layer(sp::sp.polygons(desierto, lwd=0.1, col=NA,fill = "gray30",alpha=1)) +
    #layer(sp.lines(rios.shp, lwd=0.8, col='steelblue',alpha=0.9,fisrt=FALSE))+
    latticeExtra::layer(sp::sp.polygons(depart, lwd=0.9, col='gray20',fill="transparent")) +
    latticeExtra::layer(sp::sp.points( capital.shp, pch=19, cex=.8, col='midnightblue')) +


    latticeExtra::layer(sp::sp.text(loc = c(-77,-1), txt = "Ecuador",col='white'))+
    latticeExtra::layer(sp::sp.text(loc = c(-71.5,-1), txt = "Colombia",col='white'))+
    latticeExtra::layer(sp::sp.text(loc = c(-71,-8), txt = "Brasil",col='white'))+
    latticeExtra::layer(sp::sp.text(loc = c(-68.2,-15), txt = "Bolivia",col='white'))+
    latticeExtra::layer(sp::sp.text(loc = c(-70,-18.7), txt = "Chile",col='white'))+
    latticeExtra::layer(sp::sp.text(loc = c(-79.5,-10), txt = "Océano Pacífico",col='white',srt=-60,cex=0.9))+
    latticeExtra::layer(sp::sp.text(loc = c(-74,-16.5), txt = "Océano Pacífico",col='white',srt=-25,cex=0.9))+
    latticeExtra::layer(sp::sp.text(loc = c(-74.5,-9.5), txt = "Perú",col='white',cex=1.5))+

    latticeExtra::layer(maptools::sp.pointLabel(capital.shp, label=capital.shp$NOMBREDD,cex=0.7, col='midnightblue',fontface=2))

  #fig.spi <- update(fig.spi,key = list(x=0,y=0.07,cex.title=1,title='Leyenda', columns=2,rect = list(col=c("yellow","blue"), fill = c("yellow","blue"),alpha=0.5), text = list(c("Desierto","Area de estimacisn no confiable","FFF"))))
  fig.spi <- update(fig.spi,key = list(x=0,y=0.1,cex.title=1,title='Leyenda', columns=1,


                                       rect = list(col=c("gray30","gray60"), fill = c("gray30","gray60"),alpha=c(1.0,1.0)),
                                       text = list(c("Desierto","Estimación \nde SPI no confiable")),

                                       lines=list(lty=c(1,1),col=c("steelblue","gray20"),alpha=1),
                                       text = list(c("Ríos","Límites \nde departamento")),

                                       points=list(pch=c(16,16),col=c("transparent","black")),
                                       text = list(c("","Capital del \n departamento"))
  ))

  fig.spi

  #--------

  # FIGURA DE LOGO DE SENAMHI - MEMBRETE

  mypng <- logo_senamhi

  fig.logo <- qplot(mpg, wt, data = mtcars) + theme_bw()+xlim(0,10)+ylim(0,2) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.position="none",
          plot.margin=unit(c(0,0,-0.5,-0.5), "cm")

    ) +
    annotation_raster(mypng, ymin = 0,ymax= 2,xmin = 8,xmax = 10) +
    #geom_rect(aes(NULL, NULL, xmin = 0),xmax = 10,ymin = 4, ymax = 8,fill="transparent",col="gray",size=0.5)+
    #geom_rect(aes(NULL, NULL, xmin = 0),xmax = 10,ymin = 2, ymax = 4,fill="transparent",col="gray",size=0.5)+

    geom_rect(aes(NULL, NULL, xmin = 0),xmax = 10,ymin = 0, ymax = 2,fill="transparent",col="gray",size=0.5)+

    geom_text(aes(4, 1, label= main_tittle ), col="black",fontface=12,size=5)
  #geom_text(aes(5, 1, label="?ndice Estandarizado de Precipitaci?n (SPI - 3 Meses)  Enero 2016"), col="gray20",size=5.5,fontface=14)

  name_file_s <- file.path(dir_save_file, name_file)

  # fig.logo

  #  "POTENCIAL DE INUNDACI?N PARA 10 D?AS DE ACUMULACI?N \n 19 DE DICIEMBRE DEL 2016"
  #  "LLUVIA PARA 10 D?AS DE ACUMULACI?N \n 19 DE DICIEMBRE DEL 2016"
  #multiple plot <<<<<<<<< guardar en pdf tama?o 9"*12.5"
  #png("Plot_lluvia2.png",res=200,width = 2338, height = 3307)
  png( name_file_s ,res=250,width = 2800, height = 3500)

  grid.newpage()
  #------
  vp1 <- viewport(x = 0.0, y = 0.0, height = 0.95, width = 1.0,just = c("left", "bottom"),name = "upper left")
  pushViewport(vp1);print(fig.spi, newpage = FALSE);upViewport(1)

  #------
  vp2 <- viewport(x = 0.075, y = 0.93, height = 0.07, width = 0.875,just = c("left", "bottom"),name = "lower left")
  pushViewport(vp2);print(fig.logo, newpage = FALSE);upViewport(1)

  dev.off()

}
