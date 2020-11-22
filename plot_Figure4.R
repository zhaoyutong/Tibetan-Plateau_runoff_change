#rm(list=ls())
library(raster)
library(maptools)
library(RColorBrewer)
library(circlize)

####################read map################
{
  path_fig = '/Users/zhaoyutong/Desktop/data/runoff/V3/'
  transmit_time = c(10,10,10,10,10,10);target_number=500
  path_gis = '/Users/zhaoyutong/Desktop/龙王庙/Response/TP_basin_boundary/'
  TP_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
  path_gis_all = '/Users/zhaoyutong/Desktop/data/allbasin/'
  asian_tower = rgdal::readOGR(paste(path_gis_all,"Asiantower.shp",sep=""))
  all_country = rgdal::readOGR(paste(path_gis_all,"allcountry.shp",sep=""))
  Ganges_all = rgdal::readOGR(paste(path_gis_all,"Ganges.shp",sep=""))
  Bra_all = rgdal::readOGR(paste(path_gis_all,"Brahmaputra.shp",sep=""))
  Sal_mek_all = rgdal::readOGR(paste(path_gis_all,"Sal-mek.shp",sep=""))
  Yangze_all = rgdal::readOGR(paste(path_gis_all,"Yangtze.shp",sep=""))
}
###initial figure property setting
path_pics = '/Users/zhaoyutong/Desktop/final/'
pdf(
  file = paste(path_pics,"_fig4_1.pdf", sep = ""),
  width = 7.09,
  height = 4,
  pointsize = 7,
  family = 'Helvetica'
)

colorall = c('black','#8BA4EF','#FFBC84','#DE5F61')
col_population='#6D4C41'
lonlabels<-c(expression('70'*degree*'E'),expression('90'*degree*'E'),expression('110'*degree*'E'))
latlabels<-c(expression('10'*degree*'N'),expression('25'*degree*'N'),expression('40'*degree*'N'))
{
  par( oma = c(4, 4, 2, 2),mar = c(0, 0, 0, 0))
  position1 = c(0.01,0.99,0.01,0.99)
  position2 = c(0.01,0.99,0.01,0.99)
  x_width=0.21;y_height = x_width*(pic_height/pic_width)*2
  #indus
  x4 = 0.06;y4 = 0.72;position4 = c(x4,(x4+x_width),y4,(y4+y_height))
  #bra
  x5 = 0.36;y5 = 0.72;position5 = c(x5,(x5+x_width),y5,(y5+y_height))
  #gane
  x6 = 0.32;y6 = 0.1;position6 = c(x6,(x6+x_width),y6,(y6+y_height))
  #sal-mek
  x8 = 0.73;y8 = 0.05;position8 = c(x8,(x8+x_width),y8,(y8+y_height))
  #
  x9 = 0.73;y9 = 0.38;position9 = c(x9,(x9+x_width),y9,(y9+y_height))
  #yellow
  x10 = 0.73;y10 = 0.72;position10 = c(x10,(x10+x_width),y10,(y10+y_height))
  position11 = c(0,0.2,0,0.25)
  position = rbind(position1,position2,position4,position6,position5,position8,position9,position10,position11)#position3,
  cexaxissize = cex_text=1;
}


{
  ####plot the all map
  par(fig=position[2,], new=F)
  plot(asian_tower,lwd=0.5,col = rgb(253/255, 211/255, 55/255,0.4),
       xlim=c(75,110),ylim=c(9,47))
  rect(50,0,150,60,border = rgb(237/255,246/255,253/255),
       col=rgb(237/255,246/255,253/255))
  
  plot(all_country,lwd=1,
       col = "gray90",
       border="gray90",add=T)
  plot(TP_shp,lwd=1,
       col = "gray70",
       border = "gray70",add=T)
  plot(asian_tower,lwd=0.7,
       border=rgb(1,1,1,0.4),add=T)
  
  ratio_back = 0.8;lwd_shp=0.8
  border_col = rgb(1,1,1,0.4)
  cols = rep(rgb(253/255, 211/255, 55/255,ratio_back),11);
  lineinterval_x=0;lineinterval_y=0
  axis(side=1,at=seq(70,110,20),labels=F,tck=-0.02,lwd=1,line = lineinterval_x)
  axis(side=2,at=seq(10,40,15),labels=F,tck=-0.02,lwd=1,line=lineinterval_y)
  mtext(side=1,at=seq(70,110,20),lonlabels,cex=cexaxissize,line=(lineinterval_x+0.7))
  mtext(side=2,at=seq(10,40,15),latlabels,cex=cexaxissize,line=(lineinterval_y+0.7),las=2)
  mtext(side=2,at=25,"Latitude",cex=cexaxissize,line=(lineinterval_y+2.7),las=0)
  mtext(side=1,at=90,"Longitude",cex=cexaxissize,line=(lineinterval_x+1.5))
  box(lwd=1)
}

{
  ####################plot basin shp and transmit time###########################
  
  path_V2 = '/Users/zhaoyutong/Desktop/data/runoff/V2/'
  lon_used = seq(60,124,0.25);lat_used = seq(43,8-0.25,-0.25)
  nlons_used = length(lon_used);nlats_used = length(lat_used);
  for (iriver in 1:length(rivers_name)) {#
    up_shp = rgdal::readOGR(paste(path_gis,rivers_name[iriver],".shp",sep=""))
    tibet_raster <- raster(xmn=min(lon_used),xmx=max(lon_used),ymn=min(lat_used),ymx=max(lat_used),
                           ncol=nlons_used,nrow=nlats_used)
    rivers_region  <- rasterize(up_shp,tibet_raster)
    rivers_matrix =t(as.matrix(rivers_region))
    rivers_matrix[which(is.finite(rivers_matrix)==F)]=NaN
    rivers_matrix[which(is.finite(rivers_matrix)==T)]=1
    
    influnce_matrix_used = array(NaN,dim = c(nlons_used,nlats_used))
    result = load(paste(path_V2,"all_",rivers_name[iriver],"_riverbasin_transmit_time.Rdata",sep=""))
    lon1 = min(lon_region) - 0.125;lon2 = max(lon_region) - 0.125
    lat1 = max(lat_region) - 0.125;lat2 = min(lat_region) - 0.125
    indexlon1 = (lon1-60)/0.25+1;indexlon2 = (lon2-60)/0.25+1
    indexlat1 = (43-lat1)/0.25+1;indexlat2 = (43-lat2)/0.25+1
    influnce_matrix_used[indexlon1:indexlon2,indexlat1:indexlat2] = r2.matrix_smooth
    
    
    influnce_matrix = influnce_matrix_used
    influence_region_s = array(NaN,dim = dim(influnce_matrix))
    for (iii in 1:10) {
      for (ilon in 2:(nlons_used-1)) {
        for (ilat in 2:(nlats_used-1)) {
          if(is.finite(influnce_matrix[ilon,ilat] )==T){
            influence_region_s[ilon,ilat] =  sum(influnce_matrix[ilon-1,ilat+1],influnce_matrix[ilon,ilat+1],influnce_matrix[ilon+1,ilat+1],
                                                 influnce_matrix[ilon-1,ilat],influnce_matrix[ilon,ilat],influnce_matrix[ilon+1,ilat],
                                                 influnce_matrix[ilon-1,ilat-1],influnce_matrix[ilon,ilat-1],influnce_matrix[ilon+1,ilat-1],
                                                 na.rm=T)/9
          }
        }
      }
      iii=iii+1;
      influnce_matrix=influence_region_s
    }
    
    index = which(influence_region_s <= transmit_time[iriver])
    influence_region = array(NaN,dim = c(nlons_used,nlats_used))
    influence_region[index] = 1
    
    influence_region_used = influence_region
    
    
    for (ilon in 1:nlons_plot) {
      for (ilat in 1:nlats_plot) {
        if(is.finite(influence_region_used[ilon,ilat])>0.5|is.finite(rivers_matrix[ilon,ilat])==T){
          points(lon_used[ilon],lat_used[ilat],col=rgb(157/255,206/255,240/255),
                 cex=1,pch=15)
        }
      }
    }
    
    
    lwd_shp = 1;cols_up=rgb(23/255, 155/255, 215/255);lty_shp = 1
    plot(Indus_shp,add=T,col = cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(Ganges_shp,add=T,col = cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(Brahmaputra_shp,add=T,col = cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(MekSal_shp,add=T,col =cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(Yangtze_shp,add=T,col = cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(Yellow_shp,add=T,col = cols_up,border=cols_up,lwd=lwd_shp,lty=lty_shp)
    plot(asian_tower,lwd=1,
         border=rgb(1,1,1,0.4),add=T)
    
  }
}

{
  ####write basin name
  par(usr=c(0,1,0,1))
  rivers_name_used <- c("Indus","Ganges","Brahmaputra","Salween and Mekong","Yangtze","Yellow")
  text_location = array(NaN,dim = c(length(rivers_name_used),2))
  text_location[1,] = c(0.28,0.66);
  text_location[2,] = c(0.38,0.5);
  text_location[3,] = c(0.5,0.52);
  text_location[4,] = c(0.6,0.37);
  text_location[5,] = c(0.65,0.55);
  text_location[6,] = c(0.65,0.71);
  
  srt_location = c(0,0,0,290,0,0)
  for (iriver in 1:length(rivers_name_used)) {
    text(text_location[iriver,1],text_location[iriver,2],rivers_name_used[iriver],
         cex=1,col="black",srt=srt_location[iriver])
  }
}


{
  #######plot water crowed index
  abctitle = c("a","c","b","f","e","d")
  rivers_name_used = c("Indus","Brahmaputra","Ganges","Salween and Mekong","Yangtze","Yellow")
  for (ipic in 3:8) {
    par(fig=position[ipic,], new=T)
    par(usr=c(0,1,0,ymax))
    plot(srt_location,type="n",axes=F,xant=F,yant=F)
    percent  = as.matrix(water_indiacator_used[(ipic-2),])
    percent_pos = as.matrix(population_pos[(ipic-2),])
    percent_neg = as.matrix(population_neg[(ipic-2),])
    x0_point = 0.18;width = 0.05;x_break = 0.23
    for (irect in 1:4) {
      cols = colorall[irect]
      par(usr=c(0,1,0,ymax))
      x_point = x0_point+(irect-1)*x_break
      rect((x_point-width),0,(x_point+width),percent[irect],
           col = cols,border = cols)
    }
    par(usr=c(0,1,0,ymax))
    terval_y = 0;
    axis(side=2,at=seq(0,ymax,breaks),labels=F,tck=-0.03,lwd=1,line=terval_y,col="black")
    mtext(side=2,at=seq(0,ymax,breaks),seq(0,ymax,breaks),cex=cexaxissize,
          line=(terval_y+0.3),col="black",las=1)
    box()
    par(usr=c(0,1,0,1))
    text(0.02,0.9,abctitle[ipic-2],adj=0,font=2,cex=cexaxissize)
    text(0.08,0.9,rivers_name_used[ipic-2],adj=0,font=1,cex=cexaxissize)
  }
}


{
  ####plot population
  result_pop=load(paste0(path_fig,target_number,'_population_percent_allyear.Rdata'))
  colnames(pop_percent) = rivers_name_used
  pop_percent = round(pop_percent,2)
  write.csv(pop_percent,
            paste0(path_fig,target_number,'_population_percent_allyear.csv'))
  ymax = 310;breaks=100
  water_indiacator_used = t(pop_percent)
  population_pos = t(pop_percent_p)
  population_neg = t(pop_percent_n)
  result_pop = ssp_influence_region_used/10^6
  pop_arr_used = round(result_pop,2)
  ymax_pop = 310;
  
  x1_point = 0.17;x_break1=0.23
  points_x = c(x1_point,x1_point+x_break1,x1_point+x_break1*2.1,x1_point+x_break1*3.1)
  abctitle = c("a","b","c","d","e","f")
  rivers_name_used = c("Indus","Brahmaputra","Ganges","Salween and Mekong","Yangtze","Yellow")
  for (ipic in 3:8) {
    par(fig=position[ipic,], new=T)
    if(ipic==3){
      ymin_pop = 50
      ymax_pop = 190
      breaks_y = 50
    }
    if(ipic==4){
      ymin_pop = 40
      ymax_pop = 100
      breaks_y = 30
    }
    if(ipic==5){
      ymin_pop = 150
      ymax_pop = 490
      breaks_y = 150
    }
    if(ipic==6){
      ymin_pop = 20
      ymax_pop = 70
      breaks_y = 20
    }
    if(ipic==7){
      ymin_pop = 20
      ymax_pop = 140
      breaks_y = 40
    }
    if(ipic==8){
      ymin_pop = 10
      ymax_pop = 50
      breaks_y = 15
    }
    par(usr=c(0,1,0,ymax_pop))
    pop_number = pop_arr_used[(ipic-2),]
    plot(points_x,pop_number,type="l",col="gray40",
         lty=2,axes=F,xant=F,yant=F,xlim=c(0,1),ylim=c(0,ymax_pop),
         xaxs="i",yaxs="i")
    points(points_x,pop_number,col=col_population,axes=F,xant=F,yant=F,cex=1.8,pch=16)
    
    terval_y1 = 0
    axis(side=4,at=seq(0,ymax_pop,breaks_y),labels=F,tck=-0.03,lwd=1.2,line=terval_y1,col=col_population)
    mtext(side=4,at=seq(0,ymax_pop,breaks_y),seq(0,ymax_pop,breaks_y),cex=cexaxissize,
          line=(terval_y1+0.3),col=col_population,las=1)
    segments(1,0,1,ymax_pop,col=col_population,lwd=2)
    
  }
}


{
  #####plot legend
  scales=0.7
  par(fig=c(0.01,0.28,0.0,0.4), new=T)
  par(usr=c(0,1,0,1))
  cols_rect = rgb(213/255,219/255,201/255,0.4)
  plot(srt_location,type="n",axes=F,xant=F,yant=F)
  y0 = 270;height = 20;ybreak=10;width = 18*0.8;
  xbreak = 1.5;xbreak2=3.3
  rect(0,0,300,300,col = cols_rect,border = cols_rect,lwd=2)
  
  rect_w = 0.6*0.8
  rect(xbreak,y0,rect_w+xbreak,y0+width,border = "white",col="transparent",lwd=2)
  rect(xbreak2,y0,rect_w+xbreak2,y0+width,border = "transparent",col="gray50",lwd=2)
  
  rect(xbreak,y0-height-ybreak,rect_w+xbreak,y0+width-height-ybreak,border = cols_up,col=cols_up,lwd=1,lty=31)
  rect(xbreak2,y0-height-ybreak,rect_w+xbreak2,y0+width-height-ybreak,border = "transparent",col=rgb(157/255,206/255,240/255))
  
  xbreak_text = 0.2;x0 = 1.95;
  text(x0,y0+width/2-3,"All basin",cex = cexaxissize*scales,col="black",pos=4)
  text(x0+1.75,y0+width/2-3,"Tibetan Plateau",cex =cexaxissize*scales ,col="black",pos=4)
  text(x0,y0+(width/2-height-ybreak)-3,"Upstream",cex = cexaxissize*scales,col="black",pos=4)
  text(x0+1.75,y0+(width/2-height-ybreak)-3,"Downstream area",cex = cexaxissize*scales,col="black",pos=4)
  
  xx = 0.07;yy = 0.09;
  par(fig=c(xx,(xx+x_width*0.75),yy,(yy+y_height*scales)), new=T)
  plot(srt_location,type="n",axes=F,xant=F,yant=F)
  box()
  x0_point = 0.18;width = 0.05;x_break = 0.23
  title_text = c("Present",expression(1.5*degree*C),
                 expression(2*degree*C),expression(4*degree*C))
  percent = c(100,180,120,80)
  
  for (irect in 1:4) {
    cols = colorall[irect]
    par(usr=c(0,1,0,ymax))
    x_point = x0_point+(irect-1)*x_break
    rect((x_point-width),0.05,(x_point+width),percent[irect],
         col = cols,border = cols)
    mtext(side=2,at=130,expression("Population living above" ),cex=cexaxissize*scales,
          line=2.3,las=0, adj=0.5)
    
    mtext(side=2,at=130,expression("absolute-scarcity level (%)" ),cex=cexaxissize*scales,
          line=1.4,las=0, adj=0.5)
    axis(side=2,at=seq(0,ymax,breaks),labels=F,tck=-0.05,lwd=0.8,line=0,col="black")
    mtext(side=2,at=seq(0,ymax,breaks),seq(0,ymax,breaks),cex=cexaxissize*scales,
          line=0.2,col="black",las=1)
    text(x_point+width/2,-20,title_text[irect],srt=45,cex=cexaxissize*scales,adj=1, xpd=NA)
  }
  
  
  par(fig=c(xx,(xx+x_width*0.75),yy,(yy+y_height*scales)), new=T)
  ymin_pop = 20
  ymax_pop = 60
  breaks_y = 20
  par(usr=c(0,1,0,ymax_pop))
  pop_number = c(25,20,35,55)
  xstart=x0_point;x_break_legend=x_break
  points_x = c( xstart+(1-1)*x_break_legend,xstart+(2-1)*x_break_legend,xstart+(3-1)*x_break_legend,xstart+(4-1)*x_break_legend)
  plot(points_x,pop_number,type="l",col="gray40",lty=2,axes=F,xant=F,yant=F,xlim=c(0,1),ylim=c(0,ymax_pop))
  #box()
  points(points_x,pop_number,col=col_population,axes=F,xant=F,yant=F,cex=1,pch=16)
  terval_y1 = 0
  axis(side=4,at=seq(0,ymax_pop,breaks_y),labels=F,tck=-0.05,lwd=1.2,line=terval_y1,col=col_population)
  mtext(side=4,at=seq(0,ymax_pop,breaks_y),seq(0,ymax_pop,breaks_y),cex=cexaxissize*scales,
        line=0.4,col=col_population,las=1)
  mtext(side=4,at=30,"Total population",cex=cexaxissize*scales,
        line=0.8,col=col_population,las=0,adj=0.5)
  mtext(side=4,at=30,"(millons)",cex=cexaxissize*scales,
        line=1.5,col=col_population,las=0,adj=0.5)
  
}


dev.off()
