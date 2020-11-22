rm(list=ls())
library(raster)
library(maptools)
library(RColorBrewer)
library(rgdal)
##############################the origiral setting####################
path_pics = '/Users/zhaoyutong/Desktop/final/'
pdf(
  file = paste(path_pics,"_fig1.pdf", sep = ""),
  width = 7.11,
  height = 6.8,
  pointsize = 7,
  family = 'Helvetica'
)
par( oma = c(1, 5, 0, 0),mar = c(0, 0, 0, 0))
cols_used = c(rep(rgb(38/255,89/255,214/255),2),rep('#F8B501',2),rep('#E15B66',2))
#####################################plot map###########################
#####read shp file#####
rivers_name_used <- c('Indus','Ganges','Brahmaputra','Sal-Mek','Yangtze','Yellow')
path_gis = '/Users/zhaoyutong/Desktop/龙王庙/Response/TP_basin_boundary/'
TP_shp = rgdal::readOGR(paste(path_gis,"TibetanPlateau.shp",sep=""))
Brahmaputra_shp = rgdal::readOGR(paste(path_gis,"Brahmaputra.shp",sep=""))
Ganges_shp = rgdal::readOGR(paste(path_gis,"Ganges.shp",sep=""))
Indus_shp = rgdal::readOGR(paste(path_gis,"Indus.shp",sep=""))
Salween_shp = rgdal::readOGR(paste(path_gis,"Sal-Mek.shp",sep=""))
Yangtze_shp = rgdal::readOGR(paste(path_gis,"Yangtze.shp",sep=""))
Yellow_shp = rgdal::readOGR(paste(path_gis,"Yellow.shp",sep=""))

#####the layout ######
nrow = 3;ncol = 2;
width = 0.23;height = 0.10;
xstart=0.23;ystart=0.95;
break1=0.25;break2 = 0.21
position2 = array(NaN,dim = c(nrow*ncol,4))
for (r in 1:nrow) {
  for (c in 1:ncol) {
    position2[(r-1)*ncol+c,1] = xstart + (c-1)*width+(c-1)*break1;
    position2[(r-1)*ncol+c,2] = xstart + c*width+(c-1)*break1;
    position2[(r-1)*ncol+c,3] = ystart - r*height-(r-1)*break2;
    position2[(r-1)*ncol+c,4] = ystart - (r-1)*height-(r-1)*break2;
  }
}
#####plot map#####
for (ipic in 1:6) {
  par(fig=position2[ipic,], new=T)
  if(ipic==1){
    shp_red = Indus_shp
  }
  if(ipic==2){
    shp_red = Ganges_shp
  }
  if(ipic==3){
    shp_red = Brahmaputra_shp
  }
  if(ipic==4){
    shp_red = Salween_shp
  }
  if(ipic==5){
    shp_red = Yangtze_shp
  }
  if(ipic==6){
    shp_red = Yellow_shp
  }
  lwd_shp=0.6
  plot(TP_shp,lwd=lwd_shp,border="gray50")
  cols = rep("gray80",11)
  cols_shp = "gray70"
  plot(shp_red,add=T,col = cols_shp,border=cols_shp,lwd=lwd_shp)
  
}
#################################the plot barplot############
#####the layout#####
nrow = 3;ncol = 2;
width = 0.41;height = 0.19;
xstart=0.01;ystart=0.86;
position1 = array(NaN,dim = c(nrow*ncol,4))
break1=0.1;break2 = 0.1
for (r in 1:nrow) {
  for (c in 1:ncol) {
    position1[(r-1)*ncol+c,1] = xstart + (c-1)*width+(c-1)*break1;
    position1[(r-1)*ncol+c,2] = xstart + c*width+(c-1)*break1;
    position1[(r-1)*ncol+c,3] = ystart - r*height-(r-1)*break2;
    position1[(r-1)*ncol+c,4] = ystart - (r-1)*height-(r-1)*break2;
  }
}
abstitle <- letters[1:24];text_cex=1
for (ipic in 1:6) {
  par(fig=position1[(ipic),], new=T)
  
  ylim_min=-20;ylim_max = 70;breaks=20
  plot(pr_mean,beside=T,ylim=c(ylim_min,ylim_max),xlim=c(1,9),axisnames = F,axes=F,type="n")
  ylim =c(-20,seq(0,(ylim_max),breaks))

  par(usr=c(0,1.03,ylim_min,ylim_max))
  axis(side=2,at=ylim,labels=F,tck=-0.02,lwd=1,line = 0)
  mtext(side=2,at=ylim,ylim,cline=0.4,las=2)

  # the barplot location
  num1 = 0.1;num_width = 0.12;num_interval=0.1
  x_location = c(num1,(num1+num_width-0.01+0.045),
                 (num1+2*num_width+num_interval),(num1+3*num_width+num_interval-0.004+0.038),
                 (num1+4*num_width+2*num_interval+0.01),(num1+5*num_width+2*num_interval+0.004+0.04))
  box_width = 0.04
  x_box_left = x_location - box_width;x_box_right = x_location + box_width
  for (ibar in 1:6) {
    y0=pr_mean[ibar,ipic]
    y1 = pr_mean[ibar,ipic]+pr_sd[ibar,ipic]
    y2 = pr_mean[ibar,ipic]-pr_sd[ibar,ipic]
    number = 0.022
    seg_num = 0.015
    #plot the rect
    rect(x_box_left[ibar]-number,0,x_box_right[ibar]+number,y0,col=cols_used[ibar],border=cols_used[ibar])
    #plot the error bar
    if(ibar==2|ibar==4|ibar==6){
      segments(x_box_left[ibar]-number,0,x_box_right[ibar]+number,y0,lty=1,col="white")
      segments(x_box_left[ibar]-number,y0/2,(x_box_left[ibar]+(x_box_right[ibar]-x_box_left[ibar])/2),y0,lty=1,col="white")
      segments((x_box_left[ibar]+(x_box_right[ibar]-x_box_left[ibar])/2),0,x_box_right[ibar]+number,y0/2,lty=1,col="white")
    }
    if(y0>0){
      segments(x_location[ibar],y2,x_location[ibar],y0,lty=1)
      segments(x_location[ibar],y0,x_location[ibar],y1,lty=1)
      segments(x_location[ibar]-seg_num,y1,x_location[ibar]+seg_num,y1,lty=1)
      segments(x_location[ibar]-seg_num,y2,x_location[ibar]+seg_num,y2,lty=1)
    }else{
      segments(x_location[ibar],y2,x_location[ibar],y0,lty=1)
      segments(x_location[ibar]-seg_num,y2,x_location[ibar]+seg_num,y2,lty=1)
    }
  }
  abline(h=0,lty=1)
  par(usr=c(0,1,0,1))
  mtext(side=3,at=0,letters[ipic],font=2,line=0.5,adj = 0)
  mtext(side=3,at=0.05,rivers_name[ipic],line=0.5,adj = 0)
  if(ipic==3){
    mtext(side=2,at=0.5,expression(paste("Change in wet season precipitation (%)",sep="")),
          cex=cexbar,line=2.5,las=0)
  }
}

#################################plot the legend####################################
par(fig=c(0.05,0.8,0.04,0.08), new=T)
plot(seq(1,9,1),type="n",axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),
     xaxs="i",yaxs="i")
rect_pos = c(0.02,0.17,0.3);rect_width=0.03
y1=0.3;y2=0.6
rect(rect_pos,y1,rect_pos+rect_width,y2,col=c(cols_used[1],cols_used[3],cols_used[5]),border =c(cols_used[1],cols_used[3],cols_used[5]))
names=c(expression('1.5'*degree*'C'),expression('2'*degree*'C'),expression('4'*degree*'C'))
text(rect_pos+rect_width+0.01,y1+(y2-y1)/2,names,adj=0,cex=cexbar)

rect(0.43,y1,0.43+rect_width,y2,border = "black",cex=cexbar)
text(0.43+rect_width+0.01,y1+(y2-y1)/2,"Before constraint",adj=0,cex=cexbar)
rect(0.71,y1,0.71+rect_width,y2,density = 26,angle = 45,border = "black",cex=cexbar)
text(0.71+rect_width+0.01,y1+(y2-y1)/2,"After constraint",adj=0,cex=cexbar)

dev.off()




