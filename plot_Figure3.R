rm(list=ls())

path = '/Users/zhaoyutong/Desktop/data/runoff/V3/'
###########################the orignal setting#########################
river_basin = c('Indus','Ganges','Brahmaputra','Sal-Mek','Yangtze','Yellow')
path_pics = '/Users/zhaoyutong/Desktop/final/'
pdf(
  file = paste(path_pics,"_fig3_runoff_color2.pdf", sep = ""),
  width = 7.09,
  height = 6.7,
  pointsize = 7,
  family = 'Helvetica'
)
cols=c(rep(rgb(38/255,89/255,214/255),2),
       rep('#F8B501',2),
       rep('#E15B66',2))
cols_glacier = rgb(31/255, 138/255, 230/255,0.6)#blue
abctitle = letters[1:6]
rivers_name <- c("Indus","Ganges","Brahmaputra","Salween and Mekong","Yangtze","Yellow","All basins")
#####################the output###################
nrow = 3;ncol = 2;
width0 = 0.3;break0=0.15
width = 0.1+width0;height = 0.22;
xstart=0.06;ystart=0.95;
break1=0.07;break2 = 0.08
position1 = array(NaN,dim = c(nrow*ncol,4))
for (r in 1:nrow) {
  for (c in 1:ncol) {
    position1[(r-1)*ncol+c,1] = xstart + (c-1)*width+(c-1)*break1;
    position1[(r-1)*ncol+c,2] = xstart + c*width+(c-1)*break1;
    position1[(r-1)*ncol+c,3] = ystart - r*height-(r-1)*break2;
    position1[(r-1)*ncol+c,4] = ystart - (r-1)*height-(r-1)*break2;
  }
}

breaks1 = 0.03;breaks2=0.12;widths=0.1
y_position=c(breaks1*1+widths*1/2,
             breaks1*2+widths*3/2,
             breaks1*2+widths*5/2+breaks2,
             breaks1*3+widths*7/2+breaks2,
             breaks1*3+widths*9/2+breaks2*2,
             breaks1*4+widths*11/2+breaks2*2)
##########################the main picture#############################
for (ipic in 1:6) {
  par(fig=position1[(ipic),], new=T)
  par( oma = c(0, 0, 0, 0),mar = c(0, 0, 0, 0))
  if(ipic==1){
    xmin = -70;xmax=20;ymin=0;ymax=1
    xlim = seq(-60,20,20)
    data_arr=mrro_all[,ipic]
  }else{
    xmin = -20;xmax=70;ymin=0;ymax=1
    xlim =seq(-20,60,10)
    data_arr=mrro_all[,ipic]
    box_pos = mrro_all_sd[,ipic]+mrro_all[,ipic]
    box_neg = mrro_all[,ipic] - mrro_all_sd[,ipic]
  }
  
  plot(seq(1,9,1),type="n",axes=F,xlab="",ylab="",xlim=c(xmin,xmax),ylim=c(ymin,ymax),
       xaxs="i",yaxs="i")
  box()
  abline(v=0)
  axis(side=1,at=xlim,labels=F,tck=-0.02,lwd=1,line = 0,col = "black")
  mtext(side=1,at=xlim,xlim,line=0.4,las=1,col="black")
  
  glacier_arr=glacier_change[,ipic]
  for (ibox in 1:6) {
    if(ibox==2|ibox==4|ibox==6){
      rect(0,y_position[ibox]-widths/2,data_arr[ibox],y_position[ibox]+widths/2,col = cols[ibox],border = cols[ibox])
    }else{
      rect(0,y_position[ibox]-widths/2,data_arr[ibox],y_position[ibox]+widths/2,border = "transparent",
           col = cols[ibox])
      par(fig=position1[(ipic),], new=T)
      rect(0,y_position[ibox]-widths/2,data_arr[ibox],y_position[ibox]+widths/2,border = "transparent",
           density = 30,angle = 60,col = "white")
      lwd_l = 1
      if(glacier_arr[(ibox+1)/2]>0){
        rect(data_arr[ibox],y_position[ibox]-widths/2,(data_arr[ibox]+glacier_arr[(ibox+1)/2]),y_position[ibox]+widths/2,
             border = "transparent",col = cols_glacier)
      }else{
        rect(glacier_arr[ibox],y_position[ibox]-widths/2,0,y_position[ibox]+widths/2,
             border = "transparent",col = cols_glacier)
      }
    }
    segments(box_neg[ibox],y_position[ibox],box_pos[ibox],y_position[ibox],lwd=lwd_l)
    segments(box_pos[ibox],y_position[ibox]+width/16,box_pos[ibox],y_position[ibox]-width/16,lwd=lwd_l)
    segments(box_neg[ibox],y_position[ibox]+width/16,box_neg[ibox],y_position[ibox]-width/16,lwd=lwd_l)
  }
  
  
  par(usr=c(0,1,0,1))
  number1 = 0.78
  mtext(side=3,at=0.01,abctitle[ipic],line=0.2,las=1,font=2,adj=0)
  mtext(side=3,at=0.06,rivers_name[ipic],line=0.2,las=1,font=1,adj=0)
  if(ipic==5|ipic==6){
    mtext(side=1,at=0.35,"Runoff change (%)",line=1.5,las=1,font=1,adj=0)
  }
}

################the legend###################
par(fig=c(xstart,1,0.01,0.06), new=T)
plot(seq(1,9,1),type="n",axes=F,xlab="",ylab="",xlim=c(0,1),ylim=c(0,1),
     xaxs="i",yaxs="i")
rect_pos = seq(0.02,0.48,0.12);rect_width=0.03
y1=0.3;y2=0.6
rect(rect_pos,y1,rect_pos+rect_width,y2,col=c(cols[1],cols[3],cols[5],cols_glacier),
     border = c(cols[1],cols[3],cols[5],cols_glacier))
names=c(expression('1.5'*degree*'C'),expression('2'*degree*'C'),expression('4'*degree*'C'),'Glacier-driven')
text(rect_pos+rect_width+0.01,y1+(y2-y1)/2,names,adj=0)

x_rect = 0.55
rect(x_rect,y1,x_rect+rect_width,y2,border = "black")
text(x_rect+rect_width+0.01,y1+(y2-y1)/2,"Before constraint",adj=0)
rect(x_rect+0.2,y1,x_rect+0.2+rect_width,y2,density = 30,angle = 60,border = "black")
text(x_rect+0.2+rect_width+0.01,y1+(y2-y1)/2,"Precipitation constraint",adj=0)
dev.off()

