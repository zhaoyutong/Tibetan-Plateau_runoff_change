rm(list=ls())

library(MASS)
library(sfsmisc)
##################The orginal setting
path_pics = '/Users/zhaoyutong/Desktop/final/'
path = '/Users/zhaoyutong/Desktop/data/runoff/V3/'
pdf(
  file = paste(path_pics,"_fig2_relation_P_color2.pdf", sep = ""),
  width =7.09,
  height = 6.74,
  pointsize = 7,
  family = 'Helvetica'
)
par( oma = c(1, 1.5, 1.2, 1),mar = c(0, 0, 0, 0))
cols_line = c(rgb(38/255,89/255,214/255),'#F8B501','#E15B66')
cols_polygon = c(rgb(38/255,89/255,214/255),'#F8B501','#E15B66')
cols_point = c(rgb(38/255,89/255,214/255),'#F8B501','#E15B66')
cexlab_text=1
temp_name = c(expression(1.5*degree*'C'),expression(2*degree*'C'),expression(4*degree*'C'))

######################the layout###############
nrow = 3;ncol = 4;
width1 = 0.19;width2 = 0.21;height = 0.22;
epsh = 0.06;epsw = 0.08
xstart=0.05;ystart=0.9;
position1 = array(NaN,dim = c(nrow*ncol,4))
for (r in 1:nrow) {
  for (c in 1:ncol) {
    
    if(c==1){
      number0=0
      number1=0
      number3=1
      number4=0
      number5=0
    }
    if(c==2){
      number0=1
      number1=0
      number3=1
      number4=1
      number5=0
    }
    if(c==3){
      number0=1
      number1=1
      number3=2
      number4=1
      number5=1
    }
    if(c==4){
      number0=2
      number1=1
      number3=2
      number4=2
      number5=1
    }
    
    if(r==1){
      number2 = 0
    }
    if(r==2){
      number2 = 1
    }
    if(r==3){
      number2 = 2
    }
    position1[(r-1)*ncol+c,1] = xstart + number0*width1+number1*width2+number1*epsw
    position1[(r-1)*ncol+c,2] = xstart + number3*width1+number4*width2+number5*epsw
    position1[(r-1)*ncol+c,3] = ystart - r*(height)-epsh*number2;
    position1[(r-1)*ncol+c,4] = ystart - (r-1)*(height)-epsh*number2;
  }
}

##########################################################


rect_h = 0.08
abctitle = c(" a "," b "," c "," d "," e "," f ")
temp_arr = c('1.5','2','4')

rivername = c('Indus','Ganges','Brahmaputra','Sal-Mek','Yangtze','Yellow')

lwd_cex=1.5
for (ipic in 1:12) {
  if(ipic==1){
    par(fig=position1[ipic,], new=F)
  }else{
    par(fig=position1[ipic,], new=T)
  }
  
  if(ipic==1|ipic==3|ipic==5|ipic==7|ipic==9|ipic==11){
    if(ipic==1){
      ### the sub picture(a) for Indus is 1-9
      result_mrro = load(paste0(path,'V3_c001_01A_weights_1.5_mrro_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp26_mrro = var_change_river[,(ipic+1)/2] 
      result_pr = load(paste0(path,'V3_c001_01A_weights_1.5_pr_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp26_pr = var_change_river[,(ipic+1)/2] 
      
      result_mrro = load(paste0(path,'V3_c001_01A_weights_2_mrro_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp45_mrro = var_change_river[,(ipic+1)/2]  
      result_pr = load(paste0(path,'V3_c001_01A_weights_2_pr_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp45_pr = var_change_river[,(ipic+1)/2]  
      
      result_mrro = load(paste0(path,'V3_c001_01A_weights_4_mrro_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp85_mrro = var_change_river[,(ipic+1)/2]  
      result_pr = load(paste0(path,'V3_c001_01A_weights_4_pr_river_basin_change_1-9_all_temperature_models.Rdata'))
      rcp85_pr = var_change_river[,(ipic+1)/2]  
      
    }else{
      result_mrro = load(paste0(path,'V3_c001_01A_weights_1.5_mrro_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp26_mrro = var_change_river[,(ipic+1)/2]  
      result_pr = load(paste0(path,'V3_c001_01A_weights_1.5_pr_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp26_pr = var_change_river[,(ipic+1)/2]  
      
      result_mrro = load(paste0(path,'V3_c001_01A_weights_2_mrro_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp45_mrro = var_change_river[,(ipic+1)/2]  
      result_pr = load(paste0(path,'V3_c001_01A_weights_2_pr_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp45_pr = var_change_river[,(ipic+1)/2]  
      
      result_mrro = load(paste0(path,'V3_c001_01A_weights_4_mrro_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp85_mrro = var_change_river[,(ipic+1)/2]  
      result_pr = load(paste0(path,'V3_c001_01A_weights_4_pr_river_basin_change_6-9_all_temperature_models.Rdata'))
      rcp85_pr = var_change_river[,(ipic+1)/2]  
      
    }
    
    clarity_back = 0.6
    if(ipic==1){
      xmin = -30;xmax = 65;x_break=30
      ymin = -30;ymax = 17;y_break=15
    }
    if(ipic==3){
      xmin = -50;xmax = 165;x_break=50
      ymin = -50;ymax = 130;y_break=50
    }
    if(ipic==5){
      xmin = -50;xmax = 170;x_break=50
      ymin = -50;ymax = 125;y_break=50
    }
    if(ipic==7){
      xmin = -30;xmax = 100;x_break=30
      ymin = -15;ymax = 35;y_break=15
    }
    if(ipic==9){
      xmin = -20;xmax = 65;x_break=20
      ymin = -20;ymax = 50;y_break=20
    }
    if(ipic==11){
      xmin = -15;xmax = 35;x_break=15
      ymin = -20;ymax = 28;y_break=10
    }
    xlim_break = seq(xmin,xmax,x_break)
    ylim_break = seq(ymin,ymax,y_break)
    par(usr=c(xmin,xmax,ymin,ymax))
    plot(seq(1,9,1),seq(1,9,1),type="n",axes=F,xlab="",ylab="",
         xlim=c(xmin,xmax),ylim=c(ymin,ymax),
         xaxs="i",yaxs="i")
    box()
    axis(2,at=ylim_break,tck=-0.02,cex=10,labels = F)
    mtext(side=2,at=ylim_break,ylim_break,cex=cexlab_text,line=0.4,las=1)
    axis(1,at=xlim_break,tck=-0.02,cex=10,labels = F)
    mtext(side=1,at=xlim_break,xlim_break,cex=cexlab_text,line=0.4,las=1)
    
    
    text_height = c(0.4,0.25,0.1)
    number_text = c(3,2,1)
    for (itype in 1:3) {
      if(itype==1){
        yvar = rcp26_mrro;xvar = rcp26_pr
      }
      if(itype==2){
        yvar = rcp45_mrro;xvar = rcp45_pr
      }
      if(itype==3){
        yvar = rcp85_mrro;xvar = rcp85_pr
      }
      
      ############the R squared
      res_k <- rlm(yvar~xvar)
      slope = summary(res_k)[[4]][2]
      intercept = summary(res_k)[[4]][1]
      p_value = round(f.robftest(res_k)$p.value,2)
      r2 <- function(x){ 
        #x$w = rep(1,51)
        SSe <- sum((x$w*x$residuals)^2);  
        observed <- x$residuals+x$fitted.values;  
        SSt1 <- sum((x$w*observed-mean(x$w*observed))^2); 
        SSt <- sum((observed-mean(observed))^2);  
        value <- 1-SSe/SSt1;  
        return(value);  
      } 
      R_square=sprintf("%.2f",r2(res_k))
      ##########################
      abline(intercept,slope,lty=1,col=cols_line[itype])
      text((xmin+(xmax-xmin)*0.64),(ymin+number_text[itype]*(ymax-ymin)/10),expression(paste(R^{2}," = ",sep="")),
           col=cols_line[itype],cex=cexlab_text,adj=1)
      text((xmin+(xmax-xmin)*0.8),(ymin+number_text[itype]*(ymax-ymin)/10),R_square,col=cols_line[itype],cex=cexlab_text,adj=1)
      
      if(p_value<0.01){
        text((xmin+(xmax-xmin)*0.8),(ymin+number_text[itype]*(ymax-ymin)/9),'***',col=cols_line[itype],cex=cexlab_text,adj=0)
      }
      if(p_value<0.05&p_value>=0.01){
        text((xmin+(xmax-xmin)*0.8),(ymin+number_text[itype]*(ymax-ymin)/9),'**',col=cols_line[itype],cex=cexlab_text,adj=0)
      }
      if(p_value<0.1&p_value>=0.05){
        text((xmin+(xmax-xmin)*0.8),(ymin+number_text[itype]*(ymax-ymin)/9),'*',col=cols_line[itype],cex=cexlab_text,adj=0)
      }
      if(p_value>0.1){
        text((xmin+(xmax-xmin)*0.8),(ymin+number_text[itype]*(ymax-ymin)/9),'',col=cols_line[itype],cex=cexlab_text,adj=0)
      }
    }
    
    points(rcp26_pr,rcp26_mrro,col=cols_line[1],pch=19,cex=0.8)
    points(rcp45_pr,rcp45_mrro,col=cols_line[2],pch=19,cex=0.8)
    points(rcp85_pr,rcp85_mrro,col=cols_line[3],pch=19,cex=0.8)
    
    
    par(usr=c(0,1,0,1))
    if(ipic==1|ipic==5|ipic==9){
      mtext(side=2,at=0.5,expression(paste(Delta,"R (",mm~month^{-1},')',sep="")),cex=cexlab_text,line=2,las=0,adj=0.5)
    }
    
    if(ipic==9|ipic==11){
      mtext(side=1,at=0.5,expression(paste(Delta,"P (",mm~month^{-1},')',sep="")),cex=cexlab_text,line=1.7,las=0,adj=0.5)
    }
    text(0.05,0.92,abctitle[ipic/2+0.5],adj=0,font=2)
    
  }else{
    if(ipic==2){
      xmin = -15;xmax=15;yminx=0.001;ymaxx=0.32
      xlim_break = seq(-10,xmax,5)
      ylim_break = seq(0,ymaxx,0.1)
    }
    if(ipic==4){
      xmin = -28;xmax=68;yminx=0.001;ymaxx=0.07
      xlim_break = seq(-20,xmax,20)
      ylim_break = round(seq(0,ymaxx,0.02),2)
    }
    if(ipic==6){
      xmin = -29;xmax=70;yminx=0.001;ymaxx=0.075
      xlim_break = seq(-20,xmax,20)
      ylim_break = c(0,sprintf("%.2f",seq(0.02,ymaxx,0.02)))
    }
    if(ipic==8){
      xmin = -27;xmax=62;yminx=0.001;ymaxx=0.2
      xlim_break = seq(-20,xmax,20)
      ylim_break = c(0,sprintf("%.2f",seq(0.05,ymaxx,0.05)))
    }
    if(ipic==10){
      xmin = -15;xmax=32;yminx=0.001;ymaxx=0.32
      xlim_break = seq(-10,xmax,10)
      ylim_break = seq(0,ymaxx,0.1)
    }
    if(ipic==12){
      xmin = -15;xmax=25;yminx=0.001;ymaxx=0.62
      xlim_break = seq(-10,xmax,10)
      ylim_break = seq(0,ymaxx,0.2)
    }
    lwd_cex=1.5
    
    result_rcp26 = load(paste(path,"c004_03_1.5_constrant_mrro_basin_ensemble_P.Rdata",sep=""))
    result_rcp45 = load(paste(path,"c004_03_2_constrant_mrro_basin_ensemble_P.Rdata",sep=""))
    result_rcp85 = load(paste(path,"c004_03_4_constrant_mrro_basin_ensemble_P.Rdata",sep=""))
    
    plot(rcp26_Xlim_before[ipic/2,],rcp26_Ylim_before[ipic/2,], type="l", lwd=lwd_cex,xlim=c(xmin,xmax), ylab="",  ylim=c(yminx,ymaxx),
         xlab="",lty=3,col=cols_line[1],axes=F,xaxs="i",yaxs="i")
    
    lines(rcp26_Xlim_after[ipic/2,],rcp26_Ylim_after[ipic/2,], type="l", lwd=lwd_cex, col=cols_line[1],lty=1)
    lines(rcp45_Xlim_before[ipic/2,],rcp45_Ylim_before[ipic/2,], type="l", lwd=lwd_cex, col=cols_line[2],lty=3)
    lines(rcp45_Xlim_after[ipic/2,],rcp45_Ylim_after[ipic/2,], type="l", lwd=lwd_cex, col=cols_line[2],lty=1)
    
    lines(rcp85_Xlim_before[ipic/2,],rcp85_Ylim_before[ipic/2,], type="l", lwd=lwd_cex, col=cols_line[3],lty=3)
    lines(rcp85_Xlim_after[ipic/2,],rcp85_Ylim_after[ipic/2,], type="l", lwd=lwd_cex, col=cols_line[3],lty=1)
    
    
    #par(usr=c(0,1,0,1))
    #text(0.95,0.92,rivername[ipic/2+0.5],adj=1)
    axis(1,at=xlim_break,tck=-0.02,cex=10,labels = F,line=-0.0085)
    mtext(side=1,at=xlim_break,xlim_break,cex=cexlab_text,line=0.4,las=1)
    axis(4,at=ylim_break,tck=-0.02,cex=10,labels = F)
    mtext(side=4,at=ylim_break,ylim_break,cex=cexlab_text,line=0.4,las=1)
    box()
    
    par(usr=c(0,1,0,1))
    text(0.95,0.92,rivername[ipic/2+0.5],adj=1)
    
    if(ipic==4|ipic==8|ipic==12){
      mtext(side=4,at=0.5,"Probability density",cex=cexlab_text,line=2.3,las=0)
    }
    if(ipic==12|ipic==10){
      number = -38
      mtext(side=1,at=0.5,expression(paste(Delta,"R (",mm~month^{-1},')',sep="")),cex=cexlab_text,line=1.7,las=0,adj=0.5)
    }
    
    
  }
  
}
#############the legend
position2 = c(0.07,0.87,0.90,0.98)
par(fig=position2, new=T)
xmin = 0;xmax=1;ymin=0;ymax=1
plot(seq(1,9,1),type="n",axes=F,xlab="",ylab="",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
#box()
ylib = 0.5;step=0.06
x1 =0.05;
segments(x1,ylib,x1+step,ylib,lty=1,lwd=lwd_cex-0.5)
text(x1+step+0.015,ylib,"Constrained",adj=0)
segments(x1+3.2*step+0.02,ylib,x1+4.2*step+0.02,ylib,lty=3,lwd=lwd_cex-0.5)
text(x1+4.2*step+0.015*2,ylib,"Unconstrained",adj=0)
segments(x1+7.2*step+0.02,ylib,x1+8.2*step+0.02,ylib,lty=1,col = cols_line[1],lwd=lwd_cex-0.5)
text(x1+8.2*step+0.015*2,ylib,expression(1.5*degree*'C'),adj=0)
segments(x1+10.2*step+0.02,ylib,x1+11.2*step+0.02,ylib,lty=1,col = cols_line[2],lwd=lwd_cex-0.5)
text(x1+11.2*step+0.015*2,ylib,expression(2*degree*'C'),adj=0)
segments(x1+13.2*step+0.02,ylib,x1+14.2*step+0.02,ylib,lty=1,col = cols_line[3],lwd=lwd_cex-0.5)
text(x1+14.2*step+0.015*2,ylib,expression(4*degree*'C'),adj=0)

dev.off()

