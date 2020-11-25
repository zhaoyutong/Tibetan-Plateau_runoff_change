
rm(list=ls())

variable = c('pr','psl')
for (ivar in 1:length(variable)) {
  path_tmp = "/public/home/liud/ytzhao/runoff/data/cmip6/tmp/"
  path_out = paste0("/public/home/liud/ytzhao/runoff/data/cmip6/",variable[ivar],"/")
  path = paste0("/public/home/liud/ytzhao/CMIP6/dealt/",variable[ivar],"/")
  files = list.files(path,"nc")
  
  experiment = NULL
  for (ifile in 1:length(files)) {
    experiment=c(experiment,strsplit(files,'_')[[ifile]][4])
  }
  experiment_all=c(experiment)
  experiment=experiment_all[!duplicated(experiment_all)]
  nexperiment=length(experiment)
  
  for (iexp in 1:nexperiment) {
    if(experiment[iexp]=="historical"){
      syear = 1960;eyear = 2014;
    }else{
      syear = 2015;eyear = 2099;
    }
    files_exp=files[grep(glob2rx(paste0('*_',experiment[iexp],"_*")),files)]
    
    model=NULL
    for (ifile in 1:length(files_exp)) {
      model=c(model,strsplit(files_exp,'_')[[ifile]][3])
    }
    modelalls=c(model)
    models=modelalls[!duplicated(modelalls)]
    nmodels=length(models)
    
    for (imodel in 1:nmodels) {
      print(paste0(imodel,":",experiment[iexp],":",models[imodel]))
      file_in = paste0(path,files_exp[imodel])
      year0 = as.numeric(substr(strsplit(files_exp[imodel],'_')[[1]][7],1,4))
      
      if(year0<=syear){
        start_timesteps = (syear - year0)*12 +1; end_timesteps = (eyear - year0)*12+12
        smonth = "01";emonth = "12"
        
        filetemp = paste0(path_tmp,"seletime_",files_exp[imodel])
        char_num = strsplit(files_exp[imodel],'_')[[1]]
        ncar_all = 0
        for (ichar in 1:(length(char_num)-1)) {
          ncar_used = nchar(char_num[ichar])
          ncar_all = ncar_all + ncar_used
        }
        ncar_all_used = ncar_all + (length(char_num)-1)
        file_out_global = paste0(path_out,"global/",substr(files_exp[imodel],1,ncar_all_used),syear,smonth,"-",eyear,emonth,".nc")
        file_out_tibet = paste0(path_out,"tibet/tibet_",substr(files_exp[imodel],1,ncar_all_used),syear,smonth,"-",eyear,emonth,".nc")
        
        system(paste("cdo seltimestep,",start_timesteps,"/",end_timesteps," ",file_in,' ',filetemp,sep=''))
        system(paste('cdo  remapbil,r720x360 ',filetemp,' ',file_out_global,sep=""))
        #(lon0, lon1, lat0, lat1):
        system(paste('cdo  sellonlatbox,60.0,115.0,20.5,45.3 ',file_out_global,' ',file_out_tibet,sep=""))
        system(paste0("rm -rf ",path_tmp,"*"))
      }
    }
  }
  
}
