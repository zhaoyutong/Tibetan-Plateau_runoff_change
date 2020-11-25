rm(list=ls())

variable = c("tas","pr","psl","hfls","mrro")
path_out = "/public/home/ytzhao/CMIP6/dealt/"
path = "/public/home/ytzhao/CMIP6/data/"
frequency = "Amon"

for (ivar in 1:length(variable)) {
  path_out_var = paste0(path_out,variable[ivar],"/")
  path_in = paste0(path,variable[ivar],"/")
  if(file.exists(path_out_var)==FALSE){
    dir.create(path_out_var)
  }
  if(file.exists(path_in)==FALSE){
    dir.create(path_in)
  }
  ######################################################
  files = list.files(path_in,"nc")
  experiment = NULL
  for (ifile in 1:length(files)) {
    experiment=c(experiment,strsplit(files,'_')[[ifile]][4])
  }
  experiment_all=c(experiment)
  experiment=experiment_all[!duplicated(experiment_all)]
  nexperiment=length(experiment)
  
  for (iexp in 1:nexperiment) {
    
    files_exp=files[grep(glob2rx(paste0(variable[ivar],'_*_',experiment[iexp],"_*")),files)]
    
    model=NULL
    for (ifile in 1:length(files_exp)) {
      model=c(model,strsplit(files_exp,'_')[[ifile]][3])
    }
    modelalls=c(model)
    models=modelalls[!duplicated(modelalls)]
    nmodels=length(models)
    
    for (imodel in 1:nmodels) {
      files_tmp_list=list.files(path_in,pattern=models[imodel])
      files_tmp=files_tmp_list[grep(glob2rx(paste0(variable[ivar],'_*_',models[imodel],'_',experiment[iexp],"_*")),files_tmp_list)]
      
      if(length(files_tmp)==1){
        print(paste0(imodel,":",files_tmp))
        system(paste0('cp  ',paste0(path_in,files_tmp),'  ',paste0(path_out_var,files_tmp)))
      }else{
        ensemble=paste(paste(path_in,files_tmp,sep=""),collapse=' ')#convert one line
        
        #####find data the time range
        nfiles = length(files_tmp)
        startyear_all=NULL;endyear_all=NULL;startmon_all=NULL;endmon_all=NULL
        for (ifile in 1:nfiles) {
          startyear_all[ifile]=as.numeric(substr(strsplit(files_tmp[ifile],'_')[[1]][7],1,4))
          endyear_all[ifile]=as.numeric(substr(strsplit(files_tmp[ifile],'_')[[1]][7],8,11))
          startmon_all[ifile]=as.numeric(substr(strsplit(files_tmp[ifile],'_')[[1]][7],5,6))
          endmon_all[ifile]=as.numeric(substr(strsplit(files_tmp[ifile],'_')[[1]][7],12,13))
        }
        startyear = startyear_all[1];endyear = endyear_all[nfiles]
        startmont = startmon_all[1]; endmont = endmon_all[nfiles]
        if(startmont<10){
          startmont = paste0(0,startmont)
        }
        if(endmont<10){
          endmont = paste0(0,endmont)
        }
        
        print(paste0(variable[ivar],":",experiment[iexp],":",imodel,"_",models[imodel],":",startyear,"-",endyear))
        
        #-------------------------------------------------------------
        char_num = strsplit(files_tmp[1],'_')[[1]]
        ncar_all = 0
        for (ichar in 1:(length(char_num)-1)) {
          ncar_used = nchar(char_num[ichar])
          ncar_all = ncar_all + ncar_used
        }
        ncar_all_used = ncar_all + (length(char_num)-1)
        file_out = paste0(path_out_var,substr(files_tmp[1],1,ncar_all_used),startyear,startmont,"-",endyear,endmont,".nc")
        system(paste('cdo mergetime ',ensemble,' ',file_out,sep=''))
      }
      
    }
  }
  
  
}
