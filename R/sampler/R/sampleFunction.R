library(abind)
library(plyr)
library(reshape2)

group.func.by.all <- function(md){
  data = list(adply(md,1:3))
  names(data) = "ALL"
  return(data)
}

group.func.by.each <- function(md){
  knames = dimnames(md)[['K']]
  data = llply(knames,function(k){
    md = adply(md[k,1,,,drop=FALSE],1:3)
  })
  names(data)=knames
  return(data)
}

getGroupFunc <- function(grpType){
  if ( grpType == "ALL"){
    return(group.func.by.all)
  }else if(grpType == "EACH"){
    return(group.func.by.each)
  }
}

genSample <- function(td, periods = c(1:5), grpType = "ALL", knames = list(), 
                      timeRange=list(),var.pattern="_ALL",mkt="CHINA_STOCK", freq="DAILY",paths=NULL,univ.ver="wind",
                      univ="zz800andsmb", verbose=FALSE){
  md = readicsdata(td,vers=NULL,paths=paths,verbose=verbose)
  mdFwd = readFwd(td,periods,mkt=mkt,freq=freq)
  md = panel.combine(list(md,mdFwd))
  if(length(univ)==0){
    if(length(knames) ==0){knames=dimnames(md)[[1]]}
  } else{
    mdUniv = readicsdata(td,dtype="univ",mkt=mkt,freq=freq,vers=univ.ver)
    mdUniv = mdUniv[mdUniv[,td,1,univ]==1,,,univ,drop=FALSE]
    knames=dimnames(mdUniv)[[1]]
  }
  if(length(timeRange)==0){timeRange=dimnames(md)[[3]]}
  md2=md[knames,1,timeRange,,drop=FALSE]
  if(var.pattern=="_ALL"){
    md2=md2
  } else {
    md2=md2[,,,grep(var.pattern,dimnames(md2)[[4]]),drop=FALSE]
  }
  
  groupingFunc = getGroupFunc(grpType)
  resultData = groupingFunc(md2)
  return(resultData)
  
}
  
genMultidaySample <- function(INS.days,periods,grpType,knames,timeRange,pattern,dir.list,
                              mkt,freq,sampler.dir,use.cache,verbose,univ.ver="wind",univ="zz800andsmb",error.tolerant=FALSE){
  llply(INS.days,function(td){
    resultName = file.path(sampler.dir,paste("RAW",td,"RDS",sep="."))
    if(!(use.cache && file.exists(resultName))){
      sampled = tryCatch( genSample(td, periods = periods,grpType = grpType, knames=knames,
                                    timeRange=timeRange,var.pattern = pattern,mkt=mkt,freq=freq,paths=dir.list,univ.ver=univ.ver,
                                    univ=univ, verbose=verbose), error=function(e){print(e);NULL})
      if (!is.null(sampled)){
        if (verbose) print(paste("Writing ", resultName))
        saveSample(sampled, resultName)
      } else if(!error.tolerant){
        stop(paste("GenSample error on ",td))
      }
    }
  })
}

saveSample <- function(file,fileName){
  saveRDS(file,fileName)
}

loadSample <- function(fileName){
  readRDS(fileName)
}



  
  