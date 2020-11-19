.ConfigFile <- function(sfNew, pkgname = "inmuno"){
  if(!file.exists(.libPaths()[1])){
    saveRDS(NULL, file = file.path(.libPaths()[1],pkgname) )
  }
  config <- readRDS(file.path(.libPaths()[1],pkgname) )

  if(!missing(sfNew)){
     if(is.null(config)){
       config <- list(sfNew)
       names(config) <- sfNew$Name
     }else{
       config[[sdNew$Name]] <- sfNew
     }
  }
  saveRDS(config, file = file.path(.libPaths()[1],pkgname) )
  config
}

.onLoad <- function(libname, pkgname){
  cf <- .ConfigFile(pkgname = pkgname)
  if(is.null(cf)){
    cat("\nNo softwares installed")
  }else{
    cat("\n--------------------")
    print(cf)
    cat("\n--------------------")
    cat("\nIf This softwares are already installed")
  }
}
