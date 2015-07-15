library(flowWorkspace)
f = system.file(package="BioC2015OpenCyto","extdata")
if(f!=""){
  if(file.exists(file.path(f,"tbdata","file1320c19b087dc.nc"))){
    tbdata = load_gs(file.path(f,"tbdata"))
  }else{
    message("Downloading an 800 Mb file. Please be patient. If this fails, you may need to install wget.")
    download.file(
      url = "http://dl.dropboxusercontent.com/u/2635312/file1320c19b087dc.nc",method =
        "wget",mode = "wb",destfile = file.path(f,"tbdata",
                                                "file1320c19b087dc.nc"), quiet = TRUE
    )
  }
}else{
  message("Not loading data until package is installed")
}
rm(f)
