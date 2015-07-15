library(flowWorkspace)
f = system.file(package="BioC2015OpenCyto","extdata")
if(f!=""){
  tbdata = load_gs(file.path(f,"tbdata"))
}else{
  message("Not loading data until package is installed")
}
rm(f)
