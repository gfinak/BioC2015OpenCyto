library(flowWorkspace)
datafile = "file1320c19b087dc.nc"
f = system.file(package = "BioC2015OpenCyto","extdata")
if (file.exists("/home/ubuntu/data/gfinak/flow_lab/file1320c19b087dc.nc")) {
  filepath = "file:///home/ubuntu/data/gfinak/flow_lab/file1320c19b087dc.nc"
}else if (file.exists("/Users/gfinak/Dropbox/Public/file1320c19b087dc.nc")) {
  filepath = "file:///Users/gfinak/Dropbox/Public/file1320c19b087dc.nc"
}else{
  filepath = "http://dl.dropboxusercontent.com/u/2635312/file1320c19b087dc.nc"
}

if (f != "") {
  #Found the system directory.
  if (file.exists(file.path(f,"tbdata",datafile))) {
    #Data exists, load it and proceed.
    tbdata = load_gs(file.path(f,"tbdata"))
  }else{
    #Test for write permissions
    if (file.access(f,2)) {
      message("Can't write to R system library")
      tmpdir = tempdir()
      message("Using ",tmpdir)
      #Downlaad to tmpdir
      outdir = tmpdir
    }else{
      # Download to the system directory
      outdir = f
    }
    #Does the tbdata directory exist?
    if (!file.exists(file.path(outdir,"tbdata"))) {
      #copy tbdata into place
      file.copy(from = file.path(f,"tbdata"), to = outdir,recursive = TRUE)
    }
    #download tbdata nc file if necessary
    if (file.exists(gsub("http://","",gsub("file://","",filepath)))) {
      message("Transferring data file.. please wait.")
      download.file(
        url = filepath, mode = "wb",method = ifelse(grepl("file:",filepath),"internal","wget"), destfile = file.path(outdir,"tbdata",datafile), quiet =
          TRUE
      )
    }
    tbdata = load_gs(file.path(outdir,"tbdata"))
  }
}else{
  message("Not loading data until package is installed")
}
rm(list = c("datafile","f","filepath"))
