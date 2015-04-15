library(abvanalysis)

current.wd <- getwd()
path <- "~/tmp/abvanalysis"
if(!file_test("-d", path)){
  dir.create(path, recursive = TRUE)
}
setwd(path)
prepare_analysis()
setwd(current.wd)
