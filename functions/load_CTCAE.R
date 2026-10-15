path = file.path(PKG_HOME, 'functions/CTCAE/')
scripts = list.files(path)
for (i in 1:length(scripts)){
  source(paste0(path, scripts[i]))
}
