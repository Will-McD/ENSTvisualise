
clear.dev.list = function(){

  #'
  #'Clears the dev. list
  #'
  #'importFrom grDevices dev
  #'
  #'@description
  #' Calls dev.off() x many times, x being the number of items in dev.list()
  #'
  #' Used to clear out all plots stored in XQuartz
  #'
  #'
  #'@examples clear.dev.list()
  #'
  #'
  #'@export
  #'

  cat(sprintf('closing %d device[s] \n', length(dev.list())))
  for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
    dev.off()
  }
}
