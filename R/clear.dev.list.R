
clear.dev.list = function(){

  #'
  #'Clears the dev. list
  #'
  #' @importFrom grDevices dev.off dev.list
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

  cat(sprintf('closing %d device[s] \n', length(grDevices::dev.list())))
  for (i in grDevices::dev.list()[1]:grDevices::dev.list()[length(grDevices::dev.list())]) {
    grDevices::dev.off()
  }
}
