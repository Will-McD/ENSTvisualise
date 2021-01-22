
make.flag = function(){

  #'
  #'Creates the data.table which the flagged cells are stored in.
  #' @importFrom data.table data.table
  #'
  #'@export

  Flagged <<- data.table::data.table('n' = double(), 'index' = double(), 'population' = double())
}
