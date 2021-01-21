
make.flag = function(){

  #'
  #'Creates the data.table which the flagged cells are stored in.
  #'
  #'
  #'@export

  Flagged <<- data.table('n' = double(), 'index' = double(), 'population' = double())
}
