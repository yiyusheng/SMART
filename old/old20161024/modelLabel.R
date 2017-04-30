# Label model for lead time prediction
# Only process 4 type of disk model
# Date: 2016-06-21
# Author: Pezy
modelLabel <- function(dm){
  r <- rep('0',length(dm))
  r[dm == 'ST3750330NS' | dm == 'GB0750EAFJK'] <- '750G2'
  r[dm == 'ST31000524NS' | dm == 'MB1000EAMZE'] <- '1000G2'
  r[dm == 'ST1000NM0011' | dm == 'MB1000EBZQB'] <- '1000G3'
  r[dm == 'ST2000NM0011'] <- '2000G3'
  r
}