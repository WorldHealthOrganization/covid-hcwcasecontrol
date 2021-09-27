searchNcorrect <- function(tosearch, tocorrect, colnames){
  
  for (col in colnames){
    for (w in tosearch){
      dat[grep(w, dat[,col]),col] = tocorrect
    }
  }
  
  return(dat)
  
}