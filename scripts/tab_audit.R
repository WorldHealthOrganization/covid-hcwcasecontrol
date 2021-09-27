tab_audit <- function(dat, colnam, check, valtocheckfor, err.msg){
  
  df.list = list()
  
  varnam = clean_labels(colnam)
  
  for (i in 1:length(colnam)){
    
    col = colnam[i]
    if (check == 'missing') {
      row = is.na(dat[[col]])
    } else if (check == 'value') {
      row = which(dat[[col]] != valtocheckfor)
    } else if (check == 'less than') {
      row = which(dat[[col]] < valtocheckfor)
    } else if (check == 'more than') {
      row = which(dat[[col]] > valtocheckfor)
    } else {
      row = NULL
    }
    
    if(length(row) > 0 & any(row == T)){
      df.list[[i]] = data.frame(ID = dat$init_inter_id[row], 
                                Item = varnam[i], 
                                Value = as.character(dat[row, col]),
                                Error = err.msg)
    } 
  }
  
  if (length(df.list) > 0){
    df  = do.call('rbind.data.frame', df.list)
    return(df)
  } else {
    message('No error found.')
  }
  
}