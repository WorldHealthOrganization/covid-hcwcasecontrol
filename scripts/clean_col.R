
clean_col <- function(dat = dat, colnam = '', convertto = 'factor', lvl = '', check = T){ 
  
  ### date 
  if (convertto == 'date'){
    for (col in colnam){
      message(col)
      d = as.Date(dat[,col])
      dat[,col] = d
      
      if (check == T) {
        ## checks 
        lowend = min(d, na.rm = T)
        highend = max(d, na.rm = T)
        
        print(paste('min = ', lowend))
        print(paste('max = ', highend))
        print(paste('NA = ', sum(is.na(d))))
        
        lowrows = which(d < as.Date('2020-04-01')) # date that one of the indian sites started
        if (length(lowrows) > 0 & is.na(match(col, c('covid_pt_admission_earliest_date', 'ipc_training_latest_date', 
                                                     'hcf_ipcaudit_date', 'prohpyl1_datestart', 'hcf_hhaudit_date')))) {
          message('........These dates are earlier than 2020-04-01')
          print(paste(dat$country.godata[lowrows], dat$site.godata[lowrows], dat$init_inter_id[lowrows], dat[lowrows, col],  sep = '-'))
        }
        
        hirows = which(d > Sys.Date()) # today's date 
        if (length(hirows) > 0) {
          message('........These dates are later than today')
          print(paste(dat$country.godata[hirows], dat$site.godata[hirows], dat$init_inter_id[hirows], dat[hirows, col], sep = '-'))
        }
      }
      
    }
  }
  
  ### catgerorical data 
  if (convertto == 'factor'){
    
    search.word = toupper(lvl)
    
    if (colnam == ''){ # if column name is not defined, search for all columns
      coltoclean = as.vector(unlist(apply(dat, 2, function(x){
        x[which(x=='')] = NA
        identical(sort(search.word), toupper(sort(unique(x[!is.na(x)]))))
      })))
    }
    
    if (sum(coltoclean) == 0) { # indicate which columns are being cleaned
      print('No columns match the levels!')
    }
    
    for (col in which(coltoclean == T)){
      dat[[col]] = as.factor(dat[[col]])
      dat[[col]] = factor(dat[[col]], levels = search.word)
      
      if (check == T){
        print(colnames(dat)[col])
        print(table(dat[[col]], useNA = 'ifany'))
      }
    }
  }
  
  return(dat)
}
