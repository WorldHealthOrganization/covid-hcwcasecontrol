# demographics table 

#### function to print results for proportions 
prop.perc <- function(dat, characteristic) {
  to.summarise = dat[characteristic][!is.na(dat[characteristic])]
  tab = cbind(table(to.summarise), round(table(to.summarise)/sum(table(to.summarise))*100))
  tab.print = as.data.frame(apply(tab, 1, function(x){
    paste0(x[1], ' (', x[2], ')')
  }))
  colnames(tab.print) = 'prop'
  tab.print$cat = rownames(tab.print)
  
  return(tab.print)
}

print.prop <- function(dat, characteristic) {
  
  ## get data with cases and controls entered 
  dat.case = dat[which(dat$case_control == 'CASE'),]
  dat.control = dat[which(dat$case_control == 'CONTROL'),]
  
  case = prop.perc(dat.case, characteristic)
  control = prop.perc(dat.control, characteristic)
  tab = merge(case, control, by = 'cat', all = T)
  decrease.ord = order(as.numeric(substring(str_extract_all(tab$prop.y, "\\([^()]+\\)"), 
                                            2, nchar(str_extract_all(tab$prop.y, "\\([^()]+\\)")) -1)), 
                       decreasing = T)
  
  if ('YES' %in% tab$cat){decrease.ord = c(which(tab$cat == 'YES'), which(tab$cat == 'NO'),  which(tab$cat == 'NOT SURE'))}
  if (length(grep('DAYS', tab$cat)) > 0) {decrease.ord = c(grep('MOST', tab$cat),
                                                           grep('SOME', tab$cat),  
                                                           grep('FEW', tab$cat), 
                                                           grep('NOT', tab$cat))}
  if (length(grep('ALWAYS', tab$cat)) > 0) {decrease.ord = c(grep('ALWAYS', tab$cat),
                                                           grep('MOST', tab$cat),  
                                                           grep('OCCASIONALLY', tab$cat), 
                                                           grep('RARELY', tab$cat),
                                                           grep('NEVER', tab$cat))}
  tab.sort = tab[decrease.ord,]
  tab.final = add_column(tab.sort, d = c(characteristic, 
                                         rep(NA, nrow(tab.sort) - 1)), 
                         .before = 1)
  colnames(tab.final) = c('characteristic', '', 'case', 'control')
  
  return(tab.final)
  
}

#### function to print results for mean, IQR

mean.IQR <- function(dat, characteristic) {
  to.summarise = dat[characteristic][!is.na(dat[characteristic])]
  paste0(round(mean(to.summarise), 1), ' (', 
         quantile(to.summarise, probs = 0.25), '-', quantile(to.summarise, probs = 0.75), ')')
}

#### functions to test significance



print.mean <- function(dat, characteristic) {
  
  ## get data with cases and controls entered 
  dat.case = dat[which(dat$case_control == 'CASE'),]
  dat.control = dat[which(dat$case_control == 'CONTROL'),]
  
  out = as.data.frame(t(c(characteristic = characteristic, 
                          dummy = NA,
                          case = mean.IQR(dat.case, characteristic), 
                          control = mean.IQR(dat.control, characteristic)
  )))
  colnames(out)[2] = ''
  
  return(out)
  
}


#### make table 
characteristics <- function(dat, characteristic.mean, characteristic.prop){
  
  ## extract data 
  # mean, IQR
  if (!is.null(characteristic.mean)) {
    mean.list = lapply(characteristic.mean, function(x) {print.mean(dat, x)})
    mean.tab = do.call('rbind', mean.list)
    out = mean.tab
  }
  
  # proportions, percentage 
  if (!is.null(characteristic.prop)) {
    prop.list = lapply(characteristic.prop, function(x) {print.prop(dat, x)})
    prop.tab = do.call('rbind', prop.list)
    out = prop.tab
  }
  
  if (!is.null(characteristic.prop) & !is.null(characteristic.mean)) {
    out = rbind.data.frame(mean.tab, prop.tab)
  }
  
  return(out)
}
