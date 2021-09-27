classify_cat <- function(d, var, from, to) {
  
  tochange = as.character(d[[var]])
  tochange[which(tochange %in% from)] = to
  tochange = as.factor(tochange)
  tochange = factor(tochange, levels = unique(tochange))
  d[[var]] = tochange

  return(d)
}