var_tab <- function(mod){
  
  tab = round(car::vif(mod), 2)
  tab = tab[-nrow(tab),]
  colnames(tab) = c('VIF', 'Degree of freedom (Df)', 'VIF^(1/(2*Df))')
  
  
  ht = as_hux(as.data.frame(tab))
  caption(ht) = "The variance inflation factor (VIF) quantifies the effect of collinearity on the variance of the model regression estimates."
  ht %>%
    insert_column(after = 0, c('', rownames(tab))) %>%
    set_bold(1, everywhere) %>%
    set_align(everywhere, 2:4, "center") %>%
    set_width(1) %>%
    set_bottom_border(1, everywhere, value = 0.4) %>%
    set_caption_pos("bottom")
  
}