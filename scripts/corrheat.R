corrheat <- function(d, explanatory_vars){
  
  d.s = d %>% 
    dplyr::select(explanatory_vars, case_control)
  
  corrm = catcorrm(explanatory_vars, d.s)
  
  m = reshape2::melt(corrm)
  ggplot(m, aes(Var1, Var2, fill= value)) + 
    geom_tile(aes(fill = cut(value, breaks=seq(0, 1, length.out = 6), labels= c('<0.2','0.2-0.4','0.4-0.6','0.6-0.8','>0.8'))), colour = "grey") + 
    scale_fill_manual(drop=FALSE, values=c('white','#FFD8BE','#FB4B4E','red','#101D42'), na.value="#EEEEEE", name="Cramer's V") + 
    labs(y = '', x = '') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) + 
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}