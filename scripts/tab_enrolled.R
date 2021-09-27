
tab_enrolled <- function(by.country = T, d = d){
  
  # total 
  n_case = count_n(d = d, outcome = 'total', participant.type = 'CASE')
  n_control = count_n(d = d, outcome = 'total', participant.type = 'CONTROL')
  n_case_pp = count_n(d = d, outcome = 'pp', participant.type = 'CASE')
  n_control_pp = count_n(d = d, outcome = 'pp', participant.type = 'CONTROL')
  
  # breakdown by country or site 
  by = ifelse(by.country == T, 'country.godata', 'site.godata')
  
  site_name = names(table(d[[by]]))
  
  n = list()
  
  for (site in site_name){
    
    #numbers 
    n[[site]][1] = sum(d[[by]] == site) # total enrolled 
    n[[site]][2] = sum(d[[by]] == site & d$pp == 1) # total pp 
    n[[site]][3] = sum(d[[by]] == site & d$case_control == 'CASE', na.rm = T) # total case
    n[[site]][4] = sum(d[[by]] == site & d$pp == 1 & d$case_control == 'CASE', na.rm = T)  
    n[[site]][5] = sum(d[[by]] == site & d$case_control == 'CONTROL', na.rm = T) # total control
    n[[site]][6] = sum(d[[by]] == site & d$pp == 1 & d$case_control == 'CONTROL', na.rm = T)
    
  }
  
  n[['TOTAL']] = c(paste0(n_case + n_control, ' (100%)'), 
                   paste0(n_case_pp + n_control_pp, ' (', round((n_case_pp + n_control_pp)/(n_case + n_control)*100),'%)'),
                   paste0(n_case, ' (100%)'), 
                   paste0(n_case_pp, ' (', round((n_case_pp)/(n_case)*100),'%)'),
                   paste0(n_control, ' (100%)'), 
                   paste0(n_control_pp, ' (', round((n_control_pp)/(n_control)*100),'%)'))
  
  df = do.call('rbind', n)
  
  head1 = c('', 'Total', '', 'Cases','', 'Controls','')
  head2 = c('', rep(c('Enrolled', 'Main analysis'), 3))
  
  as_hux(df, add_rownames = T) %>% 
    insert_row(head1, after = 0) %>% 
    insert_row(head2, after = 1) %>%
    merge_cells(1, 2:3) %>% 
    merge_cells(1, 4:5) %>% 
    merge_cells(1, 6:7) %>% 
    set_align(1, 2:7, "center") %>% 
    set_tb_padding(1, everywhere, 0) %>% 
    set_bold(1, everywhere) %>% 
    set_header_rows(1:2, TRUE) %>% 
    set_header_cols(1, TRUE) %>% 
    style_headers(bold = TRUE, text_color = "grey40")  %>% 
    set_bottom_border(2, everywhere) %>% 
    set_bottom_border(final(1), everywhere)
  
}