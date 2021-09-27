### functions for audit reports by site 

######## all forms done

count_isna <- function(d, var){
  paste0(sum(!is.na(d[[var]])), " (", round(mean(!is.na(d[[var]]))*100), "%)")
}

form_complete <- function(d){
  
  n = nrow(d)
  
  df = data.frame(
    time_point = c(rep('Enrolment', 5), rep('Follow-up', 4)), 
    labels = c('Investigator', 'Interview', 'Virology', 'Serology', 'Completion',
               'Investigator', 'Interview', 'Serology', 'Completion'),
    data = c(count_isna(d, var = 'init_invest_id'),
             count_isna(d, var = 'init_inter_id'),
             count_isna(d, var = 'init_viro_id'),
             count_isna(d, var = 'init_sero_id'),
             count_isna(d, var = 'init_form_completion_complete'),
             
             count_isna(d, var = 'fu_invest_id'),
             count_isna(d, var = 'fu_inter_id'),
             count_isna(d, var = 'fu_sero_id'),
             count_isna(d, var = 'fu_form_completion_complete'))
  )
  
  colnames(df) = c('Time point', 'Type of form', 'Completion (no, %)')
  
  kable(df, "html", align = "c") %>%
    kable_styling() %>%
    column_spec(column = 1:2, bold = T) %>%
    collapse_rows(columns = 1)
  
  
}


check_id <- function(d){
  
  col.to.extract = c(colnames(d)[grep('inter_id', colnames(d))], 
                     colnames(d)[grep('invest_id', colnames(d))], 
                     colnames(d)[grep('sero_id', colnames(d))], 
                     colnames(d)[grep('viro_id', colnames(d))])
  df = d[col.to.extract]
  
  ids = apply(df, 1, function(x){
    unique(x[!is.na(x)])
  })
  
  if (all(lengths(ids) <= 1)) {
    out = 'Participant IDs are all consistently entered in each of the above forms.'
  } else {
    
    id.diff = paste0(as.vector(unlist(lapply(ids[which(lengths(ids) != 1)], head, 1))), collapse = ', ')
    
    out = paste0(c('<span style="color: red;">','The following participants have inconsistent IDs entered onto the above forms: ', id.diff, '.</span>'), collapse = "")
  }
  
  return(out)
  
}

check_cc <- function(d){
  
  n.case = length(d$init_inter_id[which(d$case_control == 'CASE')])
  n.control = length(d$init_inter_id[which(d$case_control == 'CONTROL')])
  
  
  return(list(n.case = n.case, 
              n.control = n.control))
  
}

sero_tab <- function(d){
  
  n_case = nrow(d[which(d$case_control == 'CASE'),])
  n_control = nrow(d[which(d$case_control == 'CONTROL'),])
  
  checkmissingcol <- function(x, labstocheck) {
    if (!all(sort(labstocheck) == sort(names(x)))){
      missing = labstocheck[!labstocheck %in% names(x)]
      x[as.character(missing)] = '-'
    }
    
    x = x[c(labstocheck)]
    return(x)
  }
  
  test.prop <- function(type, test){
    
    if (test == 'sero'){
      
      n1 = table(d$init_sero_result[which(d$case_control == type)])
      n1 = checkmissingcol(n1, labstocheck = c('POSITIVE', 'NEGATIVE', 'INCONCLUSIVE'))
      n2 = table(d$fu_sero_result[which(d$case_control == type)])
      n2 = checkmissingcol(n2, labstocheck = c('POSITIVE', 'NEGATIVE', 'INCONCLUSIVE'))
      n = c(n1, n2) 
      
      
    } else {
      
      n1 = table(d$init_viro_result[which(d$case_control == type)])
      n1 = checkmissingcol(n1, labstocheck = c('POSITIVE FOR COVID-19', 'NEGATIVE FOR COVID-19', 'INCONCLUSIVE'))
      n2 = table(d$fu_viro_result[which(d$case_control == type)])
      n2 = checkmissingcol(n2, labstocheck = c('POSITIVE FOR COVID-19', 'NEGATIVE FOR COVID-19', 'INCONCLUSIVE'))
      n = c(n1, n2)
      
    }
    
    return(n)
    
  }
  
  sero_df = data.frame(sero_n = c('First visit', '', '', 'Follow up visit', '', ''), 
                       res = c('POSITIVE', 'NEGATIVE', 'INCONCLUSIVE', 'POSITIVE', 'NEGATIVE', 'INCONCLUSIVE'), 
                       case.sero = test.prop(type = 'CASE', test = 'sero'),
                       case.viro = test.prop(type = 'CASE', test = 'viro'),
                       control.sero = test.prop(type = 'CONTROL', test = 'sero'),
                       control.viro = test.prop(type = 'CONTROL', test = 'viro'))
  colnames(sero_df) = c('', '', 'Labelled as cases (n, %)', '', 'Labelled as controls (n, %)', '')
  sero_df = rbind(c('', '', paste0('n = ', n_case), '', paste0('n = ', n_control),''), 
                  c('', '', 'Serology', 'PCR', 'Serology', 'PCR'),
                  sero_df)
  
  sero = kable(sero_df, "html", align = "c") %>%
    kable_styling() %>%
    column_spec(column = 1:2, bold = T) %>%
    collapse_rows(columns = 1)
  
  return(sero)
  
}

count_pp <- function(d){
  
  case_pos1pcr_id = d$row.no[which(d$init_viro_result == 'POSITIVE FOR COVID-19' & d$case_control == 'CASE')]
  
  case_pp_id = unique(case_pos1pcr_id)
  n_case_pp = length(case_pp_id)
  
  control_neg1serology_id = d$row.no[which(d$init_sero_result == 'NEGATIVE' & d$case_control == 'CONTROL')]
  control_neg2serology_id = d$row.no[which(d$fu_sero_result == 'NEGATIVE' & d$case_control == 'CONTROL')]
  control_neg1pcr_id = d$row.no[which(d$init_viro_result == 'NEGATIVE FOR COVID-19'& d$case_control == 'CONTROL')]
  control_neg2pcr_id = d$row.no[which(d$fu_viro_result == 'NEGATIVE FOR COVID-19'& d$case_control == 'CONTROL')]
  
  control_pp_id = intersect(control_neg1serology_id, control_neg2serology_id) # need to have 2 serology tests
  n_control_pp = length(control_pp_id)
  
  return(list(n_case_pp =  n_case_pp, n_control_pp = n_control_pp))
}




