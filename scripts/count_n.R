count_n <- function(d, outcome, participant.type){
  
  if (outcome == 'total') {out = sum(d$case_control == participant.type, na.rm = T)}
  if (outcome == 'pp') {out = sum(d$case_control == participant.type & d$pp == 1, na.rm = T)}
  if (outcome == 'hospitalisation') {out = length(which(d$hospitalization == 'YES' & d$case_control == participant.type))}
  if (outcome == 'mortality') {
    alive = d$outcome
    alive[is.na(alive)] = 'UNKNOWN'
    out = table(alive[which(d$case_control == participant.type)])
  }
  if (outcome == 'fu') {out = length(which(!is.na(d$fu_inter_id) & d$case_control == participant.type))}
  
  if (outcome == 'vaccine2wks') {
    out = sum(d$init_form_date[which(d$case_control == participant.type)] - 
      d$init_vaccine_date[which(d$case_control == participant.type)] > 14 , na.rm = T)
  }
  
  if (outcome == 'vaccinemiss') {
    out = length(which(is.na(d$init_vaccine) & d$case_control == participant.type))
  }
  
  if (outcome == 'virologymiss') {
    out = length(which(is.na(d$init_viro_result)  & d$case_control == participant.type))
  }
  
  if (outcome == 'virology2wks') {
    out = sum(d$init_form_date[which(d$case_control == participant.type)] - 
                d$init_viro_collection_date[which(d$case_control == participant.type)] > 14, na.rm = T)
  }
  
  if (outcome == 'interviewmiss') {
    out = length(which(is.na(d$init_inter_id)  & d$case_control == participant.type))
  }
  
  if (outcome == 'serologypositive') {
    init = as.character(d$init_sero_result[which(d$case_control == 'CONTROL')]) == 'POSITIVE'
    fu = as.character(d$fu_sero_result[which(d$case_control == 'CONTROL')]) == 'POSITIVE'
    
    out = sum(init | fu, na.rm = T)
  }
  
  if (outcome == 'serologyinconclusive') {
    init = as.character(d$init_sero_result[which(d$case_control == 'CONTROL')]) == 'INCONCLUSIVE'
    fu = as.character(d$fu_sero_result[which(d$case_control == 'CONTROL')]) == 'INCONCLUSIVE'
    
    out = sum(init | fu, na.rm = T)
  }
  
  if (outcome == 'serologymiss') {
    init = is.na(d$init_sero_result[which(d$case_control == 'CONTROL')]) 
    fu = is.na(d$fu_sero_result[which(d$case_control == 'CONTROL')])
    
    out = sum(init | fu, na.rm = T)
  }
  
  return(out)
}
