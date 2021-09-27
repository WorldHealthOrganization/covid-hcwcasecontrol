clean_hcf_name <- function(hcfnames){
  
  name = unique(unlist(hcfnames[1:3]))
  
  if (length(name) == 1) { 
    ## if all  "site.godata" "init_hcf_name" "fu_hcf_name" same, fill "hcf_form_hcf_name" with the same 
    hcfnames = rep(name, 4)
  } else {
    ## if "init_hcf_name" "fu_hcf_name" are part of "site.godata"
    names = unique(unlist(hcfnames[2:3]))
    names = names[!is.na(names)]
    names = gsub("\\-"," ", names)
    name = word(names, 1) # take the first word 
    if (any(name %in% c('GENERAL', 'CLINICAL', 'REG.', 'GH', 'REGIONAL', 'UKC', 'CC')) == T) {name = word(names, -1)} # to cater for e.g. 'GENERAL HOSPITAL IN FERIZAJ'
    name = gsub(",","", name)
    name = gsub("\\,",", ", name)
    name = gsub("\\.",". ", name)
    if (all(name != 'HOSPITAL') & all(name != 'GENERAL')) { 
      ## all "init_hcf_name" "fu_hcf_name" filled with a name that is a subset of  "site.godata" , fill "hcf_form_hcf_name" with  "site.godata" 
      if (grepl(paste(name, collapse = '.*'), hcfnames[1])){
        hcfnames = rep(hcfnames[1], 4)
      } else {
        ## "init_hcf_name" "fu_hcf_name" not filled with a name that is a subset of  "site.godata" 
        if (length(name) == 2) {
          ## if 2 different names in  "init_hcf_name" "fu_hcf_name", means follow up site different but fill all others as enrolment site 
          hcfnames = c(hcfnames[2], hcfnames[2], hcfnames[3], hcfnames[2])
        }
      }
    }
  }
  
  #special cases - those with no specific GPS added into godata, or different language 
  if (hcfnames[1] %in% c('ICDDR,B, DHAKA, BANGLADESH', 'ESWATINI', 'NAIROBI', 'ICDDR,B, DHAKA, BANGLADESH', 
                         'MOMBASA', 'PORTREITZ', 'KIAMBU', 'UASIN GISHU', 'ZAMBIA')) {
    ## if "init_hcf_name" "fu_hcf_name" same
    name = unique(unlist(hcfnames[2:3]))
    name = name[!is.na(name)]
    name = gsub(",","", name)
    name = gsub("\\,",", ", name)
    name = gsub("\\.",". ", name)
    if (length(name) == 1){
      #### fill all with "init_hcf_name" 
      hcfnames = rep(hcfnames[2], 4)
    }
  } 
  
  if (hcfnames[1] %in% c(unique(dat$site.godata[which(dat$country.godata == 'SAUDI ARABIA')]),
                         unique(dat$site.godata[which(dat$country.godata == 'JORDAN')]),
                         unique(dat$site.godata[which(dat$country.godata == 'UKRAINE')])
  )) {
    ## if "init_hcf_name" "fu_hcf_name" same
    name = unique(unlist(hcfnames[2:3]))
    name = name[!is.na(name)]
    name = gsub(",","", name)
    name = gsub("\\,",", ", name)
    name = gsub("\\.",". ", name)
    if (length(name) == 1){
      #### fill all with "site.godata" name in english
      hcfnames = rep(hcfnames[1], 4)
    }
  } 
  
  if (hcfnames[1] %in% 'CLINICAL CENTER OF SERBIA') { # filled in clinic name instead 
    ## if "init_hcf_name" "fu_hcf_name" same
    name = unique(unlist(hcfnames[2:3]))
    name = name[!is.na(name)]
    name = gsub(",","", name)
    name = gsub("\\,",", ", name)
    name = gsub("\\.",". ", name)
    if (length(name) == 1){
      #### fill all with "init_hcf_name" 
      hcfnames = rep(hcfnames[1], 4)
    }
  }
  
  return(as.vector(unlist(hcfnames)))
}
