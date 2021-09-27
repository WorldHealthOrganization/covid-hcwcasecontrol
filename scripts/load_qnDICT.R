# question dictionary 
qnDICT = read.csv('~/Documents/nBox/git_projects/WHO_covid19_HCW_casecontrol/data_dictionary/qnDICT.csv', header = T)
qnDICT = as.data.frame(apply(qnDICT, 2, str_trim)) #remove all spaces before and after 
#qnDICT$qn_code[duplicated(qnDICT$qn_code)] # should be null 