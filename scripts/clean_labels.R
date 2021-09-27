clean_labels <- function(labels){
  
  
  labels = unlist(lapply(labels, function(x){
    gsub('\\_', ' ', x)
  }))
  
  labels = unlist(lapply(labels, function(x){
    gsub('init formI', 'I', x)
  }))
  
  labels = unlist(lapply(labels, function(x){
    gsub('fu investF', 'F', x)
  }))
  
  labels = unlist(lapply(labels, function(x){
    gsub('init', 'initial', x)
  }))
 
   labels = unlist(lapply(labels, function(x){
    gsub('fu', 'follow-up', x)
  }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('\\_', ' ', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('viro', 'virology', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('sero', 'serology', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('prohpyl', 'prophylaxis ', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('bodyfld', 'body fluid', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub(' no ', 'number', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('ipc ', 'IPC ', x)
   }))
   
   
   labels = unlist(lapply(labels, function(x){
     gsub('hcf', 'HCF', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('prolong15min', 'for more than 15 min', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('ppe_type', 'type of PPE ', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub(' inter ', ' interview ', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub(' invest ', ' investigator ', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('othersymptom', 'other symptoms', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('IPC pt practice', 'do you follow IPC practices when in contact with patient', x)
   }))
   
   labels = unlist(lapply(labels, function(x){
     gsub('otherneuro', 'other neurological symptoms', x)
   }))
   
   
   labels = unlist(lapply(labels, function(x){
     gsub('completion complete', 'completion', x)
   }))

  return(labels)
  
  
}
