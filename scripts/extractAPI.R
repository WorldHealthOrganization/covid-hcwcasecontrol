# extract data
extractAPI.cases <- function(outbreak_id) {
  
  link = paste0(url,"api/outbreaks/", outbreak_id, "/cases")
  
  lapply(link, function(x) {
    response = GET(x, 
                   add_headers(Authorization = paste("Bearer", access_token, sep = " ")))
    json_cases = content(response, as = "text")
    y = as_tibble(fromJSON(json_cases, flatten = TRUE))
  })
  
}

extractAPI.admin <- function(info.type) {
  
  link = paste0(url,"api/", info.type)
  response = GET(link,add_headers(Authorization = paste("Bearer", access_token, sep = " ")))
  json_cases = content(response, as = "text")
  as_tibble(fromJSON(json_cases, flatten = TRUE))
  
}
