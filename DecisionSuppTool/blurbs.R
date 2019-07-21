
htmlFormat = function(x){
  text = x %>% paste0(collapse="</p><p>") %>% paste0("<p>", .) %>% paste0(.,"</p>")
  return(text)
}

blurbs = list(
  year=htmlFormat("Background information coming soon."),
  
  scenarios=htmlFormat("Background information coming soon."),
  
  subsidence=htmlFormat("Background information coming soon."),
  
  oilSpill=htmlFormat("Background information coming soon."),
  
  vegRestore=htmlFormat("Background information coming soon."),
  
  bwRestore=htmlFormat("Background information coming soon."),
  
  haRestore=htmlFormat("Background information coming soon.")
)

