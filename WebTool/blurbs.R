
htmlFormat = function(text){
  text = text %>% gsub("\\n\\s*\\n\\s*  ", "<p></p>", .) %>% paste0("<p>", .) %>% paste0(., "</p>")
  return(text)
}

blurbs = list(
  slr=htmlFormat("Background information coming soon."),
  
  subsidence=htmlFormat("Background information coming soon."),
  
  oilSpill=htmlFormat("Background information coming soon."),
  
  vegRestore=htmlFormat("Background information coming soon."),
  
  bwRestore=htmlFormat("Background information coming soon.")
)