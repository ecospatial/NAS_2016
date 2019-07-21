library(magrittr)
library(dplyr)
library(MCMCvis)

source("../../WetlandModel/loadTHK99.R")
source("../../RUtilityFunctions/codasamplesdic.R")

loadTHK99data(local=T, regions="ALL")

data = append(thk99buff_n, list(Nobs = nrow(thk99buff_n), Nregion = length(unique(thk99buff_n$region))))

params = c("WH", "TR", "CS", "RSLR", "NDVI")

models = list.files("Models/", full.names = T)
for(m in models)
{
  model = jags.model(m,
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c("b0", paste0("b", params), "logWET.p"),
                            n.iter=20000,
                            thin=1)
  
  out = MCMCsummary(output$samples) %>%
    as.data.frame() %>%
    cbind(param=row.names(.), .) %>%
    mutate(param=as.character(param)) %>%
    filter(grepl("logWET", param))
  
  name = gsub("Models/(.*)\\.txt", "\\1", m)
  write.table(out, sprintf("Results/%s.txt", name), sep="\t", row.names=F, quote=F)
}