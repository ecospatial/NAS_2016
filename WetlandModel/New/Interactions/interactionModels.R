setwd("WetlandModel/New")

library(rjags)
source("../../RUtilityFunctions/codaSamplesDIC.R")

folderName = "All.Interactions.SlopeInter"
resultsDir = sprintf("Interactions/Results/%s", folderName)
models = list.files(sprintf("Interactions/Models/%s", folderName))

paramsFixed = c("bCS","bTR","bNDVI","bWH","bRSLR","b0")

# !!!!! Must load data from WetlandModel/New/model.R !!!!!

write.table("modelNo\tfixed\trandom\tDIC", sprintf("%s/DIC_%s.txt", resultsDir, folderName), row.names=F, quote=F, sep="\t")

for (m in models)
{
  combs = strsplit(m, "\\.")[[1]]
  combs = combs[-1]
  combs = combs[-length(combs)]
  params = paste0("bRSLRx",combs)
  
  file = sprintf("%s/%s.RData", resultsDir, paste0(combs,collapse=","))
  if (file.exists(file)) {
    print(sprintf("Skipping %s, already exists", file))
    next()
  }
  
  model = jags.model(sprintf("Interactions/Models/%s/%s", folderName, m),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c(params,paramsFixed),
                            n.iter=20000,
                            thin=1)
  write(sprintf("%s\t%s", paste0(combs,collapse=","), output$dic$deviance + output$dic$penalty),
        file = sprintf("%s/DIC_%s.txt", resultsDir, folderName),
        append = T)
  save(output,file=file)
}

#Model 211
model = jags.model("Models/logPCT~RSLR.WH.TR.CS.NDVI/211.txt",
                   data = data,
                   n.chains=3,
                   n.adapt=2000)

output = coda.samples.dic(model = model,
                          variable.names=c(params,paramsFixed),
                          n.iter=20000,
                          thin=1)
write(sprintf("%s\t%s", "211", output$dic$deviance + output$dic$penalty),
      file = sprintf("%s/DIC_%s.txt", resultsDir, folderName),
      append = T)
save(output,file=sprintf("%s/211.RData", resultsDir))