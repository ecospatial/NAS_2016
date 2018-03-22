setwd("WetlandModel/New")

library(rjags)
source("../../RUtilityFunctions/codaSamplesDIC.R")

models = list.files("Interactions/Models/")

paramsFixed = c("bCS","bTR","bNDVI","bWH","bRSLR")

# !!!!! Must load data from WetlandModel/New/model.R !!!!!

for (m in models)
{
  combs = strsplit(m, "\\.")[[1]]
  combs = combs[-1]
  combs = combs[-length(combs)]
  params = paste0("bRSLRx",combs)
  
  file = sprintf("Interactions/Results/All.Interactions/%s.RData", paste0(combs,collapse=","))
  if (file.exists(file)) {
    print(sprintf("Skipping %s, already exists", file))
    next()
  }
  
  model = jags.model(sprintf("Interactions/Models/%s", m),
                     data = data,
                     n.chains=3,
                     n.adapt=2000)
  
  output = coda.samples.dic(model = model,
                            variable.names=c(params,paramsFixed),
                            n.iter=20000,
                            thin=1)
  write(sprintf("%s\t%s", paste0(combs,collapse=","), output$dic$deviance + output$dic$penalty),
        file = "Interactions/Results/All.Interactions/DIC_All.Interactions.txt",
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
      file = "Interactions/Results/DIC.txt",
      append = T)
save(output,file="Interactions/Results/211.RData")

dic = read.delim("Interactions/Results/DIC.txt", sep="\t", header=F)
names(dic) = c("Model", "DIC")
dic[order(dic$DIC),]
