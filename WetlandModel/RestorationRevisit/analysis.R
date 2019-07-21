library(magrittr)
library(MCMCvis)

loadModel = function(x) {
  load(sprintf("Results/%s.RData", x))
  return(output)
}

#model 1 - 

output = loadModel(2)

predPost = MCMCsummary(MCMCchains(output$samples, params = c("logWET.p"))) %>% as.data.frame()
predPost_noR = MCMCsummary(MCMCchains(output$samples, params = c("logWET.noR"))) %>% as.data.frame()
predPost_HA= MCMCsummary(MCMCchains(output$samples, params = c("logWET.HA"))) %>% as.data.frame()

plot(density(predPost$`50%`), xlim=c(-4.5,3.5), ylim=c(0,1.2), main="Restoration in LA")
lines(density(predPost_noR$`50%`),col="red")
lines(density(predPost_HA$`50%`), col="blue")
legend("topleft", c("Current", "HA Restoration", "No Restoration"), col=c("black","blue","red"), lty=1)

plot(density(predPost_noR$`50%`), col="red", xlim=c(-4.5,2), ylim=c(0,1.2), main="Restoration in LA")
lines(density(predPost_HA$`50%`), col="blue")
legend("topleft", c("HA Restoration", "No Restoration"), col=c("blue","red"), lty=1)