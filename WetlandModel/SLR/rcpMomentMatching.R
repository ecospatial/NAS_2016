pctiles = c(0.05, 0.17, 0.83, 0.95)

RCPnames = c("RCP3_2100", "RCP3_2300", "RCP85_2100", "RCP85_2300")
RCP3_2100 = c(0.25, 0.4, 0.6, 0.7)
RCP85_2100 = c(0.5, 0.7, 1.2, 1.5)
RCP3_2300 = c(0.5, 0.6, 1.0, 1.2)
RCP85_2300 = c(1.3, 2.0, 3.0, 4.0)

RCPscens = list()
for (name in RCPnames)
{
  RCP = get(name)
  i=1
  scens = data.frame(mu=numeric(0), sd=numeric(0), devsq=numeric(0))
  for (mu in seq(min(RCP), max(RCP), by=0.01))
  {
    for (sd in seq(0, max(RCP)-min(RCP), by = 0.01))
    {
      qs = qnorm(pctiles, mu, sd)
      devs = RCP-qs
      scens[i,] = c(mu=mu, sd=sd, devsq=sum(devs^2))
      i = i + 1
    }
  }
  best = scens[order(scens$devsq),][2,]
  RCPscens[[name]] = best
}

write.table(RCPscens, "out.txt", quote = F, row.names = F, sep = "\t")

# RCP3_2100
# 0.48	0.13
# RCP3_2300
# 0.83	0.21
# RCP85_2100
# 0.97	0.29
# RCP85_2300
# 2.58	0.75

op=par(mfrow=c(2,2))
for (name in RCPnames)
{
  RCP = RCPscens[[name]]
  plot(density(rnorm(100000, mean = RCP$mu, sd = RCP$sd)), main = name)
  abline(v=qnorm(pctiles, mean = RCP$mu, sd = RCP$sd), col="blue")
  abline(v=get(name), col="red")
}
par(op)


# scensGam = data.frame(a=numeric(0), b=numeric(0), devsq=numeric(0))
# RCP=RCP3_2100
# i=1
# for (a in seq(0, 3, by=0.01))
# {
#   for (b in seq(0, 3, by = 0.01))
#   {
#     qs = qgamma(pctiles, a, b)
#     devs = RCP-qs
#     scensGam[i,] = c(a=a, b=b, devsq=sum(devs^2))
#     i = i +1
#   }
# }
# 
# head(scensGam[order(scensGam$devsq),])
# 
# bestGam = scensGam[order(scensGam$devsq),][1,]
# qgamma(pctiles, bestGam$a, bestGam$b)

pctiles*RCP3_2100
