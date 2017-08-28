#########################################
# Restoration Scenarios
#########################################
#ORIG_FID list for each baseline
raccBaseline = c(415, 420, 407, 433, 484, 430, 442, 445, 457)
trinityBaseline = raccBaseline
gisleBaseline = c(422, 438, 454, 510, 535, 538, 558, 583)
#ORIG_FID list for restoration
raccRestore = c(462, 447, 465)
trinityRestore = c(404, 409, 413)
gisleRestore = c(480, 487)

raccAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% raccRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% raccBaseline])
trinityAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% trinityRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% trinityBaseline])
gisleAvgNDMI = mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% gisleRestore]) -
  mean(inlandbuff$NDMI[inlandbuff$ORIG_FID %in% gisleBaseline])

bwNDMI = mean(c(raccAvgNDMI, gisleAvgNDMI))
vegNDMI = trinityAvgNDMI

#########################################
# Oil Drilling Scenarios
#########################################
# Subsidence (Chang, Mallman, Zoback 2014)
years=c(1965,1982,1992,2002,2012)
cumsub=c(0,-8,-16,-17.75,-19)

s_pred_max = 20
asymp_pred = -20
inflect_pred = 1987

s.try=data.frame(s=seq(0,s_pred_max,by=0.01), ssr=rep(NA, length(seq(0,s_pred_max,by=0.01))))
for(s in seq(0,s_pred_max,by=0.01)){
  fit=SSlogis(years,asymp_pred,inflect_pred,s)
  ssr=sum((cumsub-fit)^2)
  s.try[s.try$s==s,]$ssr = ssr
}
s_pred = s.try$s[s.try$ssr == min(s.try$ssr)]

cumsub_pred = SSlogis(seq(1965,2014),asymp_pred,inflect_pred,s_pred)
# plot(cumsub_pred~seq(1965,2015))
# points(cumsub~years,col="red")

sub_rate=-diff(cumsub_pred)

# Oil NDMI Reduction via Spill (Mishra et al 2012; Khanna et al 2013; Hese and Schmullius 2009)
oiledNDMI = c(0.079,0.127,0.253,.347,0.393,0.418,0.413,0.408,0.400,0.398,0.400,0.400)
nonNDMI = c(0.134,0.209,0.356,0.400,0.411,0.416,0.411,0.400,0.395,0.395,0.387,0.387)

ratioNDMI = 1-oiledNDMI/nonNDMI

oilNDMI = mean(ratioNDMI[1:3])