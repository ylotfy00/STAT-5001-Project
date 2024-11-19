########################################################
# Persistent Homology for Time Series                  #
########################################################
if (!require("tseries")) install.packages("tseries")

library(tseries)

if (!require("plyr")) install.packages("plyr")

library(plyr)

if (!require("purrr")) install.packages("purrr")

library(purrr)

births <- scan("nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
plot.ts(birthstimeseries)

birthstimeseriescomponents <- decompose(birthstimeseries)



plot(birthstimeseriescomponents)

plot(birthstimeseriescomponents$random)



opar <- par(mfrow=c(1,2), mex=0.6, mar=c(5,4,3,2)+.3)

acf(birthstimeseries,lag.max=20)

acf(birthstimeseriescomponents$random,na.action=na.pass,lag.max=20)

par(opar)


########################################################
# Takens Embedding                                     #
########################################################
n = 480
d = 2

period = 12
ts1 = cos(1:n*2*pi/period)

period = 48
ts2 = cos(1:n*2*pi/period)

period = 96
ts3 = cos(1:n*2*pi/period)

ts = ts3
tau = which(abs(acf(ts,plot=F)$acf) < 2/sqrt(n))[1]-1
Takens=t(purrr::map_dfc(1:(n-(d-1)*tau),~ts[seq(from=.x,by=tau,length.out=d)]))
diag=ripsDiag(Takens, maxdimension=1, maxscale=max(dist(Takens)))
ts.plot(ts,main=paste("Period =",period))
plot(Takens,xlab ="x1",ylab="x2",main=paste("Takens Embedding with Period =",period))
plot(diag$diagram,main=paste("Persistence Diagram with Period =",period))
