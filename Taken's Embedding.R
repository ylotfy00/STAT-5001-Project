########################################################
# Persistent Homology for Time Series                  #
########################################################

data = read.csv("data1.csv")
########################################################
# Takens Embedding                                     #
########################################################
n = 100
d = 2

cab_roll = data[2]


ts = cab_roll
tau = which(abs(acf(ts,plot=F)$acf) < 2/sqrt(n))[1]-1
Takens=t(purrr::map_dfc(1:(n-(d-1)*tau),~ts[seq(from=.x,by=tau,length.out=d)]))
diag=ripsDiag(Takens, maxdimension=1, maxscale=max(dist(Takens)))
ts.plot(ts,main=paste("Period =",period))
plot(Takens,xlab ="x1",ylab="x2",main=paste("Takens Embedding with Period =",period))
plot(diag$diagram,main=paste("Persistence Diagram with Period =",period))
