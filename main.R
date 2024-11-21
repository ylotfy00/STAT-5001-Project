install_packages()

data = read.csv("data1.csv")
head(data)

data = read.csv("test.wav")

head(data)
     
     
########################################################
# Takens embedding on wav file #
########################################################
     
library(seewave)
library(tuneR)

wav_file = readWave("test.wav")

head(wav)

mono_data = wav_file@left

summary(mono_data)

oscillo(wav_file)

n = length(mono_data)
d = 2
period = 12

ts = mono_data

tau = which(abs(acf(ts,plot=F)$acf) < 2/sqrt(n))[1]-1
Takens=t(purrr::map_dfc(1:(n-(d-1)*tau),~ts[seq(from=.x,by=tau,length.out=d)]))
diag=ripsDiag(Takens, maxdimension=1, maxscale=max(dist(Takens)))
ts.plot(ts,main=paste("Period =",period))
plot(Takens,xlab ="x1",ylab="x2",main=paste("Takens Embedding with Period
=",period))
plot(diag$diagram,main=paste("Persistence Diagram with Period =",period))