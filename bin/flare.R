# Input flight data:
#  * CSV file
#  * sampling rate = 8Hz 
#
#
library(splines)

## Clean workspace
rm(list=ls(all=TRUE))

#Directorio do ficheiro de dados a ler
setwd("C:/Users/210906/Dropbox/EASA/flightRtools/flare/FlightDB")

flightdata <- read.table("CS-TNG_20121217_238971.csv",header=TRUE,sep=",")
#flightdata <- read.table("CS-TNM_20150614_889929.csv",header=TRUE,sep=",")


#####################
# Function Definition
#
# Finding parameters
testpar <- function(str){
  name <- vector(mode='character')
  pars <- grep(str, names(flightdata))
  for (i in 1:nrow(as.matrix(pars))){
    name[i] <- names(flightdata)[pars[i]]  
    #print(name)
  }
  return(name)
}
# 

# derivative of a vector
derivative <- function(vector,time_interval) {
  #f_i1 is the parameter value at instant i
  #f_i2 is the parameter value at instant i+1
  #time_interval is the total row number of the parameter for the derivative
  f_derivative=mat.or.vec(NROW(time_interval),1)
  f_i1=0
  f_i2=0
  instant=seq(from=0,to=time_interval,by=1)

  for (k in 1:(time_interval-1)) {
  f_i1[k]=vector[k]
  f_i2[k]=vector[k+1]
  f_derivative[k]=(f_i2[k]-f_i1[k])/((instant[k+1])-instant[k])
  }
  
  f_derivative[time_interval]=f_i2[k]
  
  return(f_derivative)
}

gaussian_smoothing <- function(parameter,sigma_chosen) {
  
  sigma=sigma_chosen
  soma=0
  t=seq(from=0,to=NROW(parameter)-1,by=1)
  
  for (j in 11:(NROW(t)-10)) {
    t0=t[j]
    soma[j]=0
    sumgauss=0
    k=j-10
    for (i in 1:20) {
      sumgauss=sumgauss+exp((-1)*((t0-t[k])^2)/(2*sigma^2)) 
      k=k+1
    }
    
    k=j-10
    for (i in 1:20) {
      weight=exp((-1)*((t0-t[k])^2)/(2*sigma^2))/sumgauss
      soma[j]=soma[j]+parameter[k]*weight
      k=k+1
    }
  }
  
  soma[1:10]=parameter[1:10]
  parameter=soma
  
  return (parameter)
}

##########

dim_file <- dim(flightdata)
nrows <- dim_file[1]
ncolumns <- dim_file[2]
#
# cleaning NANs fron TOUCHDOWNC
flightdata$TOUCH_DOWNC[which(is.na(flightdata$TOUCH_DOWNC))] <- 0
td_instant=min(which(flightdata$TOUCH_DOWNC != 0))

# Actions Performed below:
#    * Linear interpolations -> sampling rate = 8Hz
#    * cleaning NAs (when necessary)
#
pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
pitch[which(is.na(pitch))] <- 0
#
pitch_rate <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
pitch_rate[which(is.na(pitch_rate))] <- 0
#
raltd1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
fpa <- approx(seq(1:nrows), flightdata$FPA,xout=c(1:nrows), method="linear",n=nrows)$y
fpac <- approx(seq(1:nrows), flightdata$FPAC,xout=c(1:nrows), method="linear",n=nrows)$y
#ldg_on_1 <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
#ldg_on_2 <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
gsc <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
elev1 <- approx(seq(1:nrows), flightdata$ELEV_1,xout=c(1:nrows), method="linear",n=nrows)$y
elev2 <- approx(seq(1:nrows), flightdata$ELEV_2,xout=c(1:nrows), method="linear",n=nrows)$y
aoal <- approx(seq(1:nrows), flightdata$AOAL,xout=c(1:nrows), method="linear",n=nrows)$y
aoar <- approx(seq(1:nrows), flightdata$AOAR,xout=c(1:nrows), method="linear",n=nrows)$y



################################## 
# Nota HM -> não é necessário fazer este cálculo para todo o voo... basta localmente
# (ver Rauto.r)
pitch_rate_gs <- gaussian_smoothing(pitch_rate,4)
pitch_rate_dot <-derivative(pitch_rate_gs,NROW(pitch_rate_gs))

#  
cin <- 100
cfin <- 1
initial_time <- td_instant-cin
final_time <- td_instant+cfin

# Finding the Flare initiation instant
plot(pitch_rate_dot[initial_time:final_time],type="l",col="Blue",ylab="Pitch Accel",xlab="T",main="d(pitch_rate)/dt")
ptcr_smooth <- smooth.spline(pitch_rate_dot[initial_time:final_time],spar=0.6)
lines(ptcr_smooth,col="Red")
legend("bottomleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(h=0,lty=2)
abline(v=cin, lty=2)
text(cin,mean(pitch_rate_dot[initial_time:final_time]),"touch-down", cex=0.7)
# smooting doesn't help in this case! as the flare is determined by the maximum of
# the function
max_prdot <- max(pitch_rate_dot[initial_time:final_time])
t_flare <- which(pitch_rate_dot[initial_time:final_time]==max_prdot)
abline(v=t_flare, col="green")
text(t_flare,mean(pitch_rate_dot[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
#
plot(elev1[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="ELEV1",main="Elevator Left")
elev1_smooth <- smooth.spline(elev1[initial_time:final_time],spar=0.6)
lines(elev1_smooth,col="Red")
legend("bottomleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(elev1[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(elev1[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(elev2[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="ELEV1",main="Elevator Right")
elev2_smooth <- smooth.spline(elev2[initial_time:final_time],spar=0.6)
lines(elev2_smooth,col="Red")
legend("bottomleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(elev2[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(elev2[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
# 
plot(raltd1[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="RALTD1",main="Radio Altitude #1")
grid(,,col="dark red")
abline(v=cin, lty=2)
text(cin,mean(raltd1[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(raltd1[initial_time:final_time]),"flare", cex=0.7, col="green")
#
plot(pitch[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="PITCH (º)",main="Pitch")
pitch_smooth <- smooth.spline(pitch[initial_time:final_time],spar=0.6)
lines(pitch_smooth,col="Red")
legend("topleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(pitch[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(pitch[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(pitch_rate[initial_time:final_time],type="l",col="Blue",ylab="PITCH Rate",xlab="T",main="Pitch Rate Over Time")
pitch_rate_smooth <- smooth.spline(pitch_rate[initial_time:final_time],spar=0.6)
lines(pitch_rate_smooth,col="Red")
legend("topleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(h=0,lty=2)
abline(v=cin, lty=2)
text(cin,mean(pitch_rate[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(pitch_rate[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(fpac[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="FPAC",main="Flight Path Acceleration")
fpac_smooth <- smooth.spline(fpac[initial_time:final_time],spar=0.6)
lines(fpac_smooth,col="Red")
legend("bottomleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(fpac[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(fpac[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(fpa[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="FPA (º)",main="Flight Path Angle")
fpa_smooth <- smooth.spline(fpa[initial_time:final_time],spar=0.6)
lines(fpa_smooth,col="Red")
legend("topleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(fpa[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(fpa[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(aoal[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="AOA(º)",main="Angle of Attack Left")
aoal_smooth <- smooth.spline(aoal[initial_time:final_time],spar=0.6)
lines(aoal_smooth,col="Red")
legend("topleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(aoal[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(aoal[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")
#
plot(aoar[initial_time:final_time],type="l",col="Blue",xlab="T",ylab="AOA(º)",main="Angle of Attack Right")
aoar_smooth <- smooth.spline(aoar[initial_time:final_time],spar=0.6)
lines(aoar_smooth,col="Red")
legend("topleft",c("Raw Data","Smoothed"),col=c("Blue","Red"),lty=1,bty="n", cex=0.7)
abline(v=cin, lty=2)
text(cin,mean(aoar[initial_time:final_time]),"touch-down", cex=0.7)
abline(v=t_flare, col="green")
text(t_flare,mean(aoar[initial_time:final_time]),"flare", cex=0.7, col="green")
grid(,,col="dark red")


#####################################################3
# Final Calculations (optional)
# 
# Flare time in seconds
flare_time=(cin-t_flare)/8
print(flare_time)
# 
# flare_distance=0
#   for (k in t_flare:cin) {
#     flare_distance=(flare_distance+gsc[k]*0.51444444444/8)
#   }
# print(flare_distance)
