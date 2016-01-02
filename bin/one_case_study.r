######################################################################
# Take-Off -> One Case Study
# - 
# - 
# 
#
#


# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

library(ggplot2)
library(brew)
library(tools)
library(data.table)
library(psych)
library(lubridate)
library(xtable)
library(data.table)
library(psych)

# finding parameters - Trouble-shooting
# use:
# > testpar("EXAMPLE")
testpar <- function(str){
  name <- vector(mode='character')
  pars <- grep(str, names(flightdata))
  for (i in 1:nrow(as.matrix(pars))){
    name[i] <- names(flightdata)[pars[i]]  
    #print(name)
  }
  return(name)
}


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


to_plots <- function(){

  # Take-Off Start and End Points determination
  t0 <- min(which(flightdata$FM_FWC==3)) - 200
  t1 <- max(which(flightdata$FM_FWC==4)) + 50
  
  ptcr_dot <-derivative(ptcr[t0:t1],NROW(ptcr[t0:t1]))
  gs_dot <-derivative(gs[t0:t1],NROW(gs[t0:t1]))
  
  # studies - support plots
  data_takeoff <- data.frame()
  
  #time <- vector()
  time <- c(0,seq(t1-t0)/8)
  data_takeoff <- as.data.frame(cbind(time, gs[t0:t1],raltd1[t0:t1],
                      n11[t0:t1], n21[t0:t1],n12[t0:t1], n22[t0:t1],
                      ff1[t0:t1], ff2[t0:t1], raltd2[t0:t1],
                      egt1[t0:t1], egt2[t0:t1], long[t0:t1], ptcr_dot,
                      pitch_cpt[t0:t1],pitch_fo[t0:t1],pitch[t0:t1],
                      ptcr[t0:t1]))
  
  names(data_takeoff) <- c("time", "GS", "RALTD1", "N11", "N21", "N12", "N22",
                           "FF1", "FF2", "RALTD2", "EGT1", "EGT2", "LONG",
                           "PTCR_DOT","PITCH_CPT","PITCH_FO", "PITCH",
                           "PTCR")
  # Lift-Off detection
  tlg <- min(which(LG_left[t0:t1]==0))
  trg <- min(which(LG_right[t0:t1]==0))
  
  loff <- max(tlg,trg) # Lift Off according to MLG criteria
  
  r1 <- raltd1[t0:t1]
  r2 <- raltd2[t0:t1]
  
  # loff radio alt correction
  if ( r1[loff] > 1  || r2[loff] > 1) {
    
    min <- loff - 20
    max <- loff + 10
    
    r1_positive <- min(which(r1[min:max]>0))
    lo_r1 <- min + r1_positive
    
    r2_positive <- min(which(r2[min:max]>0))
    lo_r2 <- min + r2_positive
    
    #lo_ralt <- min(lo_r1,lo_r2)
    loff <- max(lo_r1,lo_r2)    
}
  
  loff_sec <- loff/8 # in seconds

  # Rotation point
  rot <- min(which(ptcr[t0:t1]>1))
  rotation_time <- (loff - rot)/8 
  rot <- rot/8 # seconds

  # Graphics
  setwd(figurepath)
  
  png("to_gs.png")
  p <- qplot(time, GS, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]",  geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_raltd1.png")
  p <- qplot(time, RALTD1, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_n11.png")
  p <- qplot(time, N11, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_n21.png")
  p <- qplot(time, N21, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_n12.png")
  p <- qplot(time, N12, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size =I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_n22.png")
  p <- qplot(time, N22, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_ff1.png")
  p <- qplot(time, FF1, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_ff2.png")
  p <- qplot(time, FF2, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  
  png("to_raltd2.png")
  p <- qplot(time, RALTD2, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  
  png("to_egt1.png")
  p <- qplot(time, EGT1, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  png("to_egt2.png")
  p <- qplot(time, EGT2, data=data_takeoff, col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
            geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()
  
  
  png("to_long.png")
  p <- qplot(time, LONG, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) +
             geom_vline(xintercept=loff_sec, color="dark red", size=1) + 
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_ptcr_dot.png")
  p <- qplot(time, PTCR_DOT, data=data_takeoff,  col=I("blue"),
             xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
             geom_vline(xintercept=loff_sec, color="dark red", size=1) +
             geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_pitch_cpt.png")
  p <- qplot(time, PITCH_CPT, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  png("to_pitch_fo.png")
  p <- qplot(time, PITCH_FO, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  png("to_pitch.png")
  p <- qplot(time, PITCH, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()


  png("to_ptcr.png")
  p <- qplot(time, PTCR, data=data_takeoff,  col=I("blue"),
           xlab="Time [seconds]", geom=c("line"), size=I(1)) + 
           geom_vline(xintercept=loff_sec, color="dark red", size=1) +
           geom_vline(xintercept=rot, color="green", size=1)
  print(p)
  dev.off()

  
  # PDF Reporting
  #setwd(binpath)
  setwd(resultpath)
  brew("Rtoffrep.brew", paste0('Rtoffrep',s,".tex"))
  texi2dvi(paste0('Rtoffrep',s,".tex"), pdf = TRUE)
  ##$##  
}

landing_plots <- function(){
  
  # cleaning NANs fron TOUCHDOWNC
  flightdata$TOUCH_DOWNC[which(is.na(flightdata$TOUCH_DOWNC))] <- 0
  td_instant=min(which(flightdata$TOUCH_DOWNC != 0))
  td_instant <- td_instant -2
  
  
  # Landing Initial and Final points
  delta_ti <- 60
  delta_tf <- 60
  init_t <- td_instant-delta_ti
  final_t <- td_instant+delta_tf
  
  t_ldg_lft <- init_t + min(which(flightdata$LDG_ON_1[init_t:final_t]==1))
  t_ldg_rgt <- init_t + min(which(flightdata$LDG_ON_2[init_t:final_t]==1))
  t_mlg <- min(t_ldg_lft, t_ldg_rgt)
  
  t_ldg_nos <- init_t + min(which(flightdata$LDGN_ON[init_t:final_t]==1))
  delta_t_ldg <- (t_ldg_nos - t_mlg)/8
  
  max_vrtg <- max(vrtg[init_t:final_t])
  min_ptcr <- min(ptcr[init_t:final_t])
  min_long <- min(long[init_t:final_t])
  
  max_latg <- max(latg[init_t:final_t])
  min_latg <- min(latg[init_t:final_t])
  latg_ampl <- max_latg - min_latg
  
  max_hdg <- max(head_mag[init_t:final_t])
  min_hdg <- min(head_mag[init_t:final_t])
  hdg_ampl <- max_hdg - min_hdg
  
  gs_at_td <- gs[td_instant]
  pitch_at_td <- pitch[td_instant]
  ivv_at_td <- ivv[td_instant]
  ptcr_at_td <- ptcr[td_instant]
  e_at_td <- gs[td_instant]*c_knot_ms*gw1kg[td_instant]/2

  setwd(figurepath)  
  
  png("pitch.png")
  plot(pitch[init_t:final_t],type="l", col="blue", main="pitch", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("vrtg.png")
  plot(vrtg[init_t:final_t],type="l", col="blue",main="vrtg", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("long.png")
  plot(long[init_t:final_t],type="l", col="blue",main="long", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("latg.png")
  plot(latg[init_t:final_t],type="l", col="blue",main="latg", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("head_mag.png")
  plot(head_mag[init_t:final_t],type="l", col="blue",main="head_mag", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("gs.png")
  plot(gs[init_t:final_t],type="l", col="blue",main="gs", lwd=1.5)
  abline(v=delta_ti,col="green",lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("ptcr.png")
  plot(ptcr[init_t:final_t],type="l", col="blue",main="ptcr", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("raltd1.png")
  plot(raltd1[init_t:final_t],type="l", col="blue" ,main="raltd1", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("raltd2.png")
  plot(raltd2[init_t:final_t],type="l", col="blue" ,main="raltd2", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_left.png")  
  plot(LG_left[init_t:final_t],type="l", col="blue", main="LG Left", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_right.png")  
  plot(LG_right[init_t:final_t],type="l", col="blue", main="LG right", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  png("lg_nose.png")
  plot(LG_nose[init_t:final_t],type="l", col="blue", main="LG nose", lwd=1.5)
  abline(v=delta_ti,col="green", lwd=2)
  grid(,,col="dark red")
  dev.off()
  
  # PDF Reporting
  #setwd(binpath)
  setwd(resultpath)
  brew("autorep.brew", paste0('autorep',s,".tex"))
  texi2dvi(paste0('autorep',s,".tex"), pdf = TRUE)
    
}



##################################################### MAIN
### Choose the type of analysis:
###  * set 1 in the report desired
###    
take_off_study = 1
landing_study = 0


## Constants
c_knot_ms <- 1852/3600

## paths

flightpath <- "C:/FlightDB/TTD"    ## Insert case into the respective folder
binpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/bin"
resultpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/results"
figurepath <- "C:/Users/210906/Dropbox/EASA/flightRtools/Rtoff/results/figures"

# file list
setwd(flightpath)
fileList <- list.files(path=flightpath, pattern=".csv")

## Escolher o ficheiro pelo n� "s"
#s = 1
s=10


#for (s in 1:NROW(fileList)) {

# Variable and Vector Initialization
#
#flight_measurements <- mat.or.vec(NROW(fileList),20) # Matrix of dimensions: [fligths x Measurements]
#flight_measurements <- data.frame()
#mean_phase_store <- mat.or.vec(NROW(fileList),1)

  
  ## LOAD Flight From List
  setwd(flightpath)
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                      colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', 
                      RUNWAY_TO='character', RUNWAY_LD='character', DESTINATION='character', 
                      DATE='Date')) 
  
  
    nrows <- NROW(flightdata)
        
## Interpolations
    pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
    ptcr <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_cpt <- approx(seq(1:nrows), flightdata$PITCH_CPT,xout=c(1:nrows), method="linear",n=nrows)$y
    pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
    alt_std <- approx(seq(1:nrows), flightdata$ALT_STDC,xout=c(1:nrows), method="linear",n=nrows)$y
    raltd1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
    raltd2 <- approx(seq(1:nrows), flightdata$RALTD2,xout=c(1:nrows), method="linear",n=nrows)$y
    head_mag <- approx(seq(1:nrows), flightdata$HEAD_MAG,xout=c(1:nrows), method="linear",n=nrows)$y
    ivv <- approx(seq(1:nrows), flightdata$IVV,xout=c(1:nrows), method="linear",n=nrows)$y
    long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
    latg <- approx(seq(1:nrows), flightdata$LATG,xout=c(1:nrows), method="linear",n=nrows)$y
    roll <- approx(seq(1:nrows), flightdata$ROLL,xout=c(1:nrows), method="linear",n=nrows)$y
    ias <- approx(seq(1:nrows), flightdata$IASC,xout=c(1:nrows), method="linear",n=nrows)$y
    gs <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
    gw1kg <- approx(seq(1:nrows), flightdata$GW1KG,xout=c(1:nrows), method="linear",n=nrows)$y
    fpa <- approx(seq(1:nrows), flightdata$FPA,xout=c(1:nrows), method="linear",n=nrows)$y
    fpac <- approx(seq(1:nrows), flightdata$FPAC,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_left <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_nose <- approx(seq(1:nrows), flightdata$LDGN_ON,xout=c(1:nrows), method="linear",n=nrows)$y
    LG_right <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
    aoal <- approx(seq(1:nrows), flightdata$AOAL,xout=c(1:nrows), method="linear",n=nrows)$y
    aoar <- approx(seq(1:nrows), flightdata$AOAR,xout=c(1:nrows), method="linear",n=nrows)$y
    n11 <- approx(seq(1:nrows), flightdata$N11C,xout=c(1:nrows), method="linear",n=nrows)$y
    n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
    n21 <- approx(seq(1:nrows), flightdata$N21C,xout=c(1:nrows), method="linear",n=nrows)$y
    n22 <- approx(seq(1:nrows), flightdata$N22C,xout=c(1:nrows), method="linear",n=nrows)$y
    ff1 <- approx(seq(1:nrows), flightdata$FF1C,xout=c(1:nrows), method="linear",n=nrows)$y
    ff2 <- approx(seq(1:nrows), flightdata$FF2C,xout=c(1:nrows), method="linear",n=nrows)$y
    egt1 <- approx(seq(1:nrows), flightdata$EGT1C,xout=c(1:nrows), method="linear",n=nrows)$y
    egt2 <- approx(seq(1:nrows), flightdata$EGT2C,xout=c(1:nrows), method="linear",n=nrows)$y
    flx1_temp <- approx(seq(1:nrows), flightdata$FLX1_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
    flx2_temp <- approx(seq(1:nrows), flightdata$FLX2_TEMP,xout=c(1:nrows), method="linear",n=nrows)$y
    at_flx <- approx(seq(1:nrows), flightdata$AT_FLX,xout=c(1:nrows), method="linear",n=nrows)$y
    q1 <- approx(seq(1:nrows), flightdata$Q1,xout=c(1:nrows), method="linear",n=nrows)$y
    q2 <- approx(seq(1:nrows), flightdata$Q2,xout=c(1:nrows), method="linear",n=nrows)$y
    pt1 <- approx(seq(1:nrows), flightdata$PT1,xout=c(1:nrows), method="linear",n=nrows)$y
    pt2 <- approx(seq(1:nrows), flightdata$PT2,xout=c(1:nrows), method="linear",n=nrows)$y
    flight_phase <- approx(seq(1:nrows), flightdata$FLIGHT_PHASE,xout=c(1:nrows), method="linear",n=nrows)$y

#apflare <- approx(seq(1:nrows), flightdata$APFLARE,xout=c(1:nrows), method="linear",n=nrows)$y

### Acrescentar
###    
# "T12_1"    "T12_2"  "T25_1"          "T25_2"          "T31" 
# "P0_1"           "P0_2"          
#  "P2_1"           "P2_2"           "P31"            "P32"            "PS13_1"        
# "PS13_2" 
#
  vrtg <- flightdata$VRTG

## Support calcs and figs

#table(flightdata$FM_FWC)
#plot(gs[t0:t1],type="l",col="blue")


if(take_off_study==1){
  to_plots()
}

if(landing_study==1){
  landing_plots()
}


#}

