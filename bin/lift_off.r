# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

library(ggplot2)
library(brew)
library(tools)
library(data.table)


setwd('C:/Users/210906/Dropbox/EASA/lift_off_v1/flights')

#Filelist to read
file <- dir()
fileList <- list.files(path="C:/Users/210906/Dropbox/EASA/lift_off_v1/flights", pattern=".csv")

#Variable Initialization
##$##
#pitch_store <- mat.or.vec(NROW(fileList),1)
#trajectory=mat.or.vec(7850,NROW(fileList)*2)
#distance_TH_min=mat.or.vec(1,2)
#FMS_TD=mat.or.vec(NROW(fileList),2)


for (s in 1:NROW(fileList)) {

  ## Data Input
  setwd('C:/Users/210906/Dropbox/EASA/lift_off_v1/flights')
  ## LOAD Flight From List
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', RUNWAY_TO='character',
                RUNWAY_LD='character', DESTINATION='character', DATE='Date')) 
  #flightdata <- read.table(fileList[s],sep=",",header=T) 

  nrows <- nrow(flightdata)
  
  
  ## Flight Data Interpolation
  LG_left <- approx(seq(1:nrows), flightdata$LDG_ON_1,xout=c(1:nrows), method="linear",n=nrows)$y
  LG_nose <- approx(seq(1:nrows), flightdata$LDGN_ON,xout=c(1:nrows), method="linear",n=nrows)$y
  LG_right <- approx(seq(1:nrows), flightdata$LDG_ON_2,xout=c(1:nrows), method="linear",n=nrows)$y
  pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
  pitch_capt <- approx(seq(1:nrows), flightdata$PITCH_CPT,xout=c(1:nrows), method="linear",n=nrows)$y
  pitch_fo <- approx(seq(1:nrows), flightdata$PITCH_FO,xout=c(1:nrows), method="linear",n=nrows)$y
  pitch_rate <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
  ralt1 <- approx(seq(1:nrows), flightdata$RALTD1,xout=c(1:nrows), method="linear",n=nrows)$y
  ralt2 <- approx(seq(1:nrows), flightdata$RALTD2,xout=c(1:nrows), method="linear",n=nrows)$y
  ivv <- approx(seq(1:nrows), flightdata$IVV,xout=c(1:nrows), method="linear",n=nrows)$y
  long <- approx(seq(1:nrows), flightdata$LONG,xout=c(1:nrows), method="linear",n=nrows)$y
  latg <- approx(seq(1:nrows), flightdata$LATG,xout=c(1:nrows), method="linear",n=nrows)$y
  n11 <- approx(seq(1:nrows), flightdata$N11C,xout=c(1:nrows), method="linear",n=nrows)$y
  n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
  roll <- approx(seq(1:nrows), flightdata$ROLL,xout=c(1:nrows), method="linear",n=nrows)$y
  ias <- approx(seq(1:nrows), flightdata$IASC,xout=c(1:nrows), method="linear",n=nrows)$y
  gs <- approx(seq(1:nrows), flightdata$GSC,xout=c(1:nrows), method="linear",n=nrows)$y
    
  vrtg <- flightdata$VRTG
  pitch_acceleration <- diff(pitch_rate)

  
  # FIRST METHOD
  # the **lo_mlg** is the main reference for lift-off
  # It is considered as the instant when Left Gear indicates Air
  # starting from this point other fine-tunning determinations
  # are established
  #
  
  tlg <- min(which(LG_left==0))
  trg <- min(which(LG_right==0))
  
  lo_mlg <- max(tlg,trg) # Lift Off according to MLG criteria
  
  #center_point <- min(which(LG_left==0))
  
  
  # definitions for graphics
  #
  
  min_plt <- lo_mlg - 40
  max_plt <- lo_mlg + 10
  
  # Considering the Landing Gear Criteria for lift-off
  pitch_at_lift_off <- pitch[lo_mlg]
  pitch_rate_lift_off <- pitch_rate[lo_mlg]
  pitch_capt_lift_off <- pitch_capt[lo_mlg]
  pitch_fo_lift_off <- pitch_fo[lo_mlg]
  ralt1_at_lift_off <- ralt1[lo_mlg]
  ralt2_at_lift_off <- ralt2[lo_mlg]
  vrtg_at_lift_off <-  vrtg[lo_mlg]
  long_at_lift_off <-  long[lo_mlg]
  ivv_at_lift_off <-  ivv[lo_mlg]
  
  pitch_accel_lift_off <- pitch_acceleration[lo_mlg]
  
  lift_off <- lo_mlg
  lo_ralt <- lo_mlg
  method = 1
  
  
  # SECOND METHOD fine tunning
  #  using Radio Altitude If lo_mlg is not a good determination
  # fine tune is performed if RALT[lo_mlg]>1ft
  
  r1 <- ralt1[lo_mlg]
  r2 <- ralt2[lo_mlg]
  
  if ( r1 > 1  || r2 > 1) {
    
    min <- lo_mlg - 20
    max <- lo_mlg + 10
    
    r1_positive <- min(which(ralt1[min:max]>0))
    lo_r1 <- min + r1_positive
    
    r2_positive <- min(which(ralt2[min:max]>0))
    lo_r2 <- min + r2_positive
    
    #lo_ralt <- min(lo_r1,lo_r2)
    lo_ralt <- max(lo_r1,lo_r2)
    
    # Parameter values for Lift-Off Corrected
    
    pitch_at_lift_off <- pitch[lo_ralt]
    pitch_rate_lift_off <- pitch_rate[lo_ralt]
    pitch_capt_lift_off <- pitch_capt[lo_ralt]
    pitch_fo_lift_off <- pitch_fo[lo_ralt]
    ralt1_at_lift_off <- ralt1[lo_ralt]
    ralt2_at_lift_off <- ralt2[lo_ralt]
    vrtg_at_lift_off <-  vrtg[lo_ralt]
    long_at_lift_off <-  long[lo_ralt]
    ivv_at_lift_off <-  ivv[lo_ralt]
    
    pitch_accel_lift_off <- pitch_acceleration[lo_ralt]
    
    
    lift_off <- lo_ralt
    method = 2
    
  } 
  
  #pitch_store[s] <- pitch[lift_off]
  
  ### PLOTING
  ###
  setwd("C:/Users/210906/Dropbox/EASA/lift_off_v1/figures")
  
  png("landing_gear.png")
  
  ## Landing Gears
  plot(LG_left[min_plt:max_plt], type='l', col="brown", 
       ylab="Landing Gear", main="Landing Gear L-N-R", lwd=1.5)
  points(LG_nose[min_plt:max_plt], type='l', col="orange", lwd=1.5)
  points(LG_right[min_plt:max_plt], type='l', col="green", lwd=1.5)
  grid(,,, col="grey")
  #abline(v=(lift_off-min_plt), col="blue")
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  
  legend("topright", inset=.05, title="LGear",
         c("Left LG","Nose LG","Right LG"), fill=c("brown","orange","green"), cex=0.8)
  
  dev.off()
  
  
  ## Roll
  
  png("roll.png")
  
  plot(roll[min_plt:max_plt], type='l', 
       col="brown", ylab="roll",main="Roll Angle", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  
  ## N11
  
  png("n11.png")
  
  plot(n11[min_plt:max_plt], type='l', 
       col="brown", ylab="N11",main="N1 Eng1", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  
  ## Pitch
  
  png("pitch.png")
  
  plot(pitch[min_plt:max_plt], type='l', col="brown", ylab="pitch",main="Pitch Angle", lwd=1.5)
  #abline(v=v_line, col="red")
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
    
  ## pitch command
  
  png("pitch_command.png")
  
  plot(pitch_capt[min_plt:max_plt], type='l', col="brown", 
       ylab="pitch command", main="Pitch Command", ylim=c(-8,1),lwd=1.5)
  points(pitch_fo[min_plt:max_plt], type='l', col="green",lwd=1.5)
  grid(,,, col="grey")
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  legend("left", inset=.05, c("CAPT","FO"), fill=c("brown","green"), cex=0.7)
  
  dev.off()
  
  t_theta_fo_cmd <- min(which(pitch_fo[min_plt:max_plt] != 0 ))
  
  ## pitch rate
  png("pitch_rate.png")
  
  plot(pitch_rate[min_plt:max_plt], type='l', col="brown", 
       ylab="PTCR",main="Pitch Rate", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  
  ## Pitch acceleration
  
  png("pitch_acceleration.png")
  
  plot(pitch_acceleration[min_plt:max_plt], type='l', col="brown", 
       ylab="d(PTCR)/dt",main="Pitch Acceleration", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  ## radio altitude
  
  png("radio_altitude.png")
  
  plot(ralt1[min_plt:max_plt], type='l', col="brown", 
       ylab="RALT", main="Radio Altitude", lwd=1.5)
  points(ralt2[min_plt:max_plt], type='l', col="green", lwd=1.5)
  grid(,,, col="grey")
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  legend("left", inset=.05, c("#1 - CAPT","#2 - FO"), fill=c("brown","green"), cex=0.7)
  
  dev.off()
  
  
  png("vrtg.png")
  
  plot(vrtg[min_plt:max_plt], type='l', col="brown", ylab="VRTG",
       main="Vertical Acceleration", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  
  png("ivv.png")
  
  plot(ivv[min_plt:max_plt], type='l', col="brown", ylab="IVV",
       main="Inertial Vertical Velocity", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  
  png("long.png")
  
  plot(long[min_plt:max_plt], type='l', col="brown", ylab="LONG",
       main="Longitudinal Acceleration", lwd=1.5)
  abline(v=(lo_mlg-min_plt), col="blue")
  abline(v=(lo_ralt-min_plt), col="red")
  grid(,,, col="grey")
  
  dev.off()
  
  # PDF Reporting
  setwd("C:/Users/210906/Dropbox/EASA/lift_off_v1")
  brew("lift_off.brew", paste0('lift_off',s,".tex"))
  texi2dvi(paste0('lift_off',s,".tex"), pdf = TRUE)
    
}


