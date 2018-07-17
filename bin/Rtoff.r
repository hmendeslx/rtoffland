######################################################################
# Autoland Machine Learning Classification
# - New version based on Lishuai thesis
# - starting from autoland_neo.r
# 
#

# Clean workspace (& environment data)
rm(list=ls(all=TRUE))

timech1_0 <- proc.time()

library(ggplot2)
library(brew)
library(tools)
library(data.table)
library(psych)
library(lubridate)
library(xtable)
library(data.table)
library(psych)

#################################################
## Function Definition
##

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

## acrescentar Gaussian smooth


#################################################
## Constants
c_knot_ms <- 1852/3600


## paths

#flightpath <- "C:/FlightDB/cs_tnp"    ## Insert case into the respective folder
flightpath <- "C:/FlightDB/autoland"    ## Insert case into the respective folder
#flightpath <- "O:/autoland"
binpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/autoland/bin"
resultpath <- "C:/Users/210906/Dropbox/EASA/flightRtools/autoland/results"
figurepath <- "C:/Users/210906/Dropbox/EASA/flightRtools/autoland/results/figures"


# file list
setwd(flightpath)
fileList <- list.files(path=flightpath, pattern="*.csv")

# Variable and Vector Initialization
#
#flight_measurements <- mat.or.vec(NROW(fileList),20) # Matrix of dimensions: [fligths x Measurements]
flight_measurements <- data.frame()
mean_phase_store <- mat.or.vec(NROW(fileList),1)


total_flights <- NROW(fileList)

print(paste("number of flights:", total_flights))



for (s in 1:total_flights) {
  
  ## LOAD Flight From List
  setwd(flightpath)
  flightdata <- fread(fileList[s],sep=",",header=T, stringsAsFactors = F, verbose=FALSE,
                      colClasses = c(ACT='character', AC_TYPE='character', ORIGIN='character', 
                      RUNWAY_TO='character', RUNWAY_LD='character', DESTINATION='character', 
                      DATE='Date')) 
  
  ## Check CSV File Integrity
  # - 1st 100 samples of flight phase has be average lower than 2 (criteria #1)
  #
  phase <- flightdata$FLIGHT_PHASE[which(!is.na(flightdata$FLIGHT_PHASE))]
  mean_phase_store[s] <- mean(phase[1:100])
  
  nrows <- NROW(flightdata)
  
  if (mean_phase_store[s]<2){  # File Integrity
    # Produce the expected result....
    
    setwd(resultpath)
    write(paste(fileList[s]), file="FLIGHTS_OK.csv", append=TRUE)
    print(paste("s = ", s))
    print(paste(fileList[s], "- OK"))
    
    setwd(flightpath)
        
    
## Interpolations
    pitch <- approx(seq(1:nrows), flightdata$PITCH,xout=c(1:nrows), method="linear",n=nrows)$y
ff1 <- approx(seq(1:nrows), flightdata$FF1,xout=c(1:nrows), method="linear",n=nrows)$y
    alt_std <- approx(seq(1:nrows), flightdata$ALT_STD,xout=c(1:nrows), method="linear",n=nrows)$y
    ptcr <- approx(seq(1:nrows), flightdata$PTCR,xout=c(1:nrows), method="linear",n=nrows)$y
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
n21 <- approx(seq(1:nrows), flightdata$N21C,xout=c(1:nrows), method="linear",n=nrows)$y
    n12 <- approx(seq(1:nrows), flightdata$N12C,xout=c(1:nrows), method="linear",n=nrows)$y
    apflare <- approx(seq(1:nrows), flightdata$APFLARE,xout=c(1:nrows), method="linear",n=nrows)$y
    gsdev1_mddm <- approx(seq(1:nrows), flightdata$GSDEV1_MDDM,xout=c(1:nrows), method="linear",n=nrows)$y
    loc1_mddm <- approx(seq(1:nrows), flightdata$LOC1_MDDM,xout=c(1:nrows), method="linear",n=nrows)$y
    
    vrtg <- flightdata$VRTG
    

## Calcs
  ## cleaning NANs from TOUCHDOWNC
  flightdata$TOUCH_DOWNC[which(is.na(flightdata$TOUCH_DOWNC))] <- 0
  td_instant=min(which(flightdata$TOUCH_DOWNC != 0))

  td_instant <- td_instant -2  # 2/8 sec correction

  delta_ti <- 700 # ~1'30'' before TD corresponds to alt_std ~ 1000ft
  delta_tf <- 150 # ~ 20'' after TD  corresponds to gs~80knot
  init_t <- td_instant-delta_ti
  final_t <- td_instant+delta_tf

  # Flare calculation (physical flare!)
  pitch_rate_gs <- gaussian_smoothing(ptcr[init_t:final_t],4)
  pitch_rate_dot <-derivative(pitch_rate_gs,NROW(pitch_rate_gs))
  max_prdot <- max(pitch_rate_dot[600:delta_ti])
  t_flare <- 600 + which(pitch_rate_dot[600:delta_ti]==max_prdot)

  delta_t_flare <- (delta_ti - t_flare)/8
  t_flare_c <- init_t + t_flare # corrected to time-scale T
  raltd1_at_fl <- raltd1[t_flare_c]
  raltd2_at_fl <- raltd2[t_flare_c]
  gs_at_fl <- gs[t_flare_c]


  # de-rotation time
  t_ldg_lft <- init_t + min(which(flightdata$LDG_ON_1[init_t:final_t]==1))
  t_ldg_rgt <- init_t + min(which(flightdata$LDG_ON_2[init_t:final_t]==1))
  t_mlg <- min(t_ldg_lft, t_ldg_rgt)

  t_ldg_nos <- init_t + min(which(flightdata$LDGN_ON[init_t:final_t]==1))
  delta_t_ldg <- (t_ldg_nos - t_mlg)/8

  # masurements during descent : init_t -> final_t
  max_vrtg <- max(vrtg[init_t:final_t])
  min_ptcr <- min(ptcr[init_t:final_t])
  min_long <- min(long[init_t:final_t])

  max_latg <- max(latg[init_t:final_t])
  min_latg <- min(latg[init_t:final_t])
  latg_ampl <- max_latg - min_latg

  max_loc1_mddm <- max(loc1_mddm[init_t:final_t])
  min_loc1_mddm <- min(loc1_mddm[init_t:final_t])
  loc1_mddm_a <- max_latg - min_latg

  # measurements between "init_t" and "td_instant" (or before!)
  before_td <- 40
  max_gsdev1_mddm <- max(gsdev1_mddm[init_t:(td_instant-before_td)]) #3.75sec before TD (Flare!)
  min_gsdev1_mddm <- min(gsdev1_mddm[init_t:(td_instant-before_td)])
  gsdev1_mddm_a <- max_gsdev1_mddm - min_gsdev1_mddm


  # measurement from "td_intant" on.... : td_instant -> final_t
  max_hdg <- max(head_mag[td_instant:final_t])
  min_hdg <- min(head_mag[td_instant:final_t])
  hdg_ampl <- max_hdg - min_hdg


  # measurements at TD point
  gs_at_td <- gs[td_instant]
  pitch_at_td <- pitch[td_instant]
  ivv_at_td <- ivv[td_instant]
  ptcr_at_td <- ptcr[td_instant]
  e_at_td <- gs[td_instant]*c_knot_ms*gw1kg[td_instant]/2

  # measurements at init_t point


  # measurements at final_t point

  
## Create Flight Measurements Vector
  flight_measurements[s,1] <- substr(fileList[s],1,6) # A/C TAIL
  flight_measurements[s,2] <- substr(fileList[s],17,22) # AGS file number
  flight_measurements[s,3] <- flightdata$AC_TYPE[which(flightdata$AC_TYPE!="")][100] # A/C TYPE
  flight_measurements[s,4] <- flightdata$FLTNUM[which(flightdata$FLTNUM!="NA")][100]
  flight_measurements[s,5] <- flightdata$ORIGIN[which(flightdata$ORIGIN!="")][100]
  flight_measurements[s,6] <- flightdata$RUNWAY_TO[which(flightdata$RUNWAY_TO!="")][100]
  flight_measurements[s,7] <- flightdata$DESTINATION[which(flightdata$DESTINATION!="")][100]
  flight_measurements[s,8] <- flightdata$RUNWAY_LD[which(flightdata$RUNWAY_LD!="")][100]
  flight_measurements[s,9] <- flightdata$DATE[which(flightdata$DATE!="")][100]
  #
  flight_measurements[s,10] <- delta_t_ldg
  flight_measurements[s,11] <- max_vrtg
  flight_measurements[s,12] <- min_ptcr
  flight_measurements[s,13] <- min_long
  flight_measurements[s,14] <- latg_ampl
  flight_measurements[s,15] <- hdg_ampl
  flight_measurements[s,16] <- gs_at_td
  flight_measurements[s,17] <- pitch_at_td
  flight_measurements[s,18] <- ivv_at_td
  flight_measurements[s,19] <- ptcr_at_td
  flight_measurements[s,20] <- e_at_td
  flight_measurements[s,21] <- loc1_mddm_a
  flight_measurements[s,22] <- gsdev1_mddm_a
  flight_measurements[s,23] <- delta_t_flare
  flight_measurements[s,24] <- raltd1_at_fl
  flight_measurements[s,25] <- raltd2_at_fl
  flight_measurements[s,26] <- gs_at_fl

##################################################
## TEST The Following FLIGHT - change number below
file_no = "931964"

## Flight Under Test??
    if (flight_measurements[s,2]==file_no){
      
      file_name <- fileList[s]
      aircraft <- flight_measurements[s,1]
      flight_num <- flight_measurements[s,4]
      
      origin <- flight_measurements[s,5]
      destination <- flight_measurements[s,7]
      rwdestination <- flight_measurements[s,8]
      data <- flight_measurements[s,9]
      
      # Flare
      delta_t_flare_fix <- flight_measurements[s,23]
      raltd1_flare_fix <- flight_measurements[s,24]
      raltd2_flare_fix <- flight_measurements[s,25]
      gs_flare_fix <- flight_measurements[s,26]
      
      # During Descent
      delta_t_ldg_fix <- flight_measurements[s,10]
      max_vrtg_fix <- flight_measurements[s,11]
      min_ptcr_fix <- flight_measurements[s,12]
      min_long_fix <- flight_measurements[s,13]
      latg_ampl_fix <- flight_measurements[s,14]
      hdg_ampl_fix <- flight_measurements[s,15]
      loc1_mddm_a_fix <- flight_measurements[s,21]
      gsdev1_mddm_a_fix <- flight_measurements[s,22]
      
      # at TD
      gs_at_td_fix <- flight_measurements[s,16]
      pitch_at_td_fix <- flight_measurements[s,17]
      ivv_at_td_fix <- flight_measurements[s,18]
      ptcr_at_td_fix <- flight_measurements[s,19]
      e_at_td_fix  <- flight_measurements[s,20]
      
      
      ## Time series Plots (one flight)
      setwd(figurepath)  
      
      png("pitch_ts.png")
      plot(pitch[init_t:final_t],type="l", col="blue", main="pitch", lwd=1.5)
      abline(v=delta_ti,col="green",lwd=2)
      abline(v=t_flare,col="red")
      grid(,,col="dark red")
      dev.off()
      
      png("vrtg_ts.png")
      plot(vrtg[init_t:final_t],type="l", col="blue",main="vrtg", lwd=1.5)
      abline(v=delta_ti,col="green",lwd=2)
      grid(,,col="dark red")
      dev.off()
      
      png("gsdev1_mddm_ts.png")
      plot(gsdev1_mddm[init_t:final_t],type="l", col="blue",main="gsdev1_mddm", lwd=1.5)
      abline(v=delta_ti,col="green",lwd=2)
      abline(v=t_flare,col="red")
      grid(,,col="dark red")
      dev.off()
      
       png("loc1_mddm_ts.png")
       plot(loc1_mddm[init_t:final_t],type="l", col="blue",main="loc1_mddm", lwd=1.5)
       abline(v=delta_ti,col="green",lwd=2)
       grid(,,col="dark red")
       dev.off()

       png("long_ts.png")
       plot(long[init_t:final_t],type="l", col="blue",main="long", lwd=1.5)
       abline(v=delta_ti,col="green",lwd=2)
       grid(,,col="dark red")
       dev.off()
       
       png("latg_ts.png")
       plot(latg[init_t:final_t],type="l", col="blue",main="latg", lwd=1.5)
       abline(v=delta_ti,col="green",lwd=2)
       grid(,,col="dark red")
       dev.off()
       
       png("head_mag_ts.png")
       plot(head_mag[init_t:final_t],type="l", col="blue",main="head_mag", lwd=1.5)
       abline(v=delta_ti,col="green",lwd=2)
       grid(,,col="dark red")
       dev.off()
       
       png("gs_ts.png")
       plot(gs[init_t:final_t],type="l", col="blue",main="gs", lwd=1.5)
       abline(v=delta_ti,col="green",lwd=2)
       grid(,,col="dark red")
       dev.off()
       
       png("ptcr_ts.png")
       plot(ptcr[init_t:final_t],type="l", col="blue",main="ptcr", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       abline(v=t_flare,col="red")
       grid(,,col="dark red")
       dev.off()
       
       png("raltd1_ts.png")
       plot(raltd1[init_t:final_t],type="l", col="blue" ,main="raltd1", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       abline(v=t_flare,col="red")
       grid(,,col="dark red")
       dev.off()
       
       png("raltd2_ts.png")
       plot(raltd2[init_t:final_t],type="l", col="blue" ,main="raltd2", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       abline(v=t_flare,col="red")
       grid(,,col="dark red")
       dev.off()
      
       png("lg_left_ts.png")  
       plot(LG_left[init_t:final_t],type="l", col="blue", main="LG Left", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       grid(,,col="dark red")
       dev.off()
      
       png("lg_right_ts.png")  
       plot(LG_right[init_t:final_t],type="l", col="blue", main="LG right", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       grid(,,col="dark red")
       dev.off()
      
       png("lg_nose_ts.png")
       plot(LG_nose[init_t:final_t],type="l", col="blue", main="LG nose", lwd=1.5)
       abline(v=delta_ti,col="green", lwd=2)
       grid(,,col="dark red")
       dev.off()
      
    }

  } else {
    # Log unexpected flights... and go on to the next flight
    setwd(resultpath)
    write(paste(fileList[s]), file="FLIGHTS_NOT_OK.csv", append=TRUE)
    print(paste(fileList[s], "- NOT OK"))
    
    setwd(flightpath)
    
#     # Store Flight Measurements 
#     pitch_liftoff_store <- NA
#     #pitch_liftoff_store[s] <- NA
#     ptcr_liftoff_store <- NA
#     ptacc_liftoff_store <- NA
#     vrtg_liftoff_store <- NA
#     long_liftoff_store <- NA
#     latg_liftoff_store <- NA
#     ralt1_liftoff_store <- NA
#     ralt2_liftoff_store <- NA
#     ias_liftoff_store <- NA
#     gs_liftoff_store <- NA
#     roll_liftoff_store <- NA
#     liftoff_method <- NA
    
  }
  
}  
  

names(flight_measurements) <- c("ac.tail", "ags.file","ac.type", "fltnum", "origin", 
                                "runway.to","destination", "runway.ld", "date.to",
                                "delta_t_ldg", "max_vrtg","min_ptcr", "min_long",
                                "latg_ampl","hdg_ampl","gs_at_td","pitch_at_td",
                                "ivv_at_td","ptcr_at_td","e_at_td","loc1_mddm_a",
                                "gsdev1_mddm_a", "delta_t_flare", "raltd1_flare", 
                                "raltd2_flare", "gs_flare")

# normalise energy (units too big!!)
metd <- mean(flight_measurements[,20])
sdetd <-sd(flight_measurements[,20])
flight_measurements[,20] <- scale(flight_measurements[,20])

tst <- print(xtable(describe(flight_measurements[,10:NCOL(flight_measurements)])), 
             floating=TRUE ,floating.environment = "sidewaystable")

## Histogram plots (all flights)
  setwd(figurepath)
  
  png("delta_t.png")
  hist(flight_measurements$delta_t_ldg,col="blue",20, main="De-rotation Time", xlab="sec")
  abline(v=delta_t_ldg_fix,col="red", lwd=3)
  dev.off()

  png("vrtg.png")
  hist(flight_measurements$max_vrtg,col="blue",20, main="Max VRTG during Descent", xlab="g")
  abline(v=max_vrtg_fix,col="red", lwd=3)
  dev.off()

  png("min_ptcr.png")
  hist(flight_measurements$min_ptcr,col="blue",20, main="min pitch rate during Descent", xlab="deg/sec")
  abline(v=min_ptcr_fix,col="red", lwd=3)
  dev.off()

  png("min_long.png")
  hist(flight_measurements$min_long,col="blue",20, main="min LONG during Descent", xlab="g")
  abline(v=min_long_fix,col="red", lwd=3)
  dev.off()

  png("latg_ampl.png")
  hist(flight_measurements$latg_ampl,col="blue",20, main="LATG amplitude during Descent", xlab="g")
  abline(v=latg_ampl_fix,col="red", lwd=3)
  dev.off()

  png("loc1_mddm_a.png")
  hist(flight_measurements$loc1_mddm_a,col="blue",20, main="LOC1 amplitude during Descent", xlab="deg")
  abline(v=loc1_mddm_a_fix,col="red", lwd=3)
  dev.off()

  png("gsdev1_mddm_a.png")
  hist(flight_measurements$gsdev1_mddm_a,col="blue",20, main="GS1 amplitude during Descent", xlab="deg")
  abline(v=gsdev1_mddm_a_fix,col="red", lwd=3)
  dev.off()

  png("hdg_ampl.png")
  hist(flight_measurements$hdg_ampl,col="blue",20, main="HDG amplitude during roll-out", xlab="deg")
  abline(v=hdg_ampl_fix,col="red", lwd=3)
  dev.off()

  png("gs_at_td.png")
  hist(flight_measurements$gs_at_td,col="blue",20, main="GS @ TD", xlab="knot")
  abline(v=gs_at_td_fix,col="red", lwd=3)
  dev.off()

  png("pitch_at_td.png")
  hist(flight_measurements$pitch_at_td,col="blue",20, main="PITCH @ TD", xlab="deg")
  abline(v=pitch_at_td_fix,col="red", lwd=3)
  dev.off()

  png("ivv_at_td.png")
  hist(flight_measurements$ivv_at_td,col="blue",20, main="IVV @ TD", xlab="ft/min")
  abline(v=ivv_at_td_fix,col="red", lwd=3)
  dev.off()

  png("ptcr_at_td.png")
  hist(flight_measurements$ptcr_at_td,col="blue",20, main="pitch rate @ TD", xlab="deg/sec")
  abline(v=ptcr_at_td_fix,col="red", lwd=3)
  dev.off()

  png("e_at_td.png")
  hist(flight_measurements$e_at_td,col="blue",20, main="Energy @ TD", xlab="J (Normalised Value)")
  abline(v=((e_at_td_fix-metd)/sdetd),col="red", lwd=3)
  dev.off()

  png("delta_t_flare.png")
  hist(flight_measurements$delta_t_flare,col="blue",20, main="Time between Flare and TD", xlab="sec")
  abline(v=delta_t_flare_fix,col="red", lwd=3)
  dev.off()

  png("raltd1_flare.png")
  hist(flight_measurements$raltd1_flare,col="blue",20, main="RALTD1 @ Flare", xlab="sec")
  abline(v=raltd1_flare_fix,col="red", lwd=3)
  dev.off()

  png("raltd2_flare.png")
  hist(flight_measurements$raltd2_flare,col="blue",20, main="RALTD2 @ Flare", xlab="sec")
  abline(v=raltd2_flare_fix,col="red", lwd=3)
  dev.off()

  png("gs_flare.png")
  hist(flight_measurements$gs_flare,col="blue",20, main="GS @ Flare", xlab="sec")
  abline(v=gs_flare_fix,col="red", lwd=3)
  dev.off()

# PDF Reporting
#setwd(binpath)
setwd(resultpath)
#brew("Rautorep.brew", paste0('Rautorep',s,".tex"))
brew("Rautorep.brew", paste0('Rautorep',".tex"))
texi2dvi(paste0('Rautorep',".tex"), pdf = TRUE)



timech1 <- proc.time() - timech1_0
tsec <- timech1[3]
tmin <- timech1[3]/60


cat("-------------------------------------------------", "
    Processing Time [seconds]", "    ", tsec," 
    Processing Time [minutes]", "    ", tmin," 
    -------------------------------------------------")


# flight_measurements[which(flight_measurements$max_vrtg>1.5),1:11]
