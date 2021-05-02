#define libraries
library(lubridate)


st=format(Sys.time(),"%H:%M:%OS0")
cat("starting TZ converter", st, "\n")


#then convert UTC to local time

UTC.Date <- as.POSIXlt(ndf$date, tz="UTC", format="%Y-%m-%dT%H:%M:%OSZ")


if(F){
  
  ndf$minute <- ndf$hour <- ndf$day <- ndf$wday <- ndf$week <- NA
  ndf$month <- ndf$year <- ndf$DOS <- ndf$Loc.Date <- NA  
  
for(i in 1:nrow(ndf)){
  if(!is.na(ndf$tz[i])){
    LD            <- with_tz(UTC.Date[i], tz=ndf$tz[i])
    
    ndf$minute[i] <- minute(LD)
    ndf$hour[i]   <- hour(LD)
    ndf$day[i]    <- day(LD)
    ndf$wday[i]   <- wday(LD)
    ndf$week[i]   <- week(LD)
    ndf$month[i]  <- month(LD)
    ndf$year[i]   <- year(LD)
    ndf$DOS[i]    <- as.character(LD)
    ndf$Loc.Date[i] <- as.character(date(LD))
    
  }
}
}

if(T){

LD <- with_tz(UTC.Date, tz=ndf$tz)
ndf$minute <- minute(LD)
ndf$hour   <- hour(LD)
ndf$day    <- day(LD)
ndf$wday   <- wday(LD)
ndf$week   <- week(LD)
ndf$month  <- month(LD)
ndf$year   <- year(LD)
ndf$DOS    <- as.character(LD)
ndf$Loc.Date <- as.character(date(LD))

narows <- which(is.na(ndf$tz), arr.ind=TRUE)
ndf$minute[narows] <- NA
ndf$hour[narows]   <- NA
ndf$day[narows]    <- NA
ndf$wday[narows]   <- NA
ndf$week[narows]   <- NA
ndf$month[narows]  <- NA
ndf$year[narows]   <- NA
ndf$DOS[narows]    <- NA
ndf$Loc.Date[narows] <- NA

}


et=format(Sys.time(),"%H:%M:%OS0")
cat("ending TZ converter", et,"\n")


