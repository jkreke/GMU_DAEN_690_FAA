require(lubridate)
require(timeDate)

st=format(Sys.time(),"%H:%M:%OS0")
cat("starting holidays", st,"\n")

#Ndf <- read.csv(paste0(adir,"/laser_combined.csv"),stringsAsFactors = FALSE)
Ndf <- ndf

get_holidays <- function(Years=uYears, a.country="USA", FDay=FirstDay, LDay=LastDay){

  hlist_USA <- c(
    "USNewYearsDay",
    "USMLKingsBirthday",
    "USPresidentsDay",
    "USGoodFriday",
    "Easter",
    "USMemorialDay",
    "USIndependenceDay",
    "USLaborDay",
    "USColumbusDay",
    "USVeteransDay",
    "USThanksgivingDay",
    "ChristmasDay",
    "USElectionDay",
    "USInaugurationDay"
    
    #    "USCPulaskisBirthday",
    #    "USDecorationMemorialDay",
    #    "USLincolnsBirthday",
    #    "USWashingtonsBirthday",
    
  )
  
  
  hlist_UK <- c(
    "NewYearsDay",
    "GoodFriday",
    "Easter",
    "EasterMonday",
    "GBMayDay",
    "GBBankHoliday",
    "GBSummerBankHoliday",
    "GBMilleniumDay",
    "ChristmasDay",
    "BoxingDay"
  )
  
  
  if(country=="USA"){hlist=hlist_USA
  } else if(country=="UK"){hlist=hlist_UK
  } else {hlist=hlist_USA}
  
  
  
  holsdf <- holsdf1 <- data.frame(HolidayDate=as.Date("1901-01-01"), Holiday="None")
  ending="(as.numeric(as.character(Years)))"
  for (hday in hlist){
    
    func=paste0(hday,ending)
    #print(func)
    a <- eval(parse(text=func))
    #print(a)
    b <- data.frame(HolidayDate=as.Date(a), Holiday=hday)
    holsdf <- rbind(holsdf, b)
  }
  
  
  holdates <- holsdf[holsdf$HolidayDate>=FDay & holsdf$HolidayDate<=LDay,]
  holdates <- arrange(holdates, HolidayDate)
  #tdaydates <- holdates$HolidayDate[holdates$Holiday=="USThanksgivingDay"]
  #tdaydates <- as.Date(tdaydates + 1)
  #df <- data.frame(HolidayDate=tdaydates, Holiday="DayAfterThanksgiving")
  #if(nrow(df)>0){holdates <- rbind(holdates, df)}
  
  #todo  look at mondays if the holiday is tuesday
  # look at fridays if the holiday is monday
  # especially for JUly4, christmas, laborday
  #using wday from lubridate
  # 1=Sunday
  

  
  #print("check holidays")

  #for each monday, look for previous friday
  
  holdates$dow <- wday(holdates$HolidayDate)
  
  
  extend_holiday <- function(hdf, tdow, extdow, prefix){
    sdf <- subset(hdf, dow==tdow)
    if(nrow(sdf)==0){
      sdf=data.frame()
      } else {
      sdf$HolidayDate <- sdf$HolidayDate + extdow
      sdf$Holiday <- paste0(prefix, sdf$Holiday)
      }
    return(sdf)
  }
  
  #Friday before monday holiday
  bmondates <- extend_holiday(hdf=holdates, tdow=2, extdow=-3, prefix="FriBefore")
  
  #Tues after a monday holiday
  amondates <- extend_holiday(hdf=holdates, tdow=2, extdow=+1, prefix="TueAfter")

  #Monday before a tuesday holiday
  btuedates <- extend_holiday(hdf=holdates, tdow=3, extdow=-1, prefix="MonBefore")

  #Friday after a thursday holiday
  athudates <- extend_holiday(hdf=holdates, tdow=5, extdow=+1, prefix="FriAfter")

  #Monday after a friday holiday
  afridates <- extend_holiday(hdf=holdates, tdow=6, extdow=+3, prefix="MonAfter")
  
  #thursday before a friday holiday
  bfridates <- extend_holiday(hdf=holdates, tdow=6, extdow=-1, prefix="ThuBefore")

  
  holdates <- rbind(holdates, bmondates)
  holdates <- rbind(holdates, amondates)
  holdates <- rbind(holdates, btuedates)
  holdates <- rbind(holdates, athudates)
  holdates <- rbind(holdates, afridates)
  holdates <- rbind(holdates, bfridates)
  
  if(nrow(holdates)==0){holdates <- holsdf1}
  holdates$dow=NULL
  return(holdates)
  
}





get_holidays2 <- function(Years=uYears, country="USA", FDay=FirstDay, LDay=LastDay){
  
  hols <- as.Date("1900-01-01")  #establishes a date vectot that dates will be appended to.  This will be excluded later.
  
  if(country=="UK"){
    for (yr in Years){
      a <- holidayLONDON(as.numeric(as.character(yr)))
      a <- as.Date(a)
      
      hols <- c(hols, a)
    }
    
#  } else if (country=="USA"){
#    
#    for (yr in Years){
#      a <- holidayNYSE(as.numeric(as.character(yr)))
#      a <- as.Date(a)
#
#      hols <- c(hols, a)
#    }
    
  } else {
    
    #for (yr in Years){
    #  
    #  a <- holidayNYSE(as.numeric(as.character(yr)))
    #  a <- as.Date(a)
    #  
    #  hols <- c(hols, a)
    #}
    
    #eval(parse(text=ext.mdl(fit3)))
    
      holsdf <- data.frame()
      
      a1 <- as.Date(USNewYearsDay(as.numeric(as.character(Years))))
      a2 <- as.Date(USInaugurationDay(as.numeric(as.character(Years))))
      a3 <- as.Date(USMLKingsBirthday(as.numeric(as.character(Years))))
      #a4 <- as.Date(USLincolnsBirthday(as.numeric(as.character(Years))))
      #a5 <- as.Date(USWashingtonsBirthday(as.numeric(as.character(Years))))
      a6 <- as.Date(USMemorialDay(as.numeric(as.character(Years))))
      a7 <- as.Date(USIndependenceDay(as.numeric(as.character(Years))))
      a8 <- as.Date(USLaborDay(as.numeric(as.character(Years))))
      a9 <- as.Date(USElectionDay(as.numeric(as.character(Years))))
      a10 <- as.Date(USVeteransDay(as.numeric(as.character(Years))))
      a11 <- as.Date(USThanksgivingDay(as.numeric(as.character(Years))))
      a12 <- as.Date(USChristmasDay(as.numeric(as.character(Years))))
      a13 <- as.Date(USGoodFriday(as.numeric(as.character(Years))))
      a14 <- as.Date(USPresidentsDay(as.numeric(as.character(Years))))
      #hols <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
      a <- data.frame(Holiday.Date=a1, Holiday="USNewYearsDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a2, Holiday="USInaugurationDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a3, Holiday="USMLKingsBirthday")
      holsdf <- rbind(holsdf, a)
     # a <- data.frame(Holiday.Date=a4, Holiday="USLincolnsBirthday")
     # holsdf <- rbind(holsdf, a)
     # a <- data.frame(Holiday.Date=a5, Holiday="USWashingtonsBirthday")
     # holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a6, Holiday="USMemorialDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a7, Holiday="USIndependenceDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a8, Holiday="USLaborDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a9, Holiday="USElectionDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a10, Holiday="USVeteransDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a11, Holiday="USThanksgivingDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a12, Holiday="USChristmasDay")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a13, Holiday="USGoodFriday")
      holsdf <- rbind(holsdf, a)
      a <- data.frame(Holiday.Date=a14, Holiday="USPresidentsDay")
      holsdf <- rbind(holsdf, a)
    
    
    
  }
  
  holdates <- holsdf[holsdf$Holiday.Date>=FDay & holsdf$Holiday.Date<=LDay,]
  holdates <- arrange(holdates, Holiday.Date)
  
  return(holdates)
}

country="US"
#Ndf$Loc.Date <- as.Date(Ndf$DOS)

FirstDay=min(as.Date(Ndf$Loc.Date[!is.na(Ndf$Loc.Date)]))
LastDay=max(as.Date(Ndf$Loc.Date[!is.na(Ndf$Loc.Date)]))
uYears <- sort(unique(year(Ndf$Loc.Date)))
alldates <- seq(FirstDay, LastDay, by="day")

if(!exists("country")){country="unknown"}
Holidays <- get_holidays(Years=uYears, a.country=country, FDay=FirstDay, LDay=LastDay)

Ndf$Holidays <- "None"
for(i in 1:nrow(Ndf)){
  h=which(Holidays$HolidayDate==Ndf$Loc.Date[i])
  if(length(h)>0){Ndf$Holidays[i] <- Holidays$Holiday[h]}
}

#write.csv(Ndf, paste0(adir,"/laser_combined.csv"), row.names=FALSE)

ndf <- Ndf

et=format(Sys.time(),"%H:%M:%OS0")
cat("ending holidays", st,"\n")

