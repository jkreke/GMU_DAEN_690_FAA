#tdf <- ndf[ndf$vectA.dis!="No Number",]
#tdf <- tdf[!is.na(tdf$vectA.dis),]
#tdf$vectA.dis <- as.numeric(tdf$vectA.dis)
#tdf <- tdf[tdf$vectA.dis>50, ]

save.graph=FALSE

setwd(adir)
if(T){
load("ndf_04-12.rdata", verbose=T)


partnership = "SAIC / GMU / FAA \nPartnership"
cit.sta <- with(ndf, data.frame(cit, sta, pop))

a <- unique(cit.sta)
cit.sta <- a[a$pop>500000,]
cit.sta <- subset(cit.sta, !is.na(cit.sta$cit))
row.names(cit.sta) <- NULL
cit.sta <- arrange(cit.sta, sta, cit)

cit.sta$idx =1:nrow(cit.sta)
cit.sta <- with(cit.sta, data.frame(idx, cit, sta, pop))
}
#tcit="ATLANTIC CITY"  ; tsta="NJ"
tcit="MIAMI"          ; tsta="FL"
#tcit="SAN FRANCISCO"  ; tsta="CA"
#tcit="HOUSTON"        ; tsta="TX"
#tcit="SAN JOSE"       ; tsta="CA"
#tcit="PORTLAND"       ; tsta="OR"
#tcit="OAKLAND"        ; tsta="CA"
#tcit="PHOENIX"        ; tsta="AZ"
#tcit="SAN DIEGO"      ; tsta="CA"
#tcit="CHICAGO"        ; tsta="IL"
#tcit="PITTSBURGH"     ; tsta="PA"
#tcit="DALLAS"         ; tsta="TX"
#tcit="LOS ANGELES"    ; tsta="CA"
#tcit="LOS VEGAS"      ; tsta="NV"
#tcit="SACRAMENTO"     ; tsta="CA"
#tcit="TUCSON"         ; tsta="AZ"
#tcit="DAGGET"         ; tsta="CA"
#tcit="ANCHORAGE"      ; tsta="AK"

#print(cit.sta)
#a <- readline("Select the idx for the desired city and state: ")
#i=as.numeric(a)
#print(paste("i=",i))
#tcit=cit.sta$cit[cit.sta$idx==i]; 
#tsta=cit.sta$sta[cit.sta$idx==i]

p.cities =1#c(3,8,11,12,13,21,54,69)
print(paste(tcit, tsta))
for(s in p.cities){
  
  #tcit=cit.sta$cit[cit.sta$idx==s]; 
  #tsta=cit.sta$sta[cit.sta$idx==s]
  
  cat("working on", tcit, tsta,"\n")
  
kdf <- with(ndf, data.frame(DOS, Loc.Date, 
                            Holidays, lon, lat, lon.Acor, lat.Acor, 
                            month, wday, hour, year, sta, cit,col=tolower(laser_color.x)))
kdf <- subset(kdf, sta==tsta & cit==tcit)
kdf <- kdf[!is.na(kdf$lon),]
if(nrow(kdf)==0){print("next");next}

kdf$col <- str_trim(kdf$col,"both")


lon.ave <- mean(kdf$lon.Acor,na.rm=T)
lat.ave <- mean(kdf$lat.Acor,na.rm=T)
lon.sd  <- sd(kdf$lon.Acor,na.rm=T)
lat.sd  <- sd(kdf$lat.Acor,na.rm=T)
k=4
lon.min <- lon.ave-k*lon.sd
lon.max <- lon.ave+k*lon.sd
lat.min <- lat.ave-k*lat.sd
lat.max <- lat.ave+k*lat.sd


#just for MIA.PNG
#lat.min <- 25.7833
#lat.max <- 25.8222
#lon.min <- -80.31667
#lon.max <- -80.26667


kdf <- subset(kdf, lon.Acor<lon.max & 
                lon.Acor>lon.min & 
                lat.Acor<lat.max & 
                lat.Acor>lat.min )
r.lon=runif(nrow(kdf),0,1e-5)
r.lat=runif(nrow(kdf),0,1e-5)
kdf$lon.Acor=kdf$lon.Acor + r.lon
kdf$lat.Acor=kdf$lat.Acor + r.lat

#bdf <- kdf #subset(kdf,col!="unknown" & col!="multi-color")


#install.packages("jpg")
#require(ggpubr)
#require(jpeg)
#require(png)
#img.file <- paste0(gdir,"/Runways","/MIA.PNG") #system.file(file.path()
#img <- readPNG(img.file)

nbins=6
contours <- MASS::kde2d(kdf$lon.Acor, kdf$lat.Acor, n=nbins)
coords <- which(contours$z==max(contours$z), arr.ind=TRUE)
lon.pk <- round(contours$x[coords[1]],3)
lat.pk <- round(contours$y[coords[2]],3)

library(geosphere)
lat=lat.pk
lon=lon.pk
peak.coordinates <- cbind(lon, lat)
lat <- kdf$lat.Acor
lon <- kdf$lon.Acor
ls.coordinates <- cbind(lon, lat)

dm <- distm(ls.coordinates, peak.coordinates)
#dm2 <- distVincentyEllipsoid(ls.coordinates, peak.coordinates)
close.strikes <- apply(dm<5000,2,which)
csdf <- kdf[close.strikes,]

kdf$col <- factor(str_to_lower(kdf$col))
day1 <- min(kdf$Loc.Date)
dayL <- max(kdf$Loc.Date)
totKs <- nrow(kdf)


LS.v <- ggplot(kdf) + 
  #background_image(img) + 
  geom_point(aes(lon.Acor,lat.Acor,color=col),size=1.5) + 
  theme_powerpoint +

  xlim(lon.min,lon.max) +
  ylim(lat.min,lat.max)+
  scale_color_manual(values=c("red"="red",
                              "green"="green",
                              "blue" ="blue",
                              "white"="white",
                              "other"="black",
                              "unknown"="black",
                              "multi-color"="black"))

LS.c <- LS.v + 
  geom_density_2d(aes(x=lon.Acor, y=lat.Acor),bins=nbins) +
  guides(color=FALSE)

LS.cf <- LS.c + 
  geom_density_2d_filled(aes(x=lon.Acor, y=lat.Acor, alpha=0.3),bins=nbins) +
  #geom_density_2d_filled(aes(x=lon.Acor, y=lat.Acor, alpha=0.5)) +
  #scale_fill_distiller(palette=4, direction=1) +
  guides(alpha=FALSE) +
  labs(fill="Number of\nStrikes") +
  labs(title="All Laser Strikes",
      subtitle=paste0(tcit,", ",tsta,"\n",totKs," Strikes from ",day1," to ",dayL,"\n",
                     "Peak:(",lon.pk,",",lat.pk,")"),
      x="Longitude",
      y="Latitude",
  caption=partnership) 

print(LS.v)
print(LS.c)
print(LS.cf)
hdf <- subset(kdf,Holidays!="None")
tKs <- nrow(kdf)
hKs <- nrow(hdf)

LS.hist <- ggplot() +
  geom_histogram(data=subset(kdf, year<=2021),aes(as.Date(as.POSIXct(DOS))),binwidth=30) +
  geom_histogram(data=subset(hdf, year<=2021),aes(as.Date(as.POSIXct(DOS))),binwidth=30,fill="red") +
  labs(title="Laser Strikes Over Time",
       subtitle=paste0(tcit,", ",tsta),
       x="Date (30day bins)",
       y="Number of Strikes (30day period)",
       caption=partnership) +
  annotate("text", x=as.Date("2011-01-01"), y=30, label=paste0("All Strikes","(",tKs,")"), color="black", hjust=0) +
  annotate("text", x=as.Date("2011-01-01"), y=27, label=paste0("Holiday Strikes","(",hKs,")"), color="red", hjust=0)

#print(LS.hist)


if(save.graph){
fname=paste0(gitdir,"/graphics","/",tcit," ", tsta," Strikes - Contours.png")
png(file=fname,width=600, height=400)
print(LS.cf)
dev.off()
}

}