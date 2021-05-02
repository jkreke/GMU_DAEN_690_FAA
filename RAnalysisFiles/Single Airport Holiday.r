#tdf <- ndf[ndf$vectA.dis!="No Number",]
#tdf <- tdf[!is.na(tdf$vectA.dis),]
#tdf$vectA.dis <- as.numeric(tdf$vectA.dis)
#tdf <- tdf[tdf$vectA.dis>50, ]

save.graph=FALSE

setwd(adir)
hcordf <- data.frame()
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

p.cities =1#:nrow(cit.sta)  #nrow(cit.sta)#c(3,8,11,12,13,21,54,69)
print(paste(tcit, tsta))
for(s in p.cities){
  
 # tcit=cit.sta$cit[cit.sta$idx==s]; 
 # tsta=cit.sta$sta[cit.sta$idx==s]
  
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


ddf <- ddply(kdf, .(Loc.Date), function(df){
  nKs <- nrow(df)
  hol <- 1
  if(df$Holidays=="None"){hol <- 0}
  dfo <- data.frame(nKs,hol)
})

max.nKs <- max(ddf$nKs)

g2df <- data.frame()
for(s in 0:max.nKs){
  sdf <- subset(ddf, nKs>=s)
  s.cor <- cor(sdf$nKs, sdf$hol)
  gdf <- data.frame(tcit, tsta, s, s.cor)
  g2df <- rbind(g2df, gdf)
}
g2df <- g2df[!is.na(g2df$s.cor),]
g2df <- g2df[g2df$s.cor==max(g2df$s.cor, na.rm=T),]
if(nrow(g2df)>1){g2df <- g2df[g2df$s==max(g2df$s),]}
hcordf <- rbind(hcordf,g2df)


hdf <- subset(kdf,Holidays!="None")
tKs <- nrow(kdf)
hKs <- nrow(hdf)

LS.hist <- ggplot() +
  geom_histogram(data=subset(kdf, year<=2021),aes(as.Date(as.POSIXct(DOS))),binwidth=30) +
  geom_histogram(data=subset(hdf, year<=2021),aes(as.Date(as.POSIXct(DOS))),binwidth=30,fill="red") +
  labs(title="Laser Strikes Over Time with Holidays",
       subtitle=paste0(tcit,", ",tsta),
       x="Date (30day bins)",
       y="Number of Strikes (30day period)",
       caption=partnership) +
  annotate("text", x=as.Date("2011-01-01"), y=40, label=paste0("All Strikes","(",tKs,")"), color="black", hjust=0) +
  annotate("text", x=as.Date("2011-01-01"), y=37, label=paste0("Holiday Strikes","(",hKs,")"), color="red", hjust=0) +
  annotate("text", x=as.Date("2011-01-01"), y=30, 
           label=paste0("Strike-Holiday Correlation: ",round(g2df$s.cor[1],3),"\n   at ", round(g2df$s,0)," or more strikes/day"), hjust=0)

print(LS.hist)





if(save.graph){
fname=paste0(gitdir,"/graphics","/",tcit," ", tsta," Strikes - Contours.png")
png(file=fname,width=600, height=400)
print(LS.cf)
dev.off()
}

}

hcordf <- arrange(hcordf, desc(s.cor), desc(s))
#write.csv(hcordf, file="Holiday Correlations.csv", row.names=FALSE)
#write.csv(hcordf, file=paste0(gitdir,"/Holiday Correlations.csv"), row.names=FALSE)
