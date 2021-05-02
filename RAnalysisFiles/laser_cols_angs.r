require(scales)
require(ggplot2)
require(plyr)


  #work in meters


m2ml = 1609.34  #1609 meters in a mile

partnership="SAIC / GMU / FAA\nPartnership"

waist.r  <- 0.7 * 1e-3  #mm (1e9/1e3)
nphotons.5mW <- 1.6e16
power.5mW <- 5e-3    #in Watts

source("~/R/RWCode/gausFit.r")


  
#https://www.livephysics.com/physical-constants/optics-pc/wavelength-colors/
Red.wl	  <- mean(c(780, 622))* 1e-9 #in meters  red laser 650nm
Orange.wl <- mean(c(622, 597))* 1e-9  #632.8
Yellow.wl	<- mean(c(597, 577))* 1e-9
Green.wl  <- mean(c(577, 492))* 1e-9 #543.5     
Blue.wl	  <- mean(c(492, 455))* 1e-9
Violet.wl <- mean(c(455, 390))* 1e-9

l.cols.wl <- c(Red.wl, Orange.wl, Yellow.wl, Green.wl, Blue.wl, Violet.wl)
l.cols    <- c("red", "orange", "yellow", "green", "blue", "violet")


theta <- (l.cols.wl * 4) / (2*pi*waist.r)
r.z <- pi*waist.r^2/l.cols.wl

cdf <- data.frame(l.cols, l.cols.wl, theta, index=seq(1:6), r.z)


colors.wl <- ggplot(cdf) +
  geom_bar(aes(index, l.cols.wl*1e9),fill=l.cols,stat="identity") +
  labs(title="Color Wavelengths",
       x="Color",
       y="Wavelength(nm)",
       caption=partnership) +
  scale_x_continuous(breaks=seq(1,length(l.cols)),labels=l.cols) +
  theme_powerpoint

print(colors.wl)


colors.angle <- ggplot(cdf) +
  geom_bar(aes(index, theta*1e3),fill=l.cols,stat="identity") +
  labs(title="Laser Spreading Angle for Different Colors",
       subtitle="Waist=0.7mm",
       x="Color",
       y="Angle (mrad)",
       caption=partnership) +
  scale_x_continuous(breaks=seq(1,length(l.cols)),labels=l.cols) +
  theme_powerpoint

print(colors.angle)

#z=seq(0.1,3,0.1)*m2ml
z=seq(0,10000,10)
z[1]<-mean(cdf$r.z)
#z=c(1:10)*1000 
  #in meters conv to miles
#z=c(1:10)*1609.34


las.rad <- function(colr,z){
  if(colr %in% "red"){    wl=Red.wl}
  if(colr %in% "orange"){ wl=Orange.wl}
  if(colr %in% "yellow"){ wl=Yellow.wl}
  if(colr %in% "green"){  wl=Green.wl}
  if(colr %in% "blue"){   wl=Blue.wl}
  if(colr %in% "violet"){ wl=Violet.wl}
  z.R = pi*waist.r^2/wl
  rad.z <- waist.r * sqrt(1 + (z/z.R)^2)
  area.z = pi*rad.z^2
  return(rad.z)
}

coldf <- data.frame()
for(c in l.cols){
  radius <- las.rad(c,z)
  area   <- pi*radius^2
  df<-data.frame(color=c, z, radius, area)
  coldf <- rbind(coldf,df)
}

radius.spot <- ggplot(coldf) + 
  geom_line(aes(z,radius,color=color),size=1) +
  labs(title="Radius of Laser Spot",
       subtitle="Waist=0.7mm",
       x="Distance(m)",
       y="Radius(m)",
       caption=partnership) +
  theme_powerpoint +
  geom_vline(xintercept=m2ml)+
  annotate("text",x=1250, y=1.5, label="1 mile", angle=90)+
  scale_color_manual(values=c("red"="red",
                              "orange"="orange",
                              "yellow"="yellow",
                              "green"="green",
                              "blue" ="blue",
                              "violet"="violet"))

print(radius.spot)

area.spot <- ggplot(coldf) + 
  geom_line(aes(z,area,color=color),size=1) +
  labs(title="Area of Laser Spot",
       subtitle="Waist=0.7mm",
       x="Distance(m)",
       y="Area(m^2)",
       caption=partnership) +
  geom_vline(xintercept=m2ml)+
  annotate("text",x=1250, y=6, label="1 mile", angle=90)+
  theme_powerpoint +
  scale_color_manual(values=c("red"="red",
                              "orange"="orange",
                              "yellow"="yellow",
                              "green"="green",
                              "blue" ="blue",
                              "violet"="violet"))

print(area.spot)


#power
#nphotons.5mW/(pi*0.7e-3^2)
power.spot <- ggplot(coldf) + 
  geom_line(aes(z,power.5mW/area,color=color), size=1) +
  labs(title="Laser Light Power Density",
       subtitle="5mW Laser; Waist=0.7mm",
       x="Distance(m)",
       y="Irradiance(W/m^2)",
       caption=partnership) +
  theme_powerpoint +
  geom_vline(xintercept=m2ml)+
  geom_hline(yintercept=1050)+
  annotate("text",x=1000, y=1, label="1 mile", angle=90)+
  annotate("text", x=100, y=1500, label="Sun's Irradiance: 1050 W/m^2", hjust=0, size=3) +
  scale_y_log10(labels=scientific) +
  scale_x_log10(labels=scientific) +
  scale_color_manual(values=c("red"="red",
                              "orange"="orange",
                              "yellow"="yellow",
                              "green"="green",
                              "blue" ="blue",
                              "violet"="violet"))

print(power.spot)




CINdf <- read.csv("ColorInNature.csv",stringsAsFactors = F)

Sens.plot <- ggplot(CINdf) + 

  geom_vline(xintercept=cdf$l.cols.wl*1e9, color=cdf$l.cols, size=3, alpha=0.2) +
  geom_line(aes(Wavelength, Photopic.LE*683),color='black', size=1) + 
  geom_line(aes(Wavelength, Scotopic.LE*1700),color='gray40', size=1,linetype='dotted') +
  #annotate("text", x=cdf$l.cols.wl*1e9-10, y=1500, label=cdf$l.cols, angle=90, color=cdf$l.cols) +
  labs(title="Luminous Efficacy",
       subtitle="Daytime (Photopic) and Nighttime (Scotopic) Eye Sensitivity",
       x="Wavelength(nm)",
       y="Lumionus Efficacy (lm/W)",
       caption=partnership) +
  annotate("segment", x=660, xend=680, y=1500, yend=1500, color='black',size=1) +
  annotate("text",    x=690, y=1500, label="Photopic (Daytime)", color='black',size=4,hjust=0 ) +
  annotate("segment", x=660, xend=680, y=1350, yend=1350, color='gray40',size=1,linetype='dotted') +
  annotate("text",    x=690, y=1350, label="Scotopic (Nighttime)", color='black',size=4,hjust=0 )


print(Sens.plot)


cdf$Wavelength <- cdf$l.cols.wl*1e9
a <- fit.gaus.density(CINdf$Wavelength, CINdf$Photopic.Conv, c(560, 40, 694 ))
par <- a$par
pred <- gaussian(cdf$Wavelength, par)
cdf$LE.photopic <- pred
cat("photopic fit pars:", par[1], par[2], par[3], "\n")

a <- fit.gaus.density(CINdf$Wavelength, CINdf$Scotopic.Conv, c(500, 40, 1700 ))
par <- a$par
pred <- gaussian(cdf$Wavelength, par)
cdf$LE.scotopic <- pred
cat("scotopic fit pars:", par[1], par[2], par[3], "\n")



coldf <- ddply(coldf, .(color), function(df){
  tcol=df$color[1]
  df$pho.factor      <- cdf$LE.photopic[cdf$l.cols==tcol]
  df$sco.factor      <- cdf$LE.scotopic[cdf$l.cols==tcol]
  df$spectralPower.P <- (power.5mW * df$pho.factor)/df$area
  df$spectralPower.S <- (power.5mW * df$sco.factor)/df$area
  cat(tcol, "  pho", df$pho.factor[1], "  sco",df$sco.factor[1],"\n")
  dfo <- df
})

#coldf <- coldf2
#power
Spectral.Power.P <- ggplot(coldf) + 
  geom_line(aes(z,spectralPower.P,color=color), size=1) +
  labs(title="Spectral Power (Daytime)",
       subtitle="5 mW Laser; Waist=0.7mm",
       x="Distance(m)",
       y="Lux (Lumens/m^2)",
       caption=partnership) +
  theme_powerpoint +
  geom_vline(xintercept=m2ml)+
  geom_hline(yintercept=98000)+
  geom_hline(yintercept=25000)+
  annotate("text",x=1300, y=1e3, label="1 mile", angle=90)+
  annotate("text", x=100, y=170000, label="Sun's Illuminance: 98000 Lux", hjust=0, size=3) +
  annotate("text", x=100, y=40000, label="Recommended Max: 25000 Lux", hjust=0,size=3) +
  scale_y_log10(labels=scientific, breaks=c(1e-8, 1e-6, 1e-4, 1e-2, 1e0, 1e2, 1e4, 1e6)) +
  scale_x_log10(labels=scientific) +
  scale_color_manual(values=c("red"="red",
                              "orange"="orange",
                              "yellow"="yellow",
                              "green"="green",
                              "blue" ="blue",
                              "violet"="violet"))

print(Spectral.Power.P)

Spectral.Power.S <- ggplot(coldf) + 
  geom_line(aes(z,spectralPower.S,color=color), size=1) +
  labs(title="Spectral Power (Nighttime)",
       subtitle="5 mW Laser; Waist=0.7mm",
       x="Distance(m)",
       y="Lux (Lumens/m^2)",
       caption=partnership) +
  theme_powerpoint +
  geom_vline(xintercept=m2ml)+
  geom_hline(yintercept=98000)+
  geom_hline(yintercept=25000)+
  annotate("text",x=1300, y=1e3, label="1 mile", angle=90)+
  annotate("text", x=100, y=170000, label="Sun's Illuminance: 98000 Lux", hjust=0, size=3) +
  annotate("text", x=100, y=40000, label="Recommended Max: 25000 Lux", hjust=0,size=3) +
  scale_y_log10(labels=scientific, breaks=c(1e-8, 1e-6, 1e-4, 1e-2, 1e0, 1e2, 1e4, 1e6)) +
  scale_x_log10(labels=scientific) +
  scale_color_manual(values=c("red"="red",
                              "orange"="orange",
                              "yellow"="yellow",
                              "green"="green",
                              "blue" ="blue",
                              "violet"="violet"))

print(Spectral.Power.S)
