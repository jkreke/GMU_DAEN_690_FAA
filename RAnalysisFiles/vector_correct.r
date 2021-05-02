require(geosphere)

#bearing(c(-20,10),c(-20,5))
#  bearing:  the horizontal angle between the direction of one object and another object or 
#    between the direction of one object and that of true north

#corrections


#bring in ndf
nmiles.2.m <- 1852   #meters in  one nautical mile
p <- with(ndf, data.frame(lon, lat))
pm <- as.matrix(p)
b <- ndf$vectA.dir
d <- as.numeric(ndf$vectA.dis)*nmiles.2.m
a <- 6378137
corrected.vec <- destPoint(pm,b,d,a)
c.vec <- as.data.frame(corrected.vec)
names(c.vec) <- c("lon.Acor", "lat.Acor")
ndf <- cbind(ndf,c.vec)
