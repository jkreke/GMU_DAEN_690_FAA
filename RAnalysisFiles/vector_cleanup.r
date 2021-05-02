#after running arpt.dist patcnt.r, get df1

#look at com.pat for patterns
#look at com.cnt for counts of patterns

#start with multi vectors and removing duplicates

require(stringr)
#mystring <- "somexthing $ something else $ somesomething else"
df2 <- df1  #[1:100,]

splitvecs <- function(str, sym="\\$"){
  n=str_count(str, sym)
  m=n+1
  vecs <- unlist(str_split(str,sym,m))
}




vect_reduce <- function(vect){
  #if("Number" %in% vect){v=vect} else {
  v <- splitvecs(vect)
  nvecs = length(v)
  #print(v)
  v=str_trim(v, side = c("both"))
  #print(v)

  
  if(length(v)==1){
    #print(paste("length of vect is", length(v),v))
    vout=v
    tfvec=TRUE
  } else if(length(v)==2){
    #print(paste("length of vect is", length(v),v))
    tfvec=rep(TRUE, length(v))
    if(v[1]==v[2]){tfvec[1]=FALSE
      } else {
      if(grepl(v[1],v[2])){tfvec[1]=FALSE}
      if(grepl(v[2],v[1])){tfvec[2]=FALSE}
      }
  } else if(length(v)==3){
    tfvec=rep(TRUE, length(v))
    if(v[1]==v[2]){tfvec[1]=FALSE
    } else {
      if(grepl(v[1],v[2])){tfvec[1]=FALSE}
      if(grepl(v[2],v[1])){tfvec[2]=FALSE}
    }
    if(v[2]==v[3]){tfvec[2]=FALSE
    } else {
      if(grepl(v[2],v[3])){tfvec[2]=FALSE}
      if(grepl(v[3],v[2])){tfvec[3]=FALSE}
    }    
    if(v[1]==v[3]){tfvec[1]=FALSE
    } else {
      if(grepl(v[1],v[3])){tfvec[1]=FALSE}
      if(grepl(v[3],v[1])){tfvec[3]=FALSE}
    }     
  } else if(length(v)==4){

    tfvec=rep(TRUE, length(v))
    if(v[1]==v[2]){tfvec[1]=FALSE
    } else {
      if(grepl(v[1],v[2])){tfvec[1]=FALSE}
      if(grepl(v[2],v[1])){tfvec[2]=FALSE}
    }
    if(v[2]==v[3]){tfvec[2]=FALSE
    } else {
      if(grepl(v[2],v[3])){tfvec[2]=FALSE}
      if(grepl(v[3],v[2])){tfvec[3]=FALSE}
    }    
    if(v[1]==v[3]){tfvec[1]=FALSE
    } else {
      if(grepl(v[1],v[3])){tfvec[1]=FALSE}
      if(grepl(v[3],v[1])){tfvec[3]=FALSE}
    } 
    if(v[1]==v[4]){tfvec[1]=FALSE
    } else {
      if(grepl(v[1],v[4])){tfvec[1]=FALSE}
      if(grepl(v[4],v[1])){tfvec[4]=FALSE}
    }   
    if(v[2]==v[4]){tfvec[2]=FALSE
    } else {
      if(grepl(v[2],v[4])){tfvec[2]=FALSE}
      if(grepl(v[4],v[2])){tfvec[4]=FALSE}
    }    
    if(v[3]==v[4]){tfvec[3]=FALSE
    } else {
      if(grepl(v[3],v[4])){tfvec[3]=FALSE}
      if(grepl(v[4],v[3])){tfvec[4]=FALSE}
    }     
  
  } else {}
  
  vout <- v[tfvec]
  #cat("v", v,"\n")
  #cat("vout",vout,"\n")
  #cat("tfv",tfvec, "\n")
  return(vout)
}

vtype.check <- function(vec){
  num1 <- "^[1-9]{1}"
  chr1 <- "^[A-Z]{1}"
  chrTF <- grepl(chr1,vec)
  type="A"
  if(chrTF){type="B"}
  return(type)
}


df2$vect4b <- df2$vect3b <- df2$vect2b <- df2$vect1b <- NA
df2$vect4a <- df2$vect3a <- df2$vect2a <- df2$vect1a <- NA

for(i in 1:nrow(df2)){
  str=df2$com.pat[i]
  
  v.out <- vect_reduce(str)
  cat(i, str, "==>>", v.out, "\n")
  
  if(length(v.out)>=1){
    type=vtype.check(v.out[1])
    if(type=="A"){df2$vect1a[i] <- v.out[1]
    } else {df2$vect1b[i] <- v.out[1]}}
  if(length(v.out)>=2){
    type=vtype.check(v.out[2])
    if(type=="A"){df2$vect2a[i] <- v.out[2]
    } else {df2$vect2b[i] <- v.out[2]}}
  if(length(v.out)>=3){
    type=vtype.check(v.out[3])
    if(type=="A"){df2$vect3a[i] <- v.out[3]
    } else {df2$vect3b[i] <- v.out[3]}}
  if(length(v.out)>=4){
    type=vtype.check(v.out[4])
    if(type=="A"){df2$vect4a[i] <- v.out[4]
    } else {df2$vect4b[i] <- v.out[4]}}
}


require(lubridate)

df3 <- df2

df3$DOS <- with_tz(df3$date, tz="US/Eastern")
df3$YW <- strftime(df3$DOS, format="%Y%V")
df3$Ym <- strftime(df3$DOS, format="%Y%m")
vdf <- ddply(df3, .(Ym), function(df){
  dt <- ddply(df, .(DOS), function(dd){
    n=nrow(dd)
    #cat("numberof days in this set", n, "\n")
    avf <- length(dd$com.pat[dd$com.cnt>0])/n
    dfu <- data.frame(avf)
  })
  #print(head(dd))
  mnvc <- mean(dt$avf)
  sdvc <- sd(dt$avf)
  nvc <- nrow(dt)
  sevc <- sdvc/sqrt(nvc)
  n = nrow(df)
  nvecs <- length(df$com.pat[df$com.cnt>0])
  vec.ave<-nvecs/n
  Year <- as.numeric(substr(df$Ym[1],1,4))
  MonF <- as.numeric(substr(df$Ym[1],5,6))/12
  YMF <- Year + MonF
  dfo <- data.frame(vec.ave, Year, MonF, YMF, mnvc, sdvc, sevc, nvc)
})

vdf$Year.Month <- 1:nrow(vdf)
vec.plot <- ggplot(vdf) + 
  geom_point(aes(YMF, vec.ave*100),size=0.9) +
  geom_errorbar(aes(x=YMF-0.015, ymin=100*(vec.ave-sevc), ymax=100*(vec.ave+sevc)), width=0.08) +
  #ylim(0,100) +
  labs(title="Vector Content in Laser Strike Remarks",
       subtitle="Monthly Average of Daily Means",
       x="Year",
       y="Average Percentage",
       caption="FAA-GMU Partnership") +
  scale_x_continuous(breaks=seq(2011,2021,2)) +
  scale_y_continuous(breaks=seq(0,100,20), limits=c(0,100)) +
  annotate("text", x=2017, y=40, label="~80-85% in 2011\n~90-95% in 2021\n0.5-1% increase per year.")
print(vec.plot)


#
#
#start here

pattern_extractor <- function(tstr, tpattern){
  
  lllist <<- str_extract_all(tstr, tpattern)
  
  b <- llnums <- lllist
  n=length(llnums)  #n is the number of rows with at least one 
  #pattern in it; b will become a list of the  
  #number of patterns in each row
  
  
  print("converting lists to vectors")
  counter=0
  all_latlongs <- as.character()
  for(i in 1:n){
    
    if(length(b[[i]])==1){
      b[[i]]=1
      llnums[[i]]=lllist[[i]]   #unnecessary
      all_latlongs <- c(all_latlongs,unlist(llnums[[i]]))
    } else if(length(b[[i]])>1){
      #print(paste("b is", length(b[[i]]) ))
      b[[i]]=length(b[[i]])
      llnums[[i]]=paste(unlist(lllist[[i]]), collapse=" $ ")
      all_latlongs <- c(all_latlongs,unlist(lllist[[i]]))
      #print(paste(i," ", llnums[[i]]))
    } else {
      b[[i]]=0
      llnums[[i]]="No Number"
    }
    
    
    
  }
  
  all_latlongs <- sort(all_latlongs)
  unums <- unique(all_latlongs)
  ndf <- data.frame(Unique=unums)
  
  
  
  print("converting to df")
  b <- as.data.frame(b)
  print("llnums")
  llnums <- as.data.frame(llnums)
  
  print("transforming")
  b <- t(b)
  llnums <- t(llnums)
  
  print("removing row names")
  rownames(b) <- NULL
  rownames(llnums) <- NULL
  
  return(llnums)
}



#search through all vectors in vect1, look for number
#Distance
arpt.distL <- "^(?!\\'|\\-)[0-9]{1,3}(\\.|)[0-9]{0,2}"


#Reference Point
arpt.distR1 <- "[A-Z0-9]{3,4}$"
arpt.distR2 <- "[A-Z]{1}[a-z]{1,3}(\\s*|)[A-Za-z]{1,}(\\s*)[A-Za-z]{1,}$"
rp.pattern <- paste0("(",arpt.distR1,"|",arpt.distR2,")")

#Direction
begdir <- "(\\s|\\d)("
direct1 <- "[NSEWnsew]|[Nn]orth|[Ss]outh|[Ee]ast|[Ww]est"
direct2 <- "NNE|NE|ENE|ESE|SE|SSE|SSW|SW|WSW|WNW|NW|NNW"
direct2a <- "[Ss]outheast|[Ss]outhwest|[Nn]ortheast|[Nn]orthwest"
direct3 <- "[Nn]orth [Nn][Ee]|[Ee]ast [Nn][Ee]|[Ee]ast [Ss][Ee]|[Ss]outh [Ss][Ee]"
direct4 <- "[Ss]outh [Ss][Ww]|[Ww]est [Ss][Ww]|[Ww]est [Nn][Ww]|[Nn]orth [Nn][Ww]"
direct5 <- "[Nn]orth/[Nn]orth[Ee]ast|[Ee]ast/[Nn]orth[Ee]ast|[Ee]ast/[Ss]outh[Ee]ast|[Ss]outh/[Ss]outh[Ee]ast"
direct6 <- "[Ss]outh/[Ss]outh[Ww]est|[Ww]est/[Ss]outh[Ww]est|[Ww]est/[Nn]orth[Ww]est|[Nn]orth/[Nn]orth[Ww]est"
enddir  <- ")(\\s)"

onlydirects <- paste0("([",direct1, "|", direct2, "|", direct2a,"|", 
                      direct3, "|", direct4, "|", direct5, "|", direct6,"])")
directs <- paste0(begdir, direct1, "|", direct2, "|", direct2a,"|", 
                  direct3, "|", direct4, "|", direct5, "|", direct6, enddir)
justdirects <- "(?![0-9])[A-Za-z/]{1,}"


vpat3 <- "(?!XXX|XXXX)[A-Z]{3,4}[0-9]{5,6}"
vpat4 <- "(?!XXX|XXXX)[A-Z]{3,4}[0-9]{2,3}(/)[0-9]{1,3}"

dir_extractor <- function(str, dis, ref){
  #remove all from str that is dis
  rem <- str_remove(str, dis)
  #remove all from remainder that is ref
  rem <- str_remove(rem, ref)
  #remove all from remainder that not special words (NM, nm, miles, mile, of. the)
  rem <- str_remove(rem, "NM")
  rem <- str_remove(rem, "miles")
  rem <- str_remove(rem, "mile")
  rem <- str_remove(rem, "of")
  rem <- str_remove(rem, " the")
  rem[dis=="No Number" & ref=="No NUmber"] <- "No Number"
  rem <- str_trim(rem, side = c("both"))
  
  offset=0
  rem[tolower(rem)=="n" | tolower(rem)=="north"] <- 0+offset
  rem[tolower(rem)=="nne" | tolower(rem)=="northnortheast"| tolower(rem)=="north/northeast"| tolower(rem)=="en"] <- 22.5+offset
  
  rem[tolower(rem)=="ne" | tolower(rem)=="northeast"] <- 45+offset
  rem[tolower(rem)=="ene" | tolower(rem)=="eastnortheast"| tolower(rem)=="east/northeast"] <- 45+22.5+offset
 
  rem[tolower(rem)=="e" | tolower(rem)=="east"] <- 90+offset
  rem[tolower(rem)=="ese" | tolower(rem)=="eastsoutheast"| tolower(rem)=="east/southeast" | tolower(rem)=="es"] <- 90+22.5+offset
  
  rem[tolower(rem)=="se" | tolower(rem)=="southeast"] <- 90+45+offset
  rem[tolower(rem)=="sse" | tolower(rem)=="southsoutheast"| tolower(rem)=="south/southeast"] <- 90+45+22.5+offset
  
  rem[tolower(rem)=="s" | tolower(rem)=="south"] <- 180+offset
  rem[tolower(rem)=="ssw" | tolower(rem)=="southsouthwest"| tolower(rem)=="south/southwest"] <- 180+22.5+offset
  
  rem[tolower(rem)=="sw" | tolower(rem)=="southwest"] <- 180+45+offset
  rem[tolower(rem)=="wsw" | tolower(rem)=="westsouthwest"| tolower(rem)=="west/southwest" | tolower(rem)=="ws"] <- 180+45+22.5+offset
  
  rem[tolower(rem)=="w" | tolower(rem)=="west"] <- 270+offset
  rem[tolower(rem)=="wnw" | tolower(rem)=="westnorthwest"| tolower(rem)=="west/northwest" | tolower(rem)=="wn"] <- 270+22.5+offset
  
  rem[tolower(rem)=="nw" | tolower(rem)=="northwest"] <- 270+45+offset
  rem[tolower(rem)=="nnw" | tolower(rem)=="northnorthwest"| tolower(rem)=="north/northwest"] <- 270+45+22.5+offset
  
  rem[rem=="NN" & tolower(substr(ref,1,1))=="e"] <- 22.5+offset
  rem[rem=="NN" & tolower(substr(ref,1,1))=="w"] <- 270+22.5+offset
  
#  wn >- wnw
#  ws >- wsw
#  en >- ene
#  nn >- nne
#  es >- ese
  
  return(rem)
}


df2$v1a.dis <- pattern_extractor(df2$vect1a, arpt.distL)
df2$v1a.ref <- pattern_extractor(df2$vect1a, rp.pattern)
df2$v1a.dir <- dir_extractor(df2$vect1a, df2$v1a.dis, df2$v1a.ref)


df2$v2a.dis <- pattern_extractor(df2$vect2a, arpt.distL)
df2$v2a.ref <- pattern_extractor(df2$vect2a, rp.pattern)
df2$v2a.dir <- dir_extractor(df2$vect2a, df2$v2a.dis, df2$v2a.ref)


df2$v3a.dis <- pattern_extractor(df2$vect3a, arpt.distL)
df2$v3a.ref <- pattern_extractor(df2$vect3a, rp.pattern)
df2$v3a.dir <- dir_extractor(df2$vect3a, df2$v3a.dis, df2$v3a.ref)


#stop("check v1 v2 v3 first")
df2$v1a.ref <- str_replace(df2$v1a.ref, "^Eof","")
df2$v1a.ref <- str_replace(df2$v1a.ref, "^Wof","")
df2$v1a.ref <- str_replace(df2$v1a.ref, "^Sof","")
df2$v1a.ref <- str_replace(df2$v1a.ref, "^Nof","")
df2$v1a.ref <- str_replace(df2$v1a.ref, "at$","")
df2$v1a.ref <- str_replace(df2$v1a.ref, " at$","")


df2$v2a.ref <- str_replace(df2$v2a.ref, "^Eof","")
df2$v2a.ref <- str_replace(df2$v2a.ref, "^Wof","")
df2$v2a.ref <- str_replace(df2$v2a.ref, "^Sof","")
df2$v2a.ref <- str_replace(df2$v2a.ref, "^Nof","")
df2$v2a.ref <- str_replace(df2$v2a.ref, "at$","")
df2$v2a.ref <- str_replace(df2$v2a.ref, " at$","")

df2$v3a.ref <- str_replace(df2$v3a.ref, "^Eof","")
df2$v3a.ref <- str_replace(df2$v3a.ref, "^Wof","")
df2$v3a.ref <- str_replace(df2$v3a.ref, "^Sof","")
df2$v3a.ref <- str_replace(df2$v3a.ref, "^Nof","")
df2$v3a.ref <- str_replace(df2$v3a.ref, "at$","")
df2$v3a.ref <- str_replace(df2$v3a.ref, " at$","")




#pattern3 and 4
#stop("Check v1 v2 v3")

#srows are those that contain a slash ABC000/000
# and nrows have no slash ABC000000
irows <- which(!is.na(df2$vect1b), arr.ind=TRUE)
srows <- rfind("/", remarks=df2$vect1b)
nrows <- setdiff(irows, srows)
df2$v1b.dis <- df2$v1b.dir <- df2$v1b.ref <- NA

df2$v1b.dis[nrows] <- substring(df2$vect1b[nrows],7,9)
df2$v1b.dir[nrows] <- substring(df2$vect1b[nrows],4,6)
df2$v1b.ref[nrows] <- substring(df2$vect1b[nrows],1,3)

df2$v1b.dis[srows] <- substring(df2$vect1b[srows],8,10)
df2$v1b.dir[srows] <- substring(df2$vect1b[srows],4,6)
df2$v1b.ref[srows] <- substring(df2$vect1b[srows],1,3)




irows <- which(!is.na(df2$vect2b), arr.ind=TRUE)
srows <- rfind("/", remarks=df2$vect2b)
nrows <- setdiff(irows, srows)
df2$v2b.dis <- df2$v2b.dir <- df2$v2b.ref <- NA

df2$v2b.dis[nrows] <- substring(df2$vect2b[nrows],7,9)
df2$v2b.dir[nrows] <- substring(df2$vect2b[nrows],4,6)
df2$v2b.ref[nrows] <- substring(df2$vect2b[nrows],1,3)

df2$v2b.dis[srows] <- substring(df2$vect2b[srows],8,10)
df2$v2b.dir[srows] <- substring(df2$vect2b[srows],4,6)
df2$v2b.ref[srows] <- substring(df2$vect2b[srows],1,3)




irows <- which(!is.na(df2$vect3b), arr.ind=TRUE)
srows <- rfind("/", remarks=df2$vect3b)
nrows <- setdiff(irows, srows)
df2$v3b.dis <- df2$v3b.dir <- df2$v3b.ref <- NA

df2$v3b.dis[nrows] <- substring(df2$vect3b[nrows],7,9)
df2$v3b.dir[nrows] <- substring(df2$vect3b[nrows],4,6)
df2$v3b.ref[nrows] <- substring(df2$vect3b[nrows],1,3)

df2$v3b.dis[srows] <- substring(df2$vect3b[srows],8,10)
df2$v3b.dir[srows] <- substring(df2$vect3b[srows],4,6)
df2$v3b.ref[srows] <- substring(df2$vect3b[srows],1,3)

#change column order
neworder <-  c("X_id",
               "date",
               "remarks",
               "pat1",
               "cnt1",
               "pat2" ,
               "cnt2",
               "pat3",
               "cnt3",
               "pat4",
               "cnt4" ,
               "com.pat",
               "com.cnt",
               "vect1b",
               "vect2b",
               "vect3b",
               "vect4b",
               "vect1a",
               "vect2a",
               "vect3a",
               "vect4a",
               "v1a.dis",
               "v1a.dir",
               "v1a.ref",
               "v2a.dis",
               "v2a.dir",
               "v2a.ref",
               "v3a.dis",
               "v3a.dir",
               "v3a.ref",
               "v1b.dis",               
               "v1b.dir",               
               "v1b.ref",
               "v2b.dis",
               "v2b.dir",
               "v2b.ref",
               "v3b.dis",
               "v3b.dir",
               "v3b.ref"
)

df2 <- df2[,neworder]


df2$vectA.ref <- df2$vectA.dir <- df2$vectA.dis <- NA
df2$vectB.ref <- df2$vectB.dir <- df2$vectB.dis <- NA
df2$vectC.ref <- df2$vectC.dir <- df2$vectC.dis <- NA
df2$vectD.ref <- df2$vectD.dir <- df2$vectD.dis <- NA



#stop("check pre vectABCD creation")
for(i in 1:nrow(df2)){
  
  #which vec groups have a vector
  
  df2cols <- names(df2)
  scol <- which(df2cols=="v1a.dis")
  df2cols <- df2cols[scol:length(df2)]
  nacols <- names(which(sapply(df2[i,scol:length(df2)], anyNA)))
  vcols <- setdiff(df2cols,nacols)
  if(length(vcols)==3){
    df2$vectA.dis[i] <- df2[i,vcols[1]]
    df2$vectA.dir[i] <- df2[i,vcols[2]]
    df2$vectA.ref[i] <- df2[i,vcols[3]]
  }
  if(length(vcols)==6){
    df2$vectA.dis[i] <- df2[i,vcols[1]]
    df2$vectA.dir[i] <- df2[i,vcols[2]]
    df2$vectA.ref[i] <- df2[i,vcols[3]]
    df2$vectB.dis[i] <- df2[i,vcols[4]]
    df2$vectB.dir[i] <- df2[i,vcols[5]]
    df2$vectB.ref[i] <- df2[i,vcols[6]]
  }
  if(length(vcols)==9){
    df2$vectA.dis[i] <- df2[i,vcols[1]]
    df2$vectA.dir[i] <- df2[i,vcols[2]]
    df2$vectA.ref[i] <- df2[i,vcols[3]]
    df2$vectB.dis[i] <- df2[i,vcols[4]]
    df2$vectB.dir[i] <- df2[i,vcols[5]]
    df2$vectB.ref[i] <- df2[i,vcols[6]]
    df2$vectC.dis[i] <- df2[i,vcols[7]]
    df2$vectC.dir[i] <- df2[i,vcols[8]]
    df2$vectC.ref[i] <- df2[i,vcols[9]]
  }
  if(length(vcols)==12){
    df2$vectA.dis[i] <- df2[i,vcols[1]]
    df2$vectA.dir[i] <- df2[i,vcols[2]]
    df2$vectA.ref[i] <- df2[i,vcols[3]]
    df2$vectB.dis[i] <- df2[i,vcols[4]]
    df2$vectB.dir[i] <- df2[i,vcols[5]]
    df2$vectB.ref[i] <- df2[i,vcols[6]]
    df2$vectC.dis[i] <- df2[i,vcols[7]]
    df2$vectC.dir[i] <- df2[i,vcols[8]]
    df2$vectC.ref[i] <- df2[i,vcols[9]]
    df2$vectD.dis[i] <- df2[i,vcols[10]]
    df2$vectD.dir[i] <- df2[i,vcols[11]]
    df2$vectD.ref[i] <- df2[i,vcols[12]]
  }
  if(length(vcols)>12){print(i)}
}

final_cols <- c("X_id",
                "date",
                "remarks",
                "com.pat",
                "com.cnt",
                "vectA.dis",
                "vectA.dir",
                "vectA.ref",
                "vectB.dis",
                "vectB.dir",
                "vectB.ref",
                "vectC.dis",
                "vectC.dir",
                "vectC.ref",
                "vectD.dis",
                "vectD.dir",
                "vectD.ref")
rdf2 <- df2[,final_cols]

vAcols <- c(4,6,7,8)
vBcols <- c(4,9,10,11)
vCcols <- c(4,12,13,14)
vDcols <- c(4,15,16,17)
vABcols <- c(4,6,7,8,9,10,11)

