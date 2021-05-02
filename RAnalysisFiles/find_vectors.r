#define libraries
library(stringr)

source(paste0(adir,"/analysis_setup.r"))

rdf <- with(alldf, data.frame(X_id, date, remarks))
#rdf <- rdf[1:20,]
print("defining patterns")


#find vector [distance][direction][Reference POint]

#Distance
arpt.distL <- "(?!\\'|\\-)[0-9]{1,3}(\\.|)[0-9]{0,2}(\\s*)(NM|[Mm]iles|[Mm]ile|)(\\s*)"


#Reference Point
arpt.distR1 <- "(\\s*)(of|)(\\s*|)(the|)(\\s*|)[A-Z0-9]{3,4}"
arpt.distR2 <- "(\\s*)(of|)(\\s*|)(the|)(\\s*|)[A-Z]{1}[a-z]{1,3}(\\s*|)[A-Za-z]{1,}(\\s*)[A-Za-z]{1,}"


#Direction
begdir <- "("
direct1 <- "[NSEWnsew]|[Nn]orth|[Ss]outh|[Ee]ast|[Ww]est"
direct2 <- "NNE|NE|ENE|ESE|SE|SSE|SSW|SW|WSW|WNW|NW|NNW"
direct2a <- "[Ss]outheast|[Ss]outhwest|[Nn]ortheast|[Nn]orthwest"
direct3 <- "[Nn]orth [Nn][Ee]|[Ee]ast [Nn][Ee]|[Ee]ast [Ss][Ee]|[Ss]outh [Ss][Ee]"
direct4 <- "[Ss]outh [Ss][Ww]|[Ww]est [Ss][Ww]|[Ww]est [Nn][Ww]|[Nn]orth [Nn][Ww]"
direct5 <- "[Nn]orth/[Nn]orth[Ee]ast|[Ee]ast/[Nn]orth[Ee]ast|[Ee]ast/[Ss]outh[Ee]ast|[Ss]outh/[Ss]outh[Ee]ast"
direct6 <- "[Ss]outh/[Ss]outh[Ww]est|[Ww]est/[Ss]outh[Ww]est|[Ww]est/[Nn]orth[Ww]est|[Nn]orth/[Nn]orth[Ww]est"
enddir  <- ")"

directs <- paste0(begdir, direct1, "|", direct2, "|", direct2a,"|", 
                  direct3, "|", direct4, "|", direct5, "|", direct6, enddir)



vpat3 <- "(?!XXX|XXXX)[A-Z]{3,4}[0-9]{5,6}"
vpat4 <- "(?!XXX|XXXX)[A-Z]{3,4}[0-9]{2,3}(/)[0-9]{1,3}"



newpats <- c(paste0(arpt.distL, directs, arpt.distR1),
             paste0(arpt.distL, directs, arpt.distR2),
             vpat3,
             vpat4)




print(paste("making", length(newpats), "passes"))
for(ii in 1:length(newpats)){
  print("==========================================")
  print(paste("making pass number", ii))
  
  #use pattern3, more generic and output has correct pattern
  print("extracting arpt dist")
  #test.str <- "spoipdps sdfo psd 12.434.56 12.34.56 osidfs sodif sodosi dosdifj "

  lllist <<- str_extract_all(rdf$remarks, newpats[ii])   #arpt.dist2)
  #lllist <<- str_extract_all(test.str, latlong.pattern06)

  b <- llnums <- lllist
  n=length(llnums)  #n is the number of rows with at least one 
                  #phone number in it; b will become a list of the  
                  #number of phone numbers in each row


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

print("adding to main df")
assign(paste0("pat",ii),llnums)
assign(paste0("cnt",ii), b)
rdf[[(length(rdf)+1)]] <- llnums
rdf[[(length(rdf)+1)]] <- b
n1 <- paste0('pat', ii)
n2 <- paste0('cnt', ii)
names(rdf)[(length(rdf)-1)] <- n1 
names(rdf)[(length(rdf)-0)] <- n2

print(paste("found", sum(b), "patterns"))

}

