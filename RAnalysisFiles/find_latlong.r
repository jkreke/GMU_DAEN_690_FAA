#define libraries
library(stringr)

source(paste0(adir,"/analysis_setup.r"))

rdf <- with(alldf, data.frame(X_id, date, remarks))

print("defining patterns")

#phone.pattern1 <- "^(\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$"
#phone.pattern2 <- "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"
#phone.pattern3 <- "(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?"
#latlong.pattern01 <- "^(-?\d+(\.\d+)?),\s*(-?\d+(\.\d+)?)$"
#latlong.pattern02 <- "^(-?\\d+(\\.\\d+)?),\\s*(-?\\d+(\\.\\d+)?)$"
#latlong.pattern03 <- "^[-]?\\d{1,2}[ ]*??[ ]*\\d{1,2}\\.?\\d{1,2}[ ]*\\x27[ ]*\\w$"
#latlong.pattern04 <- "/\\A[+-]?(?:90(?:\\.0{1,18})?|\\d(?(?<=9)|\\d?)\\.\\d{1,18})\\z/x"
#latlong05 <- "^[-+]?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?),\\s*[-+]?(180(\\.0+)?|((1[0-7]\\d)|([1-9]?\\d))(\\.\\d+)?)$"
#latlong06 <- "[0-9 \\.]{3,10}(N|n|)[/][0-9 \\.]{3,10}(W|w|)"

latlong06 <- "[0-9.\\s]{4,}([Nn]|[Nn]orth)[0-9.\\s]{4,}([Ww]|[Ww]est)"
latlong07 <- "[0-9.\\s]{4,}([Nn]|[Nn]orth)[\\s\\/,]{1,4}[0-9.\\s]{4,}([Ww]|[Ww]est)"
latlong08 <- "([Nn]|[Nn]orth)[0-9.\\s]{4,}[\\s\\/,]{1,4}([Ww]|[Ww]est)[0-9.\\s]{4,}"
latlong09 <- "[0-9.\\s*?]{4,}/[0-9.\\s*?]{4,}"
newpats <- c(latlong06, latlong07, latlong08, latlong09)
#newpats <- latlong09



#|[0-9]{5,7}N/[0-9]{5,7}W|[0-9]{5,7}S/[0-9]{5,7}E|[0-9]{5,7}S/[0-9]{5,7}W"
#ds+()
#run this three times to mask all phone numbers

print(paste("making", length(newpats), "passes"))
for(ii in 1:length(newpats)){
  print("==========================================")
  print(paste("making pass number", ii))
  
  #use pattern3, more generic and output has correct pattern
  print("extracting latlong")
  print(newpats[ii])
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
    llnums[[i]]=paste(unlist(lllist[[i]]), collapse=" ")
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

