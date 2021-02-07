#define libraries
library(stringr)


# define directories
print("defining directories")
ddir <- "~/GMU/2021 Course 10 - DAEN 690/data/lasers"

#
print("reading in data")
fname <- paste0(ddir, "/lasers.csv")
ldf <- read.csv(fname,stringsAsFactors = FALSE)
#ldf <- ldf[c(1:200),]

print("defining patterns")

phone.pattern1 <- "^(\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$"
phone.pattern2 <- "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"
phone.pattern3 <- "(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?"

#use pattern3, more generic and output has correct pattern
print("extracting phone numbers")
pnlist <- str_extract_all(ldf$remarks, phone.pattern3)

b <- pnums <- pnlist
n=length(b)

print("converting lists to vectors")
counter=0
allnums <- as.character()
for(i in 1:n){

  if(length(b[[i]])==1){
    b[[i]]=1
    pnums[[i]]=pnlist[[i]]
    allnums <- c(allnums,pnums[[i]])
    }
  else if(length(b[[i]])>1){
    b[[i]]=length(b[[i]])
    pnums[[i]]=paste(unlist(pnlist[[i]]), collapse=" ")
    allnums <- c(allnums,unlist(pnlist[[i]]))
    #print(paste(i," ", pnums[[i]]))
  }
  else {
    b[[i]]=0
    pnums[[i]]="No Number"
  }
  
  #for each pn in i, replace that pn in the remark with a countered padded number
  if(b[[i]]>0){
    for(j in 1:length(b[[i]])){
      counter <- counter + 1
      oldnum <- pnlist[[i]][j]
      newnum <- str_pad(counter, width=nchar(oldnum), side=c('left'), pad='X')
      
      ldf$remarks[i] <- gsub(oldnum, newnum, ldf$remarks[i])
    }
  }
}

allnums <- sort(allnums)
unums <- unique(allnums)
ndf <- data.frame(Unique=unums)

#counter=0
#for each pn in i, replace that pn in the remark with a countered padded number
#for(i in 1:length(b))
# if(b[[i]]>0){
#    for(j in 1:length(b[[i]])){
#     counter <- counter + 1
#      oldnum <- pnlist[[i]][j]
#      newnum <- str_pad(counter, width=nchar(oldnum), side=c('left'), pad='X')
      
#      ldf$remarks[i] <- gsub(oldnum, newnum, ldf$remarks[i])
#    }
# }



  
print("converting to df")
print("b")
b <- as.data.frame(b)
print("pnums")
pnums <- as.data.frame(pnums)

print("transforming")
b <- t(b)
pnums <- t(pnums)

print("removing row names")
rownames(b) <- NULL
rownames(pnums) <- NULL

print("adding to main df")
ldf$poss.pn <- b
ldf$pnums <- pnums



