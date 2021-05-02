#define libraries
library(stringr)


inc.marker <- function(this.mark){
  if(!exists("pii_cnt")){
    pii_cnt<<-data.frame(marker=this.mark, counter=0)
    }
  if(length(which(this.mark==pii_cnt$marker))==0){
    tdf <- data.frame(marker=this.mark, counter=0)
    pii_cnt <<- rbind(pii_cnt, tdf)
    }
  count <- pii_cnt$counter[pii_cnt$marker==this.mark]
  count <- count + 1
  pii_cnt$counter[pii_cnt$marker==this.mark] <<- count
  return(count)
}

add.pii <- function(orig.str, masked.str){
  if(!exists("piidf")){
    piidf <<- data.frame(pii=orig.str, mask=masked.str)
  } else {
  piidf <<- rbind(piidf, data.frame(pii=orig.str, mask=masked.str))
  }
}

SC_Sub <- function(this.string){
  new.string = this.string
  db <- "\\"
  
  #\ ^ $ . ? * | + ( ) [ ] { }
  sclist <- c("(", ")", "{", "}", "^", "$", ".", "?", "*", "|", "+", "[", "]", "-")
  
  for(sc in sclist){
    a1 <- paste0(db, sc)
    a2 <- paste0(db, a1) 
    new.string <- gsub(a1, a2, new.string)
  }
  
  return(new.string)
}



# define directories
print("defining directories")
ddir <- "~/GMU/2021 Course 10 - DAEN 690/data/lasers"
uwdir <- "~/GMU/2021 Course 10 - DAEN 690/data/team uwords"
#
print("reading in data")
fname <- paste0(ddir, "/lasers.csv")
ldf <- read.csv(fname,stringsAsFactors = FALSE)
ldf$remarks <- paste0(" ", ldf$remarks," ")
#ldf <- alldf  #ldf[52456,]  #kdf #ldf[46253,]  #18950

print("defining patterns")

phone.pattern1 <- "^(\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]?\\d{3}[\\s.-]?\\d{4}$"
phone.pattern2 <- "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"
phone.pattern3 <- "(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?"



#run this three times to mask all phone numbers

print("making three passes")
for(ii in 1:3){
print(paste("making pass number", ii))
  
#use pattern3, more generic and output has correct pattern
print("extracting phone numbers")
pnlist <- str_extract_all(ldf$remarks, phone.pattern3)

b <- pnums <- pnlist
n=length(pnums)  #n is the number of rows with at least one 
                  #phone number in it; b will become a list of the  
                  #number of phone numbers in each row

print("converting lists to vectors")
counter=0
allnums <- as.character()
for(i in 1:n){

  if(length(b[[i]])==1){
    b[[i]]=1
    pnums[[i]]=pnlist[[i]]   #unnecessary
    allnums <- c(allnums,unlist(pnums[[i]]))
  } else if(length(b[[i]])>1){
    #print(paste("b is", length(b[[i]]) ))
    b[[i]]=length(b[[i]])
    pnums[[i]]=paste(unlist(pnlist[[i]]), collapse=" ")
    allnums <- c(allnums,unlist(pnlist[[i]]))
    #print(paste(i," ", pnums[[i]]))
  } else {
    b[[i]]=0
    pnums[[i]]="No Number"
  }
  
  #for each pn in i, replace that pn in the remark with a countered padded number
  if(b[[i]]>0){
    for(j in 1:b[[i]]){
      mark.char="X"
      counter=inc.marker(mark.char)
      
      oldnum <- pnlist[[i]][j]
      wwidth <- max(4,nchar(oldnum))
      newnum <- str_pad(counter, width=wwidth, side=c('left'), pad=mark.char)
      oldnum1 <- SC_Sub(oldnum)
      ldf$remarks[i] <- gsub(oldnum1, newnum, ldf$remarks[i])
      add.pii(oldnum,newnum)
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
if(ii==1){
  ldf$pn.count1 <- b
  ldf$pnums1 <- pnums
} else if(ii==2){
  ldf$pn.count2 <- b
  ldf$pnums2 <- pnums 
} else if(ii==3){
  ldf$pn.count3 <- b
  ldf$pnums3 <- pnums
} else {}


}
alldf <- ldf
