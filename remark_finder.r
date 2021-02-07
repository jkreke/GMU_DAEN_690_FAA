#This set of  routines helps find a substring and view it within the context of the whole string.
require(stringr)
require(crayon)



rfind <- function(str, remarks=ldf$remarks, ign.case=TRUE){
# for a given string, returns a vector of indexes of those elements in the remarks
# vector where the string occurs
  lvec <- lapply(str, grepl, ignore.case=ign.case, x = remarks)
  lvec <- unlist(lvec)
  rows <- which(lvec, arr.ind=TRUE)
  return(rows)
}

sfind <- function(str, spaces=FALSE, remarks=ldf$remarks, n.entries=5, ign.case=TRUE){
# prints out all elements of remarks vector where str occurs
# str is colored to easily identify the string (str) and emphasizing the 
# context where str is found.  Can identify multiple instances of str within all elements
  
  if(spaces){str <- paste0(" ",str," ")}
  #print(paste("in sfind, starting rfind for",str))
  irows <- rfind(str, remarks, ign.case)

  if(ign.case){
    cat("Looking for", red(str), "(ignoring case)\n")
  } else {
    cat("Looking for", red(str), "(case-sensitive)\n")
      }
  cat("Number of rows found:", length(irows),"\n")
  cat("Here are the list of rows", irows,"\n")

  if(length(irows)>0){
    rcounter=0

    for(i in 1:length(irows)){
      if(rcounter==n.entries){
        rcounter=0
        sel <- readline(prompt=paste("m=mask all\n s=save\n q=quit\n anything other = get next", n.entries, "rows"))
        if(sel=="m"){print("mark for masking, (placeholder for now)")
        } else if(sel=="s"){print("save for later (placeholder for now)")
        } else if(sel=="q"){stop("stopping")
        } else {
            print("continuing")
          }
        
        #invisible(readline(prompt=paste("Press [enter] to get the next", n.entries, "rows")))
      }

      rcounter=rcounter+1
      btext <- remarks[irows[i]]
      last <- nchar(btext)
      if(ign.case){
        itext <- tolower(btext)
        istr <- tolower(str)
        poss <- str_locate_all(itext, istr)
      } else {
        poss <- str_locate_all(btext, str)
      }

      poss <- as.data.frame(poss)
      n <- nrow(poss)
      poss$text=str
      for(p in 1:n){
        poss$text[p] <- substr(btext, poss$start[p], poss$end[p])
      }
      poss$col="red"

      k=n+1
      nak <- rep(NA,k)
      newdf <- data.frame(start=nak, end=nak, text=nak, col="black")
      for(j in 1:k){
        if(j==1){newdf$start[j] <- 0} else {newdf$start[j] <- poss$end[j-1]+1}
        if(j==k){newdf$end[j] <- last} else {newdf$end[j] <- poss$start[j]-1}
        newdf$text[j] <- substring(btext, newdf$start[j], newdf$end[j])
      }
    
    
      poss <- rbind(poss,newdf)
      nposs <- poss[order(poss$start),]
      ctext <- as.character()
      for(j in 1:nrow(nposs)){
        colfun <- get(nposs$col[j])
        ctext <- paste0(ctext, colfun(nposs$text[j]))
      }
      num <- paste0(irows[i],"\n")
      cat(num,ctext,"\n---------------------------\n")
    }
    
    
} else {
  cat("No entries found for",red(str))
}
  
}  
  
mfind <- function(wordlist, spaces=FALSE, remarks=ldf$remarks, n.entries=5, ign.case=TRUE){

  lastword <- rev(wordlist)[1]
  for(str in wordlist){
    cat(blue("================================================================\n"))
    ord <- which(wordlist == str)
    sfind(str, spaces, remarks, n.entries, ign.case) 
    if(str!=lastword){invisible(readline(prompt="Press [enter] to get the next string"))}
  }
  print("done")
  
}


