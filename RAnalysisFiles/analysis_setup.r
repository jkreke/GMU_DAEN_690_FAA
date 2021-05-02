#load libraries
print("Loading libraries")
require(stringr)
require(crayon)

source("c:/Users/JGK/Documents/R/UsefulCode/JGK_theme_powerpoint.r")


# define directories
print("defining directories")
gdir <- "~/GMU/2021 Course 10 - DAEN 690"

gitdir <- paste0(gdir,"/GMU_DAEN_690_FAA")
ddir   <- paste0(gdir,"/data")
ldir   <- paste0(ddir,"/lasers")   #where laser.csv is kept
uwdir  <- paste0(ddir,"/team uwords")
sdir   <- paste0(ddir,"/stages")
adir   <- paste0(gdir,"/analysis")
cdir   <- paste0(gdir,"/cities")

#get latest data
#print("loading latest data")
#load(file=paste0(sdir,"/laser_stage03.RData"), verbose=TRUE)



print("loading functions")
#
#
# helper functions
print("SC_Sub")
SC_Sub <- function(this.string){
  new.string = this.string
  db <- "\\"
  
  #\ ^ $ . ? * | + ( ) [ ] { }
  sclist <- c("(", ")", "{", "}", "^", "$", ".", "?", "*", "|", "+", "[", "]")
  
  for(sc in sclist){
    a1 <- paste0(db, sc)
    a2 <- paste0(db, a1) 
    new.string <- gsub(a1, a2, new.string)
  }
  
  return(new.string)
}


print("remove_exclusions")
remove_exclusions <- function(irows, str, remarks, prex, postx){
  #look through the remarks for strings that would exclude PII
  # like "[str] PD"
  newrows1 <- newrows2 <- irows
  postlist <- c("Co", "County", "PD", "Police", "High School",
                "Airport", "ARPT", "INTL", "Ground", "Tower", "Dispatch",
                "Hotel", "Club", "VOR", "Sheriff", "P.D.", "City", 
                ", FL", ", CA", ", SW", ", SC", "LEO", ", CT", ", MN", "International", "University",
                "Ave", "Street", "Road",
                "Jose", "Bernardino", "Diego", "Mateo", "Francisco", "Carlos", "Antonio",
                "Marcos", "Patricio", "Juan", "Joaquin", "Marcus",
                "Lake",
                
                " Co", " County", " PD", " Police", " High School", 
                " Airport", " ARPT", " INTL", " Ground", " Tower", " Dispatch",
                " Hotel", " Club", " VOR", " Sheriff", " P.D.", " City", 
                " FL", " CA", " SW", " SC", " LEO", " CT", " MN", " International", " University", 
                " Ave", " Street", " Road",
                " Jose", " Bernardino", " Diego", " Mateo", " Francisco", " Carlos", " Antonio",
                " Marcos", " Patricio", " Juan", " Joaquin", " Marcus",
                " Lake"
  )
  postlist <- c(postlist, postx)
  
  prelist <- c(".X.")
  prelist <- c(prelist,prex)
  
  #print(postlist)
  #print("")
  #print(prelist)
  
  #stop("check  pre and post lists") #words are getting in properly
  if(!is.null(postlist)){
    combined <- rep(F, length(irows)) 
    
    for(pw in postlist){
      istr=paste0(str,pw)
      a <- grepl(istr, remarks[irows], ignore.case=TRUE)
      combined <- rbind(combined, a)
    }
    #print(combined)
    
    #stop("check postlist finds")
    a <- colSums(combined)
    #print(a)
    #stop("check colsums")
    newrows1 <- irows[a==0]
    #print(newrows1)
    #stop("check newrows1")
    
    #cat("start post with",length(irows), "found", length(a), "now have", length(newrows1),"\n")
  }
  #cat("here are the rows:\n")
  #print(length(newrows1))
  
  if(length(prelist)>0){
    combined <- rep(F, length(newrows1))
    #print(combined)
    #stop("check combined")
    for(pw in prelist){
      istr=paste0(pw,str)
      #cat("new string", istr,"\n")
      #print(istr)
      b <- grepl(istr, remarks[newrows1], ignore.case=TRUE)
      
      combined <- rbind(combined, b)
      #print(combined)
    }
    #stop("check combined")
    b <- colSums(combined)
    newrows2 <- newrows1[b==0]
    #print(newrows)
    #stop("check newrows")
  }
  #cat("start pre with",length(irows), "found", length(b), "now have", length(newrows2),"\n")
  return(newrows2)
  
}

print("rfind")
rfind <- function(str, remarks=rdf$remarks, ign.case=TRUE){
  # for a given string, returns a vector of indexes of those elements in the remarks
  # vector where the string occurs
  str <- SC_Sub(str)
  lvec <- lapply(str, grepl, ignore.case=ign.case, x = remarks)
  lvec <- unlist(lvec)
  rows <- which(lvec, arr.ind=TRUE)
  return(rows)
}

print("sfind")
sfind <- function(str, 
                  spaces=FALSE, 
                  remarks=rdf$remarks, 
                  n.entries=5, 
                  ign.case=TRUE,
                  rm.exclusions=FALSE,
                  prex=NULL, 
                  postx=NULL){
  # prints out all elements of remarks vector where str occurs
  # str is colored to easily identify the string (str) and emphasizing the 
  # context where str is found.  Can identify multiple instances of str within all elements
  row.threshold=2*n.entries
  if(spaces){str <- paste0(" ",str," ")}
  irows <- rfind(str, remarks, ign.case)           #this identifies the rows that have str in it
  if(length(irows)>row.threshold & rm.exclusions){
    newrows <- remove_exclusions(irows, str, remarks, prex, postx)
    cat("#Rows Found", length(irows), "\n")
    cat("#Exclusions ", length(irows)-length(newrows), "\n")
    cat("#Rows to Scan ", length(newrows), "\n")
    irows <- newrows
  } else {
    cat("#Rows Found", length(irows), "(Not checking Exclusions)\n")
  }
  
  if(ign.case){
    cat("Looking for", red(str), "(ignoring case)\n")
  } else {
    cat("Looking for", red(str), "(case-sensitive)\n")
  }
  
  cat("Here is the list of rows:\n")
  cat(irows,"\n")
  
  
  pkey = c("H", "[ENTER]")
  pAct = c("= Quit word, move on to next word", paste("= Get next",n.entries,"rows"))
  pdf <- data.frame(key=pkey, Action=pAct)
  if(length(irows)>0){
    rcounter=0
    
    for(i in 1:length(irows)){
      moveon=FALSE
      if(rcounter==n.entries){
        rcounter=0
        
        
        print(pdf)
        sel <- readline(prompt="Enter Action Key: "
                        #paste(
                        #"\n    H = quit word, move to next word
                        #\n    [Enter] = get next", n.entries, "rows\n")
        )
        if(sel=="H"){
          print("Saving Work, quiting this word and moving on to next)")
          moveon=TRUE
        } else {
          
        }
        
        #invisible(readline(prompt=paste("Press [enter] to get the next", n.entries, "rows ")))
      }
      if(moveon){moveon=FALSE;break}
      rcounter=rcounter+1
      btext <- remarks[irows[i]]              #this gets the next row with the str in it
      last <- nchar(btext)
      kbtext <- SC_Sub(btext)
      kstr  <- SC_Sub(str)
      if(ign.case){
        itext <- tolower(btext)
        istr <- tolower(kstr)
        
        poss <- str_locate_all(itext, istr)  #this command identifies the begining and end positions
      } else {                               #  of istr within itext.
        poss <- str_locate_all(btext, kstr)
      }
      
      poss <- as.data.frame(poss)
      #print(istr)
      #print(str)
      #stop()
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
    cat("No entries found for",red(str),"\n")
  }
  
}  


print("mfind")
mfind <- function(wordlist, spaces=FALSE, remarks=rdf$remarks, n.entries=5, ign.case=TRUE){
  
  lastword <- rev(wordlist)[1]
  for(str in wordlist){
    cat(blue("================================================================\n"))
    ord <- which(wordlist == str)
    sfind(str, spaces, remarks, n.entries, ign.case) 
    if(str!=lastword){
      sel <- readline(prompt="Press [enter] to get the next string")
      if(sel=="q"){stop("quitting")
      } else {}
    }
  }
  print("done")
  
}

print(paste("setting working directory to",adir))
setwd(adir)
