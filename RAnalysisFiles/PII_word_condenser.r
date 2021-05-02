#start  with ldf which was created with mask_PNs.r

#for each row in ldf, get remarks cell, break down into individual words, concatenate with others, look forunique set


#create a new df to work with
ndf <- ldf #[1:10000,]

n.rows <- nrow(ndf)

separate_words <- function(remark){
  vec <- strsplit(remark, " ")      #split the remark into individual words
  vec <- unique(vec)                #get the unique words
}


concat_words <- function(uniquewords, newwords){
  all_words <- unlist(newwords) #unlist(newwords)
  thesewords <- unique(c(uniquewords, all_words))
  return(thesewords)
}


icount=1
uwords <- "a"
reset_level=10000
assignlist <- as.character()
for(i in 1:nrow(ndf)){
  
  if(icount==reset_level){
    print("resetting count/words")
    uw.assign <- paste0("uword",i)
    assignlist <- c(assignlist, uw.assign)
    print(uw.assign)
    assign(uw.assign, uwords)
    uwords <- "a"
    icount=0
    print(paste("working",i))
  }
  icount=icount+1
  wlist <- separate_words(ndf$remarks[i])
  ulist <- unlist(concat_words(uwords, wlist))
  uwords <- unique(c(uwords,ulist))
}


print(length(uwords))
