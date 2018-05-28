#Script to download all ABD R scrupts
#
#Scripts from ABD by Whitlock and Schluter
#
# Michael Hunt, 28-05-2018


ABDdownlaod <- function(chapter){
  if (chapter<10){
    filenum=paste0('0',chapter)
  }
  else{
    filenum=paste0(chapter)
  }
  print(filenum)
  url=paste0("http://whitlockschluter.zoology.ubc.ca/wp-content/rcode/chap",filenum,".r")
  destfile=paste0("ABD_chap",filenum,".r")
  download.file(url,destfile)  
}

for (chapter in c(2:4,6:13,15:20)){
    ABDdownlaod(chapter)
}




