########################################################################
# This file contains the function "GetStrippedFile" which takes in a path
# and strips it of comments and blank lines. It then writes a cleaned 
# file out in the same directory with the word "Cleaned" appended to the 
# file name. It returns the path to the cleaned file.
#
# Additionally, this file contains unit testing functions which compare
# the results of GetStrippedFile run on file A to a version of file A
# that had comments and blanks stripped out manually. These unit test 
# functions return booleans depending on whether the tests passed or 
# failed.
########################################################################

test this out 

########################################################################
# FUNCTION- GetStrippedFIle (strips comments and blanks for C fam files)
########################################################################

GetStrippedFile <- function(fpath = path) {
  
  #Create local function to output cleaned file
  OutputFile <- function(){
    
    #create new basename for cleaned file
    bname <- basename(fpath)
    new.bname <- paste0("Clean-", bname)
    
    #create output path and write out
    out.path <- gsub(bname, new.bname, fpath, fixed = T)
    writeLines(rm.blank, out.path, useBytes = T)
    
    return(out.path)
  }
  
  #read in file as character string (faster implementation of readlines)
  s <- file.info( fpath )$size
  buf <- readChar(fpath, s, useBytes=T)
  
  #define pattern for comment lines, blocks, and blank lines
  c.lines <- "\\/\\/.*?(\n|\r)"
  c.blocks <- "(?=\\/\\*)[\\s\\t]*\\/\\*[\\s\\S]*?\\*\\/"

  blank <- "(^\\s*[\\n\\r]*)|(\\s*(\\n|\\r)*\\s*(?=(\\n|\\r)))"
  blank.eof <- "(\\n|\\r)*$"
  
  #remove comment lines/blocks
  rm.comm.line <- gsub(c.lines, "", buf, perl=T, useBytes=T)
  rm.comm.block <- gsub(c.blocks, "", rm.comm.line, perl=T, useBytes=T)
  
  #remove blank lines
  rm.blank <- gsub(blank, "", rm.comm.block, perl=T, useBytes=T)
  rm.blank <- gsub(blank.eof, "", rm.blank, perl=T, useBytes = T)
  
  return(OutputFile())
}

########################################################################
# UNIT TEST 
#   code 900 = fail
#   code 300 = pass
########################################################################

CompareTest <- function(fpath = path, cleanpath = cleanpath){
  
  #create comparison files. fileA = file that was stripped by my code
  fileA <- readLines(GetStrippedFile(fpath))
  fileB <- readLines(cleanpath)

  if(identical(fileA, fileB)){
    return(paste0("300: CompareTest was successful on ",
                  fpath,
                  " and ", 
                  cleanpath))
  }else{    
    #get the first line where compare fails for each file
    errorlineA <- which(!fileA %in% fileB)[1]
    errorlineB <- which(!fileB %in% fileA)[1]
    
    return(paste0("900: CompareTest failed on ", fpath, " at line ", errorlineA,
                  " and on ", cleanpath, " at line ", errorlineB))
  }
}


########################################################################
# RUN TESTING SCRIPT
#   code 900 = fail
#   code 300 = pass
########################################################################

#fpath is the file that GetStrippedFile will clean
fpath <- "C:\\Users\\bfatemi\\Desktop\\rm_comments\\test.txt"

#cleanpath is the file that you have manually stripped of blanks and comments
cleanpath <- "C:\\Users\\bfatemi\\Desktop\\rm_comments\\test_nocomments.txt"

#run and observe return code
CompareTest(fpath = fpath, cleanpath = cleanpath)
