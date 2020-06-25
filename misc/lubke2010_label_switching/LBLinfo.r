################################################################################
# LBLinfo
#
# Program implementing the class label correction algorithm of 
# Tueller, Drotar, and Lubke (2010)
#
# Written by Stephen Tueller - stueller@nd.edu
# Class relabeling algorithm (LBLalgo) written by 
#   Scott Drotar and Stephen Tueller
#
# No warranty express or implied is made for this script.
#
# Version History
#   06Jan2010 - Uploaded to SourceForge
#
# Program description:
#
# LBLinfo applies the class label correction algorithm to one or more data 
# sets and produces new class labels which can be used to correct parameter
# labels from latent variable mixture model output.
#
# Before using LBLinfo.r, it must be saved in a directory. Then type
#
#   source('c:/my directory/ReLBL.r') 
#
# into R where "c:/" should be changed to the appropriate drive letter, 
# "my directory" should be changed to the full path of the directory in which 
# the script is saved, and "ReLBL.r is the file name of the script.
#
# User Inputs
#
#   filenames - the names of files containing true and assigned class
#               information. Each file must have at least 2 columns. Files must
#               flat subjects by variables plain text file such as is produced
#               by Mplus when SAVE=CPROBABILITIES is specified in the
#               SADVEDATA: command. If, for example, the probabilities are saved
#               in *.pro files, the following line of code can be used to
#               set filenames prior to calling LBLinfo():
#
#                 prodir <- "c:\my directory\"
#                 myfilenames <- list.files(prodir,glob2rx("*.pro"),full=FALSE)
#
#               where prodir is the directory in which the *.pro files are saved 
#   
#               WARNING: glob2rx does not read files in the same order as
#               windows explorer (e.g., for 10 files it reads dat1, dat10,
#               dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9).
#
#               If filenames are specified in LBLinfo, Mraw must be NULL. An
#               error is produced if both filenames and Mraw are NULL and when
#               both are non-NULL. Using the example data from Tueller, Drotar,  
#               and Lubke (2010), an example of using RLBinfo.r is
#
#                 source('c:/my directory/ReLBL.r') 
#                 prodir <- "c:/my directory/"
#                 myfilenames<-list.files(prodir,glob2rx("*.pro"),full=FALSE)
#                 LBLinfo(filenames=myfilenames,assignCol=10,trueCol=5,
#                         CLPSnum=1.2,CAnum=1.2,K=4,fileprefix="mysim")             
#
#   Mraw      - a single class assignment matrix to which the label correction
#               algorithm can be applied. If filenames=null, Mraw must be 
#               specified. The examples in Table 4 of Tueller, Drotar, and 
#               Lubke (2010) are saved to the R workspace by this script as eg1, 
#               eg2, eg3, eg4, and eg5. To view the matrix, type
#
#                 eg1
#
#               into R. To run the label correction algorithm and print new
#               class label information to the R console, type
#                                                              
#                 LBLinfo(Mraw=eg1,CAnum=1.2,CLPSnum=1.2,K=3)
#             
#               into R. 
#
#   assignCol - the column number with assigned class membership. For example, 
#               in the *.pro files described above, if assigned class membership
#               is in the 5th column, assignCol=5. If Mraw is non-NULL,
#               assignCol must be NULL.
#
#   trueCol   - the column number with true class membership, analogous to
#               assignCol. If Mraw is non-NULL, trueCol must be NULL.
#               
#   CAnum     - Correct Assignment numerator; see Eq (4) 
#               in Tueller, Drotar, and Lubke (2010). The 
#               default value is 1.2/K.
#
#   CLPSnum   - Collapsing  numerator; see Eq (5) in
#               Tueller, Drotar, and Lubke (2010). The 
#               default value is 1-1.2/K.
#
#   K         - the number of classes
#
#   fileprefix- a string of the form 'fileprefix' used to name output files;
#               the default is no prefix
#
# Outputs
#
# Four output files are produced by LBLinfo when filenames is specified and 
# Mraw is null where * is replaced by the fileprefix option:
#   *_ConditionalAssignment.txt (conditional correct assignment for each class
#     for each data set)
#   *_NewClassLabels.txt (new class labels)
#   *_RelabelSummary.txt (label correction and conditional assignment summaries)
#   *_RelabelValidation.txt (results of tests whether label correction can be
#     carried out for each data set)
#
# The *_NewClassLabels.txt file saves the new labels for each data set, where
# Column one   : 1 = labels were corrigible, 0 = labels were correct,
#                and NA = labels were incorrigible
# Column two   : Contains file names
# Column three : contains (potentially new) label for true class 1
# Column four  : contains (potentially new) label for true class 2
# Column five  : contains (potentially new) label for true class 3
# and so forth
#
# The ConditionalAssignment.txt file has the same format as NewClassLabels.txt
# where columns 3+ contain conditional correct assignment for each class. Note
# that rows with NA indicate the data set was corrigible by the collapsing
# criterion or data validation criterion. Correct assignment is still printed
# for data sets labeled corrigible because assignment did not fulfill the
# class assignment accuracy criterion, though these are excluded from the
# summary correct assignment in the RelabelSummary.txt file.
#
# To read NewClassLabels.txt into R use:
# newLabels <- read.table('*_NewClassLabels.txt')
################################################################################
LBLinfo <- function(filenames=NULL,Mraw=NULL,assignCol=NULL,trueCol=NULL,
                    CAnum=1.2,CLPSnum=1.2,K,fileprefix='')
{
  # either filenames or Mraw must be NULL
  if(!is.null(filenames) && !is.null(Mraw))
  {
    stop('either filenames or Mraw must be null')
  }
  # compute the correct assignment criteria: Eq (5)
  if(CAnum < 1.01 | CAnum > K){ stop('CAnum must be > 1.01 and <= K =',K) }
  CAcrit <- CAnum/K
  # compute the collapsing criteria: Eq (4)
  if(CLPSnum < 1.01 | CLPSnum > K){ stop('CLPSnum must be > 1.01 and <= K =',K)}
  CLPScrit <- 1 - CLPSnum/K  
  # check whether CAcrit > 1/K
  if(CAcrit <= 1/K){ stop('CAnum is too small') }
  # check whether CLPScrit < 1-1/K
  if(CLPScrit >= 1-1/K){ stop('CLPSnum is too small') }
  # get the number of files in the vector filenames
  if(!is.null(filenames)){ nreps <- length(filenames) }
  if(!is.null(Mraw)){ nreps <- 1 }
  # call main function LBLmain
  LBLmain(filenames,Mraw,assignCol,trueCol,CLPSnum,CAcrit,
            CAnum,CLPScrit,K,nreps,fileprefix)
}

################################################################################
# LBLread
#
# 05Mar09 Written by Stephen Tueller
# 08Apr11 Updated by Stephen Tueller - added check for empty files
#
# Reads data files, computes assignment matrix, and does preliminary checks
# on the matrix including dimension, empty rows/columns,  and collapsing.
################################################################################
LBLread <- function(filename,Mraw,assignCol,trueCol,CAcrit,
                      CLPScrit,K)
{ 
  # read data and create Mraw, the class assignment matrix
  emptyFile <- FALSE
  if(is.null(Mraw))
  {
    # first check whether filename is an empty file
    emptyFile <- file.info(filename)$size == 0
    if(!emptyFile)
    {
      dat <- as.matrix(read.table(filename))
      if(ncol(dat)<2){stop(paste('The file',filename,'contains only 1 column.'))}
      Mraw <- as.matrix(table(dat[,assignCol],dat[,trueCol]))
    }
  }
  # or use Mraw as input
  if(is.null(filename)){ Mraw <- Mraw; empytFile <- FALSE }
  
  # validate if Mraw is provided or the file is not empty
  if(!emptyFile)
  {
    # Begin data validation
    validation <- matrix(TRUE,1,4)
    colnames(validation) <- c('isSquare','areRsum>0','areCsum>0','isNotClpsd')
    # default indicator stating the matrix is corrigible
    corrigible <- TRUE
    # check to make sure the input is a square matrix (not necessarily symmetric)
    if(nrow(Mraw)!=K){ validation[1,1] <- FALSE }
    if(ncol(Mraw)!=K){ validation[1,1] <- FALSE }
    # check whether there are rows with zero sums (absolute collapsing)
    if(min(apply(Mraw,1,sum))==0){ validation[1,2] <- FALSE }
    # check whether there are columns with zero sums (absolute collapsing)
    if(min(apply(Mraw,2,sum))==0){ validation[1,3] <- FALSE }
  
    # check for near collapsing; CelldC = cell dividided by column
    if(all(validation)) # check if not absolutely collapsed
    {
      Csum <- apply(Mraw,2,sum)
      CelldC <- matrix(NA,K,K)
      for(i in 1:K)
      {
        CelldC[i,] <- Mraw[i,]/Csum
      }
      # check whether 2 cells in any row > CLPScrit
      temp <- CelldC > CLPScrit
      for(i in 1:K)
      {
        # declare collapsing if more than one row entry in temp is
        # true
        if(sum(temp[i,])>1){ validation[1,4] <- FALSE }
      }
    }
    # if any entries in the validation vector are false, the matrix is not
    # corrigible (i.e., it cannot be relabeled)
    if(!all(validation)){ corrigible <- FALSE }
  }
  # outputs if empty
  if(emptyFile)
  {
    Mraw <- matrix(NA,K,K)
    validation <- matrix(NA,1,4)
    corrigible <- FALSE
    cat(paste('\n\n LBLinfo WARNING::',filename,'is an empty file\n\n'))
  }
  # End data validation

  # return output
  matdat <- list(Mraw=Mraw,validation=validation,corrigible=corrigible)
  return(matdat)
}

################################################################################
# LBLalgo
#
# 28Jan09 Written by Scott Drotar and Stephen Tueller
#
# Function implementing the class relabeling algorithm
################################################################################
LBLalgo <- function(Mraw,CAcrit,K)
{
  # default indicator stating the matrix is corrigible
  corrigible <- TRUE
  # K minus 1 greater than threshold; default FALSE
  Km1gtt <- FALSE
  # default indicator stating which algorithm is used
  whichAlgo <- NA

  # define KxK matrix Mnew
  Mnew <- matrix(NA,K,K)

  # create a vector of increasing integers for labeling
  a <- 1:K

  # pre-allocate memory for newClassLabel and ConditionalCA
  newClassLabel <- rep(NA,K)
  ConditionalCA <- rep(NA,K)

  # test for assignment accuracy to assess whether labels are corrigible
  # tests whether both rows and columns have repeated maxima
  rowMax <- apply(Mraw, 1, which.max)
  colMax <- apply(Mraw, 2, which.max)
  rowDuplicated <- duplicated(rowMax)
  colDuplicated <- duplicated(colMax)
  RCmaxDup <- FALSE
  if( any(colDuplicated) && any(rowDuplicated) )
  {
    RCmaxDup     <- TRUE
    corrigible <- FALSE
  }

  # determine whether colmax or rowmax relabeling algorithm will be used;
  # if column maxima are duplicated, but row maxima are not,
  # the row algorithm will be used
  useRows <- FALSE
  if( any(colDuplicated) ){ useRows <- TRUE }

  # CLASS RELABELING ALGORITHM
  if( corrigible )
  {
    # COLMAX RELABELING ALGORITHM
    if( !useRows )
    {
      # colMax correctly sorts Mraw
      Mnew     <- Mraw[colMax,]
      # the order of the row locations of the colMax are the new labels
      newClassLabel <- order(colMax)
      whichAlgo <- 'colMax'
    }
    # ROWMAX RELABELING ALGORITHM
    if( useRows )
    {
      # order of column locations correctly sorts Mraw
      Mnew     <- Mraw[order(rowMax),]
      # rowMax column locations are the new labels
      newClassLabel <- rowMax
      whichAlgo <- 'rowMax'
    }
    # compute conditional proportion correct assignment for each column
    ConditionalCA <- diag(Mnew)/( apply(Mnew,1,sum) )
    # compute number of classes in which ConditionalCA is greater than
    # the class assignment accuracy criterion
    numberGreaterThanThreshold <- sum( ConditionalCA > CAcrit  )
    # proportion correct assignment must be above assignment criterion
    # for K-1 classes
    Km1gtt <- TRUE
    if( numberGreaterThanThreshold < (K-1) )
    {
      # reset output
      Km1gtt        <- FALSE
      corrigible  <- FALSE
      newClassLabel <- rep(NA,K)
      Mnew     <- matrix( nrow = K, ncol = K )
    }
  } # END OF CLASS RELABELING ALGORITHM

  # check whether the Mraw was corrected
  wasMrawCorrected <- TRUE # assume it was corrected
  if( any(is.na(newClassLabel)) ){ wasMrawCorrected <- NA }
  if( !is.na(wasMrawCorrected) )
  {
    if( all(newClassLabel==rep(1:K)) ){ wasMrawCorrected <- FALSE }
  }

  # return output in a list
  Relabelresults <- list(Mraw=Mraw,Mnew=Mnew,
                         corrigible=corrigible,
                         newClassLabel=newClassLabel,
                         ConditionalCA=ConditionalCA,
                         wasMrawCorrected=wasMrawCorrected,
                         Km1gtt=Km1gtt,whichAlgo=whichAlgo)

  return(Relabelresults)
}

################################################################################
# LBLout
#
# 19Feb09 Written by Stephen Tueller
# 08Apr11 Modified by Stephen Tueller - add filenames to validation output file
#
# Creates output data files storing results of readClass and classRelabel and
# an output summary of these results across the data sets in filenames
################################################################################
LBLout <- function(validation_m,ConditionalCA_m,newClassLabel_m,whichAlgo_m,
                     corrigible_m,wasMrawCorrected_m,filenames,fileprefix,
                     assignCol,trueCol,CLPSnum,CAcrit,
                     CAnum,CLPScrit,K)
{
  # output file names
  sumFile  <- paste(fileprefix,'_RelabelSummary.txt',sep='')
  valFile  <- paste(fileprefix,'_RelabelValidation.txt',sep='')
  cpcaFile <- paste(fileprefix,'_ConditionalAssignment.txt',sep='')
  nclFile  <- paste(fileprefix,'_NewClassLabels.txt',sep='')
  # write a summary of the relabeling results to a text file
  sumHeader <- paste(sumFile,'written on',date(),'in',getwd(),'\n\n')
  cat(sumHeader,file=sumFile)
  # summary of inputs
  l <- NULL
  l[[1]] <- 'SUMMARY OF INPUTS\n\n'
  l[[2]] <- paste('Assigned Class Column       :',assignCol,'\n')
  l[[3]] <- paste('True Class Column           :',trueCol,'\n')
  l[[4]] <- paste('CAnum                       :',CAnum,'\n') 
  l[[5]] <- paste('Correct Assignment Criteria :',CAcrit,'\n')
  l[[6]] <- paste('CLPSnum                     :',CLPSnum,'\n')
  l[[7]] <- paste('Collapsing Criteria         :',CLPScrit,'\n')
  l[[8]] <- paste('K                           :',K,'\n\n')	
  # write text output
  for(i in 1:length(l)){ cat(l[[i]],file=sumFile,append=TRUE) }
  # summary information
  ntotal <- nrow(corrigible_m)  # total data sets
  nunswt <- sum(corrigible_m,na.rm=TRUE)   # number corrigible
  notswt <- ntotal - nunswt       # number incorrigible
  nrelab <- sum(wasMrawCorrected_m,na.rm=TRUE) # number needing relabiling
  nnrelb <- nunswt - nrelab       # number with correct labels
  # text output
  l <- NULL
  l[[1]] <- 'SUMMARY OF LABEL CORRECTIONS\n\n'
  l[[2]] <- paste('Number of Data Sets             :',ntotal,'\n')
  l[[3]] <- paste('Number with Correct Labels      :',nnrelb,'\n')
  l[[4]] <- paste('Number with Incorrect Labels    :',nrelab,'\n')
  l[[5]] <- paste('Number with Corrigible Labels   :',nunswt,'**','\n')		
  l[[6]] <- paste('Number with Incorrigible Labels :',notswt,'\n\n')
  # write text output
  for(i in 1:length(l)){ cat(l[[i]],file=sumFile,append=TRUE) }
  # validation summary
  cat(paste('SUMMARY OF DATA VALIDATION (individual results in',valFile,')\n\n'),
      file=sumFile,append=TRUE)
  temp <- apply(validation_m,2,sum,na.rm=TRUE)
  # explanartion for validation abbreviations
  l <- NULL
  l[[1]] <- paste('Number of data sets which:\n')
  l[[2]] <- paste('Have square assignment matrices :',temp[1],'\n')
  l[[3]] <- paste('Have all row sums > 0           :',temp[2],'\n')
  l[[4]] <- paste('Have all column sums > 0        :',temp[3],'\n')
  l[[5]] <- paste('Meet collapsed criteria         :',temp[4],'\n')
  l[[6]] <- paste('Meet class assignment criteria  :',temp[5],'\n\n')
  # write text output
  for(i in 1:length(l)){ cat(l[[i]],file=sumFile,append=TRUE) }
  # assignment summary
  cat(paste('SUMMARY OF CONDITIONAL CORRECT ASSIGNMENTS (individual resluts in',
      cpcaFile,')***\n\n'),file=sumFile,append=TRUE)
  temp <- summary(ConditionalCA_m[wasMrawCorrected_m==TRUE,],digits=2)
  suppressWarnings(write.table(format(temp),file=sumFile,quote=FALSE,na="",
              append=TRUE))
  # footnote
  cat(paste('\n** See',nclFile,'for new class labes where:\n'),
      file=sumFile,append=TRUE)
  cat(paste('Column one   : 1 = labels were corrigible, 0 = labels were correct',
      ', and NA = labels incorrigible \n'),
      file=sumFile,append=TRUE)
  cat('Column two   : Contains file names\n',
      file=sumFile,append=TRUE)
  cat('Column three : contains (potentially new) label for true class 1\n',
      file=sumFile,append=TRUE)
  cat('Column four  : contains (potentially new) label for true class 2\n',
      file=sumFile,append=TRUE)
  cat('Column five  : contains (potentially new) label for true class 3\n',
      file=sumFile,append=TRUE)
  cat('and so forth\n\n',file=sumFile,append=TRUE)
  cat(paste('To read',nclFile,'into R use:\n'),file=sumFile,append=TRUE)
  cat(paste('newLabels <- read.table(',nclFile,')',sep=''),file=sumFile,append=TRUE)
  cat(paste('\n\n*** Conditional correct assignment summaries are computed\n',
            'excluding data sets with incorrigible labels (see NA count)\n',
            'after the assignment matrix has been relabeled.\n',
            'Note that assignment values are still recorded in \n',
            'ConditionalAssignment.txt for corrigible matrices which failed\n',
            'the minimum assignment criterion.'),
            file=sumFile,append=TRUE)

  # write the new class labels to a flat text file
  write.table(newClassLabel_m,file=nclFile,row.names=FALSE,col.names=FALSE,
              quote=FALSE)
  # write the validation information to a flat text file
  validation_m <- cbind(filenames,validation_m,whichAlgo_m)
  colnames(validation_m) <- c('file','isSquare','areRsum>0','areCsum>0',
                              'isNotClpsd','K-1>tol?','whichAlgo')
  write.table(validation_m,file=valFile,row.names=FALSE,
              quote=FALSE)

  # modify conditional ca matrix for printing
  ConditionalCA_m <- cbind(as.numeric(wasMrawCorrected_m), filenames,
                           round(ConditionalCA_m,3))
  # write the new class labels to a flat text file
  write.table(ConditionalCA_m,file=cpcaFile,row.names=FALSE,
              col.names=FALSE,quote=FALSE)
}

################################################################################
# LBLmain
#
# 21Feb09 Written by Stephen Tueller
#
# Main function which calls UserInput, readClass, classRelabel, and writeOutput.
################################################################################
LBLmain <- function(filenames,Mraw,assignCol,trueCol,CLPSnum,CAcrit,
                      CAnum,CLPScrit,K,nreps,fileprefix)
{
  # create storage objects
  validation_m <- matrix(NA,nreps,4)
  ConditionalCA_m <- matrix(NA,nreps,K)
  colnames(ConditionalCA_m) <- paste('class',1:K,'\t   ')
  newClassLabel_m <- matrix(NA,nreps,K)
  colnames(newClassLabel_m) <- paste('c',1:K,'isNow',sep='')
  corrigible_m       <- matrix(NA,nreps,1)
  wasMrawCorrected_m <-  matrix(NA,nreps,1)
  Km1gtt_m           <-  matrix(NA,nreps,1)
  whichAlgo_m        <-  matrix(NA,nreps,1)
  for(i in 1:nreps)
  {
    # set the filename
    filename <- filenames[i]
    # get Mraw and validation information
    matdat   <- LBLread(filename,Mraw,assignCol,trueCol,CAcrit,
                          CLPScrit,K)
    # place validation information in validation_m
    validation_m[i,] <- matdat$validation
    # only attempt label correction if labels are corrigible
    if(matdat$corrigible)
    {
      Relabelresults <- LBLalgo(Mraw=matdat$Mraw,CAcrit,K)
      ConditionalCA_m[i,]    <- Relabelresults$ConditionalCA
      newClassLabel_m[i,]    <- Relabelresults$newClassLabel
      corrigible_m[i,]       <- Relabelresults$corrigible
      wasMrawCorrected_m[i,] <- Relabelresults$wasMrawCorrected
      Km1gtt_m[i,]           <- Relabelresults$Km1gtt
      whichAlgo_m[i,]        <- Relabelresults$whichAlgo
    }
  }

  # modify class label matrix for printing
  newClassLabel_m <- cbind(as.numeric(wasMrawCorrected_m),
                           filenames,newClassLabel_m)

  # bind Km1gtt_m with validation_m
  validation_m <- cbind(validation_m,Km1gtt_m)

  # write summary output
  if(nreps > 1)
  {
    LBLout(validation_m,ConditionalCA_m,newClassLabel_m,whichAlgo_m,
             corrigible_m,wasMrawCorrected_m,filenames,fileprefix,
             assignCol,trueCol,CLPSnum,CAcrit,
             CAnum,CLPScrit,K)
  }
  if(nreps == 1)
  {
    out <- list(CLPSnum=CLPSnum,
                CLPScrit=CLPScrit,
                CAnum=CAnum,
                CAcrit=CAcrit,
                K=K,
                validation=matdat$validation,
                Relabelresults=Relabelresults)
    return(out)
  }
}

################################################################################
# table4examples
#
# Example matrices from Table 4; to run LBLinfo with eg1, use:
#
# LBLinfo(filenames=NULL,Mraw=eg1,CLPSnum=5,CAnum=5,
#       K=3,fileprefix='')
################################################################################
table4examples <- function()
{
  eg1 <<- matrix(c(96,3,1,2,7,91,6,89,5),3,3)
  eg2 <<- matrix(c(43,36,21,26,32,42,33,34,33),3,3)
  eg3 <<- matrix(c(34,36,30,33,31,36,36,35,34),3,3)
  eg4 <<- matrix(c(3,92,5,3,96,1,4,3,93),3,3)
  eg5 <<- matrix(c(80,9,11,1,60,39,8,80,12),3,3)
  eg6 <<- matrix(c(77,22,1,0,4,6,2,6,2),3,3)
}; table4examples()

 