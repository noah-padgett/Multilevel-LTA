################################################################################
# LBLcorrect
#
# Written on 21Aug09 by Stephen Tueller
#
# Function to read in *.par files produced by Mplus, new label information
# produced by the ReLBLinfo function, and then correct the parameter labels,
# produce corrected *.par files (called *.parc), and produce summaries of both
# the corrected and uncorrected parameter estimates and standard errors.
#
# No warranty express or implied is made for this script.
#
# Version History
#   24Mar2011 - added # check corrigibility first # clause in loop calling 
#               LBLcorrectMain
#   06Jan2010 - original published version
#
# IMPORTANT: This script requires the following -
#   (1) Factor means in the last class be constrained to 0 for identification
#       purposes. The default in Mplus is to use the last class as the reference
#       for the alpha_c, the logit class proportions.
#   (2) The last class must be correctly labeled. This can be checked by looking
#       at the *_NewClassLabels.txt file produce by the ReLBL function. For
#       example, if there are 4 classes (K=4), then the last column in the
#       *_NewClassLabels.txt file must be '4' or 'NA'. If not, label correction
#       must be worked out on a case by case basis for data sets without a
#       '4' or 'NA' in the last column. For such data sets, NA's are produced
#       (inspect *.parc files, see below for explanation), and a warning is
#       printed to the R console.
#
# User inputs
#
#   whichClass_v - a vector which contains a class label for all class specific
#     parameters; use NA for class invariant parameters; the length of
#     whichCLass_v must equal the number of parameters indicated in TECHNICAL 1
#     in Mplus output. Example:
#
#     whichClass_v <- c(NA,NA,NA,
#                 1,1,1,1,1,1,
#                 2,2,2,2,2,2,
#                 3,3,3,3,3,3,
#                 4,4,4,4,4,
#                 1,2,3)
#
#     where the 3 NAs are class invariant factor loadings, the first four 1's
#     are class specific error variances, the fifth 1 is the factor mean, and
#     the sixth 1 is the factor variance. The same pattern holds for classes 2
#     and 3. In class 4, the last 4 is the factor variance and the missing 4
#     is the factor mean which is fixed to zero for identification.
#
#   whichType_v - a vector of the same length as whichClass_v where each
#     parameter has a unique number; use NA for class invariant parameters.
#     Example:
#
#     whichType_v  <- c(NA,NA,NA,
#                 1,2,3,4,5,6,
#                 1,2,3,4,5,6,
#                 1,2,3,4,5,6,
#                 1,2,3,4,6,
#                 7,7,7)
#
#     where NA is the same as in the example for whichClass_v, 1-4 are the
#     class specific error variances for items 1-4, 5 are the factor means,
#     6 are the factor variances, and 7 are the alpha_c, i.e., the logit class
#     proportions.
#
#   NfitInd - the number of fit indices, which can be counted in an Mplus output
#      file under the heading 'RESULTS SAVING INFORMATION'. Example:
#
#       RESULTS SAVING INFORMATION
#
#         Order of data
#
#           Parameter estimates
#            (saved in order shown in Technical 1 output)
#           Standard errors
#            (saved in order shown in Technical 1 output)
#           H0 Loglikelihood
#           H0 Scaling Correction Factor for MLR
#           Number of Free Parameters
#           Akaike (AIC)
#           Bayesian (BIC)
#           Sample-Size Adjusted BIC
#           Entropy
#
#     where there are seven fit indicess when counting from 'H0 Loglikelihood'
#     to 'Entropy'
#
#   newLabelName - the name of the *_NewClassLabels.txt file; must be given in
#     quotes, e.g., newLabelName='myprefix_NewClassLabels.txt'
#
#   parnames - a list of the files produced by Mplus which contain the
#     parameter estimates, their standard errors, and fit indices. In the
#     working directory, use
#
#     parnames <- list.files(getwd(),glob2rx("*.par"),full=F)
#
#     to read in the names of the files, changing *.par if needed.
#
#   K - the number of classes
#
#   stand - a vector specifying the types of standardization requested in the
#     OUTPUT: command of the Mplus input file. If none are requested, the
#     default value stand=c('un') is all that is needed. If the STANDARDIZED
#     option is used, then specify stand=c('UN','STDYX','STDY','STD'). If only
#     one or two of 'STDYX','STDY', or 'STD' are requested in the OUTPUT:
#     command of Mplus, then place these in addition to 'UN' as the values for
#     stand. From stand, nblocks is computed as 2*length(stand); nblocks can
#     take on values of 2, 4, 6, or 8. For example, if stand=c('UN','STDYX'),
#     the Mplus output file containing parameter estimates (i.e., *.par) will
#     have 4 blocks: unstandardized parameter estimates, unstandardized standard
#     errors, STDYX standardized parameter estimates, and STDYX standardized
#     standard errors.
#
#   fileprefix='' - a filename prefix for output produced by this script
#
#   whichIsFmean - parameter numbers of factor means. Default value is NULL.
#     Only use when K=2. If the are more than one factors, and the parameter
#     numbers in TECHNICAL 1 in the Mplus output file are, say, 10, 14, & 21,
#     specify whichIsFmean as
#
#       whichIsFmean=c(10,14,21)
#
# Outputs
#
#   Three output files are produced, where * is replaced by the fileprefix
#     option:
#      *_CorrectedParMEAN.txt (means of label corrected parameters and SEs)
#      *_CorrectedParSD.txt (SDs of label corrected parameters and SEs)
#      *_UncorrectedParMEAN.txt (means of label uncorrected parameters and SEs)
#      *_UncorrectedParSD.txt (SDs of label uncorrected parameters and SEs)
#      *_fitSS.txt (means in 1st column, SDs in 2nd column, of fit statistics)
#     The ParMEAN and ParSD files have the first column as parameter numbers
#     corresponding to the numbers in TECHNICAL 1 of an Mplus output file. The
#     Remaining 2, 4, 6, or 8 columns are as described in stand. The fitSS
#     file has the number of rows corresponding to NfitInd, and the first column
#     has average fit indices and the second column has standard deviations of
#     the fit indices. Note that fit indices are not affected by label switching
#     and the fitSS file is produced for convenience.
#
#   For each *.par produced my Mplus, a corresponding *.parc file is produced.
#     The format of the *.parc file is label corrected parameter estimates,
#     label corrected standard errors, and fit indices in a single tab separated
#     line. Use correctedPars <- scan(*.parc) to read data into R.
#
#   The raw and summarized data matrices can be save as R objects using
#
#       LBLcdat <- LBLcorrect()
#       LBLcdat$fitSS
#       LBLcdat$mpars
#       LBLcdat$sdpars
#       LBLcdat$mopars
#       LBLcdat$sdopars
#       LBLcdat$fits_m
#       LBLcdat$opar_a
#       LBLcdat$cpar_a
#       help('$')
#
#     where fittSS is the fit summary statistics which are also printed to the
#     *_fitSS.txt file, mpars and sdpars are in *_CorrectedParMEAN.txt and
#     *_CorrectedParSD.txt, respectively, mopars and sdopars are in
#     *_UncorrectedParMEAN.txt and *_UncorrectedParSD.txt, respectively, fits_m
#     are the raw fit statistics, opar_a contains the raw parameter estimates,
#     and cpar_a contains the parameter estimates with corrected labels; opar_a
#     and cpar_a have dimension number of data sets BY number of parameters BY
#     nblocks (see stand for a description of nblocks). The dimension of fits_m
#     is number of data sets BY NfitInd. Use help('$') for information on the
#     use of the $ operator in R.
################################################################################
LBLcorrect <- function(whichClass_v,whichType_v,NfitInd,parnames,newLabelName,K,
                       stand=c('UN'),whichIsFmean=NULL,fileprefix='')
{
  # read in the new labels file
  newlab_m <- read.table(newLabelName)
  # if all none of the data sets where corrigible, exit this program
  if(all(is.na(newlab_m[,1])))
  {
    stop('\n*** All data sets have incorrigible labels. Exiting LBLcorrect.',
         '\n*** See *_NewClassLabels.txt')
  }
  # preliminary check and manipulation of user inputs
  if(length(whichClass_v) != length(whichType_v))
  {
    stop('\n*** The lengths of whichClass_v & whichType_v differ')
  }
  nTypes <- max(whichType_v,na.rm=TRUE)
  kcheck <- max(whichClass_v,na.rm=TRUE)
  if(kcheck != K){ stop('\n*** The number of classes indicated in\n',
                   '*** whichClass_v does not match K =',K) }
  ndats <- length(parnames)
  npars <- length(whichClass_v)
  if(!any(stand==c('UN','STDYX','STDY','STD')) | length(stand) >4)
  {
    stop('\n *** stand must contain UN and any combination of',
         '\n *** STDYX, STDY, or STD')
  }
  nblocks = 2*length(stand)
  expectedLength <- npars*nblocks + NfitInd
  if(K==2 && is.null(whichIsFmean))
  {
    stop('\n *** when K=2, the input whichIsFmean must be specified')
  }
  # create a storage object (opar=old parameters, cpar=corrected parameters)
  opar_a <- cpar_a <- array(NA,c(ndats,npars,nblocks))
  fits_m <- matrix(NA,ndats,NfitInd)
  for(i in 1:ndats)
  {
    # check corrigibility first
    if(!is.na(newlab_m[i,1]))
    {
      parname <- parnames[i]
      newlabInfo <- newlab_m[i,]
      correctedLBLs <- LBLcorrectMain(newlabInfo,K,parname,npars,expectedLength,
                                      nblocks)
      cpar_a[i,,] <- t(correctedLBLs$NewPars_m)
      opar_a[i,,] <- t(correctedLBLs$OldPars_m)
      fits_m[i,]  <- correctedLBLs$fits_v
    }
  }
  # if K==2, we must take the absolute value of the factor means
  # and correct ALPHA(C)
  if(K==2)
  {
    cpar_a[,whichIsFmean,] <-  abs(cpar_a[,whichIsFmean,])
    temp <- 1 - exp(cpar_a[,npars,])/( 1 + exp(cpar_a[,npars,]) )
    cpar_a[,npars,] <- log(temp/(1-temp)); rm(temp)
  }
  # summarize the results
  # fit indices
  mfits  <- apply(fits_m,2,mean,na.rm=TRUE)
  sdfits <- apply(fits_m,2,sd,na.rm=TRUE)
  fitsSS <- round(cbind(1:length(mfits),mfits,sdfits),2)
  filename <- paste(fileprefix,'_fitSS.txt',sep='')
  write.table(fitsSS,filename,row.names=F,col.names=F)
  # corrected parameters
  mpars  <- round((apply(cpar_a,c(2,3),mean,na.rm=TRUE)),2)
  sdpars <- round((apply(cpar_a,c(2,3),sd,na.rm=TRUE)),2)
  mpars  <- cbind(1:nrow(mpars),mpars)
  sdpars <- cbind(1:nrow(sdpars),sdpars)
  filename <- paste(fileprefix,'_CorrectedParMEAN.txt',sep='')
  write.table(mpars,filename,row.names=F,col.names=F)
  filename <- paste(fileprefix,'_CorrectedParSD.txt',sep='')
  write.table(sdpars,filename,row.names=F,col.names=F)
  # uncorrected (i.e., 'old') parameters
  mopars  <- round((apply(opar_a,c(2,3),mean,na.rm=TRUE)),2)
  sdopars <- round((apply(opar_a,c(2,3),sd,na.rm=TRUE)),2)
  mopars  <- cbind(1:nrow(mopars),mopars)
  sdopars <- cbind(1:nrow(sdopars),sdopars)
  filename <- paste(fileprefix,'_UncorrectedParMEAN.txt',sep='')
  write.table(mopars,filename,row.names=F,col.names=F)
  filename <- paste(fileprefix,'_UncorrectedParSD.txt',sep='')
  write.table(sdopars,filename,row.names=F,col.names=F)
  # return raw & summarized data as an object, e.g., use LBLcdat <- LBLcorrect()
  # and then LBLcdat$fitSS, etc., to view/manipulate data in R. Type
  #   help('$')
  # into R for more information on the $ operator
  outData <- list(fitsSS=fitsSS,mpars=mpars,sdpars=sdpars,
                  mopars=mopars,sdopars=sdopars,
                  fits_m=fits_m,cpar_a=cpar_a,opar_a=opar_a)
  invisible(outData)
}


################################################################################
# LBLcorrectMain
#
# Written on 21Aug09 by Stephen Tueller
#
# Main function called by LBLcorrect(). This function corrects parameter labels
# in a single data set and is called within a for loop in LBLcorrect.
#
# LBLcorrectMain calls the function newCNs(). This function is not intended for
# direct use. To use LBLcorrect on a single data set, modify a copy of the
# *_NewClassLabels.txt file so that it has only one line containing the data set
# of interest. The output files produced by LBLcorrect will have standard
# deviations of zero.
################################################################################
LBLcorrectMain <- function(newlabInfo,K,parname,
                           npars,expectedLength,nblocks)
{
  # read the par file into a vector
  pars  <- scan(parname,quiet=TRUE)
  # check whether the length of pars conforms with inputs
  lpars <- length(pars)
  if(lpars != expectedLength){ stop(paste('\n*** Inputs are not correct.\n',
                               '** Check whichClass_v, & NfitInd.')) }
  # parse the par file into etimates, standard errors, and fit indices
  OldPars_m <- NewPars_m <- t_m <- matrix(NA,nblocks,npars)
  sloc <- 1 # start location
  for(b in 1:nblocks)
  {
    OldPars_m[b,] <- pars[sloc:(sloc+npars-1)]
    sloc <- sloc + npars
  }
  fits_v <- pars[sloc:length(pars)]; rm(sloc)
  # read new labels
  newlabs <- newlabInfo[3:(K+2)]
  # create NewClass_v
  NewClass_v <- rep(NA,npars)
  for(k in 1:K)
  {
    NewClass_v[whichClass_v == k] <- newlabs[k]
  }; NewClass_v <- unlist(NewClass_v)
  # get cerrected label locations
  newColumnNumbers <- newCNs(whichClass_v,whichType_v,NewClass_v)

  # check whether the last class is correctly assigned. If not, the label
  # correction script may fail.
  if(newlabInfo[length(newlabInfo)] == K )
  {
    # correct parameter labels
    NewPars_m[,] <- OldPars_m[,newColumnNumbers]
  }
  if(newlabInfo[length(newlabInfo)] != K )
  {
    fwarn <- paste('\n*** The new label for the last class is not K =',K,
                   '\n*** for',parname,'- returning NA. Class labels should be',
                   '\n*** corrected manually for',parname)
    warning(fwarn)
  }
  # write corrected parameters to a file
  temp <- matrix(NewPars_m,nrow(NewPars_m),ncol(NewPars_m),byrow=TRUE)
  cat(c(temp,fits_v),file=paste(parname,'c',sep=''))
  # return output
  correctedLBLs <- list(OldPars_m=OldPars_m,NewPars_m=NewPars_m,fits_v=fits_v)
  return(correctedLBLs)
}

################################################################################
# newCNs (new column numbers)
#
# Written on 21Aug09 by Stephen Tueller
#
# This function creates a vector with the new column locations for parameters.
# The newColumnNumbers output is used to correct parameter labels by the
# LBLcorrectMain() function.
################################################################################
newCNs <- function(whichClass_v,whichType_v,NewClass_v)
{
  m <- cbind(whichClass_v,whichType_v,NewClass_v)
  newColumnNumbers <- rep(NA,nrow(m))
  for(i in 1:nrow(m))
  {
    t1 <- m[i,]
    if(is.na(m[i,1])){ newColumnNumbers[i] <- i }
    if(!is.na(m[i,1]))
    {
      if(all(t1 == m[i,][3:1])){ newColumnNumbers[i] <- i }
      if(any(t1 != m[i,][3:1]))
      {
        for(j in 1:nrow(m))
        {
          if(!is.na(m[j,1]))
          {
            t2 <- m[j,][3:1]
            if(all(t1 == t2)){ newColumnNumbers[i] <- j }
          }
        }
      }
    }
  }
  return(newColumnNumbers)
}