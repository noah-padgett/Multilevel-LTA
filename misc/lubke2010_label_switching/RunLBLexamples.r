################################################################################
# RunLBL
#
# Script to run the examples given in Tueller, Drotar, and Lubke (2010)
#
# 06Jan2010 by Stephen Tueller
################################################################################

# source the LBLinfo.r script - change directory as needed
# command given in section "The R Language and Environment"
source('LBLinfo.r')

# examine the example matrices given in table 4
eg1
eg2
eg3
eg4
eg5

# run the LBLinfo function for the first example class assignment matrix
# command given in the section "Using the LBLinfo.r script with One Data Set"
LBLinfo(Mraw=eg1,CAnum=1.2,CLPSnum=1.2,K=3)
# if desired, the output can be assigned to an R object
eg1out <- LBLinfo(Mraw=eg1,CAnum=1.2,CLPSnum=1.2,K=3)
# then individual results can be extracted from the object, for example
eg1out$Relabelresults$newClassLabel

# scan the *.pro files, which contain true and assigned class membership
# these are in the zip file downloaded from SourceForge
# commands given in the section "Preparatory Steps and Description of Mplus Outputs"
prodir <-  getwd()
myfilenames<-list.files(prodir,glob2rx("*.pro"),full=FALSE)
# look at the filenames
myfilenames

# run the LBLinfo function for the example datasets
# command given in the section "The LBLinfo.r Function with Multiple Data Sets"
LBLinfo(filenames=myfilenames,assignCol=10,trueCol=5,CLPSnum=1.2,CAnum=1.2,
K=4,fileprefix="mysim")
# this reproduces the output files
#   mysim_ConditionalAssignment.txt
#   mysim_NewClassLabels.txt
#   mysim_RelabelSummary.txt
#   mysim_RelabelValidation.txt
# to confirm this, delete, rename, or move the files and rerun LBLinfo above
# these files are described in the paper and in the preamble of the LBLinfo script

# read the new labels into R for further processing with code you might write
# command given in the section "LBLinfo() Output Files"
newLabels<-read.table('mysim_NewClassLabels.txt')
# look at the results, also found in Figure 1
newLabels

# run the LBLcorrect.r script with the example data from the zip file
# commands given in the section "The LBLcorrect.r Script"
source("LBLcorrect.r")
whichClass_v<-c(NA,NA,NA,
  1,1,1,1,1,1,
  2,2,2,2,2,2,
  3,3,3,3,3,3,
  4,4,4,4,4,
  1,2,3)
whichType_v<-c(NA,NA,NA,
  1,2,3,4,5,6,
  1,2,3,4,5,6,
  1,2,3,4,5,6,
  1,2,3,4,6,
  7,7,7)
NfitInd<-7
K=4
parnames<-list.files(getwd(),glob2rx("*.par"),full=F)
newLabelName<-"mysim_NewClassLabels.txt"
LBLcorrect(whichClass_v,whichType_v,NfitInd,parnames,
  newLabelName,K,fileprefix="mysim")
# running these commands produces the files
#   mysim_CorrectedParMEAN.txt
#   mysim_CorrectedParSD.txt
#   mysim_fitSS.txt
#   mysim_UncorrectedParMEAN.txt
#   mysim_UncorrectedParSD.txt
# these files are described in the preamble of the LBLcorrect script

# in mysim_UncorrectedParMEAN.txt, rows 9, 15, 21, 26 correspond to the means
# given in the left side of Table 6. 

# in mysim_CorrectedParMEAN.txt, rows 9, 15, 21, 26 correspond to the means
# given in the right side of Table 6. 
