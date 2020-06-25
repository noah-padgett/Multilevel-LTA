# MLTA Bullshit
setwd("~/Box Sync/Research/MLTA")
# 
# library(MplusAutomation)
# 
# runModels()
# 
# 


# number of variables extracted from Mplus
nvars <- 61
# number of replications
nrep <- 10

conditions <- 1

for(cond in conditions)
{
  # Read data
  filename <- paste0("alloutput_c", cond, ".DAT")
  mydata <- read.delim(filename, header = FALSE, sep="", skip=0, as.is=TRUE)
  mydata <- as.matrix(mydata)
  
  # Create dummy matrix for new values
  newdata <- matrix(0, nrow = nrep, ncol = nvars)
  
  # for original data
  i <- 1
  j <- 1
  # for new matrix
  x <- 1 # Row
  y <- 1 # Column
  
  for( i in 1:nrow(mydata)){
    for( j in 1:ncol(mydata)){
      # The following makes sure the newly written matrix stays in 
      # the correct cell dimensions
      if(y > nvars){
        x <- x + 1
        y <- 1
      }  
      
      if( is.na(mydata[i,j]) == TRUE){ # identifies if cell is empty
        j <- j + 1
        
      } else {   
        # IF cell is not empty, then writes mydata's cell 
        # into the newly written matrix
        newdata[x,y] <- mydata[i,j]
        
        # update the dimensions of the column to look at
        j <- j +1
        y <- y +1
      }
      
    } #End col
  } # end row
  
  if( cond == 1){
    fulldata <- cbind(rep(cond, nrep), newdata)
  } else {
    newdata <- cbind(rep(cond, nrep), newdata)
    fulldata <- rbind(fulldata, newdata)
  }
}

write.table(fulldata, "noah_attempt3.csv", sep = ",",  row.names = F)


# number of variables extracted from Mplus
nvars <- 61
# number of replications
nrep <- 2
mydata <- read.delim("output_c1_rep1.DAT", header = FALSE, sep="", skip=0, as.is=TRUE)
mydata <- as.matrix(mydata)

# Create dummy matrix for new values
newdata <- matrix(0, nrow = nrep, ncol = nvars)

# for original data
i <- 1
j <- 1
# for new matrix
x <- 1 # Row
y <- 1 # Column

for( i in 1:nrow(mydata)){
  for( j in 1:ncol(mydata)){
    # The following makes sure the newly written matrix stays in 
    # the correct cell dimensions
    if(y > nvars){
      x <- x + 1
      y <- 1
    }  
    
    if( is.na(mydata[i,j]) == TRUE){ # identifies if cell is empty
      j <- j + 1
      
    } else {   
      # IF cell is not empty, then writes mydata's cell 
      # into the newly written matrix
      newdata[x,y] <- mydata[i,j]
      
      # update the dimensions of the column to look at
      j <- j +1
      y <- y +1
    }
    
  } #End col
} # end row


