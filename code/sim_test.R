# sim test

library(MplusAutomation)



wd <- getwd()

# 
createModels(paste0(wd, "/code/createmodels_test_c2.txt"))

createModels(paste0(wd, "/code/createmodels_test_c2_analysis.txt"))

runModels(paste0(wd, "/code/conditions/"), recursive = T)

out <- readModels(paste0(wd, "/code/conditions/condition1"),recursive = T)

out[[2]]$class_counts$mostLikely
out[[3]]$class_counts$mostLikely
out[[4]]$class_counts$mostLikely
out[[5]]$class_counts$mostLikely
out[[6]]$class_counts$mostLikely



myfilenames <- list.files(paste0(wd, "/code/conditions/condition1"),glob2rx("*.pro"),full=T)


source(paste0(wd, "/code/LBLinfo.R"))

LBLinfo(filenames=myfilenames,assignCol=15,trueCol=10,CLPSnum=1.2,CAnum=1.2,
        K=2,fileprefix="code/mysim_c1")

LBLinfo(filenames=myfilenames,assignCol=16,trueCol=11,CLPSnum=1.2,CAnum=1.2,
        K=2,fileprefix="code/mysim_c2")

newCL <- read.table(paste0(wd, "/code/mysim_c1_NewClassLabels.txt"))
newSum <- read.table(paste0(wd, "/code/mysim_c1_RelabelSummary.txt"))
