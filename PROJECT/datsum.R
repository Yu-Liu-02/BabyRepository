library(dplyr)



##-------------------------------------------##
# Data extraction ####
# In this application, we use data from session 1
path_name = './Data/';
dat=readRDS(paste(path_name,'session1.rds',sep=""))
##-------------------------------------------##


##-------------------------------------------##
# Summary ####
names(dat) # variables in session 1

# basic information
brain_area = unique(dat$brain_area)
n.neurons = dim(dat$spks[[1]])[1]
n.trial = length(dat$contrast_left)
cat(sprintf("There are %d different brain regions recorded in session 1, which are listed below:\n",
            length(brain_area)))
cat(brain_area, sep = ', ', "\n")
cat(sprintf("There are %d neurons recorded among %d trials in session 1.\n",
            n.neurons, n.trial))


# summary of different scenarios
sum.table = matrix(0,4,ncol = 5)
for(i in 1:n.trial){
  if(dat$contrast_left[i] > dat$contrast_right[i]){
    sum.table[1,3] = sum.table[1,3] + 1
    if(dat$feedback_type[i] == 1){
      sum.table[1,2] = sum.table[1,2] + 1
    } else{
      sum.table[1,1] = sum.table[1,1] + 1
    }
  } else if(dat$contrast_left[i] < dat$contrast_right[i]){
    sum.table[2,3] = sum.table[2,3] + 1
    if(dat$feedback_type[i] == 1){
      sum.table[2,2] = sum.table[2,2] + 1
    } else{
      sum.table[2,1] = sum.table[2,1] + 1
    }
  } else if((dat$contrast_left[i] == dat$contrast_right[i]) & (dat$contrast_left[i]==0)){
    sum.table[3,3] = sum.table[3,3] + 1
    if(dat$feedback_type[i] == 1){
      sum.table[3,2] = sum.table[3,2] + 1
    } else{
      sum.table[3,1] = sum.table[3,1] + 1
    }
  } else{
    sum.table[4,3] = sum.table[4,3] + 1
    if(dat$feedback_type[i] == 1){
      sum.table[4,2] = sum.table[4,2] + 1
    } else{
      sum.table[4,1] = sum.table[4,1] + 1
    }
  }
}

for(i in 1:4){
  sum.table[i,4] = sum.table[i,1]/sum.table[i,3]
  sum.table[i,5] = sum.table[i,2]/sum.table[i,3]
}
colnames(sum.table) <- c('Failure', 'Success', 'Total', 'Failure (prob)', 'Success (prob)')
rownames(sum.table) <- c("L > R", "L < R", "N(0)", "B(!0)")

write.table(round(sum.table,digits=2),file = "datsum.txt")

##-------------------------------------------##