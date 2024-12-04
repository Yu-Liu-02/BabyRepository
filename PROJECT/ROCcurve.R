
library(xgboost)
library(pROC)
library(e1071)


path_name = './Data/'
session = list()
for(i in 1:3){
  session[[i]] = readRDS(paste(path_name,'session',i,".rds",sep = ''))
}

L = R = FB = numeric(0)
for(i in 1:3){
  L = c(L, session[[i]]$contrast_left)
  R = c(R, session[[i]]$contrast_right)
  FB = c(FB, session[[i]]$feedback_type)
}

FB[which(FB==-1)] = 0
dat = data.frame(L,R,FB)

iv = sort(unique(dat$L))
count = list()
for(i in 1:length(iv)^2){
  count[[i]] = numeric(0)
}

for(i in 1:nrow(dat)){
  row.idx = which(dat$L[i] == iv)
  col.idx = which(dat$R[i] == iv)
  count[[(row.idx-1)*4+col.idx]] = c(count[[(row.idx-1)*4+col.idx]], i)
  
}
train.idx = numeric(0)
val.idx = numeric(0)

for(i in 1:length(iv)^2){
  idx = sample(count[[i]], floor(0.8*length(count[[i]])), replace = F)
  train.idx = c(train.idx, idx)
  val.idx = c(val.idx,setdiff(count[[i]],idx))
}

##-------------------------------------------##
# Score Calculation ####
# Based on the table 1, we know that for mice, the task difficulty is order by:
# "L > R" < "N(0)" < "L < R" < "B(!0)", we borrow information to build a score
# system for each task
s_f = list()
for(i in 1:16){
  s_f[[i]] = numeric(0)
}
c_l = dat[train.idx,"L"]
c_r = dat[train.idx,"R"]
for(j in 1:length(train.idx)){
  if(c_l[j] == 0&c_r[j] == 0){s_f[[1]] = c(s_f[[1]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0&c_r[j] == 0.25){s_f[[2]] = c(s_f[[2]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0&c_r[j] == 0.5){s_f[[3]] = c(s_f[[3]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0&c_r[j] == 1){s_f[[4]] = c(s_f[[4]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.25&c_r[j] == 0){s_f[[5]] = c(s_f[[5]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.25&c_r[j] == 0.25){s_f[[6]] = c(s_f[[6]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.25&c_r[j] == 0.5){s_f[[7]] = c(s_f[[7]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.25&c_r[j] == 1){s_f[[8]] = c(s_f[[8]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.5&c_r[j] == 0){s_f[[9]] = c(s_f[[9]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.5&c_r[j] == 0.25){s_f[[10]] = c(s_f[[10]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.5&c_r[j] == 0.5){s_f[[11]] = c(s_f[[11]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 0.5&c_r[j] == 1){s_f[[12]] = c(s_f[[12]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 1&c_r[j] == 0){s_f[[13]] = c(s_f[[13]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 1&c_r[j] == 0.25){s_f[[14]] = c(s_f[[14]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 1&c_r[j] == 0.5){s_f[[15]] = c(s_f[[15]],dat[train.idx[j],"FB"])}
  if(c_l[j] == 1&c_r[j] == 1){s_f[[16]] = c(s_f[[16]],dat[train.idx[j],"FB"])}
}


# compute the rate of success
success_rate = numeric(16)
for(i in 1:16){
  success_rate[i] = length(which(s_f[[i]] == 1))/length(s_f[[i]])
}
success.matrix = matrix(success_rate,4,4,byrow = T)
row.names(success.matrix) = c("0","0.25","0.5","1")
colnames(success.matrix) = c("0","0.25","0.5","1")
score.matrix = 1 - success.matrix
score.matrix = (score.matrix-min(score.matrix))/(max(score.matrix)-min(score.matrix))

for(i in 1:nrow(dat)){
  row.idx = which(dat$L[i] == as.numeric(row.names(score.matrix)))
  col.idx = which(dat$R[i] == as.numeric(row.names(score.matrix)))
  dat[i,"score"] = score.matrix[row.idx,col.idx]
}
##-------------------------------------------##



##-------------------------------------------##
# Logistic regression Model to predict whether the mice can success ####

lr.model = glm(FB~score + L + R, dat[train.idx,], family = binomial)
FB_P = predict(lr.model, newdata = dat[val.idx,], type = "response")
FB_T = dat[val.idx,"FB"]
roc.lr = roc(FB_T, FB_P)
AUC.lr = auc(roc.lr)
options(repr.plot.width=10, repr.plot.height=10)
png(filename = "ROC_logit.png")
plot(1-roc.lr$specificities,roc.lr$sensitivities, type = "l", xlab = "
     1 - Specificity", ylab = "Sensitvity", font.lab = 2, cex.lab=1.5,cex.axis=1.5,lwd = 2, col = "red")
abline(a = 0, b=1, col = "grey", lty = 2, lwd = 2)
text(x = 0.2, y = 0.8, labels = paste("AUC = ", round(AUC.lr,2)), cex = 2)
dev.off()

##-------------------------------------------##


##-------------------------------------------##
# XG-boost to predict whether the mice can success ####
LR.train = model.matrix(~L+R+score-1, data = dat[train.idx,])
xgb.model <- xgboost(data = LR.train, label = dat[train.idx,"FB"], nrounds = 100, objective = "binary:logistic")
LR.val <- model.matrix(~L+R+score-1, data = dat[val.idx,])
FB_P.xgb <- predict(xgb.model, LR.val)
roc.xgb = roc(FB_T, FB_P.xgb)
AUC.xgb = auc(roc.xgb)
options(repr.plot.width=10, repr.plot.height=10)
png(filename = "ROC_xgb.png")
plot(1-roc.xgb$specificities,roc.xgb$sensitivities, type = "l", xlab = "
     1 - Specificity", ylab = "Sensitvity", font.lab = 2, cex.lab=1.5,cex.axis=1.5,lwd = 2, col = "red")
abline(a = 0, b=1, col = "grey", lty = 2, lwd = 2)
text(x = 0.2, y = 0.8, labels = paste("AUC = ", round(AUC.xgb,2)), cex = 2)
dev.off()
##-------------------------------------------##


##-------------------------------------------##
# SVM to predict whether the mice can success ####
svm.model = svm(factor(FB)~L+R+score, data = dat[train.idx,], kernel = "radial", cost = 1, gamma = 0.8,probability = T)
FB_P.svm <- attr(predict(svm.model, dat[val.idx,], probability = T),"probabilities")[,"1"]
roc.svm = roc(FB_T, FB_P.svm)
AUC.svm = auc(roc.svm)
options(repr.plot.width=10, repr.plot.height=10)
png(filename = "./Figure/ROC_svm.png")
plot(1-roc.svm$specificities,roc.svm$sensitivities, type = "l", xlab = "
     1 - Specificity", ylab = "Sensitvity", font.lab = 2, cex.lab=1.5,cex.axis=1.5,lwd = 2, col = "red")
abline(a = 0, b=1, col = "grey", lty = 2, lwd = 2)
text(x = 0.2, y = 0.8, labels = paste("AUC = ", round(AUC.svm,2)), cex = 2)
dev.off()
##-------------------------------------------##



