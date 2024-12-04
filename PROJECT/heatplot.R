library(tidyverse)
library(reshape2)
library(ggplot2)



path_name = './Data/'
session = list()
for(i in 1:3){
  session[[i]] = readRDS(paste(path_name,'session',i,".rds",sep = ''))
}


# extract the feedback types of all the stimulus types
s_f = list()
for(i in 1:16){
  s_f[[i]] = numeric(0)
}
for(i in 1:3){
  c_l = session[[i]]$contrast_left
  c_r = session[[i]]$contrast_right
  for(j in 1:length(session[[i]]$contrast_left)){
    if(c_l[j] == 0&c_r[j] == 0){s_f[[1]] = c(s_f[[1]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0&c_r[j] == 0.25){s_f[[2]] = c(s_f[[2]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0&c_r[j] == 0.5){s_f[[3]] = c(s_f[[3]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0&c_r[j] == 1){s_f[[4]] = c(s_f[[4]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.25&c_r[j] == 0){s_f[[5]] = c(s_f[[5]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.25&c_r[j] == 0.25){s_f[[6]] = c(s_f[[6]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.25&c_r[j] == 0.5){s_f[[7]] = c(s_f[[7]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.25&c_r[j] == 1){s_f[[8]] = c(s_f[[8]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.5&c_r[j] == 0){s_f[[9]] = c(s_f[[9]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.5&c_r[j] == 0.25){s_f[[10]] = c(s_f[[10]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.5&c_r[j] == 0.5){s_f[[11]] = c(s_f[[11]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 0.5&c_r[j] == 1){s_f[[12]] = c(s_f[[12]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 1&c_r[j] == 0){s_f[[13]] = c(s_f[[13]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 1&c_r[j] == 0.25){s_f[[14]] = c(s_f[[14]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 1&c_r[j] == 0.5){s_f[[15]] = c(s_f[[15]],session[[i]]$feedback_type[j])}
    if(c_l[j] == 1&c_r[j] == 1){s_f[[16]] = c(s_f[[16]],session[[i]]$feedback_type[j])}
  }
}


# compute the rate of success
success_rate = numeric(16)
for(i in 1:16){
  success_rate[i] = length(which(s_f[[i]] == 1))/length(s_f[[i]])
}
# To facilitate plot we represent the data as a matrix
success_matrix = matrix(success_rate,4,4,byrow = T)
row.names(success_matrix) = c("0","0.25","0.5","1")
colnames(success_matrix) = c("0","0.25","0.5","1")

heat.dat = as.data.frame(success_matrix) %>% 
  mutate(x=rownames(success_matrix)) %>%  
  melt(id='x') %>%                   
  rename('y'='variable','Corr'='value')
list = rownames(success_matrix)
list = factor(list,levels = list)

# heat plot for the average task performance
ggplot(heat.dat,aes(factor(x,levels = list),
                factor(y,levels = list), 
                fill=Corr))+ 
  geom_tile()+ 
  scale_fill_gradient2(mid = "darkorange",high = "darkorchid1",
                       limits=c(0,1),breaks=c(0,1))+
  labs(x = "right contrast",y = "left contrast")+
  theme_bw(base_size = 15) + labs(fill = "choice")
ggsave("heatplot.png",width = 18, height = 10, units = "cm", dpi = 300)
dev.off()
