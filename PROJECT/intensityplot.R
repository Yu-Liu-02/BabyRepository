# Function

aggregate_events<-function(trials,Z){
  idx=which(trials[,'Z']==Z)
  counts=length(idx);
  
  dataset=data.frame(Z=Z);
  dataset$counts=counts; 
  dataset$Nneurons=trials$Nneurons[[1]]
  
  
  for(i in 1:dataset$Nneurons){
    N.events=unlist(trials[idx,paste("N",i,sep = "")])
    N.ecdf=ecdf(N.events);
    N.epdf=approxfun(density(N.events));
    N.cdf2E=length(N.events)/counts;
    dataset$N=list(N.events);dataset$N.ecdf=list(N.ecdf);
    dataset$N.epdf=list(N.epdf);
    dataset$N.cdf2E=N.cdf2E;
    colnames(dataset)[(4*i):(3+4*i)] = paste(c("N","N","N","N"),i,c("",".ecdf",".epdf",".cdf2E"),sep = "")
  }
  
  # Y.events=unlist(trials[idx,'Y'])
  # Y.ecdf=ecdf(Y.events);
  # Y.epdf=approxfun(density(Y.events));
  # Y.cdf2E=length(Y.events)/counts;
  # 
  # dataset$Y=list(Y.events);dataset$Y.ecdf=list(Y.ecdf);
  # dataset$Y.epdf=list(Y.epdf);
  # dataset$Y.cdf2E=Y.cdf2E;
  return(dataset)
}

Data2Trial<-function(spks,brain_area.idx,IV.idx,spk.time){
  # Output: a list that record the neural activites in one brain region during the entire experiment corresponding to the values of IV
  trials=data.frame(Z=c(rep(1,length(IV.idx[[1]])),rep(0.5,length(IV.idx[[2]])),rep(0.25,length(IV.idx[[3]])),rep(0,length(IV.idx[[4]]))))
  for(i in 1:length(IV.idx)){
    if(i==1){
      forward=0
    } else{
      forward=0
      for(n in 1:(i-1)){
        forward=forward+length(IV.idx[[n]])
      }
    }
    for(j in 1:length(IV.idx[[i]])){
      spk=numeric(0)
      for(k in 1:length(brain_area.idx)){
        max.count=max(spks[[IV.idx[[i]][j]]][brain_area.idx[k],])
        for(m in 1:max.count){
          spk=c(spk,rep(spk.time[which(spks[[IV.idx[[i]][j]]][brain_area.idx[k],]==m)],m)) 
        }
      }
      trials$spk.sort[j+forward]=list(spk)
    }
  }
  return(trials)
}

KDE = function(x, y, h, kernel_name){
  # h: bin width, tuning parameter
  if(kernel_name == "Gaussian"){
    return(exp(-(x-y)^2/(2*h)))
  } else if(kernel_name == "Epanechnikov"){
    return(3*(1-(x-y)^2/h^2)/4*(abs(x-y)<h))
  } else if(kernel_name == "Tri-Cube"){
    return((1-(x-y)^3/h^3)^3*(abs(x-y)<h))
  }
}

Smooth_EmpiricalIntensity = function(intensity, h, kernel_name){
  smooth_intensity=numeric(length(intensity[,2]))
  for(i in 1:length(intensity[,2])){
    smooth_intensity[i]=sum(KDE(intensity[i,1],intensity[,1],h,kernel_name)*intensity[,2])/sum(KDE(intensity[i,1],intensity[,1],h,kernel_name))
  }
  return(smooth_intensity)
}

Plot.EmpiricalIntensity=function(dataset, plot.idx, spk.time, brainarea, smooth, h, kernel_name){
  intensity=matrix(0,length(spk.time),length(dataset$Z))
  for(i in 1:length(dataset$Z)){
    for(j in 1:length(spk.time)){
      intensity[j,i]=length(which(unlist(dataset[,plot.idx][[i]])==spk.time[j]))/dataset$counts[i]
    }
  }
  if(smooth){
    for(i in 1:length(dataset$Z)){
      intensity[,i]=Smooth_EmpiricalIntensity(cbind(spk.time,intensity[,i]),h,kernel_name)
    }
  }
  colnames(intensity)=as.character(dataset$Z)
  
  ymin=max(min(intensity)-1,0); ymax=max(intensity)+3
  
  plot(spk.time,intensity[,1],type="l",xlab="Time (in seconds)",ylab=paste("Empirical Intensity (",brainarea,")",sep=''),lwd=2,cex.axis=1.5,cex.lab=1.5,font.lab=2,col="#9BC985",ylim=c(ymin,ymax))
  lines(spk.time,intensity[,2],lwd=2,col="#F7D58B")
  lines(spk.time,intensity[,3],lwd=2,col="black")
  lines(spk.time,intensity[,4],lwd=2,col="red")
  abline(v=0,col='darkgrey',lty=2)
  legend(0.3,ymax-0.8,col=c("#9BC985","#F7D58B","black","red"),legend = c("Z=0","Z=0.25","Z=0.5","Z=1"),lty=1,lwd=2)
  text(x=0,y=ymax-2,labels="Stimulus onset",cex=1.5,col="#00CDCD",srt=90)
}



path_name = './Data/';
dat=readRDS(paste(path_name,'session1.rds',sep=""))

# First, we extracted trials corresponding to different IV values
# We let the instrumental variable to be 0,0.25,0.5,1 if the contrast in the left side is 0 and the contrast in the right side is 0,0.25,0.5,1
Ntrials=0
iv = sort(unique(dat$contrast_left))
iv.idx = list()
for(i in 1:length(iv)){
  iv.idx[[i]] = which(dat$contrast_left == 0 & dat$contrast_right == iv[i])
}
for(i in 1:length(iv)){
  Ntrials=Ntrials+length(iv.idx[[i]])
}

# Then we convert the data to the Trial type
brain_area = unique(dat$brain_area)
brain_area.idx = list()
for(i in 1:length(brain_area)){
  brain_area.idx[[i]]=which(dat$brain_area==brain_area[i])
}

# Convert
spk.time=c(seq(0,0.4,by=0.01))
trials=data.frame(Z=c(rep(iv[1],length(iv.idx[[1]])),rep(iv[2],length(iv.idx[[2]])),rep(iv[3],length(iv.idx[[3]])),rep(iv[4],length(iv.idx[[4]]))))
for(i in 1:length(brain_area)){
  trials$N = Data2Trial(dat$spks,brain_area.idx[[i]],iv.idx,spk.time)$spk.sort
  names(trials)[i+1] = paste("N",i,sep = "")
}

trials$Nneurons=rep(length(brain_area),nrow(trials)) # number of variables in treatment
##-------------------------------------------##

dataset<-aggregate_events(trials,Z=iv[1]);
for(i in 1:(length(iv)-1)){
  dataset<-add_row(dataset,aggregate_events(trials,Z=iv[i+1]));
}
for(i in 1:length(brain_area)){
  png(filename = paste("intensityplot","_",brain_area[i],".png",sep=""))
  Plot.EmpiricalIntensity(dataset,paste("N",i,sep = ""),spk.time,brain_area[i], smooth=T, h=0.04, "Epanechnikov")
  dev.off()
}
