library(RColorBrewer)


# Plot Function
plot.one.spikes<-function(id,regions,regions.col,dat){
  region.list=dat$brain_area;
  n.neurons=0
  
  for(i.r in 1:length(regions)){
    ids = length(which(region.list == regions[i.r]))
    n.neurons = ids+n.neurons
  }
  
  gap = dat$time[[id]][2] - dat$time[[id]][1]
  
  plot(0,0,xlim=c(min(dat$time[[id]]),max(dat$time[[id]]))+c(-0.1,0),ylim=c(0,n.neurons+1),col='white',
       xlab='Time (s)', ylab=paste('Neuron/cluster'),cex.lab=2,cex.axis=2,font.lab=2)
  ct=0;
  for(i.r in 1:length(regions)){
    ct0=ct;
    ids=which(region.list == regions[i.r]);
    col.this=regions.col[i.r]
    if(length(ids)>0){
      
      for(i.n in ids){
        spk.tr = numeric(0)
        max.spk = max(dat$spks[[id]][i.n,])
        if(max.spk == 0){
          ct=ct+1
          next;
        } else{
          for(num.spk in 1:max.spk){
            spk.tr = c(spk.tr, rep(which(dat$spks[[id]][i.n,]==num.spk)*gap+dat$time[[id]][1], num.spk))
          }
          ct=ct+1
        }
        if( length(spk.tr)>0 )
          points(x=spk.tr,y=rep(ct, length(spk.tr) ),pch='.',cex=2, col=col.this)
      }
      text(x=dat$trial_intervals[id,1]-0.5,y=(ct+ct0)/2,labels=regions[i.r],col=col.this,cex=3,srt=90)
      
      abline(h=ct+0.5)
      
    }
  }
  
  
  # Time points:
  abline(v=dat$time[[id]][1],col='blue',lty=1,lwd=2)
}



# spike train plot for a single trial
area.intrs = unique(dat$brain_area)
area.col=brewer.pal(n = length(area.intrs), name = "Set1")
# spike train in one trial for both regions 
id = 3 # trial id
options(repr.plot.width=10, repr.plot.height=50)
png(filename = "./Figure/spikeplot.png")
plot.one.spikes(id,area.intrs,area.col,dat)
dev.off()



