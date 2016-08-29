data<-read.csv("SP500 and sector returns.csv")
sector.data<-data[,3:ncol(data)]

#load returns into for later
sector.returns<-sector.data
SP.returns<- data.frame(data[,2])

#set window length, and periodocity
windowlength=62
periodicity=5

for(i in 1:(nrow(sector.data)-windowlength+1))
{
  if(i==1)
  {
    cov1<-cov(sector.data[i:(i+windowlength)-1,])
    contribution<-cov1%*%rep(1,ncol(sector.data))
    inverse.contribution<-1/contribution
    inverse.contribution<-inverse.contribution/sum(inverse.contribution)
    weights<-t(as.matrix(inverse.contribution))
    
    weights<-t(as.matrix(apply(weights,2,max,0))) #set min weight to 0
    weights<-weights/sum(weights)
    weights<-as.matrix(apply(weights,2,min,0.25)) #set max weight to 0.25
    weights<-t(weights)
    weightstemp<-weights
    
  }
  else{
    if(i%%periodicity==0)
    {
      cov1<-cov(sector.data[i:(i+windowlength)-1,])
      contribution<-cov1%*%rep(1,ncol(sector.data))
      inverse.contribution<-1/contribution
      inverse.contribution<-inverse.contribution/sum(inverse.contribution)
      
      weightstemp<-t(as.matrix(inverse.contribution))
      weightstemp<-t(as.matrix(apply(weightstemp,2,mkax,0))) #set min weight to 0
      weightstemp<-weightstemp/sum(weightstemp)
      weightstemp<-t(as.matrix(apply(weightstemp,2,min,0.25))) #set max weight to 0.25
      #weightstemp<-t(weightstemp)
      
      
      weights<-rbind(weights,weightstemp)
    }
    else
    {
      weights<-rbind(weights,weightstemp)
    }
  }
}
write.csv(weights,"SP500 equal contribtion weights 5 days.csv")

InvRisk.returns <- weights[1:(nrow(weights)-1),] %*% t(as.matrix(sector.returns[(windowlength+1):nrow(sector.returns),]))
InvRisk.returns <- as.matrix(diag(InvRisk.returns))
SP.returns.relevant <- as.matrix(SP.returns[(windowlength+1):nrow(SP.returns),])

SP.index <- as.matrix(cumprod(1+SP.returns.relevant))
InvRisk.index <- as.matrix(cumprod(1+InvRisk.returns))
Ratio <- InvRisk.index / SP.index

plot(InvRisk.index, type = 'l' , col = 'red')
lines(SP.index , type = 'l', col = 'blue')
lines(Ratio , type ='l' , col = 'black')