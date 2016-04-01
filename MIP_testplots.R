#First pass check on realism
#Important questions:
#Does Rn = H + LE roughly close
#Do shortwave, longwave budgets close
#Is vegetation dynamic and realistic?
#Is there a relationship between vegetation change and H and LE
#Is there a relationship between veg change and albedo? <- still need to do this for ED
#Do LAI changes match vegetataion changes


#last 200 yr
yr200<-c(11532:13932)

yr200.an<-c((1161-200):1161)

###ED
#--------
#Aggregate to year
#--------
yr.rows<-seq(from=1, to=length(ed.UNDERC[,1]), by=12)
ed.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(ed.UNDERC))

for (i in 1:ncol(ed.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(ed.UNDERC[(yr:(yr+11)),i])
    ed.UNDERC.yr[v,i]<-yr.mean
  }  
}

View(ed.UNDERC.yr)
colnames(ed.UNDERC.yr)<-colnames(ed.UNDERC)
ed.UNDERC.yr<-as.data.frame(ed.UNDERC.yr)
#--------
#Plots
#--------
#Does Rn = H + LE roughly close? No. Why is LE negative?
plot(ed.UNDERC.yr$Qh, type='l')
plot(ed.UNDERC.yr$Qle, type='l')
plot(ed.UNDERC.yr$Qh-ed.UNDERC.yr$Qle, ed.UNDERC.yr$SWnet+ed.UNDERC.yr$LWnet, xlab="H+LE", ylab="Rnet", main="E budget closure")
plot(ed.UNDERC.yr$Qh-ed.UNDERC.yr$Qle, ed.UNDERC.yr$swdown+ed.UNDERC.yr$lwdown, xlab="H+LE", ylab="Rnet", main="E budget closure, down")

plot(ed.UNDERC.yr$SWnet[yr200.an]+ed.UNDERC.yr$LWnet[yr200.an], type='l', col='orange', ylim=c(-150,560), ylab="W/m2", main="E balance parts")
lines(-ed.UNDERC.yr$Qle[yr200.an], col='blue', type='l')
lines(ed.UNDERC.yr$Qh[yr200.an], col='red')


#Do shortwave, longwave budgets close? No, why are SWnet and swdown the same?
#This is confounded by the fact that SWnet and swdown are the same
plot(ed.UNDERC.yr$SWnet,ed.UNDERC.yr$swdown*(1-ed.UNDERC.yr$SW_albedo))
abline(0,1,col='red')

#Is vegetation dynamic and realistic? Not particularly. Prediciting strong EG dominance
plot(ed.UNDERC.yr$Evergreen, type='l', main="Evergreen")
plot(ed.UNDERC.yr$Deciduous, type='l', main="Deciduous")
plot(ed.UNDERC.yr$Grass, type='l', main="Grass")

#plot(ed.UNDERC.yr$Evergreen+ed.UNDERC.yr$Deciduous+ed.UNDERC.yr$Grass, type='l')


#Is there a relationship between veg change and H or LE. Not really. LE increases with increasing EG dominance; 
#would expect opposite.H increases with EG dominance too, which is more realistic. Perhaps more SWnet.Works fairly nicely if LE is negative-d

plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(ed.UNDERC.yr$Evap[yr200.an], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")
lines(ed.UNDERC.yr$Transp[yr200.an], col='light green')
#par(new=TRUE)
#plot(approx(-ed.UNDERC.yr.yr$Qle[yr200.an],n=200), col='blue', axes='', type='l', lty=2)


plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(-ed.UNDERC.yr$Qle[yr200.an], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1))
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(ed.UNDERC.yr$Qh[yr200.an], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? Maybe. Albedo increases with the spike in decids then declines as evergreens retur.Also albedo is really low.
plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot((ed.UNDERC.yr$SW_albedo[yr200.an]), col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")



#Does LAI match vegetation changes? Not really, too high. Decreases with shift to EG's though
plot(ed.UNDERC.yr$LAI, type='l')

plot(ed.UNDERC.yr$LAI[yr200.an], type='l', main="Veg change and LAI", ylab="LAI")
par(new=TRUE)
plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")

#MEGAPLOT
#plot(approx(ed.UNDERC.yr$LAI[yr200.an],n=200), type='l', main="ALLVARS", axes='')
#par(new=TRUE)
plot(ed.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(ed.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(ed.UNDERC.yr$SWnet[yr200.an]+ed.UNDERC.yr$LWnet[yr200.an], type='l', col='yellow', ylim=c(-150,300), lty=2, ylab="W/m2", main="ALL")
lines(-ed.UNDERC.yr$Qle[yr200.an], col='blue', type='l', lty=2)
lines(ed.UNDERC.yr$Qh[yr200.an], col='red', lty=2)
legend(x=1600,y=350,legend=c("Decid","EG","Rnet","LE","H"),col=c("orange", "forest green","yellow","blue","red"), lty=c(1,1,2,2,2), cex=0.8)


#--------
### ed.lu
#--------
##Aggregate by year
#--------
yr.rows<-seq(from=1, to=length(ed.lu.UNDERC[,1]), by=12)
ed.lu.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(ed.lu.UNDERC))

for (i in 1:ncol(ed.lu.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(ed.lu.UNDERC[(yr:(yr+11)),i])
    ed.lu.UNDERC.yr[v,i]<-yr.mean
  }  
}


colnames(ed.lu.UNDERC.yr)<-colnames(ed.lu.UNDERC)
ed.lu.UNDERC.yr<-as.data.frame(ed.lu.UNDERC.yr)
View(ed.lu.UNDERC.yr)
#--------
##Plots
#--------
#Does Rn = H + LE roughly close? No. Why is LE negative?
plot(ed.lu.UNDERC.yr$Qh, type='l')
plot(ed.lu.UNDERC.yr$Qle, type='l')
plot(ed.lu.UNDERC.yr$Qh-ed.lu.UNDERC.yr$Qle, ed.lu.UNDERC.yr$SWnet+ed.lu.UNDERC.yr$LWnet, xlab="H+LE", ylab="Rnet", main="E budget closure")
plot(ed.lu.UNDERC.yr$Qh-ed.lu.UNDERC.yr$Qle, ed.lu.UNDERC.yr$swdown+ed.lu.UNDERC.yr$lwdown, xlab="H+LE", ylab="Rnet", main="E budget closure, down")

plot(ed.lu.UNDERC.yr$SWnet[yr200.an]+ed.lu.UNDERC.yr$LWnet[yr200.an], type='l', col='orange', ylim=c(-150,560), ylab="W/m2", main="E balance parts")
lines(-ed.lu.UNDERC.yr$Qle[yr200.an], col='blue', type='l')
lines(ed.lu.UNDERC.yr$Qh[yr200.an], col='red')


#Do shortwave, longwave budgets close? No, why are SWnet and swdown the same?
#This is confounded by the fact that SWnet and swdown are the same
plot(ed.lu.UNDERC.yr$SWnet,ed.lu.UNDERC.yr$swdown*(1-ed.lu.UNDERC.yr$SW_albedo))
abline(0,1,col='red')

#Is vegetation dynamic and realistic? Not particularly. Prediciting strong EG dominance
plot(ed.lu.UNDERC.yr$Evergreen, type='l', main="Evergreen")
plot(ed.lu.UNDERC.yr$Deciduous, type='l', main="Deciduous")
plot(ed.lu.UNDERC.yr$Grass, type='l', main="Grass")

#plot(ed.lu.UNDERC.yr$Evergreen+ed.lu.UNDERC.yr$Deciduous+ed.lu.UNDERC.yr$Grass, type='l')


#Is there a relationship between veg change and H or LE. Not really. LE increases with increasing EG dominance; 
#would expect opposite.H increases with EG dominance too, which is more realistic. Perhaps more SWnet.Works fairly nicely if LE is negative-d

plot(ed.lu.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.lu.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(ed.lu.UNDERC.yr$Evap[yr200.an], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")
lines(ed.lu.UNDERC.yr$Transp[yr200.an], col='light green')
#par(new=TRUE)
#plot(approx(-ed.lu.UNDERC.yr.yr$Qle[yr200.an],n=200), col='blue', axes='', type='l', lty=2)


plot(ed.lu.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.lu.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(-ed.lu.UNDERC.yr$Qle[yr200.an], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(ed.lu.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.lu.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(ed.lu.UNDERC.yr$Qh[yr200.an], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? Maybe. Albedo increases with the spike in decids then declines as evergreens retur.Also albedo is really low.
plot(ed.lu.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.lu.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot((ed.lu.UNDERC.yr$SW_albedo[yr200.an]), col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")



#Does LAI match vegetation changes? Not really, too high. Decreases with shift to EG's though
plot(ed.lu.UNDERC.yr$LAI, type='l')

plot(ed.lu.UNDERC.yr$LAI[yr200.an], type='l', main="Veg change and LAI", ylab="LAI")
par(new=TRUE)
plot(ed.lu.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(ed.lu.UNDERC.yr$Evergreen[yr200.an], col="forest green")


#--------
###CLM.bgc
#--------
##Aggregate to Year
#--------
yr.rows<-seq(from=1, to=length(clm.bgc.UNDERC[,1]), by=12)
clm.bgc.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(clm.bgc.UNDERC))

for (i in 1:ncol(clm.bgc.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(clm.bgc.UNDERC[(yr:(yr+11)),i])
    clm.bgc.UNDERC.yr[v,i]<-yr.mean
  }  
}

colnames(clm.bgc.UNDERC.yr)<-colnames(clm.bgc.UNDERC)
clm.bgc.UNDERC.yr<-as.data.frame(clm.bgc.UNDERC.yr)
View(clm.bgc.UNDERC.yr)
#--------
##Plots
#--------

#Does Rn = H + LE roughly close? No.Better than ED though; ratio is closeish but theres a bias toward Rnet
plot(clm.bgc.UNDERC.yr$Qh, type='l')
plot(clm.bgc.UNDERC.yr$Qle, type='l')
plot(clm.bgc.UNDERC.yr$Qh+clm.bgc.UNDERC.yr$Qle, (clm.bgc.UNDERC.yr$swdown*(1-(clm.bgc.UNDERC.yr$albedo)) +clm.bgc.UNDERC.yr$LWnet), xlab="H+LE", ylab="Rnet", main="E budget closure")
abline(0,1,col='red')

plot(clm.bgc.UNDERC.yr$swdown[yr200.an]*(1-clm.bgc.UNDERC.yr$albedo[yr200.an])+clm.bgc.UNDERC.yr$LWnet[yr200.an], type='l', col='orange', ylim=c(-100,300), ylab="W/m2", main="E balance parts")
lines(-clm.bgc.UNDERC.yr$Qle[yr200.an], col='blue', type='l')
lines(clm.bgc.UNDERC.yr$Qh[yr200.an], col='red')

#Do shortwave, longwave budgets close? Not enough parts to answer this question; no SWnet.


#Is vegetation dynamic and realistic? YES.Mostly; changes ar more subtle than you would expect.
plot(clm.bgc.UNDERC.yr$Evergreen, type='l', main="Evergreen")
plot(clm.bgc.UNDERC.yr$Deciduous, type='l', main="Deciduous")
plot(clm.bgc.UNDERC.yr$Grass, type='l', main="Grass")

plot(clm.bgc.UNDERC.yr$Evergreen+clm.bgc.UNDERC.yr$Deciduous+clm.bgc.UNDERC.yr$Grass, type='l')

plot(clm.bgc.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(.4,.6))
lines(clm.bgc.UNDERC.yr$Evergreen[yr200.an], col="forest green")


#Is there a relationship between veg change and H or LE? No. H and LE change a lot without much change in Veg
plot(clm.bgc.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC.yr$Evap[yr200.an], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")
#lines(approx(clm.bgc.UNDERC.yr$Transp[yr200.an],n=200), col='light green')
#par(new=TRUE)
#plot(approx(-clm.bgc.UNDERC.yr$Qle[yr200.an],n=200), col='blue', axes='', type='l', lty=2)


plot(clm.bgc.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(-clm.bgc.UNDERC.yr$Qle[yr200.an], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(clm.bgc.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC.yr$Qh[yr200.an], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? Nope. 

plot(clm.bgc.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC.yr$albedo[yr200.an], col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")

#-------
###CLM.cn
#--------
yr.rows<-seq(from=1, to=length(clm.cn.UNDERC[,1]), by=12)
clm.cn.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(clm.cn.UNDERC))

for (i in 1:ncol(clm.cn.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(clm.cn.UNDERC[(yr:(yr+11)),i])
    clm.cn.UNDERC.yr[v,i]<-yr.mean
  }  
}

colnames(clm.cn.UNDERC.yr)<-colnames(clm.cn.UNDERC)
clm.cn.UNDERC.yr<-as.data.frame(clm.cn.UNDERC.yr)
View(clm.cn.UNDERC.yr)

#--------
###JULES
#--------
##Aggregate to year
#--------
yr.rows<-seq(from=1, to=length(jules.s.UNDERC[,1]), by=12)
jules.s.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(jules.s.UNDERC))

for (i in 1:ncol(jules.s.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(jules.s.UNDERC[(yr:(yr+11)),i])
    jules.s.UNDERC.yr[v,i]<-yr.mean
  }  
}

colnames(jules.s.UNDERC.yr)<-colnames(jules.s.UNDERC)
jules.s.UNDERC.yr<-as.data.frame(jules.s.UNDERC.yr)
View(jules.s.UNDERC.yr)

#--------
##Plots
#--------
#Does Rn = H + LE roughly close? No Shortwave is way low
plot(jules.s.UNDERC.yr$Qh, type='l')
plot(jules.s.UNDERC.yr$Qle, type='l')
plot(jules.s.UNDERC.yr$Qh+jules.s.UNDERC.yr$Qle, ((jules.s.UNDERC.yr$SWnet) + jules.s.UNDERC.yr$LWnet), xlab="H+LE", ylab="Rnet", main="E budget closure")
abline(0,1,col='red')

plot(jules.s.UNDERC.yr$SWnet[yr200.an]+jules.s.UNDERC.yr$LWnet[yr200.an], type='l', col='orange', ylim=c(-100,300), ylab="W/m2", main="E balance parts")
lines(jules.s.UNDERC.yr$Qle[yr200.an], col='blue', type='l')
lines(jules.s.UNDERC.yr$Qh[yr200.an], col='red')

#Do shortwave, longwave budgets close? YES for shortwave. No albedo for longwave.
plot(jules.s.UNDERC.yr$SWnet[yr200.an], ((jules.s.UNDERC.yr$swdown[yr200.an])*(1-jules.s.UNDERC.yr$SW_albedo[yr200.an])))
abline(0,1, col='red')


#Is vegetation dynamic and realistic? No dynamic vegetation!
plot(jules.s.UNDERC.yr$Evergreen, type='l', main="Evergreen")
plot(jules.s.UNDERC.yr$Deciduous, type='l', main="Deciduous")
plot(jules.s.UNDERC.yr$Grass, type='l', main="Grass")

plot(jules.s.UNDERC.yr$Evergreen+jules.s.UNDERC.yr$Deciduous+jules.s.UNDERC.yr$Grass, type='l')

plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1))
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")


#Is there a relationship between veg change and H or LE? No dynamic vegetation!

plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(-jules.s.UNDERC.yr$Qle[yr200.an], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.s.UNDERC.yr$Qh[yr200.an], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? No dynamic veg. 

plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.s.UNDERC.yr$SW_albedo[yr200.an], col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")


#Does LAI match vegetation changes? No dynamic veg, no dynamic LAI
plot(jules.s.UNDERC.yr$LAI, type='l')

plot(jules.s.UNDERC.yr$LAI[yr200.an], type='l', main="Veg change and LAI", ylab="LAI")
par(new=TRUE)
plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")

#Does ET match veg changes? No dynamic veg! Also no Transp.
plot(jules.s.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.s.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.s.UNDERC.yr$Evap[yr200.an], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")

#--------
###JULES_triffid
#--------
##Aggregate to year
#--------
yr.rows<-seq(from=1, to=length(jules.triff.UNDERC[,1]), by=12)
jules.triff.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(jules.triff.UNDERC))

for (i in 1:ncol(jules.triff.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(jules.triff.UNDERC[(yr:(yr+11)),i])
    jules.triff.UNDERC.yr[v,i]<-yr.mean
  }  
}

colnames(jules.triff.UNDERC.yr)<-colnames(jules.triff.UNDERC)
jules.triff.UNDERC.yr<-as.data.frame(jules.triff.UNDERC.yr)
View(jules.triff.UNDERC.yr)
#--------
##Plots
#--------
#Does Rn = H + LE roughly close? No Shortwave is way low
plot(jules.triff.UNDERC.yr$Qh, type='l')
plot(jules.triff.UNDERC.yr$Qle, type='l')
plot(jules.triff.UNDERC.yr$Qh+jules.triff.UNDERC.yr$Qle, ((jules.triff.UNDERC.yr$SWnet) + jules.triff.UNDERC.yr$LWnet), xlab="H+LE", ylab="Rnet", main="E budget closure")
abline(0,1,col='red')

plot(jules.triff.UNDERC.yr$SWnet[yr200.an]+jules.triff.UNDERC.yr$LWnet[yr200.an], type='l', col='orange', ylim=c(-100,300), ylab="W/m2", main="E balance parts")
lines(jules.triff.UNDERC.yr$Qle[yr200.an], col='blue', type='l')
lines(jules.triff.UNDERC.yr$Qh[yr200.an], col='red')

#Do shortwave, longwave budgets close? YES for shortwave. No albedo for longwave.
plot(jules.triff.UNDERC.yr$SWnet[yr200.an], ((jules.triff.UNDERC.yr$swdown[yr200.an])*(1-jules.triff.UNDERC.yr$SW_albedo[yr200.an])))
abline(0,1, col='red')


#Is vegetation dynamic and realistic? No dynamic vegetation!
plot(jules.triff.UNDERC.yr$Evergreen, type='l', main="Evergreen")
plot(jules.triff.UNDERC.yr$Deciduous, type='l', main="Deciduous")
plot(jules.triff.UNDERC.yr$Grass, type='l', main="Grass")

plot(jules.triff.UNDERC.yr$Evergreen+jules.triff.UNDERC.yr$Deciduous+jules.triff.UNDERC.yr$Grass, type='l')

plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1))
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")


#Is there a relationship between veg change and H or LE? No dynamic vegetation!

plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(-jules.triff.UNDERC.yr$Qle[yr200.an], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.triff.UNDERC.yr$Qh[yr200.an], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? No dynamic veg. 

plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.triff.UNDERC.yr$SW_albedo[yr200.an], col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")


#Does LAI match vegetation changes? No dynamic veg, no dynamic LAI
plot(jules.triff.UNDERC.yr$LAI, type='l')

plot(jules.triff.UNDERC.yr$LAI[yr200.an], type='l', main="Veg change and LAI", ylab="LAI")
par(new=TRUE)
plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")

#Does ET match veg changes? No dynamic veg! Also no Transp.
plot(jules.triff.UNDERC.yr$Deciduous[yr200.an], type='l', col='orange', ylim=c(0,1), axes='')
lines(jules.triff.UNDERC.yr$Evergreen[yr200.an], col="forest green")
par(new=TRUE)
plot(jules.triff.UNDERC.yr$Evap[yr200.an], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")


