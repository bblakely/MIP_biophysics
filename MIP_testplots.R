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



###ED
yr.rows<-seq(from=1, to=length(ed.UNDERC[,1]), by=12)
ed.UNDERC.yr<-matrix(nrow=length(yr.rows),ncol=ncol(ed.UNDERC))

for (i in 1:ncol(ed.UNDERC)){
  for(v in 1:length(yr.rows)){
    yr<-yr.rows[v]
    yr.mean<-mean(ed.UNDERC[(yr:yr+11),i])
    ed.UNDERC.yr[v,i]<-yr.mean
  }  
}

View(ed.UNDERC.yr)
colnames(ed.UNDERC.yr)<-colnames(ed.UNDERC)
ed.UNDERC.yr<-as.data.frame(ed.UNDERC.yr)

#Does Rn = H + LE roughly close? No. Why is LE negative?
plot(ed.UNDERC$Qh)
plot(ed.UNDERC$Qle)
plot(ed.UNDERC$Qh+ed.UNDERC$Qle, ed.UNDERC$SWnet+ed.UNDERC$LWnet, xlab="H+LE", ylab="Rnet", main="E budget closure")
plot(ed.UNDERC$Qh+ed.UNDERC$Qle, ed.UNDERC$swdown+ed.UNDERC$lwdown, xlab="H+LE", ylab="Rnet", main="E budget closure, down")

plot(ed.UNDERC$SWnet[yr200]+ed.UNDERC$LWnet[yr200], type='l', col='orange', ylim=c(-150,560), ylab="W/m2", main="E balance parts")
lines(-ed.UNDERC$Qle[yr200], col='blue', type='l')
lines(ed.UNDERC$Qh[yr200], col='red')


#Do shortwave, longwave budgets close? No, why are SWnet and swdown the same?
#This is confounded by the fact that SWnet and swdown are the same
plot(ed.UNDERC$SWnet,ed.UNDERC$swdown*(1-ed.UNDERC$SW_albedo))
abline(0,1,col='red')

#Is vegetation dynamic and realistic? Not particularly. Prediciting strong EG dominance
plot(ed.UNDERC$Evergreen, type='l', main="Evergreen")
plot(ed.UNDERC$Deciduous, type='l', main="Deciduous")
plot(ed.UNDERC$Grass, type='l', main="Grass")

#plot(ed.UNDERC$Evergreen+ed.UNDERC$Deciduous+ed.UNDERC$Grass, type='l')

#Is there a relationship between veg change and H or LE. Not really. LE increases with increasing EG dominance; 
#would expect opposite.H increases with EG dominance too, which is more realistic. Perhaps more SWnet.Works fairly nicely if LE is negative-d

plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(ed.UNDERC$Evap[yr200], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")
lines(ed.UNDERC$Transp[yr200], col='light green')
#par(new=TRUE)
#plot(approx(-ed.UNDERC$Qle[yr200],n=200), col='blue', axes='', type='l', lty=2)


plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(-ed.UNDERC$Qle[yr200], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1))
lines(ed.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(ed.UNDERC$Qh[yr200], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? Maybe. Albedo increases with the spike in decids then declines as evergreens retur.Also albedo is really low.
plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1), axes='')
lines(ed.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot((ed.UNDERC$SW_albedo[yr200]), col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")



#Does LAI match vegetation changes? Not really, too high. Decreases with shift to EG's though
plot(ed.UNDERC$LAI, type='l')

plot(ed.UNDERC$LAI[yr200], type='l', main="Veg change and LAI", ylab="LAI")
par(new=TRUE)
plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(ed.UNDERC$Evergreen[yr200], col="forest green")

#MEGAPLOT
#plot(approx(ed.UNDERC$LAI[yr200],n=200), type='l', main="ALLVARS", axes='')
#par(new=TRUE)
plot(ed.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(0,1), main="Veg change and LAI", axes='')
lines(ed.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(ed.UNDERC$SWnet[yr200]+ed.UNDERC$LWnet[yr200], type='l', col='yellow', ylim=c(-150,560), lty=2, ylab="W/m2", main="ALL")
lines(-ed.UNDERC$Qle[yr200], col='blue', type='l', lty=2)
lines(ed.UNDERC$Qh[yr200], col='red', lty=2)
legend(x=1600,y=350,legend=c("Decid","EG","Rnet","LE","H"),col=c("orange", "forest green","yellow","blue","red"), lty=c(1,1,2,2,2), cex=0.8)


### ed.lu

####Will come back to this one; not much difference from regular ed.




###CLM.bgc

#Does Rn = H + LE roughly close? No.Better than ED though; ratio is closeish but theres a bias toward Rnet
plot(clm.bgc.UNDERC$Qh, type='l')
plot(clm.bgc.UNDERC$Qle, type='l')
plot(clm.bgc.UNDERC$Qh+clm.bgc.UNDERC$Qle, (clm.bgc.UNDERC$swdown*(1-(clm.bgc.UNDERC$albedo)) +clm.bgc.UNDERC$LWnet), xlab="H+LE", ylab="Rnet", main="E budget closure")
abline(0,1,col='red')

plot(clm.bgc.UNDERC$swdown[yr200]*(1-clm.bgc.UNDERC$albedo[yr200])+clm.bgc.UNDERC$LWnet[yr200], type='l', col='orange', ylim=c(-150,560), ylab="W/m2", main="E balance parts")
lines(-clm.bgc.UNDERC$Qle[yr200], col='blue', type='l')
lines(clm.bgc.UNDERC$Qh[yr200], col='red')

#Do shortwave, longwave budgets close? No, why are SWnet and swdown the same? Not enough parts to answer this question; no SWnet.


#Is vegetation dynamic and realistic? YES.Mostly; changes ar more subtle than you would expect.
plot(clm.bgc.UNDERC$Evergreen, type='l', main="Evergreen")
plot(clm.bgc.UNDERC$Deciduous, type='l', main="Deciduous")
plot(clm.bgc.UNDERC$Grass, type='l', main="Grass")

plot(clm.bgc.UNDERC$Evergreen+clm.bgc.UNDERC$Deciduous+clm.bgc.UNDERC$Grass, type='l')

plot(clm.bgc.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(.4,.6))
lines(clm.bgc.UNDERC$Evergreen[yr200], col="forest green")


#Is there a relationship between veg change and H or LE? No. H and LE change a lot without much change in Veg
plot(clm.bgc.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC$Evap[yr200], col='light blue', type='l', main="Veg change and ET", ylab="ET (kg/m2/s")
#lines(approx(clm.bgc.UNDERC$Transp[yr200],n=200), col='light green')
#par(new=TRUE)
#plot(approx(-clm.bgc.UNDERC$Qle[yr200],n=200), col='blue', axes='', type='l', lty=2)


plot(clm.bgc.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(-clm.bgc.UNDERC$Qle[yr200], col='blue', type='l', main="Veg change and LE", ylab="LE (W/m2)")

plot(clm.bgc.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC$Qh[yr200], col='red', type='l', main="Veg change and H")

#Is there a relationship between veg change and albedo? Nope. Albedo changes a lot and veg does not. Albedo drops (???) around deforestation time. An inverse would work.That's what's plotted here.

plot(clm.bgc.UNDERC$Deciduous[yr200], type='l', col='orange', ylim=c(.4,.6), axes='')
lines(clm.bgc.UNDERC$Evergreen[yr200], col="forest green")
par(new=TRUE)
plot(clm.bgc.UNDERC$albedo[yr200], col='dark gray', type='l', main="Veg change and albedo", ylab="albedo")


