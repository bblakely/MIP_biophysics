##Plots to show to modelers pre-meeting

##Rnet
plot(ed.UNDERC.yr$Rnet_calc, type='l', ylim=c(0,100), col='forest green')
#lines(ed.lu.UNDERC.yr$Rnet_calc, type='l', col='green')
lines(clm.bgc.UNDERC.yr$Rnet_calc, type='l', col='blue')
#lines(clm.cn.UNDERC.yr$Rnet_calc, type='l', col='orange')
lines(jules.s.UNDERC.yr$Rnet_calc,col='dark red')
lines(jules.triff.UNDERC.yr$Rnet_calc,col='purple 4')
#H
plot(ed.UNDERC.yr$Qh, type='l', ylim=c(0,150), col='forest green')
#lines(ed.lu.UNDERC.yr$Qh, type='l', col='green')
lines(clm.bgc.UNDERC.yr$Qh, type='l', col='blue')
#lines(clm.cn.UNDERC.yr$Qh, type='l', col='orange')
lines(jules.s.UNDERC.yr$Qh,col='dark red')
lines(jules.triff.UNDERC.yr$Qh,col='purple 4')

#LE
plot(-ed.UNDERC.yr$RadST_calc, type='l',ylim=c(0,400), col='forest green')
lines(-ed.lu.UNDERC.yr$RadST_calc, type='l', col='green')
lines(clm.bgc.UNDERC.yr$RadST_calc, type='l', col='blue')
lines(clm.cn.UNDERC.yr$RadST_calc, type='l', col='orange')
lines(jules.s.UNDERC.yr$RadST_calc,col='dark red')
lines(jules.triff.UNDERC.yr$RadST_calc,col='purple 4')

#ST
plot(-ed.UNDERC.yr$RadST_calc, type='l',ylim=c(0,8), col='forest green')
lines(-ed.lu.UNDERC.yr$RadST_calc, type='l', col='green')
lines(clm.bgc.UNDERC.yr$RadST_calc, type='l', col='blue')
lines(clm.cn.UNDERC.yr$RadST_calc, type='l', col='orange')
lines(jules.s.UNDERC.yr$RadST_calc,col='dark red')
lines(jules.triff.UNDERC.yr$RadST_calc,col='purple 4')


####Decadal
##ED:
dec.rows<-seq(from=1, to=length(ed.UNDERC.yr[,1]), by=10)
ed.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(ed.UNDERC))

for (i in 1:ncol(ed.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
  dec.mean<-mean(ed.UNDERC.yr[(dec:(dec+9)),i])
    ed.UNDERC.dec[v,i]<-dec.mean
  }  
}
View(ed.UNDERC.dec)
colnames(ed.UNDERC.dec)<-colnames(ed.UNDERC)
ed.UNDERC.dec<-as.data.frame(ed.UNDERC.dec)
##ED.lu
dec.rows<-seq(from=1, to=length(ed.lu.UNDERC.yr[,1]), by=10)
ed.lu.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(ed.lu.UNDERC))

for (i in 1:ncol(ed.lu.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
    dec.mean<-mean(ed.lu.UNDERC.yr[(dec:(dec+9)),i])
    ed.lu.UNDERC.dec[v,i]<-dec.mean
  }  
}
colnames(ed.lu.UNDERC.dec)<-colnames(ed.lu.UNDERC)
ed.lu.UNDERC.dec<-as.data.frame(ed.lu.UNDERC.dec)
View(ed.lu.UNDERC.dec)
##CLM.bgc
dec.rows<-seq(from=1, to=length(clm.bgc.UNDERC.yr[,1]), by=10)
clm.bgc.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(clm.bgc.UNDERC))

for (i in 1:ncol(clm.bgc.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
    dec.mean<-mean(clm.bgc.UNDERC.yr[(dec:(dec+9)),i])
    clm.bgc.UNDERC.dec[v,i]<-dec.mean
  }  
}
colnames(clm.bgc.UNDERC.dec)<-colnames(clm.bgc.UNDERC)
clm.bgc.UNDERC.dec<-as.data.frame(clm.bgc.UNDERC.dec)
View(clm.bgc.UNDERC.dec)
##CLM.cn
dec.rows<-seq(from=1, to=length(clm.cn.UNDERC.yr[,1]), by=10)
clm.cn.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(clm.cn.UNDERC))

for (i in 1:ncol(clm.cn.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
    dec.mean<-mean(clm.cn.UNDERC.yr[(dec:(dec+9)),i])
    clm.cn.UNDERC.dec[v,i]<-dec.mean
  }  
}
colnames(clm.cn.UNDERC.dec)<-colnames(clm.cn.UNDERC)
clm.cn.UNDERC.dec<-as.data.frame(clm.cn.UNDERC.dec)
View(clm.cn.UNDERC.dec)

##JULES.s
dec.rows<-seq(from=1, to=length(jules.s.UNDERC.yr[,1]), by=10)
jules.s.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(jules.s.UNDERC))

for (i in 1:ncol(jules.s.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
    dec.mean<-mean(jules.s.UNDERC.yr[(dec:(dec+9)),i])
    jules.s.UNDERC.dec[v,i]<-dec.mean
  }  
}
colnames(jules.s.UNDERC.dec)<-colnames(jules.s.UNDERC)
jules.s.UNDERC.dec<-as.data.frame(jules.s.UNDERC.dec)
View(jules.s.UNDERC.dec)

##JULES.triff
dec.rows<-seq(from=1, to=length(jules.triff.UNDERC.yr[,1]), by=10)
jules.triff.UNDERC.dec<-matrix(nrow=length(dec.rows),ncol=ncol(jules.triff.UNDERC))

for (i in 1:ncol(jules.triff.UNDERC)){
  for(v in 1:length(dec.rows)){
    dec<-dec.rows[v]
    dec.mean<-mean(jules.triff.UNDERC.yr[(dec:(dec+9)),i])
    jules.triff.UNDERC.dec[v,i]<-dec.mean
  }  
}
colnames(jules.triff.UNDERC.dec)<-colnames(jules.triff.UNDERC)
jules.triff.UNDERC.dec<-as.data.frame(jules.triff.UNDERC.dec)
View(jules.triff.UNDERC.dec)


###Plots Decadal
##Rnet
plot(ed.UNDERC.dec$Rnet_calc, type='l', ylim=c(35,85), col='forest green',
     main='Rnet', xlab='Decade',ylab='Rnet, W/m2,' ,lwd=3)
lines(ed.lu.UNDERC.dec$Rnet_calc, type='l', col='light green',lwd=3, lty=3)
lines(clm.bgc.UNDERC.dec$Rnet_calc, type='l', col='blue',lwd=3)
lines(clm.cn.UNDERC.dec$Rnet_calc, type='l', col='cyan',lwd=3, lty = 3)
lines(jules.s.UNDERC.dec$Rnet_calc,col='purple 4',lwd=3)
lines(jules.triff.UNDERC.dec$Rnet_calc,col='dark red', lwd=3, lty=3)
legend(x=0,y=72,legend=c("ED","ED LU","CLM BGC","CLM CN","JULES","JULES TRIFF"), 
       col=c("forest green","green","blue","cyan","purple 4","dark red"), lty=c(1,3,1,3,1,3),
             lwd=3, cex=0.45, text.font=2)

#H
plot(ed.UNDERC.dec$Qh, type='l', ylim=c(0,130), col='forest green',
     main='Sensible Heat', xlab='Decade',ylab='H, W/m2', lwd=3)
lines(ed.lu.UNDERC.dec$Qh, type='l', col='light green', lty=3, lwd=3)
lines(clm.bgc.UNDERC.dec$Qh, type='l', col='blue', lwd=3)
lines(clm.cn.UNDERC.dec$Qh, type='l', col='cyan', lwd=3, lty=3)
lines(jules.s.UNDERC.dec$Qh,col='purple4', lwd=3)
lines(jules.triff.UNDERC.dec$Qh,col='dark red', lwd=3, lty=3)
legend(x=0,y=125,legend=c("ED","ED LU","CLM BGC","CLM CN","JULES","JULES TRIFF"), 
       col=c("forest green","green","blue","cyan","purple 4","dark red"), lty=c(1,3,1,3,1,3),
       lwd=3, cex=0.45, text.font=2)

#LE
plot(-ed.UNDERC.dec$Qle, type='l',ylim=c(0,330), col='forest green',
     main='Latent Heat', xlab='Decade',ylab='LE, W/m2', lwd=3)
lines(-ed.lu.UNDERC.dec$Qle, type='l', col='light green', lwd=3, lty=3)
lines(clm.bgc.UNDERC.dec$Qle, type='l', col='blue', lwd=3)
lines(clm.cn.UNDERC.dec$Qle, type='l', col='cyan', lwd=3, lty=3)
lines(jules.s.UNDERC.dec$Qle,col='purple4', lwd=3)
lines(jules.triff.UNDERC.dec$Qle,col='dark red', lwd=3, lty=3)
legend(x=0,y=270,legend=c("ED","ED LU","CLM BGC","CLM CN","JULES","JULES TRIFF"), 
       col=c("forest green","green","blue","cyan","purple 4","dark red"), lty=c(1,3,1,3,1,3),
       lwd=3, cex=0.45, text.font=2)

#ST
plot(ed.UNDERC.dec$RadST_calc, type='l',ylim=c(2,6), col='forest green',
     main='Radiative Surface Temperature', xlab='decade',ylab='ST,degrees C', lwd=3)
lines(ed.lu.UNDERC.dec$RadST_calc, type='l', col='light green', lwd=3, lty=3)
lines(clm.bgc.UNDERC.dec$RadST_calc, type='l', col='blue', lwd=3)
lines(clm.cn.UNDERC.dec$RadST_calc, type='l', col='cyan', lwd=3, lty=3)
lines(jules.s.UNDERC.dec$RadST_calc,col='purple4', lwd=3)
lines(jules.triff.UNDERC.dec$RadST_calc,col='dark red', lwd=3, lty=3)
legend(x=0,y=3.7,legend=c("ED","ED LU","CLM BGC","CLM CN","JULES","JULES TRIFF"), 
       col=c("forest green","green","blue","cyan","purple 4","dark red"), lty=c(1,3,1,3,1,3),
       lwd=3, cex=0.45, text.font=2)

#Albedo
plot(ed.UNDERC.dec$SW_albedo, type='l',ylim=c(0.1,0.45), col='forest green',
     main='Albedo', xlab='Decade',ylab='Albedo', lwd=3)
lines(ed.lu.UNDERC.dec$SW_albedo, type='l', col='light green', lwd=3, lty=3)
lines(clm.bgc.UNDERC.dec$albedo, type='l', col='blue', lwd=3)
lines(clm.cn.UNDERC.dec$albedo, type='l', col='cyan', lwd=3, lty=3)
lines(jules.s.UNDERC.dec$SW_albedo,col='purple4', lwd=3)
lines(jules.triff.UNDERC.dec$SW_albedo,col='dark red', lwd=3, lty=3)
legend(x=5,y=.36,legend=c("ED","ED LU","CLM BGC","CLM CN","JULES","JULES TRIFF"), 
       col=c("forest green","green","blue","cyan","purple 4","dark red"), lty=c(1,3,1,3,1,3),
       lwd=3, cex=0.45, text.font=2)


###Plots for veg-albedo comparisons

plot(ed.UNDERC.dec$Evergreen,ed.UNDERC.dec$SW_albedo, main='Evergreen vs Albedo')
ed.Alb.eg.lm<-lm(ed.UNDERC.dec$SW_albedo~ed.UNDERC.dec$Evergreen)
abline(.26464,-.03158, col='red')
#legend(.83,.285,legend=c("r2 = 0.24")

plot(ed.UNDERC.dec$Deciduous,ed.UNDERC.dec$SW_albedo, main="Deciduous vs Albedo")
ed.Alb.dc.lm<-lm(ed.UNDERC.dec$SW_albedo~ed.UNDERC.dec$Deciduous)
abline(.296224,-.03158, col='red')
#legend(.1,.285,legend=c("r2 = 0.24"))

#plot(ed.lu.UNDERC.dec$Evergreen,ed.lu.UNDERC.dec$SW_albedo)
#plot(ed.lu.UNDERC.dec$Deciduous,ed.lu.UNDERC.dec$SW_albedo)

plot(clm.bgc.UNDERC.dec$Evergreen,clm.bgc.UNDERC.dec$albedo)
plot(clm.bgc.UNDERC.dec$Deciduous,clm.bgc.UNDERC.dec$albedo)

plot(jules.triff.UNDERC.dec$Evergreen,jules.triff.UNDERC.dec$SW_albedo, main="Evergreen vs Albedo")
ed.alb.eg.lm<-lm(jules.triff.UNDERC.dec$SW_albedo~jules.triff.UNDERC.dec$Evergreen)
abline(0.41709,-0.55573, col='red')

plot(jules.triff.UNDERC.dec$Deciduous,jules.triff.UNDERC.dec$SW_albedo)
ed.alb.dc.lm<-lm(jules.triff.UNDERC.dec$SW_albedo~jules.triff.UNDERC.dec$Deciduous)
abline(0.247129,-0.510677, col='red')

plot(jules.triff.UNDERC.dec$Grass,jules.triff.UNDERC.dec$SW_albedo)
ed.alb.gr.lm<-lm(jules.triff.UNDERC.dec$SW_albedo~jules.triff.UNDERC.dec$Grass)
abline(0.02586,0.33999, col='red')






