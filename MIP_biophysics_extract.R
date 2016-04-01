#To be used after reading in model data with 1_MIP_formatting_ModelLoop_Blakely

names(ed)
names(ed.lu)
names(clm.bgc) 
names(clm.cn)
names(lpj.g) 
names(lpj.w)
names(jules.s)
names(jules.triff)
names(linkages)
names(sib)

library(pracma)

biophys.vars<-c("LW_albedo","SW_albedo","LWnet","SWnet","Qh","Qle","LAI","Evap",
               "lwdown","swdown","Evergreen","Deciduous","Grass", "Transp","albedo")
#--------
### ED
#--------

biophys.index<-which(names(ed) %in% biophys.vars)
ed.biophys<-ed[biophys.index]

ed.UNDERC.mat<-matrix(nrow=length(ed.biophys[[1]][,3]), ncol = length(ed.biophys))

for (i in 1:length(ed.biophys)){
  
  ed.UNDERC.mat[,i]<-ed.biophys[[i]][,3]
  
}
colnames(ed.UNDERC.mat)<-names(ed.biophys)
ed.UNDERC<-as.data.frame(ed.UNDERC.mat)
View(ed.UNDERC)

#Calculate additional variables
#ed is still pretty fucked up
ed.UNDERC$LWnet_calc<-((1-ed.UNDERC$LW_albedo)*ed.UNDERC$lwdown)
ed.UNDERC$LWout_calc<-(ed.UNDERC$lwdown - ed.UNDERC$LWnet_calc) #This only makes sense with a flipped sign
ed.UNDERC$RadST_calc<-((ed.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
ed.UNDERC$SWnet_calc<-(ed.UNDERC$swdown*(1-ed.UNDERC$SW_albedo))
ed.UNDERC$Rnet_calc<-ed.UNDERC$SWnet_calc + ed.UNDERC$LWnet_calc
plot(ed.UNDERC$Rnet_calc,(ed.UNDERC$Qh+ (-ed.UNDERC$Qle)),main='ED energy budget closure', xlab='Rnet', ylab='H+LE')
abline(0,1, col='red', lwd=3)

#--------
### ED.lu
#--------

ed.lu.biophys.index<-which(names(ed.lu) %in% biophys.vars)
ed.lu.biophys<-ed.lu[ed.lu.biophys.index]

ed.lu.UNDERC.mat<-matrix(nrow=length(ed.lu.biophys[[1]][,3]), ncol = length(ed.lu.biophys))

for (i in 1:length(ed.lu.biophys)){
  
  ed.lu.UNDERC.mat[,i]<-ed.lu.biophys[[i]][,3]
  
}
colnames(ed.lu.UNDERC.mat)<-names(ed.lu.biophys)
ed.lu.UNDERC<-as.data.frame(ed.lu.UNDERC.mat)
View(ed.lu.UNDERC)

#Add calculated variables
ed.lu.UNDERC$LWnet_calc<-((1-ed.lu.UNDERC$LW_albedo)*ed.lu.UNDERC$lwdown)
ed.lu.UNDERC$LWout_calc<-(ed.lu.UNDERC$lwdown - ed.lu.UNDERC$LWnet_calc) #This only makes sense with a flipped.lu sign
ed.lu.UNDERC$RadST_calc<-((ed.lu.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
ed.lu.UNDERC$SWnet_calc<-(ed.lu.UNDERC$swdown*(1-ed.lu.UNDERC$SW_albedo))
ed.lu.UNDERC$Rnet_calc<-ed.lu.UNDERC$SWnet_calc + ed.lu.UNDERC$LWnet_calc
plot(ed.lu.UNDERC$Rnet_calc,(ed.lu.UNDERC$Qh+ (-ed.lu.UNDERC$Qle)),main='ED LU energy budget closure', xlab='Rnet', ylab='H+LE')
abline(0,1, col='red', lwd=3)

#--------
## CLM.bgc
#--------
names(clm.bgc)

clm.bgc.biophys.index<-which(names(clm.bgc) %in% biophys.vars)
clm.bgc.biophys<-clm.bgc[clm.bgc.biophys.index]

clm.bgc.UNDERC.mat<-matrix(nrow=length(clm.bgc.biophys[[1]][,3]), ncol = length(clm.bgc.biophys))

for (i in 1:length(clm.bgc.biophys)){
  
  clm.bgc.UNDERC.mat[,i]<-clm.bgc.biophys[[i]][,3]
  
}
colnames(clm.bgc.UNDERC.mat)<-names(clm.bgc.biophys)
clm.bgc.UNDERC<-as.data.frame(clm.bgc.UNDERC.mat)
View(clm.bgc.UNDERC)

#Calculate additional variables
clm.bgc.UNDERC$SWnet_calc<-((1-clm.bgc.UNDERC$albedo)*clm.bgc.UNDERC$swdown)
clm.bgc.UNDERC$LWout_calc<-(clm.bgc.UNDERC$lwdown-(-clm.bgc.UNDERC$LWnet)) #This only makes sense with a flipped sign
clm.bgc.UNDERC$RadST_calc<-((clm.bgc.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
clm.bgc.UNDERC$Rnet_calc<-clm.bgc.UNDERC$SWnet_calc + (-clm.bgc.UNDERC$LWnet)

plot(clm.bgc.UNDERC$Rnet_calc,(clm.bgc.UNDERC$Qh+clm.bgc.UNDERC$Qle), main="CLM BGC energy budget closure",xlab='Rnet',ylab='H+LE')
abline(0,1,col='red', lwd=3)

#--------
## CLM.cn
#--------
names(clm.cn)

clm.cn.biophys.index<-which(names(clm.cn) %in% biophys.vars)
clm.cn.biophys<-clm.cn[clm.cn.biophys.index]

clm.cn.UNDERC.mat<-matrix(nrow=length(clm.cn.biophys[[1]][,3]), ncol = length(clm.cn.biophys))

for (i in 1:length(clm.cn.biophys)){
  
  clm.cn.UNDERC.mat[,i]<-clm.cn.biophys[[i]][,3]
  
}
colnames(clm.cn.UNDERC.mat)<-names(clm.cn.biophys)
clm.cn.UNDERC<-as.data.frame(clm.cn.UNDERC.mat)
View(clm.cn.UNDERC)

#Add calculated variables
clm.cn.UNDERC$SWnet_calc<-((1-clm.cn.UNDERC$albedo)*clm.cn.UNDERC$swdown)
clm.cn.UNDERC$LWout_calc<-(clm.cn.UNDERC$lwdown-(-clm.cn.UNDERC$LWnet)) #This only makes sense with a flipped sign
clm.cn.UNDERC$RadST_calc<-((clm.cn.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
clm.cn.UNDERC$Rnet_calc<-clm.cn.UNDERC$SWnet_calc + (-clm.cn.UNDERC$LWnet)

plot(clm.cn.UNDERC$Rnet_calc,(clm.cn.UNDERC$Qh+clm.cn.UNDERC$Qle), main="CLM CN energy budget closure",xlab='Rnet',ylab='H+LE')
abline(0,1,col='red', lwd=3)

#--------
### lpj.g
#--------
names(lpj.g)

lpj.g.biophys.index<-which(names(clm.cn) %in% biophys.vars)
clm.cn.biophys<-clm.cn[clm.cn.biophys.index]

clm.cn.UNDERC.mat<-matrix(nrow=length(clm.cn.biophys[[1]][,3]), ncol = length(clm.cn.biophys))

for (i in 1:length(clm.cn.biophys)){
  
  clm.cn.UNDERC.mat[,i]<-clm.cn.biophys[[i]][,3]
  
}
colnames(clm.cn.UNDERC.mat)<-names(clm.cn.biophys)
clm.cn.UNDERC<-as.data.frame(clm.cn.UNDERC.mat)
View(clm.cn.UNDERC)

#--------
### LPJ.g
#--------

names(lpj.g)

lpj.g.biophys.index<-which(names(lpj.g) %in% biophys.vars)
lpj.g.biophys<-lpj.g[lpj.g.biophys.index]

lpj.g.UNDERC.mat<-matrix(nrow=length(lpj.g.biophys[[1]][,3]), ncol = length(lpj.g.biophys))

for (i in 1:length(lpj.g.biophys)){
  
  lpj.g.UNDERC.mat[,i]<-lpj.g.biophys[[i]][,3]
  
}
colnames(lpj.g.UNDERC.mat)<-names(lpj.g.biophys)
lpj.g.UNDERC<-as.data.frame(lpj.g.UNDERC.mat)
View(lpj.g.UNDERC)
#--------
### LPJ.w
#--------
names(lpj.w)

lpj.w.biophys.index<-which(names(lpj.w) %in% biophys.vars)
lpj.w.biophys<-lpj.w[lpj.w.biophys.index]

#Evap and Tranp are on monthly timesteps while composition and LAI are on yearly timesteps

names(lpj.w.biophys)

lpj.w.ET<-lpj.w.biophys[2:3]
lpj.w.comp<-lpj.w.biophys[c(1,4:6)]

#Round 1: ET

lpj.w.ET.UNDERC.mat<-matrix(nrow=length(lpj.w.ET[[1]][,3]), ncol = length(lpj.w.ET))
for (i in 1:length(lpj.w.ET)){
  lpj.w.ET.UNDERC.mat[,i]<-lpj.w.ET[[i]][,3]
}
colnames(lpj.w.ET.UNDERC.mat)<-names(lpj.w.ET)
lpj.w.ET.UNDERC<-as.data.frame(lpj.w.ET.UNDERC.mat)
View(lpj.w.ET.UNDERC)

#Round 2: Composition
lpj.w.comp.UNDERC.mat<-matrix(nrow=length(lpj.w.comp[[1]][,3]), ncol = length(lpj.w.comp))
for (i in 1:length(lpj.w.comp)){
  lpj.w.comp.UNDERC.mat[,i]<-lpj.w.comp[[i]][,3]
}
colnames(lpj.w.comp.UNDERC.mat)<-names(lpj.w.comp)
lpj.w.comp.UNDERC<-as.data.frame(lpj.w.comp.UNDERC.mat)
View(lpj.w.comp.UNDERC)

#--------
###JULES
#--------

biophys.index<-which(names(jules.s) %in% biophys.vars)
jules.s.biophys<-jules.s[biophys.index]

jules.s.UNDERC.mat<-matrix(nrow=length(jules.s.biophys[[1]][,3]), ncol = length(jules.s.biophys))

for (i in 1:length(jules.s.biophys)){
  
  jules.s.UNDERC.mat[,i]<-jules.s.biophys[[i]][,3]
  
}
colnames(jules.s.UNDERC.mat)<-names(jules.s.biophys)
jules.s.UNDERC<-as.data.frame(jules.s.UNDERC.mat)
View(jules.s.UNDERC)

#Calculate additional variables
jules.s.UNDERC$LWout_calc<-(jules.s.UNDERC$lwdown-jules.s.UNDERC$LWnet) #This only makes sense with a flipped sign
jules.s.UNDERC$RadST_calc<-((jules.s.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
jules.s.UNDERC$Rnet_calc<-jules.s.UNDERC$SWnet + jules.s.UNDERC$LWnet

plot(jules.s.UNDERC$Rnet_calc,((jules.s.UNDERC$Qh+jules.s.UNDERC$Qle)), main='JULES S energy budget closure',xlab='Rnet', ylab='H+LE') #an /2 is what makes this work!

abline(0,1, col='red', lwd=3)
abline(0,2,col='cyan',lwd=3)

legend(x=-30,y=350,legend=c("1:1","2:1"), col=c("red","cyan"), lwd=3)

#--------
###JULES.triff
#--------
biophys.index<-which(names(jules.triff) %in% biophys.vars)
jules.triff.biophys<-jules.triff[biophys.index]

jules.triff.UNDERC.mat<-matrix(nrow=length(jules.triff.biophys[[1]][,3]), ncol = length(jules.triff.biophys))

for (i in 1:length(jules.triff.biophys)){
  
  jules.triff.UNDERC.mat[,i]<-jules.triff.biophys[[i]][,3]
  
}
colnames(jules.triff.UNDERC.mat)<-names(jules.triff.biophys)
jules.triff.UNDERC<-as.data.frame(jules.triff.UNDERC.mat)
View(jules.triff.UNDERC)

#Calculate additional variables
jules.triff.UNDERC$LWout_calc<-(jules.triff.UNDERC$lwdown-jules.triff.UNDERC$LWnet) #This only makes sense with a flipped sign
jules.triff.UNDERC$RadST_calc<-((jules.triff.UNDERC$LWout_calc/(.99*5.67E-8))^(1/4))-273
jules.triff.UNDERC$Rnet_calc<-jules.triff.UNDERC$SWnet + jules.triff.UNDERC$LWnet

plot(jules.triff.UNDERC$Rnet_calc,((jules.triff.UNDERC$Qh+jules.triff.UNDERC$Qle)), main='JULES TRIFF energy budget closure',xlab='Rnet', ylab='H+LE') #an /2 is what makes this work!

abline(0,1, col='red', lwd=3)
abline(0,5,col='orange',lwd=3)

legend(x=-30,y=1000,legend=c("1:1","5:1"), col=c("red","orange"), lwd=3)


#--------
###Linkages
#--------

names(linkages)

linkages.biophys.index<-which(names(linkages) %in% biophys.vars)
linkages.biophys<-linkages[linkages.biophys.index]

linkages.UNDERC.mat<-matrix(nrow=length(linkages.biophys[[1]][,3]), ncol = length(linkages.biophys))
for (i in 1:length(linkages.biophys)){
  linkages.UNDERC.mat[,i]<-linkages.biophys[[i]][,3]
}
colnames(linkages.UNDERC.mat)<-names(linkages.biophys)
linkages.UNDERC<-as.data.frame(linkages.UNDERC.mat)
View(linkages.UNDERC)

#--------
### SiBCASA
#--------

names(sib)

sib.biophys.index<-which(names(sib) %in% biophys.vars)
sib.biophys<-sib[sib.biophys.index]

sib.UNDERC.mat<-matrix(nrow=length(sib.biophys[[1]][,3]), ncol = length(sib.biophys))
for (i in 1:length(sib.biophys)){
  sib.UNDERC.mat[,i]<-sib.biophys[[i]][,3]
}
colnames(sib.UNDERC.mat)<-names(sib.biophys)
sib.UNDERC<-as.data.frame(sib.UNDERC.mat)
View(sib.UNDERC)
