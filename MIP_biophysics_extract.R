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

biophys.vars<-c("LW_albedo","SW_albedo","LWnet","SWnet","Qh","Qle","LAI","Evap",
               "lwdown","swdown","Evergreen","Deciduous","Tranp","Grass", "Transp","albedo")

### ED
biophys.index<-which(names(ed) %in% biophys.vars)
ed.biophys<-ed[biophys.index]

ed.UNDERC.mat<-matrix(nrow=length(ed.biophys[[1]][,3]), ncol = length(ed.biophys))

for (i in 1:length(ed.biophys)){
  
  ed.UNDERC.mat[,i]<-ed.biophys[[i]][,3]
  
}
colnames(ed.UNDERC.mat)<-names(ed.biophys)
ed.UNDERC<-as.data.frame(ed.UNDERC.mat)
View(ed.UNDERC)


### ED.lu

ed.lu.biophys.index<-which(names(ed.lu) %in% biophys.vars)
ed.lu.biophys<-ed.lu[ed.lu.biophys.index]

ed.lu.UNDERC.mat<-matrix(nrow=length(ed.lu.biophys[[1]][,3]), ncol = length(ed.lu.biophys))

for (i in 1:length(ed.lu.biophys)){
  
  ed.lu.UNDERC.mat[,i]<-ed.lu.biophys[[i]][,3]
  
}
colnames(ed.lu.UNDERC.mat)<-names(ed.lu.biophys)
ed.lu.UNDERC<-as.data.frame(ed.lu.UNDERC.mat)
View(ed.lu.UNDERC)


## CLM.bgc

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

### lpj.g

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


### LPJ.g

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

### LPJ.w

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

###Linkages

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

### SiBCASA

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