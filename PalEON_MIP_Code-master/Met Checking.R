# ----------------------------------------------------
# Met Checking to figure out what's going on
# Christine R. Rollinson
# crollinson@gmail.com 
# original: 18 Jan 2015
# updated: 21 April 2015
# ----------------------------------------------------

library(ncdf4)
library(ggplot2)
library(grid)
library(nlme)
library(car)

met.paleon <- "~/Desktop/phase1a_met_drivers_v4"
met.ed <- "~/Desktop/ED_Met_drivers/met_v4.2"
setwd("../")
#outputs <- "phase1a_output_variables"
years <- 850:2010

vars.paleon <- c("lwdown", "precipf", "psurf", "qair", "swdown", "tair", "wind")
vars.ed <- recode(vars.paleon, "'lwdown'='dlwrf'; 'precipf'='prate'; 'psurf'='pres'; 'qair'='sh'; 'swdown'='vbdsf'; 'tair'='tmp'; 'wind'='ugrd' ")


dir.lwdown.paleon <- dir(file.path(met.paleon, "PHA", "lwdown"))
dir.precipf.paleon <- dir(file.path(met.paleon, "PHA", "precipf"))
dir.psurf.paleon <- dir(file.path(met.paleon, "PHA", "psurf"))
dir.qair.paleon <- dir(file.path(met.paleon, "PHA", "qair"))
dir.swdown.paleon <- dir(file.path(met.paleon, "PHA", "swdown"))
dir.tair.paleon <- dir(file.path(met.paleon, "PHA", "tair"))
dir.wind.paleon <- dir(file.path(met.paleon, "PHA", "wind"))


dir.lwdown.ed <- dir(file.path(met.ed, "PHA", "dlwrf"))
dir.precipf.ed <- dir(file.path(met.ed, "PHA", "prate"))
dir.psurf.ed <- dir(file.path(met.ed, "PHA", "pres"))
dir.qair.ed <- dir(file.path(met.ed, "PHA", "sh"))
dir.swdown.ed <- dir(file.path(met.ed, "PHA", "vbdsf"))
dir.tair.ed <- dir(file.path(met.ed, "PHA", "tmp"))
dir.wind.ed <- dir(file.path(met.ed, "PHA", "ugrd"))

lwdown.paleon <- precipf.paleon <- psurf.paleon <- qair.paleon <- swdown.paleon <- tair.paleon <- wind.paleon <- vector()

lwdown.ed <- precipf.ed <- psurf.ed <- qair.ed <- swdown.ed <- tair.ed <- wind.ed <- vector()

year.start <- 1500
year.end <- 1900


# ------------------------------------
# Graphing lwdown
# ------------------------------------
# Paleon Drivers
dir.range <- which(as.numeric(substr(dir.lwdown.paleon, 12, 15))>=year.start & as.numeric(substr(dir.lwdown.paleon, 12, 15))<=year.end)
length(dir.range)

for(i in 1:length(dir.range)){
	ncT <- nc_open(file.path(met.paleon, "PHA", "lwdown", dir.lwdown.paleon[dir.range[i]]))
	lwdown.paleon <- c(lwdown.paleon, ncvar_get(ncT, "lwdown"))	
	nc_close(ncT)
}
plot(lwdown.paleon, type="l", lwd=0.25)


# ED Drivers
dir.range.ed <- which(as.numeric(substr(dir.lwdown.ed, 11, 14))>=(year.start+1000) & as.numeric(substr(dir.lwdown.ed, 11, 14))<=(year.end+1000))
length(dir.range.ed)

for(i in 1:length(dir.range.ed)){
	ncT <- nc_open(file.path(met.ed, "PHA", "dlwrf", dir.lwdown.ed[dir.range.ed[i]]))
	lwdown.ed <- c(lwdown.ed, ncvar_get(ncT, "dlwrf"))	
	nc_close(ncT)
}
plot(lwdown.ed, type="l", lwd=0.25)

# ------------------------------------

# ------------------------------------
# Graphing swdown
# ------------------------------------
# Paleon Drivers
dir.range <- which(as.numeric(substr(dir.swdown.paleon, 12, 15))>=year.start & as.numeric(substr(dir.swdown.paleon, 12, 15))<=year.end)
length(dir.range)

for(i in 1:length(dir.range)){
	ncT <- nc_open(file.path(met.paleon, "PHA", "swdown", dir.swdown.paleon[dir.range[i]]))
	swdown.paleon <- c(swdown.paleon, ncvar_get(ncT, "swdown"))	
	nc_close(ncT)
}
plot(swdown.paleon, type="l", lwd=0.25)


# ED Drivers
dir.range.ed <- which(as.numeric(substr(dir.swdown.ed, 11, 14))>=(year.start+1000) & as.numeric(substr(dir.swdown.ed, 11, 14))<=(year.end+1000))
length(dir.range.ed)

for(i in 1:length(dir.range.ed)){
	ncT <- nc_open(file.path(met.ed, "PHA", "vbdsf", dir.swdown.ed[dir.range.ed[i]]))
	swdown.ed <- c(swdown.ed, ncvar_get(ncT, "vbdsf"))	
	nc_close(ncT)
}
plot(swdown.ed, type="l", lwd=0.25)

# ------------------------------------

# ------------------------------------
# Graphing tair
# ------------------------------------
# Paleon Drivers
dir.range <- which(as.numeric(substr(dir.tair.paleon, 10, 13))>=year.start & as.numeric(substr(dir.tair.paleon, 10, 13))<=year.end)
length(dir.range)

for(i in 1:length(dir.range)){
	ncT <- nc_open(file.path(met.paleon, "PHA", "tair", dir.tair.paleon[dir.range[i]]))
	tair.paleon <- c(tair.paleon, ncvar_get(ncT, "tair"))	
	nc_close(ncT)
}
plot(tair.paleon, type="l", lwd=0.25)


# ED Drivers
dir.range.ed <- which(as.numeric(substr(dir.tair.ed, 9, 12))>=(year.start+1000) & as.numeric(substr(dir.tair.ed, 9, 12))<=(year.end+1000))
length(dir.range.ed)

for(i in 1:length(dir.range.ed)){
	ncT <- nc_open(file.path(met.ed, "PHA", "tmp", dir.tair.ed[dir.range.ed[i]]))
	tair.ed <- c(tair.ed, ncvar_get(ncT, "tmp"))	
	nc_close(ncT)
}
plot(tair.ed, type="l", lwd=0.25)

# ------------------------------------

# Met Checking
# temp <- nc_open(file.path(outputs, "Temp.annual.nc"))
# precip <- nc_open(file.path(outputs, "Precip.annual.nc"))

df1 <- read.csv(file.path(outputs, "MIP_Data_Ann_NACP2015.csv"))
summary(df1)

temp.overview <- df1[df1$Model=="ed2",]
summary(temp.overview)

for(s in unique(temp.overview$Site)){
	for(i in (min(temp.overview$Year)+5):(max(temp.overview$Year)-5)){
		temp.overview[temp.overview$Site==s & temp.overview$Year == i, "Temp.10smooth"] <- mean(temp.overview[temp.overview$Site==s & temp.overview$Year>=(i-5) & temp.overview$Year<=(i+5), "Temp"])
}}

plot(Temp.10smooth ~ Year, data=temp.overview[temp.overview$Site=="PHA",], type="l")