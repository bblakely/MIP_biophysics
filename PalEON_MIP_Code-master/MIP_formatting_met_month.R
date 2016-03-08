# Run this script after MIP_formatting_ModelLoop.R and MIP_formatting_ModelLoop_Yr.R


models <- c("clm", "ed", "lpj.g", "lpj.w", "jules.s")

# ------------------------------------------------------------
# Monthly Mean Met Drivers (from ED)
# ------------------------------------------------------------
met <- c("lwdown", "swdown", "precipf", "psurf", "qair", "tair", "wind", "CO2")

for(i in 1:length(met)){
	if(i==1){
		tmp <- stack(ed[[met[i]]])
		names(tmp) <- c(met[i], "Site")	
		tmp$Date <- (1:nrow(ed[[met[i]]])-1)/12+850
		tmp <- tmp[,c(2,3,1)]
	} else {
		tmp2 <- stack(ed[[met[i]]])
		names(tmp2) <- c(met[i], "Site")	
		
		tmp <- cbind(tmp, tmp2[,1])
	}
}
names(tmp) <- c("Site", "Date", met)
tmp$Year <- as.numeric(substr(tmp$Date, 1, 4))
tmp$Month <- as.ordered(round((tmp$Date - tmp$Year)*12 +1,0))
tmp <- tmp[,c("Site", "Date", "Year", "Month", met)]
summary(tmp)

write.csv(tmp, "phase1a_output_variables/PalEON_MIP_Drivers_Monthly.csv", row.names=F)


# ------------------------------------------------------------
# Yearly mean ecosystem values
# ------------------------------------------------------------

summary(GPP.y[["PHA"]])

for(i in 1:length(site.list)){
	if(i==1){
		npp <- cbind(stack(NPP.y[[i]]), c(site.list[i]))
		names(npp) <- c("NPP", "Model", "Site")

		agb <- cbind(stack(AGB.y[[i]]), c(site.list[i]))
		names(agb) <- c("AGB", "Model", "Site")

		lai <- cbind(stack(LAI.y[[i]]), c(site.list[i]))
		names(lai) <- c("LAI", "Model", "Site")
	} else {
		npp2 <- cbind(stack(NPP.y[[i]]), c(site.list[i]))
		names(npp2) <- c("NPP", "Model", "Site")
		npp <- rbind(npp, npp2)

		agb2 <- cbind(stack(AGB.y[[i]]), c(site.list[i]))
		names(agb2) <- c("AGB", "Model", "Site")
		agb <- rbind(agb, agb2)

		lai2 <- cbind(stack(LAI.y[[i]]), c(site.list[i]))
		names(lai2) <- c("LAI", "Model", "Site")
		lai <- rbind(lai, lai2)
	}
}
npp$Year <- c(850:2010)
agb$Year <- c(850:2010)
lai$Year <- c(850:2010)
summary(npp)
summary(agb)
summary(npp)

tmp <- cbind(npp, agb$AGB, lai$LAI)
names(tmp) <- c(names(npp), "AGB", "LAI")
tmp <- tmp[,c("Year", "Model", "Site", "NPP", "AGB", "LAI")]
summary(tmp)

write.csv(tmp, "phase1a_output_variables/PalEON_MIP_AGB_LAI_NPP_Yearly.csv", row.names=F)

# ------------------------------------------------------------
