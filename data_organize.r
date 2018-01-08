
#folder names

fln <- c("netradZip","precipZip","relhumZip","shfZip","tempbioZip","tempsoilZip")
flnO <-c("net_rad","precip","rel_hum","shf","soil_temp","temp_bio","temp_soil")

#get file names
zipF <- list()
uzName <- list()
for(i in 1:length(fln)){
	zipF[[i]] <- list.files(paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\zip\\",fln[i]))
	uzName [[i]] <- gsub(".zip","",zipF[[i]])
}
#unzip files
for(i in 1:length(fln)){
	for(j in 1:length(zipF[[i]])){
	unzip(paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\zip\\",fln[i],"\\",zipF[[i]][j]),
		overwrite=FALSE,
		exdir=paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\all_data\\",flnO[i],"\\", uzName[[i]][j]))
	}
}
