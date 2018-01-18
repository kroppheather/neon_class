library(plyr)
library(lubridate)
#first get the data directories
Folder<-c("net_rad","precip","rel_hum","shf","soil_temp","temp_bio")
FolderDF <- data.frame(FolderName=Folder,Folder=seq(1,length(Folder)))

#get the directories
Fdir <- list()
for(i in 1:length(Folder)){
	Fdir[[i]] <- data.frame(fileFolder=as.character(list.dirs(paste0("c:/Users/hkropp/Google Drive/ES_ecology/neon_raw/all_data/",Folder[i]))),
			folder=rep(i,length(list.dirs(paste0("c:/Users/hkropp/Google Drive/ES_ecology/neon_raw/all_data/",Folder[i])))),
			f.count=seq(1,length(list.dirs(paste0("c:/Users/hkropp/Google Drive/ES_ecology/neon_raw/all_data/",Folder[i])))))
}


#turn into a data frame with folder id
FolderA <- ldply(Fdir,data.frame)
FolderA <- FolderA[FolderA$f.count!=1,]
for(i in 1:dim(FolderA)[1]){
	FolderA$site[i]<-strsplit(as.character(FolderA$fileFolder[i]),"\\.")[[1]][3]
	FolderA$month[i]<-strsplit(as.character(FolderA$fileFolder[i]),"\\.")[[1]][7]
}


#now get files in folders
Files<- list()
for(i in 1:dim(FolderA)[1]){
	Files[[i]] <- data.frame(files=list.files(as.character(FolderA$fileFolder[i])), 
					file.n=rep(i, length(list.files(as.character(FolderA$fileFolder[i])))),
					folder=rep(FolderA$folder[i], length(list.files(as.character(FolderA$fileFolder[i])))),
					f.count=rep(FolderA$f.count[i], length(list.files(as.character(FolderA$fileFolder[i])))),
					site=rep(FolderA$site[i], length(list.files(as.character(FolderA$fileFolder[i])))))

}

FilesA <- ldply(Files,data.frame)
#first subset by csv fiels



#pull out time increment, file type, and for csv: need spatial and vertical codes
FilesA$basicID<- grepl("basic",as.character(FilesA$files))
FilesA$sensorID<- grepl("sensor_positions",as.character(FilesA$files))

FilesB <- FilesA[FilesA$basicID==TRUE,]
FilesS <- FilesA[FilesA$sensorID==TRUE,]

#7=spatial var, 8= vertical var, 9=time
for(i in 1:dim(FilesB)[1]){
	FilesB$spaceI[i] <-strsplit(as.character(FilesB$files[i]),"\\.")[[1]][7]
	FilesB$vertI[i] <-strsplit(as.character(FilesB$files[i]),"\\.")[[1]][8]
	FilesB$timeI[i] <-strsplit(as.character(FilesB$files[i]),"\\.")[[1]][9]
}

Files30 <- FilesB[FilesB$timeI=="030",]

#add the folder to the data frame

FilestoRead <- join(Files30, FolderA, by=c("folder","f.count","site"), type="left")

#read in data
dataList<- list()
for(i in 1:dim(FilestoRead)[1]){
	dataList[[i]] <- read.csv(paste0(FilestoRead$fileFolder[i],"/",FilestoRead$files[i]))

}
#folder 1
#inSWMean,outSWMean, inLWMean,outLWMean

#folder 2
#secPrecipBulk

#folder 3
#RHMean,tempRHMean

#folder 4
#SHFMean

#folder 5
#bioTempMean

#folder 6 
#soilTempMean

#note names of acutal data type are switched on folders 5 and 6

FilestoRead$DataID <- seq(1, dim(FilestoRead)[1])

NRID <- FilestoRead$DataID[FilestoRead$folder==1]
PRID <- FilestoRead$DataID[FilestoRead$folder==2]
RHID <- FilestoRead$DataID[FilestoRead$folder==3]
SHFID <-FilestoRead$DataID[FilestoRead$folder==4]
BIOID <-FilestoRead$DataID[FilestoRead$folder==5]
SOILID <- FilestoRead$DataID[FilestoRead$folder==6]

NRInfo <- FilestoRead[NRID,]
NRList <- list()
for(i in 1:length(NRID)){
	NRList[[i]] <- dataList[[NRID[i]]]
	NRList[[i]]$Date <- as.Date(NRList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	NRList[[i]]$doy <- yday(NRList[[i]]$Date)
	NRList[[i]]$year <- year(NRList[[i]]$Date)
	NRList[[i]]$vertI <- rep(NRInfo$vertI[i], dim(NRList[[i]])[1])
	NRList[[i]]$spaceI <- rep(NRInfo$spaceI[i], dim(NRList[[i]])[1])
	NRList[[i]]$site <- rep(NRInfo$site[i], dim(NRList[[i]])[1])
}	
NRtoagg <- ldply(NRList,data.frame)

#aggregate each type
#folder 1
#inSWMean,outSWMean, inLWMean,outLWMean

	SWI<- aggregate(NRtoagg$inSWMean,
		by=list(NRtoagg$doy,NRtoagg$year,NRtoagg$vertI,
				NRtoagg$spaceI,NRtoagg$site),
		FUN="mean", na.action=na.omit)	
	colnames(SWI) <- c("doy","year","vertI","spaceI","site","SWI")

	SWO<- aggregate(NRtoagg$outSWMean, 
	by=list(NRtoagg$doy,NRtoagg$year,NRtoagg$vertI,
				NRtoagg$spaceI,NRtoagg$site),FUN="mean", na.action=na.omit)
	colnames(SWO) <- c("doy","year","vertI","spaceI","site","SWO")

	LWI<- aggregate(NRtoagg$inLWMean,
	by=list(NRtoagg$doy,NRtoagg$year,NRtoagg$vertI,
				NRtoagg$spaceI,NRtoagg$site),FUN="mean", na.action=na.omit)
	colnames(LWI) <- c("doy","year","vertI","spaceI","site","LWI")

	LWO<- aggregate(NRtoagg$outLWMean, 
	by=list(NRtoagg$doy,NRtoagg$year,NRtoagg$vertI,
				NRtoagg$spaceI,NRtoagg$site),FUN="mean", na.action=na.omit)
	colnames(LWO) <- c("doy","year","vertI","spaceI","site","LWO")


netRL <- list(SWI,SWO,LWI,LWO)
netRdf <- join_all(netRL, by=c("doy","year","site","vertI","spaceI"),type="inner")
#get unique list of sites

#only take measurements from the top of the tower
netRdf2 <-netRdf[netRdf$vertI!="000",]
NRinfo2 <- unique(data.frame(site=netRdf2$site,vertI=netRdf2$vertI,spaceI=netRdf2$spaceI))
#subset each into site
NRsiteL <- list()

for(i in 1:dim(NRinfo2)[1]){
	NRsiteL[[i]] <- netRdf2[netRdf2$site==NRinfo2$site[i],]
	write.table(NRsiteL[[i]], 
				paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\neon_out\\netR",NRinfo2$site[i],".csv"),
				sep=",", row.names=FALSE)
}

#organize the RH data
RHInfo <- FilestoRead[RHID,]
RHList <- list()
for(i in 1:length(RHID)){
	RHList[[i]] <- dataList[[RHID[i]]]
	RHList[[i]]$Date <- as.Date(RHList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	RHList[[i]]$doy <- yday(RHList[[i]]$Date)
	RHList[[i]]$year <- year(RHList[[i]]$Date)
	RHList[[i]]$vertI <- rep(RHInfo$vertI[i], dim(RHList[[i]])[1])
	RHList[[i]]$spaceI <- rep(RHInfo$spaceI[i], dim(RHList[[i]])[1])
	RHList[[i]]$site <- rep(RHInfo$site[i], dim(RHList[[i]])[1])
}	

RHtoagg <- ldply(RHList,data.frame)

	RH<- aggregate(RHtoagg$RHMean,
		by=list(RHtoagg$doy,RHtoagg$year,RHtoagg$vertI,
				RHtoagg$spaceI,RHtoagg$site),
		FUN="mean", na.action=na.omit)	
	colnames(RH) <- c("doy","year","vertI","spaceI","site","RH")
	
	RHT<- aggregate(RHtoagg$tempRHMean,
		by=list(RHtoagg$doy,RHtoagg$year,RHtoagg$vertI,
				RHtoagg$spaceI,RHtoagg$site),
		FUN="mean", na.action=na.omit)	
	colnames(RHT) <- c("doy","year","vertI","spaceI","site","RHT")

RHL <- list(RH,RHT)
RHdf <- join_all(RHL, by=c("doy","year","site","vertI","spaceI"),type="inner")	
RHdf2 <-RHdf[RHdf$vertI!="000",]
RHinfo2 <- unique(data.frame(site=RHdf2$site,vertI=RHdf2$vertI,spaceI=RHdf2$spaceI))
#subset each into site	


#organize the shf data
SHFInfo <- FilestoRead[SHFID,]
SHFList <- list()
for(i in 1:length(SHFID)){
	SHFList[[i]] <- dataList[[SHFID[i]]]
	SHFList[[i]]$Date <- as.Date(SHFList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	SHFList[[i]]$doy <- yday(SHFList[[i]]$Date)
	SHFList[[i]]$year <- year(SHFList[[i]]$Date)
	SHFList[[i]]$vertI <- rep(SHFInfo$vertI[i], dim(SHFList[[i]])[1])
	SHFList[[i]]$spaceI <- rep(SHFInfo$spaceI[i], dim(SHFList[[i]])[1])
	SHFList[[i]]$site <- rep(SHFInfo$site[i], dim(SHFList[[i]])[1])
}
#SHFMean

SHFtoagg <- ldply(SHFList,data.frame)
#average across soil plots
	SHF<- aggregate(SHFtoagg$SHFMean,
		by=list(SHFtoagg$doy,SHFtoagg$year,SHFtoagg$vertI,
				SHFtoagg$site),
		FUN="mean", na.action=na.omit)	
	colnames(SHF) <- c("doy","year","vertI","site","SHF")

#all at depth of 0.08m

	
#organize the bio data
BIOInfo <- FilestoRead[BIOID,]
BIOList <- list()
for(i in 1:length(BIOID)){
	BIOList[[i]] <- dataList[[BIOID[i]]]
	BIOList[[i]]$Date <- as.Date(BIOList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	BIOList[[i]]$doy <- yday(BIOList[[i]]$Date)
	BIOList[[i]]$year <- year(BIOList[[i]]$Date)
	BIOList[[i]]$vertI <- rep(BIOInfo$vertI[i], dim(BIOList[[i]])[1])
	BIOList[[i]]$spaceI <- rep(BIOInfo$spaceI[i], dim(BIOList[[i]])[1])
	BIOList[[i]]$site <- rep(BIOInfo$site[i], dim(BIOList[[i]])[1])
}

BIOtoagg <- ldply(BIOList,data.frame)
 	BIOT<- aggregate(BIOtoagg$bioTempMean,
		by=list(BIOtoagg$doy,BIOtoagg$year,BIOtoagg$vertI,
				BIOtoagg$spaceI,BIOtoagg$site),
		FUN="mean", na.action=na.omit)	
	colnames(BIOT) <- c("doy","year","vertI","spaceI","site","BIOT")	
	
BIOinfo2 <- unique(data.frame(site=BIOT$site,vertI=BIOT$vertI,spaceI=BIOT$spaceI))

BIOT0 <- BIOT[BIOT$vertI=="000",]
BIOTC <- rbind(BIOT[BIOT$vertI=="020"&BIOT$site!="BART",],
			BIOT[BIOT$vertI=="040",])
colnames(BIOTC)[6]<- "BIOTC"			
colnames(BIOT0)[6]<- "BIOT0"

BIOTC <- data.frame(BIOTC[,1:2],BIOTC[,5:6])
BIOT0 <- data.frame(BIOT0[,1:2],BIOT0[,5:6])	
#join together
BIOtemps <- join(BIOT0,BIOTC, by=c("doy","year","site"),type="full")


#soil temperature
SOILInfo <- FilestoRead[SOILID,]
SOILList <- list()
for(i in 1:length(SOILID)){
	SOILList[[i]] <- dataList[[SOILID[i]]]
	SOILList[[i]]$Date <- as.Date(SOILList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	SOILList[[i]]$doy <- yday(SOILList[[i]]$Date)
	SOILList[[i]]$year <- year(SOILList[[i]]$Date)
	SOILList[[i]]$vertI <- rep(SOILInfo$vertI[i], dim(SOILList[[i]])[1])
	SOILList[[i]]$spaceI <- rep(SOILInfo$spaceI[i], dim(SOILList[[i]])[1])
	SOILList[[i]]$site <- rep(SOILInfo$site[i], dim(SOILList[[i]])[1])
}
#just take uppermost sensor that is 1-11cm depth
SOILtoagg <- ldply(SOILList,data.frame)
SOILtoagg <- SOILtoagg[SOILtoagg$vertI=="501",]
#average across plots
 	SOILT<- aggregate(SOILtoagg$soilTempMean,
		by=list(SOILtoagg$doy,SOILtoagg$year,
				SOILtoagg$site),
		FUN="mean", na.action=na.omit)	
		
	colnames(SOILT) <- c("doy","year","site","SOILT")	

#precip units in mm
PRInfo <- FilestoRead[PRID,]
PRList <- list()
for(i in 1:length(PRID)){
	PRList[[i]] <- dataList[[PRID[i]]]
	PRList[[i]]$Date <- as.Date(PRList[[i]]$endDateTime, "%Y-%m-%dT%H:%M:%SZ")
	PRList[[i]]$doy <- yday(PRList[[i]]$Date)
	PRList[[i]]$year <- year(PRList[[i]]$Date)
	PRList[[i]]$vertI <- rep(PRInfo$vertI[i], dim(PRList[[i]])[1])
	PRList[[i]]$spaceI <- rep(PRInfo$spaceI[i], dim(PRList[[i]])[1])
	PRList[[i]]$site <- rep(PRInfo$site[i], dim(PRList[[i]])[1])
}

PRtoagg <- ldply(PRList,data.frame)	
PRtoagg <- PRtoagg[PRtoagg$spaceI=="000"|PRtoagg$spaceI=="900",]
#sum up daily
 	PR<- aggregate(PRtoagg$secPrecipBulk,
		by=list(PRtoagg$doy,PRtoagg$year,
				PRtoagg$site),
		FUN="sum" )	
		
		
#precip data doesn't look trustworthy

#combine data so that each site has all data together
#data frames include	

Lall<- list(netRdf2,SHF ,RHdf2,BIOtemps,SOILT)

DFall <- join_all(Lall, by=c("doy","year","site"),type="full")
Sites <- unique(DFall$site)

DFList<- list()
for(i in 1:length(Sites)){
	DFList[[i]] <- DFall[DFall$site==Sites[i],]
	write.table(DFList[[i]], 
				paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\neon_out\\allData",Sites[i],".csv"),
				sep=",", row.names=FALSE)
}	
#make plots of each dataset
for(i in 1:length(Sites)){
	for(j in 1:10){
	jpeg(paste0("c:\\Users\\hkropp\\Google Drive\\ES_ecology\\neon_raw\\neon_out\\plotCheck\\",
				Sites[i],colnames(DFList[[i]])[j+5],".jpg"),
				, width=700, height=500, units="px", quality=100)
	if(length(which(is.na(DFList[[i]][,j+5])==FALSE))!=0){
	plot(DFList[[i]]$doy, DFList[[i]][,j+5], type="b", pch=19, lwd=2,
	xlab=" DOY 2017", ylab=paste(colnames(DFList[[i]])[j+5]),
	main=paste(Sites[i]))
	dev.off()
	}else{
		plot(c(0,1),c(0,1), type="n",xlab=" DOY 2017", 
			ylab=paste(colnames(DFList[[i]])[j+5]),
		main=paste(Sites[i]))
		text(0.5,0.5, "No data",cex=2)
		}
	}
}

head(DFall)

	