#############################################################################
##  fetchDemoData_aprioriClimate.R: collates genet, climate, and crowding  ##
##  data into single R dataframes for vital rates statistical models.      ##
##  Climate data are the a priori defined covariates.                      ## 
#############################################################################

# Created by ATT on July 20, 2016 (with liberal copying from Britta's import code)
# Updated by ... on .............

# !!! SET WORKING DIRECTORY TO SOURCE FILE LOCATION !!! #

# Clear the workspace
rm(list=ls(all.names = TRUE))



fetchDemoData_aprioriClimate <- function(doSpp, state, grow=FALSE, surv=FALSE, recr=FALSE, dataDir) {
  
  #### TODO: CHECK WITH BRITTA AND PETER ABOUT THIS ###
  #Out-dated W approach
  if(state=="Kansas" & doSpp=="BOHI" | state=="Kansas" & doSpp=="SCSC"){
    alphaList <- c(0.3558,.01137)
    midRings <- c(seq(1,19,2),seq(22.5,47.5,5),seq(55,145,10))
  }
  
  
  ####  Climate data
  climDat <- read.csv(paste0(dataDir,"/climateData/aprioriClimate.csv"))
  
  ####  Genet data
  if(grow==TRUE){
    # load growth data
    nonCompLength.g=5; # Number of columns in SppData that are not measures of competitors
    if(state=="Idaho"| state=="Montana"){groDfile=paste(dataDir,"/speciesData/",doSpp,"/growDnoNA.csv",sep="")}else{
      groDfile=paste(dataDir,"/speciesData/",doSpp,"/growDnoNA_reduced.csv",sep="") 
    }
    groD=read.csv(file=groDfile)
    D=groD[groD$allEdge==0,]; 
    #D$year=as.factor(D$year)
    D$logarea.t0=log(D$area.t0)
    D$logarea.t1=log(D$area.t1)
    D$quad=as.character(D$quad)
    D=D[order(D$X),]
    
    ################# drop seedlings 
    #e <- (D$logarea.t0>0)
    #D <- D[e,];
    
    ##########################################################
    # Read in data on neighbors 
    ##########################################################
    ringD <- read.csv(paste(dataDir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
    ringD$year<-as.factor(ringD$year)
    
    # merge D with ringD (D contains fewer rows)
    D<-merge(D,ringD,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
    D=D[order(D$X),]
    rm(ringD)
    row.names(D) <- NULL  
    
    ## pull out annulus data for the focal species  
    sppCols=which(substr(names(D),1,4)==doSpp); 
    sppData<-data.frame(logarea.t1=D$logarea.t1,quad=as.factor(D$quad),year=D$year,ID=D$trackID,age=D$age,logarea.t0=D$logarea.t0,Group=as.factor(D$Group),as.matrix(D[,sppCols]))
    #colnames(sppData)<-c("logarea.t1","quad","year","age","logarea.t0","Group", colnames(sppData)[(1+nonCompLength.g):length(colnames(sppData))])

  } #import growth data
  
  if(surv==TRUE){
    
    nonCompLength.s=5 #Number of columns in SppData that are not measures of competitors 
    if(state=="Idaho"| state=="Montana"){survDfile=paste(dataDir,"/speciesData/",doSpp,"/survD.csv",sep="")}else{
      survDfile=paste(dataDir,"/speciesData/",doSpp,"/survD_reduced.csv",sep="") 
    }
    survD=read.csv(file=survDfile)
    D=survD[survD$allEdge==0,];
    D$year=D$year
    D$logarea=log(D$area)
    D$quad=as.character(D$quad)
    D<-D[order(D$X),];
    
    #Drop seedlings
    #e <- (D$logarea>0)
    #D <- D[e,];
    
    #########read data on neighbors
    ringD <- read.csv(paste(dataDir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
    ringD$year<-ringD$year
    
    # merge D with ringD (D contains fewer rows)
    D<-merge(D,ringD,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
    D=D[order(D$X),]
    rm(ringD)
    row.names(D) <- NULL  
    
    ## pull out annulus data for the focal species  
    sppCols=which(substr(names(D),1,4)==doSpp); 
    sppData<-data.frame(survives=D$survives,age=D$age,ID=D$trackID, year=D$year, logarea=D$logarea, Group=as.factor(D$Group), quad=D$quad, as.matrix(D[,sppCols]))
    ### Change this so sppData includes the response 
    #colnames(sppData)<-c("survives","age","ID","trackID","year","logarea","Group","quad",colnames(sppData)[(1+nonCompLength.s):length(colnames(sppData))])
  } #Import survival data
  
  if(recr==TRUE){
    
    # get recruitment data
    if(state=="Idaho"| state=="Montana"){infile1=paste(dataDir,"/speciesData/",doSpp,"/recArea.csv",sep="")}else{
      infile1=paste(dataDir,"/speciesData/",doSpp,"/recArea_reduced.csv",sep="") 
    }
    tmpD=read.csv(infile1)
    if(length(grep("Group", colnames(tmpD))) == 0){tmpD$Group <- substr(tmpD$quad, 1, 1)}
    tmpD=tmpD[,c("quad","year","NRquad","totParea","Group","recArea")]
    names(tmpD)[3]="R"
    names(tmpD)[4]="cov"
    D=tmpD
    D[is.na(D)]=0  # replace missing values 
    
    # calculate mean cover by group and year
    tmpD1=D[,c("quad","year","Group","cov")]
    tmpD1=aggregate(tmpD1$cov,by=list("year"=tmpD1$year,
                                      "Group"=tmpD1$Group),FUN=mean)
    names(tmpD1)[3]="Gcov"
    
    D=merge(D,tmpD1,all.x=T)
    D$y=D$R; 
    D$parents1=D$cov; D$parents2=D$Gcov; 
    #year=as.numeric(as.factor(D$year))
    D$fyear = factor(D$year); 
    #Nyrs=length(unique(D$year))
    #Group=factor(D$Group)
    #Ngroups=length(unique(Group))
    intraD<-D
    
  } #Import recruitment data
  
  ####  Compute Intraspecific Crowding
  if(recr==FALSE){ #if this is growth or survival, compute W otherwise, don't.
    intraD<-sppData  # focal spp and intraspecific neighbors
    intraD<-data.frame(intraD)  
    
    if(state=="Kansas" & doSpp=="BOHI" | state=="Kansas" & doSpp=="SCSC"){
      sppNum <- if(doSpp=="BOHI"){1}else{2} 
      alpha <- alphaList[sppNum]; 
      dist_wts <- exp(-alpha*midRings); 
      
    }else{
      dists <- read.csv(paste(dataDir,"/speciesData/",state,"DistanceWeights.csv",sep="")); 
      dist_wts<- dists[,paste0(doSpp)]
    }
    
    C <- data.matrix(intraD[,grep(paste(doSpp),names(intraD),value=T)]) #matrix of conspecific areas in the annuli 
    W <- C%*%dist_wts; 
    
    if(grow==TRUE){
      dataG<-data.frame(intraD[,c("logarea.t1","year","ID","age","logarea.t0","Group","quad")], W)
      dataG$Group <- as.factor(dataG$Group);
      data<-dataG
    }
    
    if(surv==TRUE){
      dataS<-data.frame(intraD[,c("survives","year","ID","logarea","age","Group","quad")], W)
      dataS$Group <- as.factor(dataS$Group);
      data<-dataS
    }#end survival T/F
  }# W import for surv and grow T/F merge
  
  if(recr==TRUE){ #for recruitment, compute true parents
    dataR<-data.frame(intraD)
    dataR$Group <- as.factor(dataR$Group);
    data<-dataR
  } # and merge
  
  ####  Merge Climate and Genet data
  if(nchar(as.character(data[1,"year"]) != 4)) { 
    data$year <- data$year+1900 # make year match the climate dataframe
  }
  outDat <- merge(data, climDat, by="year")
  
} # fetchDemoData_aprioriClimate
