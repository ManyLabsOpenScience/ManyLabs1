# MANYLABS ES-CI OF SITES AND SUMMARY ---------------------------------------------------------------------------------------------------
#
# All ES are converted to Cohen's d
# Procedure 'escalc' from the package 'metafor' will be used to calculate d from means and counts (except study 12b: 'r_to_d' from package 'MAd' )
# Exact CIs computed using 'ci.smd' from package 'MBESS' (except for study 12b: 'ci.sm' is used)
#
# The file 'MLforR.xlsx' is needed, available at the Open Science Framework project pages: https://openscienceframework.org/project/WX7Ck/
# The file 'ML_Original_ES95CI.xls' is needed, available at the Open Science Framework project pages: https://openscienceframework.org/project/WX7Ck/
# (or can be created by running the script: 'Manylabs_OriginalStudiesESCI.R')
#
# Make sure these files are in the working directory!
#
# Scipt produces a file named 'noncentralCI.pdf' with forest plots of ES of the replication sites inclusing summaries and original effects
#
# Note: This script is not optimised for speed or efficiency, but for readability and reproducability
#
# Prepared by F.Hasselman for the Manylabs project https://openscienceframework.org/project/WX7Ck/
# Contact me if you find any errors: me@fredhasselman.com


# Load required packages --------------------------------------------------------------------------------------------------------------
require("MBESS")
require("metafor")
require("MAd")
require("xlsx")
require("gplots")

# Set some variables ------------------------------------------------------------------------------------------------------------------
name <- c(study1="Sunk Costs", study2="Gain vs loss framing", study3a="Anchoring - Babies Born", study3b="Anchoring - Mt. Everest", study3c="Anchoring - Chicago",study3d="Anchoring - Distance to NYC",study4="Retrospective gambler fallacy", study5="Low vs high category scales", study6="Norm of reciprocity", study7="Allowed/Forbidden",study8="Quote Attribution", study9="Flag Priming", study10="Currency Priming", study11="Imagined contact", study12a="Sex differences in implicit math attitudes", study12b="Relations between impl. and expl. math attitudes")

# Number of worksheets to read
nWS    <- 16

# Get the data
df    <- vector("list",length=nWS)
for(i in 1:(nWS)){df[[i]] <- read.xlsx(file="MLforR.xlsx",sheetIndex=(i),header=T,stringsAsFactors=F)}

# These worksheets contain count data
chiIND <- c(2,8,9,10)

# How to organize the columns in order to represent the correct 2x2 tables for count data
tbl <- list(study2 =c(4,2,5,3),
            study8 =c(2,4,3,5),
            study9 =c(4,2,5,3),
            study10=c(5,4,2,3))


dataOut <- vector("list",length=nWS)
names(dataOut) <- names(name[1:nWS])

dfOri <- read.xlsx("ML_Original_ES95CI.xls",sheetName="OriginalES95CI",stringsAsFactors=F)

# Calculate noncentral CIs and produce pdf -----------------------------------------------------------------------------------------------
cnt=0

pdf(paste("noncentralCI.pdf",sep=""),paper="a4r",width=0,height=0)
 
for(nS in 1:nWS){
  # Replication data
  dfR <- df[[nS]]
 
  # CI for large samples is 99%
  snames <- unique(dfR$Site)
  
  ifelse(nS == 16,{CLi <- sapply(1:nrow(dfR), function(i) ifelse(dfR[i,2]>1000,{CLi=.99},{CLi=.95}))
                   # Correlation
                   dfR  <- escalc(measure="COR", ri=dfR[,4], ni=dfR[,2],slab=snames,data=dfR)
                   repCI<- sapply(1:nrow(dfR), function(i) ci.sm(sm=r_to_d(r=dfR[i,10],N=dfR[i,2]),N=dfR[i,2], conf.level=CLi[i])) },{ 
                     ifelse(nS%in%chiIND,{
                       # Count data
                       CLi <- sapply(1:nrow(dfR), function(i) ifelse(dfR[i,2]+dfR[i,3]+dfR[i,4]+dfR[i,5]>1000,{CLi=.99},{CLi=.95}))
                       cnt=cnt+1
                       RowN1 <- dfR[,tbl[[cnt]][1]]+dfR[,tbl[[cnt]][2]]
                       RowN2 <- dfR[,tbl[[cnt]][3]]+dfR[,tbl[[cnt]][4]]
                       dfR <- escalc(measure="OR2D", ai=dfR[,tbl[[cnt]][1]],
                                                     bi=dfR[,tbl[[cnt]][2]],
                                                     ci=dfR[,tbl[[cnt]][3]],
                                                     di=dfR[,tbl[[cnt]][4]],
                                                    n1i=RowN1,
                                                    n2i=RowN2,slab=snames, data=dfR)
                       repCI <- sapply(1:nrow(dfR), function(i) ci.smd(smd=dfR[i,11],n.1=RowN1[i],n.2=RowN2[i],conf.level=CLi[i]))},{
                         # Data with mean and sd 
                         CLi <- sapply(1:nrow(dfR), function(i) ifelse(dfR[i,2]+dfR[i,3]>1000,{CLi=.99},{CLi=.95}))
                         dfR  <- escalc(measure="SMD", m1i=dfR[,5], m2i=dfR[,6], sd1i=dfR[,7], sd2i=dfR[,8], n1i=dfR[,2], n2i=dfR[,3],slab=snames,data=dfR)
                         repCI<- sapply(1:nrow(dfR), function(i) ci.smd(smd=dfR[i,17], n.1=dfR[i,2], n.2=dfR[i,3], conf.level=CLi[i]))}) 
                   })

# Cast and cast and cast, all to get a dataframe with numeric numbers. Yep
repCI <- data.frame(cbind(id1=snames,id2=rep(names(name[nS]),length(snames))),data.matrix(data.frame(t(repCI))) )

# Split overall and summary ES store in id
idX <- grepl(":",repCI$id1)
id  <- which(idX==T)

lims <- c(round(min(c(as.numeric(dfOri$X95.Lower[nS]),repCI[,3]))-.5,digits=1), round(max(c(as.numeric(dfOri$X95.Upper[nS]),repCI[,5]))+.5,digits=1))

  text<-capture.output(print(name[nS]))
  textplot(text)
  title('ManyLabs1: https://osf.io/wx7ck/')

# Forest plot
forest(x=repCI[!idX,4],ci.lb=repCI[!idX,3], ci.ub=repCI[!idX,5], slab=repCI[!idX,1], xlab=expression("Cohen's "*delta),main=paste("Effect Size CI for: ",name[nS],sep=""),efac=1,cex=.9,ylim=c((-1-length(id)),(nrow(repCI)+1)),alim=lims,xlim=lims-c(3,-3))

# Summary
sapply(id, function(i){
  segments(x0=repCI[i,4],y0=(0-i),y1=(nrow(repCI)-1),col="gray60",lwd=1,lty=(1+i))
  addpoly(x=repCI[i,4],ci.lb=repCI[i,3], ci.ub=repCI[i,5],  xlab=expression("Cohen's "*delta),main=dfOri$Study.name[[nS]],mlab=paste(CLi[i]*100,"%CI  ", snames[i],sep=""),efac=.6,cex=.9,rows=(0-i))
} )

# Original
segments(x0=as.numeric(dfOri$d[nS]), y0=(-1-length(id)), y1=(nrow(repCI)-1), col="gray60", lwd=1)
addpoly(x=as.numeric(dfOri$d[nS]),ci.lb=as.numeric(dfOri$X95.Lower[nS]),ci.ub=as.numeric(dfOri$X95.Upper[nS]),rows=(-1-length(id)), mlab=paste(CLi[i]*100,"%CI  ", dfOri$Reference[[nS]],sep=""), efac=.6,cex=.9)

text((lims[2]+2.1),(nrow(dfR)+2),expression("Cohen's "*delta*" [95% CI]"))
text((lims[1]-2.6),(nrow(dfR)+2),expression("Location"))
#dev.off()

dataOut[[nS]] <- repCI
#rm(repCI, dfR, idX, id)
}

dev.off()

# Save the data to an excel file ------------------------------------------------------------------------------------------------------
require("plyr")
d<-ldply(dataOut)
write.xlsx(d,"ML_ESCIfromR.xls",paste("ESCIfromR",sep=""))