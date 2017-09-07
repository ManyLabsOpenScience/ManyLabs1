# MANYLABS HETEROGENEITY ANALYSIS ------------------------------------------------------------------------------------------------------
#
# Heterogeneity and meta analysis using package 'metafor'
#
# Produces a file named 'heterogeneity.pdf' with meta-analysis output, and several plots
# Ignore the warnings, they are produced by capturing the output
#
# The file 'MLforR.xlsx' is needed, available at the Open Science Framework project pages: https://openscienceframework.org/project/WX7Ck/
#
# Note: This script is not optimised for speed or efficiency, but for readability and reproducability
#
# Prepared by F.Hasselman for the Manylabs project https://openscienceframework.org/project/WX7Ck/
# Contact me if you find any errors: me@fredhasselman.com

# Load required packages --------------------------------------------------------------------------------------------------------------
require("gplots")
require("metafor")
require("xlsx")


# Set some variables ------------------------------------------------------------------------------------------------------------------
name <- c(study1="Sunk Costs", study2="Gain vs loss framing", study3a="Anchoring - Babies Born", study3b="Anchoring - Mt. Everest", study3c="Anchoring - Chicago",study3d="Anchoring - Distance to NYC",study4="Retrospective gambler fallacy", study5="Low vs high category scales", study6="Norm of reciprocity", study7="Allowed/Forbidden",study8="Quote Attribution", study9="Flag Priming", study10="Currency Priming", study11="Imagined contact", study12a="Sex differences in implicit math attitudes", study12b="Relations between impl. and expl. math attitudes")

# Number of worksheets to read
nWS    <- 16

# Get the data
df     <- vector("list",length=nWS)
for(i in 1:(nWS)){df[[i]] <- read.xlsx(file="MLforR.xlsx",sheetIndex=(i),header=T,stringsAsFactors=F)}

# These worksheets contain count data
chiIND <- c(2,8,9,10)

# How to organize the columns in order to represent the correct 2x2 tables for count data
tbl <- list(study2 =c(4,2,5,3),
            study8 =c(2,4,3,5),
            study9 =c(4,2,5,3),
            study10=c(5,4,2,3)) # Corrected


# Conduct meta-analysis and produce pdf -----------------------------------------------------------------------------------------------
cnt=0
pdf(paste("heterogeneity.pdf",sep=""),paper="a4r",width=0,height=0)

for(i in 1:(nWS)){
  dfma <- df[[i]]

  # Remove overall and summary ES
  idX  <- grepl(":",dfma$Site)
  dfma <- dfma[!idX,]
  snames <- unique(dfma$Site)

  # Perform meta-analysis depending on sheetindex (= study)
  ifelse(i == 16,{
    sma <- rma(measure="COR", ri=dfma[,4], ni=dfma[,2],slab=snames,data=dfma)},{
      ifelse(i%in%chiIND,{
        cnt=cnt+1
        RowN1 <- dfma[,tbl[[cnt]][1]]+dfma[,tbl[[cnt]][2]]
        RowN2 <- dfma[,tbl[[cnt]][3]]+dfma[,tbl[[cnt]][4]]
        sma  <- rma(measure="OR2D", ai=dfma[,tbl[[cnt]][1]], bi=dfma[,tbl[[cnt]][2]], ci=dfma[,tbl[[cnt]][3]], di=dfma[,tbl[[cnt]][4]],
                    n1i=RowN1,n2i=RowN2,slab=snames,data=dfma)},{
          sma  <- rma(measure="SMD", m1i=dfma[,5], m2i=dfma[,6], sd1i=dfma[,7], sd2i=dfma[,8], n1i=dfma[,2], n2i=dfma[,3],slab=snames,data=dfma) })
    })

  # Print output to pdf
  text<-capture.output(print(sma))
  textplot(text)
  title(paste("ManyLabs1: https://osf.io/wx7ck/ \n\n Output of Random Effects model for ",name[i],sep=""))

  # Forest plot
  forest.rma(sma,slab=dfma$Site,main=paste("Random Effects model for ",name[i],sep=""))

  # Funnel plot
  funnel(sma,addtau2=F,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),refline=0, yaxis="seinv", main=paste("Funnel plot\n RE model for ",name[i],"\n dotted line = ES estimate",sep=""), xlab="Observed Outcome\n (Areas around ES=0 indicate pseudo CIs 90, 95, 99)")
  segments(x0=sma$b,y0=-1,y1=(nrow(dfma)),lty=2,lwd=1)

  # Influence plot
  baujat(sma,main=paste("Influence plot (Baujat) \n RE model for ",name[i],sep=""))

  # Galbraith plot
  radial.rma(sma,main=paste("Radial plot (Galbraith) \n RE model for ",name[i],sep=""))

  rm(dfma,sma,text)
}

#print("Ignore the warnings, they are produced by capturing the output and transforming character codes")

dev.off()

