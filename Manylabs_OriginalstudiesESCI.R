# MANYLABS ES-CI OF ORIGINAL STUDIES --------------------------------------------------------------------------------------------------
#
# All ES are converted to Cohen's d
# If applicable, multiple known conversion formulas, as well as procedures from the "compute.es" and "MAd" packages will be used
#
# Exact CIs (noncentral) are computed using package "MBESS"
#
# Excel file will be saved named: "ManyLabs_ES##CI.xls", where ## is the Confidence Level used
#
# Prepared by F.Hasselman for the Manylabs project: https://openscienceframework.org/project/WX7Ck/
# Contact me if you find any errors: me@fredhasselman.com


# Load required packages --------------------------------------------------------------------------------------------------------------
require("MBESS")
require("compute.es")
require("MAd")
require("metafor")
require("xlsx")

# Confidence level
CL   <- .95


# 1  Sunk Costs (Oppenheimer et al., 2009) --------------------------------------------------------------------------------------------
#
#   Use F statistic (group n unknown):
#   F(1, 211) = 2.74, p = .1, partial η2=.01. (p. 868)

f   <- 2.74
df1 <- 1
df2 <- 211
N   <- df2+2
n1  <- round(N/2)
n2  <- round((N/2))+1

# Convert f to d
fes(f,n1,n2)                          # 0.23
f_to_d(f,n1,n2)                       # 0.227
df <- (sqrt(f)*(N))/(sqrt(df2)*sqrt(n1*n2)) # 0.229

# Exact CI based on f to d
ES1 <- c(ci.smd(smd=df,n.1=n1, n.2=n2, conf.level=CL))
round(unlist(ci.smd(smd=df,n.1=n1, n.2=n2, conf.level=CL)),digits=2)
ES1 <- c("Oppenheimer et al., 2009","F(1,211) = 2.74","868","n1,n2",f,n1,n2, ES1)


# 2  Gain versus loss framing for combating disease (Tversky & Kahneman, 1981)  --------------------------------------------------------
#    
#    Use proportion data (p. 453) to build 2x2 contingency table to obtain Chi2
#    Note that reported percentages * N do not yield integer numbers

N1 = 152
N1a= round(.72*N1)  # 109
N1b= round(.28*N1)  # 43

N2 = 155
N2a= round(.22*N2)  # 34
N2b= round(.78*N2)  # 121

N  = N1+N2

# Get Chi^2
tbl  <- rbind(gain=c(N1a,N1b),loss=c(N2a,N2b))
chi2 <- chisq.test(x=tbl)$statistic

# Convert Chi^2 to d
chies(chi.sq=chi2,n=N)                     # 1.13
dchi <- r_to_d(r_from_chi(chi2, n=N),N=N)  # 1.1295
2*sqrt(chi2/((N)-chi2))                    # 1.1313

# Exact CI based on Chi^2 to d
ES2 <- c(ci.smd(smd=dchi, n.1=N1,n.2=N2, conf.level=CL))
round(unlist(ES2),digits=2)
ES2 <- c("Tversky & Kahneman, 1981","Chi^2 from reported prop. of 2 samples","453","Cell n in 2x2 table (rounding)",chi2,N1,N2,ES2)

# 3  Anchoring (Jacowitz & Kahneman, 1995)  --------------------------------------------------------------------------------------------
#
#    Use point-biserial correlation (N unknown), rpbs = .42 (p. 1163)

rpbs <- .42
N    <- 103

# Convert point-biserial r to d
res(rpbs,n=N)                   # 0.93
r_to_d(rpbs,N=N)                # 0.921
dr <- (2*rpbs)/sqrt((1-rpbs^2)) # 0.926
#sqrt(4*rpbs^2/(1-rpbs^2)) 


# Exact CI based on point-biserial r to standardised mean
ES3  <- ci.sm(sm=dr,N=N, conf.level=CL)
round(unlist(ES3),digits=2)
ES3  <- c("Jacowitz & Kahneman, 1995","point-biserial correlation = .42","1163","",rpbs,N,"",ES3)


# 4  Retrospective gambler’s fallacy (Oppenheimer & Monin, 2009)  ----------------------------------------------------------------------
#
#    Use reported Cohen's d: .69 (p. 329)
#    Or: d based on t

N   <-  59   # t(57) = 2.65
d   <- .69

N1 <- round(N/2)
N2 <- round(N/2)-1

# Exact CI based on r to d
ES4  <- ci.smd(smd=d,n.1=N1,n.2=N2,conf.level=CL)
round(unlist(ES4),digits=2)
ES4  <- c("Oppenheimer & Monin, 2009","Cohen's d = 0.69","329","n1,n2",d,N1,N2,ES4)


# 5  Low-vs.-high category scales (Schwarz et al., 1985)  -----------------------------------------------------------------------------
#
#    Use reported Chi2 (p. 390)

N     <- 132
chi2  <- 7.7 

# Convert Chi^2 to d
chies(chi.sq=chi2,n=N)             # 0.5
r_to_d(r_from_chi(chi2, n=N),N=N)  # 0.496
dchi <- 2*sqrt(chi2/(N-chi2))      # 0.498

N1 <- round(N/2)
N2 <- round(N/2)

# Exact CI based on Chi^2 to d
ES5  <- ci.smd(smd=dchi,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES5),digits=2)
ES5  <- c("Schwarz et al., 1985","Chi^2 = 7.7","390","n1,n2",chi2,N1,N2,ES5)


# 6  Norm of reciprocity (Hyman and Sheatsley, 1950)  ---------------------------------------------------------------------------------
#
#    Use 1948 data from table comparing 1948 to 1980 to get Chi^2

# Yes to Communist reporter
N1a = round(.365*581) #212  Order= Comm\Amer
N1b = round(.731*635) #464  Order= Amer\Comm

# Yes to American reporter
N2a = round(.656*567) #372  Order= Comm\Amer
N2b = round(.898*635) #570  Order= Amer\Comm

N   = N1a+N1b+N2a+N2b

# Get Chi^2
tbl  <- rbind(RNK1st=c(N1a,N2a),USA1st=c(N1b,N2b))
chi2 <- chisq.test(x=tbl)$statistic

# Convert Chi^2 to d
chies(chi.sq=chi2,n=N)             # 0.16
r_to_d(r_from_chi(chi2, n=N),N=N)  # 0.165
dchi <- 2*sqrt(chi2/((N)-chi2))    # 0.165

# Calculate group sizes based on order
N1 = N1a+N2a # Comm\Amer
N2 = N1b+N2b # Amer\Comm

ES6 <- c(ci.smd(smd=dchi, n.1=N1,n.2=N2, conf.level=CL))
round(unlist(ES6),digits=2)
ES6 <- c("Hyman and Sheatsley, 1950","Chi^2 from reported prop. of 2 samples","Table 2.1 from 1948-1980 comp.","Cell n in 2x2 table (rounding)",chi2,N1,N2,ES6)


# 7  Allowed/Forbidden (Rugg, 1941)  --------------------------------------------------------------------------------------------------
#
#    Use 1940 data from table comparing 1940 to 1974-76 to get Chi^2

# Changed: Allow should be NOT Allow
# 
# N   = 1300 # Assumption
# N1a = round(.54*N) #702  Forbid
# N1b = round(.46*N) #598  Forbid
# 
# N2a = round(.25*N) #325  NOT Allow
# N2b = round(.75*N) #975  NOT Allow

N   = 1300 # Assumption
N2a = round(.46*N) #702  Forbid
N2b = round(.54*N) #598  NOT Forbid

N1a = round(.38*N) #325  Allow
N1b = round(.62*N) #806  NOT Allow

# Get Chi^2
tbl  <- rbind(NotAllow=c(N1b,N1a),Forbid=c(N2a,N2b))
colnames(tbl) <- c('YES','NO')
chi2 <- chisq.test(x=tbl)$statistic


# # Get Chi^2
# tbl  <- rbind(Forbid=c(N1a,N1b),Allow=c(N2a,N2b))
# colnames(tbl) <- c('1940','1974')
# chi2 <- chisq.test(x=tbl)$statistic

# Convert Chi^2 to d
chies(chi.sq=chi2,n=N) 
r_from_chi(chi2, n=N)              # 
r_to_d(r_from_chi(chi2, n=N),N=N)  # 
dchi <- 2*sqrt(chi2/(N-chi2))      # 

N1 = N1a+N1b # NOT Allow
N2 = N2a+N2b # Forbid

# Exact CI based on Chi^2 to d
ES7  <- ci.smd(smd=dchi,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES7),digits=2)
ES7 <- c("Rugg, 1941","Chi^2 from reported prop. of 2 samples","Table 11.1 from 1940-1974/76 comp.","N",chi2,N1,N2,ES7)

# 8  Quote Attribution (Lorge & Curtis, 1936)  ----------------------------------------------------------------------------------------
#
#    Lots of results in the original paper... must be an ES in there somewhere :)

ES8 <- c("Lorge & Curtis, 1936","Conceptual replication",rep(NA,5),.5,.4,.6)

# 9  Flag Priming (Carter et al., 2011; Study 2) --------------------------------------------------------------------------------------
#
#   Use reported t to get Cohen's d (p. 6), group n not reported

t <- 2.04 # t(64) = -2.04
N <- 66   # df + 2

# Convert t to d
tes(t,N/2,N/2)                                # 0.5
t_to_d(t,N/2,N/2)                             # 0.502
dt1 <- (t*N) / (sqrt(N-2)*sqrt( (N/2)*(N/2))) # 0.51
dt2 <- lambda2delta(lambda=t,n.1=N/2,n.2=N/2) # 0.502

N1 = N/2
N2 = N/2

# Exact CI based on t to d
ES9 <- ci.smd(smd=dt2,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES9),digits=2)
ES9 <- c("Carter et al., 2011; Study 2","t(64) = -2.04","6","n1,n2",t,N1,N2,ES9)


# 10  Currency priming and system justification (Caruso et al., 2012) -----------------------------------------------------------------
#
#     Use reported Cohen's d of 0.80 (experiment 1)
#     Or: d based on t


N   <- 30   # t(28) = 2.12
d   <- .80

N1 = N/2
N2 = N/2

# Exact CI based on d
ES10  <- ci.smd(smd=d,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES10),digits=2)
ES10 <- c("Caruso et al., 2012","Cohen's d = 0.80","5 (manuscript in press)","n1,n2",d,N1,N2,ES10)


# 11  Imagined contact (Husnu & Crisp, 2010, Study 1) ---------------------------------------------------------------------------------
#
#     Use reported Cohen's d of 0.86 (p. 945)
#     Or: d based on t

N   <- 33   # t(31)=2.39
d   <- .86

N1 = round(N/2)
N2 = round(N/2)+1

# Exact CI based on d
ES11  <- ci.smd(smd=d,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES11),digits=2)
ES11 <- c("Husnu & Crisp, 2010","Cohen's d = 0.86","945","n1,n2",d,N1,N2,ES11)


# 12  Sex differences in implicit math attitudes and relations with self-reported attitudes (Nosek, Banaji, & Greenwald, 2002)  --------
#
#     a. Sex differences:
#     Use reported Cohen's d of 1.01 (p. 49)
#     Or: d based on t
#
#     b. Relations:
#     Use reported r of 0.42 convert to d (p. 54)

# a. Differences
Na   <- 79   # t(77) = 4.42
da   <- 1.01

N1 = round(Na/2)
N2 = round(Na/2)-1

# Exact CI based on d
ES12a  <- ci.smd(smd=da,n.1=N1, n.2=N2, conf.level=CL)
round(unlist(ES12a),digits=2)
ES12a <- c("Nosek, Banaji, & Greenwald, 2002","Cohen's d = 1.01","49","n1,n2",da,N1,N2,ES12a)


# b. Relations
r    <- .42
Nb   <- 243

res(r=r,n=Nb)                                           # 0.93
r_to_d(r=r,N=Nb)                                        # 0.924
dr <- (r*Nb)/sqrt(round(Nb/2)*(round(Nb/2)-1)*(1-r^2))  # 0.926

# Exact CI based on single standardised mean (correlation ≠ independent groups)
ES12b  <- ci.sm(sm=dr,N=Nb, conf.level=CL)
round(unlist(ES12b),digits=2)
ES12b <- c("Nosek, Banaji, & Greenwald, 2002","r = 0.42","54","",r,Nb,"",ES12b)

# Collect and save the data --------------------------------------------------------------------------------------------------------------------
name <- c(study1="Sunk Costs", study2="Gain vs loss framing", study3a="Anchoring - Babies Born", study3b="Anchoring - Mt. Everest", study3c="Anchoring - Chicago",study3d="Anchoring - Distance to NYC",study4="Retrospective gambler fallacy", study5="Low vs high category scales", study6="Norm of reciprocity", study7="Allowed/Forbidden",study8="Quote Attribution", study9="Flag Priming", study10="Currency Priming", study11="Imagined contact", study12a="Sex differences in implicit math attitudes", study12b="Relations between impl. and expl. math attitudes")

oriCI <- cbind(name,rbind(s1=ES1,s2=ES2,s3a=ES3,s3b=ES3,s3c=ES3,s3d=ES3,s4=ES4,s5=ES5,s6=ES6,s7=ES7,s8=ES8,s9=ES9,s10=ES10,s11=ES11,s12a=ES12a,s12b=ES12b))

colnames(oriCI) <- c("Study name","Reference","Result used","Source Page)","Assumptions","Statistic","N1","N2",paste(CL*100,".Lower",sep=""),"d",paste(CL*100,".Upper",sep=""))
oriCI

filen <- paste("ML_Original_ES",CL*100,"CI.xls",sep="")
write.xlsx(oriCI,filen,paste("OriginalES",CL*100,"CI",sep=""))