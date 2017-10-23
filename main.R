# Remove all variables and data
rm(list = ls())

# Call packages that will be used
{
  library(xlsx)
  library(plyr)
  library(moments)
  library(Hmisc)
  library(rworldmap)
  library(cluster)
  library(stringr)
  library(lubridate)
  library(Lmoments)
  library(gamlss)
  library(extRemes)
  library(fitdistrplus)
  library(evd)
  library(nleqslv)
  library(XLConnect)
  library(openxlsx)
  library(trend)
  library(Kendall)
  library(lmomRFA)
  
}

# Some useful functions and IATA codes
{
  cus.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }
  
  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }
  
  pop.var <- function(x) var(x) * (length(x)-1) / length(x)
  
  pop.sd <- function(x) sqrt(pop.var(x))
  
  texty = c("comm","nthu","thun","trop")
  
  colors = c("#e41a1c", "#377eb8", "#4daf4a", 
             "#984ea3", "#ff7f00", "#a65628",
             "#f781bf", "#999999")
}

# Load IATA codes
{
  OS <- Sys.info()['sysname']
  login <- Sys.info()['login']
  if(OS == "Windows"){
    if(login == "Jai"){
      # Windows/Home
      setwd("C:/Users/Jai/Box Sync/Wind") 
      
    }else if(login != "Jai"){
      # Windows/Office
      setwd("C:/Users/jchung11/Box Sync/Wind") 
    }
  }else if(OS != "Windows"){
    # Mac/Air
    setwd("~/Box Sync/Wind")
  }
  iata = read.table("codes.txt",stringsAsFactors = F)
}

{
  m <- readline("Input 4
                  ")
  m <- as.numeric(m)
  {
    mi <- readline("0 or 20?
                  ")
    mi <- as.numeric(mi)
  }
  if(mi == 0){
    mi = ""
  }else {
    mi = paste("_",mi,"mi",sep="")
  }
}

## Set data directory
for(filii in 1:2){
  OS <- Sys.info()['sysname']
  login <- Sys.info()['login']
  if(OS == "Windows"){
    if(login == "Jai"){
      # Windows/Home
      dir = setwd("C:/Users/Jai/Box Sync/Wind/gust_data") 
      dir_fl = "C:/Users/Jai/Box Sync/Wind/Florida/"
    }else if(login != "Jai"){
      # Windows/Office
      dir = setwd("C:/Users/jchung11/Box Sync/Wind/gust_data") 
      dir_fl = "C:/Users/jchung11/Box Sync/Wind/Florida/"
    }
  }else if(OS != "Windows"){
    # Mac/Air
    dir = setwd("~/Box Sync/Wind/gust_data")
    dir_fl = ("~/Box Sync/Wind/Florida/")
  }
  file = list.files(
    paste(dir,"Lower_48",sep="/"),
    pattern=paste("^station_matrix_",sep=""))
}

stations = read.xlsx(paste("stations_risk2",mi,".xlsx",sep=""))
req_stations = 
  which(as.numeric(substr(file,16,21)) %in% stations[,2])
req_file = file[req_stations]

# Tons of empty DFs
{
  # DF Sign
  d_sign = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 4))
  
  # DF COV + WIND ANGLE
  cov = as.data.frame(
    matrix(, nrow = , ncol = 5))
  
  # DF Duplicates
  d_dup = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 1))
  
  # DF Time
  time_rec = as.data.frame(
    matrix(, nrow = length(req_file), ncol = ))
  
  # DF Significance P-Values
  sigval = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 8))
  sigval_type = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 8))
  sigval.sl = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 4))
  
  # DF Indices
  indices = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 4))
  
  # DF MannKendall
  mk = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 4))
  mk1 = as.data.frame(
    matrix(, nrow = length(req_file), ncol = 4))
  
  # DF SC
  sc = as.data.frame(
    matrix(, nrow = length(req_file), ncol =4))
}

file_i = 1
type_w = 0

# Statistical analysis on Annual Maximums
while(file_i<length(req_file) + 1){ 
  data = read.xlsx(paste(dir,"Lower_48",req_file[file_i],sep="/"))
  colnames(data) <- data[6,]
  
  # Delete unnecessary rows until first data
  data = data[(grep("^Date", 
                    data[,1])+1):length(data[,1]),]
  
  data[,1] <-as.POSIXct(as.numeric(data[,1])*24*3600
                        + as.POSIXct("1899-12-30 00:00"))
  
  # Convert strings into numbers
  data[,2] = as.numeric(data[,2])
  data[,3] = as.numeric(data[,3])
  data[,4] = as.numeric(data[,4])
  
  while(type_w<4){
    # Read storm type
    if(type_w==0){
      # Commingled
      data_u <- data
    }
    if(type_w==1){
      # Non-thunderstorm
      data_u <- data[-c(which(data[,5]!="1")),]
    }
    if(type_w==2){
      # Thunderstorm
      data_u <- data[-c(which(data[,7]!="1")),]
    }
    if(type_w==3){
      # Tropical Storm
      data_u <- data[-c(which(data[,10]!="1")),]
    }
    
    # no_YEARS: how many data points are there?
    no_YEARS = length(unique(year(data_u[,1])))
    
    if(no_YEARS < 3){
      sigval[file_i,2*(type_w)+1] = NaN
      sigval[file_i,2*(type_w)+2] = NaN
      
      sigval_type[file_i,2*(type_w)+1] = NaN
      sigval_type[file_i,2*(type_w)+2] = NaN
      
      # Mann Kendall P-Values
      mk[file_i,type_w+1] = NaN
      
      d_sign[file_i,type_w+1] = NaN
    }
    else{
      source(paste0(dir_fl,"annmax.R"))
      
      t = used_date - min(used_date)
      t[which(is.na(used_data))] = NA
      used_data <- used_data[!is.na(used_data)]
      t <- t[!is.na(t)]
      
      time_rec[file_i,type_w+1] = length(t)
      
      # Linear Regression Model
      sta.lm = lm(used_data~t)
      
      if(sta.lm$coefficients[2]>0){
        d_sign[file_i,type_w+1] = "P"
      }else{
        d_sign[file_i,type_w+1] = "N"
      }
      
      if(all((table(used_data)==1)==T)==F){      
        d_dup[file_i] = 2
      }
      
      # P-Values for Intercept and Trend Coefficients
      sigval[file_i,2*(type_w)+1] = format(round(
        summary(sta.lm)$coefficients[1,4], 4), nsmall = 5)
      sigval[file_i,2*(type_w)+2] = format(round(
        summary(sta.lm)$coefficients[2,4], 5), nsmall = 5)
      sigval.sl[file_i,type_w+1] = format(round(
        summary(sta.lm)$coefficients[2], 5), nsmall = 5)
      
      t1 = sta.lm$coefficients[2]/
        summary(sta.lm)$coefficients[1,2]
      
      alp = pt(t1,df = length(used_data)-2, lower.tail = F)
      
      t2 = qt(1-alp,df = length(used_data)-2, lower.tail = T) - 
        1/sqrt((1/(cor(log(used_data),t)^2))-1)*sqrt(length(used_data))
      
      sigval_type[file_i,2*(type_w)+1] = format(round(alp,5), nsmall = 5)
      sigval_type[file_i,2*(type_w)+2] = format(round(
        pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
      
      # Calculate Serial Correlation
      nume = 0
      deno = 0
      used_n = length(used_data)
      for(n in 1:(used_n-1)){
        nume = nume + (used_data[n]-mean(used_data))*
          (used_data[n+1]-mean(used_data))
      }
      for(n in 1:used_n){
        deno = deno + (used_data[n]-mean(used_data))^2
      }
      sc[file_i,type_w+1] = (nume/(used_n-1))/(deno/used_n)
      
      # Mann Kendall P-Values
      mk1[file_i,type_w+1] = format(round(
        as.numeric(mk.test(ts(used_data, start = used_date[1],
                              end = used_date[length(used_date)]))[5]),5),
        nsmall = 5)
      
      # Manual MK
      # S
      mk_s = 0
      mk_j = 1
      while(mk_j<length(used_data)){
        mk_i = mk_j + 1
        while(mk_i<length(used_data)+1){
          mk_s = mk_s + sign(used_data[mk_i] - used_data[mk_j])
          mk_i = mk_i + 1
        }
        mk_j = mk_j + 1
      }
      
      # VARS
      mk_n = length(used_data)
      vars = (mk_n*(mk_n-1)*(2*mk_n+5))/18
      
      # ZMK
      if(vars>0){
        mk_z = (mk_s-1)/(sqrt(vars))
      }else if(vars<0){
        mk_z = (mk_s+1)/(sqrt(vars))
      }else if(vars==0){
        mk_z = 0
      }
      
      # Pval
      mk[file_i,type_w+1] = 2*(1-pnorm(abs(mk_z)))
    }
    type_w = type_w + 1
  }
  type_w = 0
  file_i = file_i + 1
}

stations_dup = stations[!duplicated(stations[,2]),]

# Manually choose stations
wanted = as.data.frame(
  matrix(, nrow = , ncol = ))

a = 1
b = 1
while(b!=3){
  
  if(b==1){
    no = c(30,30,30,7) # Default
    
    # Look at a group of stations
    pv <- readline("Input type of data desired:
                  ======================================
                  1: Commingled
                  2: Non-Thunderstorm
                  3: Thunderstorm
                  4: Tropical
                  ")
    pv <- as.numeric(pv)
    
    o <- readline("Input minimum number of years:
                  ")
    no[pv] = as.numeric(o)
    
  }
  
  b <- readline("Desired data:
                  ======================================
                  Input station number or 
                  type 0 to stop recording
                  type 1 to get all significant stations
                  type 2 to get ALL stations.
                  ")
  b <- as.numeric(b)
  
  if(b==0){
    b = 3
    super_stations = 
      stations_dup[match(wanted[,1],stations_dup[,1]),]
  }else if(b==1){
    b = 3
    stations_dup = stations[!duplicated(stations[,2]),]
    stations_dup = cbind(stations_dup,d_sign[,pv],
                         time_rec[,pv],
                         mk[,pv],
                         sigval[,c((pv-1)+pv,2*pv)],
                         sigval_type[,c((pv-1)+pv,2*pv)],
                         sigval.sl[,pv],
                         sc[,pv])
    sigsta = 
      which((stations_dup[,14]>=no[pv])&(stations_dup[,15]<0.050001))
    wanted = as.data.frame(stations_dup[sigsta,])
    super_stations = wanted
    
  }else if(b==2){
    b = 3
    stations_dup = cbind(stations_dup,d_sign[,pv],
                         time_rec[,pv],
                         mk[,pv],
                         sc[,pv])
    stations_dup = stations_dup[
      -c(which(is.na(stations_dup$`sc[, pv]`))), ]
    stations_dup = stations_dup[
      -c(which(stations_dup$`time_rec[, pv]`<no[pv])), ]
    assign("lmom",as.data.frame(
      matrix(, nrow = length(stations_dup[,1]), ncol = 12)))
    # stations_dup = cbind(stations_dup,d_sign[,pv],
    #                      time_rec[,pv],
    #                      mk[,pv],
    #                      sigval[,c((pv-1)+pv,2*pv)],
    #                      sigval_type[,c((pv-1)+pv,2*pv)],
    #                      sigval.sl[,pv])
    sc_ind_TF = (stations_dup$`sc[, pv]`<=
                   (1.96/sqrt(stations_dup$`time_rec[, pv]`)))&
      (stations_dup$`sc[, pv]`>=
         (-1.96/sqrt(stations_dup$`time_rec[, pv]`)))
    sc_ind = which((stations_dup$`sc[, pv]`<=
                      (1.96/sqrt(stations_dup$`time_rec[, pv]`)))&
                     (stations_dup$`sc[, pv]`>=
                        (-1.96/sqrt(stations_dup$`time_rec[, pv]`))))
    
    sc_i = 1
    for (sc_i in 1:length(sc_ind_TF)){
      data = read.xlsx(
        paste(dir,"Lower_48",
              paste("station_matrix_",stations_dup[sc_i,1],
                    "_update.xlsx",sep=""),
              sep="/"))
      colnames(data) <- data[6,]
      
      # Delete unnecessary rows until first data
      data = data[(grep("^Date", 
                        data[,1])+1):length(data[,1]),]
      
      data[,1] <-as.POSIXct(as.numeric(data[,1])*24*3600
                            + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data[,2] = as.numeric(data[,2])
      data[,3] = as.numeric(data[,3])
      
      # Read storm type
      if(pv==1){
        # Commingled
        data_u <- data
        text.storm = "Commingled"
      }
      if(pv==2){
        # Non-thunderstorm
        data_u <- data[-c(which(data[,5]!="1")),]
        text.storm = "Non-Thunderstorm"
      }
      if(pv==3){
        # Thunderstorm
        data_u <- data[-c(which(data[,7]!="1")),]
        text.storm = "Thunderstorm"
      }
      if(pv==4){
        # Tropical Storm
        data_u <- data[-c(which(data[,10]!="1")),]
        text.storm = "Tropical Storm"
      }
      
      # no_YEARS: how many data points are there?
      no_YEARS = length(unique(year(data_u[,1])))
      if(no_YEARS < 4){
        # sigval[2*pv-1,file_i] = NaN
        # sigval[2*pv,file_i] = NaN
        # 
        # sigval_type[2*pv-1,file_i] = NaN
        # sigval_type[2*pv,file_i] = NaN
        # 
        # # Mann Kendall P-Values
        # mk[pv,file_i] = NaN
        # 
        # d_sign[pv,file_i] = NaN
      }else{
        source(paste0(dir_fl,"annmax.R"))
        
        
        t = used_date - min(used_date)
        t[which(is.na(used_data))] = NA
        used_data <- used_data[!is.na(used_data)]
        t <- t[!is.na(t)]
        
      }
      # sc_acf = acf(used_data, plot=F)
      # sc[type_w+1,file_i] = sc_acf$acf[2]
      # 
      if(sc_ind_TF[sc_i]==T){
        nume = 0
        deno = 0
        used_n = length(used_data)
        for(n in 1:(used_n-1)){
          nume = nume + (used_data[n]-mean(used_data))*
            (used_data[n+1]-mean(used_data))
        }
        for(n in 1:used_n){
          deno = deno + (used_data[n]-mean(used_data))^2
        }
        
        sc.temp = (nume/(used_n-1))/(deno/used_n)
        
        # von Storch autocorrelation correction
        mod_i = 2
        for (mod_i in 2:length(used_data)){
          used_data[mod_i] = used_data[mod_i] - 
            sc.temp*used_data[mod_i-1]
        }
        
        # Manual MK
        # S
        mk_s = 0
        mk_j = 1
        while(mk_j<length(used_data)){
          mk_i = mk_j + 1
          while(mk_i<length(used_data)+1){
            mk_s = mk_s + sign(used_data[mk_i] - used_data[mk_j])
            mk_i = mk_i + 1
          }
          mk_j = mk_j + 1
        }
        
        # VARS
        mk_n = length(used_data)
        vars = (mk_n*(mk_n-1)*(2*mk_n+5))/18
        
        # ZMK
        if(vars>0){
          mk_z = (mk_s-1)/(sqrt(vars))
        }else if(vars<0){
          mk_z = (mk_s+1)/(sqrt(vars))
        }else if(vars==0){
          mk_z = 0
        }
        
        # Pval
        stations_dup[sc_i,15] = 2*(1-pnorm(abs(mk_z)))
      }
      
      lmom[sc_i,1] = 
        stations_dup[sc_i,1]
      lmom[sc_i,2] = 
        length(used_data)
      lmom[sc_i,3:7] = 
        Lmoments(used_data, rmax = 5)
      cov[sc_i,1:2] =
        as.data.frame(cbind(stations_dup[sc_i,1],
                            mean(used_data)/pop.sd(used_data)))
      cov[sc_i,3] = mean(used_data)
      cov[sc_i,4] = mean(as.numeric(year[,4]))
      cov[sc_i,5] = median(as.numeric(year[,4]))
        
    }
    
    }else if((b!=0)|(b!=1)|(b!=2)|(b!=3)){
      if(nchar(b)==6){
      wanted[a,] = as.matrix(b)
      a = a + 1  
    }else{
      message("Stations number must be six digits")
    }
  }
}

colnames(stations_dup)[13:16] = c("d_sign","timerec","mk","sc")
# 
# write.csv(wanted, file = "trop_f.csv")

lmom[,8] = lmom[,4]/lmom[,3]
lmom[,9] = lmom[,5]/lmom[,4]
lmom[,10] = lmom[,6]/lmom[,4]
lmom[,11] = lmom[,7]/lmom[,4]
colnames(lmom) <- c("Station","n","mean","l2",
                    "l3","l4","l5","t","t3","t4","t5")

# 3 if Mean, 4 if Median
k_ang = 3
kdata = as.data.frame(cbind(stations_dup[,1],
                            stations_dup[,8:10],
                            cov[,c(2,5)],
                            lmom[,8:10]))
k_n = stations_dup[,14]
colnames(kdata) = c("id","lat","lon","elev", 
                    "cov","ang","L-CV","L-Skewness",
                    "L-Kurtosis")
kdata.sca = kdata

# Feature Scaling
for(i in 2:dim(kdata)[2]){
  kdata[,i] = as.numeric(kdata[,i])
  kdata.sca[,i] = (kdata[,i]-min(kdata[,i]))/
    (max(kdata[,i])-min(kdata[,i]))
}

# Group variables as string
vari = colnames(kdata)[2:dim(kdata)[2]]
wind_vari = vari[c(4,6,7,8)]
piv_vari = vari[1]
npiv_vari = vari[2:8]

# Pivoted Clustering
cl = floor(sqrt(length(stations_dup[,1])/2))
cl_m = ceiling(sqrt(length(stations_dup[,1])/2))+1
while(cl<cl_m){
  d = 1
  assign(paste("maxsos",cl,sep=""), as.data.frame(
    matrix(, nrow = 8, ncol = 9)))
  while(d<length(npiv_vari)){
    combvar = t(as.data.frame(combn(npiv_vari,d)))
    assign(paste("sos",d,"c",cl,sep=""), as.data.frame(
      matrix(, nrow = dim(combvar)[1], ncol = dim(combvar)[2]+1)))
    maxsos = get(paste("maxsos",cl,sep=""))
    dd = 1
    while(dd<dim(combvar)[1]+1){
      sos = get(paste("sos",d,"c",cl,sep=""))
      
      kmdd = pam(kdata.sca[c(piv_vari,combvar[dd,])],cl)
      kmdd.sos = summary(silhouette(kmdd))$si.summary[4]
      
      sos[dd,] = c(kmdd.sos,
                   combvar[dd,1:dim(combvar)[2]])
      sos[,1] = as.numeric(sos[,1])
      assign(paste("sos",d,"c",cl,sep=""),sos)
      dd = dd + 1
    }
    maxsos[d,] = sos[which.max(sos[,1]),]
    assign(paste("maxsos",cl,sep=""),maxsos)
    d = d + 1
  }
  cl = cl + 1
}


# DF PPCC
PPCC_ST = as.data.frame(
  matrix(, nrow = 4, ncol = length(req_file)))
PPCC_NS = as.data.frame(
  matrix(, nrow = 4, ncol = length(req_file)))

# Latitude longitude L-CV
source(paste0(dir_fl,"latlonlcv.R"))

# LLL = rbind(LLL_2_1_30,LLL_2_2_30,
#             LLL_3_1_30,LLL_3_2_30,LLL_3_3_30)
# LLL = rbind(LLL_3_1_30,LLL_3_2_30,LLL_3_3_30, 
#             LLL_4_1_30,LLL_4_2_30,LLL_4_3_30,LLL_4_4_30)
# write.table(LLL, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)

# Optimal
source(paste0(dir_fl,"optimal.R"))


if(pv==5){
  
  ## L-moment diagram 
  # LCV/LSkew
  if(pv==1){
    par(mfrow=c(2,2))
  }
  if(pv<3){
    par(mar = c(7,4,2,1))
  }else{
    par(mar = c(9,4,0,1))
  }
  plot(lmom[,9],lmom[,8],
       xlim = c(-0.5,0.5), ylim = c(0,0.5),
       pch = 19, cex = 0.5,
       xlab = "L-Skewness",
       ylab = "L-CV", 
       cex.lab = 1.25,
       cex.axis = 1.25,
       type = "n")
  sam.t3 = seq(-1,1,length.out = 100000)
  lmrd.ln2 = 1.16008*sam.t3 - 0.05325*(sam.t3^2) -
    0.10501*(sam.t3^4) - 0.00103*(sam.t3^6)
  lmrd.ga2 = 1.74139*sam.t3 - 2.59736*(sam.t3^3) +
    2.09911*(sam.t3^4) - 0.35948*(sam.t3^6)
  lmrd.we2 = 0.17864 + 1.02381*sam.t3 - 
    0.17878*(sam.t3^2) - 0.00894*(sam.t3^4) - 
    0.01443*(sam.t3^6)
  lmrd.gp2 = 0.33299 + 0.44559*sam.t3 - 
    0.16641*(sam.t3^2) + 0.09111*(sam.t3^5) - 
    0.03625*(sam.t3^7)
  lines(sam.t3, lmrd.ln2, col = colors[1])
  lines(sam.t3, lmrd.gp2, col = colors[4])
  points(lmom[,9],lmom[,8],
         pch = 19, cex = 0.5)
  text(-.54,.48,text.storm, cex = 2, pos = 4)
  
  par(mfrow=c(1,1))
  par(mar = c(0,4.1,0,1.1))
  par(oma=c(0,4,17,4))
  legend("topleft", 
         c("Observations", "LN2 Theory", 
           "GA2 Theory", "WE2 Theory", 
           "GP2 Theory"),
         col = c("black",colors[1:4]), 
         pch = c(19, NA, NA, NA, NA),
         lty = c(NA, 1, 1, 1, 1),
         pt.cex = c(0.25, NA, NA, NA, NA),
         horiz = T,
         bty = "n")
  
  # LKurtosis/LSkew
  if(pv==1){
    par(mfrow=c(2,2))
  }
  if(pv<3){
    par(mar = c(7,4,2,1))
  }else{
    par(mar = c(9,4,0,1))
  }
  plot(lmom[,9],lmom[,10],
       xlim = c(-0.5,0.5), ylim = c(-0.5,0.5),
       pch = 19, cex = 0.5,
       xlab = "L-Kurtosis",
       ylab = "L-Skewness", 
       cex.lab = 1.25,
       cex.axis = 1.25,
       type = "n")
  sam.t3 = seq(-1,1,length.out = 100000)
  lmrd.gev = 0.10701 + 0.1109*sam.t3 + 
    0.84838*(sam.t3^2) - 0.06669*(sam.t3^3) + 
    0.00567*(sam.t3^4) - 0.04208*(sam.t3^5) + 
    0.03763*(sam.t3^6)
  lmrd.ln3 = 0.12282 + 0.77518*(sam.t3^2) + 
    0.12279*(sam.t3^4) - 0.13638*(sam.t3^6) + 
    0.11368*(sam.t3^8)
  lmrd.pt3 = 0.1224 + 0.30115*(sam.t3^2) + 
    0.95812*(sam.t3^4) - 0.57488*(sam.t3^6) + 
    0.19383*(sam.t3^8)
  lmrd.glo = 0.16667 + 0.83333*(sam.t3^2)
  lmrd.gpa = 0.20196*sam.t3 + 0.95924*(sam.t3^2) - 
    0.20096*(sam.t3^3) + 0.4061*(sam.t3^4)
  lines(sam.t3, lmrd.gev, col = colors[1])
  lines(sam.t3, lmrd.ln3, col = colors[2])
  lines(sam.t3, lmrd.pt3, col = colors[3])
  lines(sam.t3, lmrd.glo, col = colors[4])
  lines(sam.t3, lmrd.gpa, col = colors[5])
  points(lmom[,9],lmom[,10],
         pch = 19, cex = 0.5)
  text(0,.48,text.storm, cex = 2)
  
  par(mfrow=c(1,1))
  par(mar = c(0,4.1,0,1.1))
  par(oma=c(0,4,0,4))
  legend("top", 
         c("Observations", "GEV Theory", 
           "LN3 Theory", "PT3 Theory", 
           "GLO Theory", "GPA Theory"),
         col = c("black",colors[1:5]), 
         pch = c(19, NA, NA, NA, NA, NA),
         lty = c(NA, 1, 1, 1, 1, 1),
         pt.cex = c(0.25, NA, NA, NA, NA, NA),
         horiz = F,
         bty = "n",
         ncol = 3)
  
  
}


plot(X_ST,Y_ST, xlab = "Normal Quantiles",
     ylab = "Ordered Standardized Observations",
     type = "n", tck = 0.02,
     xlim = c(-3,3), ylim = c(-3,3))
points(X_NS,Y_NS, pch = 21, col = "gray",
       bg = "gray", cex = 2)
points(X_ST,Y_ST, pch = 20)
abline(0,1, lwd = 2)
leg_s = bquote(PPCC[s] == .(corst))
leg_n = bquote(PPCC[ns] == .(corns))
text(-3,2.75,leg_s, pos = 4,
     bty = "n",
     cex = 1.25)
text(-3,2.25,leg_n, pos = 4,
     bty = "n",
     cex = 1.25)
text(3.25,-2.5, text_plot, pos = 2,
     cex = 1.75)

type_w = type_w + 1


## Real Space

while(type_w < 4){
  
  ######################################
  ######################################
  ######################################
  if(type_w==0){
    par(mfrow=c(3,1)) 
    y_min = min(year[,3])
  }

  
  lm.year = lm(used_data~t)
  
  # Conditional Moment
  mean_y_w = unname(mean(used_data) + 
                      lm.year$coefficients[2]*(t-mean(t)))
  
  X_cor = lm.year$coefficients[2]*sd(t)/sd(ln_used_data)
  sd_y_w = sqrt((1-X_cor^2)*var(ln_used_data))
  skew_y_w = skewness(ln_used_data) - 
    lm.year$coefficients[2]^3*skewness(t)
  
  Y_ST = sort((ln_used_data - mean(ln_used_data))/sd(ln_used_data))
  Y_NS = sort((ln_used_data - mean_y_w)/sd_y_w)
  
  t_w_ST = t[order((ln_used_data - mean(ln_used_data))/sd(ln_used_data))]
  t_w_NS = t[order((ln_used_data - mean_y_w)/sd_y_w)]
  
  Y_ST = (Y_ST + mean(ln_used_data))/sd(ln_used_data) 
  Y_NS = (Y_NS + mean(ln_used_data) + 
            lm.year$coefficients[2]*(t_w_NS-mean(t)))/sd_y_w
  p_i = (1:length(ln_used_data)-0.375)/(length(ln_used_data)+0.25)
  X = qnorm(p_i)
  X_ST = exp(mean(ln_used_data) + X*sd(ln_used_data))
  X_NS = exp(mean(ln_used_data) + lm.year$coefficients[2]*(t_w_NS-mean(t)) 
             + X*sd(ln_used_data))
  corst = round(cor(X_ST,Y_ST),4)
  corns = round(cor(X_NS,Y_NS),4)
  
  if(type_w == 0){
    par(mfrow=c(2,2), new=TRUE)
  }
  
  plot(X_ST,Y_ST, xlab = "Normal Quantiles",
       ylab = "Ordered Observations",
       type = "n", tck = 0.02,
       xlim = c(0,100), ylim = c(0,100))
  points(X_NS,Y_NS, pch = 21, col = "gray",
         bg = "gray", cex = 2)
  points(X_ST,Y_ST, pch = 20)
  abline(0,1, lwd = 2)
  leg_s = bquote(PPCC[s] == .(corst))
  leg_n = bquote(PPCC[ns] == .(corns))
  text(0,95,leg_s, pos = 4,
       bty = "n",
       cex = 1.25)
  text(0,80,leg_n, pos = 4,
       bty = "n",
       cex = 1.25)
  text(105,5, text_plot, pos = 2,
       cex = 1.75)
  
  type_w = type_w + 1
}

legend("topleft",
       legend = c(
         substitute(paste(
           PPCC[st]," = ",cors),
           list(cors = corst)),
         substitute(paste(
           PPCC[ns]," = ",corn),
           list(corn = corns))),
       bty = "n",
       cex = 1.25)

legend("topleft",
       legend = c(
         substitute(paste(
           PPCC[st]," = ",cors),
           list(cors = corst)),
         substitute(paste(
           PPCC[ns]," = ",corn),
           list(corn = corns))),
       bty = "n",
       cex = 1.25)

legend('bottomright',
       legend = c("Theoretical",
                  "Nonstationary",
                  "Stationary"),
       lwd = c(2,NA,NA),lty = c(1,NA,NA),
       pch = c(NA,21,20),pt.cex = c(NA,2,1),
       col = c("black","gray","black"),
       pt.bg = c(NA,"gray","black"))