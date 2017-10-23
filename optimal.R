# Optimal
cl.tabl = as.data.frame(
  matrix(, nrow = , ncol = 15))
clu.f = floor(sqrt(dim(wanted)[1]/2))
for (clu in clu.f:(clu.f+1)){
  par(mar=c(2,2,2,2))
  png(filename=paste0(dir,"/plots/",texty[pv],
                      "/optimal_f",clu,".png"),
      w = 800, h = 600)
  max_clu = get(paste("maxsos",clu,sep=""))
  colclu = match(c("lat",max_clu[2,c(2:3)]),colnames(kdata.sca))
  clusters = pam(kdata.sca[,colclu], clu)
  
  {
    mappoint= cbind(dis_ang[as.numeric(rownames(kdata)),c(9,10)],
                    clusters$clustering)
    newmap <- getMap(resolution = "low")
    # windows(800, 600, pointsize = 12)
    plot(newmap, xlim = c(-80, -65), ylim = c(25, 45), asp = 1)
    legend(-63,30,legend = c(1:clu,"Discordant"),
           bg = "white",col=c(colors[1:clu],"black"), 
           bty = "n", pch = c(rep(20,clu),1), 
           pt.cex = c(rep(3,clu),6), 
           pt.lwd = c(rep(1,clu),3),
           ncol = 1, cex = 1.5)
    ll = 1
    while(ll<length(colnames(clusters$medoids))+1){
      if(ll==1){
        text(-63,42.5-ll*1.5,"Lat",
             cex = 2.5)
      }
      text(-63,42.5-ll*1.5,
           capitalize(colnames(clusters$medoids)[ll]),
           cex = 2.5)
      ll = ll + 1
    }
    text(-63,35,
         paste0("Si: ",summary(silhouette(clusters))$si.summary[4]),
         cex = 2.5)
    
    cl = 1
    if(clu!=clu.f){
      dimc = dim(cl.tabl)[1]
    }else{
      dimc = 0
    }
    while(cl<length(unique(clusters$clustering))+1){
      cl.index = which(as.numeric(clusters$clustering)==cl)
      dm.table = cbind(kdata[cl.index,1],
                       wanted[cl.index,14],
                       cov[cl.index,3],
                       lmom[cl.index,8:11])
      colnames(dm.table)[1:3] = c("id", "n", "mean")
      dm.test = regtst(dm.table[,1:6],5000)
      is.dm.ex = 
        length(which(as.numeric(dm.test$D)>dm.test$Dcrit[1]))
      n.good.fit = length(which(abs(dm.test$Z)<1.64))
      
      if(is.dm.ex>0){
        dm.sta = 
          dm.table[which(as.numeric(dm.test$D)>dm.test$Dcrit[1]),1]
        dm.point = cbind(dis_ang[match(dm.sta,dis_ang[,2]),c(9,10)],cl)
        points(dm.point$LON,dm.point$LAT,
               col=colors[cl], cex=6, pch = 1,
               lwd = 3)
      }
      if(clu==clu.f){
        cl.tabl[cl,1] = clu
        cl.tabl[cl,2] = cl
        cl.tabl[cl,3] = length(dm.test$D)
        cl.tabl[cl,4] = 
          length(which(dm.test$D>dm.test$Dcrit[1]))
        cl.tabl[cl,5] = dm.test$H[1]
        if(n.good.fit!=0){
          for(n.g in 1:n.good.fit){
            cl.tabl[cl,14+(2*n.g)-1] = sort(abs(dm.test$Z))[n.g]
            cl.tabl[cl,14+(2*n.g)] = attributes(sort(abs(dm.test$Z))[n.g])
          }
        }else{
          cl.tabl[cl,15] = sort(abs(abs(dm.test$Z)-1.64))[1] + 1.64
          cl.tabl[cl,16] = attributes(sort(abs(abs(dm.test$Z)-1.64))[1])
        }
      }else{
        cl.tabl[dimc + cl,1] = clu
        cl.tabl[dimc + cl,2] = cl
        cl.tabl[dimc + cl,3] = length(dm.test$D)
        cl.tabl[dimc + cl,4] = 
          length(which(dm.test$D>dm.test$Dcrit[1]))
        cl.tabl[dimc + cl,5] = dm.test$H[1]
        if(n.good.fit!=0){
          for(n.g in 1:n.good.fit){
            cl.tabl[dimc + cl,14+(2*n.g)-1] = sort(abs(dm.test$Z))[n.g]
            cl.tabl[dimc + cl,14+(2*n.g)] = attributes(sort(abs(dm.test$Z))[n.g])
          }
        }else{
          cl.tabl[dimc + cl,15] = sort(abs(abs(dm.test$Z)-1.64))[1] + 1.64
          cl.tabl[dimc + cl,16] = attributes(sort(abs(abs(dm.test$Z)-1.64))[1])
        }
      }
      f = 1
      
      ## Obtain desired data
      file = paste0("station_matrix_",
                    kdata.sca[which(clusters$clustering==cl),1],
                    "_update.xlsx")
      if(is.dm.ex>0){
        file_r = paste0("station_matrix_",
                        dm.sta,
                        "_update.xlsx")
        file = file[which(!(file %in% file_r))]
      }
      yearr = as.data.frame(
        matrix(, nrow = , ncol = 2))
      
      ## Identify beginning and ending years
      while(f<length(file)+1){
        data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
        
        
        # Delete unnecessary rows until first data
        data_gg = data_g[(grep("^Date", 
                               data_g[,1])+1):length(data_g[,1]),]
        
        # assign data_gg column names as data_g's row 6 
        # because it has column names aligned correctly
        colnames(data_gg) <- data_g[6,]
        
        # Date/Time Format originally MM/DD/YYYY HH:MM
        # Distorted to Excel Serial Date (start from 1900-01-01)
        # numbers while loading. Hence, these are  
        # converted to YYYY-MM-DD HH:MM format
        data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                                 + as.POSIXct("1899-12-30 00:00"))
        
        # Convert strings into numbers
        data_gg[,2] = as.numeric(data_gg[,2])
        data_gg[,3] = as.numeric(data_gg[,3])
        
        # Read storm type
        # Read storm type
        
        if(pv==2){
          # Non-thunderstorm
          data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
        }
        if(pv==3){
          # Thunderstorm
          data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
        }
        if(pv==4){
          # Tropical Storm
          data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
        }
        
        # Obtain minimum and maximum years of each station
        yearr[f,1] = min(unique(year(data_gg[,1])))
        yearr[f,2] = max(unique(year(data_gg[,1])))
        
        f = f + 1
      }
      
      super_data = as.data.frame(
        matrix(, nrow = length(min(yearr[,1]):max(yearr[,2])), 
               ncol = 2*length(file)))
      super_data[,seq(2,length(file)*2,2)] = 
        min(yearr[,1]):max(yearr[,2])
      
      f = 1
      
      # Place max accordingly
      while(f<length(file)+1){
        data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
        
        # Delete unnecessary rows until first data
        data_gg = data_g[(grep("^Date", 
                               data_g[,1])+1):length(data_g[,1]),]
        
        # assign data_gg column names as data_g's row 6 
        # because it has column names aligned correctly
        colnames(data_gg) <- data_g[6,]
        
        # Date/Time Format originally MM/DD/YYYY HH:MM
        # Distorted to Excel Serial Date (start from 1900-01-01)
        # numbers while loading. Hence, these are  
        # converted to YYYY-MM-DD HH:MM format
        data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                                 + as.POSIXct("1899-12-30 00:00"))
        
        # Convert strings into numbers
        data_gg[,2] = as.numeric(data_gg[,2])
        data_gg[,3] = as.numeric(data_gg[,3])
        
        # Read storm type
        if(pv==2){
          # Non-thunderstorm
          data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
        }
        if(pv==3){
          # Thunderstorm
          data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
        }
        if(pv==4){
          # Tropical Storm
          data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
        }
        
        # Annual
        {
          i = 1
          j = 1
          k = 1
          # no_YEARS: how many data points are there?
          no_YEARS = length(unique(year(data_gg[,1])))
          year_a = as.data.frame(matrix(, nrow = no_YEARS, ncol = 2))
          data_gg[,14] = year(data_gg[,1])
          while(k<(no_YEARS)){
            # Identify which data row is the last one
            # from all of the year n
            while(data_gg[i + j - 1,14] == data_gg[i + j,14]){
              j = j + 1
            }
            j = i + j - 1
            
            if(m == 1){
              # Mean
              year[k] = sum(data_gg[i:j,3])/length(i:j)
              type = "Mean"
            }else if(m == 2){
              # Median
              year[k] = median(data_gg[i:j,3]) 
              type = "Median"
            }else if(m == 3){
              # Coefficient of Variation
              year[k] = sd(data_gg[i:j,3])/
                (sum(data_gg[i:j,3])/length(i:j)) 
              type = "Coefficient of Variation"
            }else if(m == 4){
              # Maximum
              year_a[k,1] = max(data_gg[i:j,3])
              year_a[k,2] = as.character(data_gg[i,1])
              year_a[k,3] = year(data_gg[i,1])
              type = "Maximum"
            }else if(m == 5){
              # Frequency
              year[k] = length(i:j)
              type = "Frequency"
            }else{
              # L-Moments
              L = Lmoments(data_gg[i:j,3], na.rm=TRUE)
              lcv = L[2]/L[1] # L-cv
              lsk = L[3]/L[2] # L-Skew
              lku = L[4]/L[2] # L-Kurtosis
              
              l1 = fit$mle[1]/(1+fit$mle[2])
              l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
              
              t2[k,1] = l2/l1
              t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
              
              if(n == 1){
                year[k] = lcv
                type = "L-CV"
              }else if(n == 2){
                year[k] = lsk
                type = "L-Skewness"
              }else{
                year[k] = lku
                type = "L-Kurtosis"
              }
            }
            
            # i variable is added by 1
            # to move on to the next year
            i = j + 1
            j = 1
            k = k + 1
            if(k == (no_YEARS)){
              last = length(data_gg[,3])
              if(m == 1){
                year[k] = sum(data_gg[i:last,3])/length(i:last)
              }else if(m == 2){
                year[k] = median(data_gg[i:last,3])
              }else if(m == 3){
                year[k] = sd(data_gg[i:last,3])/
                  (sum(data_gg[i:last,3])/length(i:last)) 
              }else if(m == 4){
                year_a[k,1] = max(data_gg[i:last,3])
                year_a[k,2] = as.character(data_gg[last,1])
                year_a[k,3] = year(data_gg[last,1])
              }else if(m == 5){
                year[k] = length(i:last)
              }else{
                L = Lmoments(data_gg[i:last,3], na.rm=TRUE)
                lcv = L[2]/L[1] # L-cv
                lsk = L[3]/L[2] # L-Skew
                lku = L[4]/L[2] # L-Kurtosis
                
                l1 = fit$mle[1]/(1+fit$mle[2])
                l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
                
                t2[k,1] = l2/l1
                t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
                
                if(n == 1){
                  year[k] = lcv
                }else if(n == 2){
                  year[k] = lsk
                }else{
                  year[k] = lku
                }
              }
              k = k + 1
            }
          }
          used_data = year_a[,1]
          used_date = year_a[,2]
        }
        
        # Obtain desired data
        super_data[,2*(f-1)+1] = 
          ifelse(!is.na(match(super_data[,2*(f)],year_a[,3])),
                 year_a[,1],NA)
        
        f = f + 1
      }
      
      # Obtain max from every row, ignoring NA
      cus.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
      vector.is.empty <- function(x) return(length(x) ==0 )
      
      dim_sup = dim(super_data[,seq(1,length(file)*2-1,2)])[2]
      # Superstation values
      if(vector.is.empty(dim_sup)){
        used_data = super_data[,seq(1,length(file)*2-1,2)]
        nas = which(is.na(used_data))
        used_data = used_data[!is.na(used_data)]
        yearr_t = min(yearr[,1]):max(yearr[,2])
        yearr_t[nas] = NA
        yearr_t = yearr_t[!is.na(yearr_t)]
        
        # Remove data points that are NA (not recorded)
        # by indexing together
        t = yearr_t - min(yearr_t)
        t[which(is.na(used_data))] = NA
        used_data <- used_data[!is.na(used_data)]
        t <- t[!is.na(t)]
      }else if(dim_sup>1){
        used_data = apply(super_data[,seq(1,length(file)*2-1,2)],1, cus.max)
        yearr_t = min(yearr[,1]):max(yearr[,2])
        
        # Remove data points that are NA (not recorded)
        # by indexing together
        t = yearr_t - min(yearr_t)
        t[which(is.na(used_data))] = NA
        used_data <- used_data[!is.na(used_data)]
        t <- t[!is.na(t)]
      }
      
      used_data = log(used_data)
      
      # Significance
      # Linear Regression Model
      sta.lm = lm(used_data~t)
      
      # P-Values for Intercept and Trend Coefficients
      t1 = sta.lm$coefficients[2]/
        summary(sta.lm)$coefficients[2,2]
      
      alp = pt(t1,df = length(used_data)-2, lower.tail = F)
      
      t2 = qt(1-alp,df = length(used_data)-2, lower.tail = T) - 
        1/sqrt((1/(cor(log(used_data),t)^2))-1)*sqrt(length(used_data))
      
      
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
      mk = 2*(1-pnorm(abs(mk_z)))
      
      if(clu==clu.f){
        cl.tabl[cl,6] = format(round(
          summary(sta.lm)$coefficients[2,1], 5), nsmall = 5)
        cl.tabl[cl,7] = format(round(
          summary(sta.lm)$coefficients[2,4], 5), nsmall = 5)
        cl.tabl[cl,8] = format(round(alp,5), nsmall = 5)
        cl.tabl[cl,9] = format(round(
          pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
        cl.tabl[cl,10] = mk
        cl.tabl[cl,12:14] = colnames(clusters$medoids)
      }else{
        cl.tabl[dimc + cl,6] = format(round(
          summary(sta.lm)$coefficients[2,1], 5), nsmall = 5)
        cl.tabl[dimc + cl,7] = format(round(
          summary(sta.lm)$coefficients[2,4], 5), nsmall = 5)
        cl.tabl[dimc + cl,8] = format(round(alp,5), nsmall = 5)
        cl.tabl[dimc + cl,9] = format(round(
          pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
        cl.tabl[dimc + cl,10] = mk
        cl.tabl[dimc + cl,12:14] = colnames(clusters$medoids)
      }
      
      cl = cl + 1
    }
    
    mm = 1
    while(mm < max(clusters$clustering)+1){
      mmm = which(clusters$clustering %in% mm)
      points(mappoint$LON[mmm],mappoint$LAT[mmm],
             col=colors[mm], cex=4, pch = 20)
      mm = mm + 1
    }
    
    # savePlot("clipboard", type="wmf")
    dev.off()
  }
  
  # Plotting
  cl = 1
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    
    file = paste0("station_matrix_",
                  kdata.sca[cl.index,1],
                  "_update.xlsx")
    
    dm.table = cbind(kdata[cl.index,1],
                     wanted[cl.index,14],
                     cov[cl.index,3],
                     lmom[cl.index,8:11])
    colnames(dm.table)[1:3] = c("id", "n", "mean")
    dm.test = regtst(dm.table[,1:6],5000)
    is.dm.ex = 
      length(which(as.numeric(dm.test$D)>dm.test$Dcrit[1]))
    n.good.fit = length(which(abs(dm.test$Z)<1.64))
    
    # Check if there are discordant stations
    if(is.dm.ex>0){
      file_r = paste0("station_matrix_",
                      dm.sta,
                      "_update.xlsx")
      file = file[which(!(file %in% file_r))]
    }
    yearr = as.data.frame(
      matrix(, nrow = , ncol = 2))
    
    f = 1
    ## Identify beginning and ending years
    while(f<length(file)+1){
      data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
      
      
      # Delete unnecessary rows until first data
      data_gg = data_g[(grep("^Date", 
                             data_g[,1])+1):length(data_g[,1]),]
      
      # assign data_gg column names as data_g's row 6 
      # because it has column names aligned correctly
      colnames(data_gg) <- data_g[6,]
      
      # Date/Time Format originally MM/DD/YYYY HH:MM
      # Distorted to Excel Serial Date (start from 1900-01-01)
      # numbers while loading. Hence, these are  
      # converted to YYYY-MM-DD HH:MM format
      data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                               + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data_gg[,2] = as.numeric(data_gg[,2])
      data_gg[,3] = as.numeric(data_gg[,3])
      
      # Read storm type
      # Read storm type
      
      if(pv==1){
        # Non-thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
      }
      if(pv==2){
        # Thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
      }
      if(pv==3){
        # Tropical Storm
        data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
      }
      
      # Obtain minimum and maximum years of each station
      yearr[f,1] = min(unique(year(data_gg[,1])))
      yearr[f,2] = max(unique(year(data_gg[,1])))
      
      f = f + 1
    }
    
    super_data = as.data.frame(
      matrix(, nrow = length(min(yearr[,1]):max(yearr[,2])), 
             ncol = 2*length(file)))
    super_data[,seq(2,length(file)*2,2)] = 
      min(yearr[,1]):max(yearr[,2])
    
    f = 1
    
    # Place max accordingly
    while(f<length(file)+1){
      data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
      
      # Delete unnecessary rows until first data
      data_gg = data_g[(grep("^Date", 
                             data_g[,1])+1):length(data_g[,1]),]
      
      # assign data_gg column names as data_g's row 6 
      # because it has column names aligned correctly
      colnames(data_gg) <- data_g[6,]
      
      # Date/Time Format originally MM/DD/YYYY HH:MM
      # Distorted to Excel Serial Date (start from 1900-01-01)
      # numbers while loading. Hence, these are  
      # converted to YYYY-MM-DD HH:MM format
      data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                               + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data_gg[,2] = as.numeric(data_gg[,2])
      data_gg[,3] = as.numeric(data_gg[,3])
      
      # Read storm type
      if(pv==2){
        # Non-thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
      }
      if(pv==3){
        # Thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
      }
      if(pv==4){
        # Tropical Storm
        data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
      }
      
      # Annual
      {
        i = 1
        j = 1
        k = 1
        # no_YEARS: how many data points are there?
        no_YEARS = length(unique(year(data_gg[,1])))
        year_a = as.data.frame(matrix(, nrow = no_YEARS, ncol = 2))
        data_gg[,14] = year(data_gg[,1])
        while(k<(no_YEARS)){
          # Identify which data row is the last one
          # from all of the year n
          while(data_gg[i + j - 1,14] == data_gg[i + j,14]){
            j = j + 1
          }
          j = i + j - 1
          
          if(m == 1){
            # Mean
            year[k] = sum(data_gg[i:j,3])/length(i:j)
            type = "Mean"
          }else if(m == 2){
            # Median
            year[k] = median(data_gg[i:j,3]) 
            type = "Median"
          }else if(m == 3){
            # Coefficient of Variation
            year[k] = sd(data_gg[i:j,3])/
              (sum(data_gg[i:j,3])/length(i:j)) 
            type = "Coefficient of Variation"
          }else if(m == 4){
            # Maximum
            year_a[k,1] = max(data_gg[i:j,3])
            year_a[k,2] = as.character(data_gg[i,1])
            year_a[k,3] = year(data_gg[i,1])
            type = "Maximum"
          }else if(m == 5){
            # Frequency
            year[k] = length(i:j)
            type = "Frequency"
          }else{
            # L-Moments
            L = Lmoments(data_gg[i:j,3], na.rm=TRUE)
            lcv = L[2]/L[1] # L-cv
            lsk = L[3]/L[2] # L-Skew
            lku = L[4]/L[2] # L-Kurtosis
            
            l1 = fit$mle[1]/(1+fit$mle[2])
            l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
            
            t2[k,1] = l2/l1
            t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
            
            if(n == 1){
              year[k] = lcv
              type = "L-CV"
            }else if(n == 2){
              year[k] = lsk
              type = "L-Skewness"
            }else{
              year[k] = lku
              type = "L-Kurtosis"
            }
          }
          
          # i variable is added by 1
          # to move on to the next year
          i = j + 1
          j = 1
          k = k + 1
          if(k == (no_YEARS)){
            last = length(data_gg[,3])
            if(m == 1){
              year[k] = sum(data_gg[i:last,3])/length(i:last)
            }else if(m == 2){
              year[k] = median(data_gg[i:last,3])
            }else if(m == 3){
              year[k] = sd(data_gg[i:last,3])/
                (sum(data_gg[i:last,3])/length(i:last)) 
            }else if(m == 4){
              year_a[k,1] = max(data_gg[i:last,3])
              year_a[k,2] = as.character(data_gg[last,1])
              year_a[k,3] = year(data_gg[last,1])
            }else if(m == 5){
              year[k] = length(i:last)
            }else{
              L = Lmoments(data_gg[i:last,3], na.rm=TRUE)
              lcv = L[2]/L[1] # L-cv
              lsk = L[3]/L[2] # L-Skew
              lku = L[4]/L[2] # L-Kurtosis
              
              l1 = fit$mle[1]/(1+fit$mle[2])
              l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
              
              t2[k,1] = l2/l1
              t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
              
              if(n == 1){
                year[k] = lcv
              }else if(n == 2){
                year[k] = lsk
              }else{
                year[k] = lku
              }
            }
            k = k + 1
          }
        }
        used_data = year_a[,1]
        used_date = year_a[,2]
      }
      
      
      # Obtain desired data
      super_data[,2*(f-1)+1] = 
        ifelse(!is.na(match(super_data[,2*(f)],year_a[,3])),
               year_a[,1],NA)
      
      f = f + 1
    }
    
    # Obtain max from every row, ignoring NA
    cus.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
    
    if(dim(super_data)[2]<3){
      super_data = super_data[which(!is.na(super_data[,1])),]
      yearr_t = super_data[,2]
      used_data = super_data[,1]
    }else{
      # Superstation values
      used_data = apply(super_data[,seq(1,length(file)*2-1,2)],1, cus.max)
      yearr_t = min(yearr[,1]):max(yearr[,2])
    }
    
    # Remove data points that are NA (not recorded)
    # by indexing together
    t = yearr_t
    t[which(is.na(used_data))] = NA
    used_data <- used_data[!is.na(used_data)]
    t <- t[!is.na(t)]
    
    # Significance
    # Linear Regression Model
    sta.lm = lm(log(used_data)~t)
    
    # P-Values for Intercept and Trend Coefficients
    t1 = sta.lm$coefficients[2]/
      summary(sta.lm)$coefficients[2,2]
    
    alp = pt(t1,df = length(used_data)-2, lower.tail = F)
    
    t2 = qt(1-alp,df = length(used_data)-2, lower.tail = T) - 
      1/sqrt((1/(cor(log(used_data),t)^2))-1)*sqrt(length(used_data))
    
    # Log transformation and linear regression
    ln_used_data = log(used_data)
    lm.year = lm(ln_used_data~t)
    m.year = lm(used_data~t)
    sta.data = m.year$coefficients[1]+m.year$coefficients[2]*t
    rho = cor(ln_used_data,t)
    
    # Conditional Moments of y
    mu_y.w = lm.year$coefficients[1] + 
      lm.year$coefficients[2]*t
    vr_y.w = (sd(ln_used_data)^2)*(1-rho^2)
    sk_y.w = skewness(ln_used_data) - 
      skewness(t)*(lm.year$coefficients[2]^3)
    
    # Conditional Moments of x
    mu_x.w = exp(mu_y.w + (vr_y.w)/2)
    vr_x.w = exp(2*mu_y.w + vr_y.w)*(exp(vr_y.w)-1)
    cv_x.w = sqrt(exp(vr_y.w)-1)
    sk_x.w = 3*cv_x.w + cv_x.w^3
    
    # Stationary
    mean.sta = mean(ln_used_data)
    sd.sta = sqrt(sum((ln_used_data - 
                         mean(ln_used_data))^2)/
                    length(ln_used_data))
    
    YR = c(10,20,30)
    LLL = as.data.frame(matrix(, nrow = 4, ncol = 9))
    
    {
      # PPCC 
      b_coef = cor(ln_used_data, t)*sd(ln_used_data)/sd(t)
      mu_y.pp = mean.sta + b_coef*(t-mean(t))
      sd_y.pp = sqrt(var(ln_used_data)*(1-cor(ln_used_data, t)^2))
      z_i.s = sort((ln_used_data - mean.sta)/sd.sta)
      z_i.n = sort((ln_used_data - mu_y.pp)/sd_y.pp)
      rank_i.s = rank(z_i.s)
      rank_i.n = rank(z_i.n)
      p_pos.s = (rank_i.s - 0.375)/(length(ln_used_data) + 0.25)
      p_pos.n = (rank_i.n - 0.375)/(length(ln_used_data) + 0.25)
      
      z_ii.s = qnorm(p_pos.s)
      z_ii.n = qnorm(p_pos.n)
      if(cl==1){
        if(clu==2){
          png(filename=paste0(dir,"/plots/",texty[pv],
                              "/pp_opt",clu,".png"),
              w = 800, h = 400)
          par(mar=c(8,4.5,2,2))
          par(mfrow=c(1,2))
          txt = 1
          txt2 = 1
        }else if(clu==3){
          png(filename=paste0(dir,"/plots/",texty[pv],
                              "/pp_opt",clu,".png"),
              w = 900, h = 300)
          par(mar=c(8,4.5,2,2))
          par(mfrow=c(1,3))
          txt = 1.5
          txt2 = txt
        }else if(clu==4){
          png(filename=paste0(dir,"/plots/",texty[pv],
                              "/pp_opt",clu,".png"),
              w = 800, h = 800)
          par(mar=c(8,4.5,2,2))
          par(mfrow=c(2,2))
          txt = 1.5
          txt2 = txt
        }
      }
      plot(z_ii.s,z_i.s, xlab = "Normal Quantiles",
           ylab = "Ordered Standardized Observations",
           type = "n", tck = 0.02,
           xlim = c(-3,3), ylim = c(-3,3),
           cex.lab = txt, xaxt= "n", yaxt ="n")
      axis(1, cex.axis = txt)
      axis(2, cex.axis = txt)
      
      points(z_ii.n,z_i.n, pch = 21, col = "gray",
             bg = "gray", cex = 2)
      points(z_ii.s,z_i.s, pch = 20)
      abline(0,1, lwd = 1)
      
      corst = round(cor(z_ii.s,z_i.s),4)
      corns = round(cor(z_ii.n,z_i.n),4)
      
      leg_s = bquote(PPCC[s] == .(corst))
      leg_n = bquote(PPCC[ns] == .(corns))
      text(-3,2.75,leg_s, pos = 4,
           bty = "n",
           cex = txt2)
      text(-3,2.25,leg_n, pos = 4,
           bty = "n",
           cex = txt2)
      text(3.25,-2.25, 
           paste0(clu,"-Means"), pos = 2,
           cex = txt2)
      text(3.25,-1.75, 
           text.storm, pos = 2,
           cex = txt2)
      text(3.25,-2.25, 
           paste0(clu,"-Means"), pos = 2,
           cex = txt2)
      text(3.25,-2.75, 
           paste("Cluster",cl), pos = 2,
           cex = txt2)
      if(cl==length(unique(clusters$clustering))){
        add_legend("bottom", 
                   legend = c("Stationary", "Nonstationary"),
                   ncol = 2, pch = c(20, 21),
                   col = c("black", "gray"), pt.cex = c(1,2),
                   pt.bg = c(NA, "gray"), bty ="n",
                   cex = 2)
        dev.off()
      }
    }
    
    if(dis=="pe3"){
      # LP3
      n_lp = length(ln_used_data)
      sk_y.w.lp = (1+6/n_lp)*
        ((n_lp*sum((ln_used_data-mean(ln_used_data))^3)))/
        ((n_lp-1)*(n_lp-2)*sd(ln_used_data)^3)
      a.lp3 = max(2/sk_y.w.lp,0.4)
      b.lp3 = 1 + 0.0144*((max(0,sk_y.w.lp-2.25))^2)
      f.lp3 = sk_y.w.lp - 0.063*(max(0,sk_y.w.lp-1)^1.85)
      h.lp3 = (b.lp3 - 2/(sk_y.w.lp*a.lp3))^(1/3)
      k.p_max2 = 1-((f.lp3/6)^2) + qnorm(1-(1/rp))*((f.lp3/6))
      kp_i = 1
      k.p = as.data.frame(
        matrix(, nrow = length(k.p_max2), 
               ncol = 1))
      for(kp_i in 1:length(k.p_max2)){
        k.p[kp_i,] = max(h.lp3,k.p_max2[kp_i])
      }
      k.p = a.lp3*(k.p^3) - b.lp3
      
      ppp_sta = exp(mean.sta + k.p*sd.sta)
      ppp_hom = exp(mean.hom + k.p*sd.hom)
      ppp_het = exp(mean.het + k.p*sd.het)
    }
    if(dis=="gno"){
      # LN2
      ppp_sta.ln2 = exp(mean.sta + qnorm(1-(1/rp))*sd.sta)
      ppp_hom.ln2 = exp(mean.hom + qnorm(1-(1/rp))*sd.hom)
      ppp_het.ln2 = exp(mean.het + qnorm(1-(1/rp))*sd.het)
      
      # LN3
      # u = ln(x - tau)
      # Stationarity
      vr_x = sum((used_data-mean(used_data))^2)/
        (length(used_data)-1)
      g_x = length(used_data)*sum((used_data-mean(used_data))^3)/
        ((length(used_data)-1)*(length(used_data)-2)*sqrt(vr_x)^3)
      
      B = .5*(-g_x + sqrt(g_x^2 + 4))
      the = (1 - B^(2/3))/(B^(1/3))
      ome = the^2 + 1
      
      mu_u = .5*log(vr_x/(ome*(ome-1)))
      vr_u = log(ome)
      ta_x = mean(used_data) - sqrt(vr_x)/the
      ppp_sta.ln3 = ta_x + exp(mu_u + qnorm(1-(1/rp))*sqrt(vr_u))
      
      # Nonstationarity
      # Homogeneous
      mu_x.w.hom = exp(mean.hom + (sd.hom^2)/2)
      vr_x.w.hom = exp(2*mean.hom + sd.hom^2)*(exp(sd.hom^2)-1)
      cv_x.w.hom = sqrt(exp(sd.hom^2)-1)
      sk_x.w.hom = 3*cv_x.w.hom + cv_x.w.hom^3
      
      B_non = .5*(-sk_x.w.hom + sqrt(sk_x.w.hom^2 + 4))
      the_non = (1-B_non^(2/3))/(B_non^(1/3))
      ome_non = the_non^2 + 1
      mu_u.x = .5*log(vr_x.w.hom/(ome_non*(ome_non-1)))
      vr_u.x = log(ome_non)
      ta_x.w = mu_x.w.hom - sqrt(vr_x.w.hom)/the_non
      ppp_hom.ln3 = ta_x.w + exp(mu_u.x + qnorm(1-(1/rp))*sqrt(vr_u.x))
      
      # Heterogeneous
      mu_x.w.het = exp(mean.het + (sd.het^2)/2)
      vr_x.w.het = exp(2*mean.het + sd.het^2)*(exp(sd.het^2)-1)
      cv_x.w.het = sqrt(exp(sd.het^2)-1)
      sk_x.w.het = 3*cv_x.w.het + cv_x.w.het^3
      
      B_non = .5*(-sk_x.w.het + sqrt(sk_x.w.het^2 + 4))
      the_non = (1-B_non^(2/3))/(B_non^(1/3))
      ome_non = the_non^2 + 1
      mu_u.x = .5*log(vr_x.w.het/(ome_non*(ome_non-1)))
      vr_u.x = log(ome_non)
      ta_x.w = mu_x.w.het - sqrt(vr_x.w.het)/the_non
      ppp_het.ln3 = ta_x.w + exp(mu_u.x + qnorm(1-(1/rp))*sqrt(vr_u.x))
      
      ln.com = c(ppp_sta.ln2, ppp_hom.ln2, ppp_het.ln2,
                 ppp_sta.ln3, ppp_hom.ln3, ppp_het.ln3)
      ln.max = signif(ceiling(max(ln.com)), digits = 1)
      ln.min = signif(round_any(min(ln.com), 10, f = ceiling), digits = 1)
      plot(rp,ppp_sta.ln2, log = "x", typ="n", 
           xlim = c(1,1700), ylim = c(ln.min,ln.max),
           xlab = "Return Period",
           ylab = "Recurrence Level (mph)",
           xaxt = "n")
      axis(side=1, labels = T, at=c(100,300,700,1700))
      abline(v=c(100, 300,700, 1700), 
             h=seq(40,160,5), 
             col="gray", lty=3)
      lines(rp_gev, ppp_sta.ln2, col = colors[1], lwd = 2)
      lines(rp_gev, ppp_hom.ln2, col = colors[1], lwd = 2, lty = 2)
      lines(rp_gev, ppp_het.ln2, col = colors[1], lwd = 2, lty = 3)
      lines(rp_gev, ppp_sta.ln3, col = colors[2], lwd = 2)
      lines(rp_gev, ppp_hom.ln3, col = colors[2], lwd = 2, lty = 2)
      lines(rp_gev, ppp_het.ln3, col = colors[2], lwd = 2, lty = 3)
      legend("bottomright", 
             c("Stationary", 
               "Trend in Mean",
               "Trend in Mean + Cv"), 
             lty = c(1,2,3),
             lwd = 2)
      
      
    }
    if(dis=="gev"){
      dis = "GEV"
      # Stationarity
      g_x = length(used_data)*sum((used_data-mean(used_data))^3)/
        ((length(used_data)-1)*(length(used_data)-2)*sqrt(vr_x)^3)
      if((g_x<=1.15)&(g_x>=-0.7)){
        k_sta = 0.0087*g_x^3 + 0.0582^2 - 0.32*g_x + 0.2778
      }else{
        k_sta = -.31158*(1-exp(-.4556*(g_x - 0.97134)))
      }
      
      a_sta = sign(k_sta)*k_sta*sqrt(vr_x)/
        sqrt(gamma(1+2*k_sta)-(gamma(1+2*k_sta))^2)
      e_sta = mean(used_data) - a_sta*(gamma(1+k_sta)-1)/k_sta
      
      pp = seq(0.01,1, length.out = 10001)
      rp_gev = (1-pp)^-1
      ppp_sta.gev = e_sta + (a_sta/k_sta)*(1-(-log(pp))^k_sta)
      
      # Nonstationarity
      # Homogeneous
      if((sk_x.w<=1.15)&(sk_x.w>=-0.7)){
        k_non.hom = 0.0087*sk_x.w^3 + 0.0582^2 - 0.32*sk_x.w + 0.2778
      }else{
        k_non.hom = -.31158*(1-exp(-.4556*(sk_x.w - 0.97134)))
      }
      mu_x.w.hom = exp(mean.hom + (sd.hom^2)/2)
      vr_x.w.hom = exp(2*mean.hom + sd.hom^2)*(exp(sd.hom^2)-1)
      cv_x.w.hom = sqrt(exp(sd.hom^2)-1)
      sk_x.w.hom = 3*cv_x.w.hom + cv_x.w.hom^3
      
      a_non.hom = sign(k_non.hom)*k_non.hom*sqrt(vr_x.w.hom)/
        sqrt(gamma(1+2*k_non.hom)-(gamma(1+2*k_non.hom))^2)
      e_non.hom = mean(used_data) - a_non.hom*(gamma(1+k_non.hom)-1)/k_non.hom
      ppp_hom.gev = e_non.hom + (a_non.hom/k_non.hom)*(1-(-log(pp))^k_non.hom)
      
      # Heterogeneous
      k_non.het = k_non.hom
      mu_x.w.het = exp(mean.het + (sd.het^2)/2)
      vr_x.w.het = exp(2*mean.het + sd.het^2)*(exp(sd.het^2)-1)
      cv_x.w.het = sqrt(exp(sd.het^2)-1)
      sk_x.w.het = 3*cv_x.w.het + cv_x.w.het^3
      
      a_non.het = sign(k_non.het)*k_non.het*sqrt(vr_x.w.het)/
        sqrt(gamma(1+2*k_non.het)-(gamma(1+2*k_non.het))^2)
      e_non.het = mean(used_data) - a_x*(gamma(1+k_non.het)-1)/k_non.het
      ppp_het.gev = e_non.het + (a_non.het/k_non.het)*(1-(-log(pp))^k_non.het)
      
      last1700 = tail(which(rp_gev<1701),1)
      gev.com = c(ppp_sta.gev, ppp_hom.gev, ppp_het.gev)[1:last1700]
      gev.max = signif(ceiling(max(gev.com)), digits = 1)
      gev.min = signif(round_any(min(gev.com), 10, f = ceiling), digits = 1)
      plot(rp_gev,ppp_sta.gev, log="x", typ="n", 
           xlim = c(1,1700), ylim = c(gev.min,gev.max),
           xlab = "Return Period",
           ylab = "Recurrence Level (mph)",
           xaxt = "n")
      axis(side=1, labels = T, at=c(100,300,700,1700))
      abline(v=c(100, 300,700, 1700), 
             h=seq(40,160,5), 
             col="gray", lty=3)
      lines(rp_gev, ppp_sta.gev, lwd = 2)
      lines(rp_gev, ppp_hom.gev, lwd = 2, lty = 2)
      lines(rp_gev, ppp_het.gev, lwd = 2, lty = 3)
      legend("bottomright", 
             c("Stationary", 
               "Trend in Mean",
               "Trend in Mean + Cv"), 
             lty = c(1,2,3),
             lwd = 2)
    }
    cl = cl + 1
  }
}
for (clu in clu.f:(clu.f+1)){
  max_clu = get(paste("maxsos",clu,sep=""))
  colclu = match(c("lat",max_clu[2,c(2:3)]),colnames(kdata.sca))
  clusters = pam(kdata.sca[,colclu], clu)
  
  # Plotting
  cl = 1
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    
    file = paste0("station_matrix_",
                  kdata.sca[cl.index,1],
                  "_update.xlsx")
    
    dm.table = cbind(kdata[cl.index,1],
                     wanted[cl.index,14],
                     cov[cl.index,3],
                     lmom[cl.index,8:11])
    colnames(dm.table)[1:3] = c("id", "n", "mean")
    dm.test = regtst(dm.table[,1:6],5000)
    is.dm.ex = 
      length(which(as.numeric(dm.test$D)>dm.test$Dcrit[1]))
    n.good.fit = length(which(abs(dm.test$Z)<1.64))
    
    # Check if there are discordant stations
    if(is.dm.ex>0){
      file_r = paste0("station_matrix_",
                      dm.sta,
                      "_update.xlsx")
      file = file[which(!(file %in% file_r))]
    }
    
    yearr = as.data.frame(
      matrix(, nrow = , ncol = 2))
    
    f = 1
    ## Identify beginning and ending years
    while(f<length(file)+1){
      data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
      
      
      # Delete unnecessary rows until first data
      data_gg = data_g[(grep("^Date", 
                             data_g[,1])+1):length(data_g[,1]),]
      
      # assign data_gg column names as data_g's row 6 
      # because it has column names aligned correctly
      colnames(data_gg) <- data_g[6,]
      
      # Date/Time Format originally MM/DD/YYYY HH:MM
      # Distorted to Excel Serial Date (start from 1900-01-01)
      # numbers while loading. Hence, these are  
      # converted to YYYY-MM-DD HH:MM format
      data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                               + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data_gg[,2] = as.numeric(data_gg[,2])
      data_gg[,3] = as.numeric(data_gg[,3])
      
      # Read storm type
      # Read storm type
      
      if(pv==1){
        # Non-thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
      }
      if(pv==2){
        # Thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
      }
      if(pv==3){
        # Tropical Storm
        data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
      }
      
      # Obtain minimum and maximum years of each station
      yearr[f,1] = min(unique(year(data_gg[,1])))
      yearr[f,2] = max(unique(year(data_gg[,1])))
      
      f = f + 1
    }
    
    super_data = as.data.frame(
      matrix(, nrow = length(min(yearr[,1]):max(yearr[,2])), 
             ncol = 2*length(file)))
    super_data[,seq(2,length(file)*2,2)] = 
      min(yearr[,1]):max(yearr[,2])
    
    f = 1
    
    # Place max accordingly
    while(f<length(file)+1){
      data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
      
      # Delete unnecessary rows until first data
      data_gg = data_g[(grep("^Date", 
                             data_g[,1])+1):length(data_g[,1]),]
      
      # assign data_gg column names as data_g's row 6 
      # because it has column names aligned correctly
      colnames(data_gg) <- data_g[6,]
      
      # Date/Time Format originally MM/DD/YYYY HH:MM
      # Distorted to Excel Serial Date (start from 1900-01-01)
      # numbers while loading. Hence, these are  
      # converted to YYYY-MM-DD HH:MM format
      data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                               + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data_gg[,2] = as.numeric(data_gg[,2])
      data_gg[,3] = as.numeric(data_gg[,3])
      
      # Read storm type
      if(pv==2){
        # Non-thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
      }
      if(pv==3){
        # Thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
      }
      if(pv==4){
        # Tropical Storm
        data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
      }
      
      # Annual
      {
        i = 1
        j = 1
        k = 1
        # no_YEARS: how many data points are there?
        no_YEARS = length(unique(year(data_gg[,1])))
        year_a = as.data.frame(matrix(, nrow = no_YEARS, ncol = 2))
        data_gg[,14] = year(data_gg[,1])
        while(k<(no_YEARS)){
          # Identify which data row is the last one
          # from all of the year n
          while(data_gg[i + j - 1,14] == data_gg[i + j,14]){
            j = j + 1
          }
          j = i + j - 1
          
          if(m == 1){
            # Mean
            year[k] = sum(data_gg[i:j,3])/length(i:j)
            type = "Mean"
          }else if(m == 2){
            # Median
            year[k] = median(data_gg[i:j,3]) 
            type = "Median"
          }else if(m == 3){
            # Coefficient of Variation
            year[k] = sd(data_gg[i:j,3])/
              (sum(data_gg[i:j,3])/length(i:j)) 
            type = "Coefficient of Variation"
          }else if(m == 4){
            # Maximum
            year_a[k,1] = max(data_gg[i:j,3])
            year_a[k,2] = as.character(data_gg[i,1])
            year_a[k,3] = year(data_gg[i,1])
            type = "Maximum"
          }else if(m == 5){
            # Frequency
            year[k] = length(i:j)
            type = "Frequency"
          }else{
            # L-Moments
            L = Lmoments(data_gg[i:j,3], na.rm=TRUE)
            lcv = L[2]/L[1] # L-cv
            lsk = L[3]/L[2] # L-Skew
            lku = L[4]/L[2] # L-Kurtosis
            
            l1 = fit$mle[1]/(1+fit$mle[2])
            l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
            
            t2[k,1] = l2/l1
            t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
            
            if(n == 1){
              year[k] = lcv
              type = "L-CV"
            }else if(n == 2){
              year[k] = lsk
              type = "L-Skewness"
            }else{
              year[k] = lku
              type = "L-Kurtosis"
            }
          }
          
          # i variable is added by 1
          # to move on to the next year
          i = j + 1
          j = 1
          k = k + 1
          if(k == (no_YEARS)){
            last = length(data_gg[,3])
            if(m == 1){
              year[k] = sum(data_gg[i:last,3])/length(i:last)
            }else if(m == 2){
              year[k] = median(data_gg[i:last,3])
            }else if(m == 3){
              year[k] = sd(data_gg[i:last,3])/
                (sum(data_gg[i:last,3])/length(i:last)) 
            }else if(m == 4){
              year_a[k,1] = max(data_gg[i:last,3])
              year_a[k,2] = as.character(data_gg[last,1])
              year_a[k,3] = year(data_gg[last,1])
            }else if(m == 5){
              year[k] = length(i:last)
            }else{
              L = Lmoments(data_gg[i:last,3], na.rm=TRUE)
              lcv = L[2]/L[1] # L-cv
              lsk = L[3]/L[2] # L-Skew
              lku = L[4]/L[2] # L-Kurtosis
              
              l1 = fit$mle[1]/(1+fit$mle[2])
              l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
              
              t2[k,1] = l2/l1
              t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
              
              if(n == 1){
                year[k] = lcv
              }else if(n == 2){
                year[k] = lsk
              }else{
                year[k] = lku
              }
            }
            k = k + 1
          }
        }
        used_data = year_a[,1]
        used_date = year_a[,2]
      }
      
      
      # Obtain desired data
      super_data[,2*(f-1)+1] = 
        ifelse(!is.na(match(super_data[,2*(f)],year_a[,3])),
               year_a[,1],NA)
      
      f = f + 1
    }
    
    # Obtain max from every row, ignoring NA
    cus.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
    
    if(dim(super_data)[2]<3){
      super_data = super_data[which(!is.na(super_data[,1])),]
      yearr_t = super_data[,2]
      used_data = super_data[,1]
    }else{
      # Superstation values
      used_data = apply(super_data[,seq(1,length(file)*2-1,2)],1, cus.max)
      yearr_t = min(yearr[,1]):max(yearr[,2])
    }
    
    # Remove data points that are NA (not recorded)
    # by indexing together
    t = yearr_t
    t[which(is.na(used_data))] = NA
    used_data <- used_data[!is.na(used_data)]
    t <- t[!is.na(t)]
    
    # Significance
    # Linear Regression Model
    sta.lm = lm(log(used_data)~t)
    
    # P-Values for Intercept and Trend Coefficients
    t1 = sta.lm$coefficients[2]/
      summary(sta.lm)$coefficients[2,2]
    
    alp = pt(t1,df = length(used_data)-2, lower.tail = F)
    
    t2 = qt(1-alp,df = length(used_data)-2, lower.tail = T) - 
      1/sqrt((1/(cor(log(used_data),t)^2))-1)*sqrt(length(used_data))
    
    # Log transformation and linear regression
    ln_used_data = log(used_data)
    lm.year = lm(ln_used_data~t)
    m.year = lm(used_data~t)
    sta.data = m.year$coefficients[1]+m.year$coefficients[2]*t
    rho = cor(ln_used_data,t)
    
    # Conditional Moments of y
    mu_y.w = lm.year$coefficients[1] + 
      lm.year$coefficients[2]*t
    vr_y.w = (sd(ln_used_data)^2)*(1-rho^2)
    sk_y.w = skewness(ln_used_data) - 
      skewness(t)*(lm.year$coefficients[2]^3)
    
    # Conditional Moments of x
    mu_x.w = exp(mu_y.w + (vr_y.w)/2)
    vr_x.w = exp(2*mu_y.w + vr_y.w)*(exp(vr_y.w)-1)
    cv_x.w = sqrt(exp(vr_y.w)-1)
    sk_x.w = 3*cv_x.w + cv_x.w^3
    
    # Stationary
    mean.sta = mean(ln_used_data)
    sd.sta = sqrt(sum((ln_used_data - 
                         mean(ln_used_data))^2)/
                    length(ln_used_data))
    
    YR = c(10,20,30)
    LLL = as.data.frame(matrix(, nrow = 4, ncol = 9))
    
    t_i = 1
    while(t_i<4){
      t_non = c(tail(t,1),2020,2030)
      # Nonstationary - Mean
      mean.hom = unname(lm.year$coefficients[1] + 
                          lm.year$coefficients[2]*t_non[t_i])
      sd.hom = unname(sqrt(var(ln_used_data)-
                             lm.year$coefficients[2]^2*
                             var(t)))
      
      # Nonstationary - Mean + Cv
      res = (((lm.year$residuals)^(2))^(1/3))
      lm.res = lm(res~t)
      mean.het = mean.hom
      sd.het = sqrt((lm.res$coefficients[1]+
                       lm.res$coefficients[2]*tail(t,1))^3 +
                      3*(summary(lm.res)$sigma^2)*
                      (lm.res$coefficients[1]+
                         lm.res$coefficients[2]*t_non[t_i]))
      
      # Generate return period data points
      rp = seq(1.01,1700, length.out = 10001)
      
      ppp_sta.ln2 = exp(mean.sta + qnorm(1-(1/rp))*sd.sta)
      ppp_hom.ln2 = exp(mean.hom + qnorm(1-(1/rp))*sd.hom)
      ppp_het.ln2 = exp(mean.het + qnorm(1-(1/rp))*sd.het)
      
      
      if(t_i==1){
        png(filename=paste0(dir,"/plots/",texty[pv],
                            "/cOPT",clu,"_",cl,".png"),
            w = 800, h = 600)
        par(mfrow=c(1,2))
        plot(t,ln_used_data, xlab = "Year",
             ylab = "ln (Peak Wind Gust)",,
             xaxt = "n",
             yaxt = "n",
             cex.lab = 1.1)
        axis(side=1, 
             at = c(1980,1990,2000,2010),
             cex.axis = 1.5)
        axis(side=2, 
             cex.axis = 1.5)
        abline(lm(ln_used_data~t))
        
        plot(t, res, xlab = "Year",
             ylab = expression(epsilon^{2/3}),
             xaxt = "n",
             yaxt = "n",
             cex.lab = 1.1)
        abline(lm(res~t))
        axis(side=1, 
             at = c(1980,1990,2000,2010),
             cex.axis = 1.5)
        axis(side=2, 
             cex.axis = 1.5)
        dev.off()
        
        max.mean = unname(lm.year$coefficients[1] + 
                            lm.year$coefficients[2]*2030)
        max.sd = sqrt((lm.res$coefficients[1]+
                         lm.res$coefficients[2]*2030)^3 +
                        3*(summary(lm.res)$sigma^2)*
                        (lm.res$coefficients[1]+
                           lm.res$coefficients[2]*2030))
        max.hom = exp(max.mean + qnorm(1-(1/rp))*sd.hom)
        max.het = exp(max.mean + qnorm(1-(1/rp))*max.sd)
        ln2.max = round_any(max(max.hom,max.het,ppp_sta.ln2),
                            10, 
                            f = ceiling)
        if((pv==4)&(cl==2)){
          ln2.max = 180
        }
        ln2.min = round_any(min(c(ppp_sta.ln2,ppp_hom.ln2,ppp_het.ln2)),
                            10, f = floor)
        
        
        par(mar = c(4,4,2,2))
        png(filename=paste0(dir,"/plots/",texty[pv],
                            "/opt",clu,"_",cl,".png"),
            w = 800, h = 600)
        plot(rp,ppp_sta.ln2, log = "x", typ="n", 
             xlim = c(1,3500), ylim = c(ln2.min,ln2.max),
             xlab = "Return Period",
             ylab = "Peak wind gust (mph)",
             xaxt = "n",
             yaxt = "n",
             cex.lab = 1.5)
        axis(side=1, labels = T, 
             at=c(1, 10, 50, 100,300,700,1700),
             cex.axis = 1.5)
        axis(side=2, labels = T, 
             at=seq(40,240,10),
             cex.axis = 1)
        abline(v=c(1, 10, 50, 100, 300,700, 1700), 
               h=seq(40,240,10), 
               col="gray", lty=3)
        legend("bottomright", 
               c("Stationary", 
                 "Trend in Mean",
                 "Trend in Mean + Cv"), 
               lty = c(1,2,3),
               lwd = 2,
               cex = 1.5)
        lines(rp, ppp_sta.ln2, col = colors[cl], lwd = 2)
      }
      
      
      lines(rp, ppp_hom.ln2, col = colors[cl], lwd = 1+t_i, lty = 2)
      lines(rp, ppp_het.ln2, col = colors[cl], lwd = 1+t_i, lty = 3)
      text(x= 1700, y= max(ppp_hom.ln2), cex = 0.75,
           pos = 4, labels = paste(t_non[t_i], "HOM"))
      text(x= 1700, y= max(ppp_het.ln2), cex = 0.75,
           pos = 4, labels = paste(t_non[t_i], "HET"))
      
      LLL[1,t_i] = ppp_sta.ln2[which.min(abs(rp-100))]
      LLL[2,t_i] = ppp_sta.ln2[which.min(abs(rp-300))]
      LLL[3,t_i] = ppp_sta.ln2[which.min(abs(rp-700))]
      LLL[4,t_i] = ppp_sta.ln2[which.min(abs(rp-1700))]
      LLL[1,t_i + 3] = ppp_hom.ln2[which.min(abs(rp-100))]
      LLL[2,t_i + 3] = ppp_hom.ln2[which.min(abs(rp-300))]
      LLL[3,t_i + 3] = ppp_hom.ln2[which.min(abs(rp-700))]
      LLL[4,t_i + 3] = ppp_hom.ln2[which.min(abs(rp-1700))]
      LLL[1,t_i + 6] = ppp_het.ln2[which.min(abs(rp-100))]
      LLL[2,t_i + 6] = ppp_het.ln2[which.min(abs(rp-300))]
      LLL[3,t_i + 6] = ppp_het.ln2[which.min(abs(rp-700))]
      LLL[4,t_i + 6] = ppp_het.ln2[which.min(abs(rp-1700))]
      
      if(t_i==3){
        assign(paste("LLL",clu,cl,YR[t_i], sep = "_"),
               LLL)
      }
      t_i = t_i + 1
    }
    
    dev.off()
    
    if(dis=="pe3"){
      source("pe3.R")
    }
    if(dis=="gno"){
      source("gno.R")
    }
    if(dis=="gev"){
      source("gev.R")
    }
    cl = cl + 1
  }
}
cl.tabl[which(cl.tabl[,5]<1),11] = "AHom"
cl.tabl[which(cl.tabl[,5]>2),11] = "DHet"
cl.tabl[which((cl.tabl[,5]<2)&(cl.tabl[,5]>1)),11] = "PHet"
colnames(cl.tabl) = c("Cluster","Cluster ID", "N", "N_d", 
                      "H","SlopeC","SlopeP",
                      "TypeI","TypeII","MK","Type",
                      "Var1","Var2","Var3")
emptycol = as.data.frame(
  matrix(".", nrow = dim(cl.tabl)[1], ncol = 1))
cl.tabl = cbind(lll.cl.tabl,emptycol,cl.tabl)
write.csv(cl.tabl,file = paste0(dir,"/plots/",
                                texty[pv],
                                "/table11f.csv"))

# Slope Coefficient Plot
# 3 Pval, 4 Type I, 5 Type II, 6 MK
sig_tar = 4
clu.f = floor(sqrt(dim(wanted)[1]/2))
for(clu in clu.f:(clu.f+1)){
  
  ref_st = read.csv(paste0(dir,"/plots/table.csv"), header = F)
  
  if(pv==1){
    colnames(ref_st) = sapply(ref_st[2,], as.character)
    ref_st = ref_st[3:9,]
    ref_st = ref_st[,(which(ref_st ==".", arr.ind= T)[2,2]+1):dim(ref_st)[2]]
  }else if(pv==2){
    colnames(ref_st) = sapply(ref_st[12,], as.character)
    ref_st = ref_st[13:19,]
    ref_st = ref_st[,(which(ref_st ==".", arr.ind= T)[2,2]+1):dim(ref_st)[2]]
  }else if(pv==3){
    colnames(ref_st) = sapply(ref_st[22,], as.character)
    ref_st = ref_st[23:27,]
    ref_st = ref_st[,(which(ref_st ==".", arr.ind= T)[2,2]+1):dim(ref_st)[2]]
    
  }else if(pv == 4){
    colnames(ref_st) = sapply(ref_st[30,], as.character)
    ref_st = ref_st[31:35,]
    ref_st = ref_st[,(which(ref_st ==".", arr.ind= T)[2,2]+1):dim(ref_st)[2]]
    
  }
  
  clusters = pam(kdata.sca[,colclu], clu)
  das = c(1:clu)
  cl = 1
  
  # Go to every cluster...
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    dm.table = cbind(kdata[cl.index,1],
                     wanted[cl.index,14],
                     cov[cl.index,3],
                     lmom[cl.index,8:11])
    colnames(dm.table)[1:3] = c("id", "n", "mean")
    dm.test = regtst(dm.table[,1:6],5000)
    is.dm.ex = 
      length(which(as.numeric(dm.test$D)>dm.test$Dcrit[1]))
    
    # Check if there are discordant stations
    if(is.dm.ex>0){
      dm.sta = 
        dm.table[which(as.numeric(dm.test$D)>dm.test$Dcrit[1]),1]
      dm.point = cbind(dis_ang[match(dm.sta,dis_ang[,2]),c(9,10)],cl)
    }
    f = 1
    
    ## Obtain desired data and remove discordant stations
    file = paste0("station_matrix_",
                  kdata.sca[which(clusters$clustering==cl),1],
                  "_update.xlsx")
    if(is.dm.ex>0){
      file_r = paste0("station_matrix_",
                      dm.sta,
                      "_update.xlsx")
      file = file[which(!(file %in% file_r))]
    }
    yearr = as.data.frame(
      matrix(, nrow = , ncol = 2))
    
    sig_stat = as.data.frame(
      matrix(, nrow = length(file), 
             ncol = 6))
    colnames(sig_stat) = c("id", "Pval", "TypeI",
                           "TypeII", "MK", "SlopeC")
    
    f = 1
    
    # Obtain statistics
    while(f<length(file)+1){
      data_g = read.xlsx(paste(dir,"/Lower_48/",file[f],sep=""))
      
      # Delete unnecessary rows until first data
      data_gg = data_g[(grep("^Date", 
                             data_g[,1])+1):length(data_g[,1]),]
      
      # assign data_gg column names as data_g's row 6 
      # because it has column names aligned correctly
      colnames(data_gg) <- data_g[6,]
      
      # Date/Time Format originally MM/DD/YYYY HH:MM
      # Distorted to Excel Serial Date (start from 1900-01-01)
      # numbers while loading. Hence, these are  
      # converted to YYYY-MM-DD HH:MM format
      data_gg[,1] <-as.POSIXct(as.numeric(data_gg[,1])*24*3600
                               + as.POSIXct("1899-12-30 00:00"))
      
      # Convert strings into numbers
      data_gg[,2] = as.numeric(data_gg[,2])
      data_gg[,3] = as.numeric(data_gg[,3])
      
      # Read storm type
      if(pv==2){
        # Non-thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,5]!="1")),]
      }
      if(pv==3){
        # Thunderstorm
        data_gg <- data_gg[-c(which(data_gg[,7]!="1")),]
      }
      if(pv==4){
        # Tropical Storm
        data_gg <- data_gg[-c(which(data_gg[,10]!="1")),]
      }
      
      # Annual
      {
        i = 1
        j = 1
        k = 1
        # no_YEARS: how many data points are there?
        no_YEARS = length(unique(year(data_gg[,1])))
        year_a = as.data.frame(matrix(, nrow = no_YEARS, ncol = 2))
        data_gg[,14] = year(data_gg[,1])
        while(k<(no_YEARS)){
          # Identify which data row is the last one
          # from all of the year n
          while(data_gg[i + j - 1,14] == data_gg[i + j,14]){
            j = j + 1
          }
          j = i + j - 1
          
          if(m == 1){
            # Mean
            year[k] = sum(data_gg[i:j,3])/length(i:j)
            type = "Mean"
          }else if(m == 2){
            # Median
            year[k] = median(data_gg[i:j,3]) 
            type = "Median"
          }else if(m == 3){
            # Coefficient of Variation
            year[k] = sd(data_gg[i:j,3])/
              (sum(data_gg[i:j,3])/length(i:j)) 
            type = "Coefficient of Variation"
          }else if(m == 4){
            # Maximum
            year_a[k,1] = max(data_gg[i:j,3])
            year_a[k,2] = as.character(data_gg[i,1])
            year_a[k,3] = year(data_gg[i,1])
            type = "Maximum"
          }else if(m == 5){
            # Frequency
            year[k] = length(i:j)
            type = "Frequency"
          }else{
            # L-Moments
            L = Lmoments(data_gg[i:j,3], na.rm=TRUE)
            lcv = L[2]/L[1] # L-cv
            lsk = L[3]/L[2] # L-Skew
            lku = L[4]/L[2] # L-Kurtosis
            
            l1 = fit$mle[1]/(1+fit$mle[2])
            l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
            
            t2[k,1] = l2/l1
            t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
            
            if(n == 1){
              year[k] = lcv
              type = "L-CV"
            }else if(n == 2){
              year[k] = lsk
              type = "L-Skewness"
            }else{
              year[k] = lku
              type = "L-Kurtosis"
            }
          }
          
          # i variable is added by 1
          # to move on to the next year
          i = j + 1
          j = 1
          k = k + 1
          if(k == (no_YEARS)){
            last = length(data_gg[,3])
            if(m == 1){
              year[k] = sum(data_gg[i:last,3])/length(i:last)
            }else if(m == 2){
              year[k] = median(data_gg[i:last,3])
            }else if(m == 3){
              year[k] = sd(data_gg[i:last,3])/
                (sum(data_gg[i:last,3])/length(i:last)) 
            }else if(m == 4){
              year_a[k,1] = max(data_gg[i:last,3])
              year_a[k,2] = as.character(data_gg[last,1])
              year_a[k,3] = year(data_gg[last,1])
            }else if(m == 5){
              year[k] = length(i:last)
            }else{
              L = Lmoments(data_gg[i:last,3], na.rm=TRUE)
              lcv = L[2]/L[1] # L-cv
              lsk = L[3]/L[2] # L-Skew
              lku = L[4]/L[2] # L-Kurtosis
              
              l1 = fit$mle[1]/(1+fit$mle[2])
              l2 = fit$mle[1]/((1+fit$mle[2])*(2+fit$mle[2]))
              
              t2[k,1] = l2/l1
              t3[k,1] = (1-fit$mle[2])/(3+fit$mle[2])
              
              if(n == 1){
                year[k] = lcv
              }else if(n == 2){
                year[k] = lsk
              }else{
                year[k] = lku
              }
            }
            k = k + 1
          }
        }
        used_data = year_a[,1]
        used_date = year_a[,3]
      }
      
      t = used_date - min(used_date)
      
      # Significance
      # Linear Regression Model
      sta.lm = lm(log(used_data)~t)
      
      # P-Values for Intercept and Trend Coefficients
      t1 = sta.lm$coefficients[2]/
        summary(sta.lm)$coefficients[2,2]
      
      alp = pt(t1,df = length(used_data)-2, lower.tail = F)
      
      t2 = qt(1-alp,df = length(used_data)-2, lower.tail = T) - 
        1/sqrt((1/(cor(log(used_data),t)^2))-1)*sqrt(length(used_data))
      
      ## Manual MK
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
      mk = 2*(1-pnorm(abs(mk_z)))
      
      
      colnames(sig_stat) = c("id", "SlopeC", "Pval", 
                             "TypeI", "TypeII", "MK")
      sig_stat[f,1] = as.numeric(substr(file[f],16,21))
      sig_stat[f,2] = format(round(
        summary(sta.lm)$coefficients[2,1], 5), nsmall = 5)
      sig_stat[f,3] = format(round(
        summary(sta.lm)$coefficients[2,4], 5), nsmall = 5)
      sig_stat[f,4] = format(round(alp,5), nsmall = 5)
      sig_stat[f,5] = format(round(
        pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
      sig_stat[f,6] = mk
      
      f = f + 1
    }
    
    
    if(cl==1){
      par(mar = c(4,4,2,2))
      png(filename=paste0(dir,"/plots/",texty[pv],
                          "/slopeoptC",clu,".png"),
          w = 800, h = 600)
      plot(das,
           seq(-.03,.03, length.out=clu),
           type = "n",
           xlab = "Clusters",
           ylab = "Slope Coefficient",
           xaxt = "n",
           xlim = c(0.5,clu+0.5),
           ylim = c(-0.03,0.03),
           cex.lab = 1.5)
    }
    
    if(as.numeric(sapply((
      subset(ref_st,ref_st[,1]==clu&ref_st[,2]==cl)[sig_tar+4]),
      as.character))>0.05){
      rect(cl-.5,-1,cl+.5,1,col = rgb(0.5,0.5,0.5,1/4),
           border = NA)
    }
    # Significant points
    cl_si = which(sig_stat[,sig_tar]<0.05)
    points(seq(cl,cl,length.out=length(cl_si)),
           sig_stat[cl_si,2],
           pch = 3,cex = 2)
    # Insignificant points
    cl_insi = which(sig_stat[,sig_tar]>0.050001)
    points(seq(cl,cl,length.out=length(cl_insi)),
           sig_stat[cl_insi,2],
           pch = 19,cex = 0.5)
    # Superstation point
    points(cl,
           subset(ref_st,ref_st[,1]==clu.f&ref_st[,2]==cl)[6],
           pch= 18, col = "red")
    
    abline(h = 0, col = "gray", lty = "dotted")
    axis(1, at=cl, labels = cl, tick = T)
    cl = cl + 1
  }
  legend("bottomleft",
         legend=c("Insignificant",
                  "Significant",
                  "Cluster Superstation"),
         col=c("black","black","red"),
         pch=c(19,3,18),
         pt.cex=c(0.5,2,1))
  dads = as.numeric(sapply(ref_st[which(ref_st[,1]==clu.f),6],
                           as.character))
  points(das,dads[das], pch= 18, col = "red")
  dev.off
}