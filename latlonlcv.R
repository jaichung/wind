# Lat, Lon, L-CV
cl.tabl = as.data.frame(
  matrix(, nrow = , ncol = 12))
clu.f = floor(sqrt(length(stations_dup[,1])/2))
for (clu in clu.f:(clu.f+1)){
  clusters = pam(kdata.sca[,c(2,3,7)], clu)
  {
    par(mar=c(2,2,2,2))
    png(file=paste0(dir,"/plots/",texty[pv],
                    "/latlonlcv_fff",clu,".png"),
        w = 800, h = 600)
    mappoint= cbind(kdata[,2:3],
                    clusters$clustering)
    newmap <- getMap(resolution = "low")
    # windows(800, 600, pointsize = 12)
    plot(newmap, xlim = c(-80, -65), ylim = c(25, 45), asp = 1)
    legend(-63,36,legend = c(1:clu,"Discordant"),
           bg = "white",col=c(colors[1:clu],"black"), 
           bty = "n", pch = c(rep(20,clu),1), 
           pt.cex = c(rep(3,clu),4), 
           pt.lwd = c(rep(1,clu),3),
           ncol = 1, cex = 1.25)
    
    ll = 1
    while(ll<length(colnames(clusters$medoids))+1){
      text(-63,42.5-ll*1.5,
           capitalize(colnames(clusters$medoids)[ll]),
           cex = 2)
      ll = ll + 1
    }
    
    text(-63,36.5,
         paste0("Si: ",summary(silhouette(clusters))$si.summary[4]),
         cex = 2)
    
    cl = 1
    if(clu!=clu.f){
      dimc = dim(cl.tabl)[1]
    }else{
      dimc = 0
    }
    
    # Identify
    while(cl<length(unique(clusters$clustering))+1){
      cl.index = which(as.numeric(clusters$clustering)==cl)
      dm.table = cbind(kdata[cl.index,1],
                       k_n[cl.index],
                       cov[cl.index,3],
                       lmom[cl.index,8:11])
      colnames(dm.table)[1:3] = c("id", "n", "mean")
      dm.test = regtst(dm.table[,1:6],5000)
      is.dm.ex = 
        length(which(as.numeric(dm.test$D)>dm.test$Dcrit[1]))
      n.good.fit = length(which(abs(dm.test$Z)<1.64))
      
      # Check if there are discordant stations
      if(is.dm.ex>0){
        dm.sta = 
          dm.table[which(as.numeric(dm.test$D)>dm.test$Dcrit[1]),1]
        dm.point = cbind(kdata[match(dm.sta,kdata[,1]),2:3],cl)
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
        n.g = 1
        if(n.good.fit!=0){
          for(n.g in 1:n.good.fit){
            cl.tabl[cl,11+(2*n.g)-1] = sort(abs(dm.test$Z))[n.g]
            cl.tabl[cl,11+(2*n.g)] = attributes(sort(abs(dm.test$Z))[n.g])
          }
        }else{
          cl.tabl[cl,12] = sort(abs(abs(dm.test$Z)-1.64))[1] + 1.64
          cl.tabl[cl,13] = attributes(sort(abs(abs(dm.test$Z)-1.64))[1])
        }
      }else{
        cl.tabl[dimc + cl,1] = clu
        cl.tabl[dimc + cl,2] = cl
        cl.tabl[dimc + cl,3] = length(dm.test$D)
        cl.tabl[dimc + cl,4] = 
          length(which(dm.test$D>dm.test$Dcrit[1]))
        cl.tabl[dimc + cl,5] = dm.test$H[1]
        n.g = 1
        if(n.good.fit!=0){
          for(n.g in 1:n.good.fit){
            cl.tabl[dimc + cl,11+(2*n.g)-1] = sort(abs(dm.test$Z))[n.g]
            cl.tabl[dimc + cl,11+(2*n.g)] = attributes(sort(abs(dm.test$Z))[n.g])
          }
        }else{
          cl.tabl[dimc + cl,12] = sort(abs(abs(dm.test$Z)-1.64))[1] + 1.64
          cl.tabl[dimc + cl,13] = attributes(sort(abs(abs(dm.test$Z)-1.64))[1])
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
        
        source(paste0(dir_fl,"annmax.R"))
        
        # Obtain desired data
        super_data[,2*(f-1)+1] = 
          ifelse(!is.na(match(super_data[,2*(f)],year_a[,3])),
                 year_a[,1],NA)
        
        f = f + 1
      }
      
      # Obtain max from every row, ignoring NA
      cus.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
      
      # Superstation values
      used_data = apply(super_data[,seq(1,length(file)*2-1,2)],1, cus.max)
      used_data = log(used_data)
      yearr_t = min(yearr[,1]):max(yearr[,2])
      
      # Remove data points that are NA (not recorded)
      # by indexing together
      t = yearr_t - min(yearr_t)
      t[which(is.na(used_data))] = NA
      used_data <- used_data[!is.na(used_data)]
      t <- t[!is.na(t)]
      
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
          summary(sta.lm)$coefficients[2,1], 6), nsmall = 5)
        cl.tabl[cl,7] = format(round(
          summary(sta.lm)$coefficients[2,4], 6), nsmall = 5)
        cl.tabl[cl,8] = format(round(alp,5), nsmall = 5)
        cl.tabl[cl,9] = format(round(
          pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
        cl.tabl[cl,10] = mk
      }else{
        cl.tabl[dimc + cl,6] = format(round(
          summary(sta.lm)$coefficients[2,1], 6), nsmall = 5)
        cl.tabl[dimc + cl,7] = format(round(
          summary(sta.lm)$coefficients[2,4], 6), nsmall = 5)
        cl.tabl[dimc + cl,8] = format(round(alp,5), nsmall = 6)
        cl.tabl[dimc + cl,9] = format(round(
          pt(t2,df = length(used_data)-2, lower.tail = T),5), nsmall = 5)
        cl.tabl[dimc + cl,10] = mk
      }
      
      cl = cl + 1
    }
    
    # Plot clusters!
    for(mm in 1:max(clusters$clustering)){
      mmm = which(clusters$clustering %in% mm)
      points(mappoint$lon[mmm],mappoint$lat[mmm],
             col=colors[mm], cex=4, pch = 20)
      mm = mm + 1
    }
    
    # savePlot("clipboard", type="wmf")
    
  }
  
  dev.off()
  # Plotting
  cl = 1
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    
    file = paste0("station_matrix_",
                  kdata.sca[cl.index,1],
                  "_update.xlsx")
    
    dm.table = cbind(kdata[cl.index,1],
                     k_n[cl.index],
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
      
      corst = round(cor(z_ii.s,z_i.s),4)
      corns = round(cor(z_ii.n,z_i.n),4)
      
      
      }
    
    cl = cl + 1
    }
    
  
  }
for (clu in clu.f:(clu.f+1)){
  clusters = pam(kdata.sca[,c(2,3,7)], clu)
  # Plotting
  cl = 1
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    
    file = paste0("station_matrix_",
                  kdata.sca[cl.index,1],
                  "_update.xlsx")
    
    dm.table = cbind(kdata[cl.index,1],
                     k_n[cl.index],
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
      source(paste0(dir_fl,"annmax.r"))
      
      # Obtain desired data
      super_data[,2*(f-1)+1] = 
        ifelse(!is.na(match(super_data[,2*(f)],year_a[,3])),
               year_a[,1],NA)
      
      f = f + 1
    }
    
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
      
      if(dis=="ln2"){
        ppp_sta = exp(mean.sta + qnorm(1-(1/rp))*sd.sta)
        ppp_hom = exp(mean.hom + qnorm(1-(1/rp))*sd.hom)
        ppp_het = exp(mean.het + qnorm(1-(1/rp))*sd.het)
      }
      if(dis=="gev"){
        source(paste0(dir_fl,"gev.R"))
      }
      if(dis=="pe3"){
        source(paste0(dir_fl,"pe3.R"))
      }
      if(dis=="gno"){
        source(paste0(dir_fl,"gno.R"))
      }
      
      if(t_i==1){
        png(filename=paste0(dir,"/plots/",texty[pv],
                            "/cLLL",clu,"_",cl,".png"),
            w = 800, h = 600)
        par(mfrow=c(1,2))
        plot(t,ln_used_data, xlab = "Year",
             ylab = "ln (Peak Wind Gust)",
             xaxt = "n",
             yaxt = "n",
             cex.lab = 1.1)
        abline(lm(ln_used_data~t))
        axis(side=1, 
             at = c(1980,1990,2000,2010),
             cex.axis = 1.5)
        axis(side=2, 
             cex.axis = 1.5)
        
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
        
        par(mar = c(4,4,2,2))
        png(filename=paste0(dir,"/plots/",texty[pv],
                            "/lll",clu,"_",cl,".png"),
            w = 800, h = 600)
        plot(rp,ppp_sta, log = "x", typ="n", 
             xlim = c(1,3500), ylim = c(40,150),
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
                 "Trend in Mean + CV"), 
               lty = c(1,2,3),
               lwd = 2,
               cex = 1.5)
        lines(rp, ppp_sta, col = colors[cl], lwd = 2)
      }
      
      
      lines(rp, ppp_hom, col = colors[cl], lwd = 1+t_i, lty = 2)
      lines(rp, ppp_het, col = colors[cl], lwd = 1+t_i, lty = 3)
      text(x= 1700, y= max(ppp_hom), cex = 1,
           pos = 4, labels = paste(t_non[t_i], "HOM"))
      text(x= 1700, y= max(ppp_het), cex = 1,
           pos = 4, labels = paste(t_non[t_i], "HET"))
      
      LLL[1,t_i] = ppp_sta[which.min(abs(rp-100))]
      LLL[2,t_i] = ppp_sta[which.min(abs(rp-300))]
      LLL[3,t_i] = ppp_sta[which.min(abs(rp-700))]
      LLL[4,t_i] = ppp_sta[which.min(abs(rp-1700))]
      LLL[1,t_i + 3] = ppp_hom[which.min(abs(rp-100))]
      LLL[2,t_i + 3] = ppp_hom[which.min(abs(rp-300))]
      LLL[3,t_i + 3] = ppp_hom[which.min(abs(rp-700))]
      LLL[4,t_i + 3] = ppp_hom[which.min(abs(rp-1700))]
      LLL[1,t_i + 6] = ppp_het[which.min(abs(rp-100))]
      LLL[2,t_i + 6] = ppp_het[which.min(abs(rp-300))]
      LLL[3,t_i + 6] = ppp_het[which.min(abs(rp-700))]
      LLL[4,t_i + 6] = ppp_het[which.min(abs(rp-1700))]
      if(t_i==3){
        assign(paste("LLL",clu,cl,YR[t_i], sep = "_"),
               LLL)
      }
      t_i = t_i + 1
    }
    par(fig = c(grconvertX(c(1, 10), from="user", to="ndc"),
                grconvertY(c(110, 155), from="user", to="ndc")),
        mar = c(1,0,1,0),
        new = TRUE)
    plot(newmap, xlim = c(-80, -65), ylim = c(25, 45), asp = 1)
    clm = which(clusters$clustering %in% cl)
    points(mappoint$lon[clm],mappoint$lat[clm],
           col=colors[cl], cex=1.25, pch = 20)
    dev.off()
    
    cl = cl + 1
  }
}
cl.tabl[which(cl.tabl[,5]<1),11] = "AHom" 
cl.tabl[which(cl.tabl[,5]>2),11] = "DHet" 
cl.tabl[which((cl.tabl[,5]<2)&(cl.tabl[,5]>1)),11] = "PHet"
colnames(cl.tabl) = c("Cluster", "Cluster ID", "N", 
                      "N_d", "H", "SlopeC",  
                      "SlopeP", "TypeI", "TypeII", 
                      "MK", "Type")
lll.cl.tabl = cl.tabl


# Slope Coefficient Plot
# 3 Pval, 4 Type I, 5 Type II, 6 MK
sig_tar = 4
clu.f = floor(sqrt(dim(wanted)[1]/2))
for(clu in clu.f:(clu.f+1)){
  
  ref_st = read.csv(paste0(dir,"/plots/table.csv"), header = F)
  
  if(pv==1){
    colnames(ref_st) = sapply(ref_st[2,], as.character)
    ref_st = ref_st[3:9,1:11]
    
  }else if(pv==2){
    colnames(ref_st) = sapply(ref_st[12,], as.character)
    ref_st = ref_st[13:19,1:11]
    
  }else if(pv==3){
    colnames(ref_st) = sapply(ref_st[22,], as.character)
    ref_st = ref_st[23:27,1:11]
  }else if(pv == 4){
    colnames(ref_st) = sapply(ref_st[30,], as.character)
    ref_st = ref_st[31:35,1:11]
  }
  
  clusters = pam(kdata.sca[,c(2,3,7)], clu)
  das = c(1:clu)
  cl = 1
  
  # Go to every cluster...
  while(cl<length(unique(clusters$clustering))+1){
    
    cl.index = which(as.numeric(clusters$clustering)==cl)
    dm.table = cbind(kdata[cl.index,1],
                     k_n[cl.index],
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
                          "/slopelllC",clu,".png"),
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
         pt.cex=c(0.5,2,1),
         cex = 1.5)
  dads = as.numeric(sapply(ref_st[which(ref_st[,1]==clu),6],
                           as.character))
  points(das,dads[das], pch= 18, col = "red")
  dev.off()
}