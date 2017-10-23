i = 1
j = 1
k = 1
year = as.data.frame(matrix(, nrow = no_YEARS, ncol = 2))
data_u[,14] = year(data_u[,1])
while(k<(no_YEARS)){
  # Identify which data row is the last one
  # from all of the year n
  while(data_u[i + j - 1,14] == data_u[i + j,14]){
    j = j + 1
  }
  j = i + j - 1
  
  if(m == 1){
    # Mean
    year[k,1] = sum(data_u[i:j,3])/length(i:j)
    type = "Mean"
  }else if(m == 2){
    # Median
    year[k,1] = median(data_u[i:j,3]) 
    type = "Median"
  }else if(m == 3){
    # Coefficient of Variation
    year[k,1] = sd(data_u[i:j,3])/
      (sum(data_u[i:j,3])/length(i:j)) 
    type = "Coefficient of Variation"
  }else if(m == 4){
    # Maximum
    year[k,1] = max(data_u[i:j,3])
    year[k,2] = data_u[i,1]
    year[k,3] = year(data_u[i,1])
    year[k,4] = data[match(max(data_u[i:j,3]),
                           data_u[,3]),4]
    type = "Maximum"
  }else {
    # Frequency
    year[k,1] = length(i:j)
    type = "Frequency"
  }
  
  # i variable is added by 1
  # to move on to the next year
  i = j + 1
  j = 1
  k = k + 1
  if(k == (no_YEARS)){
    last = length(data_u[,3])
    if(m == 1){
      year[k,1] = sum(data_u[i:last,3])/length(i:last)
    }else if(m == 2){
      year[k,1] = median(data_u[i:last,3])
    }else if(m == 3){
      year[k,1] = sd(data_u[i:last,3])/
        (sum(data_u[i:last,3])/length(i:last)) 
    }else if(m == 4){
      year[k,1] = max(data_u[i:last,3])
      year[k,2] = data_u[last,1]
      year[k,3] = year(data_u[last,1])
      year[k,4] = data[match(max(data_u[i:last,3]),
                             data_u[,3]),4]
    }else{
      year[k,1] = length(i:last)
    }
    k = k + 1
  }
}

used_data = year[,1]

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
sc_t = (nume/(used_n-1))/(deno/used_n)

if((sc_t<=1.96/sqrt(length(used_data)))&
   (sc_t>=-1.96/sqrt(length(used_data)))){
  
  # von Storch autocorrelation correction
  mod_i = 2
  for (mod_i in 2:length(used_data)){
    used_data[mod_i] = used_data[mod_i] - 
      sc.temp*used_data[mod_i-1]
  }
}

used_date = year[,3]