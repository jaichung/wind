vr_x = sum((used_data-mean(used_data))^2)/
  (length(used_data)-1)

# Stationarity
g_x = length(used_data)*sum((used_data-mean(used_data))^3)/
  ((length(used_data)-1)*(length(used_data)-2)*sqrt(vr_x)^3)
if((g_x<=1.15)&(g_x>=-0.7)){
  k_sta = 0.0087*g_x^3 + 0.0582^2 - 0.32*g_x + 0.2778
}else{
  k_sta = -.31158*(1-exp(-.4556*(g_x - 0.97134)))
}

a_sta = sign(k_sta)*k_sta*sqrt(vr_x)/
  sqrt(gamma(1+2*k_sta)-(gamma(1+k_sta))^2)
e_sta = mean(used_data) - a_sta/((gamma(1+k_sta)-1)*k_sta)

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
  sqrt(gamma(1+2*k_non.hom)-(gamma(1+k_non.hom))^2)
e_non.hom = mean(used_data) - a_non.hom*((gamma(1+k_non.hom)-1)*k_non.hom)
ppp_hom.gev = e_non.hom + (a_non.hom/k_non.hom)*(1-(-log(pp))^k_non.hom)

# Heterogeneous
k_non.het = k_non.hom
mu_x.w.het = exp(mean.het + (sd.het^2)/2)
vr_x.w.het = exp(2*mean.het + sd.het^2)*(exp(sd.het^2)-1)
cv_x.w.het = sqrt(exp(sd.het^2)-1)
sk_x.w.het = 3*cv_x.w.het + cv_x.w.het^3

a_non.het = sign(k_non.het)*k_non.het*sqrt(vr_x.w.het)/
  sqrt(gamma(1+2*k_non.het)-(gamma(1+2*k_non.het))^2)
e_non.het = mean(used_data) - a_non.het*(gamma(1+k_non.het)-1)/k_non.het
ppp_het.gev = e_non.het + (a_non.het/k_non.het)*(1-(-log(pp))^k_non.het)

last1700 = tail(which(rp_gev<1701),1)

plot(rp_gev,ppp_sta.gev, log = "x", typ="n", 
     xlim = c(1,3500), ylim = c(40,150),
     xlab = "Return Period",
     ylab = "Peak wind gust (mph)",
     xaxt = "n",
     yaxt = "n",
     cex.lab = 1.5)
axis(side=1, labels = T, at=c(100,300,700,1700))
abline(v=c(100, 300,700, 1700), 
       h=seq(40,160,5), 
       col="gray", lty=3)
ppp_sta = ppp_sta.gev
ppp_hom = ppp_hom.gev
ppp_het = ppp_het.gev

legend("bottomright", 
       c("Stationary", 
         "Trend in Mean",
         "Trend in Mean + Cv"), 
       lty = c(1,2,3),
       lwd = 2)