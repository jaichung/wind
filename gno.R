
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