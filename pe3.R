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