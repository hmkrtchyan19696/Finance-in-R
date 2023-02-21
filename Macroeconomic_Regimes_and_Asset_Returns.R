
load("data_4.RData")

CPI = dt$Inflation
CPI_ind = cumprod(CPI + 1) * 100
ISMNO = dt$ISMNO.Index
my = dt$MY
par(mfrow = c(3,1))
plot(CPI_ind, type = 'l', main = 'CPI-U Inflation Index(1973 = 100)')
plot(CPI, type = 'l', main = 'CPI-U Inflation')
plot(ISMNO, main = 'ISM New Orders Index (Mean = 50)')

aver = 6.5 

# 1.


comp = function(dt, sub = 'all') {
  if (!(sub %in% c('all', 'INF1GRW1', 'INF2GRW1', 
                   'INF1GRW2', 'INF2GRW2'))) {
    stop('Infalid subset!')
  }
  
  if (sub != 'all') {
    dt = dt[dt[sub] == 1,]
  }
  
  A = dt[,6:10] # assets
  R = A[,1:4] # risky assets
  RF = A[,5] # Tbill
  a = colMeans(A) # mean
  sd = apply(R, MARGIN = 2, sd) #sd
  r = colMeans(R)
  rf = mean(RF)
  sr = (r - rf) / sd
  
  
  corr = cor(R)
  VCV = cov(R)
  
  # Market Sharpe Ratio (MSR)
  w_msr = t(solve(VCV) %*% (r - rf)) / as.numeric(t(rep(1, 4)) %*% solve(VCV) %*% (r - rf))
  er_msr = w_msr %*% r
  vol_msr = sqrt(w_msr %*% VCV %*% t(w_msr))
  msr_sr = (er_msr - rf) / as.numeric(vol_msr)
  w_msr = c(w_msr, 1 - sum(w_msr))
  
  # Global Mininum Variance (GMV)
  w_gmv = t((solve(VCV) %*%  rep(1, 4)) / as.numeric(t(rep(1, 4)) %*% solve(VCV) %*% rep(1, 4)))
  er_gmv = w_gmv %*% r
  vol_gmv = sqrt(w_gmv %*% VCV %*% t(w_gmv))
  gmv_sr = (er_gmv - rf) / as.numeric(vol_gmv)
  w_gmv = c(w_gmv, 1 - sum(w_gmv))
  
  # Equally weighted portfolio (EW)
  w_ew = rep(0.25, 4)
  er_ew = w_ew %*% r
  vol_ew = sqrt(t(w_ew) %*% VCV %*% w_ew)
  ew_sr = (er_ew - rf) / as.numeric(vol_ew)
  w_ew = c(w_ew, 1 - sum(w_ew))
  
  # Optimal Portfolio of Risk Aversion 6.5
  w.a = t(solve(VCV) %*% (r - rf) / aver)
  er_a = w.a %*% r
  vol_a = sqrt(w.a %*% VCV %*% t(w.a))
  a_sr = (er_a - rf) / as.numeric(vol_a)
  w.a = c(w.a, 1 - sum(w.a))
  result = list(mean = a, Std = sd, 'SharpeRatio' = sr,
                'W.MSR' = w_msr, "SharpeRatio.MSR" = as.numeric(msr_sr),
                'W.GMV' = w_gmv, "SharpeRatio.GMV" = as.numeric(gmv_sr),
                'W.EW' = w_ew, "SharpeRatio.EW" = as.numeric(ew_sr),
                "W.aversion.6.5" = w.a, "SharpeRatio.6.5" = as.numeric(a_sr))
  
  return (result)
}

### Question 1 - slide

## unconditional
(res1 = comp(dt))

data_unconditional = as.data.frame(rbind(res1$mean, res1$Std, res1$SharpeRatio))
rownames(data_unconditional) = c("Mean return", "Standard Deviation", "Sharpe Ratio")
data_unconditional = round(data_unconditional, 3)
data_unconditional[2:3,5] = NA

## INF1GRW1
(res2 = comp(dt, sub = 'INF1GRW1')) # low inflation and low growth regime

data_INF1GRW1 = as.data.frame(rbind(res2$mean, res2$Std, res2$SharpeRatio))
rownames(data_INF1GRW1) = c("Mean return", "Standard Deviation", "Sharpe Ratio")
data_INF1GRW1 = round(data_INF1GRW1, 3)
data_INF1GRW1[2:3,5] = NA

## INF2GRW1
(res3 = comp(dt, sub = 'INF2GRW1')) # high inflation and low growth regime

data_INF2GRW1 = as.data.frame(rbind(res3$mean, res3$Std, res3$SharpeRatio))
rownames(data_INF2GRW1) = c("Mean return", "Standard Deviation", "Sharpe Ratio")
data_INF2GRW1 = round(data_INF2GRW1, 3)
data_INF2GRW1[2:3,5] = NA

## INF1GRW2
(res4 = comp(dt, sub = 'INF1GRW2')) # low inflation and high growth regime

data_INF1GRW2 = as.data.frame(rbind(res4$mean, res4$Std, res4$SharpeRatio))
rownames(data_INF1GRW2) = c("Mean return", "Standard Deviation", "Sharpe Ratio")
data_INF1GRW2 = round(data_INF1GRW2, 3)
data_INF1GRW2[2:3,5] = NA

## INF2GRW2
(res5 = comp(dt, sub = 'INF2GRW2')) # high inflation and high growth regime

data_INF2GRW2 = as.data.frame(rbind(res5$mean, res5$Std, res5$SharpeRatio))
rownames(data_INF2GRW2) = c("Mean return", "Standard Deviation", "Sharpe Ratio")
data_INF2GRW2 = round(data_INF2GRW2, 3)
data_INF2GRW2[2:3,5] = NA


### Question 2 - slide

data_MSR = as.data.frame(rbind(res1$W.MSR, res1$SharpeRatio.MSR, res2$W.MSR, res2$SharpeRatio.MSR,
                               res3$W.MSR, res3$SharpeRatio.MSR, res4$W.MSR, res4$SharpeRatio.MSR,
                               res5$W.MSR, res5$SharpeRatio.MSR))
rownames(data_MSR) = c("U/C W", "U/C SR", "INF1GRW1 W", "INF1GRW1 SR", "INF2GRW1 W", 'INF2GRW1 SR',
                       "INF1GRW2 W", "INF1GRW2 SR", "INF2GRW2 W", "INF2GRW2 SR")
colnames(data_MSR) = colnames(dt[6:10])
data_MSR = round(data_MSR, 3)

# Question 3 - slide

data_sr = data.frame("MSR" = c(res1$SharpeRatio.MSR, res2$SharpeRatio.MSR, res3$SharpeRatio.MSR, res4$SharpeRatio.MSR, res5$SharpeRatio.MSR),
                        "GMV" = c(res1$SharpeRatio.GMV, res2$SharpeRatio.GMV, res3$SharpeRatio.GMV, res4$SharpeRatio.GMV, res5$SharpeRatio.GMV),
                        "EW" = c(res1$SharpeRatio.EW, res2$SharpeRatio.EW, res3$SharpeRatio.EW, res4$SharpeRatio.EW, res5$SharpeRatio.EW),
                        "R-AV 6.5" = c(res1$SharpeRatio.6.5, res2$SharpeRatio.6.5, res3$SharpeRatio.6.5, res4$SharpeRatio.6.5, res5$SharpeRatio.6.5))
rownames(data_sr) = c("Unconditional", "INF1GRW1", 'INF2GRW1',
                       "INF1GRW2", "INF2GRW2")

# 2.

tab_mean = rbind(res1$mean, res2$mean, res3$mean, res4$mean, res5$mean) #Mean
tab_sd = rbind(res1$Std, res2$Std, res3$Std, res4$Std, res5$Std) #Standard Deviation
tab_sr = rbind(res1$SharpeRatio, res2$SharpeRatio, res3$SharpeRatio, res4$SharpeRatio, res5$SharpeRatio) #Sharpe Ratio
tab_MSR_w = rbind(res1$W.MSR, res2$W.MSR, res3$W.MSR, res4$W.MSR, res5$W.MSR) #MSR weight
tab_Aver_w = rbind(res1$W.aversion.6.5, res2$W.aversion.6.5, 
                   res3$W.aversion.6.5, res4$W.aversion.6.5, res5$W.aversion.6.5) #Risk Aversion = 6.5 weight 
colnames(tab_MSR_w) = colnames(tab_Aver_w) = colnames(tab_mean)
rownames(tab_mean) = rownames(tab_sd) = rownames(tab_sr) = rownames(tab_MSR_w) = rownames(tab_Aver_w) = 
  c('Unconditional', 'INF1GRW1', 'INF2GRW1', 'INF1GRW2', 'INF2GRW2')

## 3. 
tab_Aver_w[2,]
prob = c(1/6, 1/6, 1/6, 1/2)
prob %*% tab_Aver_w[2:5,]

data_weights = data.frame(rbind(tab_Aver_w[2,], prob %*% tab_Aver_w[2:5,]))
rownames(data_weights) = c("INF1GRW1", "Prob INF2GRW2")
data_weights = round(data_weights, 3)
