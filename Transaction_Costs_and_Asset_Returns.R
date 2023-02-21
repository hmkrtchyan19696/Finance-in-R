
load('data_3.RData')
library("writexl")


#### Question 1

## Calculation
rec = dt1[, ncol(dt1)] # recession
div12 = dt1[, ncol(dt1) - 1] # div12
s = 1:7
Ft = dt1[, s] # F_t
yt = dt1[, 7+s] #y_t
yield = rep(1, nrow(dt1)) %*% t(1/s) * log(div12/Ft) + yt
yield = cbind(yield, yield[, 7] - yield[, 1])
mean = colMeans(yield) #Sample Mean
sd = apply(yield, MARGIN = 2, sd) #Standard Deviation
mean.rec = colMeans(yield[rec == 1 ,]) #Recession Mean
mean.nrec = colMeans(yield[rec != 1 ,]) # Non-Recession Mean

## Formatting
strip_yield = rbind(mean, sd, mean.rec, mean.nrec)
strip_yield = apply(round(strip_yield * 100, 2), 1:2, function(x) paste0(x, '%')) # convert to percentage
colnames(strip_yield) = c(1:7, '7-1')
rownames(strip_yield) = c("Mean", "Standard Deviation", "Recession Mean", "Non-Recession Mean")
strip_yield[2,8] = NA
strip_yield


#### Question 2

Ft = dt2[, 1:15]
yt = dt2[, 16:30]
rec = dt2[, 31]
n = nrow(Ft)
p = ncol(Ft)

ret_1 = NULL
for (i in 1:(n-1)){
  r = Ft[i+1, 2*s] / Ft[i, 2*s+1] - 1
  ret_1 = rbind(ret_1, r)
}

ret_12 = NULL
for (i in 1:(n-12)) {
  r = Ft[i+12, 2*s-1] / Ft[i, 2*s+1] - 1
  ret_12 = rbind(ret_12, r)
}

n = nrow(yt)
p = ncol(yt)

Bret_1 = NULL
for (i in 1:(n-1)) {
  r = exp((s*12*30)/360 * yt[i, 2*s+1]) / exp(((s*12 - 1) * 30)/360 * yt[i+1, 2*s]) - 1
  Bret_1 = rbind(Bret_1, r)
}

Bret_12 = NULL
for (i in 1:(n - 12)) {
  r = exp((s*12*30)/360 * yt[i, 2*s+1]) / exp(((s*12 - 12) * 30)/360 * yt[i+12, 2*s-1]) - 1
  Bret_12 = rbind(Bret_12, r)
}

Pret_1 = (ret_1 + 1) * (Bret_1 + 1) - 1
Pret_1 = cbind(Pret_1, Pret_1[,7] - Pret_1[,1])
Pret_12 = (ret_12 + 1) * (Bret_12 + 1) - 1
Pret_12 = cbind(Pret_12, Pret_12[,7] - Pret_12[,1])

rfmo = exp(yt[,1])^(1/12) - 1
rfyr = exp(yt[,3]) - 1
mean.rfmo = mean(rfmo[-1]) * 12
sd.rfmo = sd(rfmo[-1]) * sqrt(12)
mean.rfyr = mean(rfyr[-1])
sd.rfyr = sd(rfyr[-1])

#Summary stats
mean_1 = colMeans(Pret_1) * 12
sd_1 = apply(Pret_1, MARGIN  = 2, sd) * sqrt(12)
an_sharpe_1 = (mean_1 - mean.rfmo) / sd_1
mean_12 = colMeans(Pret_12)
sd_12 = apply(Pret_12, MARGIN = 2, sd)
an_sharpe_12 = (mean_12 - mean.rfyr) / sd_12
strip_returns = rbind(mean_1, sd_1, an_sharpe_1, mean_12, sd_12, an_sharpe_12)

# Formatting
colnames(strip_returns) = c(1:7, '7-1')
rownames(strip_returns) = c('Mean Monthly Return (%)', 'Monthly Standard Deviation (%)', 'Annualized Sharpe',
                            'Annual Mean (%)', 'Annual Standard Deviation (%)', 'Annual Sharpe')
strip_returns[-c(3,6),] = strip_returns[-c(3,6),] * 100
strip_returns[c(2,3,5,6),8] = NA
round(strip_returns, 2)


#### Question 3

F_bid = dt3[, 1:15]
F_ask = dt3[, 16:30]
rec = dt3[, 31]
spread = (F_ask[, 2*s+1] - F_bid[, 2*s+1]) / (0.5 * (F_ask[, 2*s+1] + F_bid[, 2*s+1])) #Bid-Ask Spread
mean = colMeans(spread)
sd = apply(spread, MARGIN = 2, sd)
mean.r = colMeans(spread[rec == 1,])
mean.nr = colMeans(spread[rec != 1,])
BA_spread = rbind(mean, sd, mean.r, mean.nr)

# Formatting
colnames(BA_spread) = c(1:7)
rownames(BA_spread) = c('Mean Spread', 'Standard Deviation Spread', 
                        'Recession Spread', 'Non-Recession Spread')
BA_spread = apply(round(BA_spread * 100, 2), 1:2, function(x) paste0(x, '%')) # to percentage
BA_spread

# write_xlsx(as.data.frame(BA_spread),"BA_Spread.xlsx")


#### Question 4

n = nrow(F_bid)
p = ncol(F_bid)

ret_1 = NULL
for (i in 1:(n-1)) {
  r = F_bid[i+1, 2*s] / F_ask[i, 2*s+1] - 1
  ret_1 = rbind(ret_1, r)
}

ret_12 = NULL
for (i in 1:(n-12)) {
  r = F_bid[i+12, 2*s-1] / F_ask[i+1, 2*s+1] - 1
  ret_12 = rbind(ret_12, r)
}

# recall Bret_1 Bret_12 from Q2
Pret_1 = (ret_1 + 1) * (Bret_1 +1) - 1
Pret_1 = cbind(Pret_1, Pret_1[,7] - Pret_1[,1])
Pret_12 = (ret_12 + 1) * (Bret_12 + 1) - 1
Pret_12 = cbind(Pret_12, Pret_12[,7] - Pret_12[,1])

# Summary Stats
mean_1 = colMeans(Pret_1) * 12
sd_1 = apply(Pret_1, MARGIN  = 2, sd) * sqrt(12)
an_sharpe_1 = (mean_1 - mean.rfmo) / sd_1
mean_12 = colMeans(Pret_12)
sd_12 = apply(Pret_12, MARGIN = 2, sd)
an_sharpe_12 = (mean_12 - mean.rfyr) / sd_12
SASR = rbind(mean_1, sd_1, an_sharpe_1, mean_12, sd_12, an_sharpe_12) #Spread Adjusted Strip Returns

# Formatting
colnames(SASR) = c(1:7,'7-1')
rownames(SASR) = c('Mean Monthly Return (%)', 'Monthly Standard Deviation (%)', 'Annualized Sharpe',
                   'Annual Mean (%)', 'Annual Standard Deviation (%)', 'Annual Sharpe')
SASR[-c(3,6),] = SASR[-c(3,6),] * 100
SASR[c(2,3,5,6),8] = NA
round(SASR, 2)


#### Question 5

sy = (rep(1, nrow(F_ask)) %*% t(1/s)) * log(div12/F_ask[, 2*s+1]) + yt[, 2*s+1] #strip yield
sy = cbind(sy, sy[,7]-sy[,1])
msy = colMeans(sy) # mean strip yield
msy.rec = colMeans(sy[rec == 1,]) # mean of recession strip yield
msy.nrec = colMeans(sy[rec != 1,]) # mean of non-recession strip yield
tb0 = cbind(tb0, tb0[,7] - tb0[,1])
mer = msy + tb0[1,] # mean expected return
mer.rec = msy.rec + tb0[2,] # mean of recession expected return
mer.nrec = msy.nrec + tb0[3,] # mean of non-recession expected return

# Formatting
expected_returns = rbind(msy, msy.rec, msy.nrec, mer, mer.rec, mer.nrec)[,-9]
colnames(expected_returns) = c(1:7,'7-1')
rownames(expected_returns) = c('Mean Monthly Yield (%)', 'Recession Strip Yield (%)', 'Non-Recession Strip Yield (%)',
                   'Mean Expected Return (%)', 'Recession Expected Return (%)', 'Non-Recession Expected Return (%)')
expected_returns = expected_returns * 100
round(expected_returns, 2)

write_xlsx(as.data.frame(expected_returns),"ER.xlsx")
