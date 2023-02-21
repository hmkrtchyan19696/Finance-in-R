### Load the data
load("data_2.RData")

### Question 1
nts = dt1[,1:5] # new trading strategy
kts = dt1[,6:9] # known trade strategy
rf = dt1[,10]

nts.mean = colMeans(nts - rf)*12 #mean of nts annualized
nts.sd = apply(nts, 2, sd) * 12^0.5 # standard deviation of nts annualized
nts.sr = nts.mean / nts.sd

q1 = cbind(nts.mean, nts.sd, nts.sr)

q1

### Question 2
reg_capm = NULL

nts.names = colnames(nts)

for (nts.name in nts.names) {
  lm.temp = lm( unlist(nts[nts.name] - rf) ~ kts$Mkt.RF )
  sum = summary(lm.temp)
  ab = sum$coefficients[, "Estimate"] # alpha and beta
  ab.se = sum$coefficients[, "Std. Error"]
  R = sum$r.squared
  ab.h0 = c(0,1)
  ab.tstat = (ab - ab.h0)
  reg = c(ab, R, ab.tstat)
  reg_capm = rbind(reg_capm, reg)
  
}

reg_capm[1, 4:5] = NA
rownames(reg_capm) = nts.names
colnames(reg_capm) = c('Alpha', "Beta", "R-squared", "tstat-alpha", 'tstat-beta')
reg_capm = as.data.frame(reg_capm)
reg_capm$Alpha = paste0(round(reg_capm$Alpha * 100, 3), '%')
reg_capm

sys = reg_capm$`R-squared`
idio = 1 - sys
var = data.frame(nts.mean, nts.sd, nts.sr, sys, idio)
var

### Apply the same for excess returns in dt1

### Question 3
reg_4fac = NULL

for (nts.name in nts.names) {
  lm.temp = lm( unlist(nts[nts.name] - rf) ~ kts$Mkt.RF + kts$SMB + kts$HML
                + kts$MOM)
  sum = summary(lm.temp)
  coef = sum$coefficients[, "Estimate"]
  coef.se = sum$coefficients[, "Std. Error"]
  R = sum$r.squared
  coef.h0 = c(0,1,0,0,0) #H0's for t tests
  coef.tstat = (coef - coef.h0) / coef.se
  reg = c(coef, R, coef.tstat)
  reg_4fac = rbind(reg_4fac, reg)
}

reg_4fac[, 2:5] = round(reg_4fac[,2:5], 3)
reg_4fac[,6:11] = round(reg_4fac[, 6:11], 2)
reg_4fac[1, 7:11] = NA
rownames(reg_4fac) = nts.names
colnames(reg_4fac) = c('alpha', 'Mkt-RF', 'SMB', "HML", "MOM", "R-Squared", 'tstat-alpha',
                       'tstat.Mkt-RF', 'tstat.SMB', 'tstat.HML', 'tstat.MOM')
reg_4fac = as.data.frame(reg_4fac)
reg_4fac$alpha = paste0(round(reg_4fac$alpha * 100, 3), '%')
reg_4fac


### Question 4 alphas should not be significant as per TA

reg_bkr = NULL
full = rep(TRUE, nrow(dt2))
sub1 = substr(rownames(dt2), 1, 4) %in% as.character(1980:2007)
sub2 = !sub1
ind = cbind(full, sub1, sub2)

for (i in 1:ncol(ind)) {
  samp = dt2[ind[,i],] #sample
  rf2 = samp[,6] #rf
  BKR = samp[,1]
  kts2 = samp[, 2:5]
  msr = mean(kts2[,1]) / sd(kts2[,1] + rf2)
  sr = mean(BKR - rf2) / sd(BKR)
  lm.temp = lm(BKR - rf2 ~ kts2$Mkt.RF + kts2$SMB + kts2$HML + kts2$MOM)
  sum = summary(lm.temp)
  coef = sum$coefficients[, "Estimate"]
  coef.se = sum$coefficients[, "Std. Error"]
  R = sum$r.squared
  coef.h0 = c(0,1,0,0,0) #H0's for t tests
  coef.tstat = (coef - coef.h0) / coef.se
  reg = c(msr, sr, coef, R, coef.tstat)
  reg_bkr = rbind(reg_bkr, reg)
}

reg_bkr = as.data.frame(reg_bkr)
rownames(reg_bkr) = c("Full", "1980-2007", "2008-2013")
colnames(reg_bkr) = c('Market Sharpe Ratio', 'Sharpe Ratio', 'alpha', 'Mkt-Rf', 'SMB', 'HML', 'MOM',
                      "R-Squared", 'tstat-alpha', 'tstat.Mkt-RF', 'tstat.SMB', 'tstat.HML', 'tstat.MOM')
reg_bkr$alpha = paste0(round(reg_bkr$alpha * 100, 3), '%')
reg_bkr = round(reg_bkr, 3)

#library("writexl")
# write.table(reg_bkr,"Q3.csv", row.names=TRUE, sep = ',')

### Question 5
library(stringr)
library(Rsolnp)

y = dt3$BRK.A
x1 = dt3$Vanguard.S.P.500.Index.Inv..VFINX.
x2 = dt3$Vanguard.Small.Cap.Index.Inv..NAESX.
x3 = dt3$Vanguard.Value.Index.Inv..VIVAX.

ind_sub = str_sub(rownames(dt3), -4, -1) %in% as.character(2008:2013)

# define loss function
f_loss = function(b) { #b is a vector containing parameters
  p = y #portfolio
  b = b[1] + b[2]*x1 + b[3]*x2 + b[4]*x3 # benchmark
  
  return (sum((b-p)^2))
}

# equality constraints function
eq = function(b) {
  return (b[2] + b[3] + b[4])
}

# random start points for parameter optimization
theta = c(0.1, 0.5, 0.25, 0.75)

f = function(dt, sub = FALSE, Shorts = FALSE) {
  
  if (sub) {
    dt = dt[ind_sub,]
  }
  
  y = dt[,1]
  x1 = dt[,2]
  x2 = dt[,3]
  x3 = dt[,4]
  
  # define the loss function
  f_loss = function(b) { #b is a vector containing parameters
    p = y #portfolio
    b = b[1] + b[2]*x1 + b[3]*x2 + b[4]*x3 # benchmark
    
    return (sum((b-p)^2))
  }
  
  if (Shorts) {
    m = solnp(theta, f_loss, eqfun = eq, eqB = 1, LB = c(-Inf, -Inf, -Inf, -Inf))
  }
  else {
    m = solnp(theta, f_loss, eqfun = eq, eqB = 1, LB = c(-Inf, 0, 0, 0))
  }
  
  w = m$pars
  y_b = as.matrix(dt) %*% w
  mar = mean(y - y_b) # mean active return
  te = sd(y - y_b) # tracking error
  
  return (c(w, mar, te))
}

# No Shorts Unit Exposure
r1 = f(dt3, sub = F, Shorts = F)
# Shorts Unit Exposure
r2 = f(dt3, sub = F, Shorts = T)
# recent no shorts unit exposure
r3 = f(dt3, sub = T, Shorts = F)
# recent shorts unit exposure
r4 = f(dt3, sub = T, Shorts = T)

r = as.data.frame(rbind(r1, r2, r3, r4))
rownames(r) = c('1993-2013 No Shorts Unit Exposure',
                '1993-2013 Shorts Unit Exposure',
                '2008-2013 No Shorts Unit Exposure',
                '2008-2013 Shorts Unit Exposure')
colnames(r) = c('alpha', 'Large', 'Small', 'Value', 'Mean Active returns', 'Tracking Error')
r$alpha = paste0(round(r$alpha * 100, 3), '%')
r[,-1] = round(r[,-1], 3)
r

library("writexl")
write.xlsx(reg_bkr,"Q3.csv", row.names=TRUE)
