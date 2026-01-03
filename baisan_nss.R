
# Nelson-Siegel-Svensson

rm(list = ls()); gc()
library(dplyr)
library(tidyr)
library(openxlsx)
library(YieldCurve) # easy mode
setwd("C:/Users/takat/Desktop/test")
version_control <- "2026-01-02" # Modify versions
selectedcolumns <- 0

alldata <- readRDS(paste0("mergedata_", selectedcolumns, "_", version_control, ".RDS"))
tradedatevec <- unique(alldata$tradedate)
alldata <- alldata %>% mutate(rownum = row_number())

#### Functions ####
# NSS factor loading
nss_loading2 <- function(tau,m) {
  # Note that la is 1/tau in the original equation
  la1 <- 1/tau[1]; la2 <- 1/tau[2] # modified
  C  <- cbind(
    rep(1,length(m)),    
    (1-exp(-la1*m))/(la1*m),             
    (1-exp(-la1*m))/(la1*m) - exp(-la1*m),
    (1-exp(-la2*m))/(la2*m) - exp(-la2*m))
  return(C)
}

# fitting function
nss_fit2 <- function(para, m) {
  beta <- para[1:4]
  return(nss_loading2(para[5:6],m) %*% beta)
}



#### Fit NSS model ####
parambox  <- as.data.frame(matrix(NA_real_, length(tradedatevec), 6))

# data <- alldata %>% filter(tradedate == as.POSIXct("2008-07-02 JST")) %>%
#   arrange(residmatu)

for(i in 1:length(tradedatevec)){
  # Select one day
  data <- alldata %>%
    filter(tradedate == tradedatevec[i]) %>% 
    arrange(residmatu) # necessary for Svensson source code, to make sequence vector correctly
  # Save row number
  rownum_saved <- data$rownum
  
  # •½‹Ï•¡—˜
  param <- Svensson(data$avgcompoundyield, data$residmatu) # tau
  parambox[i,] <- param
  
  # Fit
  fitted <- nss_fit2(param, data$residmatu) # tau fit
  fitted <- as.data.frame(cbind(data$residmatu, fitted, rownum_saved))
  
  # Sort rows into the original order before sorting by residual maturity
  fitted <- fitted %>% arrange(rownum_saved) %>% select(-rownum_saved)
  colnames(fitted) <- c("residmatu", "fittedcompoundyield")
  fitted <- fitted %>%
    mutate(tradedate = tradedatevec[i])
  
  if(i == 1){
    merged_fitted <- fitted
  }else{
    merged_fitted <- rbind(merged_fitted, fitted)
  }
  
  print(tradedatevec[i])
}

merged_fitted <- merged_fitted$fittedcompoundyield

alldata <- cbind(alldata, merged_fitted)
colnames(alldata)[ncol(alldata)] <- "fittedcompoundyield" # %
if(sum(is.na(alldata$fittedcompoundyield)) != 0) print("wrong") 

alldata <- alldata %>%
  mutate(error   = (avgcompoundyield - fittedcompoundyield) * 100, # bps
         sqerror = error^2)

# maturity category
alldata <- alldata %>%
  mutate(residmatu_categ = case_when(
    residmatu <= 1 ~ "categ0",
    residmatu  > 1  & residmatu <= 3  ~ "categ1",
    residmatu  > 3  & residmatu <= 5  ~ "categ2",
    residmatu  > 5  & residmatu <= 10 ~ "categ3",
    residmatu  > 10 & residmatu <= 25 ~ "categ4",
    residmatu  > 25 ~ "categ5"
  ))

# all bonds
noise <- alldata %>%
  group_by(tradedate) %>%
  summarise(rmse = mean(sqerror, na.rm = T)^(0.5))
plot(noise$tradedate, noise$rmse, type = "l", col = "red", xlab = "Date", ylab = "")
noise$tradedate[which.max(noise$rmse)]

# drop bonds whose residual maturity is below 1 year (as HPW and Hattori)
noise_excateg0 <- alldata %>%
  filter(residmatu_categ != "categ0") %>%
  group_by(tradedate) %>%
  summarise(rmse    = mean(sqerror, na.rm = T)^(0.5),
            sdyield = sd(avgcompoundyield, na.rm = T),
            rmse_sd = rmse/sdyield, # Normalized
            rmse_mm = rmse/(max(avgcompoundyield, na.rm = T) - min(avgcompoundyield, na.rm = T))) %>%
  ungroup()
par(mfrow=c(3,1))
plot(noise_excateg0$tradedate, noise_excateg0$rmse,    type = "l", col = "black",
     xlab = "Date", ylab = "", ylim = c(0, 10))
plot(noise_excateg0$tradedate, noise_excateg0$rmse_sd, type = "l", col = "red",
     xlab = "Date", ylab = "", ylim = c(0, 30))
plot(noise_excateg0$tradedate, noise_excateg0$rmse_mm, type = "l", col = "blue",
     xlab = "Date", ylab = "", ylim = c(0, 10))
noise_excateg0$tradedate[which.max(noise_excateg0$rmse)]

# by maturity
noise2 <- alldata %>%
  group_by(tradedate, residmatu_categ) %>%
  summarise(rmse = mean(sqerror, na.rm = T)^(0.5)) %>% ungroup() %>%
  pivot_wider(names_from = residmatu_categ, values_from = rmse)
noise2$tradedate[which.max(noise2$categ0)]
noise2$tradedate[which.max(noise2$categ1)]
noise2$tradedate[which.max(noise2$categ2)]
noise2$tradedate[which.max(noise2$categ3)]
noise2$tradedate[which.max(noise2$categ4)]
noise2$tradedate[which.max(noise2$categ5)]

par(mfrow = c(3,2))
plot(noise2$tradedate, noise2$categ0, type = "l", col = "black", xlab = "Date", ylab = "")
plot(noise2$tradedate, noise2$categ1, type = "l", col = "black", xlab = "Date", ylab = "")
plot(noise2$tradedate, noise2$categ2, type = "l", col = "black", xlab = "Date", ylab = "")
plot(noise2$tradedate, noise2$categ3, type = "l", col = "black", xlab = "Date", ylab = "")
plot(noise2$tradedate, noise2$categ4, type = "l", col = "black", xlab = "Date", ylab = "")
plot(noise2$tradedate, noise2$categ5, type = "l", col = "black", xlab = "Date", ylab = "")

saveRDS(alldata, paste0("alldata_", version_control, ".RDS"))
write.xlsx(parambox,  paste0("parambox_", version_control, ".xlsx"))
write.xlsx(noise,     paste0("noise_",    version_control, ".xlsx"))
write.xlsx(noise2,    paste0("noise2_",   version_control, ".xlsx"))
write.xlsx(noise_excateg0, paste0("noise_excateg0",    version_control, ".xlsx"))

alldata <- readRDS(paste0("alldata_", version_control, ".RDS"))

# plot
par(mfrow = c(2,1))
plotdata <- alldata %>% filter(tradedate == "2025-06-16 JST") %>% arrange(residmatu)
plot(plotdata$residmatu, plotdata$avgcompoundyield, type = "l", col = "red", ylim = c(0, 4),
     xlab = "Residual maturity", ylab = "")
par(new = T)
plot(plotdata$residmatu, plotdata$fittedcompoundyield, type = "l", col = "blue", ylim = c(0, 4),
     xlab = "", ylab = "")
# legend("bottomright", c("actual","fitted"), col = c("red","blue"), lty = c("solid","solid"))

plotdata <- alldata %>% filter(tradedate == "2025-06-13 JST") %>% arrange(residmatu)
plot(plotdata$residmatu, plotdata$avgcompoundyield, type = "l", col = "red", ylim = c(0, 4),
     xlab = "Residual maturity", ylab = "")
par(new = T)
plot(plotdata$residmatu, plotdata$fittedcompoundyield, type = "l", col = "blue", ylim = c(0, 4),
     xlab = "", ylab = "")
# legend("bottomright", c("actual","fitted"), col = c("red","blue"), lty = c("solid","solid"))

par(mfrow = c(1,1))
plotdata <- alldata %>% filter(tradedate == "2020-03-16 JST") %>% arrange(residmatu)
plot(plotdata$residmatu, plotdata$avgcompoundyield, type = "l", col = "red", ylim = c(-1, 4),
     xlab = "Residual maturity", ylab = "")
par(new = T)
plot(plotdata$residmatu, plotdata$fittedcompoundyield, type = "l", col = "blue", ylim = c(-1, 4),
     xlab = "", ylab = "")
legend("bottomright", c("actual","fitted"), col = c("red","blue"), lty = c("solid","solid"))

par(mfrow = c(1,1))
plotdata <- alldata %>% filter(tradedate == "2023-03-09 JST") %>% arrange(residmatu)
plot(plotdata$residmatu, plotdata$avgcompoundyield, type = "l", col = "red", ylim = c(-1, 2),
     xlab = "Residual maturity", ylab = "")
par(new = T)
plot(plotdata$residmatu, plotdata$fittedcompoundyield, type = "l", col = "blue", ylim = c(-1, 2),
     xlab = "", ylab = "")
legend("bottomright", c("actual","fitted"), col = c("red","blue"), lty = c("solid","solid"))

par(mfrow = c(1,1))
plotdata <- alldata %>% filter(tradedate == "2025-12-30 JST") %>% arrange(residmatu)
plot(plotdata$residmatu, plotdata$avgcompoundyield, type = "l", col = "red", ylim = c(0, 4),
     xlab = "Residual maturity", ylab = "")
par(new = T)
plot(plotdata$residmatu, plotdata$fittedcompoundyield, type = "l", col = "blue", ylim = c(0, 4),
     xlab = "", ylab = "")
legend("bottomright", c("actual","fitted"), col = c("red","blue"), lty = c("solid","solid"))





###################################
# https://actsc.jp/learning/yieldcurve
# https://www.r-bloggers.com/2022/07/nelson-siegel-svensson-yield-curve-model-using-r-code/#google_vignette
# # Nelson-Siegel-Svensson
# data(ECBYieldCurve)
# maturity.ECB <- c(0.25,0.5,seq(1,30,by=1))
# A <- Svensson(ECBYieldCurve[1,], maturity.ECB)
# Svensson.rate <- Srates(A, maturity.ECB, "Spot")
# plot(maturity.ECB, Svensson.rate[5,],main="Fitting Svensson yield curve",
#      ylab="", xlab=c("Pillars in years"), type="l", col=3)
# lines( maturity.ECB, ECBYieldCurve[5,],col=2)
# legend("bottomright",legend=c("fitted yield curve","observed yield curve"),
#        col=c(3,2),lty=1)
# grid()
# 
# # Nelson-Siegel
# ECB.Rates <- Nelson.Siegel(ECBYieldCurve[5,], maturity.ECB)
# ECB.yield.curve <- NSrates(ECB.Rates, maturity.ECB)
# plot(maturity.ECB, ECB.yield.curve, main="Fitting Nelson Siegel yield curve",
#      ylab="", xlab=c("Pillars in years"), type="l", col=3)
# lines( maturity.ECB, ECBYieldCurve[5,],col=2)
# legend("topleft",legend=c("fitted yield curve","observed yield curve"),
#        col=c(3,2),lty=1)
# grid()
