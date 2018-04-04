#-----------------------------------------------------------------------------------------------------------
# データ作成
#-----------------------------------------------------------------------------------------------------------

timeframe <- "1440"

#-----------------------------------------------------------------------------------------------------------

UJ1440.xts <- fun.xts.single("USDJPY1440")
EJ1440.xts <- fun.xts.single("EURJPY1440")
EU1440.xts <- fun.xts.single("EURUSD1440")
EA1440.xts <- fun.xts.single("EURAUD1440")

UJ60.xts <- fun.xts.single("USDJPY60")

#-----------------------------------------------------------------------------------------------------------

pairnames <- c("USDJPY","EURUSD","EURAUD","EURJPY")

Close1440.xts <- fun.xts.multi(pairnames, timeframe)

head(Close1440.xts)
fun.xts.plot(Close1440.xts, "USDJPY", '2014::', grd=T, vgrd="months", ylabel="USDJPY", ylimit=FALSE)
any(is.na(Close1440.xts))
any(is.na(Close1440.xts['2007::']))
any(is.na(Close1440.xts$EURAUD['2007::']))

Close0714.xts <- Close1440.xts['2007::']

#-----------------------------------------------------------------------------------------------------------
# ALL
#-----------------------------------------------------------------------------------------------------------

pairnames <- c("AUDCAD", "AUDCHF", "AUDJPY", "AUDNZD", "AUDUSD", "CADCHF", "CADJPY", "CHFJPY",
               "EURAUD", "EURCAD", "EURCHF", "EURGBP", "EURJPY", "EURNZD", "EURUSD",
               "GBPAUD", "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "GBPUSD",
               "NZDJPY", "NZDUSD", "USDCAD", "USDCHF", "USDJPY")

ALL1440.xts <- fun.xts.multi(pairnames, timeframe)

Arow <- index(ALL1440.xts[nrow(ALL1440.xts)])
for(i in nrow(ALL1440.xts):1){
   if(any(is.na(ALL1440.xts[i])) == FALSE) Arow <- index(ALL1440.xts[i])
   else break
}
Arow

ALL1014.dl <- diff(log(ALL1440.xts[paste(Arow, "::", sep="")]))[-1]*100

rm(ALL1440.xts)

#-----------------------------------------------------------------------------------------------------------
# NZD, AUD, JPY, CHF, EUR, GBP, USD, CAD
#-----------------------------------------------------------------------------------------------------------

### NZD Cross

pairnames <- c("AUDNZD", "NZDJPY", "EURNZD", "GBPNZD", "NZDUSD")

NZD1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(NZD1440.xts['2010::']))

NZD1014.dl <- diff(log(NZD1440.xts['2010::']))[-1]*100

### AUD Cross

pairnames <- c("AUDNZD", "AUDJPY", "AUDCHF", "EURAUD", "GBPAUD", "AUDUSD", "AUDCAD")
#pairnames <- c("AUDCAD", "AUDNZD", "AUDJPY", "AUDCHF", "EURAUD", "GBPAUD", "AUDUSD") # 変化なし 

AUD1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(AUD1440.xts['2010::']))

AUD1014.dl <- diff(log(AUD1440.xts['2010::']))[-1]*100

### JPY Cross

pairnames <- c("NZDJPY", "AUDJPY", "CHFJPY", "EURJPY", "GBPJPY", "USDJPY", "CADJPY")

JPY1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(JPY1440.xts['2007::']))

JPY0714.dl <- diff(log(JPY1440.xts['2007::']))[-1]*100

### CHF Cross

pairnames <- c("AUDCHF", "CHFJPY", "EURCHF", "GBPCHF", "USDCHF", "CADCHF")

CHF1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(CHF1440.xts['2007::']))

CHF0714.dl <- diff(log(CHF1440.xts['2007::']))[-1]*100

### EUR Cross

pairnames <- c("EURNZD", "EURAUD", "EURJPY", "EURCHF", "EURGBP", "EURUSD", "EURCAD")

EUR1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(EUR1440.xts['2007::']))

EUR0714.dl <- diff(log(EUR1440.xts['2007::']))[-1]*100

### EUR 3

pairnames <- c("EURAUD", "EURJPY", "EURUSD")

EUR3.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(EUR3.xts['2007::']))

EUR3.0714.dl <- diff(log(EUR3.xts['2007::']))[-1]*100

### EUR 3b

pairnames <- c("EURUSD", "USDJPY", "EURJPY")

EUR3b.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(EUR3b.xts['2005::']))

EUR3b.0514.dl <- diff(log(EUR3b.xts['2005::']))[-1]*100

### EUR 4

pairnames <- c("EURUSD", "EURJPY", "EURGBP", "EURAUD")

EUR4.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(EUR4.xts['2006::']))

EUR4.0714.dl <- diff(log(EUR4.xts['2007::']))[-1]*100

### GBP Cross

pairnames <- c("GBPNZD", "GBPAUD", "GBPJPY", "GBPCHF", "EURGBP", "GBPUSD", "GBPCAD")

GBP1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(GBP1440.xts['2010::']))

GBP1014.dl <- diff(log(GBP1440.xts['2010::']))[-1]*100

### USD Cross

pairnames <- c("NZDUSD", "AUDUSD", "USDJPY", "USDCHF", "EURUSD", "GBPUSD", "USDCAD")

USD1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(USD1440.xts['2005::']))

USD0514.dl <- diff(log(USD1440.xts['2005::']))[-1]*100

### CAD Cross

pairnames <- c("AUDCAD", "CADJPY", "CADCHF", "EURCAD", "GBPCAD", "USDCAD")

CAD1440.xts <- fun.xts.multi(pairnames, timeframe)

any(is.na(CAD1440.xts['2010::']))

CAD1014.dl <- diff(log(CAD1440.xts['2010::']))[-1]*100

#-----------------------------------------------------------------------------------------------------------
# EURJPY
#-----------------------------------------------------------------------------------------------------------

any(is.na(Close1440.xts$EURJPY['2004::']))

fun.xts.plot(EJ1440.xts, "Close", '2004', grd=T, vgrd="months", ylabel="EURJPY", ylimit=FALSE)

EJ0414.xts <- EJ1440.xts['2004::']
EJ0414.xts <- merge(EJ0414.xts, (EJ0414.xts$Close-EJ0414.xts$Open))
EJ0414.xts <- merge(EJ0414.xts, (EJ0414.xts$High-EJ0414.xts$Low))
EJ0414.xts <- merge(EJ0414.xts, ((EJ0414.xts$Open-EJ0414.xts$Low)/EJ0414.xts[,6]))
EJ0414.xts <- merge(EJ0414.xts, ((EJ0414.xts$Close-EJ0414.xts$Low)/EJ0414.xts[,6]))
EJ0414.xts <- merge(EJ0414.xts, (EJ0414.xts$High/EJ0414.xts$Open))
EJ0414.xts <- merge(EJ0414.xts, (EJ0414.xts$Low/EJ0414.xts$Open))
EJ0414.xts <- merge(EJ0414.xts, (EJ0414.xts$Close/EJ0414.xts$Open))
colnames(EJ0414.xts) <- c(colnames(EJ0414.xts[,1:4]), "Change","Range","O.HL","C.HL","H.O","L.O","C.O")

summary(EJ0414.xts)
length(EJ0414.xts[,1])

for(i in 9:11){
   fun.xts.plot(EJ0414.xts, i, '2004::', grd=T, vgrd="months", ylabel="EURJPY", ylimit=FALSE)
}

#EJ.1 <- (EJ0414.xts[,9:11]) 
EJ.1 <- diff(log(EJ0414.xts[,1:4]))*100
EJ.1 <- EJ.1[-1]

adf.test(EJ.1[,1])
adf.test(EJ.1[,2])
adf.test(EJ.1[,3])
adf.test(EJ.1[,4])

#EJ.1 <- tail(EJ.1, 250)

#-----------------------------------------------------------------------------------------------------------
# Realized Volatility
# 1st data & 2011-08-08 to 12 BadData
#-----------------------------------------------------------------------------------------------------------

EJ.RV.CC.xts <- fun.xts.single.indicator("EURJPY1440_LogRV_HF5_Price0_Calc0.csv")
RV.CC <- EJ.RV.CC.xts[,7]
#RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")
RV.CC <- RV.CC[-1]
RV.CC[which.min(RV.CC)]
RV.CC <- RV.CC[-(which.min(RV.CC))]
RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")
# RV.CC[which.max(RV.CC)]
# RV.CC.trim <- RV.CC[-(which.max(RV.CC))] #不良データではない
# RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")

EJ.RV.OC.xts <- fun.xts.single.indicator("EURJPY1440_LogRV_HF5_Price1_Calc0.csv")
RV.OC <- EJ.RV.OC.xts[,7]
#RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")
RV.OC <- RV.OC[-1]
RV.OC[which.min(RV.OC)]
RV.OC <- RV.OC[-(which.min(RV.OC))]
RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")
# RV.OC[which.max(RV.OC)]
# RV.OC.trim <- RV.OC[-(which.max(RV.OC))] #不良データではない
# RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")

EJ.RV.HL.xts <- fun.xts.single.indicator("EURJPY1440_LogRV_HF5_Price2_Calc0.csv")
RV.HL <- EJ.RV.HL.xts[,7]
#RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")
RV.HL <- RV.HL[-1]
RV.HL[which.min(RV.HL)]
RV.HL <- RV.HL[-(which.min(RV.HL))]
RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")
# RV.HL[which.max(RV.HL)]
# RV.HL.trim <- RV.HL[-(which.max(RV.HL))] #不良データではない
# RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")

plot(EJ.RV.CC.xts[,4], type = "l") # CLose
plot(EJ.RV.CC.xts[,5], type = "l") # Volume
plot(EJ.RV.CC.xts[,6], type = "l") # Earn
plot(EJ.RV.OC.xts[,6], type = "l") # Earn

#-----------------------------------------------------------------------------------------------------------

NKD.RV.CC.xts <- fun.xts.single.indicator("#YMZ41440_LogRV_HF5_Price0_Calc0.csv")
RV.CC <- NKD.RV.CC.xts[,7]
#RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")
RV.CC <- RV.CC[-1]
RV.CC[which.min(RV.CC)]
#RV.CC <- RV.CC[-(which.min(RV.CC))]
RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")
# RV.CC[which.max(RV.CC)]
# RV.CC.trim <- RV.CC[-(which.max(RV.CC))] #不良データではない
# RV.CC.index <- index(RV.CC); plot(RV.CC.index, as.vector(RV.CC), type ="l")

NKD.RV.OC.xts <- fun.xts.single.indicator("#YMZ41440_LogRV_HF5_Price1_Calc0.csv")
RV.OC <- NKD.RV.OC.xts[,7]
#RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")
RV.OC <- RV.OC[-1]
RV.OC[which.min(RV.OC)]
#RV.OC <- RV.OC[-(which.min(RV.OC))]
RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")
# RV.OC[which.max(RV.OC)]
# RV.OC.trim <- RV.OC[-(which.max(RV.OC))] #不良データではない
# RV.OC.index <- index(RV.OC); plot(RV.OC.index, as.vector(RV.OC), type ="l")

NKD.RV.HL.xts <- fun.xts.single.indicator("#YMZ41440_LogRV_HF5_Price2_Calc0.csv")
RV.HL <- NKD.RV.HL.xts[,7]
#RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")
RV.HL <- RV.HL[-1]
RV.HL[which.min(RV.HL)]
#RV.HL <- RV.HL[-(which.min(RV.HL))]
RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")
# RV.HL[which.max(RV.HL)]
# RV.HL.trim <- RV.HL[-(which.max(RV.HL))] #不良データではない
# RV.HL.index <- index(RV.HL); plot(RV.HL.index, as.vector(RV.HL), type ="l")

#-----------------------------------------------------------------------------------------------------------

LRV.CC <- log(RV.CC)
LRV.CC.index <- index(LRV.CC); plot(LRV.CC.index, as.vector(LRV.CC), type ="l")

LRV.OC <- log(RV.OC)
LRV.OC.index <- index(LRV.OC); plot(LRV.OC.index, as.vector(LRV.OC), type ="l")

LRV.HL <- log(RV.HL)
LRV.HL.index <- index(LRV.HL); plot(LRV.HL.index, as.vector(LRV.HL), type ="l")

Earn.CC <- EJ.RV.CC.xts[,6]
Earn.CC <- Earn.CC[-1]
# which(index(Earn.CC) == as.POSIXct("2011-08-12", tz="EET"))
# Earn.CC <- Earn.CC[-44]
Earn.CC.index <- index(Earn.CC); plot(Earn.CC.index, as.vector(Earn.CC), type ="l")

Earn.OC <- EJ.RV.OC.xts[,6]
Earn.OC <- Earn.OC[-1]
# which(index(Earn.OC) == as.POSIXct("2011-08-12", tz="EET"))
# Earn.OC <- Earn.OC[-44]
Earn.OC.index <- index(Earn.OC); plot(Earn.OC.index, as.vector(Earn.OC), type ="l")

SRV.CC <- Earn.CC / sqrt(RV.CC)
SRV.CC.index <- index(SRV.CC); plot(SRV.CC.index, as.vector(SRV.CC), type ="l")
SRV.OC <- Earn.OC / sqrt(RV.OC)
SRV.OC.index <- index(SRV.OC); plot(SRV.OC.index, as.vector(SRV.OC), type ="l")
SRV.HL <- Earn.OC / sqrt(RV.HL)
SRV.HL.index <- index(SRV.HL); plot(SRV.HL.index, as.vector(SRV.HL), type ="l")

#-----------------------------------------------------------------------------------------------------------

EV.CC <- Earn.CC ^ 2
EV.CC.index <- index(EV.CC); plot(EV.CC.index, as.vector(EV.CC), type ="l")

EV.OC <- Earn.OC ^ 2
EV.OC.index <- index(EV.OC); plot(EV.OC.index, as.vector(EV.OC), type ="l")

LEV.CC <- log(EV.CC[-(which(EV.CC == 0))])
LEV.CC.index <- index(LEV.CC); plot(LEV.CC.index, as.vector(LEV.CC), type ="l")

LEV.OC <- log(EV.OC[-(which(EV.OC == 0))])
LEV.OC.index <- index(LEV.OC); plot(LEV.OC.index, as.vector(LEV.OC), type ="l")

#-----------------------------------------------------------------------------------------------------------

RV.1m <- xts(cbind(RV.CC, RV.OC, RV.HL, LRV.CC, LRV.OC, LRV.HL, SRV.CC, SRV.OC, SRV.HL))
names(RV.1m) <- c("RV.CC", "RV.OC", "RV.HL", "LRV.CC", "LRV.OC", "LRV.HL", "SRV.CC", "SRV.OC", "SRV.HL")

RV.5m <- xts(cbind(RV.CC, RV.OC, RV.HL, LRV.CC, LRV.OC, LRV.HL, SRV.CC, SRV.OC, SRV.HL))
names(RV.5m) <- c("RV.CC", "RV.OC", "RV.HL", "LRV.CC", "LRV.OC", "LRV.HL", "SRV.CC", "SRV.OC", "SRV.HL")

EJ.RV.1m <- RV.1m
EJ.RV.5m <- RV.5m
NKD.RV.1m <- RV.1m
NKD.RV.5m <- RV.5m
YMM.RV.1m <- RV.1m
YMM.RV.5m <- RV.5m
#-----------------------------------------------------------------------------------------------------------

