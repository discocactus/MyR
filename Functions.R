#-----------------------------------------------------------------------------------------------------------
# fun.xts.single
# 単一ペアのOHLCを読み込み
#-----------------------------------------------------------------------------------------------------------

fun.xts.single=function(dat){
   # fun.xts.single("USDJPY1440")
   
   x.csv=paste("C:/HistoricalData/", dat,".csv",sep="")
   
   x.df=read.csv(x.csv,header=FALSE,
                 col.names=c("Date","Time","Open","High","Low","Close","Volume"),
                 colClasses=c("character","character","numeric","numeric","numeric","numeric","integer"))
   x.df$Datetime=paste(chartr(".","-",x.df$Date),x.df$Time) # 挿入文字変換 # DateとTimeを結合してDatetime列を作成
   
   x.xts=as.xts(zoo(x.df[,3:6],as.POSIXct(x.df$Datetime,"EET"))) # AlpariはEET(東ヨーロッパ時間)
   
   rm(x.df)
   
   return(x.xts)
}

#-----------------------------------------------------------------------------------------------------------
# fun.xts.multi
# 複数ペアに対応、読み込みデータ列の指定機能
#-----------------------------------------------------------------------------------------------------------

# pairnames <- c("AUDCAD", "AUDCHF", "AUDJPY", "AUDNZD", "AUDUSD", "CADCHF", "CADJPY", "CHFJPY",
#                "EURAUD", "EURCAD", "EURCHF", "EURGBP", "EURJPY", "EURNZD", "EURUSD",
#                "GBPAUD", "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "GBPUSD",
#                "NZDJPY", "NZDUSD", "USDCAD", "USDCHF", "USDJPY")
# 
# pairnames <- c("AUDCAD","AUDJPY","AUDNZD","AUDUSD","EURAUD","EURCAD","EURCHF","EURGBP","EURJPY","EURUSD",
#                "GBPAUD","GBPCHF","GBPJPY","GBPUSD","NZDUSD","USDCAD","USDCHF","USDJPY")
# 
# timeframe <- "60"

fun.xts.multi <- function(pairnames, timeframe, Open=FALSE, High=FALSE, Low=FALSE, Close=TRUE, Volume=FALSE){
   #fun.xts.multi(pairnames,"60")
   
   reads <- c(FALSE, FALSE, Open, High, Low, Close, Volume)
   count <- length(pairnames)
   m.xts <- xts()
   
   for(i in 1:count){
      x.df <- read.csv(paste("C:/HistoricalData/", pairnames[i], timeframe, ".csv", sep=""), 
                       header=FALSE,
                       col.names = c("Date", "Time", "Open", "High", "Low", "Close", "Volume"),
                       colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "integer"))
      x.df$Datetime <- paste(chartr(".", "-", x.df$Date), x.df$Time)
      s.xts <- as.xts(zoo(x.df[,reads], as.POSIXct(x.df$Datetime, "EET")))
      
      if(count==1){
         colnames(s.xts) <- names(x.df)[reads]
      }else if(sum(as.integer(reads))==1){
         colnames(s.xts) <- pairnames[i]
      }else{
      colnames(s.xts) <- paste(pairnames[i], names(x.df)[reads], sep=".")
      }
      
      m.xts <- merge(m.xts, s.xts, tzone="EET")
   }
   
   rm(x.df)
   rm(s.xts)
   
   return(m.xts)
}

#-----------------------------------------------------------------------------------------------------------
# fun.xts.single.indicator
#-----------------------------------------------------------------------------------------------------------
fun.xts.single.indicator=function(dat){
   # fun.xts.single("EURJPY1440_LogRV_HF1_HL1_Log1.csv")
   
   x.df <- read.csv(dat, header = TRUE, 
                    colClasses=c("character","character","numeric","numeric","numeric","numeric",
                                 "integer","numeric","numeric"))
   x.df$Datetime=paste(chartr(".","-",x.df$Date),x.df$Time) # 挿入文字変換 # DateとTimeを結合してDatetime列を作成
   
   x.xts=as.xts(zoo(x.df[,3:9],as.POSIXct(x.df$Datetime,"EET"))) # AlpariはEET(東ヨーロッパ時間)
   
   rm(x.df)
   
   return(x.xts)
}

#-----------------------------------------------------------------------------------------------------------
# fun.xts.logreturn
# 対数差収益率を作成
#-----------------------------------------------------------------------------------------------------------

fun.xts.logreturn <- function(dat){
   #fun.xts.logreturn(fx26.60.xts)
   
   count <- length(dat[1])
   l.xts <- xts()
   
   for(i in 1:count){
      l.xts <- merge(l.xts, diff(log(dat[,i]))*100, tzone="EET")
   }
   
   l.xts=l.xts[-1] # 差分なので値のない1日目の行を削除
   
   return(l.xts)
}

#-----------------------------------------------------------------------------------------------------------
# fun.xts.cumreturn
# 対数差累積収益率を作成
#-----------------------------------------------------------------------------------------------------------

fun.xts.cumreturn <- function(dat, period){
   #fun.xts.cumreturn(lr26.60, "2013-05::")
   
   count <- length(dat[1])
   c.xts <- xts()
   
   for(i in 1:count){
      c.xts <- merge(c.xts, cumprod(1+dat[period,i]/100), tzone="EET")
   }
   
   return(c.xts)
}

#-----------------------------------------------------------------------------------------------------------
# fun.index.first
#-----------------------------------------------------------------------------------------------------------

fun.index.first=function(x){
   ids=as.zoo(as.Date(first(index(x))))
   return(ids)
}

#-----------------------------------------------------------------------------------------------------------
# fun.xts.plot
# xtsデータをプロット
#-----------------------------------------------------------------------------------------------------------

fun.xts.plot=function(dat,pair,period,grd=TRUE,vgrd="months",hgrd=1,ylabel="",ylimit=TRUE){
   # fun.xts.plot(cr26.60,"USDJPY"(or 1:5),"2013-05::",T,"months",0.02,"Cum.Return",ylimit=FALSE)
   
   if(class(pair)=="character"){
      pair <- which(colnames(dat)==pair)
      }
   
   y.range <- range(dat[period, pair], na.rm=TRUE)
   y.max <- max(-(y.range[1]), y.range[2], na.rm=TRUE)
   if(ylimit){
      y.lim <- c(-(y.max), y.max)
   } else {
      y.lim = NULL
   }
   
   label.x <- axTicksByTime(dat[period], "days", format.labels="%m-%d")
   
   if(grd == TRUE){
      grid.x <- axTicksByTime(dat[period], vgrd, format.labels="%m-%d")
   }
   
   for(pair.x in pair){
      plot(as.vector(dat[period,pair.x]), type="l", ylim=y.lim, main=colnames(dat[,pair.x]),
           xlab=period, ylab=ylabel, xaxt="n")
      axis(side=1, at=label.x, labels=names(label.x))
      
      if(grd == TRUE){
         abline(h=seq(-3, 3, by=hgrd), v=grid.x, lty=2, col=8)
         } else {
            abline(h=seq(-3,3), lty=2)}
      }
}

#-----------------------------------------------------------------------------------------------------------
# old version - fun.plot.cumlog
# old version - (対数差)累積収益率、対数差収益率をプロット
#-----------------------------------------------------------------------------------------------------------

fun.plot.cumlog=function(dat,pair,period,plotraw=2,grd=FALSE,per){
   # fun.plot.cumlog("fx1440.logret.xts","USDJPY"(or 1:5),"2012-09::2014",2,grd=T,"quarterly")
   # requires fun.index.first
   par(mfcol=c(plotraw,1))
   data.x=as.name(dat)
   if(class(pair)=="character"){
      pair=which(colnames(eval(data.x))==pair)
   }
   
   if(grd == TRUE){
      grdpar=as.name(paste("apply.",per,sep=""))
      gridid=eval(grdpar)(eval(data.x),fun.index.first)
   }
   
   for(pair.x in pair){
      cumret.x=cumprod(1+eval(data.x)[period,pair.x]/100)
      plot(as.Date(index(cumret.x)),cumret.x,type="l",main=colnames(eval(data.x)[,pair.x]),ylab="cumret")
      if(grd == TRUE){
         abline(v=gridid,lty=2)
      }
      plot(as.Date(index(eval(data.x)[period])),eval(data.x)[period,pair.x],type="l",ylab="logret")
      if(grd == TRUE){
         abline(h=0,v=gridid,lty=2)
      }
      else{abline(h=0,lty=2)}
   }
}

#-----------------------------------------------------------------------------------------------------------
# fun.plot.cumlog
# (対数差)累積収益率、対数差収益率をプロット
#-----------------------------------------------------------------------------------------------------------

fun.plot.cumlog=function(dat,pair,period,plotlog=TRUE,plotraw=2,grd=TRUE,vgrd="months"){
   # fun.plot.cumlog(fx1440.logret.xts,"USDJPY"(or 1:5),"2012-09::2014",T,2,T,"months")
   
   par(mfcol=c(plotraw,1))
   
   if(class(pair)=="character"){
      pair=which(colnames(dat)==pair)
   }
   
   label.x <- axTicksByTime(dat[period], "days", format.labels="%m-%d")
   #label.y <- axTicksByTime(dat[period], "days")
   
   if(grd == TRUE){
      grid.x <- axTicksByTime(dat[period], vgrd, format.labels="%m-%d")
   }
   
   for(pair.x in pair){
      cumret.x=cumprod(1+dat[period,pair.x]/100)
      
      plot(as.vector(cumret.x), type="l", main=colnames(dat[,pair.x]),
           xlab=period, ylab="Cum.Return", xaxt="n")
      axis(side=1, at=label.x, labels=names(label.x))
      
      if(grd == TRUE){
         abline(h=seq(0,2,by=0.02),v=grid.x,lty=2)
      }else{abline(h=1,lty=2)}
      
      if(plotlog == TRUE){
         plot(as.vector(dat[period,pair.x]), type="l",
              xlab=period, ylab="Log.Return", xaxt="n")
         axis(side=1, at=label.x, labels=names(label.x))
      
         if(grd == TRUE){
            abline(h=seq(-3,3,by=0.5),v=grid.x,lty=2)
         }else{abline(h=0,lty=2)}
      }
   }
   
   par(mfcol=c(1,1))
   #return(label.y)
}

#-----------------------------------------------------------------------------------------------------------
# fun.hist.norm - 探索的データ解析法に基づく階級数
#-----------------------------------------------------------------------------------------------------------

fun.hist.norm <- function(dat, plot.curve=TRUE, plot.sd=TRUE, x.lim=range(brk), y.lim=NULL,
                          mainlab=NULL, x.lab=NULL, x.leg=NULL, y.leg=NULL){
   #dat <- dat[period, pair]
   mean_n <- mean(dat) 
   sd_n <- sd(dat)
   
   #階級数を求める
   scale_n <- floor(10*log10(length(dat)))
   #有効桁数を求める
   digits_n <- max(nchar(dat-trunc(dat)))
   if (digits_n > 2) digits_n <- digits_n -2
   else digits_n <- 0
   #階級幅を求める
   M <- round((max(dat)-min(dat))/scale_n, digits=digits_n)
   #スタートポイントとエンドポイントを求める
   SP <- round((min(dat)-M/2), digits=digits_n)
   EP <- round((max(dat)+M/2), digits=digits_n)
   #階級ベクトルを設定する
   brk=seq(SP,EP,length=scale_n+1)
   
   #ヒストグラムを描く
   hist(dat, breaks=brk, freq=FALSE, border = "white", col = "lightgray", xlim=x.lim, ylim=y.lim, main=mainlab, xlab=x.lab)
   if(plot.sd){
      abline(v=mean_n, col=2, lty=1, lwd=2)
      abline(v=c(mean_n-sd_n, mean_n+sd_n), col=4, lty=1, lwd=2)
      abline(v=c(mean_n-sd_n*2, mean_n+sd_n*2), col=3, lty=1, lwd=2)
   }
   if(plot.curve){
      curve(dnorm(x, mean_n, sd_n), add=T, col=1, lty=1, lwd=2) # from=-5, to=5, col=6
   }
   box()
   leg <- c(paste("mean =", round(mean_n, 6)), paste("sd =", round(sd_n, 6)))
   if(is.null(x.leg) || is.null(y.leg)) legend(locator(1), leg)
   else legend(x.leg, y.leg, leg)
}

#-----------------------------------------------------------------------------------------------------------
# fun.hist.fGarch
#-----------------------------------------------------------------------------------------------------------
# fun.hist.fGarch(SRV.close, type="snorm", x.leg=2, y.leg=0.5)

fun.hist.fGarch <- function(dat, type="snorm", plot.curve=TRUE, plot.sd=TRUE, x.lim=range(brk), y.lim=NULL,
                            mainlab = NULL, x.lab = NULL, x.leg=NULL, y.leg=NULL){
   fit_n <- switch(type,
                   snorm = snormFit(dat),
                   std   = stdFit(dat),
                   sstd  = sstdFit(dat))
   if(type == "sstd"){
      mean_n <- fit_n$estimate[1]
      sd_n <- fit_n$estimate[2]
   } else {
      mean_n <- fit_n$par[1]
      sd_n <- fit_n$par[2]
   }
   
   #階級数を求める
   scale_n <- floor(10*log10(length(dat)))
   #有効桁数を求める
   digits_n <- max(nchar(dat-trunc(dat)))
   if (digits_n > 2) digits_n <- digits_n -2
   else digits_n <- 0
   #階級幅を求める
   M <- round((max(dat)-min(dat))/scale_n, digits=digits_n)
   #スタートポイントとエンドポイントを求める
   SP <- round((min(dat)-M/2), digits=digits_n)
   EP <- round((max(dat)+M/2), digits=digits_n)
   #階級ベクトルを設定する
   brk=seq(SP,EP,length=scale_n+1)
   
   #ヒストグラムを描く
   hist(dat, breaks=brk, freq=FALSE, border = "white", col = "lightgray", xlim=x.lim, ylim=y.lim, main=mainlab, xlab=x.lab)
   box()
   if(plot.sd){
      abline(v=mean_n,                          col=2, lty=1, lwd=2)
      abline(v=c(mean_n-sd_n,   mean_n+sd_n),   col=3, lty=5, lwd=1)#4
      abline(v=c(mean_n-sd_n*2, mean_n+sd_n*2), col=4, lty=5, lwd=1)#3
   }
   if(plot.curve){
      curve(switch(type,
                   snorm = dsnorm(x, mean_n, sd_n, xi=fit_n$par[3]),
                   std   =   dstd(x, mean_n, sd_n, nu=fit_n$par[3]),
                   sstd  =  dsstd(x, mean_n, sd_n, nu=fit_n$estimate[3], xi=fit_n$estimate[4])),
            add=T, col=1, lty=1, lwd=2)
      leg <- switch(type,
                    snorm = c("< Skew Normal >", paste("objective =", round(fit_n$objective, 3)),
                              paste("mean =", round(mean_n, 6)), paste("sd =", round(sd_n, 6)),
                              paste("xi =", round(fit_n$par[3], 6))),
                    std   = c("< Student-t >", paste("objective =", round(fit_n$objective, 3)),
                              paste("mean =", round(mean_n, 6)), paste("sd =", round(sd_n, 6)),
                              paste("nu =", round(fit_n$par[3], 6))),
                    sstd  = c("< Skew Student-t >", paste("minimum =", round(fit_n$minimum, 3)),
                              paste("mean =", round(mean_n, 6)), paste("sd =", round(sd_n, 6)),
                              paste("nu =", round(fit_n$estimate[3], 6)), paste("xi =", round(fit_n$estimate[4], 6))))
      if(is.null(x.leg) || is.null(y.leg)) legend(locator(1), leg)
      else legend(x.leg, y.leg, leg)
   }
}

#-----------------------------------------------------------------------------------------------------------
# 初期バージョン fun.hist - 探索的データ解析法に基づく階級数
#-----------------------------------------------------------------------------------------------------------
fun.hist <- function(dat, pair, period, plot.curve=TRUE, plot.sd=TRUE, x.lim=range(brk), y.lim=NULL){
   dat <- dat[period, pair]
   mean_n <- mean(dat) 
   sd_n <- sd(dat)
   
   #階級数を求める
   scale_n <- floor(10*log10(length(dat)))
   #有効桁数を求める
   digits_n <- max(nchar(dat-trunc(dat)))
   if (digits_n > 2){ 
      digits_n <- digits_n -2
   }else{
      digits_n <- 0
   }
   #階級幅を求める
   M <- round((max(dat)-min(dat))/scale_n, digits=digits_n)
   #スタートポイントとエンドポイントを求める
   SP <- round((min(dat)-M/2), digits=digits_n)
   EP <- round((max(dat)+M/2), digits=digits_n)
   #階級ベクトルを設定する
   brk=seq(SP,EP,length=scale_n+1)
   
   #ヒストグラムを描く
   hist(dat, breaks=brk, freq=F, col="gray", main=names(lr26.60)[i], xlab=plot.period, xlim=x.lim, ylim=y.lim)
   if(plot.curve){
      curve(dnorm(x, mean_n, sd_n), add=T, col=6, lty=1) # from=-5, to=5, 
   }
   if(plot.sd){
      abline(v=mean_n, col=2, lty=1)
      abline(v=c(mean_n-sd_n, mean_n+sd_n), col=4, lty=1)
      abline(v=c(mean_n-sd_n*2, mean_n+sd_n*2), col=3, lty=1)
   }
}

#-----------------------------------------------------------------------------------------------------------
# apply.roll # rolling function from テラモナギ # 例えばmeanなら、元データよりもsize.window-1、短くなる
#-----------------------------------------------------------------------------------------------------------

apply.roll <- function(x,size.window,func){
   result <- numeric(length(x) - size.window +1)
   for(index.end in size.window:length(x)){
      index.start <- (1 + index.end) - size.window
      result[index.start] <- func(x[index.start:index.end])
   }
   return(result)
}

#-----------------------------------------------------------------------------------------------------------
# fun.apply.roll # rolling function from テラモナギ # 計算期間自動追加版
#-----------------------------------------------------------------------------------------------------------

fun.apply.roll <- function(x,size.window,func){
   result <- numeric(length(x))
   result[1:(size.window-1)] <- NA
   for(index.end in size.window:length(x)){
      index.start <- (1 + index.end) - size.window
      result[index.end] <- func(x[index.start:index.end])
   }
   return(result)
}

#-----------------------------------------------------------------------------------------------------------
# fun.apply.roll.2 # rolling function from テラモナギ # 計算期間自動追加版 # 引数追加版
#-----------------------------------------------------------------------------------------------------------

fun.apply.roll.2 <- function(x, size.window, func, ...){
   result <- numeric(length(x))
   result[1:(size.window-1)] <- NA
   for(index.end in size.window:length(x)){
      index.start <- (1 + index.end) - size.window
      result[index.end] <- func(x[index.start:index.end], ...)
   }
   return(result)
}

#-----------------------------------------------------------------------------------------------------------
# fun.rolling # from 「ファイナンスの統計学」 # オリジナルは遅い # 頭に計算期間分のNAが入る
#-----------------------------------------------------------------------------------------------------------
# 結局テラモナギロールとほぼ同じ内容、速度に。 違いは繰り返し数の起点終点の数え方

fun.rolling=function(x, size.window, func, ...){
   #len=length(x)
   #result <- NULL # この後、代入がないインデックスには自動的にNAが入る
   result <- numeric(length(x)) # ベクトルのサイズを確定させた方が速くなる
   result[1:(size.window-1)] <- NA # numericでは0が入っているのでNAに置換
   loop.end <- length(x)-size.window+1
   for(index.start in 1:loop.end){ # for内の計算式は少ない方が速くなる 下の実行分も同様
   #for(index.start in 1:(length(x)-size.window+1)){
      index.end <- index.start+size.window-1
      result[index.end]=func(x[index.start:index.end], ...)
      #result[index.start+size.window-1]=func(x[index.start:(index.start+size.window-1)], ...)
   }
   return(result)
}
