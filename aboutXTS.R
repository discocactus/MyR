library(zoo)
library(xts)

### MT4 History.csv からの変換

# 単一ペアのOHLCを読み込み
uj.list=list() # データ長が違うペアを追加する予定がなければリストでなくてもよいと思う
uj.list[[1]]=read.csv("USDJPY1440.csv",header=FALSE,
                      col.names=c("Date","Time","Open","High","Low","Close","Volume"),
                      colClasses=c("character","character","numeric","numeric","numeric","numeric","integer"))
uj.list[[1]]$Date=chartr(".","-",uj.list[[1]]$Date) # 挿入文字変換
uj.list[[1]]$Datetime=paste(uj.list[[1]]$Date,uj.list[[1]]$Time) # DateとTimeを結合してDatetime列を作成
uj.xts=as.xts(zoo(uj.list[[1]][,3:6],as.POSIXct(uj.list[[1]]$Datetime,"EET"))) # AlpariはEET(東ヨーロッパ時間)

# 一括変換 # 名称(タイムフレーム)注意!
symbols=c("AUDCAD","AUDJPY","AUDNZD","AUDUSD","EURAUD","EURCAD","EURCHF","EURGBP","EURJPY","EURUSD",
          "GBPAUD","GBPCHF","GBPJPY","GBPUSD","NZDUSD","USDCAD","USDCHF","USDJPY")
timeframe="1440"
fx1440.list=list()  # ペア毎にデータ長が違うのでデータフレームではなくリストに格納する
# fx1440.xts=xts()   # 名称(タイムフレーム)注意!
pairs=length(symbols)
for(i in 1:pairs){
  fx1440.list[[i]]=read.csv(paste(symbols[i],timeframe,".csv",sep=""),header=FALSE,
                            col.names=c("Date","Time","Open","High","Low","Close","Volume"),
                            colClasses=c("character","character","numeric","numeric","numeric","numeric","integer"))
  fx1440.list[[i]]$Date=chartr(".","-",fx1440.list[[i]]$Date)
  fx1440.list[[i]]$Datetime=paste(fx1440.list[[i]]$Date,fx1440.list[[i]]$Time)
  if(i==1){
    fx1440.xts=as.xts(zoo(fx1440.list[[i]]$Close,as.POSIXct(fx1440.list[[i]]$Datetime,"EET")))
  }else{
    fx1440.xts=merge(fx1440.xts,as.xts(zoo(fx1440.list[[i]]$Close,as.POSIXct(fx1440.list[[i]]$Datetime,"EET"))))
  }
}
names(fx1440.list)=symbols
colnames(fx1440.xts)=symbols

# 列内容の差し替え(本当は差分追記をしたいがやり方がわからない)
# 新規列に差し替えたい内容(xts)を追加して・・・
fx1440.xts[,"old"]=fx1440.xts[,"new"] # 新規列を差し替えたい列へ上書き
fx1440.xts[,"new"]=NULL # 不要になった新規列の削除

# 内容の確認
head(fx1440.xts,n=5)
index(first(fx1440.xts))
attributes(fx1440.xts)$tzone

write.csv(as.data.frame(fx1440.xts),file="fx1440.csv")

fx1440.0911.1404.xts=fx1440.xts['2009-11-09::2014-05-01']

any(is.na(fx1440.0911.1404.xts))

# xtsオブジェクトから日付インデックスを取り出す
head(index(fx1440.xts))

par(mfcol=c(4,1))
for(i in 1:pairs){
  plot(as.Date(index(fx1440.0911.1404.xts)),fx1440.0911.1404.xts[,i],type="l",main=symbols[i])
}

# 色指定で複数プロットする場合、xtsのままではエラーが出るのでzooとして認識させる
# plot.type指定無だと複数枚分割プロット
plot.zoo(fx1440.cumret.xts[,1:2],col = c("red", "blue"),plot.type="single")

# ローリング (zoo FUN)
ma100=rollapply(fx1440.xts$USDJPY,100,mean)
plot(as.Date(index(fx1440.xts)),ma100,type="l")

# 欠損値補間 (zoo FUN) # na.locfはxts FUN
plot(as.Date(index(fx1440.xts["2008::2009"])),na.spline(fx1440.xts$GBPAUD["2008::2009"]),type="l") # スプライン補間
plot(as.Date(index(fx1440.xts["2008::2009"])),na.approx(fx1440.xts$GBPAUD["2008::2009"]),type="l") # 線形補間
plot(as.Date(index(fx1440.xts["2008::2009"])),na.locf(fx1440.xts$GBPAUD["2008::2009"]),type="l") # 直前値補間
plot(as.Date(index(fx1440.xts["2008::2009"])),na.locf(fx1440.xts$GBPAUD["2008::2009"],fromLast=TRUE),type="l") # 直後値補間
plot(as.Date(index(fx1440.xts["2008::2009"])),na.aggregate(fx1440.xts$GBPAUD["2008::2009"]),type="l") # 前後中間値補間 
ma100=rollapply(na.spline(fx1440.xts$GBPAUD["2008::2009"]),100,mean)
lines(as.Date(index(fx1440.xts["2008::2009"])),ma100,type="l",col="blue")



### 対数差収益率を作成

pairs=length(symbols)
for(i in 1:pairs){
  if(i==1){
    fx1440.logret.xts=as.xts(diff(log(fx1440.0911.1404.xts[,i]))*100)
  }else{
    fx1440.logret.xts=merge(fx1440.logret.xts,diff(log(fx1440.0911.1404.xts[,i]))*100)
  }
}
fx1440.logret.xts=fx1440.logret.xts[-1] # 差分なので値のない1日目の行を削除

head(fx1440.logret.xts,n=5)

sink(paste("csl_fx",timeframe,".txt",sep=""),append=T, split=T)
summary(fx1440.logret.xts)
sink()

par(mfcol=c(2,1))
for(i in 1:pairs){
  plot(as.Date(index(fx1440.xts)),fx1440.logret.xts[,i],type="l",
       main=symbols[i],ylim=range(fx1440.logret.xts))
}



### 対数差収益率から累積収益率を作成 # 分析期間に合わせてその都度計算する方がよいかも

for(i in 1:pairs){
  if(i==1){
    fx1440.cumret.xts=as.xts(cumprod(1+fx1440.logret.xts[,i]/100))
  }else{
    fx1440.cumret.xts=merge(fx1440.cumret.xts,cumprod(1+fx1440.logret.xts[,i]/100))
  }
}

head(fx1440.cumret.xts,n=5)

sink(paste("csl_fx",timeframe,".txt",sep=""),append=T, split=T)
summary(fx1440.cumret.xts)
sink()

par(mfcol=c(2,1))
for(i in 1:pairs){
  plot(as.Date(index(fx1440.xts)),fx1440.cumret.xts[,i],type="l",
       main=symbols[i],ylim=range(fx1440.cumret.xts))
}

# 期間を指定して作成

fx1440.cumret.1.xts=xts()
for(i in 1:pairs){
  fx1440.cumret.1.xts=merge(fx1440.cumret.1.xts,cumprod(1+fx1440.logret.xts['2010-11::',i]/100))
}

head(fx1440.cumret.1.xts,n=5)

sink(paste("csl_fx",timeframe,".txt",sep=""),append=T, split=T)
summary(fx1440.cumret.xts)
sink()

par(mfcol=c(2,1))
for(i in 1:pairs){
  plot(as.Date(index(fx1440.xts)),fx1440.cumret.xts[,i],type="l",
       main=symbols[i],ylim=range(fx1440.cumret.xts))
}
