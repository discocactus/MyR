#-----------------------------------------------------------------------------------------------------------
# library
#-----------------------------------------------------------------------------------------------------------

#library(zoo)
library(xts) # include zoo
library(tseries)
library(lubridate)
library(fGarch) # include timeseries, fBasics, MASS, e1071
library(FinTS)
library(forecast)
library(vars)
library(tsDyn)
library(lattice) # 汎用QQプロット qqmath 時系列青本
library(ggplot2)
library(GGally)
library(Rcpp)
library(RcppRoll)
library(dplyr)
library(data.table)
library(ccgarch2)
library(robustbase)
library(quantmod) # include xts, TTR
library(assertthat) # is.error
#library(xlsx)
#library(MSBVAR)
#library(devtools)
#install_github('rCharts', 'ramnathv')
library(rCharts)

#-----------------------------------------------------------------------------------------------------------
# パッケージ関連
#-----------------------------------------------------------------------------------------------------------

library() # インストール済みパッケージの確認
search() # 読み込み済みパッケージの確認
detach("package:MSBVAR") # 読み込み済みパッケージの削除
install.packages("C:/R/R-3.1.1/library/fGarch.tar.gz", repos = NULL, type = "source") # .tar.gzのインストール
.libPaths() # ライブラリパスの確認

#-----------------------------------------------------------------------------------------------------------
# よく使う関数
#-----------------------------------------------------------------------------------------------------------

getwd()
setwd("C:/MyR/")

system.time(x1 <- fun.rolling(dat[,i],20,mean))

apply(data.sample, MARGIN=2, adf.test) # rows: MARGIN=1, columns: MARGIN=2

read.csv("x", header=FALSE)
write.table(x, "x.csv", sep=",")
write.csv(trim.df, "trim.csv", quote = FALSE, row.names = FALSE) # sep=",", 

#-----------------------------------------------------------------------------------------------------------
# データ内容
#-----------------------------------------------------------------------------------------------------------

class(xts15)
length(xts15)
mode(xts15)
summary(xts15)
str(xts15) # オブジェクトの持つ全情報を簡略に示す汎用関数
attributes(head(xts15)) # modeとlength以外の全ての属性を調べる attributes(head(xts15))$index
attr(xts15,"index") # attr(xts15,"dimnames") 任意の属性を操作
head(xts15)
tail(xts15)

#-----------------------------------------------------------------------------------------------------------
# データ操作
#-----------------------------------------------------------------------------------------------------------

anyNA(fx1440.xts) # v3.1以降 それ以前は any(is.na(fx1440.xts))
which(is.na(x)) # xの値がNAの成分の添え字を返す
x[is.na(x)] <- 0 # xのNA値を0に置き換える
identical(x,y) # 完全一致検査
all.equal(as.data.frame(ymm460_1), as.data.frame(ymm460_2)) # ほとんど(どの程度)等しいか?
NROW(x) # nrow,ncolでも同じ、大文字だとベクトルに対しても利用可能
NCOL(x)
args(fun.x) # 関数の引数を確認する
seq_along(x) # == 1:length(x)
numeric(＜要素数＞) # 要素がすべて 0 であるベクトルの生成
max.col(dat) # 行毎の最大値の列。同値処理の引数 ties.method = c("random", "first", "last")
max.col(t(dat)) # 列毎の最大値。max.rowはないのでこう書く
which.max(dat[,i]) # これ max.col(t(dat[,i])) とこれ which(dat[,i]==max(dat[,i])) は同じ
x[order(rnorm(length(x)))] # 変数の中身をランダムに並べ替える
which(index(Earn) == as.POSIXct("2011-08-12", tz="EET"))

#-----------------------------------------------------------------------------------------------------------
# オブジェクト操作
#-----------------------------------------------------------------------------------------------------------

ls()
rm(list=ls()) # オブジェクトの一括消去
rm(list=ls()[!sapply(ls(), FUN=exists, mode="function")]) # 関数以外のオブジェクトの一括消去
rm(list=ls(all=T)) # 隠しオブジェクト(名前が「.」(ドット)で始まるオブジェクト)も削除される

#-----------------------------------------------------------------------------------------------------------
# 出力関連
#-----------------------------------------------------------------------------------------------------------

cat(x, fill=30) # 30文字以内で改行
sink("csl.txt",append=T, split=T) # テキストファイル出力 sink()で閉じる

#-----------------------------------------------------------------------------------------------------------
# 作図関連
#-----------------------------------------------------------------------------------------------------------

par(originalpar)
dev.off()
locator(1) # 作図領域をクリックした位置の座標を返す
colors()

#-----------------------------------------------------------------------------------------------------------
# その他
#-----------------------------------------------------------------------------------------------------------

if(0){
   複数行のコメントアウト
}

#mailパッケージ # 処理が終わるとRからメールが来る
adress<-”xxx@xxu.com” #自分のアドレス
sendmail(adress) # アドレスにメール送信

apropos("mean") # コマンドやオブジェクトの名称を検索
