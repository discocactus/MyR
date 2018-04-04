#-----------------------------------------------------------------------------------------------------------
# some plots
#-----------------------------------------------------------------------------------------------------------

chartSeries(na.locf(fx26.60.xts$USDJPY.Close["2014-01"]),minor.ticks=FALSE,type="l",
            theme = chartTheme("white",up.col="black"))
chartSeries(uj60x["2014-01"],minor.ticks=FALSE,TA="addBBands();addMACD()")

chartSeries(uj60.test$Close,minor.ticks=FALSE,type="l",theme = chartTheme("white",up.col="black"))

plot(uj60.test$Close,minor.ticks=FALSE,type="l")
plot(seq(1,length(index(uj60.test)),by=1),uj60.test$Close,type="l")

ggplot(uj60.test,aes(x=index(uj60.test),y=Close))+geom_line()
ggplot(uj60.test,aes(x=seq(1,length(index(uj60.test)),by=1),y=Close))+geom_line()

#-----------------------------------------------------------------------------------------------------------
# xts plot Quita
#-----------------------------------------------------------------------------------------------------------

USDJPY60.xts <- fun.xts.multi("USDJPY","60",TRUE,TRUE,TRUE,TRUE)
USDJPY60.xts <- USDJPY60.xts["2014-01"]
head(USDJPY60.xts,5)

plot(USDJPY60.xts$Close,minor.ticks=FALSE,type="l",main="USDJPY60$Close",ylab="price")

chartSeries(USDJPY60.xts$Close,minor.ticks=FALSE,type="l",theme = chartTheme("white",up.col="black"))

USDJPY60.NA.xts <- USDJPY60.xts
USDJPY60.NA.xts["2014-01-10::2014-01-15", 1:4] <- NA
USDJPY60.NA.xts["2014-01-09 22:00::2014-01-10 02:00"]

chartSeries(USDJPY60.NA.xts$Close, minor.ticks=FALSE, type="l", theme = chartTheme("white", up.col="black"))
chartSeries(na.locf(USDJPY60.NA.xts$Close), minor.ticks=FALSE, type="l", theme = chartTheme("white", up.col="black"))

plot(as.vector(USDJPY60.xts$Close), minor.ticks=FALSE, type="l", main="USDJPY60$Close", ylab="price")

plot(as.vector(USDJPY60.xts$Close), type="l", main="USDJPY60$Close", ylab="price", xaxt="n")
label.day <- axTicksByTime(USDJPY60.xts, "days", format.labels="%m-%d")
label.day
axis(side=1, at=label.day, labels=names(label.day))

plot(as.vector(USDJPY60.NA.xts$Close), type="l", main="USDJPY60$Close", ylab="price", xaxt="n")
axis(side=1, at=label.day, labels=names(label.day))

label.week <- axTicksByTime(USDJPY60.xts, "weeks")
label.week

label.day2 <- axTicksByTime(USDJPY60.xts, "days")
label.day2

label.month <- axTicksByTime(uj60x, "months")
label.month

abline(v=c(49, 169, 289, 409), h=c(seq(102, 105, by=0.5)), col=8, lty=2)
abline(v=label.day, h=c(seq(102, 105, by=0.5)), col=8, lty=2)

#-----------------------------------------------------------------------------------------------------------
# ggplot2 example
#-----------------------------------------------------------------------------------------------------------

df <- data.frame(
   x  = (x  <- rnorm(100)),
   y  = (y  <- rnorm(100, x)),
   zx = (zx <- (rep(1:10, 10) - 5) / 2),
   zy = (zy <- (rep(1:10, each=10) - 5) / 2),
   z  = (z  <- zx*zy),
   w  = 1:100,
   g  = rep(1:2, 50),
   h  = c(rep(1, 50), rep(2, 50))
)
sdf <- subset(df, w <= 10)

p1 <- ggplot(df,  aes(x = x, y = y)) + geom_line()
p2 <- ggplot(df,  aes(x = w, y = y)) + geom_line()
p  <- ggplot(df,  aes(x = x, y = y, colour = factor(g))) + geom_line()
q  <- ggplot(sdf, aes(x = factor(w), y = y)) + geom_bar()
r  <- ggplot(df,  aes(x = x, y = ..density..)) + geom_histogram()
c  <- ggplot(df,  aes(x = zx, y = zy, z = z)) + geom_contour()

#-----------------------------------------------------------------------------------------------------------
# png plot
#-----------------------------------------------------------------------------------------------------------
png("hoge.png", width=960, height=480, type="cairo")
#par(originalpar)
dev.off()

plot.period="2014"
for(i in 1:26){
   png(paste("fx_",i,"_",pairnames[i],"_60_",plot.period,".png",sep=""),
       width=960, height=480, type="cairo-png")
   par(mar=c(4,4,2,0.5))
   fun.plot.cumlog("lr26.60",i,plot.period,plotraw=2,grdper="months")
   dev.off()
}

#-----------------------------------------------------------------------------------------------------------
# original plot function test 
#-----------------------------------------------------------------------------------------------------------

ts.plot(fx26.60$AUDJPY,fx26.60$USDJPY,type="l")
plot(fx26.60$USDJPY,type="l")
lines(fx26.60$AUDJPY,type="l",col="red")

fun.plot.cumlog(lr26.60,1,"2014-04-09::",T,2,T,"months")

par(mfcol=c(2,1))
fun.xts.plot(cr2014,"GBPUSD","2014-04::",h=0.02,ylab="cumret")
fun.xts.plot(lr26.60,1:26,"2013-05::2014-05",h=0.5,ylab="logret")
par(mfcol=c(1,1))

lr26.60$GBPUSD["2014-03-31::2014-04-01"]
cr2014$GBPUSD["2014-03-31::2014-04-01"]

fun.xts.plot(fx1440.logret.xts,1:18,"2014-04::",h=0.5,ylab="logret")

#-----------------------------------------------------------------------------------------------------------
# 階級数を変えてヒストグラムを求める関数
#-----------------------------------------------------------------------------------------------------------
exhist <- function(   	x, 				#xはデータベクトル
                       n=NULL,    	# 階級数の指定
                       ...)          	# hist に引き渡す任意の引数
{
   N <- length(x)				#データ数を求める
   scale_n <- floor(10*log10(N))		#階級数を求める
   #階級数が与えられていたら
   if(!is.null(n)){		
      scale_n <- n 
   }
   digits_n <- max(nchar(x-trunc(x)))	#有効桁数を求める
   if (digits_n > 2){ 
      digits_n <- digits_n -2
   }else{
      digits_n <- 0
   }
   #階級幅を求める
   M <- round((max(x)-min(x))/scale_n,digits=digits_n)
   #スタートポイントとエンドポイントを求める
   SP <- round((min(x)-M/2),digits=digits_n)
   EP <- round((max(x)+M/2),digits=digits_n)
   #階級ベクトルを設定する
   brk=seq(SP,EP,length=scale_n+1)
   #ヒストグラムを描く
   hist(x,breaks=brk,...)
   #階級ベクトルを返す
   return(brk)
}

#-----------------------------------------------------------------------------------------------------------
# lm関数の結果の取り出し例
#-----------------------------------------------------------------------------------------------------------
# http://cse.naro.affrc.go.jp/takezawa/r-tips/r/71.html

names(lmresults[[1]])
names(summary(lmresults[[1]]))
adj.r.squared.x=summary(lmresults[[1]])$adj.r.squared

# 内容のプロットもできる
plot(lmresults[[1]])

#-----------------------------------------------------------------------------------------------------------
# rolling function from 「ファイナンスの統計学」 # 激遅!
#-----------------------------------------------------------------------------------------------------------
# 例えばmeanなら、頭に計算期間分のNAが入る

rolling=function(x, w=12, f=mean, ...){
   len=length(x)
   result=NULL
   for(i in 1:(len-w+1)){
      result[i+w-1]=f(x[i:(i+w-1)], ...)
   }
   return(result)
}

# 使用例
ma20=rolling(fx1440.xts$USDJPY,20,mean)
plot(as.Date(index(fx1440.xts)),ma20,type="l")

#-----------------------------------------------------------------------------------------------------------
# rolling function from テラモナギ
#-----------------------------------------------------------------------------------------------------------
# 例えばmeanなら、元データよりも計算期間分、短くなる

apply.roll <- function(x,size.window,func){
   result <- numeric(length(x) - size.window +1)
   for(index.end in size.window:length(x)){
      index.start <- (1 + index.end) - size.window
      result[index.start] <- func(x[index.start:index.end])
   }
   return(result)
}

#-----------------------------------------------------------------------------------------------------------
# Rolling by Rcpp & zoo
#-----------------------------------------------------------------------------------------------------------
### RcppRoll
# 例えばmeanなら、元データよりも計算期間分、短くなる

ma20=roll_mean(fx1440.xts$USDJPY,20)

### Zoo
ma100=rollapply(fx1440.xts$USDJPY,100,mean)

#-----------------------------------------------------------------------------------------------------------
# system.time
#-----------------------------------------------------------------------------------------------------------
x.null <- NULL   # 未定義、無値のオブジェクト（以下の代入によりベクトルになる）
system.time(for(i in 1:100000){x.null[i] <- i/100000})

x.df <- data.frame(numeric(100000))   # データフレーム
system.time(for(i in 1:100000){x.df[i,1] <- i/100000})

#-----------------------------------------------------------------------------------------------------------
# bayesGARCH example
#-----------------------------------------------------------------------------------------------------------

library(bayesGARCH)

## LOAD DATA
data(dem2gbp)
y <- dem2gbp[1:750]
## RUN THE SAMPLER (2 chains)
## NB: CONSIDER LARGER l.chain!
MCMC <- bayesGARCH(y, control = list(n.chain = 2, l.chain = 500))
## MCMC ANALYSIS (using coda)
plot(MCMC)
autocorr.diag(MCMC)
gelman.diag(MCMC)
1-rejectionRate(MCMC)
## FORM THE POSTERIOR SAMPLE
smpl <- formSmpl(MCMC, l.bi = 100)
## POSTERIOR STATISTICS
summary(smpl)
smpl <- as.matrix(smpl)
pairs(smpl)

#-----------------------------------------------------------------------------------------------------------
# MSBVARのサンプルコード 引数も結果も複雑でよくわからん。。。
#-----------------------------------------------------------------------------------------------------------

### msbvar

data(HamiltonGDP)
summary(HamiltonGDP)
plot(HamiltonGDP)

set.seed(214)

m2 <- msbvar(HamiltonGDP, p=1, h=2,
             lambda0=0.8, lambda1=0.15, lambda3=1, lambda4=0.25,
             lambda5=1, mu5=0, mu6=0, qm=12,
             alpha.prior=c(100, 30)*diag(2) +
                matrix(12, 2, 2), prior=0, max.iter=30,
             initialize.opt=NULL)

# Now plot the filtered probabilities of a recession

fp.rec <- ts(m2$fp[,1], start=tsp(HamiltonGDP)[1],
             freq=tsp(HamiltonGDP)[3])
plot(fp.rec)



### gibbs.msbvar

data(IsraelPalestineConflict)
summary(IsraelPalestineConflict)
plot(IsraelPalestineConflict)

# Find the mode of an msbvar model
# Initial guess is based on random draw, so set seed.
set.seed(123)

xm <- msbvar(IsraelPalestineConflict, p=3, h=2,
             lambda0=0.8, lambda1=0.15,
             lambda3=1, lambda4=1, lambda5=0, mu5=0,
             mu6=0, qm=12,
             alpha.prior=matrix(c(10,5,5,9), 2, 2))

# Plot out the initial mode
plot(ts(xm$fp))
print(xm$Q)

# Now sample the posterior
N1 <- 1000
N2 <- 2000

# First, so this with random permutation sampling
x1 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=TRUE)

# Identify the regimes using clustering in plotregimeid()
plotregimeid(x1, type="all")

# Now re-estimate based on desired regime identification seen in the
# plots.  Here we are using the intercept of the first equation, so
# Beta.idx=c(7,1).

x2 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=FALSE, Beta.idx=c(7,1))

# Plot regimes
plot.SS(x2)

# Summary of transition matrix
summary(x2$Q.sample)

# Plot of the variance elements
plot(x2$Sigma.sample)

#-----------------------------------------------------------------------------------------------------------
# ccgarch/2 with RFinanceYJ
#-----------------------------------------------------------------------------------------------------------

library(vars)
library(ccgarch)

library(RFinanceYJ)
source("RFinanceYJ_patch.R")

NEC <- quoteStockTsData('6701.t', since='1983-01-04',date.end='2006-03-01')
Hitachi <- quoteStockTsData('6501.t', since='1983-01-04',date.end='2006-03-01')
Toshiba <- quoteStockTsData('6502.t', since='1983-01-04',date.end='2006-03-01')

kabuka.df <- data.frame(NEC$date, NEC$close, Toshiba$close, Hitachi$close)
colnames(kabuka.df) <- c("date", "NEC", "Toshiba", "Hitachi")
kabuka <- read.zoo(kabuka.df)

returns <- 100*diff(log(kabuka))
nobs <- nrow(returns)
ndim <- ncol(returns)

select.lag <- VARselect(returns, type="const", lag.max=20)
nlag <- select.lag$selection[1] #4
var.model <- VAR(returns, p=nlag)
eps <- residuals(var.model)
d.index <- index(returns) #日付処理
eps <- zoo(eps, d.index[(nlag+1):nobs])

a <- c(0.003, 0.005, 0.001)
A <- diag(c(0.2, 0.3, 0.15))
B <- diag(c(0.75, 0.6, 0.8))
dcc.para <- c(0.1, 0.8)

dcc.results <- dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dcc.para, dvar=eps, model="diagonal")
dcc.estimete <- dcc.results$out

# Id <- diag(ndim)
# DCC <- zoo(dcc.results$DCC[, lower.tri(Id)], d.index[(nlag+1):nobs])
# colnames(DCC) <- c("NEC-Toshiba", "NEC-Hitachi", "Toshiba-Hitachi")
# plot(DCC)

#-----------------------------------------------------------------------------------------------------------

# 動的相関係数の出力 by http://d.hatena.ne.jp/hamadakoichi/20110427/p1

# 動的相関係数dcc.results$DCCは、行ごとにt期の条件付相関係数行列を
# ベクトル化した値として保存されているため、
# 全体として（サンプル数）×（次元の２乗）の行列になっている。
head(dcc.results$DCC) #推定された動的条件付相関係数

# プロットする際には対角成分や重複した列を除外する必要があるため、
# ndim 次 元 の DCC-GARCHモデルにおいては、diam(ndim) (単位行列)を設定し、
# lower.tri関数によって正方行列の対角成分を含まない下三角行列の位置を指定する。
Id <- diag(ndim) #単位行列
head(dcc.results$DCC[, lower.tri(Id)]) #下三角行列

DCC <- zoo(dcc.results$DCC[, lower.tri(Id)], d.index[(nlag + 1):nobs]) # zooオブジェクト生成

#列名を付ける
cn <- colnames(kabuka)
cnames <- {}
for (i in 1:(length(cn)-1)) {
   for (j in (i+1):length(cn)) {
      cnames <- c(cnames, paste(cn[i], cn[j], sep = "-"))   			
   }
}
colnames(DCC) <- cnames

head(DCC) # データ確認

plot(DCC)

#-----------------------------------------------------------------------------------------------------------

# install.packages("ccgarch2", repos = c("http://R-Forge.R-project.org", "http://cran.rstudio.com/"), dep = TRUE)

library(ccgarch2)

dcc.results2 <- estimateDCC(data=eps, model="diagonal")
dcc.estimete2 <- summary(dcc.results2)

# Id <- diag(ndim)
DCC2 <- zoo(dcc.results2$DCC[, lower.tri(Id)], d.index[(nlag+1):nobs])
# cn <- colnames(kabuka)
# cnames <- {}
# for (i in 1:(length(cn)-1)) {
#    for (j in (i+1):length(cn)) {
#       cnames <- c(cnames, paste(cn[i], cn[j], sep = "-"))      		
#    }
# }
colnames(DCC2) <- cnames
plot(DCC2)

# 事前にVAR()推定した自前のデータで

nobs <- nrow(data.sample)
ndim <- ncol(data.sample)
nlag <- data.var$p

eps <- residuals(data.var)
eps <- zoo(eps, index(data.sample[(nlag+1):nobs]))

dcc.results2 <- estimateDCC(data=eps, model="diagonal")
summary(dcc.results2)

Id <- diag(ndim)
DCC2 <- zoo(dcc.results2$DCC[, lower.tri(Id)], d.index[(nlag+1):nobs])
colnames(DCC2) <- c("EURUSD-EURJPY", "EURUSD-USDJPY", "EURJPY-USDJPY")
plot(DCC2)

plot(zoo(EUR3b.xts[paste(index(eps[1]), "::", sep="")]))

#-----------------------------------------------------------------------------------------------------------
# ロバスト回帰推定
#-----------------------------------------------------------------------------------------------------------

library(robustbase)
hst <- read.table("C:/Users/Hideshi Honma/Documents/Datas/USDJPY60.csv", sep=",")
x <- 0:287
y <- tail(hst$V3, 288)
fit <- lmrob(y~x)
a <- fit$coefficients[2]
b <- fit$coefficients[1]
b
a
plot(x, y)
curve(a*x+b, add=T, col=2, lty=1, lwd=3)

#-----------------------------------------------------------------------------------------------------------
# SIT percent.rankのコード解析
#-----------------------------------------------------------------------------------------------------------

percent.rank <- function(data,n=10){
   pctRank <- function(x,i) sum(x[i,1] >= x[(i- (n-1) ):i,])
   #x = data, i = 10に対して
   #sum(x[10i, 1] >= x[(10i - (10n-1)) : 10i,]) #i=2 sum(x[11i, 1] >= x[(11i - (10n-1)) : 11i,])
   #sum(x[10, 1] >= x[(10  - 9) : 10,]) #i=2 sum(x[11, 1] >= x[2 : 11,])
   #sum(x[10, 1] >= x[1 : 10,]) #[1]から[10]までの中で[10]以下の要素数の合計数
   
   out = data
   data = coredata(data)
   if( is.null(dim(data)) ) dim(data) = c(len(data),1)
   rng = n:len(data)
   
   out[] = c( rep(NA,(n-1)), sapply(rng, function(i) pctRank(data, i) / n) )
   #計算期間分(n-1)のNAと
   #i = rng([n]から[要素数]までのベクトル)に対してpctrank/要素数をapplyしたものを
   #ｃ結合したベクトルを出力
   return(out)
}

#-----------------------------------------------------------------------------------------------------------
# CalcGARCH.prの書き換え # 速くはならなそう
#-----------------------------------------------------------------------------------------------------------
system.time(CalcGARCH.pr(Calcsize))

x <- outer(1:260, 0:259, "+")
garch.coef <- function(x){
   data.fit = tryCatch(garchFit(formula(form), data.sample[x], include.mean = F, trace = F),
                       error = function(err) FALSE, warning = function(warn) FALSE)
   #coef.list[[i]] <- 
   data.fit@fit$matcoef[,1]
}

CalcGARCH.pr <- function(Calcsize){
   status <<- FALSE
   
   form <- paste('~garch(', p, ', ', q, ')', sep = '')
   #form <- paste('~arma(1, 0) + garch(', p, ', ', q, ')', sep = '')
   
   data.sample <- diff(tail(log(Price), GARCHsize + Calcsize)) * 100
   
   data.result <- rep(NA, Calcsize)
   coef.list <- vector("list", Calcsize)
   sigma.last <- rep(NA, Calcsize)
   
   system.time(
   for(i in seq_along(data.result)){
      data.temp = data.sample[i:(i + GARCHsize - 1)]
      data.last = tail(data.temp, 1)
      
      data.fit = tryCatch(garchFit(formula(form), data.temp, include.mean = F, trace = F),
                          error = function(err) FALSE, warning = function(warn) FALSE)
      coef.list[[i]] <- data.fit@fit$matcoef[,1]
      #sigma.last[i] <- tail(data.fit@sigma.t, 1)
      #if(!is.logical(data.fit)){
      #   data.result[i] <- sqrt(sum(data.fit@fit$matcoef[,1] * c(1, data.last ^ 2, tail(data.fit@sigma.t, 1) ^ 2)))
      #}
   }
   )
   
   if(is.null(garch.pr) && is.na(data.result[1])){
      data.result[1] <- 0
   }else if(is.na(data.result[1])){
      data.result[1] <- tail(garch.pr, 1)
   }
   data.result <- ifna.prev(data.result)
   
   status <<- TRUE
   
   return(data.result)
}

#-----------------------------------------------------------------------------------------------------------
x <- 0
for(i in seq_along(EJ130225[,1])){
   x <- x + as.numeric(log(EJ141016[i]$Close / EJ141016[i]$Open) ^ 2 * 100)
   #x <- x + as.numeric((log(EJ130225[i]$Close) - log(EJ130225[i]$Open)) ^ 2 * 100)
}