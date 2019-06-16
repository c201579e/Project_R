#######################################
#Volatility Prediction by E-GARCH Model
#######################################

# Note: this process could take a couple of minutes

#Installation of needed packages if it is not installed
#Installing function
installer <- function(x){
  for( i in x ){
    #  If we can load the package, return TRUE
    if( ! require( i , character.only = TRUE ) ){
      # cannot load, then install packages
      install.packages( i , dependencies = TRUE )
      #  after installation, load the -package
      require( i , character.only = TRUE )
    }
  }
}

#packages required
installer(c("rugarch","xts","forecast","fGarch","dplyr"))

nikkei <- read.csv(file="~/N225.csv")
n225<- as.xts(read.zoo(nikkei,header=TRUE,sep=","))
head(n225)

#Function: Seach for the most suitable value for ARIMA
sp <- n225$LogReturn
opt.au.arima <- auto.arima(sp,ic="aic",max.p=3,max.q=3,
                           start.P=0,start.Q=0,
                           stepwise=T,trace=T,approximation=F)
#Most suitable value: ARIMA(0,0,0)

#plot REsiduals and ACF from ARIMA model
tsdiag(opt.au.arima,10)

#Loop Function: Search for the most suitable value for GARCH
sp.garch <- as.list(NULL)
i <- 1; for (P in 1:5){ for (Q in 0:5){
  sp.garch[[i]] <- try(eval(parse(text =
                                    paste("garchFit( ~ garch(",P,",",Q,"),data =sp, trace=FALSE)")
  )),silent=TRUE)
  i <- i + 1}}
#optimizing result
opt.sp.garch <-
  Reduce(function(x,y) if(x@fit$ics[1] < y@fit$ics[1]){x}else{y}, sp.garch)
opt.sp.garch

#Most suitable value: GARCH(3,0)
#GARCH(3,0)。
spec.sg <- ugarchfit(spec = ugarchspec(mean.model = list(armaOrder = c(0,0,0),
                                                         include.mean = TRUE), variance.model = list(model = "sGARCH", garchOrder = c(3, 0))), 
                     solver.control = list(tol = 1e-12),data = sp)
#AIC of ARIMA(0,0,0)-GARCH(3,0)。
aic.sg <- infocriteria(spec.sg)[1]

#eGARCH(1,1)。
#eGARCH does not converges when we use garchOrder(3,0) as like in sGARCH(basic GARCH model)
spec.eg <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                        include.mean = TRUE), variance.model = list(model = "eGARCH", garchOrder = c(1,1)))

#AIC of ARIMA(0,0,0)-eGARCH(1,0)。
fit.eg <- ugarchfit(spec.eg,sp)
aic.eg <- infocriteria(fit.eg)[1]

#Compare AIC of sGARCH and eGARCH
print(c(aic.sg = aic.sg, aic.eg = aic.eg))

#Prediction of Volatility 
roll.eg <- ugarchroll(
  spec.eg, sp,
  #use every 50 days data to predict.
  refit.every = 50, 
  #predict 1 day ahead.
  n.ahead = 1,
  #Assign the start point of prediction 
  n.start = 100, #predict from 100 days ahead. dataframe contains 255 datas.
  refit.window = "moving", solver = "hybrid", keep.coef = TRUE)

#Show the Result of Volatility Prediction
show(roll.eg)

#Convert ugarchroll to data.frame
df.eg <- as.data.frame(roll.eg)

#save plots in png and jpeg
#f1 <- "sample1.png"
#f2 <- "sample2.jpeg"
par(mfrow=c(2,1))
#plot predicted volatility
#png(f1, width = 800, height = 600)
plot(df.eg[, 'Sigma', drop = FALSE]$Sigma,type="l")
#dev.off()
#Calculate Return
N225 <- mutate(nikkei,Return = (1+(Close-Open)/Open))
#Compare with Return of Nikkei225
#png(f2, width = 800, height = 600)
plot(tail(N225,n=155)$Return,type="l")
#dev.off()
par(mfrow=c(1,1))