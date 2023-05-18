Sys.setenv(TZ="UTC")
packages=c("tseries","ggplot2","anytime","moments","urca","lmtest","vars",
           "RTransferEntropy","quantmod","readxl","zoo","ggfortify","igraph",
           "future","dynlm","QuantPsyc")
installed_packages=packages%in%rownames(installed.packages())
if(any(installed_packages==FALSE)){
  install.packages(packages[!installed_packages],dependencies=TRUE)
}
invisible(lapply(packages,library,character.only=TRUE))

#import of data(yahoo finance and excel)
getSymbols("^GDAXI",from="2017-01-01",to="2022-12-31",auto.assign =TRUE) #DAX Index
getSymbols("^FCHI",from="2017-01-01",to="2022-12-31",auto.assign =TRUE) #CAC 40 Index
getSymbols("BZ=F",from="2017-01-01",to="2022-12-31",auto.assign =TRUE) #Brent Crude Oil
colnames(`BZ=F`)[6]="BZF.Adjusted"
BZF=`BZ=F`
getSymbols("TTF=F",from="2017-01-01",to="2022-12-31",auto.assign =TRUE) #TTF Natural Gas
colnames(`TTF=F`)[6]="TTFF.Adjusted"
TTFF=`TTF=F`
OBX=as.data.frame(read_excel("C:\\Users\\Matej\\Desktop\\OBX.xlsx")) #OBX Index
OBX=xts(x=OBX[2],order.by=as.Date(OBX$time))
CETOP=as.data.frame(read_excel("C:\\Users\\Matej\\Desktop\\CETOP.xlsx"))  #Central European Blue Chip Index
CETOP=xts(x=CETOP[2],order.by=as.Date(CETOP$Date))
Wheat=as.data.frame(read_excel("C:\\Users\\Matej\\Desktop\\Wheat.xlsx"))
Wheat=xts(x=Wheat[6],order.by=as.Date(Wheat$Date)) #Wheat
Rapeseed=as.data.frame(read_excel("C:\\Users\\Matej\\Desktop\\Rapeseed.xlsx")) 
Rapeseed=xts(x=Rapeseed[2],order.by=as.Date(Rapeseed$Date)) #Rapeseed
Corn=as.data.frame(read_excel("C:\\Users\\Matej\\Desktop\\Corn.xlsx")) 
Corn=xts(x=Corn[2],order.by=as.Date(Corn$Date)) #Corn

#NA replacement
na.replace=function(tser){
  matrix=as.data.frame(tser)
  for(i in 2:(nrow(matrix)-1)){
    if(is.na(matrix[i,6])==TRUE){
      matrix[i,6]=1/4*matrix[(i-1),1]+3/4*matrix[(i+1),1]
    }
  }
  return(as.xts(matrix,order.by=as.Date(row.names(matrix))))
}
GDAXI=na.replace(GDAXI)
FCHI=na.replace(FCHI)
BZF=na.replace(BZF)
TTFF=na.replace(TTFF)

SERIES_ALL=merge(GDAXI$GDAXI.Adjusted,FCHI$FCHI.Adjusted,OBX,CETOP,BZF1,TTFF$TTFF.Adjusted,Wheat,Rapeseed,Corn)
SERIES_ALL=na.omit(SERIES_ALL)

autoplot(SERIES_ALL[,1])
autoplot(SERIES_ALL[,2])
autoplot(SERIES_ALL[,3])
autoplot(SERIES_ALL[,4])
autoplot(SERIES_ALL[,5])
autoplot(SERIES_ALL[,6])
autoplot(SERIES_ALL[,7])
autoplot(SERIES_ALL[,8])
autoplot(SERIES_ALL[,9])

#data transformation
SERIES_ALL=diff(log(SERIES_ALL))[-1,]

#stationarity tests
autoplot(SERIES_ALL[,1])+geom_hline(yintercept=mean(SERIES_ALL[,1]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,1],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,2])+geom_hline(yintercept=mean(SERIES_ALL[,2]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,2],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,3])+geom_hline(yintercept=mean(SERIES_ALL[,3]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,3],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,4])+geom_hline(yintercept=mean(SERIES_ALL[,4]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,4],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,5])+geom_hline(yintercept=mean(SERIES_ALL[,5]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,5],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,6])+geom_hline(yintercept=mean(SERIES_ALL[,6]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,6],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,7])+geom_hline(yintercept=mean(SERIES_ALL[,7]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,7],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,8])+geom_hline(yintercept=mean(SERIES_ALL[,8]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,8],type="none",lags=7,selectlags="BIC"))
autoplot(SERIES_ALL[,9])+geom_hline(yintercept=mean(SERIES_ALL[,9]),color="red")+geom_hline(yintercept=0,color="blue")
summary(ur.df(SERIES_ALL[,9],type="none",lags=7,selectlags="BIC"))

BIC=function(model) {
  rss=sum(model$residuals^2)
  t=length(model$residuals)
  npar=length(model$coef)
  return(round(c("p" = npar-1,"BIC" = log(rss/t)+npar*log(t)/t,
                 "R2" = summary(model)$r.squared),4))
}
L=function(tseries,lagnum){
  matrix=tseries[(lagnum+1):(length(tseries))]
  for(i in 1:lagnum){
    matrix=data.frame(matrix,tseries[(lagnum-i+1):(length(tseries)-i)])
  }
  return(as.xts(matrix[,-1],order.by=as.Date(row.names(matrix))))
}

#Granger causality tests
opt_lag_lengths=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
stats_granger=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values=matrix(2,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:ncol(SERIES_ALL)){
  for(j in 1:ncol(SERIES_ALL)){
    order=1:12
    if(i!=j){
      BICs=sapply(order, function(x) BIC(lm(SERIES_ALL[(x+1):nrow(SERIES_ALL),i] ~ L(SERIES_ALL[,i], x) + L(SERIES_ALL[,j], x))))
      testgranger=grangertest(SERIES_ALL[,j],SERIES_ALL[,i],order=which.min(BICs[2,]))
      p_values[j,i]=testgranger[2,4]
      stats_granger[j,i]=testgranger[2,3]
      opt_lag_lengths[j,i]=which.min(BICs[2,])
    }
  }
}

#settings for plots
Arrows=c("GDAXI","GDAXI","GDAXI","FCHI","GDAXI","OBX","GDAXI","CETOP","GDAXI","BZF","GDAXI","TTFF","GDAXI","Wheat","GDAXI","RPS","GDAXI","Corn",
         "FCHI","GDAXI","FCHI","FCHI","FCHI","OBX","FCHI","CETOP","FCHI","BZF","FCHI","TTFF","FCHI","Wheat","FCHI","RPS","FCHI","Corn",
         "OBX","GDAXI","OBX","FCHI","OBX","OBX","OBX","CETOP","OBX","BZF","OBX","TTFF","OBX","Wheat","OBX","RPS","OBX","Corn",
         "CETOP","GDAXI","CETOP","FCHI","CETOP","OBX","CETOP","CETOP","CETOP","BZF","CETOP","TTFF","CETOP","Wheat","CETOP","RPS","CETOP","Corn",
         "BZF","GDAXI","BZF","FCHI","BZF","OBX","BZF","CETOP","BZF","BZF","BZF","TTFF","BZF","Wheat","BZF","RPS","BZF","Corn",
         "TTFF","GDAXI","TTFF","FCHI","TTFF","OBX","TTFF","CETOP","TTFF","BZF","TTFF","TTFF","TTFF","Wheat","TTFF","RPS","TTFF","Corn",
         "Wheat","GDAXI","Wheat","FCHI","Wheat","OBX","Wheat","CETOP","Wheat","BZF","Wheat","TTFF","Wheat","Wheat","Wheat","RPS","Wheat","Corn",
         "RPS","GDAXI","RPS","FCHI","RPS","OBX","RPS","CETOP","RPS","BZF","RPS","TTFF","RPS","Wheat","RPS","RPS","RPS","Corn",
         "Corn","GDAXI","Corn","FCHI","Corn","OBX","Corn","CETOP","Corn","BZF","Corn","TTFF","Corn","Wheat","Corn","RPS","Corn","Corn")
g_widths=c(0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
           1.5,0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,
           1.5,1.5,0,1.5,1.5,1.5,1.5,1.5,1.5,
           1.5,1.5,1.5,0,1.5,1.5,1.5,1.5,1.5,
           1.5,1.5,1.5,1.5,0,1.5,1.5,1.5,1.5,
           1.5,1.5,1.5,1.5,1.5,0,1.5,1.5,1.5,
           1.5,1.5,1.5,1.5,1.5,1.5,0,1.5,1.5,
           1.5,1.5,1.5,1.5,1.5,1.5,1.5,0,1.5,
           1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0)
eqarrowPlot=function(graph,arrowwidths,arrowcolours){
  plot(graph,vertex.size=30,edge.lty=0,edge.arrow.size=0,layout=layout.circle)
  for(i in seq_len(ecount(graph))){
    graph2=delete.edges(graph,E(graph)[(1:ecount(graph))[-i]])
    if(arrowwidths[i]>0.001){
      plot(graph2,vertex.size=30,edge.width=arrowwidths[i],edge.arrow.size=arrowwidths[i]/25,edge.curved=0.2,layout=layout.circle,edge.color=arrowcolours[i],add=TRUE)
    }
  }
}

#Granger causality graph
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
graphPlot=eqarrowPlot(graph(Arrows),g_widths,Arrow_colours)

#Transfer entropy tests
plan(multisession)
ETE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
TE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_TE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=transfer_entropy(as.numeric(SERIES_ALL[,i]),as.numeric(SERIES_ALL[,j]),nboot=1009)
    TE[i,j]=te$coef[1,1]
    TE[j,i]=te$coef[2,1]
    ETE[i,j]=te$coef[1,2]
    ETE[j,i]=te$coef[2,2]
    p_values_TE[i,j]=te$coef[1,4]
    p_values_TE[j,i]=te$coef[2,4]
  }
}

#Transfer entropy graph
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(ETE)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_TE))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

#period 10/2017-01/2020
##Granger causality
opt_lag_lengths_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
stats_granger_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_1719=matrix(2,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:ncol(SERIES_ALL)){
  for(j in 1:ncol(SERIES_ALL)){
    order=1:12
    if(i!=j){
      BICs=sapply(order, function(x) BIC(lm(SERIES_ALL[(x+1):526,i] ~ L(SERIES_ALL[1:526,i], x) + L(SERIES_ALL[1:526,j], x))))
      testgranger=grangertest(SERIES_ALL[1:526,j],SERIES_ALL[1:526,i],order=which.min(BICs[2,]))
      p_values_1719[j,i]=testgranger[2,4]
      stats_granger_1719[j,i]=testgranger[2,3]
      opt_lag_lengths_1719[j,i]=which.min(BICs[2,])
    }
  }
}
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_1719))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),g_widths,Arrow_colours)

##Transfer entropy
plan(multisession)
ETE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
TE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_TE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=transfer_entropy(as.numeric(SERIES_ALL[1:526,i]),as.numeric(SERIES_ALL[1:526,j]),nboot=1009)
    TE_1719[i,j]=te$coef[1,1]
    TE_1719[j,i]=te$coef[2,1]
    ETE_1719[i,j]=te$coef[1,2]
    ETE_1719[j,i]=te$coef[2,2]
    p_values_TE_1719[i,j]=te$coef[1,4]
    p_values_TE_1719[j,i]=te$coef[2,4]
  }
}
tete=transfer_entropy(as.numeric(SERIES_ALL[1:526,6]),as.numeric(SERIES_ALL[1:526,1]),lx=2,ly=2,nboot=1009)
TE_1719[6,1]=tete$coef[1,1]
ETE_1719[6,1]=tete$coef[1,2]
p_values_TE_1719[6,1]=tete$coef[1,4]

Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(ETE_1719)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_TE_1719))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

#period 02/2020-12/2022
##Granger causality
opt_lag_lengths_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
stats_granger_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_2022=matrix(2,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:ncol(SERIES_ALL)){
  for(j in 1:ncol(SERIES_ALL)){
    order=1:12
    if(i!=j){
      BICs=sapply(order, function(x) BIC(lm(SERIES_ALL[(x+527):nrow(SERIES_ALL),i] ~ L(SERIES_ALL[527:nrow(SERIES_ALL),i], x) + L(SERIES_ALL[527:nrow(SERIES_ALL),j], x))))
      testgranger=grangertest(SERIES_ALL[527:nrow(SERIES_ALL),j],SERIES_ALL[527:nrow(SERIES_ALL),i],order=which.min(BICs[2,]))
      p_values_2022[j,i]=testgranger[2,4]
      stats_granger_2022[j,i]=testgranger[2,3]
      opt_lag_lengths_2022[j,i]=which.min(BICs[2,])
    }
  }
}
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_2022))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),g_widths,Arrow_colours)

##Transfer entropy
plan(multisession)
ETE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
TE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_TE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=transfer_entropy(as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),i]),as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),j]),nboot=1009)
    TE_2022[i,j]=te$coef[1,1]
    TE_2022[j,i]=te$coef[2,1]
    ETE_2022[i,j]=te$coef[1,2]
    ETE_2022[j,i]=te$coef[2,2]
    p_values_TE_2022[i,j]=te$coef[1,4]
    p_values_TE_2022[j,i]=te$coef[2,4]
  }
}
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(ETE_2022)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_TE_2022))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

#Renyi transfer entropy
custom_transfer_entropy=function (x, y, lx = 1, ly = 1, q = 0.1, entropy = "Shannon", 
                                  shuffles = 100, type = "quantiles", quantiles = c(5, 95), 
                                  bins = NULL, limits = NULL, nboot = 300, burn = 50, quiet = NULL, 
                                  seed = NULL, na.rm = TRUE) 
{
  if (!is.null(seed)) 
    set.seed(seed)
  t0 <- Sys.time()
  if (length(x) != length(y)) {
    stop("x and y must be of same length.")
  }
  if (is.null(quiet)) 
    quiet <- as.logical(options("RTransferEntropy::quiet"))
  if ("zoo" %in% class(x)) 
    x <- as.numeric(sort(x))
  if ("zoo" %in% class(y)) 
    y <- as.numeric(sort(y))
  type <- tolower(type)
  if (!type %in% c("quantiles", "bins", "limits", "q", "b", 
                   "l")) {
    stop("type must be either 'quantiles', 'bins' or 'limits'.")
  }
  if (nchar(type) == 1) {
    if (type == "q") {
      type <- "quantiles"
    }
    else if (type == "b") {
      type <- "bins"
    }
    else {
      type <- "limits"
    }
  }
  if (length(quantiles) > 20 || length(bins) > 20 || length(limits) > 
      20) {
    stop(paste("Number of classes should not exceed 20.", 
               "Do not expect sensical results when using too many classes and/or lags."))
  }
  if (lx > 20 || ly > 20) {
    stop(paste("Markov order/number of lags should not exceed 20.", 
               "Do not expect sensical results when using too many classes and/or lags."))
  }
  if (lx != ly) {
    warning(paste("Markov order/number of lags should be identical for both", 
                  "time series to facilitate interpretation of results.", 
                  "Consider setting lx = ly."))
  }
  entropy <- tolower(entropy)
  if (nchar(entropy) == 1 && entropy %in% c("s", "r")) {
    entropy <- if (entropy == "s") 
      "shannon"
    else "renyi"
  }
  if (!entropy %in% c("shannon", "renyi")) {
    stop("entropy must be either 'Shannon' or 'Renyi'.")
  }
  if (entropy == "renyi") {
    if (q < 0) {
      stop("q must follow 0 < q < 1")
    }
    else if (q == 1) {
      entropy <- "shannon"
    }
  }
  if (type == "quantiles" && (min(quantiles) < 0 || max(quantiles) > 
                              100)) {
    stop("Quantiles must be between 0 and 100")
  }
  if (type == "quantiles" && max(quantiles) <= 1) {
    warning(paste("Expected quantiles between 0 and 100 but found between 0 and 1,", 
                  "multiplying by 100."))
    quantiles <- quantiles * 100
  }
  if (nboot > 0 && nboot < 100) {
    warning(paste("Number of bootstrap replications is below 100. Use a higher number of", 
                  "bootstrap replications, you are relying on asymptotic arguments here."))
  }
  x <- check_dimension(x)
  y <- check_dimension(y)
  mis_values <- is.na(x) | is.na(y)
  if (na.rm == TRUE) {
    x <- x[!mis_values]
    y <- y[!mis_values]
  }
  else {
    if (any(mis_values)) 
      return(NA)
  }
  if (length(x) == 0) 
    stop("x and y must have non-missing values.")
  if (!quiet) {
    cat(sprintf("%s's entropy on %s core%s with %s shuffle%s.\n", 
                fupper(entropy), future::nbrOfWorkers(), mult_s(future::nbrOfWorkers()), 
                shuffles, mult_s(shuffles)))
    cat(sprintf("  x and y have length %s (%s NAs removed)\n", 
                length(x), sum(mis_values)))
  }
  if (entropy == "shannon") {
    te <- te_shannon(x = x, lx = lx, y = y, ly = ly, shuffles = shuffles, 
                     type = type, quantiles = quantiles, bins = bins, 
                     limits = limits, nboot = nboot, burn = burn, quiet = quiet)
  }
  else {
    te <- custom_te_renyi(x = x, lx = lx, y = y, ly = ly, q = q, 
                          shuffles = shuffles, type = type, quantiles = quantiles, 
                          bins = bins, limits = limits, nboot = nboot, burn = burn, 
                          quiet = quiet)
  }
  if (nboot > 1) {
    seteyx <- sd(te$boot[2, ])
    setexy <- sd(te$boot[1, ])
    pval <- function(x, est) length(x[x > est])/length(x)
    psteyx <- pval(te$boot[2, ], te$teyx)
    pstexy <- pval(te$boot[1, ], te$texy)
  }
  else {
    seteyx <- NA
    setexy <- NA
    psteyx <- NA
    pstexy <- NA
  }
  coef <- matrix(c(te$texy, te$stexy, setexy, pstexy, te$teyx, 
                   te$steyx, seteyx, psteyx), nrow = 2, byrow = T, dimnames = list(c("X->Y", 
                                                                                     "Y->X"), c("te", "ete", "se", "p-value")))
  if (entropy == "shannon") {
    coef[, "te"] <- pmax(0, coef[, "te"])
    coef[, "ete"] <- pmax(0, coef[, "ete"])
  }
  q <- ifelse(entropy == "renyi", q, NA)
  res <- list(entropy = entropy, obs = list(x = x, y = y), 
              coef = coef, nobs = length(x), q = q, boot = te$boot)
  if (!quiet) {
    t <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat("Done - Total time", round(t, 2), "seconds\n")
  }
  class(res) <- "transfer_entropy"
  return(res)
}

custom_te_renyi=function (x, lx, y, ly, q, shuffles, type, quantiles, bins, 
                          limits, nboot, burn, quiet) 
{
  x <- code_sample(x, type, quantiles, bins, limits)
  y <- code_sample(y, type, quantiles, bins, limits)
  if (!quiet) 
    cat("  [calculate] X->Y transfer entropy\n")
  texy <- custom_calc_te_renyi(x = y, lx = ly, y = x, ly = lx, q = q)
  consty <- custom_shuffle_renyi(x = y, lx = ly, y = x, ly = lx, 
                                 q = q, shuffles = shuffles)
  stexy <- texy - consty
  if (!quiet) 
    cat("  [calculate] Y->X transfer entropy\n")
  teyx <- custom_calc_te_renyi(x = x, lx = lx, y = y, ly = ly, q = q)
  constx <- custom_shuffle_renyi(x = x, lx = lx, y = y, ly = ly, 
                                 q = q, shuffles = shuffles)
  steyx <- teyx - constx
  if (nboot > 1) {
    if (!quiet) {
      cat(sprintf("  [bootstrap] %s time%s\n", nboot, 
                  mult_s(nboot)))
    }
    boot <- future.apply::future_sapply(seq_len(nboot), 
                                        function(i) {
                                          custom_bootstrap_renyi(x = x, lx = lx, y = y, ly = ly, 
                                                                 q = q, burn = burn)
                                        }, future.seed = TRUE)
  }
  else {
    boot <- NA
  }
  return(list(teyx = teyx, texy = texy, steyx = steyx, stexy = stexy, 
              boot = boot))
}

custom_shuffle_renyi=function (x, lx, y, ly, q, shuffles) 
{
  n <- length(x)
  if (shuffles > 200) {
    shuffle <- future.apply::future_replicate(shuffles, 
                                              custom_calc_te_renyi(x = x, y = sample(y, n, replace = TRUE), 
                                                                   lx = lx, ly = ly, q = q))
  }
  else {
    shuffle <- replicate(shuffles, custom_calc_te_renyi(x = x, 
                                                        y = sample(y, n, replace = TRUE), lx = lx, ly = ly, 
                                                        q = q))
  }
  ste <- mean(shuffle)
  return(ste)
}

custom_bootstrap_renyi=function (x, lx, y, ly, q, burn = 50) 
{
  bootx <- markov_boot_step(x, lx, burn)
  booty <- markov_boot_step(y, ly, burn)
  dteyx <- custom_calc_te_renyi(x = bootx, lx = lx, y = y, ly = ly, 
                                q = q)
  dtexy <- custom_calc_te_renyi(x = booty, lx = ly, y = x, ly = lx, 
                                q = q)
  teboot <- c(dtexy, dteyx)
  names(teboot) <- c("dtexy", "dteyx")
  return(teboot)
}
custom_calc_te_renyi=function (x, lx, y, ly, q) {
  k1_j <- cluster_gen(x, lx = lx, y, ly = ly)
  k1_j <- k1_j^q
  for(i in 1:length(names(k1_j))){
    names(k1_j)[i]=paste(substr(names(k1_j)[i],1,2*lx),substr(names(k1_j)[i],2*(lx+1),nchar(names(k1_j)[i])),sep="")
  }
  k1_ji=unique(names(k1_j))
  k1_ji_sum=rep(0,length(k1_ji))
  for(i in 1:length(k1_ji)){
    for(j in 1:length(k1_j)){
      if(names(k1_j)[j]==k1_ji[i]){
        k1_ji_sum[i]=k1_ji_sum[i]+k1_j[j]
      }
    }
  }
  k1_ji_sum=k1_ji_sum^(1/q)
  k1 <- cluster_gen(x, lx = lx)
  k1 <- k1^q
  for(i in 1:length(names(k1))){
    names(k1)[i]=paste(substr(names(k1)[i],1,2*lx),substr(names(k1)[i],2*(lx+1),nchar(names(k1)[i])),sep="")
  }
  k1_i=unique(names(k1))
  k1_i_sum=rep(0,length(k1_i))
  for(i in 1:length(k1_i)){
    for(j in 1:length(k1)){
      if(names(k1)[j]==k1_i[i]){
        k1_i_sum[i]=k1_i_sum[i]+k1[j]
      }
    }
  }
  k1_i_sum=k1_i_sum^(1/q)
  numerator <- k1_i_sum
  denominator <- k1_ji_sum
  ren_entropy <- q/(1 - q) * log2(sum(numerator)/sum(denominator))
  return(ren_entropy)
}

environment(custom_calc_te_renyi) <- asNamespace('RTransferEntropy')
assignInNamespace("calc_te_renyi", custom_calc_te_renyi, ns = "RTransferEntropy")
environment(custom_transfer_entropy) <- asNamespace('RTransferEntropy')
assignInNamespace("transfer_entropy", custom_transfer_entropy, ns = "RTransferEntropy")
environment(custom_te_renyi) <- asNamespace('RTransferEntropy')
assignInNamespace("te_renyi", custom_te_renyi, ns = "RTransferEntropy")
environment(custom_shuffle_renyi) <- asNamespace('RTransferEntropy')
assignInNamespace("shuffle_renyi", custom_shuffle_renyi, ns = "RTransferEntropy")
environment(custom_bootstrap_renyi) <- asNamespace('RTransferEntropy')
assignInNamespace("bootstrap_renyi", custom_bootstrap_renyi, ns = "RTransferEntropy")

#custom_transfer_entropy(SERIES_ALL[,1],SERIES_ALL[,2],lx=1,ly=1,entropy="Renyi",q=0.99999999)
#custom_transfer_entropy(SERIES_ALL[,1],SERIES_ALL[,2],lx=1,ly=1,entropy="Renyi",q=1) #check of convergence to Shannon entropy as q-->1

##10/2017-12/2022 
plan(multisession) #q=0.5
RETE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[,i]),as.numeric(SERIES_ALL[,j]),entropy="renyi",q=0.5, nboot=1009)
    RTE[i,j]=te$coef[1,1]
    RTE[j,i]=te$coef[2,1]
    RETE[i,j]=te$coef[1,2]
    RETE[j,i]=te$coef[2,2]
    p_values_RTE[i,j]=te$coef[1,4]
    p_values_RTE[j,i]=te$coef[2,4]
  }
}
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

plan(multisession) #q=1.5
RETE_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[,i]),as.numeric(SERIES_ALL[,j]),entropy="renyi",q=1.5, nboot=1009)
    RTE_up[i,j]=te$coef[1,1]
    RTE_up[j,i]=te$coef[2,1]
    RETE_up[i,j]=te$coef[1,2]
    RETE_up[j,i]=te$coef[2,2]
    p_values_RTE_up[i,j]=te$coef[1,4]
    p_values_RTE_up[j,i]=te$coef[2,4]
  }
}
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE_up)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE_up))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

##10/2017-01/2020
plan(multisession) #q=0.5
RETE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE_1719=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[1:526,i]),as.numeric(SERIES_ALL[1:526,j]),entropy="renyi",q=0.5, nboot=1009)
    RTE_1719[i,j]=te$coef[1,1]
    RTE_1719[j,i]=te$coef[2,1]
    RETE_1719[i,j]=te$coef[1,2]
    RETE_1719[j,i]=te$coef[2,2]
    p_values_RTE_1719[i,j]=te$coef[1,4]
    p_values_RTE_1719[j,i]=te$coef[2,4]
  }
}
tete=custom_transfer_entropy(as.numeric(SERIES_ALL[1:526,6]),as.numeric(SERIES_ALL[1:526,1]),lx=2,ly=2,entropy="renyi",q=0.5,nboot=1009)
RTE_1719[6,1]=tete$coef[1,1]
RETE_1719[6,1]=tete$coef[1,2]
p_values_RTE_1719[6,1]=tete$coef[1,4]

Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE_1719)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE_1719))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

plan(multisession) #q=1.5
RETE_1719_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE_1719_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE_1719_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[1:526,i]),as.numeric(SERIES_ALL[1:526,j]),entropy="renyi",q=1.5, nboot=1009)
    RTE_1719_up[i,j]=te$coef[1,1]
    RTE_1719_up[j,i]=te$coef[2,1]
    RETE_1719_up[i,j]=te$coef[1,2]
    RETE_1719_up[j,i]=te$coef[2,2]
    p_values_RTE_1719_up[i,j]=te$coef[1,4]
    p_values_RTE_1719_up[j,i]=te$coef[2,4]
  }
}
tete=custom_transfer_entropy(as.numeric(SERIES_ALL[1:526,6]),as.numeric(SERIES_ALL[1:526,1]),lx=2,ly=2,entropy="renyi",q=1.5,nboot=1009)
RTE_1719_up[6,1]=tete$coef[1,1]
RETE_1719_up[6,1]=tete$coef[1,2]
p_values_RTE_1719_up[6,1]=tete$coef[1,4]

Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE_1719_up)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE_1719_up))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

##02/2020-12/2022
plan(multisession) #q=0.5
RETE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE_2022=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),i]),as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),j]),entropy="renyi",q=0.5, nboot=1009)
    RTE_2022[i,j]=te$coef[1,1]
    RTE_2022[j,i]=te$coef[2,1]
    RETE_2022[i,j]=te$coef[1,2]
    RETE_2022[j,i]=te$coef[2,2]
    p_values_RTE_2022[i,j]=te$coef[1,4]
    p_values_RTE_2022[j,i]=te$coef[2,4]
  }
}
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE_2022)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE_2022))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

plan(multisession) #q=1.5
RETE_2022_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
RTE_2022_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
p_values_RTE_2022_up=matrix(0,nrow=ncol(SERIES_ALL),ncol=ncol(SERIES_ALL))
for(i in 1:(ncol(SERIES_ALL)-1)){
  for(j in (i+1):ncol(SERIES_ALL)){
    te=custom_transfer_entropy(as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),i]),as.numeric(SERIES_ALL[527:nrow(SERIES_ALL),j]),entropy="renyi",q=1.5, nboot=1009)
    RTE_2022_up[i,j]=te$coef[1,1]
    RTE_2022_up[j,i]=te$coef[2,1]
    RETE_2022_up[i,j]=te$coef[1,2]
    RETE_2022_up[j,i]=te$coef[2,2]
    p_values_RTE_2022_up[i,j]=te$coef[1,4]
    p_values_RTE_2022_up[j,i]=te$coef[2,4]
  }
}
Widths=round(matrix(500,1,(ncol(SERIES_ALL)^2))*c(t(RETE_2022_up)),2)
Arrow_colours=matrix(0,1,(ncol(SERIES_ALL)^2))
for(i in 1:(ncol(SERIES_ALL)^2)){
  if(c(t(p_values_RTE_2022_up))[i]<0.05){
    Arrow_colours[i]="red"
  }else{
    Arrow_colours[i]="grey"
  }
}
eqarrowPlot(graph(Arrows),Widths,Arrow_colours)

#RTE for different values of q
plan(multisession)
qs=c(0.005,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.65,1.7,1.75)
TE_pval=matrix(0,2,length(qs))
for(i in 1:length(qs)){
  te=custom_transfer_entropy(as.numeric(SERIES_ALL[,4]),as.numeric(SERIES_ALL[,8]),entropy="renyi",q=qs[i], nboot=1009)
  TE_pval[1,i]=te$coef[1,1]
  TE_pval[2,i]=te$coef[1,4]
}
plot(qs,TE_pval[1,])