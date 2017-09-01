library(quantmod)
library(RCurl)
library(jsonlite)

sp500 <- c('MMM','ABT','ABBV','ACN','ATVI','AYI','ADBE','AMD','AAP','AES','AET','AMG','AFL','A','APD','AKAM','ALK','ALB','ARE','ALXN','ALGN','ALLE','AGN','ADS','LNT','ALL','GOOGL','GOOG','MO','AMZN','AEE','AAL','AEP','AXP','AIG','AMT','AWK','AMP','ABC','AME','AMGN','APH','APC','ADI','ANDV','ANSS','ANTM','AON','AOS','APA','AIV','AAPL','AMAT','ADM','ARNC','AJG','AIZ','T','ADSK','ADP','AZO','AVB','AVY','BHGE','BLL','BAC','BK','BCR','BAX','BBT','BDX','BRK.B','BBY','BIIB','BLK','HRB','BA','BWA','BXP','BSX','BHF','BMY','AVGO','BF.B','CHRW','CA','COG','CPB','COF','CAH','CBOE','KMX','CCL','CAT','CBG','CBS','CELG','CNC','CNP','CTL','CERN','CF','SCHW','CHTR','CHK','CVX','CMG','CB','CHD','CI','XEC','CINF','CTAS','CSCO','C','CFG','CTXS','CLX','CME','CMS','COH','KO','CTSH','CL','CMCSA','CMA','CAG','CXO','COP','ED','STZ','COO','GLW','COST','COTY','CCI','CSRA','CSX','CMI','CVS','DHI','DHR','DRI','DVA','DE','DLPH','DAL','XRAY','DVN','DLR','DFS','DISCA','DISCK','DISH','DG','DLTR','D','DOV','DOW','DPS','DTE','DRE','DD','DUK','DXC','ETFC','EMN','ETN','EBAY','ECL','EIX','EW','EA','EMR','ETR','EVHC','EOG','EQT','EFX','EQIX','EQR','ESS','EL','ES','RE','EXC','EXPE','EXPD','ESRX','EXR','XOM','FFIV','FB','FAST','FRT','FDX','FIS','FITB','FE','FISV','FLIR','FLS','FLR','FMC','FL','F','FTV','FBHS','BEN','FCX','GPS','GRMN','IT','GD','GE','GGP','GIS','GM','GPC','GILD','GPN','GS','GT','GWW','HAL','HBI','HOG','HRS','HIG','HAS','HCA','HCP','HP','HSIC','HSY','HES','HPE','HLT','HOLX','HD','HON','HRL','HST','HPQ','HUM','HBAN','IDXX','INFO','ITW','ILMN','IR','INTC','ICE','IBM','INCY','IP','IPG','IFF','INTU','ISRG','IVZ','IRM','JEC','JBHT','SJM','JNJ','JCI','JPM','JNPR','KSU','K','KEY','KMB','KIM','KMI','KLAC','KSS','KHC','KR','LB','LLL','LH','LRCX','LEG','LEN','LVLT','LUK','LLY','LNC','LKQ','LMT','L','LOW','LYB','MTB','MAC','M','MRO','MPC','MAR','MMC','MLM','MAS','MA','MAT','MKC','MCD','MCK','MDT','MRK','MET','MTD','MGM','KORS','MCHP','MU','MSFT','MAA','MHK','TAP','MDLZ','MON','MNST','MCO','MS','MOS','MSI','MYL','NDAQ','NOV','NAVI','NTAP','NFLX','NWL','NFX','NEM','NWSA','NWS','NEE','NLSN','NKE','NI','NBL','JWN','NSC','NTRS','NOC','NRG','NUE','NVDA','ORLY','OXY','OMC','OKE','ORCL','PCAR','PKG','PH','PDCO','PAYX','PYPL','PNR','PBCT','PEP','PKI','PRGO','PFE','PCG','PM','PSX','PNW','PXD','PNC','RL','PPG','PPL','PX','PCLN','PFG','PG','PGR','PLD','PRU','PEG','PSA','PHM','PVH','QRVO','PWR','QCOM','DGX','RRC','RJF','RTN','O','RHT','REG','REGN','RF','RSG','RMD','RHI','ROK','COL','ROP','ROST','RCL','CRM','SCG','SLB','SNI','STX','SEE','SRE','SHW','SIG','SPG','SWKS','SLG','SNA','SO','LUV','SPGI','SWK','SPLS','SBUX','STT','SRCL','SYK','STI','SYMC','SYF','SNPS','SYY','TROW','TGT','TEL','FTI','TXN','TXT','TMO','TIF','TWX','TJX','TMK','TSS','TSCO','TDG','TRV','TRIP','FOXA','FOX','TSN','UDR','ULTA','USB','UA','UAA','UNP','UAL','UNH','UPS','URI','UTX','UHS','UNM','VFC','VLO','VAR','VTR','VRSN','VRSK','VZ','VRTX','VIAB','V','VNO','VMC','WMT','WBA','DIS','WM','WAT','WEC','WFC','HCN','WDC','WU','WRK','WY','WHR','WFM','WMB','WLTW','WYN','WYNN','XEL','XRX','XLNX','XL','XYL','YUM','ZBH','ZION','ZTS')

ftse250 <- c('3IN.L','AA.L','ASL.L','ACA.L','AGK.L','ALD.L','ATST.L','AMFW.L','ASCL.L','ASHM.L','AGR.L','AUTO.L','AVV.L','BME.L','BBY.L','BNKR.L','BAG.L','BBA.L','BEZ.L','BWY.L','BRSN.L','BKG.L','BGEO.L','BYG.L','BOY.L','BOK.L','BVS.L','BTEM.L','BRW.L','BVIC.L','BWNG.L','BTG.L','CNE.L','CLDN.L','CAPC.L','CPI.L','CARD.L','CLLN.L','CEY.L','CINE.L','CTY.L','CKN.L','CBG.L','CLI.L','COA.L','COB.L','CCC.L','CSP.L','CWK.L','CRST.L','CYBG.L','DJAN.L','DCG.L','DPH.L','DLN.L','DTY.L','DPLM.L','DC.L','DOM.L','DRX.L','DNLM.L','EDIN.L','ELTA.L','ECM.L','ELM.L','ETO.L','ESNT.L','ESUR.L','ERM.L','EVR.L','FCPT.L','FDM.L','FXPO.L','FEV.L','FCSS.L','FDSA.L','FGT.L','FGP.L','FSJ.L','FRCL.L','GFRD.L','GCP.L','GSS.L','GNS.L','GOG.L','GPOR.L','GFTU.L','GRI.L','UKW.L','GNC.L','GNK.L','GRG.L','GVC.L','HFD.L','HLMA.L','HSTN.L','HVPE.L','HSTG.L','HAS.L','HICL.L','HIK.L','HILS.L','HSX.L','HOC.L','HSV.L','HWDN.L','HTG.L','IBST.L','IGG.L','IMI.L','INCH.L','INDV.L','ISAT.L','ICP.L','INPP.L','INTU.L','INVP.L','IPO.L','IWG.L','JLT.L','JD.L','CHOO.L','JLG.L','JLIF.L','JII.L','JAM.L','JMG.L','JUP.L','JE.L','JUST.L','KAZ.L','KWE.L','KIE.L','LCL.L','LRE.L','LMP.L','EMG.L','MSLH.L','MARS.L','MCS.L','MGGT.L','MRO.L','MRC.L','MTRO.L','MLC.L','MAB.L','MTO.L','MONY.L','MNKS.L','MGAM.L','MYI.L','NEX.L','NBLS.L','NRR.L','NXG.L','NMC.L','NTG.L','NOG.L','OCDO.L','OSB.L','P2P.L','PAGE.L','PAG.L','PAY.L','PAYS.L','PNN.L','PLI.L','PSH.L','PNL.L','PDL.L','PFC.L','PETS.L','PHNX.L','PTEC.L','PCT.L','POLY.L','PLP.L','PZC.L','QQ.L','RNK.L','RAT.L','RDI.L','RDW.L','TRIG.L','RSW.L','RTN.L','RMV.L','RCP.L','RSE.L','ROR.L','RPC.L','SAFE.L','SAGA.L','SNN.L','SVS.L','SCIN.L','SNR.L','SRP.L','SHB.L','SHI.L','SXX.L','SMDS.L','SCT.L','SOPH.L','SXS.L','SPX.L','SPI.L','SPD.L','SSPG.L','SMP.L','SGC.L','STOB.L','SGP.L','SYNC.L','SYNT.L','TALK.L','TATE.L','TBCG.L','TED.L','TEP.L','TMPL.L','TEM.L','TCG.L','TCAP.L','TRY.L','TPK.L','BBOX.L','TLW.L','UBM.L','UDG.L','UKCM.L','ULE.L','UTG.L','VEC.L','VED.L','VSVS.L','VCT.L','VEIL.L','VM.L','WEIR.L','JDW.L','SMWH.L','WMH.L','WTAN.L','WIZZ.L','WG.L','WPCT.L','WKP.L','WWH.L','ZPG.L')

#####################################################################
#####################################################################
shortlst <- c('FXPO.L','KAZ.L','WIZZ.L')
excluded <- c('GNC.L','IWG.L')
lim <- c(0,0,0,0,0) # Stop-loss limit
stocks <- c(0,0,0,0,0,0) # No of stocks
tax <- c(0,0,0,0,0,0) # Taxes
long <- c(0,0,0,0,0,0) # Buying price
#lst <- shortlst
#quotes <- TRUE
#web <- TRUE
#lst <- sp500
                                        #lst <- ftse250
#####################################################################
### Functions ###
#####################################################################
gQuote <- function(n='FXPO.L'){ # get quotes from google (real-time)
    url <- paste('http://finance.google.com/finance/info?client=ig&q=',n,sep='')
    data <- getURL(url)
    json <- fromJSON(gsub('\n|[//]|[\\]','',data))
    return(paste(json$lt,json$l))
}
updQuote <- function(obj,n) #Update price table by actual quotes
{
    q <- getQuote(n)
    d <- Sys.Date()
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
    q <- xts(q,d)
    obj <- rbind(obj,q)
    obj <- obj[!duplicated(index(obj),fromLast = TRUE ),]
    return(obj)
}

dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}
longDate <- function(d) {return(as.Date(as.character(d),'%b %d, %Y'))}

#' Data convertor from finantial time
toXTS <- function(data){
    d <- cbind(as.Date(apply(data[1],2,longDate)),data[2])
    return(xts(d[,-1], order.by=d[,1]))
}

#' Quote update
upd <- function(){
    d <- 'GB00B886CK92.L'
    b <- 'GB00B8QYPR36.L'
    uk <- '^UK50'
    print(getQuote(d))
    print(getQuote(b))
    print(getQuote(uk))
}
#####################################################################
analyse <- function(lst=ftse250,quotes=TRUE,web=FALSE,change=5,adx=25){
    for (n in lst) {
        if (!n %in% excluded) {
        try({
            print(paste('Getting',n))
            obj <- getSymbols(n,src='google',env=NULL)
            obj <- na.omit(obj)
            if (quotes) obj <- updQuote(obj,n)

            adx <- last(ADX(obj[,c(2,3,4)]))$ADX[[1]]
            macd <- MACD(obj[,4],12,26,9,maType='EMA')
            lmacd <- last(macd)$macd[[1]]
            lsig <- last(macd)$signal[[1]]
            div <- macd$macd-macd$signal
            sma200 <- last(SMA(obj[,4],200))[[1]]
            cls <- last(obj[,4])[[1]]
            low <- last(obj[,3])[[1]]
            sma <- last(SMA(obj[,4],10))[[1]]
            lsma <- SMA(obj[,4])
            lsma50 <- lsma[length(lsma)-50][[1]]
            change50 <- (sma-lsma50)/sma*100
            lsma15 <- lsma[length(lsma)-15][[1]]
            change15 <- (sma-lsma15)/sma*100
            lsma5 <- lsma[length(lsma)-5][[1]]
            change5 <- (sma-lsma5)/sma*100
            ema <- last(EMA(obj[,4],30))[[1]]
            atr <- last(ATR(obj))$atr[[1]]
            pct <- last(BBands(obj[,4]))$up[[1]]
            pct <- (pct-cls)/pct
                                        #volat <- last(ADR(obj))[[1]] # If you’re swing trading, you want stocks that show high ADRs.
                                        #print(volat,calc='Close')
            rsi <- last(RSI(obj[,4]))[[1]]
            bol <- last(BBands(obj[,c(2:4)]))
                                        # Check unusial volume
            lastV <- head(tail(obj[,5],2),1)[[1]]
            avgV <- mean(unlist(head(tail(obj[,5],32),30)))[[1]] # Avg of last 30 elements
            factor <- lastV/avgV
            p <- length(div)
                                        #print(paste(n,'div=',div[p-3][[1]],div[p-2][[1]],div[p-1][[1]],div[p][[1]],'MACD=',macd,'ADX=',adx,'avgVol=',avgV,'SMA200=',sma200,'Close=',cls))
############################################################################
############################################################################
            k <- 0.05
            if (n %in% shortlst || (avgV>=1e5 && cls>sma200 && cls>=5 &&
                ((div[p-3][[1]]>div[p-2][[1]] && div[p-2][[1]]<div[p-1][[1]] && div[p-1][[1]]<div[p][[1]])
                    || (div[p-2][[1]]>div[p-1][[1]] && div[p-1][[1]]<div[p][[1]]) || ((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd))
                && adx>=adx && change15>change)){# && low<=bol[[1]]){
### Add SMI cross criteria AND lmacd>(<)0 removed ##########################
                cond <- ''
                if (div[p-3][[1]]>div[p-2][[1]] && div[p-2][[1]]<div[p-1][[1]] && div[p-1][[1]]<div[p][[1]]) cond <- paste(cond,'two risen divs,')
                if (div[p-2][[1]]>div[p-1][[1]] && div[p-1][[1]]<div[p][[1]])
                    cond <- paste(cond,'hole divs,')
                if ((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd) cond <- paste(cond,'macd crossed signal,')

                print(paste('Plotting',n,'because of',cond,'adx=',round(adx),'atr=',signif(atr,2),'rsi14(30long/70short)=',round(rsi),'change,% 1w/3w/3m=',signif(change5,2),'/',signif(change15,2),'/',signif(change50,2)))
                if (web==TRUE)
                    browseURL(paste('https://finance.yahoo.com/calendar/earnings?day=',format(Sys.Date(),'%Y-%m-%d'),'&symbol=',n,sep=''))
                print(tail(obj))
                print(tail(merge(div,macd)))
                if (quotes) try(print(getOptionChain(n)))
                if (n %in% shortlst){
                    no <- which(shortlst == n)
                    balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
                    print(paste('Balance=',signif(balance,3)))
                }
                no <- which(shortlst == n)
# Chart ################################################################
                                        # MACD could be (5,34,5) to see Elliot's wave

                chartSeries(obj,subset='last 9 months',TA=c(addSMA(200),addMACD(),addSMA(),addEMA(30),addVo()),multi.col=FALSE,name=n,log.scale=T)
                plot(addLines(h=lim[no],col='red'))
                invisible(readline(prompt="Press [enter] to continue"))
                chartSeries(obj,subset='last 3 months',TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
                plot(addLines(h=lim[no],col='red'))
                invisible(readline(prompt="Press [enter] to continue"))
            }})}}}

ticker <- function(n='FXPO.L',min=10,subset='last 3 months'){
    obj <- getSymbols(n,src='google',env=NULL)
    obj <- na.omit(obj)
    while (T) {
        obj <- updQuote(obj,n)
        #print(getQuote(n))
        if (n %in% shortlst){
            no <- which(shortlst == n)
            cls <- getQuote(n)[[2]]
            tm <- getQuote(n)[[1]]
            balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
            #print(paste(format(Sys.time(),'%H:%M'),'Balance=',signif(balance,3),'Last=',cls,'(',tm,')'))
            print(gQuote(n))
        }
        chartSeries(obj,subset=subset,TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
        plot(addLines(h=lim[no],col='red'))
        Sys.sleep(60*min)}
}
##############################################################################
### Funds charting using csv ###
#############################################################################

stat <- function(data='~/Desktop/b886ck92.csv',series='S',subset='last 6 months'){
# '~/Desktop/b8qypr36.csv
    ck92 <- read.csv(data)
    ck <- toXTS(ck92)
    if (series=='S'){
        chartSeries(ck,subset=subset,TA=c(addSMA(),addEMA(30),addSMA(200),addMACD()))
        #addMACD()
        #addEMA()
        #addCCI()
        #addRSI()
        }
    else if (series=='w') barplot(weeklyReturn(ck))
    else if (series=='m') barplot(monthlyReturn(ck))
}
