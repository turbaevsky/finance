library(quantmod)

shortlst <- c('CSP.L','FCSS.L','NMC.L')

sp500 <- c('MMM','ABT','ABBV','ACN','ATVI','AYI','ADBE','AMD','AAP','AES','AET','AMG','AFL','A','APD','AKAM','ALK','ALB','ARE','ALXN','ALGN','ALLE','AGN','ADS','LNT','ALL','GOOGL','GOOG','MO','AMZN','AEE','AAL','AEP','AXP','AIG','AMT','AWK','AMP','ABC','AME','AMGN','APH','APC','ADI','ANDV','ANSS','ANTM','AON','AOS','APA','AIV','AAPL','AMAT','ADM','ARNC','AJG','AIZ','T','ADSK','ADP','AZO','AVB','AVY','BHGE','BLL','BAC','BK','BCR','BAX','BBT','BDX','BRK.B','BBY','BIIB','BLK','HRB','BA','BWA','BXP','BSX','BHF','BMY','AVGO','BF.B','CHRW','CA','COG','CPB','COF','CAH','CBOE','KMX','CCL','CAT','CBG','CBS','CELG','CNC','CNP','CTL','CERN','CF','SCHW','CHTR','CHK','CVX','CMG','CB','CHD','CI','XEC','CINF','CTAS','CSCO','C','CFG','CTXS','CLX','CME','CMS','COH','KO','CTSH','CL','CMCSA','CMA','CAG','CXO','COP','ED','STZ','COO','GLW','COST','COTY','CCI','CSRA','CSX','CMI','CVS','DHI','DHR','DRI','DVA','DE','DLPH','DAL','XRAY','DVN','DLR','DFS','DISCA','DISCK','DISH','DG','DLTR','D','DOV','DOW','DPS','DTE','DRE','DD','DUK','DXC','ETFC','EMN','ETN','EBAY','ECL','EIX','EW','EA','EMR','ETR','EVHC','EOG','EQT','EFX','EQIX','EQR','ESS','EL','ES','RE','EXC','EXPE','EXPD','ESRX','EXR','XOM','FFIV','FB','FAST','FRT','FDX','FIS','FITB','FE','FISV','FLIR','FLS','FLR','FMC','FL','F','FTV','FBHS','BEN','FCX','GPS','GRMN','IT','GD','GE','GGP','GIS','GM','GPC','GILD','GPN','GS','GT','GWW','HAL','HBI','HOG','HRS','HIG','HAS','HCA','HCP','HP','HSIC','HSY','HES','HPE','HLT','HOLX','HD','HON','HRL','HST','HPQ','HUM','HBAN','IDXX','INFO','ITW','ILMN','IR','INTC','ICE','IBM','INCY','IP','IPG','IFF','INTU','ISRG','IVZ','IRM','JEC','JBHT','SJM','JNJ','JCI','JPM','JNPR','KSU','K','KEY','KMB','KIM','KMI','KLAC','KSS','KHC','KR','LB','LLL','LH','LRCX','LEG','LEN','LVLT','LUK','LLY','LNC','LKQ','LMT','L','LOW','LYB','MTB','MAC','M','MRO','MPC','MAR','MMC','MLM','MAS','MA','MAT','MKC','MCD','MCK','MDT','MRK','MET','MTD','MGM','KORS','MCHP','MU','MSFT','MAA','MHK','TAP','MDLZ','MON','MNST','MCO','MS','MOS','MSI','MYL','NDAQ','NOV','NAVI','NTAP','NFLX','NWL','NFX','NEM','NWSA','NWS','NEE','NLSN','NKE','NI','NBL','JWN','NSC','NTRS','NOC','NRG','NUE','NVDA','ORLY','OXY','OMC','OKE','ORCL','PCAR','PKG','PH','PDCO','PAYX','PYPL','PNR','PBCT','PEP','PKI','PRGO','PFE','PCG','PM','PSX','PNW','PXD','PNC','RL','PPG','PPL','PX','PCLN','PFG','PG','PGR','PLD','PRU','PEG','PSA','PHM','PVH','QRVO','PWR','QCOM','DGX','RRC','RJF','RTN','O','RHT','REG','REGN','RF','RSG','RMD','RHI','ROK','COL','ROP','ROST','RCL','CRM','SCG','SLB','SNI','STX','SEE','SRE','SHW','SIG','SPG','SWKS','SLG','SNA','SO','LUV','SPGI','SWK','SPLS','SBUX','STT','SRCL','SYK','STI','SYMC','SYF','SNPS','SYY','TROW','TGT','TEL','FTI','TXN','TXT','TMO','TIF','TWX','TJX','TMK','TSS','TSCO','TDG','TRV','TRIP','FOXA','FOX','TSN','UDR','ULTA','USB','UA','UAA','UNP','UAL','UNH','UPS','URI','UTX','UHS','UNM','VFC','VLO','VAR','VTR','VRSN','VRSK','VZ','VRTX','VIAB','V','VNO','VMC','WMT','WBA','DIS','WM','WAT','WEC','WFC','HCN','WDC','WU','WRK','WY','WHR','WFM','WMB','WLTW','WYN','WYNN','XEL','XRX','XLNX','XL','XYL','YUM','ZBH','ZION','ZTS')

ftse250 <- c('3IN.L','AA.L','ASL.L','ACA.L','AGK.L','ALD.L','ATST.L','AMFW.L','ASCL.L','ASHM.L','AGR.L','AUTO.L','AVV.L','BME.L','BBY.L','BNKR.L','BAG.L','BBA.L','BEZ.L','BWY.L','BRSN.L','BKG.L','BGEO.L','BYG.L','BOY.L','BOK.L','BVS.L','BTEM.L','BRW.L','BVIC.L','BWNG.L','BTG.L','CNE.L','CLDN.L','CAPC.L','CPI.L','CARD.L','CLLN.L','CEY.L','CINE.L','CTY.L','CKN.L','CBG.L','CLI.L','COA.L','COB.L','CCC.L','CSP.L','CWK.L','CRST.L','CYBG.L','DJAN.L','DCG.L','DPH.L','DLN.L','DTY.L','DPLM.L','DC.L','DOM.L','DRX.L','DNLM.L','EDIN.L','ELTA.L','ECM.L','ELM.L','ETO.L','ESNT.L','ESUR.L','ERM.L','EVR.L','FCPT.L','FDM.L','FXPO.L','FEV.L','FCSS.L','FDSA.L','FGT.L','FGP.L','FSJ.L','FRCL.L','GFRD.L','GCP.L','GSS.L','GNS.L','GOG.L','GPOR.L','GFTU.L','GRI.L','UKW.L','GNC.L','GNK.L','GRG.L','GVC.L','HFD.L','HLMA.L','HSTN.L','HVPE.L','HSTG.L','HAS.L','HICL.L','HIK.L','HILS.L','HSX.L','HOC.L','HSV.L','HWDN.L','HTG.L','IBST.L','IGG.L','IMI.L','INCH.L','INDV.L','ISAT.L','ICP.L','INPP.L','INTU.L','INVP.L','IPO.L','IWG.L','JLT.L','JD.L','CHOO.L','JLG.L','JLIF.L','JII.L','JAM.L','JMG.L','JUP.L','JE.L','JUST.L','KAZ.L','KWE.L','KIE.L','LCL.L','LRE.L','LMP.L','EMG.L','MSLH.L','MARS.L','MCS.L','MGGT.L','MRO.L','MRC.L','MTRO.L','MLC.L','MAB.L','MTO.L','MONY.L','MNKS.L','MGAM.L','MYI.L','NEX.L','NBLS.L','NRR.L','NXG.L','NMC.L','NTG.L','NOG.L','OCDO.L','OSB.L','P2P.L','PAGE.L','PAG.L','PAY.L','PAYS.L','PNN.L','PLI.L','PSH.L','PNL.L','PDL.L','PFC.L','PETS.L','PHNX.L','PTEC.L','PCT.L','POLY.L','PLP.L','PZC.L','QQ.L','RNK.L','RAT.L','RDI.L','RDW.L','TRIG.L','RSW.L','RTN.L','RMV.L','RCP.L','RSE.L','ROR.L','RPC.L','SAFE.L','SAGA.L','SNN.L','SVS.L','SCIN.L','SNR.L','SRP.L','SHB.L','SHI.L','SXX.L','SMDS.L','SCT.L','SOPH.L','SXS.L','SPX.L','SPI.L','SPD.L','SSPG.L','SMP.L','SGC.L','STOB.L','SGP.L','SYNC.L','SYNT.L','TALK.L','TATE.L','TBCG.L','TED.L','TEP.L','TMPL.L','TEM.L','TCG.L','TCAP.L','TRY.L','TPK.L','BBOX.L','TLW.L','UBM.L','UDG.L','UKCM.L','ULE.L','UTG.L','VEC.L','VED.L','VSVS.L','VCT.L','VEIL.L','VM.L','WEIR.L','JDW.L','SMWH.L','WMH.L','WTAN.L','WIZZ.L','WG.L','WPCT.L','WKP.L','WWH.L','ZPG.L')

#####################################################################
#####################################################################
#lst <- shortlst
lst <- sp500
#lst <- ftse250

for (n in lst) {
    try({
        print(paste('Getting',n))
        obj <- getSymbols(n,src='google',env=NULL)
        obj <- na.omit(obj)
    # get quotes
    q <- getQuote(n)
    d <- Sys.Date()
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
    q <- xts(q,d)
    #print(q)
                                        #obj <- merge(obj,q,by="Date")
    ################ Add current values ##################
    obj <- rbind(obj,q)
    obj <- obj[ ! duplicated( index(obj), fromLast = TRUE ),  ]


                                        # Check ADX
    adx <- last(ADX(obj[,c(2,3,4)]))$ADX[[1]]
    div <- MACD(obj[,4],12,26,9,maType='EMA')
    macd <- last(div)$macd[[1]]
    div <- div$macd-div$signal
    Div3 <- last(div)[[1]]
    Div2 <- div[nrow(div)-1,][[1]]
    Div1 <- div[nrow(div)-2,][[1]]
    #print(n)
    sma200 <- last(SMA(obj[,4],200))[[1]]
    lst <- last(obj[,4])[[1]]
    low <- last(obj[,3])[[1]]
    sma <- last(SMA(obj[,4],10))[[1]]
    ema <- last(EMA(obj[,4],30))[[1]]
        atr <- last(ATR(obj))$atr[[1]]
        pct <- last(BBands(obj[,4]))$up[[1]]
        pct <- (pct-lst)/pct

        volat <- last(volatility(obj))[[1]]
        #print(volat,calc='Close')
    #print(paste(n,'adx=',adx,'last=',lst,'sma200=',sma200,'macd=',macd,
#'lastDiv=',lastDiv,',prelast=',prelastDiv,'first=',preprelastDiv))
    print(paste('Testing',n))
                                        #print(tail(div))
                                        # Check unusial volume
        lastV <- head(tail(obj[,5],2),1)[[1]]
        avgV <- mean(unlist(head(tail(obj[,5],5),3)))[[1]] # Avg of last 3 elements
        factor <- lastV/avgV
        #print(tail(obj))
        #print(paste(n,'fact=',factor,'last=',lastV,'avg=',avgV))
############################################################################
############################################################################
    if (factor>=1.2 && avgV>=1e5 && lst>sma200 && macd>0 && Div3>=Div2 && Div2>=Div1 && Div1<0){
                                        # Update quotes
    print(paste(n,'adx=',adx,'last=',lst,'sma200=',sma200,'macd=',macd,
                'lastDiv=',Div1,Div2,Div3,'atr=',atr,'sma10=',sma,
                'low=',low,'pctB=',pct,'volat.=',volat,'Vol.fact=',factor))
    print(paste('Plotting',n))
    print(tail(obj))
    # Chart
    # MACD could be (5,34,5) to see Elliot's wave
    #chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo(),addATR(20)),multi.col=FALSE,name=n)
    # addBBonds()
        chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
        invisible(readline(prompt="Press [enter] to continue"))
        chartSeries(obj,subset='last 9 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
        invisible(readline(prompt="Press [enter] to continue"))
    }})}
