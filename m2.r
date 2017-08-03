library(quantmod)

shortlst <- c('III.L','ARM.L','ASC.L','AGK.L','AMFW.L','AAL.L','ABF.L','AZN.L','BLT.L','BP.L','BOY.L','BATS.L','BNZL.L','BRBY.L','CSR.L','CCH.L','DCG.L','ESNT.L','EXPN.L','FRES.L','FLG.L','GWP.L','GSK.L','HSBA.L','HIK.L','HTG.L','IAP.L','INCH.L','IHG.L','ICP.L','ISYS.L','INVP.L','WG.L','JMAT.L','JUP.L','JE.L','KAZ.L','LAND.L','LSE.L','MNDI.L','NXG.L','NTG.L','PAGE.L','PAG.L','PHNX.L','PTEC.L','PLUS.L','POLY.L','PRU.L','RRS.L','REX.L','RIO.L','SGRO.L','SDR.L','SMT.L','SVT.L','SHP.L','STJ.L','STAN.L','SL.L','TT.L','SGE.L','TPK.L','ULVR.L','UU.L','VED.L','CKSN.L','WEIR.L','WTB.L')

extlst <- c('III.L','ARM.L','ASC.L','ABC.L','ADM.L','AGK.L','ATST.L','AML.L','AAL.L','ANTO.L','AHT.L','ABF.L','AZN.L','AV.L','BA.L','BLT.L','BTG.L','BAB.L','BDEV.L','BWY.L','BRSN.L','BKG.L','BET.L','BOY.L','BVS.L','BATS.L','BLND.L','BVIC.L','BNZL.L','BRBY.L','CSR.L','CPI.L','CCL.L','CGL.L','CINE.L','CCH.L','CPG.L','CRST.L','CRDA.L','DMGT.L','DCG.L','DTG.L','DLN.L','DGE.L','DNLM.L','ECM.L','ESNT.L','EXPN.L','FEVR.L','FRES.L','GVC.L','GWP.L','GFRD.L','GSK.L','GNK.L','HSBA.L','HLMA.L','HMSO.L','HL.L','HIK.L','HSX.L','IGG.L','IMI.L','ISF.L','IMB.L','INCH.L','ISAT.L','IHG.L','ICP.L','IAG.L','ITRK.L','ISYS.L','INVP.L','WG.L','JMAT.L','JUP.L','JE.L','KAZ.L','KWE.L','LRE.L','LAND.L','LSE.L','MDC.L','MCRO.L','MNDI.L','NXG.L','NXT.L','NG.L','NVA.L','PAYS.L','PSON.L','PNN.L','PSN.L','PHNX.L','PTEC.L','PLUS.L','POLY.L','PFG.L','PRU.L','RPC.L','RSA.L','RRS.L','RB.L','RDW.L','REL.L','REX.L','RIO.L','RR.L','RDSA.L','SAB.L','SKY.L','SSE.L','SDR.L','SVT.L','SHB.L','SHP.L','SN.L','SMIN.L','STJ.L','STAN.L','SYR.L','TATE.L','TCY.L','SGE.L','TPK.L','UBM.L','UTG.L','UDG.L','ULVR.L','UU.L','VED.L','CKSN.L','SMWH.L','ATK.L','WEIR.L','WTB.L','WIZZ.L','EZJ.L')

lst <- c('ECM.L','HSBA.L','WIZZ.L','KAZ.L','RIO.L','STAN.L','JUP.L','FEVR.L')
alst <- c('AMD','NVDA','GOOG','TSLA','AAPL','FB')

lst <- c(extlst)

for (n in lst) {
    try({
    print(paste('Getting',n))	
    obj <- getSymbols(n,src='google',env=NULL)
                                        # Check ADX
    adx <- last(ADX(obj['2017-01::'],maType='EMA'))$ADX[[1]]
    div <- MACD(obj[,4],12,26,9,maType='EMA')
    macd <- last(div)$macd[[1]]
    div <- div$macd-div$signal
    lastDiv <- last(div)[[1]]
    prelastDiv <- div[nrow(div)-1,][[1]]
    preprelastDiv <- div[nrow(div)-2,][[1]]
    #print(n)
    sma200 <- last(SMA(obj[,4],200))[[1]]
    lst <- last(obj[,4])[[1]]
    sma <-  last(SMA(obj[,4]))[[1]]
    ema <-  last(EMA(obj[,4],30))[[1]]
    #print(paste(n,'adx=',adx,'last=',lst,'sma200=',sma200,'macd=',macd,
#'lastDiv=',lastDiv,',prelast=',prelastDiv,'first=',preprelastDiv))
    print(paste('Testing',n))
    if (adx<30 && lst>sma200 && macd>0 && lastDiv>=prelastDiv && prelastDiv>preprelastDiv && preprelastDiv<0){
    # Update quotes
    print(paste('Plotting',n))
    try({
    q <- getQuote(n)
    d <- Sys.Date()
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
    q <- xts(q,d)
    #print(q)
                                        #obj <- merge(obj,q,by="Date")
    obj <- rbind(obj,q)
    obj <- obj[ ! duplicated( index(obj), fromLast = TRUE ),  ]
    })
    #print(tail(obj))
    # Chart
    # MACD could be (5,34,5) to see Elliot's wave
    chartSeries(obj,subset='last 9 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
                                        #chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo(),addATR(20)),multi.col=FALSE,name=n)
    # addBBonds()
        chartSeries(obj,subset='last 4 months',TA=c(addSMA(),addEMA(30),addMACD(),addSMA(200),addVo(),addADX()),multi.col=FALSE,name=n)
    invisible(readline(prompt="Press [enter] to continue"))
    }})}
