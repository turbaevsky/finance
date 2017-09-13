library(quantmod)
library(RCurl)
library(jsonlite)
library(telegram)

bot <- TGBot$new(token = '183104961:AAFOVTLmfQ0MDHdt2ZnLgtUZYkM_gbDFkLs')
bot$set_default_chat_id(181982455)

sp500 <- c('MMM','ABT','ABBV','ACN','ATVI','AYI','ADBE','AMD','AAP','AES','AET','AMG','AFL','A','APD','AKAM','ALK','ALB','ARE','ALXN','ALGN','ALLE','AGN','ADS','LNT','ALL','GOOGL','GOOG','MO','AMZN','AEE','AAL','AEP','AXP','AIG','AMT','AWK','AMP','ABC','AME','AMGN','APH','APC','ADI','ANDV','ANSS','ANTM','AON','AOS','APA','AIV','AAPL','AMAT','ADM','ARNC','AJG','AIZ','T','ADSK','ADP','AZO','AVB','AVY','BHGE','BLL','BAC','BK','BCR','BAX','BBT','BDX','BRK.B','BBY','BIIB','BLK','HRB','BA','BWA','BXP','BSX','BHF','BMY','AVGO','BF.B','CHRW','CA','COG','CPB','COF','CAH','CBOE','KMX','CCL','CAT','CBG','CBS','CELG','CNC','CNP','CTL','CERN','CF','SCHW','CHTR','CHK','CVX','CMG','CB','CHD','CI','XEC','CINF','CTAS','CSCO','C','CFG','CTXS','CLX','CME','CMS','COH','KO','CTSH','CL','CMCSA','CMA','CAG','CXO','COP','ED','STZ','COO','GLW','COST','COTY','CCI','CSRA','CSX','CMI','CVS','DHI','DHR','DRI','DVA','DE','DLPH','DAL','XRAY','DVN','DLR','DFS','DISCA','DISCK','DISH','DG','DLTR','D','DOV','DOW','DPS','DTE','DRE','DD','DUK','DXC','ETFC','EMN','ETN','EBAY','ECL','EIX','EW','EA','EMR','ETR','EVHC','EOG','EQT','EFX','EQIX','EQR','ESS','EL','ES','RE','EXC','EXPE','EXPD','ESRX','EXR','XOM','FFIV','FB','FAST','FRT','FDX','FIS','FITB','FE','FISV','FLIR','FLS','FLR','FMC','FL','F','FTV','FBHS','BEN','FCX','GPS','GRMN','IT','GD','GE','GGP','GIS','GM','GPC','GILD','GPN','GS','GT','GWW','HAL','HBI','HOG','HRS','HIG','HAS','HCA','HCP','HP','HSIC','HSY','HES','HPE','HLT','HOLX','HD','HON','HRL','HST','HPQ','HUM','HBAN','IDXX','INFO','ITW','ILMN','IR','INTC','ICE','IBM','INCY','IP','IPG','IFF','INTU','ISRG','IVZ','IRM','JEC','JBHT','SJM','JNJ','JCI','JPM','JNPR','KSU','K','KEY','KMB','KIM','KMI','KLAC','KSS','KHC','KR','LB','LLL','LH','LRCX','LEG','LEN','LVLT','LUK','LLY','LNC','LKQ','LMT','L','LOW','LYB','MTB','MAC','M','MRO','MPC','MAR','MMC','MLM','MAS','MA','MAT','MKC','MCD','MCK','MDT','MRK','MET','MTD','MGM','KORS','MCHP','MU','MSFT','MAA','MHK','TAP','MDLZ','MON','MNST','MCO','MS','MOS','MSI','MYL','NDAQ','NOV','NAVI','NTAP','NFLX','NWL','NFX','NEM','NWSA','NWS','NEE','NLSN','NKE','NI','NBL','JWN','NSC','NTRS','NOC','NRG','NUE','NVDA','ORLY','OXY','OMC','OKE','ORCL','PCAR','PKG','PH','PDCO','PAYX','PYPL','PNR','PBCT','PEP','PKI','PRGO','PFE','PCG','PM','PSX','PNW','PXD','PNC','RL','PPG','PPL','PX','PCLN','PFG','PG','PGR','PLD','PRU','PEG','PSA','PHM','PVH','QRVO','PWR','QCOM','DGX','RRC','RJF','RTN','O','RHT','REG','REGN','RF','RSG','RMD','RHI','ROK','COL','ROP','ROST','RCL','CRM','SCG','SLB','SNI','STX','SEE','SRE','SHW','SIG','SPG','SWKS','SLG','SNA','SO','LUV','SPGI','SWK','SPLS','SBUX','STT','SRCL','SYK','STI','SYMC','SYF','SNPS','SYY','TROW','TGT','TEL','FTI','TXN','TXT','TMO','TIF','TWX','TJX','TMK','TSS','TSCO','TDG','TRV','TRIP','FOXA','FOX','TSN','UDR','ULTA','USB','UA','UAA','UNP','UAL','UNH','UPS','URI','UTX','UHS','UNM','VFC','VLO','VAR','VTR','VRSN','VRSK','VZ','VRTX','VIAB','V','VNO','VMC','WMT','WBA','DIS','WM','WAT','WEC','WFC','HCN','WDC','WU','WRK','WY','WHR','WFM','WMB','WLTW','WYN','WYNN','XEL','XRX','XLNX','XL','XYL','YUM','ZBH','ZION','ZTS')
ftse250 <- c('3IN.L','AA.L','ASL.L','ACA.L','AGK.L','ALD.L','ATST.L','AMFW.L','ASCL.L','ASHM.L','AGR.L','AUTO.L','AVV.L','BME.L','BBY.L','BNKR.L','BAG.L','BBA.L','BEZ.L','BWY.L','BRSN.L','BKG.L','BGEO.L','BYG.L','BOY.L','BOK.L','BVS.L','BTEM.L','BRW.L','BVIC.L','BWNG.L','BTG.L','CNE.L','CLDN.L','CAPC.L','CPI.L','CARD.L','CLLN.L','CEY.L','CINE.L','CTY.L','CKN.L','CBG.L','CLI.L','COA.L','COB.L','CCC.L','CSP.L','CWK.L','CRST.L','CYBG.L','DJAN.L','DCG.L','DPH.L','DLN.L','DTY.L','DPLM.L','DC.L','DOM.L','DRX.L','DNLM.L','EDIN.L','ELTA.L','ECM.L','ELM.L','ETO.L','ESNT.L','ESUR.L','ERM.L','EVR.L','FCPT.L','FDM.L','FXPO.L','FEV.L','FCSS.L','FDSA.L','FGT.L','FGP.L','FSJ.L','FRCL.L','GFRD.L','GCP.L','GSS.L','GNS.L','GOG.L','GPOR.L','GFTU.L','GRI.L','UKW.L','GNC.L','GNK.L','GRG.L','GVC.L','HFD.L','HLMA.L','HSTN.L','HVPE.L','HSTG.L','HAS.L','HICL.L','HIK.L','HILS.L','HSX.L','HOC.L','HSV.L','HWDN.L','HTG.L','IBST.L','IGG.L','IMI.L','INCH.L','INDV.L','ISAT.L','ICP.L','INPP.L','INTU.L','INVP.L','IPO.L','IWG.L','JLT.L','JD.L','CHOO.L','JLG.L','JLIF.L','JII.L','JAM.L','JMG.L','JUP.L','JE.L','JUST.L','KAZ.L','KWE.L','KIE.L','LCL.L','LRE.L','LMP.L','EMG.L','MSLH.L','MARS.L','MCS.L','MGGT.L','MRO.L','MRC.L','MTRO.L','MLC.L','MAB.L','MTO.L','MONY.L','MNKS.L','MGAM.L','MYI.L','NEX.L','NBLS.L','NRR.L','NXG.L','NMC.L','NTG.L','NOG.L','OCDO.L','OSB.L','P2P.L','PAGE.L','PAG.L','PAY.L','PAYS.L','PNN.L','PLI.L','PSH.L','PNL.L','PDL.L','PFC.L','PETS.L','PHNX.L','PTEC.L','PCT.L','POLY.L','PLP.L','PZC.L','QQ.L','RNK.L','RAT.L','RDI.L','RDW.L','TRIG.L','RSW.L','RTN.L','RMV.L','RCP.L','RSE.L','ROR.L','RPC.L','SAFE.L','SAGA.L','SNN.L','SVS.L','SCIN.L','SNR.L','SRP.L','SHB.L','SHI.L','SXX.L','SMDS.L','SCT.L','SOPH.L','SXS.L','SPX.L','SPI.L','SPD.L','SSPG.L','SMP.L','SGC.L','STOB.L','SGP.L','SYNC.L','SYNT.L','TALK.L','TATE.L','TBCG.L','TED.L','TEP.L','TMPL.L','TEM.L','TCG.L','TCAP.L','TRY.L','TPK.L','BBOX.L','TLW.L','UBM.L','UDG.L','UKCM.L','ULE.L','UTG.L','VEC.L','VED.L','VSVS.L','VCT.L','VEIL.L','VM.L','WEIR.L','JDW.L','SMWH.L','WMH.L','WTAN.L','WIZZ.L','WG.L','WPCT.L','WKP.L','WWH.L','ZPG.L')
ftse350 <- c('III.L','3IN.L','AA.L','ASL.L','ACA.L','ADM.L','AGK.L','ALD.L','ATST.L','AMFW.L','AAL.L','ANTO.L','ASCL.L','ASHM.L','AHT.L','ABF.L','AGR.L','AZN.L','AUTO.L','AVV.L','AV.L','BME.L','BAB.L','BA.L','BBY.L','BNKR.L','BARC.L','BAG.L','BDEV.L','BBA.L','BEZ.L','BWY.L','BRSN.L','BKG.L','BGEO.L','BLT.L','BYG.L','BOY.L','BOK.L','BVS.L','BP.L','BRW.L','BATS.L','BTEM.L','BLND.L','BVIC.L','BT.A.L','BTG.L','BNZL.L','BRBY.L','CNE.L','CLDN.L','CPI.L','CAPC.L','CARD.L','CLLN.L','CCL.L','CEY.L','CNA.L','CINE.L','CTY.L','CKN.L','CBG.L','CLI.L','COA.L','COB.L','CCH.L','CPG.L','CCC.L','CTEC.L','CSP.L','CWK.L','CRST.L','CRH.L','CRDA.L','CYBG.L','DJAN.L','DCG.L','DCC.L','DPH.L','DLN.L','DGE.L','DTY.L','DPLM.L','DLG.L','DC.L','DOM.L','DRX.L','DNLM.L','EZJ.L','EDIN.L','ELTA.L','ECM.L','ELM.L','ETO.L','ESNT.L','ESUR.L','ERM.L','EVR.L','EXPN.L','FCPT.L','FDM.L','FERG.L','FXPO.L','FCSS.L','FEV.L','FDSA.L','FGT.L','FGP.L','FSJ.L','FRCL.L','FRES.L','GFS.L','GFRD.L','GCP.L','GSS.L','GNS.L','GKN.L','GSK.L','GLEN.L','GOG.L','GFTU.L','GRI.L','GPOR.L','UKW.L','GNC.L','GNK.L','GRG.L','GVC.L','HFD.L','HLMA.L','HMSO.L','HSTN.L','HVPE.L','HL.L','HSTG.L','HAS.L','HICL.L','HIK.L','HILS.L','HSX.L','HOC.L','HSV.L','HWDN.L','HSBA.L','HTG.L','IBST.L','IGG.L','IMI.L','IMB.L','INCH.L','INDV.L','INF.L','ISAT.L','IHG.L','ICP.L','IAG.L','INPP.L','ITRK.L','INTU.L','INVP.L','IPO.L','ITV.L','IWG.L','JLT.L','JD.L','JLG.L','JLIF.L','JMAT.L','JAM.L','JMG.L','JII.L','JUP.L','JE.L','JUST.L','KAZ.L','KWE.L','KIE.L','KGF.L','LCL.L','LRE.L','LAND.L','LGEN.L','LLOY.L','LSE.L','LMP.L','EMG.L','MKS.L','MSLH.L','MARS.L','MCS.L','MDC.L','MGGT.L','MRO.L','MRC.L','MERL.L','MTRO.L','MCRO.L','MLC.L','MAB.L','MTO.L','MNDI.L','MONY.L','MNKS.L','MGAM.L','MRW.L','MYI.L','NEX.L','NG.L','NBLS.L','NRR.L','NXG.L','NXT.L','NMC.L','NTG.L','NOG.L','OCDO.L','OML.L','OSB.L','P2P.L','PPB.L','PAGE.L','PAG.L','PAY.L','PAYS.L','PSON.L','PNN.L','PLI.L','PSH.L','PSN.L','PNL.L','PDL.L','PFC.L','PETS.L','PHNX.L','PTEC.L','PCT.L','POLY.L','PLP.L','PFG.L','PRU.L','PZC.L','QQ.L','RRS.L','RNK.L','RAT.L','RB.L','RDI.L','RDW.L','REL.L','RSW.L','RTO.L','RTN.L','RMV.L','RIO.L','RCP.L','RSE.L','RR.L','ROR.L','RBS.L','RDSA.L','RDSB.L','RMG.L','RPC.L','RSA.L','RYA.L','SAFE.L','SAGA.L','SGE.L','SBRY.L','SNN.L','SVS.L','SDR.L','SCIN.L','SMT.L','SGRO.L','SNR.L','SRP.L','SVT.L','SHB.L','SHP.L','SHI.L','SXX.L','SKY.L','SN.L','SMDS.L','SMIN.L','SKG.L','SCT.L','SOPH.L','SXS.L','SPX.L','SPI.L','SPD.L','SSE.L','SSPG.L','STJ.L','SMP.L','SGC.L','STAN.L','SLA.L','STOB.L','SGP.L','SYNC.L','SYNT.L','TALK.L','TATE.L','TW.L','TBCG.L','TED.L','TEP.L','TMPL.L','TEM.L','TSCO.L','TRIG.L','TCG.L','TCAP.L','TRY.L','TPK.L','BBOX.L','TUI.L','TLW.L','UBM.L','UDG.L','UKCM.L','ULE.L','ULVR.L','UTG.L','UU.L','VEC.L','VED.L','VSVS.L','VCT.L','VM.L','VOD.L','WEIR.L','JDW.L','SMWH.L','WTB.L','WMH.L','WTAN.L','WIZZ.L','WG.L','WPCT.L','WKP.L','WPG.L','WWH.L','WPP.L','ZPG.L')
ftas <- c('III.L','3IN.L','FOUR.L','AA.L','AAIF.L','AAS.L','ADIG.L','ABD.L','ANII.L','ASL.L','ACA.L','ACL.L','ADM.L','AGK.L','AEFS.L','ALD.L','ATST.L','ATT.L','ALM.L','AMFW.L','AAL.L','AEP.L','ANTO.L','AO.L','ARW.L','ATS.L','ASCL.L','ASHM.L','AHT.L','ABF.L','AGR.L','AZN.L','AUTO.L','AVV.L','AV.L','AVON.L','BME.L','BAB.L','BA.L','BGFD.L','BGS.L','BBY.L','BNKR.L','BARC.L','BEE.L','BAG.L','BDEV.L','BBH.L','BBA.L','BBGI.L','BEZ.L','BWY.L','BRSN.L','BKG.L','BGEO.L','BHGG.L','BHMG.L','BLT.L','BIFF.L','BYG.L','BIOG.L','BRFI.L','BRGE.L','BRLA.L','BRNA.L','BRSC.L','THRG.L','BRWM.L','BMY.L','BSIF.L','BOY.L','BOK.L','BVS.L','BP.L','BRW.L','BATS.L','BTEM.L','BLND.L','BVIC.L','BWNG.L','BUT.L','BT.A.L','BTG.L','BNZL.L','BRBY.L','CNE.L','CLDN.L','CMBN.L','CIU.L','CPI.L','CAPC.L','CAL.L','CGT.L','CAR.L','CARD.L','CLLN.L','CCL.L','CPR.L','CARR.L','CEY.L','CNA.L','CTR.L','CHG.L','CSN.L','CINE.L','CIR.L','CMHY.L','CTY.L','CLIG.L','CSH.L','CKN.L','CLG.L','CBG.L','CLI.L','CMCX.L','COA.L','COB.L','CCH.L','CMS.L','CPG.L','CCC.L','CNCT.L','CSRT.L','CTEC.L','COST.L','CSP.L','CWD.L','NCYF.L','CWK.L','CRST.L','CRH.L','CRDA.L','CREI.L','CCPG.L','CYBG.L','DJAN.L','DCG.L','DCC.L','DLAR.L','DEB.L','DPH.L','DLN.L','DVO.L','DFS.L','DGE.L','DIA.L','DTY.L','DPLM.L','DLG.L','DIVI.L','DC.L','DOM.L','DRX.L','DIG.L','DNDL.L','DNLM.L','EZJ.L','EGL.L','EFM.L','EDIN.L','EWI.L','EPIC.L','EIG.L','ELTA.L','ECM.L','ELM.L','ESP.L','ENQ.L','ETO.L','EPG.L','EQN.L','ESNT.L','ESUR.L','ERM.L','EUT.L','EVR.L','EXI.L','EXPN.L','FCI.L','FCPT.L','FCS.L','FPEO.L','FCRE.L','FDM.L','FENR.L','FERG.L','FXPO.L','FAS.L','FCSS.L','FEV.L','FJV.L','FSV.L','FDSA.L','FDL.L','FGT.L','FGP.L','FSJ.L','FRCL.L','FSFL.L','FORT.L','FOXT.L','FRES.L','FSTA.L','FCIF.L','FEET.L','GFS.L','GFRD.L','GAW.L','GABI.L','GCP.L','DIGS.L','GEMD.L','GSS.L','GNS.L','GHG.L','GKN.L','GSK.L','GLEN.L','GOG.L','GOCO.L','GDWN.L','GFTU.L','GRI.L','GPOR.L','UKW.L','GNC.L','GNK.L','GRG.L','GMS.L','GVC.L','HFD.L','HLMA.L','HMSO.L','HAN.L','HSD.L','HSTN.L','HVPE.L','HL.L','HSTG.L','HAS.L','HEAD.L','HLCL.L','HAST.L','HDIV.L','HEFT.L','HNE.L','HFEL.L','HHI.L','HINT.L','HSL.L','BOOT.L','HRI.L','HGT.L','HICL.L','HMSF.L','HIK.L','HILS.L','HFG.L','HSX.L','HOC.L','HRG.L','BOWL.L','HSV.L','HSW.L','HWDN.L','HSBA.L','HSS.L','HTG.L','HNT.L','IBST.L','ICGT.L','LBOW.L','IGG.L','IMG.L','IMI.L','IEM.L','IMB.L','INCH.L','IIT.L','INDV.L','INF.L','ISAT.L','IHG.L','ICP.L','IBT.L','IAG.L','IPF.L','INPP.L','IRV.L','ITRK.L','INTU.L','IAT.L','IVI.L','IPU.L','INVP.L','IPO.L','ITE.L','ITV.L','IWG.L','JLT.L','JD.L','CHOO.L','JLEN.L','JLG.L','JLIF.L','JMAT.L','JAM.L','JAI.L','JMC.L','JCH.L','JMG.L','JESC.L','JETG.L','JETI.L','JGCI.L','JPGI.L','JEMI.L','JII.L','JPS.L','JFJ.L','JMF.L','JRS.L','JMI.L','JUSC.L','JEO.L','JUP.L','JUS.L','JE.L','JUST.L','KNOS.L','KAZ.L','KCOM.L','KLR.L','KMR.L','KWE.L','KIT.L','KIE.L','KGF.L','LCL.L','LRD.L','LAM.L','LRE.L','LAND.L','LWDB.L','LGEN.L','LTI.L','LIO.L','LLOY.L','LSE.L','LMP.L','LMI.L','LOOK.L','LWB.L','LWI.L','LSL.L','LUCE.L','LXI.L','MPO.L','MAJE.L','EMG.L','MKS.L','MSLH.L','MARS.L','MCP.L','MNP.L','MCB.L','MCS.L','MCLS.L','MCKS.L','MER.L','MGP.L','MDC.L','MXF.L','MGGT.L','MRO.L','MNZS.L','MRC.L','MRCH.L','MERL.L','MTRO.L','MCRO.L','MLC.L','MAB.L','MTO.L','GLE.L','MNDI.L','MONY.L','MNKS.L','MTE.L','MTU.L','MGAM.L','MGNS.L','MRW.L','MOSB.L','MTC.L','MOTR.L','MUT.L','MYI.L','NANO.L','NEX.L','NG.L','NBLS.L','NBPE.L','NCC.L','NRR.L','NXG.L','NXT.L','NESF.L','NMC.L','NXR.L','NAIT.L','NAS.L','NTG.L','NOG.L','NVA.L','OCDO.L','OML.L','OTB.L','OSB.L','OPHR.L','OXB.L','OXIG.L','P2P.L','PAC.L','PHI.L','PPB.L','PAGE.L','PIN.L','PAG.L','PAY.L','PAYS.L','PSON.L','PDG.L','PNN.L','PLI.L','PSH.L','PSN.L','PNL.L','PDL.L','PFC.L','POG.L','PETS.L','PHNX.L','PSDL.L','PHTM.L','PCTN.L','PTEC.L','PCFT.L','PCGH.L','PCT.L','POLY.L','PLP.L','PRV.L','PFD.L','PMO.L','PHP.L','PFG.L','PRU.L','PRTC.L','PZC.L','QQ.L','RRS.L','RDL.L','RNK.L','RAT.L','RUS.L','RECI.L','RB.L','RDI.L','RDW.L','RGL.L','REL.L','RWI.L','RSW.L','RNO.L','RTO.L','RTN.L','RCDO.L','RMV.L','RIO.L','RCP.L','RSE.L','RWA.L','RR.L','ROR.L','RBS.L','RDSA.L','RDSB.L','RMG.L','RPC.L','RPS.L','RSA.L','RICA.L','SUS.L','SAFE.L','SAGA.L','SGE.L','SBRY.L','SNN.L','SVS.L','SDP.L','ATR.L','SERE.L','SCF.L','SJG.L','SOI.L','SREI.L','SDU.L','SCP.L','SDR.L','SCAM.L','SCIN.L','SMT.L','SST.L','SDL.L','STS.L','SGRO.L','SNR.L','SEQI.L','SRP.L','SERV.L','SFR.L','SVT.L','SHB.L','SHP.L','SHI.L','SXX.L','SKY.L','SN.L','SMDS.L','SMIN.L','SKG.L','SIA.L','SCT.L','SOPH.L','SXS.L','SDY.L','SPX.L','SPI.L','SPT.L','SPO.L','SPD.L','SQN.L','SQNX.L','SSE.L','SSPG.L','STJ.L','SMP.L','SGC.L','STAN.L','SLA.L','SLET.L','SLI.L','SLPE.L','SLS.L','SWEF.L','STHR.L','STOB.L','STCK.L','SEC.L','STVG.L','SGP.L','SYNC.L','SYNT.L','TALK.L','THRL.L','TRS.L','TATE.L','TW.L','TBCG.L','TED.L','TEP.L','TMPL.L','TEM.L','TSCO.L','GYM.L','TRIG.L','TCG.L','TPT.L','TOWN.L','TCAP.L','TRG.L','TRY.L','TPK.L','TET.L','TRI.L','TNI.L','BBOX.L','TIGT.L','TTG.L','TUI.L','TLW.L','TFIF.L','SMIF.L','TYMN.L','UAI.L','UBM.L','UDG.L','UKCM.L','ULE.L','ULVR.L','UTG.L','UU.L','UPGS.L','UEM.L','VIN.L','VEC.L','VED.L','VSVS.L','VCT.L','VEIL.L','VOF.L','VM.L','VOD.L','FAN.L','VP.L','VSL.L','WEIR.L','JDW.L','SMWH.L','WTB.L','WMH.L','WIN.L','WTAN.L','WPC.L','WIZZ.L','WG.L','WPCT.L','WKP.L','WPG.L','WWH.L','WPP.L','XAR.L','XAF.L','XPP.L','ZTF.L','ZPG.L')
#####################################################################
shortlst <- c('FXPO.L','KAZ.L','WIZZ.L')
excluded <- c('GNC.L','IWG.L')
lim <- c(323.2-7.8*2,795-40,0,0,0) # Stop-loss limit
stocks <- c(935,240,0,0,0,0) # No of stocks
tax <- c(25.55+10.5,10.5*2+9.59,0,0,0,0) # Taxes
long <- c(320.8,795,0,0,0,0) # Buying price
#lst <- shortlst
#quotes <- TRUE
#web <- TRUE
#lst <- sp500
                                        #lst <- ftse250
#####################################################################
### Functions ###
#####################################################################
addSSTO <- newTA(stoch,HLC,col=c(4,5,6),type=c('n','l','l'))
#####################################################################
gQuote <- function(n='FXPO.L'){ # get quotes from google (real-time)
    url <- paste('http://finance.google.com/finance/info?client=ig&q=',n,sep='')
    data <- getURL(url)
    json <- fromJSON(gsub('\n|[//]|[\\]','',data))
    return(json)
}
#####################################################################
updQuote <- function(obj,n,google=FALSE) #Update price table by actual quotes
{
    q <- getQuote(n)
    #print(q)
    #q <- na.fill(q)
    d <- as.Date(q[[1]])
    row.names(q)<- trunc(q[,"Trade Time"], units="days")
    q <- q[,c("Open","High","Low","Last","Volume")]
    names(q) <- c("Open","High","Low","Close","Volume")
################### Insert google quotes (gQuote) ###############
    if (google){
        cls <- as.numeric(gQuote(n)$l)
        q$Close <- cls
        if (q$High<cls) q$High <- cls
        if (q$Low>cls) q$Low <- cls
        }
##################################################
    q <- xts(q,d)
    #print(q)
    #print(tail(obj[,1:5]))
    obj <- rbind(obj[,1:5],q)
    obj <- obj[!duplicated(index(obj),fromLast = TRUE ),]
    #print(tail(obj))
    return(obj)
}
#####################################################################
dT <- function(d) {return(as.Date(d,'%d-%b-%y'))}
longDate <- function(d) {return(as.Date(as.character(d),'%b %d, %Y'))}

#' Data convertor from finantial time
toXTS <- function(data){
    d <- cbind(as.Date(apply(data[1],2,longDate)),data[2])
    return(xts(d[,-1], order.by=d[,1]))
}
#####################################################################
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
#####################################################################
analyse <- function(lst=ftas,quotes=TRUE,web=FALSE,change=5,adx=25,k=0.05,ssto=0.9,google=FALSE,src='google',telegram=FALSE,long=TRUE){
# change = price change for 3 weeks in percent, adx = ADX low limit, k = koeff. for MACD and SSTO crossing
# ssto - SSTO D% high limit
    for (n in lst) {
        if (!n %in% excluded) {
        try({
            print(paste('Getting',n,'from',src))
            obj <- getSymbols(n,src=src,env=NULL)
            obj <- na.omit(obj)

            if (quotes) obj <- na.omit(updQuote(obj,n,google=google))
            #print(tail(obj))
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
            st <- last(stoch(HLC(obj))) # low stochastic, %K, %D and lowD
                                        #print(paste(n,'div=',div[p-3][[1]],div[p-2][[1]],div[p-1][[1]],div[p][[1]],'MACD=',macd,'ADX=',adx,'avgVol=',avgV,'SMA200=',sma200,'Close=',cls))
            #print(st)
############################################################################
### IF module ###
############################################################################
            if ((n %in% shortlst) ||
                ((avgV>=1e5 && cls>sma200 && cls>=5
		        && adx>=adx && change15>change) &&
		        #(((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd) ||
		        ((1+k)*st[[2]]>=st[[3]] && (1-k)*st[[2]]<=st[[3]] && st[[2]]<=ssto)
                )){
############################################################################
                cond <- ''
                if (div[p-3][[1]]>div[p-2][[1]] && div[p-2][[1]]<div[p-1][[1]] && div[p-1][[1]]<div[p][[1]]) cond <- paste(cond,'two risen divs,')
                if (div[p-2][[1]]>div[p-1][[1]] && div[p-1][[1]]<div[p][[1]])
                    cond <- paste(cond,'hole divs,')
                if ((1+k)*lsig>=lmacd && (1-k)*lsig<=lmacd) cond <- paste(cond,'macd crossed signal,')
                if ((1+k)*st[[2]]>=st[[3]] && (1-k)*st[[2]]<=st[[3]]) cond <- paste(cond,'SSTO crossed')

                print(paste('Plotting',n,'because of',cond))
                msg <- paste('adx=',round(adx),'atr=',signif(atr,2),'rsi14(30long/70short)=',round(rsi),'change,% 1w/3w/3m=',signif(change5,2),'/',signif(change15,2),'/',signif(change50,2),',SSTO:fastD=',signif(st$fastD,3),'slowD=',signif(st$slowD,3))
                print(msg)
                if (web==TRUE)
                    browseURL(paste('https://finance.yahoo.com/calendar/earnings?day=',format(Sys.Date(),'%Y-%m-%d'),'&symbol=',n,sep=''))
                print(tail(obj[,1:5]))
                print(tail(merge(div,macd)))
                if (quotes) try(print(getOptionChain(n)))
                if (n %in% shortlst){
                    no <- which(shortlst == n)
                    balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
                    print(paste('Balance=',signif(balance,3)))
                }
                no <- which(shortlst == n)
# Chart ##
# MACD could be (5,34,5) to see Elliot's wave
                if (!telegram && long){
                chartSeries(obj,subset='last 9 months',TA=c(addSMA(200),addMACD(),addSMA(),addEMA(30),addVo()),multi.col=FALSE,name=n,log.scale=T)
                plot(addLines(h=lim[no],col='red'))
                invisible(readline(prompt="Press [enter] to continue"))
                }

                if (telegram) png('test.png')
                chartSeries(obj,subset='last 3 months',TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
                #plot(addTA(stoch(HLC(obj))),col=c('red','blue','green'))
                plot(addSSTO())
                plot(addLines(h=lim[no],col='red'))
                if (telegram) {
                    dev.off()
                    bot$sendPhoto('test.png', caption=paste(n,cond,msg))
                         }
                if (!telegram) invisible(readline(prompt="Press [enter] to continue"))
            }})}}}
##############################################################################
ticker <- function(name='FXPO.L',min=10,subset='last 3 months',src='google',
                   telegram=FALSE,google=FALSE,chart=FALSE){
    while(TRUE){
    for (n in name){
        obj <- getSymbols(n,src=src,env=NULL)
        obj <- na.omit(obj)
        q <- getQuote(n)
        obj <- updQuote(obj,n,google=google)
        st <- last(stoch(HLC(obj))) # low stochastic, %K, %D and lowD
                                        #print(getQuote(n))
        if (n %in% shortlst){
            no <- which(shortlst == n)
            cls <- q[[2]]
            tm <- q[[1]]
            balance <- stocks[no]/100*cls-tax[no]-stocks[no]/100*long[no]
            msg1 <- paste(n,format(Sys.time(),'%H:%M'),'Bal=',signif(balance,3),'Last=',cls,'Limit=',lim[no],'(',tm,')')
            if (google) msg <- paste(n,gQuote(n)$lt,gQuote(n)$l,'SSTO fastD=',signif(st$fastD,3),'slowD=',signif(st$slowD,3))
            if (telegram) bot$sendMessage(msg1)
            if (google) print(msg)
            else print(msg1)
        }
        if (chart){
            chartSeries(obj,subset=subset,TA=c(addSMA(),addEMA(30),addBBands(),addMACD(),addVo()),multi.col=FALSE,name=n,log.scale=T)
            plot(addLines(h=lim[no],col='red'))
            plot(addSSTO())
            }
        Sys.sleep(60/length(name)*min)
    }}
}
##############################################################################
### Funds charting using csv ###
##############################################################################
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
##############################################################################
dayTicker <- function(n='FXPO.L',min=5,imitate=FALSE,google=FALSE){
    f <- data.frame()
    while (TRUE){
        for (t in 1:min){
            if (google){
                j <- gQuote(name)
                dt <- strptime(j$lt,"%b %d, %I:%M%p")
                val <- as.numeric(j$l)
                        }
            else {
                j <- getQuote(n)
                dt <- j$Trade
                val <- j$Last
                  }
            if (imitate){
                dt <- Sys.time()
                val <- runif(1,200,400)
                }
            if (t==1) {
                price <- data.frame(dt,val,val,val,val)
                names(price) <- c('Date','Open','High','Low','Close')
                        }
            else if (t==min) price$Close <- val
            if (val>price$High) price$High <- val
            if (val<price$Low) price$Low <- val
            if (imitate) Sys.sleep(1)
            else Sys.sleep(60)
            }
        f <- rbind(f,price)
        #print(f)
        x <- xts(f[2:5],f$Date)
        print(nrow(x))
        if (nrow(x)>2 && nrow(x)<=10) chartSeries(x)
        else if (nrow(x)>10 && nrow(x)<=30) chartSeries(x,TA=c(addSMA()))
        else if (nrow(x)>30) chartSeries(x,TA=c(addSMA(),addEMA(30),addSSTO()))
    }
##############################################################################






}
