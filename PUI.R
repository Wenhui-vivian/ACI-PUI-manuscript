#raw data addresses
#ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
#PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
#NOTEEVENTS <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/NOTEEVENTS.csv")
#CPTEVENTS<-read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/CPTEVENTS.csv")


#load libraries
library(dplyr)
library(reshape2)
library(stringr)
library (VennDiagram)
library(tidyverse)
library(data.table)

#ICDs
#load data
ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
ICD<-ICD[!is.na(ICD$SEQ_NUM),]
ICD<-ICD[order(ICD$SUBJECT_ID,ICD$HADM_ID,ICD$SEQ_NUM,ICD$ICD9_CODE),]
PUIICD<- filter(ICD,ICD$ICD9_CODE=='70700'|ICD$ICD9_CODE=='70701'|
                  ICD$ICD9_CODE=='70702'|
                  ICD$ICD9_CODE=='70703'|ICD$ICD9_CODE=='70704'|
                  ICD$ICD9_CODE=='70705'|
                  ICD$ICD9_CODE=='70706'|ICD$ICD9_CODE=='70707'|
                  ICD$ICD9_CODE=='70709'|ICD$ICD9_CODE=='70720'|
                  ICD$ICD9_CODE=='70721'|
                  ICD$ICD9_CODE=='70722'|ICD$ICD9_CODE=='70723'|
                  ICD$ICD9_CODE=='70724'|
                  ICD$ICD9_CODE=='70725')
PUIICD$ICDSTAGE = ifelse(PUIICD$ICD9_CODE=='70720'|PUIICD$ICD9_CODE=='70721'|
                           PUIICD$ICD9_CODE=='70722'|
                         PUIICD$ICD9_CODE=='70723'|PUIICD$ICD9_CODE=='70724'|
                           PUIICD$ICD9_CODE=='70725', 1, 0)
PUIICD%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firstseq=first(SEQ_NUM),firstpuiicd=first(ICD9_CODE),
            lastseq=last(SEQ_NUM),lastpuiicd=last(ICD9_CODE),
            secondseq=nth(SEQ_NUM,2),secondpuiicd=nth(ICD9_CODE,2),
            thirdseq=nth(SEQ_NUM,3),thirdpuiicd=nth(ICD9_CODE,3),
            fourthseq=nth(SEQ_NUM,4),fourthpuiicd=nth(ICD9_CODE,4),
            fifthseq=nth(SEQ_NUM,5),fifthpuiicd=nth(ICD9_CODE,5), 
            sixthseq=nth(SEQ_NUM,6),sixthpuiicd=nth(ICD9_CODE,6),
            seventhseq=nth(SEQ_NUM,7),seventhpuiicd=nth(ICD9_CODE,7),
            nicdpui=n(),nicdstage=sum(ICDSTAGE), nicdsite=n()-sum(ICDSTAGE))->PUIICDR

PUIICDR$nicdsitestagediff<-PUIICDR$nicdsite-PUIICDR$nicdstage
PUIICDR$ICD_yes<-1
names(PUIICDR)[1] <- "SUBJECT_IDicd"
rm(ICD,PUIICD)
table(PUIICDR$nicdstage,PUIICDR$nicdsite, exclude=NULL)
table(PUIICDR$nicdsitestagediff, exclude=NULL)
table(PUIICDR$lastseq,PUIICDR$nicdsitestagediff)

#Chart events
PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
PUICHARTEVENTS<- PUICHARTEVENTS[order(PUICHARTEVENTS$subject_id,
                                      PUICHARTEVENTS$hadm_id, 
                                      PUICHARTEVENTS$itemid,
                                      PUICHARTEVENTS$charttime,
                                      PUICHARTEVENTS$cgid),]
PUICHARTEVENTS$value<-ifelse(PUICHARTEVENTS$value=="",
                             as.character(PUICHARTEVENTS$valuenum),
                             PUICHARTEVENTS$value)
PUICHARTEVENTS<-PUICHARTEVENTS[is.na(PUICHARTEVENTS$value)==FALSE,]
#PUICHARTEVENTS<-PUICHARTEVENTS[(#PUICHARTEVENTS$value!="Not applicable"|
                                  #PUICHARTEVENTS$value!="Not Applicable"|
                                  #PUICHARTEVENTS$value!="Other/Remarks"|
                                  #PUICHARTEVENTS$value!="Unable to assess; dressing not removed"|
                                  #PUICHARTEVENTS$value!="Unable to Stage"|
                                  #PUICHARTEVENTS$value!="Unchanged"|
                                  #PUICHARTEVENTS$value!="Yes"|
                                  #PUICHARTEVENTS$value!="0"),]
PUICHARTEVENTS%>%
  group_by(subject_id,hadm_id,itemid)%>%
  summarise(firstcharttime=first(charttime),lastcharttime=last(charttime))->PUICHART
PUICHART$carevue<- ifelse(PUICHART$itemid<=8465,1,0)
PUICHART$metavision<- ifelse(PUICHART$itemid>8465,1,0)
PUICHART$risk<-ifelse(PUICHART$itemid==87,1,0)
PUICHART$chartstage<-ifelse(PUICHART$itemid>=551 & 
                               PUICHART$itemid<=553|
                               PUICHART$itemid== 224631|
                               PUICHART$itemid>=224965 & 
                               PUICHART$itemid<=224971|
                               PUICHART$itemid>=227618
                             & PUICHART$itemid<=227619,1,0)
PUICHART$chartsite<-ifelse(PUICHART$itemid>=576 & 
                            PUICHART$itemid<=577|
                            PUICHART$itemid== 554|
                            PUICHART$itemid>=228506 
                            & PUICHART$itemid<=228515,1,0)
PUICHART$depth<-ifelse(PUICHART$itemid>=555 & 
                               PUICHART$itemid<=557|
                               PUICHART$itemid>=228610 
                             & PUICHART$itemid<=228619,1,0)
PUICHART$width<-ifelse(PUICHART$itemid>=561 & 
                               PUICHART$itemid<=563|
                               PUICHART$itemid>=228620& 
                               PUICHART$itemid<=228629,1,0)
PUICHART$treatment<-ifelse(PUICHART$itemid>=567
                                 & PUICHART$itemid<=569|
                                   PUICHART$itemid>=228539& 
                                   PUICHART$itemid<=228548,1,0)
PUICHART$drainage<-ifelse(PUICHART$itemid>=558& 
                                  PUICHART$itemid<=560,1,0)
PUICHART$cleansing<-ifelse(PUICHART$itemid>=564
                                 & PUICHART$itemid<=566,1,0)
PUICHART$woundbase<-ifelse(PUICHART$itemid>=570
                                 & PUICHART$itemid<=572,1,0)
PUICHART$odor<-ifelse(PUICHART$itemid>=573
                            & PUICHART$itemid<=575,1,0)
PUICHART$reduce<-ifelse(PUICHART$itemid==579,1,0)
PUICHART$heal<-ifelse(PUICHART$itemid>=8457& 
                              PUICHART$itemid<=8459,1,0)
PUICHART$amount<-ifelse(PUICHART$itemid>=8460& 
                                PUICHART$itemid<=8462,1,0)
PUICHART$length<-ifelse(PUICHART$itemid>=8463
                              & PUICHART$itemid<=8465,1,0)
PUICHART$chartPUI<-ifelse(PUICHART$itemid>=83&PUICHART$itemid<=88|
                       PUICHART$itemid==578|
                       PUICHART$itemid==579,0,1)
PUICHART<- PUICHART[order(PUICHART$subject_id,
                                      PUICHART$hadm_id, 
                                      PUICHART$firstcharttime),]
PUICHART%>%
  group_by(subject_id,hadm_id,carevue)%>%
  summarise(carevuecount=sum(carevue),
            carevuefirsttime=first(firstcharttime),
            carevuelasttime=last(lastcharttime))->PUICHART_carevue
PUICHART_carevue[(PUICHART_carevue$carevue==1),]->PUICHART_carevue
PUICHART%>%
  group_by(subject_id,hadm_id,metavision)%>%
  summarise(metavisioncount=sum(metavision),
            metavisionfirsttime=first(firstcharttime),
            metavisionlasttime=last(lastcharttime))->PUICHART_metavision
PUICHART_metavision[(PUICHART_metavision$metavision==1),]->PUICHART_metavision
PUICHART<- PUICHART[order(PUICHART$subject_id,
                                      PUICHART$hadm_id, 
                                      PUICHART$itemid,
                                      PUICHART$firstcharttime),]
PUICHART%>%
  group_by(subject_id,hadm_id,risk)%>%
  summarise(riskcount=sum(risk),
            riskfirsttime=first(firstcharttime),
            risklasttime=last(lastcharttime))->PUICHART_risk
PUICHART_risk[(PUICHART_risk$risk==1),]->PUICHART_risk
PUICHART%>%
  group_by(subject_id,hadm_id,chartstage)%>%
  summarise(chartstagecount=sum(chartstage),
            chartstagefirsttime=first(firstcharttime),
            chartstagelasttime=last(lastcharttime),
            maxstageitemid=max(itemid))->PUICHART_stage
PUICHART_stage[(PUICHART_stage$chartstage==1),]->PUICHART_stage
PUICHART%>%
  group_by(subject_id,hadm_id,chartsite)%>%
  summarise(chartsitecount=sum(chartsite),
            chartsitefirsttime=first(firstcharttime),
            chartsitelasttime=last(lastcharttime),
            maxsiteitemid=max(itemid))->PUICHART_site
PUICHART_site[(PUICHART_site$chartsite==1),]->PUICHART_site
PUICHART%>%
  group_by(subject_id,hadm_id,depth)%>%
  summarise(depthcount=sum(depth),
            depthfirsttime=first(firstcharttime),
            depthlasttime=last(lastcharttime))->PUICHART_depth
PUICHART_depth[(PUICHART_depth$depth==1),]->PUICHART_depth
PUICHART%>%
  group_by(subject_id,hadm_id,drainage)%>%
  summarise(drainagecount=sum(drainage),
            drainagefirsttime=first(firstcharttime),
            drainagelasttime=last(lastcharttime))->PUICHART_drainage
PUICHART_drainage[(PUICHART_drainage$drainage==1),]->PUICHART_drainage
PUICHART%>%
  group_by(subject_id,hadm_id,width)%>%
  summarise(widthcount=sum(width),
            widthfirsttime=first(firstcharttime),
            widthlasttime=last(lastcharttime))->PUICHART_width
PUICHART_width[(PUICHART_width$width==1),]->PUICHART_width
PUICHART%>%
  group_by(subject_id,hadm_id,cleansing)%>%
  summarise(cleansingcount=sum(cleansing),
            cleansingfirsttime=first(firstcharttime),
            cleansinglasttime=last(lastcharttime))->PUICHART_cleansing
PUICHART_cleansing[(PUICHART_cleansing$cleansing==1),]->PUICHART_cleansing
PUICHART%>%
  group_by(subject_id,hadm_id,treatment)%>%
  summarise(treatmentcount=sum(treatment),
            treatmentfirsttime=first(firstcharttime),
            treatmentlasttime=last(lastcharttime))->PUICHART_treatment
PUICHART_treatment[(PUICHART_treatment$treatment==1),]->PUICHART_treatment
PUICHART%>%
  group_by(subject_id,hadm_id,woundbase)%>%
  summarise(woundbasecount=sum(woundbase),
            woundbasefirsttime=first(firstcharttime),
            woundbaselasttime=last(lastcharttime))->PUICHART_woundbase
PUICHART_woundbase[(PUICHART_woundbase$woundbase==1),]->PUICHART_woundbase
PUICHART%>%
  group_by(subject_id,hadm_id,odor)%>%
  summarise(odorcount=sum(odor),
            odorfirsttime=first(firstcharttime),
            odorlasttime=last(lastcharttime))->PUICHART_odor
PUICHART_odor[(PUICHART_odor$odor==1),]->PUICHART_odor
PUICHART%>%
  group_by(subject_id,hadm_id,reduce)%>%
  summarise(reducecount=sum(reduce),
            reducefirsttime=first(firstcharttime),
            reducelasttime=last(lastcharttime))->PUICHART_reduce
PUICHART_reduce[(PUICHART_reduce$reduce==1),]->PUICHART_reduce
PUICHART%>%
  group_by(subject_id,hadm_id,heal)%>%
  summarise(healcount=sum(heal),
            healfirsttime=first(firstcharttime),
            heallasttime=last(lastcharttime))->PUICHART_heal
PUICHART_heal[(PUICHART_heal$heal==1),]->PUICHART_heal
PUICHART%>%
  group_by(subject_id,hadm_id,amount)%>%
  summarise(amountcount=sum(amount),
            amountfirsttime=first(firstcharttime),
            amountlasttime=last(lastcharttime))->PUICHART_amount
PUICHART_amount[(PUICHART_amount$amount==1),]->PUICHART_amount
PUICHART%>%
  group_by(subject_id,hadm_id,length)%>%
  summarise(lengthcount=sum(length),
            lengthfirsttime=first(firstcharttime),
            lengthlasttime=last(lastcharttime))->PUICHART_length
PUICHART_length[(PUICHART_length$length==1),]->PUICHART_length
PUICHART%>%
  group_by(subject_id,hadm_id,chartPUI)%>%
  summarise(chartPUIcount=sum(chartPUI),
            chartPUIfirsttime=first(firstcharttime),
            chartPUIlasttime=last(lastcharttime))->PUICHART_PUI
PUICHART_PUI[(PUICHART_PUI$chartPUI==1),]->PUICHART_PUI
PUICHARTR<-Reduce(function(...) merge(..., all=TRUE), 
list(PUICHART_carevue,PUICHART_metavision,
     PUICHART_risk,PUICHART_stage,
     PUICHART_site,PUICHART_depth,
     PUICHART_drainage,PUICHART_width,
     PUICHART_cleansing,PUICHART_treatment, 
     PUICHART_woundbase,PUICHART_odor,
     PUICHART_reduce,
     PUICHART_heal,PUICHART_amount,
     PUICHART_length,PUICHART_PUI))
names(PUICHARTR)[2] <- "HADM_ID"
names(PUICHARTR)[1] <- "SUBJECT_IDchart"
PUICHARTR$chartsitestagediff<-PUICHARTR$chartsitecount-PUICHARTR$chartstagecount
rm(PUICHARTEVENTS,PUICHART,PUICHART_carevue,PUICHART_metavision,
   PUICHART_risk,
   PUICHART_stage,PUICHART_site,
   PUICHART_depth,PUICHART_drainage,
   PUICHART_width,PUICHART_cleansing,
   PUICHART_treatment, PUICHART_woundbase,
   PUICHART_odor,
   PUICHART_reduce,PUICHART_heal,
   PUICHART_amount,PUICHART_length,PUICHART_PUI)
table(PUICHARTR$chartstagecount,PUICHARTR$chartsitecount, exclude=NULL)
table(PUICHARTR$chartsitestagediff, exclude=NULL)

#Note events
NOTEEVENTS <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/NOTEEVENTS.csv")
PUInotes<-NOTEEVENTS[str_detect(NOTEEVENTS$TEXT, regex('bed[ ]*sore|bed[ ]*ulcer|pressure[ ]*sore|pressure[ ]*ulcer|decub.*[ ]*ulcer|decub.*[ ]*sore[s]*', ignore_case = T)),]
PUInotes <- PUInotes[order(PUInotes$SUBJECT_ID,PUInotes$HADM_ID, PUInotes$CHARTDATE, PUInotes$CHARTTIME,PUInotes$CATEGORY),]
##negation checks: no/not/n't checked, only need to exclude no PUI 
PUInotes$no<-str_detect(PUInotes$TEXT, regex('no[ ]*bed[ ]*sore|no[ ]*bed[ ]*ulcer|no[ ]*pressure[ ]*sore|no[ ]*pressure[ ]*ulcer|no[ ]*decub.*[ ]*ulcer|no[ ]*decub.*[ ]*sore[s]*', ignore_case = T))
PUInotes<-PUInotes[(PUInotes$no==FALSE),]
PUInotes$PUIfirstdate<-"PUInotefirstdate"
PUInotes%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(PUIfirstnotedate=first(CHARTDATE))->PUInoteR
PUInoteR$notePUI_yes<-1
names(PUInoteR)[1] <- "SUBJECT_IDnote"
rm(PUInotes,NOTEEVENTS)

#merge three datasets
PUI<-Reduce(function(...) merge(..., all=TRUE), 
                               list(PUInoteR,PUIICDR,PUICHARTR))
PUI$SUBJECT_ID<-ifelse(is.na(PUI$SUBJECT_IDchart), PUI$SUBJECT_IDicd,PUI$SUBJECT_IDchart)
PUI$SUBJECT_ID<-ifelse(is.na(PUI$SUBJECT_ID), PUI$SUBJECT_IDnote,PUI$SUBJECT_ID)
table(PUI$chartstagecount,PUI$nicdstage, exclude=NULL)
table(PUI$nicdsite,PUI$chartsitecount, exclude=NULL)
saveRDS(PUI, file = "PUI.Rds")

#save data
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUI.RData")

#load PUI data
load("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUI.RData")