#raw data addresses
#ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
#PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
#NOTEEVENTS <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/NOTEEVENTS.csv")

#load libraries
library(dplyr)
library(tidyverse)
library(stringr)

#load cleaned data
load("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUI.RData")
#import, restructure and merge features with PUI data
PT <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PATIENTS.csv")
PT<-PT[order(PT$SUBJECT_ID),]
PUI<-PUI[order(PUI$SUBJECT_ID),]
PUIPT<-data.table(PT, key="SUBJECT_ID")[
  data.table(PUI,key="SUBJECT_ID"),
  allow.cartesian=TRUE
]
AD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/ADMISSIONS.csv")
AD<-AD[order(AD$HADM_ID),]
PUIPTAD<-left_join(PUIPT,AD,by=c("HADM_ID","SUBJECT_ID"))

SER<-read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/SERVICES.csv")
SER<-SER[order(SER$HADM_ID,SER$TRANSFERTIME),]
SER$PREV_SERVICE[SER$PREV_SERVICE==""]<-"ADM"
SER%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firstser=first(CURR_SERVICE),lastser=last(CURR_SERVICE),
            firstsertime=first(TRANSFERTIME),secondsertime=nth(TRANSFERTIME,2),
            lastsertime=last(TRANSFERTIME), 
            sertransfern=length(TRANSFERTIME)-1)->SERR
PUIPTADSER<-left_join(x=PUIPTAD,y=SERR,by=c("HADM_ID","SUBJECT_ID"))
TR<-read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/TRANSFERS.csv")
TR<-TR[order(TR$HADM_ID,TR$INTIME),]
TR<-TR[TR$EVENTTYPE=="transfer"|TR$EVENTTYPE=="discharge"|TR$EVENTTYPE=="admit",]
TR<-TR[(TR$PREV_CAREUNIT!=TR$CURR_CAREUNIT|TR$PREV_WARDID!=TR$CURR_WARDID),]
TR%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firsttr=first(CURR_CAREUNIT),lasttr=last(PREV_CAREUNIT),
            firsttype=first(EVENTTYPE),lasttype=last(EVENTTYPE),
            firsttrtime=first(OUTTIME), lasttrtime=last(INTIME), 
            transfern=length(INTIME)-2)->TRR
TRR$firsttr[TRR$firsttype!="admit"]<-NA
TRR$lasttr[TRR$lasttype!="discharge"]<-NA
TRR$firsttrtime[TRR$firsttype!="admit"]<-NA
TRR$lasttrtime[TRR$lasttype!="discharge"]<-NA
TRR$transfern[TRR$firsttype!="admit"]<-NA
TRR$transfern[TRR$lasttype!="discharge"]<-NA
PUIPTADSERTR<-left_join(x=PUIPTADSER,y=TRR,by=c("HADM_ID","SUBJECT_ID"))
ICU<-read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/ICUSTAYS.csv")
ICU<-ICU[order(ICU$HADM_ID,ICU$INTIME),]
ICU%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firsticu=first(FIRST_CAREUNIT),lasticu=last(LAST_CAREUNIT),
            firsticuintime=first(INTIME), lasticuouttime=last(OUTTIME), 
            icun=length(ICUSTAY_ID), iculengthtot=sum(LOS))->ICUR
PUIF<-left_join(x=PUIPTADSERTR,y=ICUR,by=c("HADM_ID","SUBJECT_ID"))
#ICDs
ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
ICD<-ICD[is.na(ICD$SEQ_NUM)==FALSE,]
ICD<-ICD[order(ICD$SUBJECT_ID,ICD$HADM_ID,ICD$SEQ_NUM,ICD$ICD9_CODE),]
ICD%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firsticdseq=first(SEQ_NUM),firsticd=first(ICD9_CODE),
            secondicdseq=nth(SEQ_NUM,2),secondicd=nth(ICD9_CODE,2),
            thirdicdseq=nth(SEQ_NUM,3),thirdicd=nth(ICD9_CODE,3),
            fourthicdseq=nth(SEQ_NUM,4),fourthicd=nth(ICD9_CODE,4),
            fifthicdseq=nth(SEQ_NUM,5),fifthicd=nth(ICD9_CODE,5),
            lasticdseq=last(SEQ_NUM),lasticd=last(ICD9_CODE),
            nicdtot=n())->ICDR
PUIF<-left_join(x=PUIF,y=ICDR,by=c("HADM_ID","SUBJECT_ID"))
#caregiver data
CG<-read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/CAREGIVERS.csv")
CG<-CG[order(CG$CGID),]
CG$DES <-ifelse(CG$DESCRIPTION=="", CG$LABEL, CG$DESCRIPTION)
names(CG)[names(CG) == "CGID"] <- "cgid"
PUICHART<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
PUICHARTCG<-merge(PUICHART, CG, by="cgid")
PUICHARTCG%>%
  group_by(subject_id,hadm_id)%>%
  summarise(nchartCG=n(),
            distinctchartCG=n_distinct(cgid)
  )->PUICHARTCGR
names(PUICHARTCGR)[1] <- "SUBJECT_ID"
names(PUICHARTCGR)[2] <- "HADM_ID"
PUIF<-left_join(x=PUIF,y=PUICHARTCGR,by=c("HADM_ID","SUBJECT_ID"))
rm(AD,ICU,ICUR,PT,PUIPT,PUIPTAD,PUIPTADSER,PUIPTADSERTR,SER,SERR,TR,TRR,ICD,ICDR,
   CG,PUICHART,PUICHARTCG,PUICHARTCGR)
#save data
saveRDS(PUIF, file = "PUIF.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIF.RData")

#develop features
PUIF$ICDsitestagediffR<-ifelse(PUIF$nicdsitestagediff==0,0,1)
PUIF$ADMISSION_LOCATIONR[PUIF$ADMISSION_LOCATION=="EMERGENCY ROOM ADMIT"]<-0
PUIF$ADMISSION_LOCATIONR[PUIF$ADMISSION_LOCATION!="EMERGENCY ROOM ADMIT"]<-1
PUIF$ADMISSION_LOCATIONR[PUIF$ADMISSION_LOCATION=="** INFO NOT AVAILABLE **"]<-NA
PUIF$DISCHARGE_LOCATIONR[PUIF$DISCHARGE_LOCATION=="DEAD/EXPIRED"|
                           PUIF$DISCHARGE_LOCATION=="HOSPICE"|
                           PUIF$DISCHARGE_LOCATION=="HOSPICE-MEDICAL FACILITY"]<-"DEAD/HOSPICE"
PUIF$DISCHARGE_LOCATIONR[PUIF$DISCHARGE_LOCATION=="LONG TERM CARE HOSPITAL"]<-"LONGTERM CARE"
PUIF$DISCHARGE_LOCATIONR[PUIF$DISCHARGE_LOCATION=="ICF"|
                           PUIF$DISCHARGE_LOCATION=="OTHER FACILITY"|
                           PUIF$DISCHARGE_LOCATION=="REHAB/DISTINCT PART HOSP"|
                           PUIF$DISCHARGE_LOCATION=="SHORT TERM HOSPITAL"|
                           PUIF$DISCHARGE_LOCATION=="SNF"|
                           PUIF$DISCHARGE_LOCATION=="SNF-MEDICAID ONLY CERTIF"]<-"SHORTTERM CARE"
PUIF$DISCHARGE_LOCATIONR[PUIF$DISCHARGE_LOCATION=="HOME"|
                           PUIF$DISCHARGE_LOCATION=="HOME HEALTH CARE"|
                           PUIF$DISCHARGE_LOCATION=="HOME WITH HOME IV PROVIDR"|
                           PUIF$DISCHARGE_LOCATION=="LEFT AGAINST MEDICAL ADVI"]<-"HOME/LEFT AGST ADV"
PUIF$RELIGIONR[PUIF$RELIGION=="HEBREW"|
                 PUIF$RELIGION=="JEWISH"|
                 PUIF$RELIGION=="BUDDHIST"|
                 PUIF$RELIGION=="MUSLIM"|
                 PUIF$RELIGION=="HINDU"|
                 PUIF$RELIGION=="UNITARIAN-UNIVERSALIST"|
                 PUIF$RELIGION=="ROMANIAN EAST. ORTH"|
                 PUIF$RELIGION=="UNOBTAINABLE"|
                 PUIF$RELIGION=="NOT SPECIFIED"]<-"OTH"
PUIF$RELIGIONR[PUIF$RELIGION=="7TH DAY ADVENTIST"|
                 PUIF$RELIGION=="BAPTIST"|
                 PUIF$RELIGION=="CATHOLIC"|
                 PUIF$RELIGION=="CHRISTIAN SCIENTIST"|
                 PUIF$RELIGION=="EPISCOPALIAN"|
                 PUIF$RELIGION=="GREEK ORTHODOX"|
                 PUIF$RELIGION=="JEHOVAH'S WITNESS"|
                 PUIF$RELIGION=="LUTHERAN"|
                 PUIF$RELIGION=="METHODIST"|
                 PUIF$RELIGION=="PROTESTANT QUAKER"]<-"CHRISTIAN"
PUIF$RELIGIONR[PUIF$RELIGION=="UNOBTAINABLE"|
                 PUIF$RELIGION=="NOT SPECIFIED"]<-NA
PUIF$MARITAL_STATUSR[PUIF$MARITAL_STATUS=="DIVORCED"|
                       PUIF$MARITAL_STATUS=="SEPARATED"|
                       PUIF$MARITAL_STATUS=="SINGLE"|
                       PUIF$MARITAL_STATUS=="WIDOWED"]<-"UNMARRIED"
PUIF$MARITAL_STATUSR[PUIF$MARITAL_STATUS=="LIFE PARTNER"|
                       PUIF$MARITAL_STATUS=="MARRIED"]<-"MARRIED/SIG"
PUIF$ETHNICITYR<-PUIF$ETHNICITY
PUIF$ETHNICITYR[PUIF$ETHNICITY=="UNKNOWN/NOT SPECIFIED"|
                  PUIF$ETHNICITY=="UNABLE TO OBTAIN"]<-NA
PUIF$ETHNICITYWR<-ifelse(str_detect(PUIF$ETHNICITYR, regex("WHITE"))==TRUE,1,0)
PUIF$ETHNICITYAAR<-ifelse(str_detect(PUIF$ETHNICITYR, regex("BLACK"))==TRUE,1,0)
PUIF$ETHNICITYASR<-ifelse(str_detect(PUIF$ETHNICITYR, regex("ASIAN"))==TRUE,1,0)
PUIF$ETHNICITYHR<-ifelse(str_detect(PUIF$ETHNICITYR, regex("HISPANIC"))==TRUE,1,0)
PUIF$ETHNICITYWAAR[str_detect(PUIF$ETHNICITYR, regex("WHITE"))==TRUE]<-"WHITE"
PUIF$ETHNICITYWAAR[str_detect(PUIF$ETHNICITYR, regex("BLACK"))==TRUE]<-"BLACK"
PUIF$age<-year(PUIF$ADMITTIME)-year(PUIF$DOB)
PUIF$age[PUIF$age>90]<-90
PUIF$LOStot<-as.numeric(difftime(PUIF$DISCHTIME,PUIF$ADMITTIME,units="days"))
PUIF$tosecondser<-as.numeric(difftime(PUIF$secondsertime,PUIF$ADMITTIME,units="days"))
PUIF$tofirsttr<-as.numeric(difftime(as.Date(PUIF$firsttrtime),PUIF$ADMITTIME,units="days"))
PUIF$tofirsticu<-as.numeric(difftime(PUIF$firsticuintime,PUIF$ADMITTIME,units="days"))
PUIF$tofirsttr<-ifelse(PUIF$tofirsttr<0,0,PUIF$tofirsttr)
PUIF$tofirsticu<-ifelse(PUIF$tofirsticu<0,0,PUIF$tofirsticu)
PUIF$tosecondser<-ifelse(PUIF$tosecondser<0,0,PUIF$tosecondser)
PUIF$firstserR[str_detect(PUIF$firstser, regex("MED"))==TRUE]<-"MED"
PUIF$firstserR[str_detect(PUIF$firstser, regex("SURG"))==TRUE]<-"SURG"
PUIF$firstserR[PUIF$firstser=="ORTHO"]<-"SURG"
PUIF$firstserR[PUIF$firstser=="TRAUM"]<-"SURG"
PUIF$lastserR[str_detect(PUIF$lastser, regex("MED"))==TRUE]<-"MED"
PUIF$lastserR[str_detect(PUIF$lastser, regex("SURG"))==TRUE]<-"SURG"
PUIF$lastserR[PUIF$lastser=="ORTHO"]<-"SURG"
PUIF$lastserR[PUIF$lastser=="TRAUM"]<-"SURG"
PUIF$firsttrR[PUIF$firsttr=="MICU"]<-"MICU"
PUIF$firsttrR[PUIF$firsttr=="SICU"]<-"SICU"
PUIF$firsttrR[PUIF$firsttr=="TSICU"]<-"SICU"
PUIF$firsttrR[PUIF$firsttr=="CCU"]<-"MICU"
PUIF$firsttrR[PUIF$firsttr=="CSRU"]<-"SICU"
PUIF$lasttrR[PUIF$lasttr=="MICU"]<-"MICU"
PUIF$lasttrR[PUIF$lasttr=="SICU"]<-"SICU"
PUIF$lasttrR[PUIF$lasttr=="TSICU"]<-"SICU"
PUIF$lasttrR[PUIF$lasttr=="CCU"]<-"MICU"
PUIF$lasttrR[PUIF$lasttr=="CSRU"]<-"SICU"
PUIF$firsticuR[PUIF$firsticu=="MICU"]<-"MICU"
PUIF$firsticuR[PUIF$firsticu=="SICU"]<-"SICU"
PUIF$firsticuR[PUIF$firsticu=="TSICU"]<-"SICU"
PUIF$firsticuR[PUIF$firsticu=="CCU"]<-"MICU"
PUIF$firsticuR[PUIF$firsticu=="CSRU"]<-"SICU"
PUIF$lasrserR[str_detect(PUIF$lastser, regex("MED"))==TRUE]<-"MED"
PUIF$lastserR[str_detect(PUIF$lastser, regex("SURG"))==TRUE]<-"SURG"
PUIF$lastserR[PUIF$lastser=="ORTHO"]<-"SURG"
PUIF$lastserR[PUIF$lastser=="TRAUM"]<-"SURG"
PUIF$lastttrR[PUIF$lasttr=="MICU"]<-"MICU"
PUIF$lasttrR[PUIF$lasttr=="SICU"]<-"SICU"
PUIF$lastrR[PUIF$lasttr=="TSICU"]<-"SICU"
PUIF$lasticuR[PUIF$lasticu=="MICU"]<-"MICU"
PUIF$lasticuR[PUIF$lasticu=="SICU"]<-"SICU"
PUIF$lasticuR[PUIF$lasticu=="TSICU"]<-"SICU"
PUIF$lasticuR[PUIF$lasticu=="CCU"]<-"MICU"
PUIF$lasticuR[PUIF$lasticu=="CSRU"]<-"SICU"
PUIF$LANGUAGER<-ifelse(PUIF$LANGUAGE=="ENGL",1,0)
PUIF$noted<-ifelse(!is.na(PUIF$notePUI_yes),1,0)
PUIF$EDt<-as.numeric(difftime(as.Date(PUIF$EDOUTTIME),as.Date(PUIF$EDREGTIME),units="days"))
PUIF$EDtR[PUIF$EDt==0]<-"sameday ER discharge"
PUIF$EDtR[PUIF$EDt==1]<-"secondday ER discharge"
PUIF$firstseqR<-ifelse(PUIF$firstseq<8,"First PUI ICD seq<8",">=8")
PUIF$ADMISSION_TYPER<-ifelse(PUIF$ADMISSION_TYPE=="EMERGENCY","EMERGENCY","Others")
PUIF$INSURANCER<-ifelse(PUIF$INSURANCE=="Medicare","Medicare","Others")

#Carevue PUI site&stage n differences by features
PUIFc<-PUIF[(PUIF$carevue==1&is.na(PUIF$metavision)),]
PUIFc$chartsitecount[is.na(PUIFc$chartsitecount)]<-0
PUIFc$chartstagecount[is.na(PUIFc$chartstagecount)]<-0
PUIFc$chartsitestagediff<-PUIFc$chartsitecount-PUIFc$chartstagecount
table(PUIFc$chartsitestagediff)
PUIFc$chartsitestagediffR<-ifelse(PUIFc$chartsitestagediff==0,0,1)
#Metavision
#compare metavision PUI ICD&stage n differences by features
PUIFm<-PUIF[(!is.na(PUIF$metavision)&is.na(PUIF$carevue)&!is.na(PUIF$nicdstage)),]
table(PUIFm$nicdstage,PUIFm$chartstagecount, exclude=NULL)
PUIFm$ICDchartstagediff<-PUIFm$nicdstage-PUIFm$chartstagecount
table(PUIFm$ICDchartstagediff)
PUIFm$ICDchartstagediffR<-ifelse(PUIFm$ICDchartstagediff==0,0,1)

#save data
saveRDS(PUIF, file = "PUIF.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIF.RData")
#save data
saveRDS(PUIFc, file = "PUIFc.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIFc.RData")
#save data
saveRDS(PUIFm, file = "PUIFm.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIFm.RData")
