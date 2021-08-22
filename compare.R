#raw data addresses
#ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
#PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
#NOTEEVENTS <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/NOTEEVENTS.csv")

#load libraries
library(dplyr)
library(tidyverse)
library(stringr)

#To check ICD and chart consistencies
#create more detailed PUICHART stage and site  and merge with PUIF
load("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIF.RData")
PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
PUICHARTEVENTS<- PUICHARTEVENTS[order(PUICHARTEVENTS$subject_id,
                                      PUICHARTEVENTS$hadm_id, 
                                      PUICHARTEVENTS$charttime,
                                      PUICHARTEVENTS$itemid,
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
PUICHART<-PUICHARTEVENTS
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
PUICHARTstage<-PUICHART[(PUICHART$itemid>=551 & 
                           PUICHART$itemid<=553|
                           PUICHART$itemid== 224631|
                           PUICHART$itemid>=224965 & 
                           PUICHART$itemid<=224971|
                           PUICHART$itemid>=227618
                         & PUICHART$itemid<=227619),]
PUICHARTsite<-PUICHART[(PUICHART$itemid>=576 & 
                          PUICHART$itemid<=577|
                          PUICHART$itemid== 554|
                          PUICHART$itemid>=228506 
                        & PUICHART$itemid<=228515),]
PUICHARTstage%>%
  group_by(subject_id,hadm_id,itemid)%>%
  summarise(firstchartstagecharttime=first(charttime),
            lastchartstagecharttime=last(charttime),
            firstchartstagevalue=first(value),
            lastchartstagevalue=last(value),
            modechartstagevalue=calculate_mode(value),
            firstchartstagecg=first(cgid),
            lastchartstagecg=last(cgid),
            modechartstagecg=calculate_mode(cgid)
  )->PUICHARTstageR
PUICHARTstageR%>%
  group_by(subject_id,hadm_id)%>%
  summarise(nchartstageitem=n(),
            firstchartstageitemid=first(itemid),
            lastchartstageitemid=last(itemid),
            firstchartstagefirstcharttime=first(firstchartstagecharttime),
            lastchartstagelastcharttime=last(lastchartstagecharttime),
            firstchartstagefirstvalue=first(firstchartstagevalue),
            firstchartstagelastvalue=first(lastchartstagevalue),
            secondchartstagefirstvalue=nth(firstchartstagevalue,2),
            secondchartstagelastvalue=nth(lastchartstagevalue,2),
            lastchartstagefirstvalue=last(firstchartstagevalue),
            lastchartstagelastvalue=last(lastchartstagevalue),
            firstchartstagemodevalue=first(modechartstagevalue),
            secondchartstagemodevalue=nth(modechartstagevalue,2),
            lastchartstagemodevalue=last(modechartstagevalue),
            firstchartstagefirstcg=first(firstchartstagecg),
            lastchartstagelastcg=last(lastchartstagecg)
  )->PUICHARTstageHAR
PUICHARTstage%>%
  group_by(subject_id,hadm_id)%>%
  summarise(nchartstage=n(),
            distinctchartstagecharttime=n_distinct(charttime),
            distinctchartstagevalue=n_distinct(value),
            distinctchartstagecg=n_distinct(cgid)
  )->PUICHARTstageHARv
PUICHARTsite%>%
  group_by(subject_id,hadm_id,itemid)%>%
  summarise(firstchartsitecharttime=first(charttime),
            lastchartsitecharttime=last(charttime),
            firstchartsitevalue=first(value),
            lastchartsitevalue=last(value),
            modechartsitevalue=calculate_mode(value),
            firstchartsitecg=first(cgid),
            lastchartsitecg=last(cgid),
            modechartsitecg=calculate_mode(cgid)
  )->PUICHARTsiteR
PUICHARTsiteR%>%
  group_by(subject_id,hadm_id)%>%
  summarise(nchartsiteitem=n(),
            firstchartsiteitemid=first(itemid),
            lastchartsiteitemid=last(itemid),
            firstchartsitefirstcharttime=first(firstchartsitecharttime),
            lastchartsitelastcharttime=last(lastchartsitecharttime),
            firstchartsitefirstvalue=first(firstchartsitevalue),
            firstchartsitelastvalue=first(lastchartsitevalue),
            secondchartsitefirstvalue=nth(firstchartsitevalue,2),
            secondchartsitelastvalue=nth(lastchartsitevalue,2),
            lastchartsitefirstvalue=last(firstchartsitevalue),
            lastchartsitelastvalue=last(lastchartsitevalue),
            firstchartsitemodevalue=first(modechartsitevalue),
            secondchartsitemodevalue=nth(modechartsitevalue,2),
            lastchartsitemodevalue=last(modechartsitevalue),
            firstchartsitefirstcg=first(firstchartsitecg),
            lastchartsitelastcg=last(lastchartsitecg)
  )->PUICHARTsiteHAR
PUICHARTsite%>%
  group_by(subject_id,hadm_id)%>%
  summarise(nchartsite=n(),
            distinctchartsitecharttime=n_distinct(charttime),
            distinctchartsitevalue=n_distinct(value),
            distinctchartsitecg=n_distinct(cgid)
  )->PUICHARTsiteHARv
PUICHARTR2<-Reduce(function(...) merge(..., all=TRUE), 
                   list(PUICHARTstageHAR,PUICHARTstageHARv,
                        PUICHARTsiteHAR,PUICHARTsiteHARv))
names(PUICHARTR2)[2] <- "HADM_ID"
names(PUICHARTR2)[1] <- "SUBJECT_ID"
rm(PUICHARTEVENTS,PUICHART,
   PUICHARTstageHAR,PUICHARTstageHARv,PUICHARTstageR,PUICHARTstage,
   PUICHARTsiteHAR,PUICHARTsiteHARv,PUICHARTsiteR,PUICHARTsite)
PUIFA<-merge(x=PUIF,y=PUICHARTR2, by.x =c("SUBJECT_ID", "HADM_ID"),
             by.y = c("SUBJECT_ID", "HADM_ID"),all.x=TRUE)
#merge with more detailed ICDR
ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
ICD<-ICD[!is.na(ICD$SEQ_NUM),]
ICD<-ICD[order(ICD$SUBJECT_ID,ICD$HADM_ID,ICD$SEQ_NUM,ICD$ICD9_CODE),]
PUIICDsite<- filter(ICD,ICD$ICD9_CODE=='70700'|ICD$ICD9_CODE=='70701'|
                      ICD$ICD9_CODE=='70702'|
                      ICD$ICD9_CODE=='70703'|ICD$ICD9_CODE=='70704'|
                      ICD$ICD9_CODE=='70705'|
                      ICD$ICD9_CODE=='70706'|ICD$ICD9_CODE=='70707'|
                      ICD$ICD9_CODE=='70709')
PUIICDstage<- filter(ICD,ICD$ICD9_CODE=='70720'|
                       ICD$ICD9_CODE=='70721'|
                       ICD$ICD9_CODE=='70722'|ICD$ICD9_CODE=='70723'|
                       ICD$ICD9_CODE=='70724'|
                       ICD$ICD9_CODE=='70725')
PUIICDsite%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firstseqsite=first(SEQ_NUM),firstpuiicdsite=first(ICD9_CODE),
            lastseqsite=last(SEQ_NUM),lastpuiicdsite=last(ICD9_CODE),
            secondseqsite=nth(SEQ_NUM,2),secondpuiicdsite=nth(ICD9_CODE,2),
            thirdseqsite=nth(SEQ_NUM,3),thirdpuiicdsite=nth(ICD9_CODE,3),
            fourthseqsite=nth(SEQ_NUM,4),fourthpuiicdsite=nth(ICD9_CODE,4))->PUIICDsiteR
PUIICDstage%>%
  group_by(SUBJECT_ID,HADM_ID)%>%
  summarise(firstseqstage=first(SEQ_NUM),firstpuiicdstage=first(ICD9_CODE),
            lastseqstage=last(SEQ_NUM),lastpuiicdstage=last(ICD9_CODE),
            secondseqstage=nth(SEQ_NUM,2),secondpuiicdstage=nth(ICD9_CODE,2),
            thirdseqstage=nth(SEQ_NUM,3),thirdpuiicdstage=nth(ICD9_CODE,3))->PUIICDstageR
summary(PUIICDsiteR)
summary(PUIICDstageR)
PUIICDR2<-merge(x=PUIICDsiteR,y=PUIICDstageR, by.x =c("SUBJECT_ID", "HADM_ID"),
                by.y = c("SUBJECT_ID", "HADM_ID"), all=TRUE)
PUIFA2<-merge(x=PUIFA,y=PUIICDR2, by.x =c("SUBJECT_ID", "HADM_ID"),
              by.y = c("SUBJECT_ID", "HADM_ID"),all.x=TRUE)
rm(ICD,PUIICDsite,PUIICDstage, PUIFA)
saveRDS(PUIFA2, file = "PUIFA2.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIFA2.RData")
#compare N=1 PUI site/stage across ICD and chart
PUIFAsite<-PUIFA2[(PUIFA2$nicdsite==1&PUIFA2$chartsitecount==1),]
PUIFAstage<-PUIFA2[(PUIFA2$nicdstage==1&PUIFA2$chartstagecount==1),]

PUIFAsite$firstpuiicdchartsiteC<-ifelse(PUIFAsite$firstpuiicdsite==70701&
                                          PUIFAsite$firstchartsitemodevalue=="Elbow, Left"|
                                          PUIFAsite$firstpuiicdsite==70702&
                                          PUIFAsite$firstchartsitemodevalue=="Back, Upper"|
                                          PUIFAsite$firstpuiicdsite==70703&
                                          (PUIFAsite$firstchartsitemodevalue=="Back, Lower"|
                                             PUIFAsite$firstchartsitemodevalue=="Coccyx"|
                                             PUIFAsite$firstchartsitemodevalue=="Scrotom")|
                                          PUIFAsite$firstpuiicdsite==70704&
                                          (PUIFAsite$firstchartsitemodevalue=="Hip, Left"|
                                             PUIFAsite$firstchartsitemodevalue=="Hip, Right")|
                                          PUIFAsite$firstpuiicdsite==70705&
                                          (PUIFAsite$firstchartsitemodevalue=="Gluteal, Left"|
                                             PUIFAsite$firstchartsitemodevalue=="Gluteal, Right")|
                                          PUIFAsite$firstpuiicdsite==70706&
                                          (PUIFAsite$firstchartsitemodevalue=="Ankle, Left"|
                                             PUIFAsite$firstchartsitemodevalue=="Ankle, Right")|
                                          PUIFAsite$firstpuiicdsite==70707&
                                          (PUIFAsite$firstchartsitemodevalue=="Heel, Left"|
                                             PUIFAsite$firstchartsitemodevalue=="Heel, Right")|
                                          PUIFAsite$firstpuiicdsite==70709&
                                          (PUIFAsite$firstchartsitemodevalue=="Facial"|
                                             PUIFAsite$firstchartsitemodevalue=="Head"|
                                             PUIFAsite$firstchartsitemodevalue=="Shoulder, Right"),1,0)
table1<-table(PUIFAsite$firstpuiicdchartsiteC,PUIFAsite$firstpuiicdsite,PUIFAsite$firstchartsitemodevalue)
view(table1)
write.csv(table1, file = "sitecompare1.csv")
PUIFAstage$firstpuiicdchartstageC<-ifelse(PUIFAstage$firstpuiicdstage==70720&
                                            (PUIFAstage$firstchartstagemodevalue=="Deep tissue injury"|
                                               PUIFAstage$firstchartstagemodevalue=="Not applicable")|
                                            PUIFAstage$firstpuiicdstage==70721&
                                            PUIFAstage$firstchartstagemodevalue=="Red; unbroken"|
                                            PUIFAstage$firstpuiicdstage==70722&
                                            (PUIFAstage$firstchartstagemodevalue=="Part. Thickness"|
                                               PUIFAstage$firstchartstagemodevalue=="Partial thickness skin loss through epidermis and/or dermis; ulcer may present as an abrasion, blister, or shallow crater")|
                                            PUIFAstage$firstpuiicdstage==70723&
                                            PUIFAstage$firstchartstagemodevalue=="Full Thickness"|
                                            PUIFAstage$firstpuiicdstage==70724&
                                            (PUIFAstage$firstchartstagemodevalue=="Full thickness skin loss that may extend down to underlying fascia; ulcer may have tunneling or undermining"|
                                               PUIFAstage$firstchartstagemodevalue=="Full thickness skin loss with damage to muscle, bone, or supporting structures; tunneling or undermining may be present")|
                                            PUIFAstage$firstpuiicdstage==70725&
                                            (PUIFAstage$firstchartstagemodevalue=="Unable to assess; dressing not removed"|
                                               PUIFAstage$firstchartstagemodevalue=="Unable to stage; wound is covered with eschar"),
                                          1,0)
table2<-table(PUIFAstage$firstpuiicdchartstageC,PUIFAstage$firstpuiicdstage,PUIFAstage$firstchartstagemodevalue)
write.csv(table2, file = "stagecompare1.csv")
saveRDS(PUIFAsite, file = "PUIFAsite.Rds")
saveRDS(PUIFAstage, file = "PUIFAstage.Rds")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIFAsitestage.RData")
