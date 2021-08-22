#raw data addresses
#ICD <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/DIAGNOSES_ICD.csv")
#PUICHARTEVENTS<- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PU_chart_vivian_ids.csv")
#NOTEEVENTS <- read.csv("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/NOTEEVENTS.csv")

#load libraries
library(dplyr)
library(tidyverse)
library(stringr)
library(tableone)
#load cleaned data
load("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/PUIF.RData")
#create table one for PUIF, PUIFc, PUIFm:
#PUIF
#checked normality and nonnormal variables as below 
#"sertransfern","transfern","icun", "iculengthtot","LOStot", 
#"tosecondser", "tofirsttr", "tofirsticu", "distinctchartCG","age"

shapiro.test(PUIF$age[PUIF$ICDsitestagediffR==1])
shapiro.test(PUIF$age[PUIF$ICDsitestagediffR==0])
hist(PUIF$age[PUIF$ICDsitestagediffR==1], 
     border="light blue", 
     col="blue")
hist(PUIF$age[PUIF$ICDsitestagediffR==0], 
     border="light blue", 
     col="blue")

myVars<- c("ADMISSION_TYPER","ADMISSION_LOCATIONR","INSURANCER","DISCHARGE_LOCATIONR",
           "GENDER","LANGUAGER","RELIGIONR", "MARITAL_STATUSR", "ETHNICITYWR", 
           "ETHNICITYAAR", "ETHNICITYWAAR", "age", "EDtR","firstseqR","noted", "distinctchartCG",
           "sertransfern","transfern","icun", "iculengthtot","LOStot", 
           "tosecondser", "tofirsttr", "tofirsticu",
           "firstserR", "lastserR", "firsttrR", "lasttrR","firsticuR",
           "lasticuR")
catVars<- c("ADMISSION_TYPER","ADMISSION_LOCATIONR","INSURANCER","DISCHARGE_LOCATIONR",
            "GENDER","LANGUAGER","RELIGIONR", "MARITAL_STATUSR", "ETHNICITYWR", 
            "ETHNICITYAAR", "ETHNICITYWAAR","EDtR","firstseqR","noted",
            "firstserR", "lastserR", "firsttrR","lasttrR", "firsticuR",
             "lasticuR")
nnormal <- c("sertransfern","transfern","icun", "iculengthtot","LOStot", 
             "tosecondser", "tofirsttr", "tofirsticu", "distinctchartCG","age")
tab2 <- CreateTableOne(vars = myVars, strata = "ICDsitestagediffR" , data = PUIF, factorVars = catVars)
tab2Mat <- print(tab2, showAllLevels = TRUE,nonnormal = nnormal,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab2Mat, file = "ACITable1-2.csv")
table(PUIF$nicdsitestagediff,exclude=NULL)
table(PUIF$nicdsite,PUIF$nicdstage, exclude=NULL)
#PUIFc
tab4 <- CreateTableOne(vars = myVars, strata = "chartsitestagediffR" , data = PUIFc, factorVars = catVars)
tab4Mat <- print(tab4, showAllLevels = TRUE,nonnormal = nnormal,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab4Mat, file = "ACITable2-2.csv")
table(PUIFc$chartsitestagediff,exclude=NULL)
table(PUIFc$chartsitecount,PUIFc$chartstagecount, exclude=NULL)
#PUIFm
tab6 <- CreateTableOne(vars = myVars, strata = "ICDchartstagediffR" , data = PUIFm, factorVars = catVars)
tab6Mat <- print(tab6, showAllLevels = TRUE,nonnormal = nnormal,  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab6Mat, file = "ACITable3-2.csv")
save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/ACI.RData")
table(PUIFm$ICDchartstagediff,exclude=NULL)
table(PUIFm$nicdstage,PUIFm$chartstagecount, exclude=NULL)

save.image("C:/Users/wzhan61/OneDrive - Emory University/Documents/Postdoc/Research/Collabrations/Data center/Vicki's projects/NSF/MIMIC III/data analysis/PUI/ACI.RData")