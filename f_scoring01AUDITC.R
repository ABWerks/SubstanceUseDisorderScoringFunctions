# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Substance Use Disorders Survey Analysis
# Function: Scoring the AUDITC
# ABWerks
# November 2021
# 
# Dependency: Hmisc
#
# Description:
# Function to score the Alcohol Use Disorders Identification Test (AUDITC)
#  https://cde.nlm.nih.gov/formView?tinyId=myWNfJaZwe
# @article{saunders1993development,
#   title={Development of the alcohol use disorders identification test (AUDIT): WHO collaborative project on early detection of persons with harmful alcohol consumption-II},
#   author={Saunders, John B and Aasland, Olaf G and Babor, Thomas F and De la Fuente, Juan R and Grant, Marcus},
#   journal={Addiction},
#   volume={88},
#   number={6},
#   pages={791--804},
#   year={1993},
#   publisher={Wiley Online Library}
# }
# @article{bush1998audit,
#   title={The AUDIT alcohol consumption questions (AUDIT-C): an effective brief screening test for problem drinking},
#   author={Bush, Kristen and Kivlahan, Daniel R and McDonell, Mary B and Fihn, Stephan D and Bradley, Katharine A and Ambulatory Care Quality Improvement Project (ACQUIP and others},
#   journal={Archives of internal medicine},
#   volume={158},
#   number={16},
#   pages={1789--1795},
#   year={1998},
#   publisher={American Medical Association}
# }
# Arguments:
#   data: the survey. Assumes AUDITC items are named: AUDITC_1, AUDITC_2, etc... and the survey has an item asking about Sex/Gender
#
# Details:
# AUDITC
# Questions 1-8 are scored on a 0-4 scale
# Questions 9-10 are scored 0, 2, or 4
# A score of 8 or more is associated with harmful or hazardous drinking. 
# A score of 13 or more in women, and 15 or more in men, is likely to indicate alcohol dependence.
# (http://www.agencymeddirectors.wa.gov/Files/aas.pdf) 
#
# Items
# 1.  How often do you have a drink containing alcohol? If "Never" skip items 2-10.
# 2.  How many drinks containing alcohol do you have on a typical day when you are drinking?
# 3.  How often do you have six or more drinks on one occasion?
# 4.  How often during the last year have you found that you were not able to stop drinking once you had started?
# 5.  How often during the last year have you failed to do what was normally expected of you because of drinking?"
# 6.  How often during the last year have you needed a first drink in the morning to get yourself going after a heavy drinking session?
# 7.  How often during the last year have you had a feeling of guilt or remorse after drinking?
# 8.  How often during the last year have you been unable to remember what happened the night before because of your drinking?
# 9.  Have you or someone else been injured because of your drinking?
# 10. Has a relative, friend, doctor, or other health care worker been concerned about your drinking or suggested you cut down?
#
# Values:
#   scoreAUDITCTotal: Total Score for AUDITC
#   scoreAUDITCHazardousDrinking: if scoreAUDITCTotal > 7
#   scoreAUDITCAlcoholDependence: if (SEX == "F" (Female) & scoreAUDITCTotal > 12) | (SEX == "M" (Male) & scoreAUDITCTotal > 14)
#   AUDIT_*: All missing values converted to 0
#   AUDIT[1,...,10]: Factors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Table shortcut
f_tableNA <- function(...) 
  table(..., useNA = "ifany")

# Items And Questions
dsItems  <- read.csv("SUDItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(AUDITC)", dsItems$Item, ignore.case = T), ]

f_scoringAUDITC <- function(data){
  varsAUDITC <- paste('AUDITC', 1:10, sep = '_')
  
  # recode
  data[, varsAUDITC] <- sapply(
    data[, varsAUDITC]
    , function(x) ifelse(is.na(x), 0, x))
  # Total
  data$scoreAUDITCTotal <- rowSums(data[, varsAUDITC], na.rm = T)
  # AUDIT-C Hazardous Drinking
  data$scoreAUDITCHazardousDrinking <- ifelse(data$scoreAUDITCTotal > 7, 1, 0)
  # AUDIT-C Alcohol Dependence
  data$scoreAUDITCAlcoholDependence <- ifelse(
    (data$SEX == "F" & data$scoreAUDITCTotal > 12) |
      (data$SEX == "M" & data$scoreAUDITCTotal > 14)
    , 1, 0)
  
  cat('AUDIT-C Total \n')
  print(summary(data$scoreAUDITCTotal))
  cat('AUDIT-C Hazardous Drinking \n')
  print(f_tableNA(data$scoreAUDITCHazardousDrinking))
  print(f_tableNA(`AUDIT-C Total` = data$scoreAUDITCTotal
                  , `AUDIT-C Hazardous` = data$scoreAUDITCHazardousDrinking))
  cat('AUDIT-C Alcohol Dependence \n')
  print(f_tableNA(data$scoreAUDITCAlcoholDependence))
  print(f_tableNA(`AUDIT-C Total` = data$scoreAUDITCTotal
                  , `AUDIT-C Dependence` = data$scoreAUDITCAlcoholDependence))
  print(f_tableNA(`Alcohol Dependence` = data$scoreAUDITCAlcoholDependence
                  , Sex = data$SEX))
  
  foo <- data[, varsAUDITC]
  colnames(foo) <- gsub("_", "", colnames(foo))
  data  <- cbind(data, foo)

  data$AUDITC1 <- factor(data$AUDITC1
    , labels = c("0 Never", "1 Monthly or less", "2 2−4 times a month", "3 2−3 times a week", "4 4 or more times a week"))
  data$AUDITC2 <- factor(data$AUDITC2
    , labels = c("0 1 or 2", "1 3 or 4", "2 5 or 6", "3 7 to 9", "4 10 or more"))
  factorLabels <- c("0 Never", "1 Less than monthly", "2 Monthly", "3 Weekly", "4 Daily or almost daily")
  for(i in 3:8){
    data[, paste0("AUDITC", i)] <- factor(
      data[, paste0("AUDITC", i)]
      , labels = factorLabels[sort(unique(data[, paste0("AUDITC", i)]))+1]
    )
  }
  factorLabels <- c("0 No", "1 Yes, but not in the past year", "2 Yes, during the past year")
  for(i in 9:10){
    data[, paste0("AUDITC", i)] <- factor(
      data[, paste0("AUDITC", i)]
      , labels = factorLabels[sort(unique(data[, paste0("AUDITC", i)]))+1]
    )
  }
  for(i in 1:10){
    Hmisc::label(data[, paste0("AUDITC", i)]) <- dsItems$Question[dsItems$Item == paste0("AUDITC", i)]
    cat(paste0("AUDITC", i), "\n")
    print(f_tableNA(data[, paste0("AUDITC", i)]))
  }

  return(data)
}