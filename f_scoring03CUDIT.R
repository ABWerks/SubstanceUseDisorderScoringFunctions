# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Substance Use Disorders Survey Analysis
# Function: Scoring the CUDIT
# ABWerks
# November 2021
# 
# Dependency: Hmisc
# 
# Description:
# Function to score the Cannabis Use Disorders Test-Revised (CUDIT-R)
# @article{adamson2010improved,
#   title={An improved brief measure of cannabis misuse: the Cannabis Use Disorders Identification Test-Revised (CUDIT-R)},
#   author={Adamson, Simon J and Kay-Lambkin, Frances J and Baker, Amanda L and Lewin, Terry J and Thornton, Louise and Kelly, Brian J and Sellman, J Douglas},
#   journal={Drug and alcohol dependence},
#   volume={110},
#   number={1-2},
#   pages={137--143},
#   year={2010},
#   publisher={Elsevier}
# }
#
# Arguments:
#   data: the survey. Assumes items are named: CUDIT_1, CUDIT_2, etc... 
#
# Details:
#   CUDIT
#   Questions 1-7 are scored on a 0-4 scale
#   Question 8 is score 0, 2, or 4
#   Scores of 8 or more indicate hazardous cannabis use. Scores of 12 or more indicate a possible cannabis use disorder.
# 
# Items:
#   1. How often do you use cannabis?
#   2. How many hours were you “stoned” on a typical day when you had been using cannabis?
#   3. How often during the past 6 months did you find that you were not able to stop using cannabis once you had started?
#   4. How often during the past 6 months did you fail to do what was usually expected from you because of using cannabis?
#   5. How often in the past 6 months have you devoted a great deal of your time to getting, using, or recovering from cannabis?
#   6. How often in the past 6 months have you had a problem with your memory or concentration after using cannabis?
#   7. How often do you use cannabis in situations that could be physically hazardous, such as driving, operating machinery, or caring for children?
#   8. Have you ever through about cutting down, or stopping, your use of cannabis?
#
# Values:
#   scoreCUDITTotal: Total Score for CUDIT
#   scoreCUDITHazardous: if scoreCUDITTotal > 7
#   scoreCUDITCannabisUseDisorder: if scoreCUDITTotal > 11 then 1 else 0
#   CUDIT*: All missing values converted to 0
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Table shortcut
f_tableNA <- function(...) 
  table(..., useNA = "ifany")

# Items And Questions
dsItems  <- read.csv("SUDItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(CUDIT)", dsItems$Item, ignore.case = T), ]

f_scoringCUDIT <- function(data){
  varsCUDIT <- paste('CUDIT', 1:8, sep = '_')
  
  # recode
  data[, varsCUDIT] <- sapply(
    data[, varsCUDIT]
    , function(x) ifelse(is.na(x), 0, x))
  # Total
  data$scoreCUDITTotal <- rowSums(data[, varsCUDIT], na.rm = T)
  # CUDIT Hazardous Drinking
  data$scoreCUDITHazardous <- ifelse(data$scoreCUDITTotal > 7, 1, 0)
  # CUDIT Alcohol Dependence
  data$scoreCUDITCannabisUseDisorder <- ifelse(data$scoreCUDITTotal > 11, 1, 0)
  
  cat('CUDIT Total \n')
  print(summary(data$scoreCUDITTotal))
  cat('CUDIT Hazardous \n')
  print(f_tableNA(data$scoreCUDITHazardous))
  print(f_tableNA(`CUDIT Total` = data$scoreCUDITTotal
                  , `CUDIT Hazardous` = data$scoreCUDITHazardous))
  cat('CUDIT CUD \n')
  print(f_tableNA(data$scoreCUDITCannabisUseDisorder))
  print(f_tableNA(`CUDIT Total` = data$scoreCUDITTotal
                  , `CUDIT CUD` = data$scoreCUDITCannabisUseDisorder))
  
  foo <- data[, varsCUDIT]
  colnames(foo) <- gsub("_", "", colnames(foo))
  data  <- cbind(data, foo)

  data$CUDIT8 <- factor(data$CUDIT8, labels = c("0 Never", "2 Yes, but not in the past 6 months", "4 Yes, during the past 6 months"))
  factorLabels <- c("0 Never", "1 Less than monthly", "2 Monthly", "3 Weekly", "4 Daily or almost daily")
  for(i in c(1:7)){
    data[, paste0("CUDIT", i)] <- factor(
      data[, paste0("CUDIT", i)]
      , labels = factorLabels[sort(unique(data[, paste0("CUDIT", i)]))+1]
    )
  }
  for(i in 1:10){
    Hmisc::label(data[, paste0("CUDIT", i)]) <- dsItems$Question[dsItems$Item == paste0("CUDIT", i)]
    cat(paste0("CUDIT", i), "\n")
    print(f_tableNA(data[, paste0("CUDIT", i)]))
  }

  return(data)

}