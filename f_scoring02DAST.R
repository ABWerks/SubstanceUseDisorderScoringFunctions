# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Substance Use Disorder Survey Analysis
# Function: Scoring the DAST
# ABWerks
# November 2021
#
# Dependency: Hmisc
# 
# Description:
# Function to score the Drug Abuse Screening Test (DAST)
# @article{skinner1982drug,
#   title={The drug abuse screening test},
#   author={Skinner, Harvey A},
#   journal={Addictive behaviors},
#   volume={7},
#   number={4},
#   pages={363--371},
#   year={1982},
#   publisher={Elsevier}
# }
#
# Arguments:
#   data: the survey. Assumes items are named: DAST_1, DAST_2, etc...
#
# Details:
#   DAST-10
#   Scoring and Interpretation – For the DAST-10, score 1 point for each question answered, “YES”, except for
#   question (3) for which a “NO” answer receives 1 point and (0) for a “YES”. Add up the points and
#   interpretations are as followed:
#   
# Items
#   1. Have you used drugs other than those required for medical reasons?
#   2. Do you abuse more than one drug at a time?
#   3. Are you always able to stop using drugs when you want to?
#   4. Have you had “blackouts” or “flashbacks” as a result of drug use?
#   5. Do you ever feel bad or guilty about your drug use?
#   6. Does your spouse (or parent) ever complain about your involvement with drugs?
#   7. Have you neglected your family because of your use of drugs?
#   8. Have you engaged in illegal activities in order to obtain drugs?
#   9. Have you ever experienced withdrawal symptoms (felt sick) when you stopped taking drugs?
#   10. Have you had medical problems as a result of your drug use (e.g., memory loss, hepatitis, convulsions, bleeding etc...)?.
# 
# Values:
#   scoreDASTTotal: Total Score for DAST
#   scoreDASTCAT:
#     DAST-10 Score	Degree of Problem	Suggested Action
#     0	No problems reported,	None at this time
#     1-2	Low level	Monitor, reassess at a later date
#     3-5	Moderate level,	Further investigation required
#     6-8	Substantial level,	Assessment required
#     9-10	Severe level,	Assessment required
#   DAST_*: All missing values converted to 0
#   DAST[1,...,10]: Factors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Table shortcut
f_tableNA <- function(...) 
  table(..., useNA = "ifany")

# Items And Questions
dsItems  <- read.csv("SUDItemsAndQuestions.csv")
dsItems <- dsItems[grepl("^(DAST)", dsItems$Item, ignore.case = T), ]

f_scoringDAST <- function(data){
  varsDAST <- paste('DAST', 1:10, sep = '_')
  
  # recode
  data[, varsDAST] <- sapply(
    data[, varsDAST]
    , function(x) ifelse(is.na(x), 0, x))
  # Total
  data$scoreDASTTotal <- rowSums(data[, varsDAST], na.rm = T)
  # DAST Categorical
  data$scoreDASTCAT <- cut(
    data$scoreDASTTotal, breaks = c(0, 1, 3, 6, 9, 100)
    , include.lowest = T, right = F
    , labels = c('0 No problems'
                 , '1-2 Low level'
                 , '3-5 Moderate level'
                 , '6-8 Substantial level'
                 , '9-10 Severe level')
    )
  
  cat('DAST Total \n')
  print(summary(data$scoreDASTTotal))
  cat('DAST Categorical \n')
  print(f_tableNA(data$scoreDASTCAT))
  print(f_tableNA(DASTTotal = data$scoreDASTTotal, DASTCat = data$scoreDASTCAT))

  foo <- data[, varsDAST]
  colnames(foo) <- gsub("_", "", colnames(foo))
  data  <- cbind(data, foo)

  data$DAST3 <- factor(data$DAST3, labels = c("0 Yes", "1 No"))
  factorLabels <- c("0 No", "1 Yes")
  for(i in c(1,2,4:10)){
    data[, paste0("DAST", i)] <- factor(
      data[, paste0("DAST", i)], labels = factorLabels
    )
  }
  for(i in 1:10){
    Hmisc::label(data[, paste0("DAST", i)]) <- dsItems$Question[dsItems$Item == paste0("DAST", i)]
    cat(paste0("DAST", i), "\n")
    print(f_tableNA(data[, paste0("DAST", i)]))
  }

  return(data)
}