setwd("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files")

source("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/DataPrep887B.R")

# Create new dataframe so we don't overwrite old dataframe
recodedMaster <- swappedMaster

# Recoding values from the qualtrics questionnaires.

############################# 1. Exit Quesitonnaire is first ##################################

# compliance with baseline

recodedMaster$ComplianceBaseline_Y1N0 <- mapvalues(recodedMaster$ComplianceBaseline_Y1N0, from = c(2,1), to = c(1,0))

# compliance with test

recodedMaster$ComplianceTest_Y1N0 <- mapvalues(recodedMaster$ComplianceTest_Y1N0, from = c(2,1), to = c(1,0))

# Do you think procedure differed

recodedMaster$ProcedureDiffered_Y1N0 <- mapvalues(recodedMaster$ProcedureDiffered_Y1N0, from = c(2,1), to = c(0,1))

# Do you think the true dose differed?

recodedMaster$TrueDoseDiffered_Y1N0 <- mapvalues(recodedMaster$TrueDoseDiffered_Y1N0, from = c(2,1), to = c(0,1))

######################## 2. Demographics Questionnaire #######################################

# gender 

recodedMaster$genderM1F0 <- mapvalues(recodedMaster$genderM1F0, from = c(2,1), to = c(0,1))

recodedMaster$genderM1F0 <- factor(recodedMaster$genderM1F0, levels = c(0,1), labels = c("F", "M"), ordered = FALSE)

# employment

# mapFunct <- function (varNam, fromVal, toVal, ordered = FALSE) {
#   varNamX <- mapvalues(varNam, from = fromVal, to = toVal)
#   varNam <- varNamX
# }
# 
# mapFunct(recodedMaster$edLevel, c(1,2,3,4), c(0,1,2,3))

recodedMaster$edLevel <- factor(recodedMaster$edLevel, levels = c(1,2,3,4), labels = c("Primary", "Secondary", "Tertiary", "Other"), ordered = FALSE)


# Marital Status

recodedMaster$maritalStat <- factor(recodedMaster$maritalStat, levels = c(1,2,3,4,5,6), labels = c("Single", "inRelationship", "Married", "Divorced", "Widowed", "Other"), ordered = FALSE)

# language other than english spoken at home

recodedMaster$langOtherEng <- mapvalues(recodedMaster$langOtherEng, from = c(2,1), to = c(0,1))

recodedMaster$langOtherEng <- factor(recodedMaster$langOtherEng, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

recodedMaster$langOtherEng

# coffee every day

recodedMaster$coffeeEveryday <- mapvalues(recodedMaster$coffeeEveryday, from = c(1,2), to = c(1,0))

recodedMaster$coffeeEveryday <- factor(recodedMaster$coffeeEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# tea every day

recodedMaster$teaEveryday <- mapvalues(recodedMaster$teaEveryday, from = c(1,2), to = c(1,0))

recodedMaster$teaEveryday <- factor(recodedMaster$teaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# cola every day

recodedMaster$colaEveryday <- mapvalues(recodedMaster$colaEveryday, from = c(1,2), to = c(1,0))

recodedMaster$colaEveryday <- factor(recodedMaster$colaEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)

# energy drinks every day

recodedMaster$energyDrinksEveryday <- mapvalues(recodedMaster$energyDrinksEveryday, from = c(1,2), to = c(1,0))

recodedMaster$energyDrinksEveryday <- factor(recodedMaster$energyDrinksEveryday, levels = c(0,1), labels = c("No", "Yes"), ordered = FALSE)


############################## 3. Expectancy questionnaire ###################################

# what form of caffeine answers pertain to

recodedMaster$EQformOfCaffeine <- factor(recodedMaster$EQformOfCaffeine, levels = c(1,2,3,4,5,6), labels = c("coffee", "softDrink", "tea", "medication", "caffeineInGeneral", "Other"), ordered = FALSE)

# recoding items of EQ questionnaire

colsToRecodeExp <-  c("EQformOfCaffeine",
                   "EQformOfCaffeine_TEXT",
                   "EQ1_PicksMeUp",
                   "EQ2_BetterConvers",
                   "EQ3_HelpsAvoidEating",
                   "EQ4_CaffMakesStress",
                   "EQ5_CaffImprovesAthl",
                   "EQ6_CaffLessSleepy",
                   "EQ7_CaffSuppressHunger",
                   "EQ8_NoCaffMakesMiser",
                   "EQ9_CaffImproveMood",
                   "EQ10_NoCaffAnxious",
                   "EQ11_CaffMakesJittery",
                   "EQ12_CaffMakesWorkoutsBetter",
                   "EQ13_NoCaffMakesWithdrawal",
                   "EQ14_DontLikeCaffFeel",
                   "EQ15_NoCaffFeelSick",
                   "EQ16_CaffIncreaseMotiv",
                   "EQ17_CaffMoreConf",
                   "EQ18_CaffThrowsSleep",
                   "EQ19_CaffMakesNervous",
                   "EQ20_CaffMakesAlert",
                   "EQ21_CaffSmallMakesAnxious",
                   "EQ22_CaffImproveConc",
                   "EQ23_CaffMakesFriendly",
                   "EQ24_NoCaffNeedCaffDaily",
                   "EQ25_CaffMakesSweat",
                   "EQ26_CaffAllowsMealSkipping",
                   "EQ27_NoCaffMakesDesire",
                   "EQ28_CaffMakesDifficult",
                   "EQ29_CaffMakesIrritable",
                   "EQ30_CaffHelpsMeWork",
                   "EQ31_CaffMakesHappy",
                   "EQ32_NoCaffNoFunction",
                   "EQ33_CaffMakesIrregularHeartbeat",
                   "EQ34_OftenCraveCaff",
                   "EQ35_NoCaffTroubleStartingDay",
                   "EQ36_CaffUpsetsStomach",
                   "EQ37_TroubleGivingUpCaff",
                   "EQ38_CaffLateDisruptsSleep",
                   "EQ39_CaffHelpsControlWeight",
                   "EQ40_NoCaffMakesHeadache",
                   "EQ41_CaffImprovesAttention",
                   "EQ42_CaffMakesSociable",
                   "EQ43_CaffMakesExerciseLonger",
                   "EQ44_CaffHelpsMeGetThroughDay",
                   "EQ45_CaffMakesMoreEnergy",
                   "EQ46_CaffDecreaseAppetite",
                   "EQ47_CaffLateMakesInsomnia"                          
) 


for (col_name in colsToRecodeExp) { #for each column name in the vector 'cols_to_reverse_code' 
  recodedMaster[,  col_name] <- mapvalues( #apply all the arguments in the curly brackets to 
    # each. So performs map values function for all entries in each of the columns one at a time.
    recodedMaster[, col_name], 
    from=c(1, 2, 3, 4, 5, 6),
    to=c(5, 4, 3, 2, 1, 0)
  )
}



################ 4. CWSQ Questionnaire #################################


for (col_name in CWSQ887BNames_re) { # using the re-ordered column names for the CWSQ questions in the vector 'CWSQ887BNames_re' (created in DataPrep887B.R)
  recodedMaster[,  col_name] <- mapvalues( #apply all the arguments in the curly brackets to 
    # each. So performs map values function for all entries in each of the columns one at a time.
    recodedMaster[, col_name], 
    from=c(1, 2, 3, 4, 5),
    to=c(0, 1, 2, 3, 4)
  )
}


write.csv(recodedMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/recodedMaster.csv")











 





























