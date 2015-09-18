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


# certain items in the
write.csv(recodedMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/recodedMaster.csv")









######################### Reverse coding 'positive' items in the CWSQ ####################### 

# create vector of columns to reverse code

# creates a sort of mini data frame where every element in one column is matched with every element in the other
colsToRev <- expand.grid(testDays = c(paste("B", 1:2, sep=""), paste("T", 1:9, sep="")),  
                             itemsToReverse = c("Q2selfconfident",
                                                "Q4alert",
                                                "Q6content",
                                                "Q12urgework",
                                                "Q15talkative",
                                                "Q18clearheaded",
                                                "Q19desiresoc",
                                                "Q20energetic"))


# this then pastes those two columns together.
colsToReverse <- paste(colsToRev$testDays, colsToRev$itemsToReverse, sep="")

colsToReverse# create a new dataframe to work on so original is preserved and so we can check
recodedRevMaster <- recodedMaster

colnames(recodedRevMaster)




# now we run through the columns we specified with a for loop, remapping values (reverse coding in this case)
for (columnN in colsToReverse) {
  recodedRevMaster[, columnN] <- mapvalues(recodedRevMaster[, columnN],
                                                  from = c(0, 1, 2, 3, 4),
                                                  to = c(4, 3, 2, 1, 0)
                                                 )
}

write.csv(recodedRevMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/recodedRevMaster.csv")





############################# Making New Factors ##################################


####### CaffEq #####################################

factoredMaster <- recodedRevMaster

EQWithdrawalItems <-  c("EQ8_NoCaffMakesMiser",
                   "EQ10_NoCaffAnxious",
                   "EQ13_NoCaffMakesWithdrawal",
                   "EQ15_NoCaffFeelSick",
                   "EQ24_NoCaffNeedCaffDaily",
                   "EQ27_NoCaffMakesDesire",
                   "EQ32_NoCaffNoFunction",
                   "EQ34_OftenCraveCaff",
                   "EQ35_NoCaffTroubleStartingDay",
                   "EQ37_TroubleGivingUpCaff",
                   "EQ40_NoCaffMakesHeadache",
                   "EQ44_CaffHelpsMeGetThroughDay"                        
) 


# create new variable which is the factor of the CaffEQ pertaining to withdrawal. Sum of scores on all withdrawal items
factoredMaster$EQFactWithdrawal <- rowSums(factoredMaster[, EQWithdrawalItems], na.rm=T)






# Making CWSQ factors


#factor vectors
DrowsyFatigued <- c("Q1drowsy", "Q3yawning",  "Q5tiredfatigued", "Q16sluggish")

DecreasedAlertDiffConc <- c("Q2selfconfident", "Q4alert", "Q6content", "Q7diffconc", "Q18clearheaded")

Mood <- c("Q8irritable", "Q10depressedmood", "Q11grouchy", "Q23discouraged")

DecreasedSocMotiv <- c("Q12urgework", "Q15talkative", "Q19desiresoc", "Q20energetic")

Nausea <- c("Q17upsetstomach", "Q21nauseavom", "Q24queasy", "Q25nauseaous", "Q26vomiting")

FluLike <- c("Q9heavyfeel", "Q13flulike", "Q22musclepain")

Headache <- c("Q14headache", "Q27headachey")

Acute <- c("Q28anxious", "Q29nervous", "Q30jittery")

Craving <- c("Q31cravingcoffee", "Q32cravingcaffeine")


# create list of these elements
factorList <- list(DrowsyFatigued,
                   DecreasedAlertDiffConc,
                   Mood,
                   DecreasedSocMotiv,
                   Nausea,
                   FluLike,
                   Headache,
                   Acute,
                   Craving)

# create factor scores for each day

###### B1

B1DrowsyFacNames <- paste("B1", DrowsyFatigued, sep="")
factoredMaster$B1DrowsyFac <- rowSums(factoredMaster[, B1DrowsyFacNames], na.rm=F)

B1DecreasedAlertDiffConcFacNames <- paste("B1", DecreasedAlertDiffConc, sep="")
factoredMaster$B1DecAlertFac <- rowSums(factoredMaster[, B1DecreasedAlertDiffConcFacNames], na.rm=F)

B1MoodFacNames <- paste("B1", Mood, sep="")
factoredMaster$B1MoodFac <- rowSums(factoredMaster[, B1MoodFacNames], na.rm=F)

B1DecreasedSocMotivFacNames <- paste("B1", DecreasedSocMotiv, sep="")
factoredMaster$B1DecreasedSocMotivFac <- rowSums(factoredMaster[, B1DecreasedSocMotivFacNames], na.rm=F)

B1NauseaFacNames <- paste("B1", Nausea, sep="")
factoredMaster$B1NauseaFac <- rowSums(factoredMaster[, B1NauseaFacNames], na.rm=F)

B1FluLikeFacNames <- paste("B1", FluLike, sep="")
factoredMaster$B1FluLikeFac <- rowSums(factoredMaster[, B1FluLikeFacNames], na.rm=F)

B1HeadacheFacNames <- paste("B1", Headache, sep="")
factoredMaster$B1HeadacheFac <- rowSums(factoredMaster[, B1HeadacheFacNames], na.rm=F)

B1AcuteFacNames <- paste("B1", Acute, sep="")
factoredMaster$B1AcuteFac <- rowSums(factoredMaster[, B1AcuteFacNames], na.rm=F)

B1CravingFacNames <- paste("B1", Craving, sep="")
factoredMaster$B1CravingFac <- rowSums(factoredMaster[, B1CravingFacNames], na.rm=F)

#### B2

B2DrowsyFacNames <- paste("B2", DrowsyFatigued, sep="")
factoredMaster$B2DrowsyFac <- rowSums(factoredMaster[, B2DrowsyFacNames], na.rm=F)

B2DecreasedAlertDiffConcFacNames <- paste("B2", DecreasedAlertDiffConc, sep="")
factoredMaster$B2DecAlertFac <- rowSums(factoredMaster[, B2DecreasedAlertDiffConcFacNames], na.rm=F)

B2MoodFacNames <- paste("B2", Mood, sep="")
factoredMaster$B2MoodFac <- rowSums(factoredMaster[, B2MoodFacNames], na.rm=F)

B2DecreasedSocMotivFacNames <- paste("B2", DecreasedSocMotiv, sep="")
factoredMaster$B2DecreasedSocMotivFac <- rowSums(factoredMaster[, B2DecreasedSocMotivFacNames], na.rm=F)

B2NauseaFacNames <- paste("B2", Nausea, sep="")
factoredMaster$B2NauseaFac <- rowSums(factoredMaster[, B2NauseaFacNames], na.rm=F)

B2FluLikeFacNames <- paste("B2", FluLike, sep="")
factoredMaster$B2FluLikeFac <- rowSums(factoredMaster[, B2FluLikeFacNames], na.rm=F)

B2HeadacheFacNames <- paste("B2", Headache, sep="")
factoredMaster$B2HeadacheFac <- rowSums(factoredMaster[, B2HeadacheFacNames], na.rm=F)

B2AcuteFacNames <- paste("B2", Acute, sep="")
factoredMaster$B2AcuteFac <- rowSums(factoredMaster[, B2AcuteFacNames], na.rm=F)

B2CravingFacNames <- paste("B2", Craving, sep="")
factoredMaster$B2CravingFac <- rowSums(factoredMaster[, B2CravingFacNames], na.rm=F)

### T1

T1DrowsyFacNames <- paste("T1", DrowsyFatigued, sep="")
factoredMaster$T1DrowsyFac <- rowSums(factoredMaster[, T1DrowsyFacNames], na.rm=F)

T1DecreasedAlertDiffConcFacNames <- paste("T1", DecreasedAlertDiffConc, sep="")
factoredMaster$T1DecAlertFac <- rowSums(factoredMaster[, T1DecreasedAlertDiffConcFacNames], na.rm=F)

T1MoodFacNames <- paste("T1", Mood, sep="")
factoredMaster$T1MoodFac <- rowSums(factoredMaster[, T1MoodFacNames], na.rm=F)

T1DecreasedSocMotivFacNames <- paste("T1", DecreasedSocMotiv, sep="")
factoredMaster$T1DecreasedSocMotivFac <- rowSums(factoredMaster[, T1DecreasedSocMotivFacNames], na.rm=F)

T1NauseaFacNames <- paste("T1", Nausea, sep="")
factoredMaster$T1NauseaFac <- rowSums(factoredMaster[, T1NauseaFacNames], na.rm=F)

T1FluLikeFacNames <- paste("T1", FluLike, sep="")
factoredMaster$T1FluLikeFac <- rowSums(factoredMaster[, T1FluLikeFacNames], na.rm=F)

T1HeadacheFacNames <- paste("T1", Headache, sep="")
factoredMaster$T1HeadacheFac <- rowSums(factoredMaster[, T1HeadacheFacNames], na.rm=F)

T1AcuteFacNames <- paste("T1", Acute, sep="")
factoredMaster$T1AcuteFac <- rowSums(factoredMaster[, T1AcuteFacNames], na.rm=F)

T1CravingFacNames <- paste("T1", Craving, sep="")
factoredMaster$T1CravingFac <- rowSums(factoredMaster[, T1CravingFacNames], na.rm=F)

##### T2

T2DrowsyFacNames <- paste("T2", DrowsyFatigued, sep="")
factoredMaster$T2DrowsyFac <- rowSums(factoredMaster[, T2DrowsyFacNames], na.rm=F)

T2DecreasedAlertDiffConcFacNames <- paste("T2", DecreasedAlertDiffConc, sep="")
factoredMaster$T2DecAlertFac <- rowSums(factoredMaster[, T2DecreasedAlertDiffConcFacNames], na.rm=F)

T2MoodFacNames <- paste("T2", Mood, sep="")
factoredMaster$T2MoodFac <- rowSums(factoredMaster[, T2MoodFacNames], na.rm=F)

T2DecreasedSocMotivFacNames <- paste("T2", DecreasedSocMotiv, sep="")
factoredMaster$T2DecreasedSocMotivFac <- rowSums(factoredMaster[, T2DecreasedSocMotivFacNames], na.rm=F)

T2NauseaFacNames <- paste("T2", Nausea, sep="")
factoredMaster$T2NauseaFac <- rowSums(factoredMaster[, T2NauseaFacNames], na.rm=F)

T2FluLikeFacNames <- paste("T2", FluLike, sep="")
factoredMaster$T2FluLikeFac <- rowSums(factoredMaster[, T2FluLikeFacNames], na.rm=F)

T2HeadacheFacNames <- paste("T2", Headache, sep="")
factoredMaster$T2HeadacheFac <- rowSums(factoredMaster[, T2HeadacheFacNames], na.rm=F)

T2AcuteFacNames <- paste("T2", Acute, sep="")
factoredMaster$T2AcuteFac <- rowSums(factoredMaster[, T2AcuteFacNames], na.rm=F)

T2CravingFacNames <- paste("T2", Craving, sep="")
factoredMaster$T2CravingFac <- rowSums(factoredMaster[, T2CravingFacNames], na.rm=F)

###### T3

T3DrowsyFacNames <- paste("T3", DrowsyFatigued, sep="")
factoredMaster$T3DrowsyFac <- rowSums(factoredMaster[, T3DrowsyFacNames], na.rm=F)

T3DecreasedAlertDiffConcFacNames <- paste("T3", DecreasedAlertDiffConc, sep="")
factoredMaster$T3DecAlertFac <- rowSums(factoredMaster[, T3DecreasedAlertDiffConcFacNames], na.rm=F)

T3MoodFacNames <- paste("T3", Mood, sep="")
factoredMaster$T3MoodFac <- rowSums(factoredMaster[, T3MoodFacNames], na.rm=F)

T3DecreasedSocMotivFacNames <- paste("T3", DecreasedSocMotiv, sep="")
factoredMaster$T3DecreasedSocMotivFac <- rowSums(factoredMaster[, T3DecreasedSocMotivFacNames], na.rm=F)

T3NauseaFacNames <- paste("T3", Nausea, sep="")
factoredMaster$T3NauseaFac <- rowSums(factoredMaster[, T3NauseaFacNames], na.rm=F)

T3FluLikeFacNames <- paste("T3", FluLike, sep="")
factoredMaster$T3FluLikeFac <- rowSums(factoredMaster[, T3FluLikeFacNames], na.rm=F)

T3HeadacheFacNames <- paste("T3", Headache, sep="")
factoredMaster$T3HeadacheFac <- rowSums(factoredMaster[, T3HeadacheFacNames], na.rm=F)

T3AcuteFacNames <- paste("T3", Acute, sep="")
factoredMaster$T3AcuteFac <- rowSums(factoredMaster[, T3AcuteFacNames], na.rm=F)

T3CravingFacNames <- paste("T3", Craving, sep="")
factoredMaster$T3CravingFac <- rowSums(factoredMaster[, T3CravingFacNames], na.rm=F)

#### T4

T4DrowsyFacNames <- paste("T4", DrowsyFatigued, sep="")
factoredMaster$T4DrowsyFac <- rowSums(factoredMaster[, T4DrowsyFacNames], na.rm=F)

T4DecreasedAlertDiffConcFacNames <- paste("T4", DecreasedAlertDiffConc, sep="")
factoredMaster$T4DecAlertFac <- rowSums(factoredMaster[, T4DecreasedAlertDiffConcFacNames], na.rm=F)

T4MoodFacNames <- paste("T4", Mood, sep="")
factoredMaster$T4MoodFac <- rowSums(factoredMaster[, T4MoodFacNames], na.rm=F)

T4DecreasedSocMotivFacNames <- paste("T4", DecreasedSocMotiv, sep="")
factoredMaster$T4DecreasedSocMotivFac <- rowSums(factoredMaster[, T4DecreasedSocMotivFacNames], na.rm=F)

T4NauseaFacNames <- paste("T4", Nausea, sep="")
factoredMaster$T4NauseaFac <- rowSums(factoredMaster[, T4NauseaFacNames], na.rm=F)

T4FluLikeFacNames <- paste("T4", FluLike, sep="")
factoredMaster$T4FluLikeFac <- rowSums(factoredMaster[, T4FluLikeFacNames], na.rm=F)

T4HeadacheFacNames <- paste("T4", Headache, sep="")
factoredMaster$T4HeadacheFac <- rowSums(factoredMaster[, T4HeadacheFacNames], na.rm=F)

T4AcuteFacNames <- paste("T4", Acute, sep="")
factoredMaster$T4AcuteFac <- rowSums(factoredMaster[, T4AcuteFacNames], na.rm=F)

T4CravingFacNames <- paste("T4", Craving, sep="")
factoredMaster$T4CravingFac <- rowSums(factoredMaster[, T4CravingFacNames], na.rm=F)

#### T5

T5DrowsyFacNames <- paste("T5", DrowsyFatigued, sep="")
factoredMaster$T5DrowsyFac <- rowSums(factoredMaster[, T5DrowsyFacNames], na.rm=F)

T5DecreasedAlertDiffConcFacNames <- paste("T5", DecreasedAlertDiffConc, sep="")
factoredMaster$T5DecAlertFac <- rowSums(factoredMaster[, T5DecreasedAlertDiffConcFacNames], na.rm=F)

T5MoodFacNames <- paste("T5", Mood, sep="")
factoredMaster$T5MoodFac <- rowSums(factoredMaster[, T5MoodFacNames], na.rm=F)

T5DecreasedSocMotivFacNames <- paste("T5", DecreasedSocMotiv, sep="")
factoredMaster$T5DecreasedSocMotivFac <- rowSums(factoredMaster[, T5DecreasedSocMotivFacNames], na.rm=F)

T5NauseaFacNames <- paste("T5", Nausea, sep="")
factoredMaster$T5NauseaFac <- rowSums(factoredMaster[, T5NauseaFacNames], na.rm=F)

T5FluLikeFacNames <- paste("T5", FluLike, sep="")
factoredMaster$T5FluLikeFac <- rowSums(factoredMaster[, T5FluLikeFacNames], na.rm=F)

T5HeadacheFacNames <- paste("T5", Headache, sep="")
factoredMaster$T5HeadacheFac <- rowSums(factoredMaster[, T5HeadacheFacNames], na.rm=F)

T5AcuteFacNames <- paste("T5", Acute, sep="")
factoredMaster$T5AcuteFac <- rowSums(factoredMaster[, T5AcuteFacNames], na.rm=F)

T5CravingFacNames <- paste("T5", Craving, sep="")
factoredMaster$T5CravingFac <- rowSums(factoredMaster[, T5CravingFacNames], na.rm=F)

####### T6

T6DrowsyFacNames <- paste("T6", DrowsyFatigued, sep="")
factoredMaster$T6DrowsyFac <- rowSums(factoredMaster[, T6DrowsyFacNames], na.rm=F)

T6DecreasedAlertDiffConcFacNames <- paste("T6", DecreasedAlertDiffConc, sep="")
factoredMaster$T6DecAlertFac <- rowSums(factoredMaster[, T6DecreasedAlertDiffConcFacNames], na.rm=F)

T6MoodFacNames <- paste("T6", Mood, sep="")
factoredMaster$T6MoodFac <- rowSums(factoredMaster[, T6MoodFacNames], na.rm=F)

T6DecreasedSocMotivFacNames <- paste("T6", DecreasedSocMotiv, sep="")
factoredMaster$T6DecreasedSocMotivFac <- rowSums(factoredMaster[, T6DecreasedSocMotivFacNames], na.rm=F)

T6NauseaFacNames <- paste("T6", Nausea, sep="")
factoredMaster$T6NauseaFac <- rowSums(factoredMaster[, T6NauseaFacNames], na.rm=F)

T6FluLikeFacNames <- paste("T6", FluLike, sep="")
factoredMaster$T6FluLikeFac <- rowSums(factoredMaster[, T6FluLikeFacNames], na.rm=F)

T6HeadacheFacNames <- paste("T6", Headache, sep="")
factoredMaster$T6HeadacheFac <- rowSums(factoredMaster[, T6HeadacheFacNames], na.rm=F)

T6AcuteFacNames <- paste("T6", Acute, sep="")
factoredMaster$T6AcuteFac <- rowSums(factoredMaster[, T6AcuteFacNames], na.rm=F)

T6CravingFacNames <- paste("T6", Craving, sep="")
factoredMaster$T6CravingFac <- rowSums(factoredMaster[, T6CravingFacNames], na.rm=F)

##### T7

T7DrowsyFacNames <- paste("T7", DrowsyFatigued, sep="")
factoredMaster$T7DrowsyFac <- rowSums(factoredMaster[, T7DrowsyFacNames], na.rm=F)

T7DecreasedAlertDiffConcFacNames <- paste("T7", DecreasedAlertDiffConc, sep="")
factoredMaster$T7DecAlertFac <- rowSums(factoredMaster[, T7DecreasedAlertDiffConcFacNames], na.rm=F)

T7MoodFacNames <- paste("T7", Mood, sep="")
factoredMaster$T7MoodFac <- rowSums(factoredMaster[, T7MoodFacNames], na.rm=F)

T7DecreasedSocMotivFacNames <- paste("T7", DecreasedSocMotiv, sep="")
factoredMaster$T7DecreasedSocMotivFac <- rowSums(factoredMaster[, T7DecreasedSocMotivFacNames], na.rm=F)

T7NauseaFacNames <- paste("T7", Nausea, sep="")
factoredMaster$T7NauseaFac <- rowSums(factoredMaster[, T7NauseaFacNames], na.rm=F)

T7FluLikeFacNames <- paste("T7", FluLike, sep="")
factoredMaster$T7FluLikeFac <- rowSums(factoredMaster[, T7FluLikeFacNames], na.rm=F)

T7HeadacheFacNames <- paste("T7", Headache, sep="")
factoredMaster$T7HeadacheFac <- rowSums(factoredMaster[, T7HeadacheFacNames], na.rm=F)

T7AcuteFacNames <- paste("T7", Acute, sep="")
factoredMaster$T7AcuteFac <- rowSums(factoredMaster[, T7AcuteFacNames], na.rm=F)

T7CravingFacNames <- paste("T7", Craving, sep="")
factoredMaster$T7CravingFac <- rowSums(factoredMaster[, T7CravingFacNames], na.rm=F)

###### T8

T8DrowsyFacNames <- paste("T8", DrowsyFatigued, sep="")
factoredMaster$T8DrowsyFac <- rowSums(factoredMaster[, T8DrowsyFacNames], na.rm=F)

T8DecreasedAlertDiffConcFacNames <- paste("T8", DecreasedAlertDiffConc, sep="")
factoredMaster$T8DecAlertFac <- rowSums(factoredMaster[, T8DecreasedAlertDiffConcFacNames], na.rm=F)

T8MoodFacNames <- paste("T8", Mood, sep="")
factoredMaster$T8MoodFac <- rowSums(factoredMaster[, T8MoodFacNames], na.rm=F)

T8DecreasedSocMotivFacNames <- paste("T8", DecreasedSocMotiv, sep="")
factoredMaster$T8DecreasedSocMotivFac <- rowSums(factoredMaster[, T8DecreasedSocMotivFacNames], na.rm=F)

T8NauseaFacNames <- paste("T8", Nausea, sep="")
factoredMaster$T8NauseaFac <- rowSums(factoredMaster[, T8NauseaFacNames], na.rm=F)

T8FluLikeFacNames <- paste("T8", FluLike, sep="")
factoredMaster$T8FluLikeFac <- rowSums(factoredMaster[, T8FluLikeFacNames], na.rm=F)

T8HeadacheFacNames <- paste("T8", Headache, sep="")
factoredMaster$T8HeadacheFac <- rowSums(factoredMaster[, T8HeadacheFacNames], na.rm=F)

T8AcuteFacNames <- paste("T8", Acute, sep="")
factoredMaster$T8AcuteFac <- rowSums(factoredMaster[, T8AcuteFacNames], na.rm=F)

T8CravingFacNames <- paste("T8", Craving, sep="")
factoredMaster$T8CravingFac <- rowSums(factoredMaster[, T8CravingFacNames], na.rm=F)

####### T9

T9DrowsyFacNames <- paste("T9", DrowsyFatigued, sep="")
factoredMaster$T9DrowsyFac <- rowSums(factoredMaster[, T9DrowsyFacNames], na.rm=F)

T9DecreasedAlertDiffConcFacNames <- paste("T9", DecreasedAlertDiffConc, sep="")
factoredMaster$T9DecAlertFac <- rowSums(factoredMaster[, T9DecreasedAlertDiffConcFacNames], na.rm=F)

T9MoodFacNames <- paste("T9", Mood, sep="")
factoredMaster$T9MoodFac <- rowSums(factoredMaster[, T9MoodFacNames], na.rm=F)

T9DecreasedSocMotivFacNames <- paste("T9", DecreasedSocMotiv, sep="")
factoredMaster$T9DecreasedSocMotivFac <- rowSums(factoredMaster[, T9DecreasedSocMotivFacNames], na.rm=F)

T9NauseaFacNames <- paste("T9", Nausea, sep="")
factoredMaster$T9NauseaFac <- rowSums(factoredMaster[, T9NauseaFacNames], na.rm=F)

T9FluLikeFacNames <- paste("T9", FluLike, sep="")
factoredMaster$T9FluLikeFac <- rowSums(factoredMaster[, T9FluLikeFacNames], na.rm=F)

T9HeadacheFacNames <- paste("T9", Headache, sep="")
factoredMaster$T9HeadacheFac <- rowSums(factoredMaster[, T9HeadacheFacNames], na.rm=F)

T9AcuteFacNames <- paste("T9", Acute, sep="")
factoredMaster$T9AcuteFac <- rowSums(factoredMaster[, T9AcuteFacNames], na.rm=F)

T9CravingFacNames <- paste("T9", Craving, sep="")
factoredMaster$T9CravingFac <- rowSums(factoredMaster[, T9CravingFacNames], na.rm=F)


##### Total CWSQ Scores across each day

# removing the email element from the list of CWSQ items set up in 887BDataPrep
CWSQ887Nom <- CWSQ887Names[-33]

# create new day total CWSQ score variables

# B1 Total

B1Names <- paste("B1", CWSQ887Nom, sep="")
factoredMaster$B1Total <- rowSums(factoredMaster[, B1Names], na.rm = F) 

# B2 Total

B2Names <- paste("B2", CWSQ887Nom, sep="")
factoredMaster$B2Total <- rowSums(factoredMaster[, B2Names], na.rm = F) 

# T1 Total

T1Names <- paste("T1", CWSQ887Nom, sep="")
factoredMaster$T1Total <- rowSums(factoredMaster[, T1Names], na.rm = F) 

# T2 Total

T2Names <- paste("T2", CWSQ887Nom, sep="")
factoredMaster$T2Total <- rowSums(factoredMaster[, T2Names], na.rm = F) 

# T3 Total

T3Names <- paste("T3", CWSQ887Nom, sep="")
factoredMaster$T3Total <- rowSums(factoredMaster[, T3Names], na.rm = F) 

# T4 Total

T4Names <- paste("T4", CWSQ887Nom, sep="")
factoredMaster$T4Total <- rowSums(factoredMaster[, T4Names], na.rm = F) 

# T5 Total

T5Names <- paste("T5", CWSQ887Nom, sep="")
factoredMaster$T5Total <- rowSums(factoredMaster[, T5Names], na.rm = F) 

# T6 Total

T6Names <- paste("T6", CWSQ887Nom, sep="")
factoredMaster$T6Total <- rowSums(factoredMaster[, T6Names], na.rm = F) 

# T7 Total

T7Names <- paste("T7", CWSQ887Nom, sep="")
factoredMaster$T7Total <- rowSums(factoredMaster[, T7Names], na.rm = F) 

# T8 Total

T8Names <- paste("T8", CWSQ887Nom, sep="")
factoredMaster$T8Total <- rowSums(factoredMaster[, T8Names], na.rm = F) 

# T9 Total

T9Names <- paste("T9", CWSQ887Nom, sep="")
factoredMaster$T9Total <- rowSums(factoredMaster[, T9Names], na.rm = F) 

write.csv(factoredMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/factoredMaster.csv")