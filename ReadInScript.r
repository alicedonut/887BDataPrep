setwd("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files")

library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(data.table)


############################ Building data files for 887B ##################### 


##################### 1. Master file, taken from the xlsx master file. We want to turn it into an object we can attach.
master887B <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/Caff887B.csv")

# Remove the rows that have incomplete data (as defined by whether I have excluded them or not)
master887B <- master887B[!is.na(master887B$Exclude_Y1N0),]

head(master887B)


####@@@@@@@@@@@@@@@@@ 2. Demographics questionnaire

all_content_Demog <- readLines("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/Demographics_and_Caffeine_Use_Questionnaire_887B.csv") # this reads in the qualtrics file and converts it to an object

skip_second_Demog <- all_content_Demog[-2] # removes second line with unneeded qualtrics headings

demog887B <- read.csv(textConnection(skip_second_Demog), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

# this uses the rename function in the plyr package to rename the variables
demog887B <- rename(demog887B, c("Q1"="genderM1F0",
                                 "Q2"="age",
                                 "Q3_1"="emp_FT",
                                 "Q3_2"="emp_Casual",
                                 "Q3_3"="emp_Vol",
                                 "Q3_4"="emp_UnEmp",
                                 "Q3_5"="emp_Student",
                                 "Q3_6"="emp_Retired",
                                 "Q3_7"="emp_Other",
                                 "Q3_7_TEXT"="emp_OtherTxt",
                                 "Q4"="edLevel",
                                 "Q5"="maritalStat",
                                 "Q6"="ethnicity",
                                 "Q7"="langOtherEng",
                                 "Q7_TEXT"="langOtherEng_TEXT",
                                 "Q8"="engFirst",
                                 "Q9"="coffeeEveryday",
                                 "Q11"="numCupsCoffee",
                                 "Q15"="teaEveryday",
                                 "Q17"="numCupsTea",
                                 "Q20"="colaEveryday",
                                 "Q40"="numCola",
                                 "Q25"="typeCola",
                                 "Q26"="energyDrinksEveryday",
                                 "Q41"="typeEnergyDrinks",
                                 "Q28"="numEnergyDrinks",
                                 "Q31"="email"
                                 ))

# setnames(demog887B, c("Q1", "Q2", "Q3_1"), c("gender", "Age", "Emp_FT")) # uses setnames function from data.table package to reassign names in a similar way as rename function above but with slightly different syntax


colsToKeep <- c("genderM1F0", 
  "age",
  "emp_FT",
  "emp_Casual",
  "emp_Vol",
  "emp_UnEmp",
  "emp_Student",
  "emp_Retired",
  "emp_Other",
  "emp_OtherTxt",
  "edLevel",
  "maritalStat",
  "ethnicity",
  "langOtherEng",
  "langOtherEng_TEXT",
  "engFirst",
  "coffeeEveryday",
  "numCupsCoffee",
  "teaEveryday",
  "numCupsTea",
  "colaEveryday",
  "numCola",
  "typeCola",
  "energyDrinksEveryday",
  "typeEnergyDrinks",
  "numEnergyDrinks",
  "email"
)


# the command below selects those columns in demog887B which are included in the vector colsToKeep

demog887B <- demog887B[, which(names(demog887B) %in% colsToKeep)] #if you wanted to remove these columns you'd simply put a - in front of which

# Now we add a column where each entry is a substring of the email column. We will use this to merge with the master csv above.

demog887B$substr_ID <- substr(demog887B$email, 1, 4)


#unique(demog887B$substr_ID) to check if it worked.


# Now we can remove the email column because it already exists in the master spreadsheet and the Substr_ID column is now the reference column


demog887B <- demog887B[, -which(names(demog887B) %in% "email")]

# demog887B  # to check





#################### 3. Expectancy questionnaire

all_content_EQ <- readLines("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/CaffEQ_887B.csv") # this reads the content of the csv file as raw text without trying to parse it into columns. We have to keep it as text until we get rid of the second line, which contains the text of the qualtrics questions as text in each cell. R will see this second row as a string cell belonging under the column heading designated by the top row column headings. Because there is a string in each column R can't make the column numeric and read it properly. qualtrics generates the cell names for the top line automatically. It is these we will keep as they don't have spaces, which R does not like as column names. Once we get rid of this second row of column names the numeric columns just contain numbers and we can parse it properly into columns.

skip_second_EQ <- all_content_EQ[-2] # removes second line with unneeded qualtrics headings

EQ887B <- read.csv(textConnection(skip_second_EQ), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

colsToKeepEQ <-  c("EQformOfCaffeine",
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
                           "EQ47_CaffLateMakesInsomnia",
                           "email"                           
) 


setnames(EQ887B, c("Q2", "Q2_TEXT", paste("Q", 4:51, sep="")), colsToKeepEQ) # uses setnames function from data.table package to reassign names in a similar way as rename function above but with slightly different syntax

# transform EQ887B, leaving in only the columns listed above
EQ887B <- EQ887B[, which(names(EQ887B) %in% colsToKeepEQ)]

# creates Substr_ID reference column
EQ887B$substr_ID <- substr(EQ887B$email, 1, 4)

# Remove email column

EQ887B <- EQ887B[, -which(names(EQ887B) %in% "email")]

# unique(EQ887B$Substr_ID) # checking.






############################# 4. Reading in the withdrawal symptom questionnaire #######################


### Need to remove the secondary header from the Qualtrics file by skipping the second line of the csv
all_content <- readLines("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/CWSQ_887_B.csv") # this reads in the qualtrics file and converts it to an object

skip_second <- all_content[-2] # removes second line with unneeded qualtrics headings

CWSQ887B <- read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

### Make sure column from the qualtrics file V8 (questionnaire start date) is a datetime

CWSQ887B$V8 = parse_date_time(CWSQ887B$V8, "%Y%m%d %H%M%S") # this is a function from lubridate package which automatically turns any input vector into a POSIXct date-time object. Here we are using the V8 column of the qualtrics spreadsheet as a 

######### Renaming the columns we want to keep ################################################

# vector of names for each CWSQ item

CWSQN <- c("drowsy",
           "selfconfident",
           "yawning",
           "alert",
           "tiredfatigued",
           "content",
           "diffconc",
           "irritable",
           "heavyfeel",
           "depressedmood",
           "grouchy",
           "urgework",
           "flulike",
           "headache",
           "talkative",
           "sluggish",
           "upsetstomach",
           "clearheaded",
           "desiresoc",
           "energetic",
           "nauseavom",
           "musclepain",
           "discouraged",
           "queasy",
           "nauseaous",
           "vomiting",
           "headachey",
           "anxious",
           "nervous",
           "jittery",
           "cravingcoffee",
           "cravingcaffeine",
           "email")


# creates the vector 
CWSQ887Names <- paste("Q", 1:33, CWSQN, sep="")

# change the relevant columns, which now have column names as you want them to appear in the composite file
setnames(CWSQ887B, paste("Q", 2:34, sep=""), CWSQ887Names)

setnames(CWSQ887B, "V8", "testtime") # changes the name of the data column used to sort the rows for each participant into chronological order to something more intuitive than V8


######################## removing unwanted columns. This can only be done using column numbers. But with the data.table package we can do it using names instead, which is always better.

# create vector of column names you want to drop

colsToDrop <- c(paste("V", 1:7, sep = "" ), paste("V", 9:10, sep=""), "Q1", "Q35", paste("Location", c("Latitude", "Longitude", "Accuracy"), sep=""), "X" )

# Turn CWSQ887B into a data.table so that we can apply the ':=' operator from the data.table package (this is another way to do the same thing as the which(names)... command above and below )

CWSQ887B <- as.data.table(CWSQ887B)

# Drop the columns using the ':=' operator

CWSQ887B <- CWSQ887B[, (colsToDrop):=NULL]

CWSQ887B <- as.data.frame(CWSQ887B)


###### Cleaning up the reference column

#Creating a reference column in the new object (which will be matched to a corresponding column in the master csv file). NOTE: Using first 4 letters of email will probably only work for current subjects, figure out something more robust!

CWSQ887B$substr_ID <- substr(CWSQ887B$Q33email, 1, 4) # creates a column in the CWSQ dataframe created above. This column is comprised of a substring extracted from the email address, 1st argument is the object (in this case the column Q34 which is the email address entered by each participant). 1st number is starting number, second number is how many characters to extract.

################ Check for typos

# Try to fix spaces etc. in email ID field (using regular expressions)

CWSQ887B$Q33email <- str_replace(CWSQ887B$Q33email, "\\s", "")

#unique(CWSQ887B$Q33email) # a check for typos

#unique(CWSQ887B[,"Substr_ID"]) # this tells us how many unique id codes we might have (this should match subject)


##################### Turning long format into wide for each subject ##############################

# Split dataset into pieces, one per subject, and order the V8/StartDate values within each subject

CWSQ887B <- ddply(CWSQ887B, # specifies data frame we are looking at
                  
                  "substr_ID", # specifies the variable which we will be subsetting by
                  
                  function(sub_rows) { # creates a function called sub_rows to apply to each subset within the id_code column of the dataframe CWSQ887B. Here the 'sub-rows' argument stands in for the dataframe CWSQ887B
  sub_rows$survey_num <- order(sub_rows$testtime) # creates a new column called survey_num in which each row is an order number for the place of that row within each subset of the 'id_code' column. The order numbers in this case are assigned based on the criteria 'date-time in column V8 of dataframe CWSQ887B' (Remember above on line 29 we specified this column as a date-time)
  return(sub_rows) # returns the newly augmented CWSQ887B
                  }
)



### Creates a new column assigning more meaningful names to the subject codes 

CWSQ887B$SurveyCode <- mapvalues(
  CWSQ887B$survey_num,
  from=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
  to=c("B1", "B2", "T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9"),
  warn_missing=TRUE
)

# Check that everything looks sensible

head(CWSQ887B[, c("substr_ID", "testtime", "survey_num", "SurveyCode")])

# get rid of superfluous numbered surveynumber (this role now filled by Survey_Code) and email columns
survNumEmail <- c("survey_num", "Q33email")

CWSQ887B <- CWSQ887B[, -which(names(CWSQ887B) %in% survNumEmail)]


# Go from long to wide, using the survey code as the suffix for each variable

CWSQ887B_reshaped <- reshape(CWSQ887B, idvar="substr_ID", timevar="SurveyCode", direction="wide", sep = "") # you might want to order the column-naming convention here so that the test name (e.g. QB1, QB2, QT1 etc) comes first followed by the 


# Giving columns proper names with Day/Test number as prefix

CWSQ887BNames <- colnames(CWSQ887B_reshaped[, 2:364])

CWSQ887BNames_re <- gsub("(.*)([A-Z][0-9])$", "\\2\\1", CWSQ887BNames)

setnames(CWSQ887B_reshaped, colnames(CWSQ887B_reshaped[,2:364]), CWSQ887BNames_re)

colnames(CWSQ887B_reshaped[, 2:364])






####################### 5. Exit Questionnaires #############################################

# Removing second line from the two exit questionnaires

# First questionnare (abstinent on second day)

all_content_ExitUsual <- readLines("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/Exit_887B_Usual.csv") # this reads in the qualtrics file and converts it to an object

skip_second_Usual <- all_content_ExitUsual[-2] # removes second line with unneeded qualtrics headings

ExitUsual887B <- read.csv(textConnection(skip_second_Usual), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

# Second questionnaire (abstinent on first day)

all_content_ExitCounter <- readLines("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/Exit_887_Counterbalanced.csv") # this reads in the qualtrics file and converts it to an object

skip_second_Counter <- all_content_ExitCounter[-2] # removes second line with unneeded qualtrics headings

ExitCounter887B <- read.csv(textConnection(skip_second_Counter), header = TRUE, stringsAsFactors = FALSE) # creates new object from the csv file, which has the second line removed

# merge the two exit questionnaires into one object

Exit887B <- rbind(ExitUsual887B, ExitCounter887B)



############# removing unwanted columns ###############################################

colsToKeepExit <- c("DurationCaff",
                    "Quit_Y1N0",
                    "NumQuit",
                    "ComplianceBaseline_Y1N0",
                    "ComplianceBaseline_TEXT",
                    "ComplianceTest_Y1N0",
                    "ComplianceTest_TEXT",
                    "Purpose",
                    "OtherComments",
                    "ProcedureDiffered_Y1N0",
                    "ProcedureDiffered_TEXT",
                    "TrueDoseDiffered_Y1N0",
                    "TrueDoseDiffered_TEXT",
                    "email"                    
                    )


# changes column names 

setnames(Exit887B, paste("Q", 2:15, sep=""), colsToKeepExit)


# removes superfluous columns by specifying and including only those we want to keep

Exit887B <- Exit887B[, which(names(Exit887B) %in% colsToKeepExit)]


# Adds Substr_ID column

Exit887B$substr_ID <- substr(Exit887B$email, 1, 4) 


# remove superfluous email column

Exit887B <- Exit887B[, -which(names(Exit887B) %in% "email")]










################### Merge Files ###############################################################

# Reference category is substr_ID

MasterList <- list(master887B, Exit887B, demog887B, EQ887B, CWSQ887B_reshaped)


compiledMaster <- Reduce( function (...) merge(..., by = "substr_ID", all = F), MasterList)

nrow(compiledMaster)

compiledMaster$substr_ID

write.csv (compiledMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/compiledMaster.csv")




####################### Swap over CWSQ scores for subjects whose abstinence baseline occurred on the first day 

# So we don't destroy the original
compMas <- compiledMaster

# preserves the original for reference
compMasOrig <- compMas

# Creating the lists of column names we want to swap

CWSQSymptoms <- c("drowsy",
                  "selfconfident",
                  "yawning",
                  "alert",
                  "tiredfatigued",
                  "content",
                  "diffconc",
                  "irritable",
                  "heavyfeel",
                  "depressedmood",
                  "grouchy",
                  "urgework",
                  "flulike",
                  "headache",
                  "talkative",
                  "sluggish",
                  "upsetstomach",
                  "clearheaded",
                  "desiresoc",
                  "energetic",
                  "nauseavom",
                  "musclepain",
                  "discouraged",
                  "queasy",
                  "nauseaous",
                  "vomiting",
                  "headachey",
                  "anxious",
                  "nervous",
                  "jittery",
                  "cravingcoffee",
                  "cravingcaffeine")

CWSQItems <- paste("Q", 1:32, CWSQSymptoms, sep="")

# 2 vectors of all item names for CWSQ for days 1 and 2. It is these that have to be swapped.
B1Items <- c("B1testtime", paste("B1", CWSQItems, sep = ""))

B2Items <- c("B2testtime", paste("B2", CWSQItems, sep = ""))

B2Items


# Create function for swapping columns around

swapColsCWSQ <- function (colNum) {
  compMas[colNum, B1Items] <- compMasOrig[colNum, B2Items] 
  compMas[colNum, B2Items] <- compMasOrig[colNum, B1Items]
  return(compMas)
}

# create Boolean vector of which rows/subjects abstained on the first day
CounterBal <- which(compMas$Counterbalanced_A1st1A2nd0 == 1)

# using that vector as criterion for which columns to swap
swappedMaster <- swapColsCWSQ(CounterBal)

# write new swapped file
write.csv(swappedMaster, "~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/swappedMaster.csv")






















