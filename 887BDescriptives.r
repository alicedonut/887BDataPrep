setwd("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files")

source("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/887BMasterScript.R")

descriptiveMaster <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/887BMaster.csv")

# means by group for morning and afternoon test sessions

# morning
morningGroupMeans887B <- aggregate(cbind(descriptiveMaster$T1Total, 
                                         descriptiveMaster$T3Total,
                                         descriptiveMaster$T5Total,
                                         descriptiveMaster$T7Total,
                                         descriptiveMaster$T9Total) ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, FUN = mean)

# afternoon
arvoGroupMeans887B <- aggregate(cbind(descriptiveMaster$T2Total, 
                                      descriptiveMaster$T4Total,
                                      descriptiveMaster$T6Total,
                                      descriptiveMaster$T8Total) ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, FUN = mean)


# rows using ddply from plyr package. subsets factored master dataset according to group level, then applies nrow function to each subset.

groupRows887B <- ddply(descriptiveMaster, "Group_Info2_MisInfo1_NoInfo0", nrow)

groupRows887B

# changes names of object groupRows887B using setnames function from data.table package. Could also use rename from plyr

setnames(groupRows887B, c("Group", "Number")) # this is a pared down version of setnames, used for when you are renaming all the columns. Below is the equivalent where you specify old column names and new column names. This is useful if you only want to name certain columns and leave others untouched.

#setnames(groupRows887B, c("Group_Info2_MisInfo1_NoInfo0", "V1"), c("Group", "Number"))

#################### Graph Morning Group Means ################################
#-------------------------------------------------------------------------------#
# now lets reassign column names
colnames(morningGroupMeans887B) <- c("Group",
                                     "Test1_Morning",
                                     "Test2_Morning",
                                     "Test3_Morning",
                                     "Test4_Morning",
                                     "Test5_Morning")

# Now we need to turn it into a long-form dataframe so we can graph it

require(tidyr)

longGroupsMorning887B <- gather(morningGroupMeans887B, Day, totalScore,
                                Test1_Morning:
                                  Test2_Morning:
                                  Test3_Morning:
                                  Test4_Morning:
                                  Test5_Morning)


# Now we graph it

require(ggplot2)


par(mgp = c(1,1,1))
pGroupMorn <- ggplot(data = longGroupsMorning887B, aes(x = Day, y = totalScore, group = Group, shape = Group)) +
  geom_line(aes(linetype = Group), size = 1) +
  geom_point(size = 5, fill = "white") +
  expand_limits(y = 0) +
  scale_color_hue(name = "Group", l = 30) +
  scale_shape_manual(name = "Group", values = c(23,22, 21)) +
  scale_linetype_discrete(name = "Group") +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  xlab("Day") + ylab ("CWSQ Scores") +
  ggtitle("Total Morning CWSQ Score \n Per Day By Group") +
  theme_bw() 




# Fixing up graph. First create variables for each element you want to change
titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
axisTextFont <- element_text(face = "plain", color = "black", size = 12)
axisTextFontX <- element_text(face = "plain", color = "black", size = 14, angle = 45, vjust = 1, hjust = 1)
legendTitle <- element_text(face = "bold", color = "black", size = 14)
legendText <- element_text(face = "plain", color = "black", size = 14)


#Call up pGroup2 (which is the variable assigned to the graph) and add fix-ups
pGroupMorn + theme(title = titleFont, 
                   axis.title = titleFont,
                   axis.title.x = titleFontX,
                   axis.text  = axisTextFont,
                   axis.text.x = axisTextFontX,
                   legend.title = legendTitle,
                   legend.text = legendText,
                   ##### remove #'s to put legend inside area of graph, otherwise will automatically create legend
                   #                legend.justification = c(0,0), #anchors legend in bottom left
                   #                legend.position = c(.7,.1), # uses anchor point to place legend
                   panel.grid.minor = element_blank(), #gets rid of gridlines
                   panel.grid.major = element_blank())  #+ #(uncomment '+' if you want to add the annotate argument below) 


#   # adds text
#   annotate("text", x = 1:7,
#            y = c(17, 36, 40, 36, 37, 44.5, 43),
#            label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
#                      "italic(p) == .61",
#                      "italic(p) == .72",
#                      "italic(p) == .30",
#                      "italic(p) == .46",
#                      "italic(p) == .80",
#                      "italic(p) == .29"),
#            size = 4.2,
#            family = "Helvetica", # specifies the font type 
#            parse = T) # the parse = TRUE argument allows you to add plotmath
# # arguments for mathematical notation, such as the italics


#dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory

###################### Graph Afternoon Group Means ###############################
#--------------------------------------------------------------------------------#
# now lets reassign column names
colnames(arvoGroupMeans887B) <- c("Group",
                                  "Test1_Afternoon",
                                  "Test2_Afternoon",
                                  "Test3_Afternoon",
                                  "Test4_Afternoon")

# Now we need to turn it into a long-form dataframe so we can graph it

require(tidyr)

longGroupsArvo887B <- gather(arvoGroupMeans887B, Day, totalScore,
                             Test1_Afternoon:
                               Test2_Afternoon:
                               Test3_Afternoon:
                               Test4_Afternoon)


# Now we graph it

require(ggplot2)


par(mgp = c(1,1,1))
pGroupArvo <- ggplot(data = longGroupsArvo887B, aes(x = Day, y = totalScore, group = Group, shape = Group)) +
  geom_line(aes(linetype = Group), size = 1) +
  geom_point(size = 5, fill = "white") +
  expand_limits(y = 0) +
  scale_color_hue(name = "Group", l = 30) +
  scale_shape_manual(name = "Group", values = c(23,22, 21)) +
  scale_linetype_discrete(name = "Group") +
  scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60)) +
  xlab("Day") + ylab ("CWSQ Scores") +
  ggtitle("Total Afternnon CWSQ Score \nPer Day By Group") +
  theme_bw() 




# Fixing up graph. First create variables for each element you want to change
titleFont <- element_text(face = "bold", color = "black", size = 16, vjust = 1.5)
titleFontX <- element_text(face = "bold", color = "black", size = 16, vjust = 0.01)
axisTextFont <- element_text(face = "plain", color = "black", size = 12)
axisTextFontX <- element_text(face = "plain", color = "black", size = 14, angle = 45, vjust = 1, hjust = 1)
legendTitle <- element_text(face = "bold", color = "black", size = 14)
legendText <- element_text(face = "plain", color = "black", size = 14)


#Call up pGroup2 (which is the variable assigned to the graph) and add fix-ups
pGroupArvo + theme(title = titleFont, 
                   axis.title = titleFont,
                   axis.title.x = titleFontX,
                   axis.text  = axisTextFont,
                   axis.text.x = axisTextFontX,
                   legend.title = legendTitle,
                   legend.text = legendText,
                   ##### remove #'s to put legend inside area of graph, otherwise will automatically create legend
                   #                legend.justification = c(0,0), #anchors legend in bottom left
                   #                legend.position = c(.7,.1), # uses anchor point to place legend
                   panel.grid.minor = element_blank(), #gets rid of gridlines
                   panel.grid.major = element_blank())  #+ #(uncomment '+' if you want to add the annotate argument below) 


# adds text
# annotate("text", x = 1:7,
#          y = c(17, 36, 40, 36, 37, 44.5, 43),
#          label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
#                    "italic(p) == .61",
#                    "italic(p) == .72",
#                    "italic(p) == .30",
#                    "italic(p) == .46",
#                    "italic(p) == .80",
#                    "italic(p) == .29"),
#          size = 4.2,
#          family = "Helvetica", # specifies the font type 
#          parse = T) # the parse = TRUE argument allows you to add plotmath
# # arguments for mathematical notation, such as the italics


# dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory

#-------------------------------------------------------------------------------#
######################### Testing Assumptions ####################################
#-------------------------------------------------------------------------------#

# First create a list of all the column names you need

visits <- c(paste("B", 1:2, sep=""), paste("T", 1:9, sep=""))

colTots <- paste(visits, "Total",  sep="")

drowsyTots <- paste(visits, "Drowsy", "Fac", sep="")
decAlertTots <- paste(visits, "DecAlert", "Fac", sep="")
moodTots <- paste(visits, "Mood", "Fac", sep="")
nauseaTots <- paste(visits, "DecreasedSocMotiv", "Fac", sep="")
flulikeTots <- paste(visits, "FluLike", "Fac", sep="")
headacheTots <- paste(visits, "Headache", "Fac", sep="")
acuteTots <- paste(visits, "Acute", "Fac", sep="")
cravingTots <- paste(visits, "Craving", "Fac", sep="")


######################### 1st assumption of ANOVA: No outliers #################

# 1a: Checking boxplots. Looking at the boxplots

require(RColorBrewer)

par(mfrow = c(1,1))
boxplot(descriptiveMaster[, drowsyTots], # add list of columns from above list here to check outliers
        horizontal = TRUE, 
        col = brewer.pal(7, "PuBu"),
        las = 2,
        cex.axis = 0.6)


# 1b: look at boxplots by group

par(mfrow = c(2,4))
boxplot(descriptiveMaster$T1Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T1")
boxplot(descriptiveMaster$T2Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T2")
boxplot(descriptiveMaster$T3Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T3")
boxplot(descriptiveMaster$T4Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T4")
boxplot(descriptiveMaster$T5Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T5")
boxplot(descriptiveMaster$T6Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T6")
boxplot(descriptiveMaster$T7Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T7")
boxplot(descriptiveMaster$T8Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0, main = "T8")

# we want to check the studentised residuals
T1lm <- lm(descriptiveMaster$T1Total ~ descriptiveMaster$Group_Info2_MisInfo1_NoInfo0)
t1Res <- resid(T1lm) # these are unstandardised residuals
t1StdRes <- rstandard(T1lm) #these are studentised residuals. These are standardised values of how how many SDs each score is for the prdicted score. You want to check through these for each DV and look for any that are >3 sd

# 1c: Look at an aq.plot

# aq.plot allows you to identify multivariate outliers by plotting the ordered square robust Mahalanobis distances of the observations against the empirical function of the MD-squared. The function produces four graphs and returns a boolean vector identifying the outliers.

# The function aq.plot plots the ordered squared robust Mahalanobis distances of the observations against the empirical distribution function of the $MD^2_i$. The distance calculations are based on the MCD estimator. For outlier detection two different methods are used. The first one marks observations as outliers if they exceed a certain quantile of the chi-squared distribution. The second is an adaptive procedure searching for outliers specifically in the tails of the distribution, beginning at a certain chisq-quantile (see Filzmoser et al., 2005). The function behaves differently depending on the dimension of the data.  If the data is more than two-dimensional the data are projected on the first two robust principal components.

require(mvoutlier)

outliers887B <- aq.plot(descriptiveMaster[, colTots])

outliers887B

############################## 2nd assumption of ANOVA: normality #######################

# 2a: Q-Plot

#do a qqplot of standardised residuals (we created this variable above remember. Need to do this for all the DVs)

par(mfcol = c(1,1))

qqnorm(t1StdRes)
qqline(t1StdRes)


# 2b: Shapiro-Wilk test. If the 'Sig.' level is less than .05 then the data for that group is not normally distributed. Shapiro-Wilk tests the Null hypothesis that the data is normally distributed So you WANT this statistic to be non-significant.


shapiro.test(descriptiveMaster$T1Total)

# 2c: multivariate normality

require(mvnormtest)

# use the function mshapiro.test to test for multivariate normality

#if we have p x 1 multivariate normal random vector 'X ~ N(mu, Sum) then the squared Mahanalobis distance between x and Mu is going to be chi-square distributed with p degrees of freedom. We can use this fact to construct a q-plot to assess multivariate normality

par(mfrow = c(1,1))
multVar <- as.matrix(descriptiveMaster[, colTots]) # n x p numeric matrix
center <- colMeans(multVar) # centroid
n <- nrow(multVar); p <- ncol(multVar); cov <- cov(multVar);
d <- mahalanobis(multVar,center,cov) # distances
View(multVar)
qqplot(qchisq(ppoints(n),df=p),d,
       main="QQ Plot Assessing Multivariate Normality",
       ylab="Mahalanobis D2")
abline(a=0,b=1)


#################### 3rd Assumption of ANOVA: Homogeneity of variance ###############

#The assumption of mixed ANOVA is that there are equal variances between the levels of the between-subjects factor at each level of the within-Ss factor. If the variances are unequal it can affect the Type 1 error rate. For this we use tests of equality of variances. For all these tests, the null hypothesis is that all populations variances are equal; the alternative hypothesis is that at least two of them differ. If it is  <.05 then there is a significant difference in variance between-groups. We want a non-significant value




# 3a: Bartlett's test

# If the data is normally distributed, this is the best test to use. It is sensitive to data which is not non-normally distribution; it is more likely to return a “false positive” when the data is non-normal.

bartlett.test(B1Total ~ Group_Info2_MisInfo1_NoInfo0, data = descriptiveMaster)


# for two IVS you need to include an interaction function, otherwise the degrees of freedom will be wrong

bartlett.test(B1Total ~ interaction(Group_Info2_MisInfo1_NoInfo0, Paid_Y1N0),
              data = descriptiveMaster)

# 3b: Levene's test 

# this is more robust to departures from normality than Bartlett’s test.

require(car)

leveneTest(B1Total ~ Group_Info2_MisInfo1_NoInfo0, data = descriptiveMaster)

# you can also do a multiple-IV Levene test, but we don't need a separate function

leveneTest(B1Total ~ Group_Info2_MisInfo1_NoInfo0*Paid_Y1N0, data = descriptiveMaster)


# 3c: fligner-killeen test

# this is a non-parametric test which is very robust against departures from normality.

fligner.test(B1Total ~ Group_Info2_MisInfo1_NoInfo0, data = descriptiveMaster)

# interactions are the same format as bartlett's

fligner.test(B1Total ~ interaction(Group_Info2_MisInfo1_NoInfo0, Paid_Y1N0),
             data = descriptiveMaster)


# 3d: hov plot

require(HH)

# The hovPlot( ) function in the HH package provides a graphic test of homogeneity of variances based on Brown-Forsyth. In the following example, dependent variable is numeric and predictor is a grouping factor. Note that predictor must be of type factor

# to perform hov procedure must have a grouping variable which is a factor

hov(B2Total ~ Group_Info2_MisInfo1_NoInfo0, data = descriptiveMaster)

hovPlot(B2Total ~ Group_Info2_MisInfo1_NoInfo0, data = descriptiveMaster)

#################### Assumption 4: homogeneity of covariance ###########################

# we test for homogeneity of covariance using box's test of covariance matrices. If the sign. level of this test is p <.001 then the covariances are not equal. If p >.001 then covariances are equal and assumption of homogeneity of covariance is satisfied. Note: there needs to be more observations per level of the grouping variable than there repeated measures variables. If there are less you'ss get an error message

require(biotools)

boxVar887B <- descriptiveMaster[, c(colTots, "Group_Info2_MisInfo1_NoInfo0")] #creates object with all DVs and group factor IV



boxM(boxVar887B[, -length(boxVar887B)], boxVar887B[,which(names(boxVar887B) %in% "Group_Info2_MisInfo1_NoInfo0")]) # data must be dataframe of ONLY numeric, but grouping must be factor, so the dataframe considered by the BoxM function has the group factor subtracted for the first argument, but then ONLY that factor for the following argument.

#---------------------------------------------------------------------------------------------#
################## Assumption 5: Sphericity ####################################################

# Sphericity is, in a nutshell, that the variances of the differences between the repeated measurements should be about the same.

# haven't written anything

