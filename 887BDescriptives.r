setwd("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files")

source("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/887BMasterScript.R")

# means by group for morning and afternoon test sessions

# morning
morningGroupMeans887B <- aggregate(cbind(factoredMaster$T1Total, 
                          factoredMaster$T3Total,
                          factoredMaster$T5Total,
                          factoredMaster$T7Total,
                          factoredMaster$T9Total) ~ factoredMaster$Group_Info2_MisInfo1_NoInfo0, FUN = mean)

# afternoon
arvoGroupMeans887B <- aggregate(cbind(factoredMaster$T2Total, 
                                         factoredMaster$T4Total,
                                         factoredMaster$T6Total,
                                         factoredMaster$T8Total) ~ factoredMaster$Group_Info2_MisInfo1_NoInfo0, FUN = mean)

arvoGroupMeans887B

# rows using ddply from plyr package. subsets factored master dataset according to group level, then applies nrow function to each subset.

groupRows887B <- ddply(factoredMaster, "Group_Info2_MisInfo1_NoInfo0", nrow)


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
pGroup2 <- ggplot(data = longGroupsMorning887B, aes(x = Day, y = totalScore, group = Group, shape = Group)) +
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
pGroup2 + theme(title = titleFont, 
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
  annotate("text", x = 1:7,
           y = c(17, 36, 40, 36, 37, 44.5, 43),
           label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
                     "italic(p) == .61",
                     "italic(p) == .72",
                     "italic(p) == .30",
                     "italic(p) == .46",
                     "italic(p) == .80",
                     "italic(p) == .29"),
           size = 4.2,
           family = "Helvetica", # specifies the font type 
           parse = T) # the parse = TRUE argument allows you to add plotmath
# arguments for mathematical notation, such as the italics


dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory

###################### Graph Afternoon Group Means ###############################
#--------------------------------------------------------------------------------#
# now lets reassign column names
colnames(arvoGroupMeans887B) <- c("Group",
                                     "Test1_Afternoon",
                                     "Test2_Afternoon",
                                     "Test3_Afternoon",
                                     "Test4_Afternoon")

View(arvoGroupMeans887B)
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
pGroup2 <- ggplot(data = longGroupsArvo887B, aes(x = Day, y = totalScore, group = Group, shape = Group)) +
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
pGroup2 + theme(title = titleFont, 
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
annotate("text", x = 1:7,
         y = c(17, 36, 40, 36, 37, 44.5, 43),
         label = c("italic(p) == .63", # these are plotmath arguments, p in italics 
                   "italic(p) == .61",
                   "italic(p) == .72",
                   "italic(p) == .30",
                   "italic(p) == .46",
                   "italic(p) == .80",
                   "italic(p) == .29"),
         size = 4.2,
         family = "Helvetica", # specifies the font type 
         parse = T) # the parse = TRUE argument allows you to add plotmath
# arguments for mathematical notation, such as the italics


dev.print(cairo_ps, "image.eps") #### sends open plot window to current working directory