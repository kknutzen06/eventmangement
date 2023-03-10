######INSTALL AND LOAD PACKAGES
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(readxl)){install.packages('readxl')}
if(!require(DescTools)){install.packages('DescTools')}
if(!require(sjPlot)){install.packages('sjPlot')}
if(!require(magrittr)){install.packages('magrittr')}
if(!require(rcompanion)){install.packages('rcompanion')}
if(!require(expss)){install.packages('expss')}
if(!require(chron)){install.packages('chron')}
if(!require(emmeans)){install.packages('emmeans')}
if(!require(car)){install.packages('car')}
if(!require(gmodels)){install.packages('gmodels')}
if(!require(Hmisc)){install.packages('Hmisc')}
if(!require(psych)){install.packages('psych')}
if(!require(dplyr)){install.packages('dplyr')}
if(!require(moments)){install.packages('moments')}
if(!require(ggeasy)){install.packages('ggeasy')}
if(!require(rstatix)){install.packages('rstatix')}
if(!require(lsr)){install.packages('lsr')}
if(!require(ggpubr)){install.packages('ggpubr')}
if(!require(WRS2)){install.packages('WRS2')}
if(!require(pgirmess)){install.packages("pgirmess")}
if(!require(ggcorrplot)){install.packages('ggcorrplot')}
if(!require(GPArotation)){install.packages('GPArotation')}
if(!require(ggridges)){install.packages('ggridges')}
if(!require(fmsb)){install.packages('fmsb')}
#load packages
library(ggcorrplot)
library(lsr)
library(ggplot2)
library(ggeasy)
library(lubridate)
library(moments)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(DescTools)
library(car)
library(gmodels)
library(reshape)
library(psych)
library(expss)
library(chron)
library(emmeans)
library(stringr)
library(rcompanion)
library(rstudioapi)
library(nlme)
library(matrixStats)
library(lme4)
library(ggpubr)
library(rstatix)
library(Hmisc)
library(GPArotation)
library(hrbrthemes)
library(ggridges)
#########WORKING DIRECTORY AND DATASET EDIT#######################################
dirpath <- rstudioapi::getActiveDocumentContext()
setwd(dirname(dirpath$path))
wd <- getwd()
options(scipen=999)
df<- read_csv2("sdgc.csv", col_names = TRUE,
               na = "-77")
set.seed(1)

###############################################################
##                                                           ##
##            SAMPLE DESCRIPTION                             ##
##                                                           ##
##############################################################
gender <- df$v_2
age <- df$v_3
country <- df$v_6
vrinterest <- df$v_9
gdexp <- df$v_10
gdknow <-df$v_11
svrexp <- df$v_99
role <- df$v_100
df_sample <-cbind.data.frame(gender,age, country, vrinterest, gdexp, gdknow, svrexp, role)
describe(df_sample) # interval scale descriptives
fre(df_sample$gdexp) # frequencies of filter question
fre(df_sample$gdknow)
fre(df_sample$gender)
fre(df_sample$country) # ordinal scale/ frequencies for countr of origin
fre(df_sample$role) # ordinal scale/ frequencies for role of user
#######################################################################################
#                                                                                     #
#           CALCULATING ALL SCORES OF THE RESPECTIVE QUESTIONNAIRES                   #
#                                                                                     #
#######################################################################################

######PRESENCE
####reference: http://dx.doi.org/10.1162/105474600566600
####Instructions: count all occurrences that are 6 or bigger and take the mean

df_sus <- cbind.data.frame(df$v_101, df$v_102, df$v_103, df$v_104, df$v_105, df$v_106, df_sample$role) # put all variables in one  dataframe
df_sus$big6 <- rowSums(df_sus >= 6)# counting all occurences that are bigger or equal to 6
fre(df_sus$big6) # frequencies of the counting
describe(df_sus$big6) # descriptives of the counting
df$sus <- df_sus$big6 # merging it with the original data frame
fre(df_sus$big6)
#check reliability
sus_alpha.sel <- dplyr::select(df_sus,df$v_101, df$v_102, df$v_103, df$v_104, df$v_105, df$v_106)
sus_alpha <- psych::alpha(sus_alpha.sel)
sus_alpha


########################Social Presence
df_socialpresence <- cbind.data.frame(subset(df, select = c(v_108: v_121))) # put all variables in a dataframe
df_socialpresence$v_116 <- car::recode(df_socialpresence$v_116, "1=5; 2=4;3=3;4=2;5=1") # reverse code items
df_socialpresence$v_117 <- car::recode(df_socialpresence$v_117, "1=5; 2=4;3=3;4=2;5=1") # reverse code
df_socialpresence$v_118 <- car::recode(df_socialpresence$v_118, "1=5; 2=4;3=3;4=2;5=1") # reverse code
df_socialpresence$mean <- rowMeans(df_socialpresence)# calculate mean
describe(df_socialpresence$mean) # mean results, descriptives
df$spres_mean <- df_socialpresence$mean # put the results back in the overall dataframe
###reliability
spres_alpha.sel <- dplyr::select(df_socialpresence, select = c(v_108:v_121))
spres_alpha <- psych::alpha(spres_alpha.sel)
spres_alpha
####plotting both presence scale  ###############
plot.pres <- data.frame(
  type = c( rep("social Presence",29), rep("Presence", 29)),
  value = c( df_socialpresence$mean, df_sus$big6))
p <- plot.pres %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p


################USABILITY score
####source: https://www.researchgate.net/publication/228593520_SUS_A_quick_and_dirty_usability_scale
#instruction: To calculate the SUS score, first sum the score contributions from each item. Each item's
#score contribution will range from 0 to 4. For items 1,3,5,7,and 9 the score contribution is the
#scale position minus 1. For items 2,4,6,8 and 10, the contribution is 5 minus the scale position.
#Multiply the sum of the scores by 2.5 to obtain the overall value of SU.
#SUS scores have a range of 0 to 100

df_susass <- cbind.data.frame(subset(df, select = c(v_122:v_131))) # select the 10 variables that belong to the sus

for (i in df_susass){ # copy the items 1,3,5,7,9  to a new data frame to subtract 1 from them collectively
  df_susass.trans1 <- df_susass$v_122
  df_susass.trans3 <- df_susass$v_124
  df_susass.trans5 <- df_susass$v_126
  df_susass.trans7 <- df_susass$v_128
  df_susass.trans9 <- df_susass$v_130
  i = i+1
}
#new dataframe with 1. part of transformed values for the final score
df_susass.trans_1 <- cbind.data.frame(df_susass.trans1, df_susass.trans3, df_susass.trans5, df_susass.trans7, df_susass.trans9)
#now subtract 1
df_susass.trans_1 <- df_susass.trans_1-1
for (i in df_susass){# same here but 5- the score for items 2,4,6,8,10
  df_susass.trans2 <- df_susass$v_123
  df_susass.trans4 <- df_susass$v_125
  df_susass.trans6 <- df_susass$v_127
  df_susass.trans8 <- df_susass$v_129
  df_susass.trans10 <- df_susass$v_131
  i = i+1
}
#new dataframe for the variables that shifted by 5
df_susass.trans_2 <- cbind.data.frame(df_susass.trans2, df_susass.trans4, df_susass.trans6, df_susass.trans8, df_susass.trans10)
#subtract 5- score
df_susass.trans_2  <- 5-df_susass.trans_2
#put both parts together
df_suass_transtotal <- cbind.data.frame(df_susass.trans_1, df_susass.trans_2)
#sum the score
df_suass_transtotal$sum <- rowSums(df_suass_transtotal)
#multiply the temporary score by 2,5 for the final score
df_suass_transtotal$multiply <- df_suass_transtotal$sum * 2.5
describe(df_suass_transtotal$multiply)#  deescriptives of the results
df$usability <- df_suass_transtotal$multiply # put the final score in the overall data frame

###reliability
usa_alpha.sel <- dplyr::select(df_suass_transtotal, select = c(df_suass_transtotal$df_susass.trans1:df_susass.trans10))
usa_alpha <- psych::alpha(spres_alpha.sel, check.keys = TRUE)
usa_alpha

#plotting usability
usa <- df%>%
  ggplot( aes(x=usability)) +
  geom_histogram( binwidth=15, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Usability") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
usa

#########social interaction
#source: https://www.researchgate.net/publication/341707689_Pre_Print_Grasping_the_Social_Dimensions_of_Event_Experiences_Introducing_the_Event_Social_Interaction_Scale_ESIS

df_esis <- cbind.data.frame(subset(df, select = c(v_132:v_145))) # select all variables
df_esis$v_137 <- car::recode(df_esis$v_137, "1=5; 2=4;3=3;4=2;5=1")# reverse code
df_esis$v_143 <- car::recode(df_esis$v_143, "1=5; 2=4;3=3;4=2;5=1") # reverse code
esis.mean <- rowMeans(df_esis)# calculate the mean
describe(esis.mean) # descriptives of the mean
df$esis.mean <- esis.mean # put the final mean back in the overall data frame
#reliability
esis_alpha.sel <- dplyr::select(df_esis, select = c(v_132:v_145))
esis_alpha <- psych::alpha(esis_alpha.sel)
esis_alpha


####plotting esis mean

esis.plot <- df%>%
  ggplot( aes(x=esis.mean)) +
  geom_histogram( binwidth=0.5, fill="#900562", color="#e9ecef", alpha=0.9) +
  ggtitle("Social Interaction") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )
esis.plot
##################################################################
##                                                              ##
##                        FACTOR ANALYSIS                       ##
##                                                              ##
##################################################################

df_esis_r <-df_esis %>% # renaming all variables for easier identificatio for varimax rotation
  dplyr::rename("newacquaintances" = "v_132",
                "enjoynewppl" = "v_133",
                "dothingswstrangers" = "v_134",
                "sharedinterests" = "v_135",
                "truststranger" = "v_136",
                "avoidstranger" = "v_137",
                "sensebelonging" = "v_138",
                "help" = "v_139",
                "partgroup" = "v_140",
                "sharedinfogroup" = "v_141",
                "groupcode" = "v_142",
                "talkgroup" = "v_143",
                "rituals" = "v_144",
                "symbolic" = "v_145",
  )

#factor analysis for 3 to 4 factors as suggested by the source material
esisr.fa.4 <- factanal(df_esis_r, factors = 4, rotation = "varimax")
esisr.fa.4
esisr.fa.3 <- factanal(df_esis_r, factors = 3, rotation = "varimax")
esisr.fa.3

faktorplots <-plot(eigen(cor(df_esis_r))$values, type = "b") #check the eigenvalues in the plot and if 3  or 4 factors cover the explained variance
dev.off() # delete the plot
df_esis_r.pa.promax <- principal(df_esis_r, # apply varimax rotation
                                 nfactors=4,
                                 rotate="varimax"
)
print(df_esis_r.pa.promax) # results of varimax rotation, 4 factors seem likely

###checking each factor if they correlate with each other and their reliability
#factor 1 , "contact with strangers" according to source
factor1.m <- as.matrix(subset(df_esis_r, select = c(newacquaintances, enjoynewppl, dothingswstrangers, sharedinterests)))# select the variables and put them in a matrix
######correlation
factor1.c <- rcorr(factor1.m,type = "spearman") # apply spearman correlation
factor1.c # show correlation coefficients
factor1.c$P # show significance level
######reliability
factor1_alpha <- psych::alpha(factor1.m)
factor1_alpha
#descriptives for the mean of the factor 
factor1.mean <- rowMeans(factor1.m)
describe(factor1.mean)
df$esis.f1 <- factor1.mean


#repeat for factor 2
factor2.m <- as.matrix(subset(df_esis_r, select = c(truststranger, help, sharedinfogroup, groupcode)))

###correlation
factor2.c <- rcorr(factor2.m,type = "spearman")
factor2.c
factor2.c$P
###reliability
factor2_alpha <- psych::alpha(factor2.m)
factor2_alpha
##descriptives
factor2.mean <- rowMeans(factor2.m)
describe(factor2.mean)
df$esis.f2 <- factor2.mean


#repeat for factor 3
factor3.m <- as.matrix(subset(df_esis_r, select = c(avoidstranger, enjoynewppl, help, newacquaintances, talkgroup)))
###correlation
factor3.c <- rcorr(factor3.m,type = "spearman")
factor3.c
factor3.c$P
###reliability
factor3_alpha <- psych::alpha(factor3.m)
factor3_alpha
#descriptives
factor3.mean <- rowMeans(factor3.m)
describe(factor3.mean)
df$esis.f3 <- factor3.mean
#repeat for factor 4
factor4.m <- as.matrix(subset(df_esis_r, select = c(rituals, symbolic,partgroup)))
#correlation
factor4.c <- rcorr(factor4.m,type = "spearman")
factor4.c
factor4.c$P
####reliability
factor4_alpha <- psych::alpha(factor4.m)
factor4_alpha
#descriptives
factor4.mean <- rowMeans(factor4.m)
describe(factor4.mean)
factor4.mean
df$esis.f4 <- factor4.mean

##plotting factors
plot.sinterac <- data.frame(
  factor = c(rep("Contact with trangers",29), rep("Sharing same interests", 29), rep("Known group socialization", 29), rep ("Symbolic events and rituals", 29)),
  value.f =c(df$esis.f1, df$esis.f2, df$esis.f3, df$esis.f4 )
)

esisplott <- ggplot(plot.sinterac, aes(x = value.f, y = factor, fill = factor)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
esisplott

##plotting a radar chart
library(fmsb)
describe(df$esis.f1)
radar_sinterac <- data.frame(matrix(c(max(df$esis.f1), max(df$esis.f2), max(df$esis.f3), max(df$esis.f4)), ncol = 4))
                             
                             
radar_sinterac <- rbind(radar_sinterac, 
                        c(min(round(df$esis.f1)), min(round(df$esis.f2)), min(round(df$esis.f3)), min(round(df$esis.f4))),
                        c(mean(df$esis.f1),mean(df$esis.f2),mean(df$esis.f3),mean(df$esis.f4))
                        )
head(radar_sinterac)
colnames(radar_sinterac)  = c("Strangers", "Same\ninterests","Knowngroup\n socialization", "Events and\n rituals")

head(radar_sinterac)
typeof(radar_sinterac)
radar_sinterac <- data.frame(radar_sinterac)
typeof(radar_sinterac)

radarchart(radar_sinterac,seg = 6,  axistype = 1, plty = 1,pcol = 3,  plwd = 5,cglcol = 1,vlcex = 0.75,  caxislabels = c(1,2,3,4,5,6,7), axislabcol = "black")

# 2nd radar chart with single items
##plotting a radar chart
library(fmsb)
radar_sinterac2 <- data.frame(matrix(c(max(df$v_132), max(df$v_144), max(df$v_143), max(df$v_137), max(df$v_143), max(df$v_140), max(df$v_138, max(df$v_133), max(df$v_138), max(df$v_136), max(df$v_132), max(df$v_142), max(df$v_139), max(df$v_141))), ncol = 14))

mean <- c(mean(df$v_132), mean(df$v_144), mean(df$v_143), mean(df$v_137), mean(df$v_143), mean(df$v_140), mean(df$v_138), mean(df$v_133), mean(df$v_138), mean(df$v_136), mean(df$v_132), mean(df$v_142), mean(df$v_139), mean(df$v_141))



min <- c(min(round(df$v_132)), min(round(df$v_144)), min(round(df$v_143)), min(round(df$v_137)), min(round(df$v_143)), min(round(df$v_140)), min(round(df$v_138)), min(round(df$v_133)), min(round(df$v_138)), min(round(df$v_136)), min(round(df$v_132)), min(round(df$v_142)), min(round(df$v_139)), min(round(df$v_141)))

mean
radar_sinterac2
radar_sinterac2 <- rbind(radar_sinterac2, min, mean)
                        

                        
head(radar_sinterac2,)
colnames(radar_sinterac2)  = c("I made new acquaintances", "Event as a symbolic meeting moment","Group's rituals at the event", "Not avoiding contact with strangers", "Not talking only with known-group ", "I felt part of a larger group", "I felt a sense of belonging", "I enjoyed meeting new people","We shared the same interests", "I felt I could trust strangers at the event", "I did things together with strangers", "I followed my group's code of behaviour", "I helped anybody who needed", "I shared information with people in my group")

head(radar_sinterac2)
typeof(radar_sinterac2)
radar_sinterac <- data.frame(radar_sinterac2)
typeof(radar_sinterac2)

radarchart(radar_sinterac2,seg = 6,  axistype = 1, plty = 6,pcol = 2,  plwd = 3, cglcol = 1,vlcex = 0.75,  caxislabels = c(1,2,3,4,5,6,7), axislabcol = "black")
################################################################################################
##                                                                                            ##
##                        EXPLORATORY QUESTIONS WRT TECHNICAL ISSUES                          ##
##                                                                                            ##
################################################################################################
#descriptives
df_sec7 <- cbind.data.frame(df$v_156,df$v_160, df$v_161, df$v_147, df$v_146)# put them in one data frame
fre(df$v_154)# frequencies
describe(df_sec7) # descriptives
######################################################
##                                                  ##
##              CORRELATIONS                        ##
##                                                  ##
######################################################

###technical difficulties -> usability, are technical issues related to their experienced usability of the platform?

usa_tech.df <-cbind.data.frame(df_sec7$`df$v_147`,df_sec7$`df$v_146`, df_suass_transtotal$multiply)# put all variables in one data frame
usa_tech.m <- as.matrix(usa_tech.df) # transform in a matrix
pairs(usa_tech.m)  ##create scatterplots for linearity assumption inspecting to check if we can use pearson or spearman correlation
usa_tech.cor <- rcorr(usa_tech.m, type = "spearman") # spearman correlation
usa_tech.cor$r # correlation coefficients
usa_tech.cor$P # significance levels
# plotting the results
usa_tech_plot<- ggcorrplot(usa_tech.cor$r, hc.order = TRUE, type = "lower", lab = TRUE, p.mat = usa_tech.cor$P, insig = "blank", colors = c("#de2c00", "white", "#04d40f"), title = "Technical difficulties in Correlation to Usability")
usa_tech_plot
dev.off()

####social presence - social interaction - presence, how are these 3 variables correlated to each other?
spres_sinterac <- cbind.data.frame(df$spres_mean, df$esis.mean, df$sus ) # put variables in one data frame
spres_sinterac.m <- as.matrix(spres_sinterac) # convert to matrix
pairs(spres_sinterac.m) # check linearity plots

spres_sinterac.cor <- rcorr(spres_sinterac.m, type = "spearman") # apply correlation tests
spres_sinterac.cor$r # correlation coefficients
spres_sinterac.cor$P # significance levels
#plotting the results
spres_sinterac_plot<- ggcorrplot(spres_sinterac.cor$r, hc.order = TRUE, type = "lower", lab = TRUE, p.mat = spres_sinterac$P, insig = "blank", colors = c("#de2c00", "white", "#04d40f"), title = "(Social) Presence - Social Interaction - Usability - Presence")
spres_sinterac_plot

###factors of social interaction - social presence, how are the selective factors correlated to social presence?
f_spres_sinterac <- cbind.data.frame(df$spres_mean,df$esis.mean, df$esis.f1,df$esis.f2, df$esis.f3, df$esis.f4)
f_spres_sinterac.m <- as.matrix(f_spres_sinterac)
pairs(f_spres_sinterac.m)

f_spres_sinterac.cor <- rcorr(f_spres_sinterac.m, type = "spearman")
f_spres_sinterac.cor$r
f_spres_sinterac.cor$P

f_spres_sinterac_plot<- ggcorrplot(f_spres_sinterac.cor$r, hc.order = TRUE, type = "lower", lab = TRUE, p.mat = f_spres_sinterac$P, insig = "blank", colors = c("#de2c00", "white", "#04d40f"), title = "Social Presence - Social Interaction Factors ")
f_spres_sinterac_plot

f_spres_sinterac.cor
##############################################################
##                                                          ##
##          SPLITTING BY KNOWLEDGE  (WIP)                   ##
##                                                          ##
##############################################################
fre(df$v_10)
newbie <- subset(df, df$v_10< 2)
newbie.df <- as.data.frame(newbie)
newbie.df$group <- 1

experienced <- subset(df, df$v_10 >1)
experienced.df <- as.data.frame(experienced)
experienced.df$group <- 2
df_split <- rbind(newbie.df, experienced.df)

###sample description

describeBy(df_split$v_2, group = df_split$group)#age
describeBy(df_split$v_9, group = df_split$group)#vrinterest
describeBy(df_split$v_2, group = df_split$group)#age

describeBy(df_split$v_10, group = df_split$group)#gd experience
describeBy(df_split$v_11, group = df_split$group)#gd knowledge
describeBy(df_split$v_99, group = df_split$group)#social vr experience

df_split %>%# count country of origin per group
  group_by(group, v_6) %>%
  dplyr::summarize(count = n())

df_split %>% # count role per group
  group_by(group, v_100) %>%
  dplyr::summarize(count = n())

df_split %>% # count gender per group
  group_by(group, v_2) %>%
  dplyr::summarize(count = n())
##dependent variables
describeBy(df_split$spres_mean, group = df_split$group)#social presence
describeBy(df_split$sus, group = df_split$group)#presence
describeBy(df_split$esis.mean, group = df_split$group)#social interaction
describeBy(df_split$esis.f1, group = df_split$group)#social interaction, factor 1, contact to strangers
describeBy(df_split$esis.f2, group = df_split$group)# social interaction, factor 2, sharing interests
describeBy(df_split$esis.f3, group = df_split$group) # social interaction, factor 3, knowngroup socialization
describeBy(df_split$esis.f4, group = df_split$group)# social interaction, factor 4, events and rituals
describeBy(df_split$usability, group = df_split$group)#usability

#####################################functions for normality testing
qqnormPlot <- function(variableQQ)
{
  result <- qqnorm(variableQQ, main = deparse(substitute(variableQQ)))
}

qqlinePlot <- function(variableQQ)
{
  result <- qqline(variableQQ, main = deparse(substitute(variableQQ)))
}

histPlot <- function(variableHist)
{
  
  result <- hist(variableHist)
}

boxPlotND <- function(variableBox)
{
  result <- boxplot(variableBox, main = deparse(substitute(variableBox)))
}

outliers <- function(variableOut)
{
  out_variable <- boxplot(variableOut, plot = FALSE) $out
  outliersIndex <- which (variableOut %in% out_variable)
  return(outliersIndex)
}

shapiroFunction <- function(variableShapiro)
{
  shapiroResult <- shapiro.test(variableShapiro)
  return(shapiroResult)
}



normal_distribution.ttest <- function(dataValidVariable, dataGroup1Variable, dataGroup2variable, dataframe, name)
{
  #Show descriptives
  descriptive_overall <- describe(dataValidVariable, IQR = TRUE, quant = c(.25, .75)) ##whole sample
  descriptive_pergroup <- describeBy(dataValidVariable, group = df_split$group, IQR = TRUE, quant = c(.25, .75)) ##per group
  variableName = toString(name) ##get name of variable for plots
  
  #set up layout for plots for observed data
  par(mar=c(2.5,2.5,1,1))
  layout(matrix(c(1,2,3,4,1,5,3,6),ncol=2),heights=c(1,3,1,3))
  plot.new()
  text(0.5,0.5,labels = paste(name , " Group Cues"),cex=2,font=2)
  
  #QQPlots of observed data
  qqnormPlot(dataGroup1Variable)
  qqlinePlot(dataGroup1Variable)
  
  plot.new()
  text(0.5,0.5,labels = paste(name , " Without Group Cues"),cex=2,font=2)
  qqnormPlot(dataGroup2variable)
  qqlinePlot(dataGroup2variable)
  
  #histograms of observed data
  histPlot(dataGroup1Variable)
  histPlot(dataGroup2variable)
  
  #skewness and kurtosis of observed data
  kurtosisGroup <- kurtosis(dataGroup1Variable)
  skewnessGroup <- skewness(dataGroup1Variable)
  kurtosisGroup2 <- kurtosis(dataGroup2variable)
  skewnessGroup2 <- skewness(dataGroup2variable)
  
  #boxplots
  par(mfrow=c(1,2))
  boxPlotND(dataGroup1Variable)
  boxPlotND(dataGroup2variable)
  
  #outliers
  outliers_group <- outliers(dataGroup1Variable)
  outliers_wo <- outliers(dataGroup2variable)
  
  #Shapiro-Wilk test for observed data
  shapiroGroup <- shapiroFunction(dataGroup1Variable)
  shapiroGroup2 <- shapiroFunction(dataGroup2variable)
  
  ##Levene's test
  dataframe$group <- as.factor(dataframe$group)
  levene <- leveneTest(dataValidVariable, group = dataframe$group)
  
  ###print plots and results
  returnList <- list("Name" = name, "Descriptives Overall" = descriptive_overall, "Descriptives Per Group" = descriptive_pergroup,
                     "Kurtosis Group Cues" = kurtosisGroup, "Skewness Group 1" = skewnessGroup,
                     "Kurtosis Group2" = kurtosisGroup2, "Skewness Group 2" = skewnessGroup2,
                     "Outliers Group 1" = outliers_group, "Outliers Group 2" = outliers_wo,
                     "Shapiro-Wilk-Test Group 1" = shapiroGroup, "Shapiro-Wilk-Test Group 2" = shapiroGroup2,
                     "Levene" = levene)
  return (returnList)
}
normal_distribution.ttest(df_split$spres_mean,newbie.df$spres_mean, experienced.df$spres_mean, df_split, "Social Presence")

normal_distribution.ttest(df_split$sus,newbie.df$sus, experienced.df$sus, df_split, "Presence")

normal_distribution.ttest(df_split$esis.mean,newbie.df$esis.mean, experienced.df$esis.mean, df_split, "Social Interaction")
##social inteaction mean is normally distributed
normal_distribution.ttest(df_split$esis.f1,newbie.df$esis.f1, experienced.df$esis.f1, df_split, "Social Interaction (Factor 1)")
normal_distribution.ttest(df_split$esis.f2,newbie.df$esis.f2, experienced.df$esis.f2, df_split, "Social Interaction (Factor 2)")
normal_distribution.ttest(df_split$esis.f3,newbie.df$esis.f3, experienced.df$esis.f3, df_split, "Social Interaction (Factor 3)")
normal_distribution.ttest(df_split$esis.f4,newbie.df$esis.f4, experienced.df$esis.f4, df_split, "Social Interaction (Factor 4)")
# factor 4 is normally distributed
normal_distribution.ttest(df_split$usability,newbie.df$usability, experienced.df$usability, df_split, "Usability")
#usability is normally distributed

t.test(df_split$esis.mean ~ df_split$group, na.action = na.exclude, alternative = "two.sided")#social interaction
t.test(df_split$esis.f4 ~ df_split$group, na.action = na.exclude, alternative = "two.sided") # factor 4
t.test(df_split$usability ~ df_split$group, na.action = na.exclude, alternative = "two.sided")# usability
wilcox.test(df_split$sus ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided") # presence
wilcox.test(df_split$spres_mean ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided") # social presence
wilcox.test(df_split$sus ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided")
wilcox.test(df_split$esis.f1 ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided")
wilcox.test(df_split$esis.f2 ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided")
wilcox.test(df_split$esis.f3 ~ df_split$group, exact = FALSE, na.action = na.exclude, alternative = "two.sided")

