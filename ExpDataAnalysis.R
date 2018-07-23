library(RTextTools)
library(e1071)
library(ggplot2)
library(tm)
library(gridExtra)
library(stringr)
library(MASS)
library(lubridate)
library(scales)
library(corrplot)
library(data.table)
library(plyr)
library(dplyr)


##### DATA PRE-PROCESSING AND META-DATA ANALYSIS #####

#load status updates from FB
load("status_updates.RData") 
#head(status_updates)

#load FB stats/ freq
freq <- read.table("freq.csv", sep = ",", header = TRUE)

#load demog from FB
demog1 <- read.table("demog.csv", sep = ",", header = TRUE)

#load cesd results
cesd <- read.table("cesd_item_level.csv", sep = ",", header = TRUE)

#add columns to hold cesd values that correspond to standard scale/scoring (orig values minus 1)
cesd$new_q1 <- cesd$q1 -1
cesd$new_q2 <- cesd$q2 -1
cesd$new_q3 <- cesd$q3 -1
cesd$new_q4 <- cesd$q4 -1
cesd$new_q5 <- cesd$q5 -1
cesd$new_q6 <- cesd$q6 -1
cesd$new_q7 <- cesd$q7 -1
cesd$new_q8 <- cesd$q8 -1
cesd$new_q9 <- cesd$q9 -1
cesd$new_q10 <- cesd$q10 -1
cesd$new_q11 <- cesd$q11 -1
cesd$new_q12 <- cesd$q12 -1
cesd$new_q13 <- cesd$q13 -1
cesd$new_q14 <- cesd$q14 -1
cesd$new_q15 <- cesd$q15 -1
cesd$new_q16 <- cesd$q16 -1
cesd$new_q17 <- cesd$q17 -1
cesd$new_q18 <- cesd$q18 -1
cesd$new_q19 <- cesd$q19 -1
cesd$new_q20 <- cesd$q20 -1

#replace negative cesd values with 0
cesd[ cesd == -2 ] <- 0


#sum up cesd scores
cesd$cesd_sum <- rowSums(cesd[,c(28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)])

#add label corresponding to CESD score cutoff
cesd[, "cesd_label"] <- NA
cesd <- within(cesd, cesd_label[cesd_sum<16] <- 0)
cesd <- within(cesd, cesd_label[cesd_sum>=16] <- 1)


#load big 5 results
big5 <- read.table("big5.csv", sep = ",", header = TRUE)


# merge two data frames: big 5 and freq by ID
big5_freq <- merge(big5, freq, by="userid")


# merge two data frames: cesd and freq by ID
cesd_freq <- merge(cesd, freq, by="userid")


# merge three data frames: big5, cesd and freq by ID
big5_cesd_freq <- merge(big5_freq, cesd, by="userid")


#merge two data frames: status_updates and freq by ID
fb_ids <- status_updates$userid[match(status_updates$userid,freq$userid)]

#number of unique users with status updates in FB
num_users_status <- length(unique(status_updates$userid))

#merge status updates and big 5
big5_status <- merge(big5, status_updates, by="userid")
big5_status_users_only <- unique(big5_status$userid)
#num_big5_users_status <- length(unique(big5_status$userid))
big5_users_results <- big5[big5$userid %in% big5_status_users_only,]
#length(unique(big5_users_results$userid))
# [1] 115873
big5_users_status <- status_updates[status_updates$userid %in% big5_status_users_only,]
big5_users_status_num <- table(big5_users_status$userid)




#merge status updates and cesd
cesd_status <- merge(cesd, status_updates, by="userid")
#num_cesd_users_status <- length(unique(cesd_status$userid))

cesd_status_users_only <- unique(cesd_status$userid)
cesd_users_results <- cesd[cesd$userid %in% cesd_status_users_only,]
# length(unique(cesd_users_results$userid))
# [1] 1047
#cesd_users_results$sum <- rowSums(cesd_users_results[,c(28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)])
cesd_users_status <- status_updates[status_updates$userid %in% cesd_status_users_only,]
cesd_users_status_num <- table(cesd_users_status$userid)

#get status updates of users with cesd label = 1
cesd_status_dep_users <- cesd_users_results[cesd_users_results$cesd_label == 1,]
#length(unique(cesd_status_dep_users$userid)) - 927

#get list of unique user ids
cesd_status_dep_userids <- unique(cesd_status_dep_users$userid)
num_unique <- length(unique(cesd_status_dep_users$userid))

cesd_status_dep <- cesd_status[cesd_status$userid %in% cesd_status_dep_userids,]


#################################################################################

#merge status updates and cesd and big5
big5_cesd_status <- merge(cesd, big5_status, by="userid")
big5_cesd_status_users_only <- unique(big5_cesd_status$userid)
test_big5_results <- big5[big5$userid %in% big5_cesd_status_users_only,]
test_cesd_results <- cesd[cesd$userid %in% big5_cesd_status_users_only,]

#add label corresponding to CESD score cutoff
#test_cesd_results$cesd_sum <- rowSums(test_cesd_results[,c(28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)])
#test_cesd_results[, "cesd_label"] <- NA
#test_cesd_results <- within(test_cesd_results, cesd_label[cesd_sum<20] <- 0)
#test_cesd_results <- within(test_cesd_results, cesd_label[cesd_sum>=20] <- 1)

test_status <- status_updates[status_updates$userid %in% big5_cesd_status_users_only,]
#big5_cesd_status$cesd_sum <- rowSums(big5_cesd_status[,c(28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)])
# length(unique(test_cesd_results$userid))
# [1] 981
# length(unique(test_big5_results$userid))
# [1] 981

#add label corresponding to CESD score cutoff
#big5_cesd_status[, "cesd_label"] <- NA
#big5_cesd_status <- within(big5_cesd_status, cesd_label[cesd_sum<16] <- 0)
#big5_cesd_status <- within(big5_cesd_status, cesd_label[cesd_sum>=16] <- 1)



### initial visualizations for big5_users_results

hist(big5_users_results$ope, main = "Histogram for Big 5 - Openness to Experience", xlab="Openness", border="black",col="black")
hist(big5_users_results$con, main = "Histogram for Big 5 - Conscientiousness", xlab="Conscientiousness", border="black",col="black")
hist(big5_users_results$ext, main = "Histogram for Big 5 - Extraversion", xlab="Extraversion", border="black",col="black")
hist(big5_users_results$agr, main = "Histogram for Big 5 - Agreeableness", xlab="Agreeableness", border="black",col="black")
hist(big5_users_results$neu, main = "Histogram for Big 5 - Neuroticism", xlab="Neuroticism", border="black",col="black")
hist(big5_users_status_num, main = "Histogram for Number of Status Updates in Table of Big 5 and Status updates, per User", xlab="Count of Status Updates", border="black",col="black")

### initial visualizations for cesd_users_results
hist(cesd_users_results$cesd_label, main = "Histogram for CESD results (total)", xlab="Label", border="black",col="black")
hist(cesd_users_results$cesd_sum, main = "Histogram for CESD results (sum)", xlab="CESD score (sum)", border="black",col="black")

hist(cesd_users_status_num, main = "Histogram for Number of Status Updates in Table of CESD and Status updates, per User", xlab="Count of Status Updates", border="black",col="black")


### create pie chart of cesd labels from CESD only Dataset
#source_count <- data.frame(table(cesd_users_results$cesd_label))
source_count <- data.frame(table(cesd$cesd_label))
#source_count <- source_count[order(-source_count$Freq),]
slices <- as.array(source_count$Freq)
lbls <-c("No Risk", "At Risk")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
cl <- c("#4169E1", "#DC143C")

pie(slices,labels = lbls, col=cl,
    main="CESD Score Labels for CESD Dataset", radius=0.9, clockwise = T, cex=.75)


### create pie chart of cesd labels from merged CESD and Big 5 Dataset
source_count <- data.frame(table(test_cesd_results$cesd_label))
slices <- as.array(source_count$Freq)
lbls <-c("No Risk", "At Risk")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
cl <- c("#4169E1", "#DC143C")

pie(slices,labels = lbls, col=cl,
    main="CESD Score Labels for merged CESD and Big 5 Dataset", radius=0.9, clockwise = T, cex=.75)



# compute and plot correlation between big 5 personality types
big5corrm <- cor(cbind(big5_users_results$ope, big5_users_results$con, big5_users_results$ext,big5_users_results$agr,big5_users_results$neu))
rownames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
colnames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
title <- "Correlations among Big 5 Personality Traits"
corrplot(big5corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))

# compute and plot correlation between big 5 personality types and cesd score
big5_cesd_corrm <- cor(cbind(big5_cesd_status$ope, big5_cesd_status$con, big5_cesd_status$ext,big5_cesd_status$agr,big5_cesd_status$neu, big5_cesd_status$cesd_label))
rownames(big5_cesd_corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "CESD")
colnames(big5_cesd_corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "CESD")
title <- "Correlations among Big 5 Personality Traits and CESD Score"
corrplot(big5_cesd_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))

library(polycor)
testcor <- hetcor(cbind(big5_cesd_status$ope, big5_cesd_status$con, big5_cesd_status$ext,big5_cesd_status$agr,big5_cesd_status$neu, big5_cesd_status$cesd_label), ML = FALSE, std.err = TRUE, bins=4, pd=TRUE)
test2cor <- polyserial(big5_cesd_status$neu, big5_cesd_status$cesd_label, ML = FALSE, std.err = FALSE, maxcor=.9999, bins=4)

###############################################################
#                                                             #
# START OF LIWC ANALYSIS                                      #
#                                                             #
###############################################################

#load liwc aggr data
liwc_agg <- read.table("liwc_aggr.csv", sep = ",", header = TRUE)

#merge LIWC and cesd total scores
cesd_liwc_agg <- merge(cesd[ , c("userid", "cesd_label")], liwc_agg, by="userid")


# compute and plot correlation between cesd and liwc 
cesd_liwc_corrm <- cor(cesd_liwc_agg[ , -1])
#rownames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#colnames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
title <- "Correlations between CESD and LIWC categories"
corrplot(cesd_liwc_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))


#scatter plot between cesd label and i
sp <- ggplot(cesd_liwc_agg, aes(x=cesd_label, y=i)) + 
  geom_point() +
  labs(x="CESD Label", y = " Use of I", title="Use of I by CESD label") 
#+  geom_text(hjust=0.4,vjust=1.5,size=2, check_overlap = TRUE)
sp

#boxplot between cesd label and i
bplot <- boxplot(i~cesd_label, data=cesd_liwc_agg, 
                 main="Use of I by CESD label", xlab="CESD Label",
                 ylab="Use of I") #, outline=FALSE)
bplot


#boxplot between cesd label and we
bplot <- boxplot(we~cesd_label, data=cesd_liwc_agg, 
                 main="Use of We by CESD label", xlab="CESD Label",
                 ylab="Use of We") #, outline=FALSE)
bplot


#boxplot between cesd label and posemo
bplot <- boxplot(posemo~cesd_label, data=cesd_liwc_agg, 
                 main="Use of Positive Emotions by CESD label", xlab="CESD Label",
                 ylab="Use of Positive") #, outline=FALSE)
bplot

#boxplot between cesd label and negemo
bplot <- boxplot(negemo~cesd_label, data=cesd_liwc_agg, 
                 main="Use of Negative Emotions by CESD label", xlab="CESD Label",
                 ylab="Use of Negative") #, outline=FALSE)
bplot

#boxplot between cesd label and analytic
bplot <- boxplot(Analytic~cesd_label, data=cesd_liwc_agg, 
                 main="Use of Analytic by CESD label", xlab="CESD Label",
                 ylab="Use of Analytic") #, outline=FALSE)
bplot

#boxplot between cesd label and sad
bplot <- boxplot(sad~cesd_label, data=cesd_liwc_agg, 
                 main="Use of Sad by CESD label", xlab="CESD Label",
                 ylab="Use of Sad") #, outline=FALSE)
bplot


#merge LIWC and big 5 personality types
big5_liwc_agg <- merge(big5[ , c("userid", "ope", "con", "ext", "agr", "neu")], liwc_agg, by="userid")


# compute and plot correlation between liwc and big 5 personality types
big5_liwc_corrm <- cor(big5_liwc_agg[ , -1])
#rownames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#colnames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
title <- "Correlations between Big5 Personality Traits and LIWC categories"
corrplot(big5_liwc_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))


###############################################################
#                                                             #
# START OF DEMOGRAPHICS ANALYSIS                              #
#                                                             #
###############################################################

#merge demog1 and cesd total scores
cesd_demog1 <- merge(cesd[ , c("userid", "cesd_label", "cesd_sum")], demog1, by="userid")


# compute and plot correlation between cesd and demog1: age, network size
cesd_demog1_corrm <- cor(cbind(cesd_demog1$cesd_label, cesd_demog1$age,cesd_demog1$network_size), use = "complete.obs")
rownames(cesd_demog1_corrm) <- c("CESD", "Age", "Network Size")
colnames(cesd_demog1_corrm) <- c("CESD", "Age", "Network Size")
title <- "Correlations between CESD and Age, Network Size"
corrplot(cesd_demog1_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))

#percentage of NAs in age, network size data
cesd_age_na_perc <- sum(is.na(cesd_demog1$age))/ length(cesd_demog1$age)
cesd_networksize_na_perc <- sum(is.na(cesd_demog1$network_size))/ length(cesd_demog1$network_size)

#merge demog1 and big 5 personality types
big5_demog1 <- merge(big5[ , c("userid", "ope", "con", "ext", "agr", "neu")], demog1, by="userid")


# compute and plot correlation between age, network size, and big 5 personality types
big5_demog1_corrm <- cor(cbind(big5_demog1$ope, big5_demog1$con, big5_demog1$ext, big5_demog1$agr, big5_demog1$neu, big5_demog1$age, big5_demog1$network_size), use = "complete.obs")
rownames(big5_demog1_corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Age", "Network Size")
colnames(big5_demog1_corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", "Age", "Network Size")
title <- "Correlations between Big5 Personality Traits and Age, Network Size"
corrplot(big5_demog1_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))

#percentage of NAs in age, network size data
big5_age_na_perc <- sum(is.na(big5_demog1$age))/ length(big5_demog1$age)
big5_networksize_na_perc <- sum(is.na(big5_demog1$network_size))/ length(big5_demog1$network_size)



###############################################################
#                                                             #
# START OF SNA ANALYSIS                                       #
#                                                             #
###############################################################

#load sna data
sna <- read.table("sna.csv", sep = ",", header = TRUE)

#merge sna and cesd total scores
cesd_sna <- merge(cesd[ , c("userid", "cesd_label", "cesd_sum")], sna, by="userid")


# compute and plot correlation between cesd and sna measures 
cesd_sna_corrm <- cor(cesd_sna[ , -1])
#rownames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#colnames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
title <- "Correlations between CESD and SNA measures"
corrplot(cesd_sna_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))


#merge sna and big 5 personality types
big5_sna <- merge(big5[ , c("userid", "ope", "con", "ext", "agr", "neu")], sna, by="userid")


# compute and plot correlation between big 5 personality types and sna measures
big5_sna_corrm <- cor(big5_sna[ , -1])
#rownames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
#colnames(big5corrm) <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
title <- "Correlations between Big5 Personality Traits and SNA measures"
corrplot(big5_sna_corrm, method = "number",tl.col="black", tl.srt=45, title=title, mar=c(0,0,2,0))


###############################################################
#                                                             #
# START OF STATUS UPDATES ANALYSIS                              #
#                                                             #
###############################################################
library(lubridate)
Sys.setenv(TZ='America/Toronto')
#add attribute to extract hour from the time
status_updates$updated_hour <- 0
status_updates$updated_hour <- format(ymd_hms(status_updates$updated_time), "%H")
#add attribute on number of night status updates between 9 PM - 5:59 AM
status_updates$night_posts <- 0
status_updates <- within(status_updates, night_posts[updated_hour>=21 & updated_hour<=5] <- 1)

#add attribute on number of day status updates between 6:01 AM - 8:59 PM
status_updates$day_posts <- 0
status_updates <- within(status_updates, day_posts[updated_hour>=6 & updated_hour<9] <- 1)

#compute number of night posts and day posts
user_night_posts <- data.frame(table(status_updates$userid, status_updates$night_posts))
user_day_posts <- user_night_posts[user_night_posts$Var2==0,]
user_night_posts <- user_night_posts[user_night_posts$Var2==1,]
colnames(user_night_posts)[colnames(user_night_posts)=="Freq"] <- "night_posts"
colnames(user_day_posts)[colnames(user_day_posts)=="Freq"] <- "day_posts"
user_insomnia_index <- merge(user_day_posts[, c("Var1", "day_posts")], user_night_posts[, c("Var1", "night_posts")], by="Var1")
colnames(user_insomnia_index)[colnames(user_insomnia_index)=="Var1"] <- "userid"
user_insomnia_index$value <- 0
user_insomnia_index$value <- scale(user_insomnia_index$night_posts)-scale(user_insomnia_index$day_posts)
colnames(user_insomnia_index)[colnames(user_insomnia_index)=="value"] <- "insomnia_index"

#get total number of status updates per user
user_num_status_updates <- data.frame(table(status_updates$userid))
colnames(user_num_status_updates)[colnames(user_num_status_updates)=="Var1"] <- "userid"
colnames(user_num_status_updates)[colnames(user_num_status_updates)=="Freq"] <- "nupdates"


#get mean of number of characters per user
user_mean_nchar <- aggregate(status_updates$nchar, list(status_updates$userid), mean)
colnames(user_mean_nchar)[colnames(user_mean_nchar)=="Group.1"] <- "userid"
colnames(user_mean_nchar)[colnames(user_mean_nchar)=="x"] <- "mean_nchar"






