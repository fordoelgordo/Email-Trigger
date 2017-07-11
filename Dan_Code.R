##Project aimed at identifying triggers that we can test to improve email campaign open rates##
##Read in, clean and merge datasets from Vertica##
require(plyr)
require(caTools)
require(ROCR)
require(rpart)
require(rpart.plot)
require(picante)
require(scales)
require(Information)
require(reshape2)
require(ClustOfVar)
Email_tran <- read.csv("Email respondents with transactions.csv")
Email_tran$contact_date <- as.Date(Email_tran$contact_date)
Email_days <- read.csv("Email respondents with days since last email.csv")
Email_days$r_c_date <- as.Date(Email_days$r_c_date)
Email_days$c_max_date <- as.Date(Email_days$c_max_date)
Email_days$days_last_email <- as.numeric(Email_days$c_max_date - Email_days$r_c_date)
Email_days <- subset(Email_days, select = -(c_max_date))
names(Email_days)[names(Email_days) == "r_c_date"] <- "most_recent_email_date"
Email_contact <- read.csv("Email respondents email contacts.csv")
Email_contact <- subset(Email_contact, select = -(max_c_date))
Email_coupon <- read.csv("Email coupon.csv")
Email_coupon <- subset(Email_coupon, select = -(contact_date))
Email_sp <- read.csv("Email speedperks member.csv")
Email_sp <- subset(Email_sp, select = -c(contact_date))
Email_scores <- read.csv("Email respondents model scores.csv")
Email_extra <- read.csv("Email respondents thirty sixty ninety.csv")
Email_extra <- subset(Email_extra, select = c("campaign_id","indv_id","tran3060","net_sales3060","units3060","tran6090","net_sales6090","units6090"))
E_merge <- join(Email_tran, Email_days, by = c("indv_id","campaign_id"), type = "left")
E_merge <- join(E_merge, Email_contact, by = c("indv_id", "campaign_id"), type = "left")
E_merge <- join(E_merge, Email_coupon, by = c("indv_id", "campaign_id"), type = "left")
E_merge <- join(E_merge, Email_sp, by = c("indv_id", "campaign_id"), type = "left")
E_merge <- join(E_merge, Email_extra, by = c("indv_id", "campaign_id"), type = "left")
E_merge$day <- weekdays(E_merge$contact_date)
E_merge$day <- factor(E_merge$day, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
E_merge$coupon_user <- ifelse(E_merge$num_coupons > 0, 1, 0)
E_merge$coupon_user <- as.factor(E_merge$coupon_user)
E_merge$sp_member <- as.factor(E_merge$sp_member)

summary(E_merge) #we can see that there are missing values for days between most recent email.  This is due to a number of factors - it could be that we've never emailed that person before, they've been emailed before but the email was never delivered, or they've been contacted on a direct mail campaign but haven't been contacted on an email campaign.  Additionally, there are missing values for coupon users, SP members, and CLV/Defector scores.  The original make-up of respondents when the data was first pulled can change, so there are some indv_ids in the original data set that no longer fit the original parameters.  For CLV/Defector scores, these models are based on behavior in the last 24 months.  If customers in the original email list haven't transacted during this time period, they won't have a score.  For the purpose of this analysis, let's remove the rows with NA values
E_merge <- na.omit(E_merge)

##Investigate why so few indv_ids are present in the CLV/Defector dataset##
ncut <- cut(E_merge$days_last_tran, c(0,365,720,Inf), include.lowest = TRUE, labels = c("1","2","3+"))
summary(ncut)
##Doesn't seem to make sense, since there were plenty of customers transacting in the past 1-2 years##
#We might have been using the wrong CLV/Defector table.  Until further updates are made to the table, let's pull in all the indv_ids we can and attempt to match to our original dataset##
require(RODBC)
tblData <- sqlQuery(dbCon, sql, stringsAsFactors = FALSE)
E_merge <- join(E_merge, tblData, by = "indv_id", type = "left")
E_merge$rCLV <- round(E_merge$CLV,2)

#Introductory logisitic response model on the entire dataset to identify statistically relevant predictors of the probability that a person will open an email
model1 <- glm(opened ~ days_last_tran + tran30 + net_sales30 + units30 + tran60 + net_sales60 + units60 + tran90 + net_sales90 + units90 + days_last_email + contact30 + contact60 + contact90 + day + coupon_user + num_coupons + sp_member + CLV + Defector + tran6090 + net_sales6090 + units6090 + tran3060 + net_sales3060 + units3060, data = E_merge, family = "binomial")
summary(model1)
x <- summary(model1)$coef
x <- round(x,5)
x[order(abs(x[,1])),]

#Review of pairwise correlations between variables
cordata <- subset(E_merge, select = c(tran30, tran60, tran90, contact30, contact60, contact90, days_last_tran, days_last_email, Defector, CLV, tran3060, tran6090))
cordata <- na.omit(cordata)
cor.table(cordata, cor.method = "pearson")$r

#WOE and IV analysis to attempt to verify most predictive variables
IVData <- subset(E_merge, select = -c(campaign_id, campaign_code, campaign_name, program, indv_id, contact_date))
IV <- create_infotables(data = IVData, y = "opened", parallel = FALSE)
knitr::kable(IV$Summary)
##Our top 2 variables ranked by IV are days_last_email and contact30.  Day is technically a top 5 variable by IV, but because AAP has not experimented with different days, it's not a particularly representive variable.  Let's investigate the WOE tables further
knitr::kable(IV$Tables$days_last_email)
knitr::kable(IV$Tables$contact30)
plot_infotables(IV, IV$Summary$Variable[1:2], same_scales = TRUE)

#Exploratory graphical analysis to analyze the relationship between our different predictors and email open rate
#Relationship between days since the person was last emailed and open rate
days_last_email_WOE <- cut(E_merge$days_last_email, c(0,6,10,15,31,446), include.lowest = TRUE, labels = c("1-6","7-10","11-15","16-31","32-446"))
E_merge$days_last_email_WOE <- days_last_email_WOE
##Comment or uncomment the line below if you'd like to print the image##
jpeg("Days since last email.jpeg", width = 8, height = 8, units = "in", res = 300)
tab <- table(E_merge$days_last_email_WOE, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$days_last_email_WOE <- as.character(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tab$days_last_email_WOE)+1), ylim = c(0,250)))
axis(1, labels = tab$days_last_email_WOE, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 250, by = 25), labels = seq(0, 250, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Customers (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Days since last email", cex.lab = 1.15)
title(main = "Open rate vs. Days since last Email")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue")
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$days_last_email_WOE)+1))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .35, by = .025)), at = seq(0, .35, by = .025), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
dev.off()
#Relationship between number of times the customer has been contacted in the past 30 days and the open rate
jpeg("Emails last 30 days.jpeg", width = 8, height = 8, units = "in", res = 300)
tab <- table(E_merge$contact30, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$contact30 <- as.numeric(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tab$contact30)+2), ylim = c(0,150)))
axis(1, labels = tab$contact30, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 150, by = 25), labels = seq(0, 150, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Customers (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Times emailed in 30 days", cex.lab = 1.15)
title(main = "Open rate vs. Times emailed in 30 days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue")
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$contact30)+2))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, 1, by = .1)), at = seq(0, 1, by = .1), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
dev.off()
##Relationship between number of times the customer has been contacted in the past 30 days and open rate (binned)
jpeg("Emails last 30 days binned.jpeg", width = 8, height = 8, units = "in", res = 300)
x <- .bincode(E_merge$contact30, breaks = c(0,0,1,3,4,5,66), right = TRUE, include.lowest = TRUE)
contact30_WOE <- factor(x, labels = c("0","1","2-3","4","5","6-66"))            
E_merge$contact30_WOE <- contact30_WOE
tab <- table(E_merge$contact30_WOE, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$contact30_WOE <- as.character(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tab$contact30_WOE)+1), ylim = c(0,100)))
axis(1, labels = tab$contact30_WOE, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 100, by = 20), labels = seq(0, 100, by = 20), las = 2, cex.axis = .8)
title(ylab = "Count of Customers (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Times contacted last 30 days", cex.lab = 1.15)
title(main = "Open rate vs. Times contacted last 30 days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue")
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$contact30_WOE)+1))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .30, by = .025)), at = seq(0, .30, by = .025), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
dev.off()
#Relationship between transactions in 30 days and open rate
tab <- table(E_merge$tran30, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- cbind(tab, rep_rate)
tab <- data.frame(tab)
tab$tran30 <- as.numeric(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tran30)+2), ylim = c(0,375)))
axis(1, labels = tab$tran30, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 350, by = 25), labels = seq(0, 350, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Emails (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Transactions in 30 days", cex.lab = 1.15)
title(main = "Open rate vs. Transactions in 30 Days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue")
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$tran30)+2))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .35, by = .025)), at = seq(0, .35, by = .025), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
#Relationship between transactions in 60 days and open rate
tab <- table(E_merge$tran60, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$tran60 <- as.numeric(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tran60)+3), ylim = c(0,375)))
axis(1, labels = tab$tran60, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 350, by = 25), labels = seq(0, 350, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Emails (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Transactions in 60 days", cex.lab = 1.15)
title(main = "Open rate vs. Transactions in 60 Days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue")
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$tran60)+3))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .35, by = .025)), at = seq(0, .35, by = .025), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
#Relationship between transactions in 90 days and open rate
tab <- table(E_merge$tran90, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$tran90 <- as.numeric(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tran90)+2), ylim = c(0,350)))
axis(1, labels = tab$tran60, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 300, by = 25), labels = seq(0, 300, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Emails (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Transactions in 90 days", cex.lab = 1.15)
title(main = "Open rate vs. Transactions in 90 Days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue", srt = 45, adj = c(.5,5))
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$tran90)+2))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .35, by = .025)), at = seq(0, .35, by = .025), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
##Example of a variable that has a poor relationship to response rate##
jpeg("Transactions last 30-60 days.jpeg", width = 8, height = 8, units = "in", res = 300)
tab <- table(E_merge$tran3060, E_merge$opened)
num_respondents <- rowSums(tab)
tab <- cbind(tab, num_respondents)
rep_rate <- tab[,2]/tab[,3]
tab <- data.frame(cbind(tab, rep_rate))
tab$tran3060 <- as.numeric(rownames(tab))

par(mar = c(8,5,5,8))
bar <- with(tab, barplot(num_respondents/1000, yaxt = "n", ylab ="", xaxt = "n", xlab = "", col = "azure3", xlim = c(0, length(tran3060)+2), ylim = c(0,400)))
axis(1, labels = tab$tran3060, at = bar[,1], cex.axis = .8, col = NA, col.ticks = 1)
axis(2, at = seq(0, 400, by = 25), labels = seq(0, 400, by = 25), las = 2, cex.axis = .8)
title(ylab = "Count of Emails (000s)", line = 2.5, cex.lab = 1.15)
title(xlab = "Transactions in 30-60 days", cex.lab = 1.15)
title(main = "Open rate vs. Transactions in 30-60 Days")
text(x = bar[,1], y = tab$num_respondents/1000, label = tab$num_respondents, pos = 3, cex = .8, col = "blue", srt = 0, adj = c(.5,5))
par(new = TRUE)
plot(bar[,1], tab$rep_rate, type = "n", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0,length(tab$tran3060)+2))
points(bar[,1], tab$rep_rate, col = "red3", pch = 10)
lines(bar[,1], tab$rep_rate, col = "red3")
axis(4, labels = percent(seq(0, .50, by = .05)), at = seq(0, .50, by = .05), line =1,  las = 2, cex.axis = .8)
mtext("Open Rate", side = 4, line = 4.25, cex = 1.15)
dev.off()

#Backwards regression selection testing
backwards <- step(model1, trace = 0)

#For correct model validation, we need to see if this response model accurately predicts the probability that a person will open an email for out-of-sample email campaigns (e.g. the model needs to be externally validated).  A good first step though is to test the internal validation of this model, or how predictive it is on a randomly sampled subset of the data
set.seed(11)
spl <- sample.split(E_merge$opened, SplitRatio = .70)
E_train <- subset(E_merge, spl == TRUE)
E_test <- subset(E_merge, spl == FALSE)
#We can rerun our IV/WOE tests now that we have a testing and training set to evaluate the penalty on IV
IVdtrain <- subset(E_train, select = -c(campaign_id, campaign_code, campaign_name, program, indv_id, contact_date))
IVdtest <- subset(E_test, select = -c(campaign_id, campaign_code, campaign_name, program, indv_id, contact_date))
IV2 <- create_infotables(data = IVdtrain, valid = IVdtest, y = "opened", parallel = FALSE)
knitr::kable(IV2$Summary)
#It appears that our original variables have essentially the same IV as before, as the penalty adjustments are pretty low.  This is likely because we are not externally validating our model, but are instead validating it on a subset of the data itself

#Given our first regression model, let's see if we can refine the model further
modellog <- glm(opened ~ days_last_tran + tran30 + tran60 + tran90 + tran3060 + tran6090 + units30 + units60 + units90 + units3060 + units6090 + net_sales30 + net_sales60 + net_sales90 + net_sales3060 + net_sales6090 + days_last_email + contact30 + contact60 + contact90 + num_coupons + sp_member + CLV + Defector + coupon_user + days_last_email_WOE + contact30_WOE, data = E_train, family = "binomial")

#Backwards regression selection testing
back <- step(modellog, trace = 0)
#The backwards selection removes sp_member, num_coupons, contact30, and the majority of the net_sales variables from the model, albeit with minimal improvement in AIC
AIC(modellog)
AIC(back)
exp((AIC(back) - AIC(modellog))/2) #relative likelihood of modellog.  Intepreted as modellog is x times as probable as the the model back to minimize information loss

#Because our backwards selection process selected a lot of variables that are highly correlated, let's complete a variable cluster analysis to determine if there is a singular variable that we can use within each cluster instead of all variables without significant loss in predictive power
ctrain <- E_train[,!(names(E_train) %in% c("campaign_id","campaign_code","campaign_name","program","indv_id","contact_date","opened","clicked","most_recent_email_date","day","contact30_WOE","days_last_email_WOE","coupon_user","sp_member"))]
ctrain2 <- E_train[,(names(E_train) %in% c("day","contact30_WOE","days_last_email_WOE","coupon_user","sp_member"))]
ctest <- E_test[,!(names(E_test) %in% c("campaign_id","campaign_code","campaign_name","program","indv_id","contact_date","opened","clicked","most_recent_email_date","day","contact30_WOE","days_last_email_WOE","coupon_user","sp_member"))]
ctest2 <- E_test[,(names(E_test) %in% c("day","contact30_WOE","days_last_email_WOE","coupon_user","sp_member"))]

tree <- hclustvar(X.quanti = ctrain, X.quali = ctrain2)
nvars <- length(tree[tree$height<.7])
part_init <- cutreevar(tree,nvars)$cluster
kmeans <- kmeansvar(X.quanti = ctrain, X.quali = ctrain2, init=part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster","Variable")
clusters <- join(clusters,IV$Summary, by = "Variable", type = "left")
clusters <- clusters[order(clusters$Cluster),]
clusters$Rank <- stats::ave(-clusters$IV, clusters$Cluster, FUN = rank)

#The code above conducted hiearchial clustering, then appended the original IV value to each variable.  We then ranked each variable within each cluster by IV.  In order to select the variables to use for modeling, we could simply select the variable that has the highest IV within each cluster.
selected_members <- subset(clusters, Rank == 1)
selected_members$Rank <- NULL
print(selected_members, row.names = FALSE)

clustmodel <- glm(opened ~ days_last_tran + tran30 + net_sales30 + tran90 + net_sales90 + units90 + days_last_email + contact30 + tran3060 + net_sales3060 + tran6090 + net_sales6090 + units6090 + CLV + Defector + sp_member, data = E_train, family = "binomial")
back2 <- step(clustmodel, trace = 0)

#our baseline prediction would be to always predict that a respondent will not open an email, as the majority of contacts do not open their email
table(E_test$opened) #our baseline accuracy is ~81% of respondents won't open their email
predictlog <- predict(back2, newdata = E_test, type = "response")
table(E_test$opened, predictlog > .50) #our model accuracy is ~ 80%, which is slightly less than our baseline model. That being said, we are still accurately predicting that people will open emails, instead of the baseline model which always predicts that people will not open emails
#which threshold value to pick?  Let's generate the ROC curve and see what the trade-offs are
ROCRpred <- prediction(predictlog, E_test$opened)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
pdf(NULL)
dev.control(displaylist="enable")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,.6,.05), text.adj=c(-.2,1.7), main = "ROC Curve")
pl.base <- recordPlot()
invisible(dev.off())
grid::grid.newpage()
pl.base
#A threshold value of .2 gives us an 80% true positive rate while giving us a little less than a 40% false positive rate.  Our model accuracy at this threshold value is computed below
moda <- table(E_test$opened, predictlog > .20)
(moda[1,1]+moda[2,2])/sum(moda)
AUC <- as.numeric(performance(ROCRpred, "auc")@y.values) #Also referred to as the C-stat
D <- 2*(AUC-.50)
plot(residuals(back2, type = "pearson"), main = "Pearson residual plot")

#Susan suggested that we try using WOE transformations in our model.  Let's see if our AIC improves using the two WOE transformations for our two variables with the best IV scores
woemodel <- glm(opened ~ days_last_tran + net_sales30 + days_last_email_WOE + contact30_WOE + tran3060 + tran6090 + CLV + Defector + sp_member, data = E_train, family = "binomial")
AIC(back2) - AIC(woemodel)
woeprediction <- predict(woemodel, newdata = E_test, type = "response")
ROCRwoe <- prediction(woeprediction, E_test$opened)
AUCwoe <- as.numeric(performance(ROCRwoe, "auc")@y.values)
rbind(AUC, AUCwoe)
#It looks like our AIC decreases using WOE transformations (notice that the difference in AIC between the two models is positive).   Let's see if we can generate more accurate predictions using these WOE transformations
##Create WOE variable transformations for the variables in our woemodel
net_sales30_WOE <- as.factor(ifelse(E_merge$net_sales30 <= 0, "<=0",">0"))
days_last_tran_WOE <- cut(E_merge$days_last_tran, c(0,30,76,116,195,331,533,835,1201,1811,3752), include.lowest = TRUE, labels = c("0-30","31-76","77-116","117-195","196-331","332-533","534-835","836-1201","1202-1811","1812-3752"))
tran3060_WOE <- sapply(seq_along(E_merge$tran3060), function(i) as.factor(ifelse(E_merge$tran3060[i] == 0, "0", "1-13")))
tran6090_WOE <- sapply(seq_along(E_merge$tran6090), function(i) as.factor(ifelse(E_merge$tran6090[i] == 0, "0", "1-13")))
Defector_WOE <- cut(E_merge$Defector, c(0,30,40,46,50,54,57,61,64,68,Inf), include.lowest = TRUE, labels = c("1-30","31-40","41-46","47-50","51-54","55-57","58-61","62-64","65-68","69-72"))
x <- IV$Tables$rCLV[,1]
x <- x[-1]
CLV_WOE <- cut(E_merge$rCLV, c(-1340.5,101.8,113.53,123.05,131.86,140.26,149.97,164.8,188.57,244.11,Inf), include.lowest = TRUE, labels = x)
sp_member_WOE <- as.factor(ifelse(E_merge$sp_member == 0, "0","1"))
##Add variable transformations back into the dataset
E_merge$net_sales30_WOE <- net_sales30_WOE
E_merge$days_last_tran_WOE <- days_last_tran_WOE
E_merge$tran3060_WOE <- tran3060_WOE
E_merge$tran6090_WOE <- tran6090_WOE
E_merge$Defector_WOE <- Defector_WOE
E_merge$CLV_WOE <- CLV_WOE
E_merge$sp_member_WOE <- sp_member_WOE
#Resample the dataset for modeling using the same seed value so we get the same training and testing set as before
set.seed(11)
spl <- sample.split(E_merge$opened, SplitRatio = .70)
E_train <- subset(E_merge, spl == TRUE)
E_test <- subset(E_merge, spl == FALSE)
#Let's try building a logistical regression model that only uses variables that have been WOE transformed
woemodel2 <- glm(opened ~ days_last_tran_WOE + net_sales30_WOE + days_last_email_WOE + contact30_WOE + tran3060_WOE + tran6090_WOE + CLV_WOE + Defector_WOE + sp_member_WOE, data = E_train, family = "binomial")
AIC(woemodel) - AIC(woemodel2) #We achieved a 157 point improvement in AIC with the new model
woeprediction2 <- predict(woemodel2, newdata = E_test, type = "response")
ROCRwoe2 <- prediction(woeprediction2, E_test$opened)
AUCwoe2 <- as.numeric(performance(ROCRwoe2, "auc")@y.values)
AUCwoe - AUCwoe2 #Our c-stat for our model with all WOE transformations performed slightly better in predicting opens than our model with only a few WOE transformations

woemodel3 <- glm(opened ~ days_last_tran_WOE + days_last_email_WOE + contact30_WOE + tran3060_WOE + tran6090_WOE + CLV_WOE, data = E_train, family = "binomial")
AIC(woemodel2) - AIC(woemodel3) #By only transforming some variables to WOE, we lower our AIC from the 2 WOE model a litte bit further
woeprediction3 <- predict(woemodel3, newdata = E_test, type = "response")
ROCRwoe3 <- prediction(woeprediction3, E_test$opened)
AUCwoe3 <- as.numeric(performance(ROCRwoe3, "auc")@y.values)
woepredictiontrain <- predict(woemodel3, newdata = E_train, type = "response")
ROCRtrain <- prediction(woepredictiontrain, E_train$opened)
AUCwoetrain <- as.numeric(performance(ROCRtrain, "auc")@y.values)
AUCwoe - AUCwoe3
#It appears that our model using a combination of WOE variable transformations and normal variable values produces the best c-stat.  Let's recreate our ROC curve and evaluate the number of customers falling within the top 10% of predictions.
ROCRperf3 <- performance(ROCRwoe3, "tpr", "fpr")
pdf(NULL)
dev.control(displaylist="enable")
plot(ROCRperf3, colorize = TRUE, print.cutoffs.at = seq(0,.6,.05), text.adj=c(-.2,1.7), main = "ROC Curve")
pl.base <- recordPlot()
invisible(dev.off())
grid::grid.newpage()
pl.base
##Code to print ROC curve##
jpeg("WOE Model ROC.jpeg", width = 8, height = 8, units = "in", res = 300)
ROCRperf3 <- performance(ROCRwoe3, "tpr", "fpr")
plot(ROCRperf3, colorize = TRUE, print.cutoffs.at = seq(0,.6,.05), text.adj=c(-.2,1.7), main = "ROC Curve")
dev.off()

woepredictionfull <- predict(woemodel3, newdata = E_merge, type = "response")
E_merge$predprob <- woepredictionfull
round(quantile(woepredictionfull, probs = seq(0,1,.10), na.rm = TRUE),5)#top 10% of predictions are 34.92% and up
table(subset(E_merge, predprob >= .34916)$opened)
table(subset(E_merge, predprob < .34916)$opened)
##Code to export model coefficient table to existing Excel sheet##
require(xlsx)
coeftable <- summary(woemodel3)$coef
IVTables <- rbind(as.matrix(IV$Tables$days_last_tran), as.matrix(IV$Tables$days_last_email), as.matrix(IV$Tables$contact30), as.matrix(IV$Tables$tran3060), as.matrix(IV$Tables$tran6090), as.matrix(IV$Tables$CLV))
write.xlsx(coeftable, "Email Triggers Model Viz.xlsx", sheetName = "Coef Table", row.names = TRUE, append = TRUE)
write.xlsx(IVTables, "Email Triggers Model Viz.xlsx", sheetName = "IV Tables", row.names = TRUE, append = TRUE)

#Code to bin the model predictions by quantile, and compare to average open rate within each bin
q <- quantile(woeprediction3, probs = seq(0, 1, .20), na.rm = TRUE)
qcut <- cut(woeprediction3, breaks = c(q[[1]],q[[2]],q[[3]],q[[4]],q[[5]],q[[6]]), labels = c("0-20","20-40","40-60","60-80","80-100"), include.lowest = TRUE)
mpred <- tapply(woeprediction3, qcut, mean)
tab <- table(qcut, E_test$opened)
tab <- cbind(tab, round(tab[,2]/rowSums(tab),3))
tab <- cbind(tab, mpred)
##Code for model validation on an external dataset##
require(data.table)
val_merge <- join(emailstats, dayslasttran, by = "indv_id", type = "left")
val_merge <- join(val_merge, dayslastemail, by = "indv_id", type = "left")
setnames(tran3060, old = c("transactions", "net_sales", "units"), new = c("tran3060", "net_sales3060", "units3060"))
val_merge <- join(val_merge, tran3060, by = "indv_id", type = "left")
setnames(tran6090, old = c("transactions", "net_sales", "units"), new = c("tran6090", "net_sales6090", "units6090"))
val_merge <- join(val_merge, tran6090, by = "indv_id", type = "left")
val_merge <- join(val_merge, val_CLV, by = "indv_id", type = "left")
val_merge <- join(val_merge, contact30, by= "indv_id", type = "left")
val_merge$rCLV <- round(val_merge$CLV, 2)
##Replace NA values with zero in the transaction columns, since technically it's not a missing value, it's a value for zero since the customer didn't transact during that time period
val_merge$tran3060[is.na(val_merge$tran3060)] <- 0
val_merge$net_sales3060[is.na(val_merge$net_sales3060)] <- 0
val_merge$units3060[is.na(val_merge$units3060)] <- 0
val_merge$tran6090[is.na(val_merge$tran6090)] <- 0
val_merge$net_sales6090[is.na(val_merge$net_sales6090)] <- 0
val_merge$units6090[is.na(val_merge$units6090)] <- 0
##Add WOE transformations to variables in order to generate predictions
val_merge$days_last_email_WOE <- cut(val_merge$days_last_email, c(0,6,10,15,31,446), include.lowest = TRUE, labels = c("1-6","7-10","11-15","16-31","32-446"))
x <- .bincode(val_merge$contact30, breaks = c(0,0,1,3,4,5,Inf), right = TRUE, include.lowest = TRUE)
val_merge$contact30_WOE <- factor(x, labels = c("0","1","2-3","4","5","6-66"))            
val_merge$days_last_tran_WOE <- cut(val_merge$days_last_tran, c(0,30,76,116,195,331,533,835,1201,1811,Inf), include.lowest = TRUE, labels = c("0-30","31-76","77-116","117-195","196-331","332-533","534-835","836-1201","1202-1811","1812-3752"))
val_merge$tran3060_WOE <- sapply(seq_along(val_merge$tran3060), function(i) as.factor(ifelse(val_merge$tran3060[i] == 0, "0", "1-15")))
val_merge$tran6090_WOE <- sapply(seq_along(val_merge$tran6090), function(i) as.factor(ifelse(val_merge$tran6090[i] == 0, "0", "1-13")))
x <- IV$Tables$rCLV[,1]
x <- x[-1]
val_merge$CLV_WOE <- cut(val_merge$rCLV, c(-1340.5,101.8,113.53,123.05,131.86,140.26,149.97,164.8,188.57,244.11,Inf), include.lowest = TRUE, labels = x)


valpred <- predict(woemodel3, newdata = val_merge, type = "response")
ROCRvalpred <- prediction(valpred, val_merge$opened)
AUCval <- as.numeric(performance(ROCRvalpred, "auc")@y.values)

jpeg("External Validation Model ROC.jpeg", width = 8, height = 8, units = "in", res = 300)
ROCRvalperf <- performance(ROCRvalpred, "tpr", "fpr")
plot(ROCRvalperf, colorize = TRUE, print.cutoffs.at = seq(0,.6,.05), text.adj=c(-.2,1.7), main = "ROC Curve")
dev.off()
##Generate prediction quantiles and compare average predicted probability of opening an email and actual open rate
qval <- quantile(valpred, probs = seq(0, 1, .20), na.rm = TRUE)
qvalcut <- cut(valpred, breaks = c(qval[[1]],qval[[2]],qval[[3]],qval[[4]],qval[[5]],qval[[6]]), labels = c("0-20","20-40","40-60","60-80","80-100"), include.lowest = TRUE)
mvalpred <- tapply(valpred, qvalcut, mean)
tab <- table(qvalcut, val_merge$opened)
tab <- cbind(tab, round(tab[,2]/rowSums(tab),3))
tab <- cbind(tab, mvalpred)
####THE CODE BELOW IS USED TO EVALUATE THE EFFECT OF THE DIFFERENT CAMPAIGNS REPRESENTED IN THE DATA AS WELL AS EVALUATE THE EFFICACY OF CART AND RANDOM FOREST MODELS FOR PREDICTION### 

#Our underlying dataset consisted of two campaigns with every different open rates
tab <- table(E_merge$campaign_id, E_merge$opened)
c1 <- tab[1,2]/(tab[1,1]+tab[1,2])
c2 <- tab[2,2]/(tab[2,1]+tab[2,2])
tab <- cbind(tab,rbind(c1,c2))
tab
#If we separate the data by campaign_id, will our general model parameters change?
camp1 <- subset(E_merge, campaign_id == -1890596347)
camp2 <- subset(E_merge, campaign_id == -1887079586)
logcamp1 <- glm(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons, data = camp1, family = "binomial")
logcamp2 <- glm(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons, data = camp2, family = "binomial")
comp <- cbind(summary(back)$coef[,1], summary(logcamp1)$coef[,1], summary(logcamp2)$coef[,1])
colnames(comp) <- c("Combined model", "Campaign 1", "Campaign 2")
#Interestingly enough, we do have slight variations when we split the data by campaign.  In general the variables are still all significant, but we do have some slight sign changes.  Regardless, modeling by campaign is ineffective, as we are trying to determine attributes that influence open rates regardless of campaign

##Above we used a logistic regression model to predict the probability that a respondent would open an email.  Below we will see if we can get better predictions using CART trees##
set.seed(1000)
E_merge$opened <- as.factor(E_merge$opened)
spl <- sample.split(E_merge$opened, SplitRatio = .70)
train <- subset(E_merge, spl == TRUE)
test <- subset(E_merge, spl == FALSE)
EmailTree <- rpart(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons + coupon_user + sp_member, data = train, control = rpart.control(minsplit=1, minbucket=1, cp=.0001), method = "class")
prp(EmailTree)
##Apply cross-fold validation to find best cp parameter value##
numFolds <- trainControl(method ="cv", number = 10)
cpGrid <- expand.grid(.cp=seq(.00001,.0002,.00001))
train(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons + coupon_user + sp_member, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid)
#Based on CV testing, the optimal CP value is .00007
EmailTreeCV <- rpart(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons + coupon_user + sp_member, data = train, method = "class", cp = .00007)
prp(EmailTreeCV)
predCV <- predict(EmailTreeCV, newdata = test)
predCV <- predCV[,2]
table(test$opened, predCV > .5)
table(test$opened, predCV > .2)
ROCRtreecv <- prediction(predCV, test$opened)
TreeAUC <- as.numeric(performance(ROCRtreecv, "auc")@y.values)
TreeAUC
##Using CART models, we were able to achieve a slightly better AUC then we were able to achieve under logistical regression.  That being said, our ROC curve is slightly different under the CART model vs. the logistical regression model.
par(mfrow = c(1,2))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,.6,.05), text.adj=c(-.2,1.7), main = "GLM ROC Curve")
ROCRtreeperf <- performance(ROCRtreecv, "tpr","fpr")
plot(ROCRtreeperf, colorize = TRUE, print.cutoffs.at = seq(0,.6,.1), text.adj=c(-.2,1.7), main = "CART ROC Curve")

##While random forest models are typically less interpretable, they can sometimes generate better predictions then CART trees.  Let's see if we can improve our predictions using random forest models
library(randomForest)
set.seed(123)
Emailforest <- randomForest(opened ~ days_last_tran + tran30 + tran60 + tran90 + days_last_email + contact30 + contact60 + contact90 + num_coupons + coupon_user + sp_member, data = train, nodesize = 25, ntree = 50)
#Create chart showing number of times a variable has been chosen to split on over all of the trees in the forest
par(mfrow = c(1,1))
vu <- varUsed(Emailforest, count = TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(Emailforest$forest$xlevels[vusorted$ix]))
#Show the reduction in impurity for each variable that we're splitting on in the model
varImpPlot(Emailforest)
#Create predictions from the random forest model on the test set
predforest <- predict(Emailforest, newdata = test, type ="prob")[,2]
table(test$opened, predforest > .5)
table(test$opened, predforest > .2)
ROCRforest <- prediction(predforest, test$opened)
ROCRforestperf <- performance(ROCRforest, "tpr","fpr")
forestAUC <- as.numeric(performance(ROCRforest, "auc")@y.values)
forestAUC
plot(ROCRforestperf, colorize = TRUE, print.cutoffs.at = seq(0,.6,.1), text.adj=c(-.2,1.7), main = "CART ROC Curve")
#Our accuracy is actually a little bit worse then our CART tree built with an optimal cp value.  We also have a much worse AUC compared to the logisitic model and CART model.  We could likely increase accuracy by amending the nodesize and ntree parameters
CVtab <- table(test$opened, predCV > .2)
accCV <- (sum(diag(CVtab)))/sum(CVtab)
RFtab <- table(test$opened, predforest > .2)
accRF <- sum(diag(RFtab))/sum(RFtab)
