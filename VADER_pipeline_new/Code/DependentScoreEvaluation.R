#
#
# Associations between dependent measured scores and self reportes symptom-based
#
# Developed by Saeid Parvandeh Oct/2017
# 
# ----------------------------------------------------------
# Read csv self repoted file
rm(list=ls())
setwd("E:/LIBR/Hamed/new version/Data")

# load data
load("case.control_stat.RData")
load("self_reports_scores.RData")
load("narratives_notes.RData")
load("sentiment_scores.RData")


# Mood and Anxiety
case.control_stat_MA <- case.control_stat[which(case.control_stat$GroupAssignment=="Mood/Anxiety"), ]
SelfReport_MA <- self_reports_scores[which(case.control_stat$GroupAssignment=="Mood/Anxiety"), ]
dim(SelfReport_MA)
narratives_notes_MA <- narratives_notes[which(narratives_notes$id%in%SelfReport_MA$id), ]
MeasrdScore_MA <- sentiment_scores[which(sentiment_scores$id%in%SelfReport_MA$id), ]

# histogram of dependent scores:
par(mfrow=c(2, 2))
hist(MeasrdScore_MA$pos, col = "blue", main = "Histogram of positive scores",     xlab = "positive scores", breaks = 50, prob = T)
curve(dnorm(x, mean=mean(MeasrdScore_MA$pos), sd=sd(MeasrdScore_MA$pos)), add = TRUE)
hist(MeasrdScore_MA$neg, col = "blue", main = "Histogram of negative scores",     xlab = "negative scores", breaks = 50, prob = T)
curve(dnorm(x, mean=mean(MeasrdScore_MA$neg), sd=sd(MeasrdScore_MA$neg)), add = TRUE)
hist(MeasrdScore_MA$obj, col = "blue", main = "Histogram of objectivity scores",     xlab = "objectivity scores", breaks = 50, prob = T)
curve(dnorm(x, mean=mean(MeasrdScore_MA$obj), sd=sd(MeasrdScore_MA$obj)), add = TRUE)
hist(MeasrdScore_MA$pol, col = "blue", main = "Histogram of polarity scores",     xlab = "polarity scores", breaks = 50, prob = T)
curve(dnorm(x, mean=mean(MeasrdScore_MA$pol), sd=sd(MeasrdScore_MA$pol)), add = TRUE)

# Correlation between dependent scores
library(psych)
# sum of score
pos.score <- MeasrdScore_MA$pos
neg.score <- MeasrdScore_MA$neg
obj.score <- MeasrdScore_MA$obj
pol.score <- MeasrdScore_MA$pol
tot.words <- MeasrdScore_MA$total_words
tot.sen   <- MeasrdScore_MA$total_sen
pairs.panels(data.frame(pos.score, neg.score, obj.score, pol.score, tot.words, tot.sen), main = "regular score")

pairs.panels(data.frame(pos.score, neg.score, obj.score, pol.score, tot.words), main = "regular score")

# Extract Age, Gender, Income, and Education
# mood/anxiety
Age_MA    <- case.control_stat_MA[, grep("Age", names(SelfReport_MA), fixed = T)]
Gender_MA <- as.numeric(case.control_stat_MA[, grep("Gender", names(SelfReport_MA), fixed = T)])
Edu_MA    <- case.control_stat_MA[, grep("Education", names(SelfReport_MA), fixed = T)]
Income_MA <- log2(case.control_stat_MA[, grep("Income", names(SelfReport_MA), fixed = T)])
Income_MA <- replace(Income_MA, which(Income_MA=="-Inf"), NA)
LC_MA <- as.numeric(narratives_notes_MA$LC_conducted_by)

# Interviewers 
lc_ma_fit <- lm(LC_MA ~ tot.words+pos.score+neg.score+obj.score+pol.score+tot.sen)
anova(lc_ma_fit)[1:6, c(1, 5)]

#	Determine the degree to which socio-demographic characteristics effect the dependent measures
# ----- mood/anxiety group -----
# Positive score as dependent measure
par(mfrow = c(4, 4))
lm.fit_age <- lm(pos.score ~ Age_MA)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_MA, pos.score, main = "age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "Positive score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(pos.score ~ Gender_MA)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_MA, pos.score, main = "gender", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "Positive score")
abline(lm.fit_gender, col = "red")


lm.fit_edu <- lm(pos.score ~ Edu_MA)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_MA, pos.score, main = "education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "Positive score")
abline(lm.fit_edu, col = "red")


lm.fit_inc <- lm(pos.score ~ Income_MA)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_MA, pos.score, main = "income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "Positive score")
abline(lm.fit_inc, col = "red")

# Negetive score  as dependent measure
lm.fit_age <- lm(neg.score ~ Age_MA)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_MA, neg.score, main = "age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "Negative score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(neg.score ~ Gender_MA)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_MA, neg.score, main = "gender", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "Negative score")
abline(lm.fit_gender, col = "red")

lm.fit_edu <- lm(neg.score ~ Edu_MA)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_MA, neg.score, main = "education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "Negative score")
abline(lm.fit_edu, col = "red")

lm.fit_inc <- lm(neg.score ~ Income_MA)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_MA, neg.score, main = "income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "Negative score")
abline(lm.fit_inc, col = "red")

# Subjectivity score  as dependent measure
lm.fit_age <- lm(obj.score ~ Age_MA)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_MA, obj.score, main = "age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(obj.score ~ Gender_MA)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_MA, obj.score, main = "gender ", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_gender, col = "red")

lm.fit_edu <- lm(obj.score ~ Edu_MA)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_MA, obj.score, main = "education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_edu, col = "red")

lm.fit_inc <- lm(obj.score ~ Income_MA)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_MA, obj.score, main = "income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_inc, col = "red")

# Polarity score  as dependent measure
lm.fit_age <- lm(pol.score ~ Age_MA)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_MA, pol.score, main = "age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "Polarity score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(pol.score ~ Gender_MA)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_MA, pol.score, main = "gender ", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "Polarity score")
abline(lm.fit_gender, col = "red")

lm.fit_edu <- lm(pol.score ~ Edu_MA)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_MA, pol.score, main = "education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "Polarity score")
abline(lm.fit_edu, col = "red")

lm.fit_inc <- lm(pol.score ~ Income_MA)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_MA, pol.score, main = "income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "Polarity score")
abline(lm.fit_inc, col = "red")

# Determine the relationship with respect to the following trait variables:
# (PANAS-X, BIF, Symptom scales PHQ-9) 
# mood/anxiety group
PANASX_pos_MA <- SelfReport_MA[ ,grepl("PANASX_PosAffect", names(SelfReport_MA))]
PANASX_neg_MA <- SelfReport_MA[ ,grepl("PANASX_NegAffect", names(SelfReport_MA))]
PHQ_MA <- narratives_notes_MA$PHQ

# Positive score as dependent measure
par(mfcol = c(3, 4))
lm_fit_PAN.pos <- lm(pos.score ~ PANASX_pos_MA)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_MA, pos.score, main = "PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(pos.score ~ PANASX_neg_MA)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_MA, pos.score, main = "PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(pos.score ~ PHQ_MA)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_MA, pos.score, main = "PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PHQ, col = "red")

# Negative score as dependent measure
lm_fit_PAN.pos <- lm(neg.score ~ PANASX_pos_MA)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_MA, neg.score, main = "PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(neg.score ~ PANASX_neg_MA)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_MA, neg.score, main = "PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(neg.score ~ PHQ_MA)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_MA, neg.score, main = "PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PHQ, col = "red")

# Subjectivity score as dependent measure
lm_fit_PAN.pos <- lm(obj.score ~ PANASX_pos_MA)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_MA, obj.score, main = "PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(obj.score ~ PANASX_neg_MA)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_MA, obj.score, main = "PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(obj.score ~ PHQ_MA)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_MA, obj.score, main = "PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PHQ, col = "red")

# Polarity score as dependent measure
lm_fit_PAN.pos <- lm(pol.score ~ PANASX_pos_MA)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_MA, pol.score, main = "PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(pol.score ~ PANASX_neg_MA)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_MA, pol.score, main = "PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(pol.score ~ PHQ_MA)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_MA, pol.score, main = "PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PHQ, col = "red")

# mood and anxiety
BFI_con_MA <- SelfReport_MA[ ,grepl("BFI_Conscious", names(SelfReport_MA))]
BFI_open_MA <- SelfReport_MA[ ,grepl("BFI_Openness", names(SelfReport_MA))]
BFI_agree_MA <- SelfReport_MA[ ,grepl("BFI_Agreeb", names(SelfReport_MA))]
BFI_extrav_MA <- SelfReport_MA[ ,grepl("BFI_Extrav", names(SelfReport_MA))]
BFI_neurot_MA <- SelfReport_MA[ ,grepl("BFI_Neurot", names(SelfReport_MA))]

# Positive score as dependent measure
par(mfrow = c(4, 5))
lm_fit_BFI.con <- lm(pos.score ~ BFI_con_MA)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_MA, pos.score, main = "BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(pos.score ~ BFI_open_MA)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_MA, pos.score, main = "BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(pos.score ~ BFI_agree_MA)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_MA, pos.score, main = "BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(pos.score ~ BFI_extrav_MA)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_MA, pos.score, main = "BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(pos.score ~ BFI_neurot_MA)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_MA, pos.score, main = "BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.neurot, col = "red")

# Negative score as dependent measure
lm_fit_BFI.con <- lm(neg.score ~ BFI_con_MA)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_MA, neg.score, main = "BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(neg.score ~ BFI_open_MA)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_MA, neg.score, main = "BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(neg.score ~ BFI_agree_MA)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_MA, neg.score, main = "BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(neg.score ~ BFI_extrav_MA)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_MA, neg.score, main = "BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(neg.score ~ BFI_neurot_MA)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_MA, neg.score, main = "BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.neurot, col = "red")

# Subjectivity score as dependent measure
lm_fit_BFI.con <- lm(obj.score ~ BFI_con_MA)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_MA, obj.score, main = "BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(obj.score ~ BFI_open_MA)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_MA, obj.score, main = "BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(obj.score ~ BFI_agree_MA)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_MA, obj.score, main = "BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(obj.score ~ BFI_extrav_MA)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_MA, obj.score, main = "BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(obj.score ~ BFI_neurot_MA)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_MA, obj.score, main = "BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.neurot, col = "red")

# Polarity score as dependent measure
lm_fit_BFI.con <- lm(pol.score ~ BFI_con_MA)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_MA, pol.score, main = "BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(pol.score ~ BFI_open_MA)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_MA, pol.score, main = "BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(pol.score ~ BFI_agree_MA)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_MA, pol.score, main = "BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(pol.score ~ BFI_extrav_MA)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_MA, pol.score, main = "BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(pol.score ~ BFI_neurot_MA)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_MA, pol.score, main = "BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_BFI.neurot, col = "red")

# Determine the relationship with respect to the following trait variables:
# (Symptom scales OASIS), PROMIS (anxiety, depression, anger), Interviewers)
# mood and anxiety group
OASIS_MA <- case.control_stat_MA$OASIS
PROMIS.Ang_MA <- SelfReport_MA[ ,grepl("PROMIS_AngerTscore", names(SelfReport_MA))]
PROMIS.Dep_MA <- SelfReport_MA[ , grepl("PROMIS_DepressTscore", names(SelfReport_MA))]
PROMIS.Anx_MA <- SelfReport_MA[ , grepl("PROMIS_AnxietyTscore", names(SelfReport_MA))]

# Positive score as dependent measure
par(mfrow = c(4, 4))
lm_fit_OASIS <- lm(pos.score ~ OASIS_MA)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_MA, pos.score, main = "OASIS", xlab =c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(pos.score ~ PROMIS.Ang_MA)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_MA, pos.score, main = "PROMIS Anger", xlab = c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(pos.score ~ PROMIS.Dep_MA)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_MA, pos.score, main = "PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(pos.score ~ PROMIS.Anx_MA)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_MA, pos.score, main = "PROMIS Anxiety", xlab =c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Anx, col = "red")

# Negative score as dependent measure
lm_fit_OASIS <- lm(neg.score ~ OASIS_MA)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_MA, neg.score, main = "OASIS", xlab = c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(neg.score ~ PROMIS.Ang_MA)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_MA, neg.score, main = "PROMIS Anger", xlab = c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(neg.score ~ PROMIS.Dep_MA)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_MA, neg.score, main = "PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(neg.score ~ PROMIS.Anx_MA)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_MA, neg.score, main = "PROMIS Anxiety", xlab = c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Anx, col = "red")

# Subjectivity score as dependent measure
lm_fit_OASIS <- lm(obj.score ~ OASIS_MA)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_MA, obj.score, main = "OASIS", xlab = c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(obj.score ~ PROMIS.Ang_MA)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_MA, obj.score, main = "PROMIS Anger", xlab =c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(obj.score ~ PROMIS.Dep_MA)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_MA, obj.score, main = "PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(obj.score ~ PROMIS.Anx_MA)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_MA, obj.score, main = "PROMIS Anxiety", xlab = c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Anx, col = "red")

# Polarity score as dependent measure
lm_fit_OASIS <- lm(pol.score ~ OASIS_MA)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_MA, pol.score, main = "OASIS", xlab = c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(pol.score ~ PROMIS.Ang_MA)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_MA, pol.score, main = "PROMIS Anger", xlab =c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(pol.score ~ PROMIS.Dep_MA)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_MA, pol.score, main = "PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(pol.score ~ PROMIS.Anx_MA)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_MA, pol.score, main = "PROMIS Anxiety", xlab = c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_PROMIS.Anx, col = "red")

# Determine mediating influences (CTQ, TEC)on dependent measured scores
# mood/anxiety
CTQ_MA <- SelfReport_MA[ ,grepl("CTQ_score", names(SelfReport_MA))]
TES_MA <- SelfReport_MA[ ,grepl("TES_TotalOccurrence", names(SelfReport_MA))]
DAST_MA <- case.control_stat_MA[ ,grepl("DAST", names(SelfReport_MA))]

# mood and anxiety
# Positive score as dependent measure
par(mfcol = c(3, 4))
lm_fit_CTQ <- lm(pos.score ~ CTQ_MA)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_MA, pos.score, main = "CTQ", xlab = c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(pos.score ~ TES_MA)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_MA, pos.score, main = "TES", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(pos.score ~ DAST_MA)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_MA, pos.score, main = "DAST", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_DAST, col = "red")

# Negative score as dependent measure
lm_fit_CTQ <- lm(neg.score ~ CTQ_MA)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_MA, neg.score, main = "CTQ", xlab =c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(neg.score ~ TES_MA)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_MA, neg.score, main = "TES", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(neg.score ~ DAST_MA)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_MA, neg.score, main = "DAST", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_DAST, col = "red")

# Subjectivity score as dependent measure
lm_fit_CTQ <- lm(obj.score ~ CTQ_MA)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_MA, obj.score, main = "CTQ", xlab = c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(obj.score ~ TES_MA)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_MA, obj.score, main = "TES", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(obj.score ~ DAST_MA)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_MA, obj.score, main = "DAST", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_DAST, col = "red")

# Polarity score as dependent measure
lm_fit_CTQ <- lm(pol.score ~ CTQ_MA)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_MA, pol.score, main = "CTQ", xlab = c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(pol.score ~ TES_MA)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_MA, pol.score, main = "TES", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(pol.score ~ DAST_MA)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_MA, pol.score, main = "DAST", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Polarity score")
abline(lm_fit_DAST, col = "red")

