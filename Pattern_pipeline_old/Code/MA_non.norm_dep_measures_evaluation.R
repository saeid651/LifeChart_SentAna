#
#
# Associations between dependent measured scores and self reportes symptom-based
#
# The dependent measured scores did not adjust.
#
# Developed by Saeid Parvandeh Feb/2017
# 
# ----------------------------------------------------------
# Read csv self repoted file
rm(list=ls())
setwd("E:/LIBR/Data")

# load data
load("case.control_stat.RData")
load("self_reports_scores.RData")
load("narratives_notes.RData")
load("dep_measrd_scores.RData")


# Mood and Anxiety
case.control_stat_M.A <- case.control_stat[which(case.control_stat$GroupAssignment=="Mood/Anxiety"), ]
SelfReport_M.A <- self_reports_scores[which(case.control_stat$GroupAssignment=="Mood/Anxiety"), ]
dim(SelfReport_M.A)
narratives_notes_M.A <- narratives_notes[which(narratives_notes$id%in%SelfReport_M.A$id), ]
MeasrdScore_M.A <- dep_measrd_scores[which(dep_measrd_scores$id%in%SelfReport_M.A$id), ]

# Comparing the number of total words in mood/anxiety:
hist(MeasrdScore_M.A$total_words, col = "blue", main = "Histogram of total number of words",
     xlab = "Number of words", ylim = c(0, 60), xlim = c(0, 2500))

# Association of total number of words and adjective words with PHQ, OASIS, AND CTQ
# mood and anxiety
total_words_M.A <- MeasrdScore_M.A$total_words
adj_words_M.A <- MeasrdScore_M.A$X..sub_words
pos_words_M.A <- MeasrdScore_M.A$X..pos.words
neg_words_M.A <- MeasrdScore_M.A$X..neg.words
PHQ_M.A <- narratives_notes_M.A$PHQ
OASIS_M.A <- narratives_notes_M.A$OASIS
CTQ_M.A <- narratives_notes_M.A$CTQ_score

par(mfrow = c(2, 3))
lm.fit_PHQ <- lm(total_words_M.A ~ PHQ_M.A)
plot(PHQ_M.A, total_words_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Total words")
abline(lm.fit_PHQ, col = "red")
PHQ_pval <- summary(lm.fit_PHQ)$coefficients[2,4]
legend(-3, 2000, legend = c("p-value", round(PHQ_pval, digits = 4)), cex = .9, bty = "n")

lm.fit_OASIS <- lm(total_words_M.A ~ OASIS_M.A)
plot(OASIS_M.A, total_words_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Total words")
abline(lm.fit_OASIS, col = "red")
OASIS_pval <- summary(lm.fit_OASIS)$coefficients[2,4]
legend(-1, 2000, legend = c("p-value", round(OASIS_pval, digits = 4)), cex = .9, bty = "n")

lm.fit_CTQ <- lm(total_words_M.A ~ CTQ_M.A)
plot(CTQ_M.A, total_words_M.A, main = "effect of CTQ", xlab = "CTQ", ylab = "Total words")
abline(lm.fit_CTQ, col = "red")
CTQ_pval <- summary(lm.fit_CTQ)$coefficients[2,4]
legend(15, 2000, legend = c("p-value", round(CTQ_pval, digits = 4)), cex = .9, bty = "n")

lm.fit_PHQ <- lm(adj_words_M.A ~ PHQ_M.A)
plot(PHQ_M.A, adj_words_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Adjective words")
abline(lm.fit_PHQ, col = "red")
PHQ_pval <- summary(lm.fit_PHQ)$coefficients[2,4]
legend(-3, 85, legend = c("p-value", round(PHQ_pval, digits = 4)), cex = .9, bty = "n")

lm.fit_OASIS <- lm(adj_words_M.A ~ OASIS_M.A)
plot(OASIS_M.A, adj_words_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Adjective words")
abline(lm.fit_OASIS, col = "red")
OASIS_pval <- summary(lm.fit_OASIS)$coefficients[2,4]
legend(-1, 85, legend = c("p-value", round(OASIS_pval, digits = 4)), cex = .9, bty = "n")

lm.fit_CTQ <- lm(adj_words_M.A ~ CTQ_M.A)
plot(CTQ_M.A, adj_words_M.A, main = "effect of CTQ", xlab = "CTQ", ylab = "Adjective words")
abline(lm.fit_CTQ, col = "red")
CTQ_pval <- summary(lm.fit_CTQ)$coefficients[2,4]
legend(15, 85, legend = c("p-value", round(CTQ_pval, digits = 4)), cex = .9, bty = "n")


# Association of a random subject's dependent measure scores and corresponding narrative note
# Mood and anxiety
rand_subject <- 7
cat("A random narrative note:\n\n" ,as.character(narratives_notes_M.A[rand_subject, 2])) 

temp_M.A <- MeasrdScore_M.A[rand_subject, ]
cat("Subjectivity Score:", temp_M.A$sub_score, temp_M.A$sub_score_norm, temp_M.A$sub_score_totalNorm,"\n\nSubjectivity Words:", as.character(temp_M.A$sub_words))

cat("Positive Score:", temp_M.A$pos_score, temp_M.A$pos_score_norm, temp_M.A$pos_score_totalNorm,"\n\nPositive Words:", as.character(temp_M.A$pos_words))

cat("Negative Score:", temp_M.A$neg_score, temp_M.A$neg_score_norm, temp_M.A$neg_score_totalNorm,"\n\nNegative Words:", as.character(temp_M.A$neg_words))


# Distributional characteristics in mood/anxiety group
par(mfrow = c(3, 2))
# subjectivity score
hist(MeasrdScore_M.A$sub_score, main= "Histogram of Subjectivity Scores",xlab="Subjectivity Scores", ylim = c(0, 100))
hist(MeasrdScore_M.A$sub_score, prob = T, main = "Distributional Characteristics", xlab="Subjectivity Scores", ylim = c(0, .12))
curve(dnorm(x, mean=mean(MeasrdScore_M.A$sub_score), sd=sd(MeasrdScore_M.A$sub_score)), add = TRUE)
abline(v = temp_M.A$sub_score, col = "red", lwd = 2)
# positive score
hist(MeasrdScore_M.A$pos_score, main = "Histogram of Positive Scores" ,xlab="Positive Score", ylim = c(0, 60))
hist(MeasrdScore_M.A$pos_score, prob = T, main = "Distributional Characteristics", xlab="Positive Scores", ylim = c(0, .4))
curve(dnorm(x, mean=mean(MeasrdScore_M.A$pos_score), sd=sd(MeasrdScore_M.A$pos_score)), add = TRUE)
abline(v = temp_M.A$pos_score, col = "red", lwd = 2)
# negative score
hist(MeasrdScore_M.A$neg_score, main= "Histogram of Negative Scores",xlab = "Negative Score", ylim = c(0, 80))
hist(MeasrdScore_M.A$neg_score, prob = T, main = "Distributional Characteristics", xlab = "Negative Scores", ylim = c(0, .4))
curve(dnorm(x, mean=mean(MeasrdScore_M.A$neg_score), sd=sd(MeasrdScore_M.A$neg_score)), add = TRUE)
abline(v = temp_M.A$neg_score, col = "red", lwd = 2)


# Correlation between dependend measure scores
library(psych)
# sum of score
Sub.score <- MeasrdScore_M.A$sub_score
Pos.score <- MeasrdScore_M.A$pos_score
Neg.score <- MeasrdScore_M.A$neg_score
pairs.panels(data.frame(Pos.score, Neg.score, Sub.score), main = "regular score")

# Extract Age, Gender, Income, and Education
# mood/anxiety
Age_M.A    <- case.control_stat_M.A[, grep("Age", names(SelfReport_M.A), fixed = T)]
Gender_M.A <- as.numeric(case.control_stat_M.A[, grep("Gender", names(SelfReport_M.A), fixed = T)])
Edu_M.A    <- case.control_stat_M.A[, grep("Education", names(SelfReport_M.A), fixed = T)]
Income_M.A <- log2(case.control_stat_M.A[, grep("Income", names(SelfReport_M.A), fixed = T)])
Income_M.A <- replace(Income_M.A, which(Income_M.A=="-Inf"), NA)

#	Determine the degree to which socio-demographic characteristics effect the dependent measures
# ----- mood/anxiety group -----
# Subjectivity score as dependent measure
par(mfrow = c(3, 4))
lm.fit_age <- lm(Sub.score ~ Age_M.A)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_M.A, Sub.score, main = "effect of age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(Sub.score ~ Gender_M.A)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_M.A, Sub.score, main = "effect of gender", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_gender, col = "red")


lm.fit_edu <- lm(Sub.score ~ Edu_M.A)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_M.A, Sub.score, main = "effect of education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_edu, col = "red")


lm.fit_inc <- lm(Sub.score ~ Income_M.A)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_M.A, Sub.score, main = "effect of income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm.fit_inc, col = "red")

# Positive score  as dependent measure
lm.fit_age <- lm(Pos.score ~ Age_M.A)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_M.A, Pos.score, main = "effect of age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "positive score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(Pos.score ~ Gender_M.A)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_M.A, Pos.score, main = "effect of gender", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "positive score")
abline(lm.fit_gender, col = "red")

lm.fit_edu <- lm(Pos.score ~ Edu_M.A)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_M.A, Pos.score, main = "effect of education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "positive score")
abline(lm.fit_edu, col = "red")

lm.fit_inc <- lm(Pos.score ~ Income_M.A)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_M.A, Pos.score, main = "effect of income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "positive score")
abline(lm.fit_inc, col = "red")

# Negative score  as dependent measure
lm.fit_age <- lm(Neg.score ~ Age_M.A)
age_pval <- summary(lm.fit_age)$coefficients[2,4]
plot(Age_M.A, Neg.score, main = "effect of age", xlab = c("p-value" ,round(age_pval, digits = 4)), ylab = "negative score")
abline(lm.fit_age, col = "red")

lm.fit_gender <- lm(Neg.score ~ Gender_M.A)
gender_pval <- summary(lm.fit_gender)$coefficients[2,4]
plot(Gender_M.A, Neg.score, main = "effect of gender ", xlab = c("p-value" ,round(gender_pval, digits = 4)), ylab = "negative score")
abline(lm.fit_gender, col = "red")

lm.fit_edu <- lm(Neg.score ~ Edu_M.A)
edu_pval <- summary(lm.fit_edu)$coefficients[2,4]
plot(Edu_M.A, Neg.score, main = "effect of education", xlab = c("p-value" ,round(edu_pval, digits = 4)), ylab = "negative score")
abline(lm.fit_edu, col = "red")

lm.fit_inc <- lm(Neg.score ~ Income_M.A)
inc_pval <- summary(lm.fit_inc)$coefficients[2,4]
plot(Income_M.A, Neg.score, main = "effect of income", xlab = c("p-value" ,round(inc_pval, digits = 4)), ylab = "negative score")
abline(lm.fit_inc, col = "red")

# Determine the relationship with respect to the following trait variables:
# (PANAS-X, BIF, Symptom scales PHQ-9) 
# mood/anxiety group
PANASX_pos_M.A <- SelfReport_M.A[ ,grepl("PANASX_PosAffect", names(SelfReport_M.A))]
PANASX_neg_M.A <- SelfReport_M.A[ ,grepl("PANASX_NegAffect", names(SelfReport_M.A))]
PHQ_M.A <- narratives_notes_M.A$PHQ

# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_PAN.pos <- lm(Sub.score ~ PANASX_pos_M.A)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_M.A, Sub.score, main = "effect of PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(Sub.score ~ PANASX_neg_M.A)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_M.A, Sub.score, main = "effect of PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(Sub.score ~ PHQ_M.A)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_M.A, Sub.score, main = "effect of PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PHQ, col = "red")

# Positive score as dependent measure
lm_fit_PAN.pos <- lm(Pos.score ~ PANASX_pos_M.A)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_M.A, Pos.score, main = "effect of PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(Pos.score ~ PANASX_neg_M.A)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_M.A, Pos.score, main = "effect of PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(Pos.score ~ PHQ_M.A)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_M.A, Pos.score, main = "effect of PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PHQ, col = "red")

# Negative score as dependent measure
lm_fit_PAN.pos <- lm(Neg.score ~ PANASX_pos_M.A)
PAN.pos_pval <- summary(lm_fit_PAN.pos)$coefficients[2,4]
plot(PANASX_pos_M.A, Neg.score, main = "effect of PANASX positive", xlab = c("p-value" ,round(PAN.pos_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PAN.pos, col = "red")

lm_fit_PAN.neg <- lm(Neg.score ~ PANASX_neg_M.A)
PAN.neg_pval <- summary(lm_fit_PAN.neg)$coefficients[2,4]
plot(PANASX_neg_M.A, Neg.score, main = "effect of PANASX negative", xlab = c("p-value" ,round(PAN.neg_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PAN.neg, col = "red")

lm_fit_PHQ <- lm(Neg.score ~ PHQ_M.A)
PHQ_pval <- summary(lm_fit_PHQ)$coefficients[2,4]
plot(PHQ_M.A, Neg.score, main = "effect of PHQ", xlab = c("p-value" ,round(PHQ_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PHQ, col = "red")

# mood and anxiety
BFI_con_M.A <- SelfReport_M.A[ ,grepl("BFI_Conscious", names(SelfReport_M.A))]
BFI_open_M.A <- SelfReport_M.A[ ,grepl("BFI_Openness", names(SelfReport_M.A))]
BFI_agree_M.A <- SelfReport_M.A[ ,grepl("BFI_Agreeb", names(SelfReport_M.A))]
BFI_extrav_M.A <- SelfReport_M.A[ ,grepl("BFI_Extrav", names(SelfReport_M.A))]
BFI_neurot_M.A <- SelfReport_M.A[ ,grepl("BFI_Neurot", names(SelfReport_M.A))]

# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_BFI.con <- lm(Sub.score ~ BFI_con_M.A)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_M.A, Sub.score, main = "effect of BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(Sub.score ~ BFI_open_M.A)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_M.A, Sub.score, main = "effect of BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(Sub.score ~ BFI_agree_M.A)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_M.A, Sub.score, main = "effect of BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(Sub.score ~ BFI_extrav_M.A)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_M.A, Sub.score, main = "effect of BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(Sub.score ~ BFI_neurot_M.A)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_M.A, Sub.score, main = "effect of BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_BFI.neurot, col = "red")

# Positive score as dependent measure
lm_fit_BFI.con <- lm(Pos.score ~ BFI_con_M.A)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_M.A, Pos.score, main = "effect of BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(Pos.score ~ BFI_open_M.A)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_M.A, Pos.score, main = "effect of BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(Pos.score ~ BFI_agree_M.A)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_M.A, Pos.score, main = "effect of BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(Pos.score ~ BFI_extrav_M.A)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_M.A, Pos.score, main = "effect of BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(Pos.score ~ BFI_neurot_M.A)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_M.A, Pos.score, main = "effect of BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_BFI.neurot, col = "red")

# Negative score as dependent measure
lm_fit_BFI.con <- lm(Neg.score ~ BFI_con_M.A)
BFI.con_pval <- summary(lm_fit_BFI.con)$coefficients[2,4]
plot(BFI_con_M.A, Neg.score, main = "effect of BFI conscious", xlab = c("p-value" ,round(BFI.con_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.con, col = "red")

lm_fit_BFI.open <- lm(Neg.score ~ BFI_open_M.A)
BFI.open_pval <- summary(lm_fit_BFI.open)$coefficients[2,4]
plot(BFI_open_M.A, Neg.score, main = "effect of BFI openness", xlab = c("p-value" ,round(BFI.open_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.open, col = "red")

lm_fit_BFI.agree <- lm(Neg.score ~ BFI_agree_M.A)
BFI.agree_pval <- summary(lm_fit_BFI.agree)$coefficients[2,4]
plot(BFI_agree_M.A, Neg.score, main = "effect of BFI Agreeb", xlab = c("p-value" ,round(BFI.agree_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.agree, col = "red")

lm_fit_BFI.extrav <- lm(Neg.score ~ BFI_extrav_M.A)
BFI.extrav_pval <- summary(lm_fit_BFI.extrav)$coefficients[2,4]
plot(BFI_extrav_M.A, Neg.score, main = "effect of BFI Extrav", xlab = c("p-value" ,round(BFI.extrav_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.extrav, col = "red")

lm_fit_BFI.neurot <- lm(Neg.score ~ BFI_neurot_M.A)
BFI.neurot_pval <- summary(lm_fit_BFI.neurot)$coefficients[2,4]
plot(BFI_neurot_M.A, Neg.score, main = "effect of BFI Neurot", xlab = c("p-value" ,round(BFI.neurot_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_BFI.neurot, col = "red")

# Determine the relationship with respect to the following trait variables:
# (Symptom scales OASIS), PROMIS (anxiety, depression, anger), Interviewers)
# mood and anxiety group
OASIS_M.A <- case.control_stat_M.A$OASIS
PROMIS.Ang_M.A <- SelfReport_M.A[ ,grepl("PROMIS_AngerTscore", names(SelfReport_M.A))]
PROMIS.Dep_M.A <- SelfReport_M.A[ , grepl("PROMIS_DepressTscore", names(SelfReport_M.A))]
PROMIS.Anx_M.A <- SelfReport_M.A[ , grepl("PROMIS_AnxietyTscore", names(SelfReport_M.A))]
LC_M.A <- as.numeric(narratives_notes_M.A$LC_conducted_by)

# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_OASIS <- lm(Sub.score ~ OASIS_M.A)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_M.A, Sub.score, main = "effect of OASIS", xlab =c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(Sub.score ~ PROMIS.Ang_M.A)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_M.A, Sub.score, main = "effect of PROMIS Anger", xlab = c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(Sub.score ~ PROMIS.Dep_M.A)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_M.A, Sub.score, main = "effect of PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(Sub.score ~ PROMIS.Anx_M.A)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_M.A, Sub.score, main = "effect of PROMIS Anxiety", xlab =c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Anx, col = "red")

lm_fit_LC <- lm(Sub.score ~ LC_M.A)
LC_pval <- summary(lm_fit_LC)$coefficients[2,4]
sub_LC <- data.frame(Sub.score,LC_M.A)
boxplot(Sub.score~LC_M.A, data= sub_LC,main = "effect of LC", xlab = c("p-value" ,round(LC_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_LC, col = "red")

# Positive score as dependent measure
lm_fit_OASIS <- lm(Pos.score ~ OASIS_M.A)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_M.A, Pos.score, main = "effect of OASIS", xlab = c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(Pos.score ~ PROMIS.Ang_M.A)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_M.A, Pos.score, main = "effect of PROMIS Anger", xlab = c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(Pos.score ~ PROMIS.Dep_M.A)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_M.A, Pos.score, main = "effect of PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(Pos.score ~ PROMIS.Anx_M.A)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_M.A, Pos.score, main = "effect of PROMIS Anxiety", xlab = c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_PROMIS.Anx, col = "red")

lm_fit_LC <- lm(Pos.score ~ LC_M.A)
LC_pval <- summary(lm_fit_LC)$coefficients[2,4]
pos_LC <- data.frame(Pos.score, LC_M.A)
boxplot(Pos.score~LC_M.A, data= pos_LC, main = "effect of LC", xlab = c("p-value" ,round(LC_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_LC, col = "red")

# Negative score as dependent measure
lm_fit_OASIS <- lm(Neg.score ~ OASIS_M.A)
OASIS_pval <- summary(lm_fit_OASIS)$coefficients[2,4]
plot(OASIS_M.A, Neg.score, main = "effect of OASIS", xlab = c("p-value" ,round(OASIS_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_OASIS, col = "red")

lm_fit_PROMIS.Ang <- lm(Neg.score ~ PROMIS.Ang_M.A)
PROMIS.Ang_pval <- summary(lm_fit_PROMIS.Ang)$coefficients[2,4]
plot(PROMIS.Ang_M.A, Neg.score, main = "effect of PROMIS Anger", xlab =c("p-value" ,round(PROMIS.Ang_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Ang, col = "red")

lm_fit_PROMIS.Dep <- lm(Neg.score ~ PROMIS.Dep_M.A)
PROMIS.Dep_pval <- summary(lm_fit_PROMIS.Dep)$coefficients[2,4]
plot(PROMIS.Dep_M.A, Neg.score, main = "effect of PROMIS Depress", xlab = c("p-value" ,round(PROMIS.Dep_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Dep, col = "red")

lm_fit_PROMIS.Anx <- lm(Neg.score ~ PROMIS.Anx_M.A)
PROMIS.Anx_pval <- summary(lm_fit_PROMIS.Anx)$coefficients[2,4]
plot(PROMIS.Anx_M.A, Neg.score, main = "effect of PROMIS Anxiety", xlab = c("p-value" ,round(PROMIS.Anx_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_PROMIS.Anx, col = "red")

lm_fit_LC <- lm(Neg.score ~ LC_M.A)
neg_LC <- data.frame(Neg.score, LC_M.A)
boxplot(Neg.score~LC_M.A, data= neg_LC, main = "effect of LC", xlab = c("p-value" ,round(LC_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_LC, col = "red")

# Determine mediating influences (CTQ, TEC)on dependent measured scores
# mood/anxiety
CTQ_M.A <- SelfReport_M.A[ ,grepl("CTQ_score", names(SelfReport_M.A))]
TES_M.A <- SelfReport_M.A[ ,grepl("TES_TotalOccurrence", names(SelfReport_M.A))]
DAST_M.A <- case.control_stat_M.A[ ,grepl("DAST", names(SelfReport_M.A))]

# mood and anxiety
# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_CTQ <- lm(Sub.score ~ CTQ_M.A)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_M.A, Sub.score, main = "effect of CTQ (mood/anxiety)", xlab = c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(Sub.score ~ TES_M.A)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_M.A, Sub.score, main = "effect of TES (mood/anxiety)", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(Sub.score ~ DAST_M.A)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_M.A, Sub.score, main = "effect of DAST (mood/anxiety)", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Subjectivity score")
abline(lm_fit_DAST, col = "red")

# Positive score as dependent measure
lm_fit_CTQ <- lm(Pos.score ~ CTQ_M.A)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_M.A, Pos.score, main = "effect of CTQ (mood/anxiety)", xlab =c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(Pos.score ~ TES_M.A)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_M.A, Pos.score, main = "effect of TES (mood/anxiety)", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(Pos.score ~ DAST_M.A)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_M.A, Pos.score, main = "effect of DAST (mood/anxiety)", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Positive score")
abline(lm_fit_DAST, col = "red")

# Negative score as dependent measure
lm_fit_CTQ <- lm(Neg.score ~ CTQ_M.A)
CTQ_pval <- summary(lm_fit_CTQ)$coefficients[2,4]
plot(CTQ_M.A, Neg.score, main = "effect of CTQ (mood/anxiety)", xlab = c("p-value" ,round(CTQ_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_CTQ, col = "red")

lm_fit_TES <- lm(Neg.score ~ TES_M.A)
TES_pval <- summary(lm_fit_TES)$coefficients[2,4]
plot(TES_M.A, Neg.score, main = "effect of TES (mood/anxiety)", xlab = c("p-value" ,round(TES_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_TES, col = "red")

lm_fit_DAST <- lm(Neg.score ~ DAST_M.A)
DAST_pval <- summary(lm_fit_DAST)$coefficients[2,4]
plot(DAST_M.A, Neg.score, main = "effect of DAST (mood/anxiety)", xlab = c("p-value" ,round(DAST_pval, digits = 4)), ylab = "Negative score")
abline(lm_fit_DAST, col = "red")

# Severity of disorders between interviewers (PHQ, OASIS) if patient's scores are same, are the notes same lenghth
Severity_df <- cbind.data.frame(OASIS = OASIS_M.A, PHQ = PHQ_M.A, Total_words = total_words_M.A, Interviewer = as.character(narratives_notes_M.A$LC_conducted_by))

# Severity sorted by PHQ
Severity_sort.1 <- Severity_df[order(Severity_df[,2], decreasing = TRUE),]
# patients with same PHQ
head(Severity_sort.1, 15)

# Severity sorted by OASIS
Severity_sort.2 <- Severity_df[order(Severity_df[,1], decreasing = TRUE ),]
# patients with same OASIS
head(Severity_sort.2, 15)

# common OASIS and PHQ
Severity_df[which(Severity_df[,1]==Severity_df[,2]), ]

# concatenate valence words
pos_words <- gsub("[[:punct:]]", "", MeasrdScore_M.A$pos_words)
pos_words <- gsub('[0-9]+', '', pos_words)
neg_words <- gsub("[[:punct:]]", "", MeasrdScore_M.A$neg_words)
neg_words <- gsub('[0-9]+', '', neg_words)
sub_words <- gsub("[[:punct:]]", "", dep_measrd_scores$sub_words)
sub_words <- gsub('[0-9]+', '', sub_words) 
valence_words <- c(tolower(pos_words), tolower(neg_words), tolower(sub_words))
valence_words <- paste(valence_words, collapse = '')
valence_words <- strsplit(valence_words, " ")
# Top 200 words with highest frequency
head(sort(table(valence_words), decreasing = TRUE), 100)




