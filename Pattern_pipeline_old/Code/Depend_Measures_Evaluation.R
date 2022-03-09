#
#
# Associations between dependent measured scores and self reportes symptom-based
#
# Developed by Saeid Parvandeh Jan/2017
# 
# ----------------------------------------------------------
# Read csv self repoted file
rm(list=ls())
setwd("E:/LIBR/Hamed/old version/Data")

Dat1 <- read.csv("Dependent_Measured_Scores.csv")
Dat2 <- read.csv("T500_2017-03-18.csv")
Dat3 <- read.csv("T500_Lifechart_Narratives_Scores.csv")
common_ids <- intersect(intersect(as.vector(Dat1[, 1]), as.vector(Dat2[, 1])),Dat3[, 1])
length(common_ids)

case.control_stat <- Dat2[which(Dat2$visit=="S" & Dat2$id%in%common_ids), ]
save(case.control_stat, file = "case.control_stat.RData")
self_reports_scores <- Dat2[which(Dat2$visit=="T0" & Dat2$id%in%common_ids), ]
save(self_reports_scores, file = "self_reports_scores.RData")
narratives_notes <- Dat3[which(Dat3$id%in%common_ids), ]
save(narratives_notes, file = "narratives_notes.RData")
dep_measrd_scores <- Dat1[which(Dat1$id%in%common_ids), ]
save(dep_measrd_scores, file = "dep_measrd_scores.RData")

# Healthy Control, Mood/Anxiety, and Substance Use
case.control_stat_HC.MA.SU <- subset(case.control_stat, 
                                     case.control_stat$GroupAssignment== "Healthy Control" |
                                       case.control_stat$GroupAssignment=="Mood/Anxiety"| 
                                       case.control_stat$GroupAssignment=="Substance Use")
SelfReport_HC.MA.SU <- subset(self_reports_scores, 
                              case.control_stat$GroupAssignment== "Healthy Control" |
                                case.control_stat$GroupAssignment=="Mood/Anxiety"| 
                                case.control_stat$GroupAssignment=="Substance Use")
dim(SelfReport_HC.MA.SU)
narratives_notes_HC.MA.SU <- narratives_notes[which(narratives_notes$id%in%SelfReport_HC.MA.SU$id), ]
MeasrdScore_HC.MA.SU <- dep_measrd_scores[which(dep_measrd_scores$id%in%SelfReport_HC.MA.SU$id), ]
# write a csv file
write.csv(narratives_notes_HC.MA.SU, file = "E:/LIBR/Hamed/old version/Data/T500_ThreeGroups.csv", row.names = FALSE)


# Mood and Anxiety
case.control_stat_MA <- subset(case.control_stat, GroupAssignment=="Mood/Anxiety")
SelfReport_M.A <- subset(self_reports_scores, case.control_stat$GroupAssignment=="Mood/Anxiety")
dim(SelfReport_M.A)
narratives_notes_M.A <- narratives_notes[which(narratives_notes$id%in%SelfReport_M.A$id), ]
MeasrdScore_M.A <- dep_measrd_scores[which(dep_measrd_scores$id%in%SelfReport_M.A$id), ]

# Healthy Control
case.control_stat_HC <- subset(case.control_stat, GroupAssignment=="Healthy Control")
SelfReport_H <- subset(self_reports_scores, case.control_stat$GroupAssignment=="Healthy Control")
dim(SelfReport_H)
narratives_notes_H <- narratives_notes[which(narratives_notes$id%in%SelfReport_H$id), ]
MeasrdScore_H <- dep_measrd_scores[which(dep_measrd_scores$id%in%SelfReport_H$id), ]

# Comparing the number of total words in mood/anxiety and healthy/control groups
hist(MeasrdScore_M.A$total_words, col = "blue", main = "Histogram of total number of words",
     xlab = "Number of words")
hist(MeasrdScore_H$total_words, col = "purple", add = TRUE)
legend("topright", c("Mood and Anxiety", "Healthy Control"), col =c("blue","purple"), lwd = 10)

# Association of total number of words and adjective words with PHQ, OASIS, AND CTQ
# mood and anxiety
total_words_M.A <- MeasrdScore_M.A$total_words
adj_words_M.A <- MeasrdScore_M.A$X..sub_words
PHQ_M.A <- case.control_stat_MA$PHQ
OASIS_M.A <- case.control_stat_MA$OASIS
CTQ_M.A <- SelfReport_M.A$CTQ_score

par(mfrow = c(2, 3))
lm.fit_PHQ <- lm(total_words_M.A ~ PHQ_M.A)
plot(PHQ_M.A, total_words_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Total words")
abline(lm.fit_PHQ)

lm.fit_OASIS <- lm(total_words_M.A ~ OASIS_M.A)
plot(OASIS_M.A, total_words_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Total words")
abline(lm.fit_OASIS)

lm.fit_CTQ <- lm(total_words_M.A ~ CTQ_M.A)
plot(CTQ_M.A, total_words_M.A, main = "effect of CTQ", xlab = "CTQ", ylab = "Total words")
abline(lm.fit_CTQ)

lm.fit_PHQ <- lm(adj_words_M.A ~ PHQ_M.A)
plot(PHQ_M.A, adj_words_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Adjective words")
abline(lm.fit_PHQ)

lm.fit_OASIS <- lm(adj_words_M.A ~ OASIS_M.A)
plot(OASIS_M.A, adj_words_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Adjective words")
abline(lm.fit_OASIS)

lm.fit_CTQ <- lm(adj_words_M.A ~ CTQ_M.A)
plot(CTQ_M.A, adj_words_M.A, main = "effect of CTQ", xlab = "CTQ", ylab = "Adjective words")
abline(lm.fit_CTQ)

# healthy control
total_words_H <- MeasrdScore_H$total_words
adj_words_H <- MeasrdScore_H$X..sub_words
PHQ_H <- case.control_stat_HC$PHQ
OASIS_H <- case.control_stat_HC$OASIS
CTQ_H <- SelfReport_H$CTQ_score

par(mfrow = c(2, 3))
lm.fit_PHQ <- lm(total_words_H ~ PHQ_H)
plot(PHQ_H, total_words_H, main = "effect of PHQ", xlab = "PHQ", ylab = "Total words")
abline(lm.fit_PHQ)

lm.fit_OASIS <- lm(total_words_H ~ OASIS_H)
plot(OASIS_H, total_words_H, main = "effect of OASIS", xlab = "OASIS", ylab = "Total words")
abline(lm.fit_OASIS)

lm.fit_CTQ <- lm(total_words_H ~ CTQ_H)
plot(CTQ_H, total_words_H, main = "effect of CTQ", xlab = "CTQ", ylab = "Total words")
abline(lm.fit_CTQ)

lm.fit_PHQ <- lm(adj_words_H ~ PHQ_H)
plot(PHQ_H, adj_words_H, main = "effect of PHQ", xlab = "PHQ", ylab = "Adjective words")
abline(lm.fit_PHQ)

lm.fit_OASIS <- lm(adj_words_H ~ OASIS_H)
plot(OASIS_H, adj_words_H, main = "effect of OASIS", xlab = "OASIS", ylab = "Adjective words")
abline(lm.fit_OASIS)

lm.fit_CTQ <- lm(adj_words_H ~ CTQ_H)
plot(CTQ_H, adj_words_H, main = "effect of CTQ", xlab = "CTQ", ylab = "Adjective words")
abline(lm.fit_CTQ)

# Association of a random subject's dependent measure scores and corresponding narrative note
# Mood and anxiety
rand_subject <- 52
cat("A random narrative note:\n\n" ,as.character(narratives_notes_M.A[rand_subject, 2])) 

temp_M.A <- MeasrdScore_M.A[rand_subject, ]
cat("Subjectivity Score:", temp_M.A$sub_score, temp_M.A$sub_score_norm, temp_M.A$sub_score_totalNorm,"\n\nSubjectivity Words:", as.character(temp_M.A$sub_words))

cat("Positive Score:", temp_M.A$pos_score, temp_M.A$pos_score_norm, temp_M.A$pos_score_totalNorm,"\n\nPositive Words:", as.character(temp_M.A$pos_words))

cat("Negative Score:", temp_M.A$neg_score, temp_M.A$neg_score_norm, temp_M.A$neg_score_totalNorm,"\n\nNegative Words:", as.character(temp_M.A$neg_words))

# Association of a random subject's dependent measure scores and corresponding narrative note
# Healthy control
rand_subject <- 4
cat("A random narrative note:\n\n" ,as.character(narratives_notes_H[rand_subject, 2])) 

temp_H <- MeasrdScore_H[rand_subject, ]
cat("Subjectivity Score:", temp_H$sub_score,temp_H$sub_score_norm, temp_H$sub_score_totalNorm,"\n\nSubjectivity Words:", as.character(temp_H$sub_words))

cat("Positive Score:", temp_H$pos_score,temp_H$pos_score_norm, temp_H$pos_score_totalNorm, "\n\nPositive Words:", as.character(temp_H$pos_words))

cat("Negative Score:", temp_H$neg_score, temp_H$neg_score_norm, temp_H$neg_score_totalNorm, "\n\nNegative Words:", as.character(temp_H$neg_words))

# Distributional characteristics in mood/anxiety group
par(mfrow = c(3, 2))
# subjectivity score
hist(MeasrdScore_M.A$sub_score, main= "Histogram of Subjectivity Scores",xlab="Subjectivity Scores")
hist(MeasrdScore_M.A$sub_score, prob = T, main = "Distributional Characteristics", xlab="Subjectivity Scores")
curve(dnorm(x, mean=mean(MeasrdScore_M.A$sub_score), sd=sd(MeasrdScore_M.A$sub_score)), add = TRUE)
abline(v = temp_M.A$sub_score, col = "red", lwd = 2)
# positive score
hist(MeasrdScore_M.A$pos_score, main = "Histogram of Positive Scores" ,xlab="Positive Score")
hist(MeasrdScore_M.A$pos_score, prob = T, main = "Distributional Characteristics", xlab="Positive Scores")
curve(dnorm(x, mean=mean(MeasrdScore_M.A$pos_score), sd=sd(MeasrdScore_M.A$pos_score)), add = TRUE)
abline(v = temp_M.A$pos_score, col = "red", lwd = 2)
# negative score
hist(MeasrdScore_M.A$neg_score, main= "Histogram of Negative Scores",xlab = "Negative Score")
hist(MeasrdScore_M.A$neg_score, prob = T, main = "Distributional Characteristics", xlab = "Negative Scores")
curve(dnorm(x, mean=mean(MeasrdScore_M.A$neg_score), sd=sd(MeasrdScore_M.A$neg_score)), add = TRUE)
abline(v = temp_M.A$neg_score, col = "red", lwd = 2)

# Distributional characteristics in healthy control group
par(mfrow = c(3, 2))
# subjectivity score
hist(MeasrdScore_H$sub_score, main= "Histogram of Subjectivity Scores",xlab="Subjectivity Scores")
hist(MeasrdScore_H$sub_score, prob = T, main = "Distributional Characteristics", xlab="Subjectivity Scores")
curve(dnorm(x, mean=mean(MeasrdScore_H$sub_score), sd=sd(MeasrdScore_H$sub_score)), add = TRUE)
abline(v = temp_H$sub_score, col = "red", lwd = 2)
# positive score
hist(MeasrdScore_H$pos_score, main = "Histogram of Positive Scores" ,xlab="Positive Score")
hist(MeasrdScore_H$pos_score, prob = T, main = "Distributional Characteristics", xlab="Positive Scores")
curve(dnorm(x, mean=mean(MeasrdScore_H$pos_score), sd=sd(MeasrdScore_H$pos_score)), add = TRUE)
abline(v = temp_H$pos_score, col = "red", lwd = 2)
# negative score
hist(MeasrdScore_H$neg_score, main= "Histogram of Negative Scores",xlab = "Negative Score")
hist(MeasrdScore_H$neg_score, prob = T, main = "Distributional Characteristics", xlab = "Negative Scores")
curve(dnorm(x, mean=mean(MeasrdScore_H$neg_score), sd=sd(MeasrdScore_H$neg_score)), add = TRUE)
abline(v = temp_H$neg_score, col = "red", lwd = 2)


# Correlation between dependend measure scores
library(psych)
# mood/anxiety
pairs.panels(MeasrdScore_M.A[c(2, 7, 12)])
# healthy control
pairs.panels(MeasrdScore_H[c(2, 7, 12)])

# Extract Age, Gender, Incom, and Education
# mood/anxiety
Age_M.A    <- case.control_stat_MA[, grep("Age", names(case.control_stat_MA), fixed = T)]
Gender_M.A <- as.vector(case.control_stat_MA[, grep("Gender", names(case.control_stat_MA), fixed = T)])
Edu_M.A    <- case.control_stat_MA[, grep("Education", names(case.control_stat_MA), fixed = T)]
Income_M.A <- case.control_stat_MA[, grep("Income", names(case.control_stat_MA), fixed = T)]
# Assign dependent measures
Sub.score_M.A <- MeasrdScore_M.A$sub_score
Pos.score_M.A <- MeasrdScore_M.A$pos_score
Neg.score_M.A <- MeasrdScore_M.A$neg_score

# healthy control
Age_H    <- case.control_stat_HC[, grep("Age", names(case.control_stat_HC), fixed = T)]
Gender_H <- as.vector(case.control_stat_HC[, grep("Gender", names(case.control_stat_HC), fixed = T)])
Edu_H    <- case.control_stat_HC[, grep("Education", names(case.control_stat_HC), fixed = T)]
Income_H <- case.control_stat_HC[, grep("Income", names(case.control_stat_HC), fixed = T)]
# Assign dependent measures
Sub.score_H <- MeasrdScore_H$sub_score
Pos.score_H <- MeasrdScore_H$pos_score
Neg.score_H <- MeasrdScore_H$neg_score

#	Determine the degree to which socio-demographic characteristics effect the dependent measures
# ----- mood/anxiety group -----
# Subjectivity score as dependent measure
par(mfrow = c(3, 4))
lm.fit_age <- lm(Sub.score_M.A ~ Age_M.A)
plot(Age_M.A, Sub.score_M.A, main = "effect of age", xlab = "Age", ylab = "Subjectivity score")
abline(lm.fit_age)

gend_MA <- ifelse(Gender_M.A=="Male", 1, 0)
boxplot(gend_MA, Sub.score_M.A, main = "effect of gender", xlab = "Gender", ylab = "Subjectivity score")


lm.fit_edu <- lm(Sub.score_M.A ~ Edu_M.A)
plot(Edu_M.A, Sub.score_M.A, main = "effect of education", xlab = "Education", ylab = "Subjectivity score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Sub.score_M.A ~ Income_M.A)
plot(Income_M.A, Sub.score_M.A, main = "effect of income", xlab = "Income", ylab = "Subjectivity score")
abline(lm.fit_inc)

# Positive score  as dependent measure
lm.fit_age <- lm(Pos.score_M.A ~ Age_M.A)
plot(Age_M.A, Pos.score_M.A, main = "effect of age", xlab = "Age", ylab = "positive score")
abline(lm.fit_age)

boxplot(gend_MA, Pos.score_M.A, main = "effect of gender", xlab = "Gender", ylab = "positive score")


lm.fit_edu <- lm(Pos.score_M.A ~ Edu_M.A)
plot(Edu_M.A, Pos.score_M.A, main = "effect of education", xlab = "Education", ylab = "positive score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Pos.score_M.A ~ Income_M.A)
plot(Income_M.A, Pos.score_M.A, main = "effect of income", xlab = "Income", ylab = "positive score")
abline(lm.fit_inc)

# Negative score  as dependent measure
lm.fit_age <- lm(Neg.score_M.A ~ Age_M.A)
plot(Age_M.A, Neg.score_M.A, main = "effect of age", xlab = "Age", ylab = "negative score")
abline(lm.fit_age)


boxplot(gend_MA, Neg.score_M.A, main = "effect of gender ", xlab = "Gender", ylab = "negative score")


lm.fit_edu <- lm(Neg.score_M.A ~ Edu_M.A)
plot(Edu_M.A, Neg.score_M.A, main = "effect of education", xlab = "Education", ylab = "negative score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Neg.score_M.A ~ Income_M.A)
plot(Income_M.A, Neg.score_M.A, main = "effect of income", xlab = "Income", ylab = "negative score")
abline(lm.fit_inc)

# ----- healthy control group -----
# Subjectivity score as dependent measure
par(mfrow = c(3, 4))
lm.fit_age <- lm(Sub.score_H ~ Age_H)
plot(Age_H, Sub.score_H, main = "effect of age", xlab = "Age", ylab = "Subjectivity score")
abline(lm.fit_age)

gend_H <- ifelse(Gender_H=="Male", 1, 0)
boxplot(gend_H, Sub.score_H, main = "effect of gender", xlab = "Gender", ylab = "Subjectivity score")


lm.fit_edu <- lm(Sub.score_H ~ Edu_H)
plot(Edu_H, Sub.score_H, main = "effect of education", xlab = "Education", ylab = "Subjectivity score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Sub.score_H ~ Income_H)
plot(Income_H, Sub.score_H, main = "effect of income", xlab = "Income", ylab = "Subjectivity score")
abline(lm.fit_inc)

# Positive score  as dependent measure
lm.fit_age <- lm(Pos.score_H ~ Age_H)
plot(Age_H, Pos.score_H, main = "effect of age", xlab = "Age", ylab = "positive score")
abline(lm.fit_age)


boxplot(gend_H, Pos.score_H, main = "effect of gender", xlab = "Gender", ylab = "positive score")


lm.fit_edu <- lm(Pos.score_H ~ Edu_H)
plot(Edu_H, Pos.score_H, main = "effect of education", xlab = "Education", ylab = "positive score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Pos.score_H ~ Income_H)
plot(Income_H, Pos.score_H, main = "effect of income", xlab = "Income", ylab = "positive score")
abline(lm.fit_inc)

# Negative score  as dependent measure
lm.fit_age <- lm(Neg.score_H ~ Age_H)
plot(Age_H, Neg.score_H, main = "effect of age", xlab = "Age", ylab = "negative score")
abline(lm.fit_age)

boxplot(gend_H, Neg.score_H, main = "effect of gender ", xlab = "Gender", ylab = "negative score")


lm.fit_edu <- lm(Neg.score_H ~ Edu_H)
plot(Edu_H, Neg.score_H, main = "effect of education", xlab = "Education", ylab = "negative score")
abline(lm.fit_edu)

lm.fit_inc <- lm(Neg.score_H ~ Income_H)
plot(Income_H, Neg.score_H, main = "effect of income", xlab = "Income", ylab = "negative score")
abline(lm.fit_inc)

# Determine the relationship with respect to the following trait variables:
# (PANAS-X, BIF, Symptom scales PHQ-9) 
# mood/anxiety group
PANASX_pos_M.A <- SelfReport_M.A[ ,grepl("PANASX_PosAffect", names(SelfReport_M.A))]
PANASX_neg_M.A <- SelfReport_M.A[ ,grepl("PANASX_NegAffect", names(SelfReport_M.A))]
PHQ_M.A <- case.control_stat_MA$PHQ

# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_PAN.pos <- lm(Sub.score_M.A ~ PANASX_pos_M.A)
plot(PANASX_pos_M.A, Sub.score_M.A, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Subjectivity score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Sub.score_M.A ~ PANASX_neg_M.A)
plot(PANASX_neg_M.A, Sub.score_M.A, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Subjectivity score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Sub.score_M.A ~ PHQ_M.A)
plot(PHQ_M.A, Sub.score_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Subjectivity score")
abline(lm_fit_PHQ)

# Positive score as dependent measure
lm_fit_PAN.pos <- lm(Pos.score_M.A ~ PANASX_pos_M.A)
plot(PANASX_pos_M.A, Pos.score_M.A, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Positive score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Pos.score_M.A ~ PANASX_neg_M.A)
plot(PANASX_neg_M.A, Pos.score_M.A, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Positive score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Pos.score_M.A ~ PHQ_M.A)
plot(PHQ_M.A, Pos.score_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Positive score")
abline(lm_fit_PHQ)

# Negative score as dependent measure
lm_fit_PAN.pos <- lm(Neg.score_M.A ~ PANASX_pos_M.A)
plot(PANASX_pos_M.A, Neg.score_M.A, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Negative score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Neg.score_M.A ~ PANASX_neg_M.A)
plot(PANASX_neg_M.A, Neg.score_M.A, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Negative score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Neg.score_M.A ~ PHQ_M.A)
plot(PHQ_M.A, Neg.score_M.A, main = "effect of PHQ", xlab = "PHQ", ylab = "Negative score")
abline(lm_fit_PHQ)

# healthy control group
PANASX_pos_H <- SelfReport_H[ ,grepl("PANASX_PosAffect", names(SelfReport_H))]
PANASX_neg_H <- SelfReport_H[ ,grepl("PANASX_NegAffect", names(SelfReport_H))]
PHQ_H <- case.control_stat_HC$PHQ

# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_PAN.pos <- lm(Sub.score_H ~ PANASX_pos_H)
plot(PANASX_pos_H, Sub.score_H, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Subjectivity score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Sub.score_H ~ PANASX_neg_H)
plot(PANASX_neg_H, Sub.score_H, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Subjectivity score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Sub.score_H ~ PHQ_H)
plot(PHQ_H, Sub.score_H, main = "effect of PHQ", xlab = "PHQ", ylab = "Subjectivity score")
abline(lm_fit_PHQ)

# Positive score as dependent measure
lm_fit_PAN.pos <- lm(Pos.score_H ~ PANASX_pos_H)
plot(PANASX_pos_H, Pos.score_H, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Positive score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Pos.score_H ~ PANASX_neg_H)
plot(PANASX_neg_H, Pos.score_H, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Positive score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Pos.score_H ~ PHQ_H)
plot(PHQ_H, Pos.score_H, main = "effect of PHQ", xlab = "PHQ", ylab = "Positive score")
abline(lm_fit_PHQ)

# Negative score as dependent measure
lm_fit_PAN.pos <- lm(Neg.score_H ~ PANASX_pos_H)
plot(PANASX_pos_H, Neg.score_H, main = "effect of PANASX positive", xlab = "PANASX positive", ylab = "Negative score")
abline(lm_fit_PAN.pos)

lm_fit_PAN.neg <- lm(Neg.score_H ~ PANASX_neg_H)
plot(PANASX_neg_H, Neg.score_H, main = "effect of PANASX negative", xlab = "PANASX negative", ylab = "Negative score")
abline(lm_fit_PAN.neg)

lm_fit_PHQ <- lm(Neg.score_H ~ PHQ_H)
plot(PHQ_H, Neg.score_H, main = "effect of PHQ", xlab = "PHQ", ylab = "Negative score")
abline(lm_fit_PHQ)

# mood and anxiety
BFI_con_M.A <- SelfReport_M.A[ ,grepl("BFI_Conscious", names(SelfReport_M.A))]
BFI_open_M.A <- SelfReport_M.A[ ,grepl("BFI_Openness", names(SelfReport_M.A))]
BFI_agree_M.A <- SelfReport_M.A[ ,grepl("BFI_Agreeb", names(SelfReport_M.A))]
BFI_extrav_M.A <- SelfReport_M.A[ ,grepl("BFI_Extrav", names(SelfReport_M.A))]
BFI_neurot_M.A <- SelfReport_M.A[ ,grepl("BFI_Neurot", names(SelfReport_M.A))]

# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_BFI.con <- lm(Sub.score_M.A ~ BFI_con_M.A)
plot(BFI_con_M.A, Sub.score_M.A, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Subjectivity score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Sub.score_M.A ~ BFI_open_M.A)
plot(BFI_open_M.A, Sub.score_M.A, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Subjectivity score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Sub.score_M.A ~ BFI_agree_M.A)
plot(BFI_agree_M.A, Sub.score_M.A, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Subjectivity score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Sub.score_M.A ~ BFI_extrav_M.A)
plot(BFI_extrav_M.A, Sub.score_M.A, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Subjectivity score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Sub.score_M.A ~ BFI_neurot_M.A)
plot(BFI_neurot_M.A, Sub.score_M.A, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Subjectivity score")
abline(lm_fit_BFI.neurot)



# Positive score as dependent measure
lm_fit_BFI.con <- lm(Pos.score_M.A ~ BFI_con_M.A)
plot(BFI_con_M.A, Pos.score_M.A, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Positive score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Pos.score_M.A ~ BFI_open_M.A)
plot(BFI_open_M.A, Pos.score_M.A, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Positive score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Pos.score_M.A ~ BFI_agree_M.A)
plot(BFI_agree_M.A, Pos.score_M.A, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Positive score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Pos.score_M.A ~ BFI_extrav_M.A)
plot(BFI_extrav_M.A, Pos.score_M.A, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Positive score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Pos.score_M.A ~ BFI_neurot_M.A)
plot(BFI_neurot_M.A, Pos.score_M.A, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Positive score")
abline(lm_fit_BFI.neurot)



# Negative score as dependent measure
lm_fit_BFI.con <- lm(Neg.score_M.A ~ BFI_con_M.A)
plot(BFI_con_M.A, Neg.score_M.A, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Negative score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Neg.score_M.A ~ BFI_open_M.A)
plot(BFI_open_M.A, Neg.score_M.A, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Negative score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Neg.score_M.A ~ BFI_agree_M.A)
plot(BFI_agree_M.A, Neg.score_M.A, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Negative score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Neg.score_M.A ~ BFI_extrav_M.A)
plot(BFI_extrav_M.A, Neg.score_M.A, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Negative score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Neg.score_M.A ~ BFI_neurot_M.A)
plot(BFI_neurot_M.A, Neg.score_M.A, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Negative score")
abline(lm_fit_BFI.neurot)

# healthy control group
BFI_con_H <- SelfReport_H[ ,grepl("BFI_Conscious", names(SelfReport_H))]
BFI_open_H <- SelfReport_H[ ,grepl("BFI_Openness", names(SelfReport_H))]
BFI_agree_H <- SelfReport_H[ ,grepl("BFI_Agreeb", names(SelfReport_H))]
BFI_extrav_H <- SelfReport_H[ ,grepl("BFI_Extrav", names(SelfReport_H))]
BFI_neurot_H <- SelfReport_H[ ,grepl("BFI_Neurot", names(SelfReport_H))]


# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_BFI.con <- lm(Sub.score_H ~ BFI_con_H)
plot(BFI_con_H, Sub.score_H, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Subjectivity score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Sub.score_H ~ BFI_open_H)
plot(BFI_open_H, Sub.score_H, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Subjectivity score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Sub.score_H ~ BFI_agree_H)
plot(BFI_agree_H, Sub.score_H, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Subjectivity score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Sub.score_H ~ BFI_extrav_H)
plot(BFI_extrav_H, Sub.score_H, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Subjectivity score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Sub.score_H ~ BFI_neurot_H)
plot(BFI_neurot_H, Sub.score_H, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Subjectivity score")
abline(lm_fit_BFI.neurot)


# Positive score as dependent measure
lm_fit_BFI.con <- lm(Pos.score_H ~ BFI_con_H)
plot(BFI_con_H, Pos.score_H, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Positive score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Pos.score_H ~ BFI_open_H)
plot(BFI_open_H, Pos.score_H, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Positive score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Pos.score_H ~ BFI_agree_H)
plot(BFI_agree_H, Pos.score_H, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Positive score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Pos.score_H ~ BFI_extrav_H)
plot(BFI_extrav_H, Pos.score_H, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Positive score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Pos.score_H ~ BFI_neurot_H)
plot(BFI_neurot_H, Pos.score_H, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Positive score")
abline(lm_fit_BFI.neurot)


# Negative score as dependent measure
lm_fit_BFI.con <- lm(Neg.score_H ~ BFI_con_H)
plot(BFI_con_H, Neg.score_H, main = "effect of BFI conscious", xlab = "BFI conscious", ylab = "Negative score")
abline(lm_fit_BFI.con)

lm_fit_BFI.open <- lm(Neg.score_H ~ BFI_open_H)
plot(BFI_open_H, Neg.score_H, main = "effect of BFI openness", xlab = "BFI openness", ylab = "Negative score")
abline(lm_fit_BFI.open)

lm_fit_BFI.agree <- lm(Neg.score_H ~ BFI_agree_H)
plot(BFI_agree_H, Neg.score_H, main = "effect of BFI Agreeb", xlab = "BFI Agreeb", ylab = "Negative score")
abline(lm_fit_BFI.agree)

lm_fit_BFI.extrav <- lm(Neg.score_H ~ BFI_extrav_H)
plot(BFI_extrav_H, Neg.score_H, main = "effect of BFI Extrav", xlab = "BFI Extrav", ylab = "Negative score")
abline(lm_fit_BFI.extrav)

lm_fit_BFI.neurot <- lm(Neg.score_H ~ BFI_neurot_H)
plot(BFI_neurot_H, Neg.score_H, main = "effect of BFI Neurot", xlab = "BFI Neurot", ylab = "Negative score")
abline(lm_fit_BFI.neurot)

# Determine the relationship with respect to the following trait variables:
# (Symptom scales OASIS), PROMIS (anxiety, depression, anger), Interviewers)
# mood and anxiety group
OASIS_M.A <- case.control_stat_MA$OASIS
PROMIS.Ang_M.A <- SelfReport_M.A[ ,grepl("PROMIS_AngerTscore", names(SelfReport_M.A))]
PROMIS.Dep_M.A <- SelfReport_M.A[ , grepl("PROMIS_DepressTscore", names(SelfReport_M.A))]
PROMIS_Anx_M.A <- SelfReport_M.A[ , grepl("PROMIS_AnxietyTscore", names(SelfReport_M.A))]
LC_M.A <- as.numeric(narratives_notes_M.A$LC_conducted_by)

# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_OASIS <- lm(Sub.score_M.A ~ OASIS_M.A)
plot(OASIS_M.A, Sub.score_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Subjectivity score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Sub.score_M.A ~ PROMIS.Ang_M.A)
plot(PROMIS.Ang_M.A, Sub.score_M.A, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Sub.score_M.A ~ PROMIS.Dep_M.A)
plot(PROMIS.Dep_M.A, Sub.score_M.A, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Sub.score_M.A ~ PROMIS_Anx_M.A)
plot(PROMIS_Anx_M.A, Sub.score_M.A, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Subjectivity score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Sub.score_M.A ~ LC_M.A)
plot(LC_M.A, Sub.score_M.A, main = "effect of LC", xlab = "LC", ylab = "Subjectivity score")
abline(lm_fit_LC)

# Positive score as dependent measure
lm_fit_OASIS <- lm(Pos.score_M.A ~ OASIS_M.A)
plot(OASIS_M.A, Pos.score_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Positive score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Pos.score_M.A ~ PROMIS.Ang_M.A)
plot(PROMIS.Ang_M.A, Pos.score_M.A, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Positive score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Pos.score_M.A ~ PROMIS.Dep_M.A)
plot(PROMIS.Dep_M.A, Pos.score_M.A, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Positive score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Pos.score_M.A ~ PROMIS_Anx_M.A)
plot(PROMIS_Anx_M.A, Pos.score_M.A, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Positive score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Pos.score_M.A ~ LC_M.A)
plot(LC_M.A, Pos.score_M.A, main = "effect of LC", xlab = "LC", ylab = "Positive score")
abline(lm_fit_LC)

# Negative score as dependent measure
lm_fit_OASIS <- lm(Neg.score_M.A ~ OASIS_M.A)
plot(OASIS_M.A, Neg.score_M.A, main = "effect of OASIS", xlab = "OASIS", ylab = "Negative score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Neg.score_M.A ~ PROMIS.Ang_M.A)
plot(PROMIS.Ang_M.A, Neg.score_M.A, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Negative score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Neg.score_M.A ~ PROMIS.Dep_M.A)
plot(PROMIS.Dep_M.A, Neg.score_M.A, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Negative score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Neg.score_M.A ~ PROMIS_Anx_M.A)
plot(PROMIS_Anx_M.A, Neg.score_M.A, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Negative score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Neg.score_M.A ~ LC_M.A)
plot(LC_M.A, Neg.score_M.A, main = "effect of LC", xlab = "LC", ylab = "Negative score")
abline(lm_fit_LC)

# healthy control group
OASIS_H <- case.control_stat_HC$OASIS
PROMIS.Ang_H <- SelfReport_H[ ,grepl("PROMIS_AngerTscore", names(SelfReport_H))]
PROMIS.Dep_H <- SelfReport_H[ , grepl("PROMIS_DepressTscore", names(SelfReport_H))]
PROMIS_Anx_H <- SelfReport_H[ , grepl("PROMIS_AnxietyTscore", names(SelfReport_H))]
LC_H <- as.numeric(narratives_notes_H$LC_conducted_by)

# Subjectivity score as dependent measure
par(mfrow = c(3, 5))
lm_fit_OASIS <- lm(Sub.score_H ~ OASIS_H)
plot(OASIS_H, Sub.score_H, main = "effect of OASIS", xlab = "OASIS", ylab = "Subjectivity score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Sub.score_H ~ PROMIS.Ang_H)
plot(PROMIS.Ang_H, Sub.score_H, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Sub.score_H ~ PROMIS.Dep_H)
plot(PROMIS.Dep_H, Sub.score_H, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Subjectivity score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Sub.score_H ~ PROMIS_Anx_H)
plot(PROMIS_Anx_H, Sub.score_H, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Subjectivity score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Sub.score_H ~ LC_H)
plot(LC_H, Sub.score_H, main = "effect of LC", xlab = "LC", ylab = "Subjectivity score")
abline(lm_fit_LC)

# Positive score as dependent measure
lm_fit_OASIS <- lm(Pos.score_H ~ OASIS_H)
plot(OASIS_H, Pos.score_H, main = "effect of OASIS", xlab = "OASIS", ylab = "Positive score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Pos.score_H ~ PROMIS.Ang_H)
plot(PROMIS.Ang_H, Pos.score_H, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Positive score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Pos.score_H ~ PROMIS.Dep_H)
plot(PROMIS.Dep_H, Pos.score_H, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Positive score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Pos.score_H ~ PROMIS_Anx_H)
plot(PROMIS_Anx_H, Pos.score_H, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Positive score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Pos.score_H ~ LC_H)
plot(LC_H, Pos.score_H, main = "effect of LC", xlab = "LC", ylab = "Positive score")
abline(lm_fit_LC)

# Negative score as dependent measure
lm_fit_OASIS <- lm(Neg.score_H ~ OASIS_H)
plot(OASIS_H, Neg.score_H, main = "effect of OASIS", xlab = "OASIS", ylab = "Negative score")
abline(lm_fit_OASIS)

lm_fit_PROMIS.Ang <- lm(Neg.score_H ~ PROMIS.Ang_H)
plot(PROMIS.Ang_H, Neg.score_H, main = "effect of PROMIS Anger", xlab = "PROMIS Anger", ylab = "Negative score")
abline(lm_fit_PROMIS.Ang)

lm_fit_PROMIS.Dep <- lm(Neg.score_H ~ PROMIS.Dep_H)
plot(PROMIS.Dep_H, Neg.score_H, main = "effect of PROMIS Depress", xlab = "PROMIS Depress", ylab = "Negative score")
abline(lm_fit_PROMIS.Dep)

lm_fit_PROMIS_Anx <- lm(Neg.score_H ~ PROMIS_Anx_H)
plot(PROMIS_Anx_H, Neg.score_H, main = "effect of PROMIS Anxiety", xlab = "PROMIS Anxiety", ylab = "Negative score")
abline(lm_fit_PROMIS_Anx)

lm_fit_LC <- lm(Neg.score_H ~ LC_H)
plot(LC_H, Neg.score_H, main = "effect of LC", xlab = "LC", ylab = "Negative score")
abline(lm_fit_LC)


# Determine mediating influences (CTQ, TEC)on dependent measured scores
# mood/anxiety
CTQ_M.A <- SelfReport_M.A[ ,grepl("CTQ_score", names(SelfReport_M.A))]
TES_M.A <- SelfReport_M.A[ ,grepl("TES_TotalOccurrence", names(SelfReport_M.A))]
DAST_M.A <- case.control_stat_MA[ ,grepl("DAST", names(SelfReport_M.A))]
# healthy control
CTQ_H <- SelfReport_H[ ,grepl("CTQ_score", names(SelfReport_H))]
TES_H <- SelfReport_H[ ,grepl("TES_TotalOccurrence", names(SelfReport_H))]
DAST_H <- case.control_stat_HC[ ,grepl("DAST", names(SelfReport_H))]


# mood and anxiety
# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_CTQ <- lm(Sub.score_M.A ~ CTQ_M.A)
plot(CTQ_M.A, Sub.score_M.A, main = "effect of CTQ (mood/anxiety)", xlab = "CTQ", ylab = "Subjectivity score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Sub.score_M.A ~ TES_M.A)
plot(TES_M.A, Sub.score_M.A, main = "effect of TES (mood/anxiety)", xlab = "TES", ylab = "Subjectivity score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Sub.score_M.A ~ DAST_M.A)
plot(DAST_M.A, Sub.score_M.A, main = "effect of DAST (mood/anxiety)", xlab = "DAST", ylab = "Subjectivity score")
abline(lm_fit_DAST)

# Positive score as dependent measure
lm_fit_CTQ <- lm(Pos.score_M.A ~ CTQ_M.A)
plot(CTQ_M.A, Pos.score_M.A, main = "effect of CTQ (mood/anxiety)", xlab = "CTQ", ylab = "Positive score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Pos.score_M.A ~ TES_M.A)
plot(TES_M.A, Pos.score_M.A, main = "effect of TES (mood/anxiety)", xlab = "TES", ylab = "Positive score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Pos.score_M.A ~ DAST_M.A)
plot(DAST_M.A, Pos.score_M.A, main = "effect of DAST (mood/anxiety)", xlab = "DAST", ylab = "Positive score")
abline(lm_fit_DAST)

# Negative score as dependent measure
lm_fit_CTQ <- lm(Neg.score_M.A ~ CTQ_M.A)
plot(CTQ_M.A, Neg.score_M.A, main = "effect of CTQ (mood/anxiety)", xlab = "CTQ", ylab = "Negative score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Neg.score_M.A ~ TES_M.A)
plot(TES_M.A, Neg.score_M.A, main = "effect of TES (mood/anxiety)", xlab = "TES", ylab = "Negative score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Neg.score_M.A ~ DAST_M.A)
plot(DAST_M.A, Neg.score_M.A, main = "effect of DAST (mood/anxiety)", xlab = "DAST", ylab = "Negative score")
abline(lm_fit_DAST)

# healthy control
# Subjectivity score as dependent measure
par(mfrow = c(3, 3))
lm_fit_CTQ <- lm(Sub.score_H ~ CTQ_H)
plot(CTQ_H, Sub.score_H, main = "effect of CTQ (healthy control)", xlab = "CTQ", ylab = "Subjectivity score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Sub.score_H ~ TES_H)
plot(TES_H, Sub.score_H, main = "effect of TES (healthy control)", xlab = "TES", ylab = "Subjectivity score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Sub.score_H ~ DAST_H)
plot(DAST_H, Sub.score_H, main = "effect of DAST (healthy control)", xlab = "DAST", ylab = "Subjectivity score")
abline(lm_fit_DAST)

# Positive score as dependent measure
lm_fit_CTQ <- lm(Pos.score_H ~ CTQ_H)
plot(CTQ_H, Pos.score_H, main = "effect of CTQ (healthy control)", xlab = "CTQ", ylab = "Positive score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Pos.score_H ~ TES_H)
plot(TES_H, Pos.score_H, main = "effect of TES (healthy control)", xlab = "TES", ylab = "Positive score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Pos.score_H ~ DAST_H)
plot(DAST_H, Pos.score_H, main = "effect of DAST (healthy control)", xlab = "DAST", ylab = "Positive score")
abline(lm_fit_DAST)


# Negative score as dependent measure
lm_fit_CTQ <- lm(Neg.score_H ~ CTQ_H)
plot(CTQ_H, Neg.score_H, main = "effect of CTQ (healthy control)", xlab = "CTQ", ylab = "Negative score")
abline(lm_fit_CTQ)

lm_fit_TES <- lm(Neg.score_H ~ TES_H)
plot(TES_H, Neg.score_H, main = "effect of TES (healthy control)", xlab = "TES", ylab = "Negative score")
abline(lm_fit_TES)

lm_fit_DAST <- lm(Neg.score_H ~ DAST_H)
plot(DAST_H, Neg.score_H, main = "effect of DAST (healthy control)", xlab = "DAST", ylab = "Negative score")
abline(lm_fit_DAST)

