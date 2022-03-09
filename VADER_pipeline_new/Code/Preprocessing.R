#
#
# Associations between dependent measured scores and self reportes symptom-based
#
# Developed by Saeid Parvandeh Jan/2017
# 
# ----------------------------------------------------------
# Read csv self repoted file
rm(list=ls())
setwd("E:/LIBR/Hamed/new version/Data")


Dat1 <- read.csv("sentiment_scores.csv")
Dat.norm <- read.csv("sentiment_scores_SentenceNormalized.csv")
Dat2 <- read.csv("T500_2017-03-18.csv")
Dat3 <- read.csv("T500_Lifechart_Narratives_Scores.csv")
common_ids <- intersect(intersect(as.vector(Dat1[, 1]), as.vector(Dat2[, 1])),Dat3[, 1])
length(common_ids)

case.control_stat <- Dat2[which(Dat2$visit=="S" & Dat2$id%in%common_ids), ]
dim(case.control_stat)
save(case.control_stat, file = "case.control_stat.RData")
self_reports_scores <- Dat2[which(Dat2$visit=="T0" & Dat2$id%in%common_ids), ]
dim(self_reports_scores)
save(self_reports_scores, file = "self_reports_scores.RData")
narratives_notes <- Dat3[which(Dat3$id%in%common_ids), ]
dim(narratives_notes)
save(narratives_notes, file = "narratives_notes.RData")
sentiment_scores <- Dat1[which(Dat1$id%in%common_ids), ]
dim(sentiment_scores)
save(sentiment_scores, file = "sentiment_scores.RData")
sentiment_scores_SentenceNormalized <- Dat.norm[which(Dat.norm$id%in%common_ids), ]
dim(sentiment_scores_SentenceNormalized)
save(sentiment_scores_SentenceNormalized, file = "sentiment_scores_SentenceNormalized.RData")

# Healthy Control, Mood/Anxiety, and Substance Use
case.control_stat_HC.MA.SU <- subset(case.control_stat, 
                                     case.control_stat$GroupAssignment== "Healthy Control" |
                                       case.control_stat$GroupAssignment=="Mood/Anxiety"| 
                                       case.control_stat$GroupAssignment=="Substance Use")
dim(case.control_stat_HC.MA.SU)
SelfReport_HC.MA.SU <- subset(self_reports_scores, 
                              case.control_stat$GroupAssignment== "Healthy Control" |
                                case.control_stat$GroupAssignment=="Mood/Anxiety"| 
                                case.control_stat$GroupAssignment=="Substance Use")
dim(SelfReport_HC.MA.SU)
narratives_notes_HC.MA.SU <- narratives_notes[which(narratives_notes$id%in%SelfReport_HC.MA.SU$id), ]
dim(narratives_notes_HC.MA.SU)
MeasrdScore_HC.MA.SU <- sentiment_scores_SentenceNormalized[which(sentiment_scores_SentenceNormalized$id%in%SelfReport_HC.MA.SU$id), ]
dim(MeasrdScore_HC.MA.SU)
# write a csv file
write.csv(narratives_notes_HC.MA.SU, file = "T500_ThreeGroups.csv", row.names = FALSE)


# Mood and Anxiety
case.control_stat_MA <- subset(case.control_stat, GroupAssignment=="Mood/Anxiety")
SelfReport_M.A <- subset(self_reports_scores, case.control_stat$GroupAssignment=="Mood/Anxiety")
dim(SelfReport_M.A)
narratives_notes_M.A <- narratives_notes[which(narratives_notes$id%in%SelfReport_M.A$id), ]
MeasrdScore_M.A <- sentiment_scores[which(sentiment_scores$id%in%SelfReport_M.A$id), ]

# Healthy Control
case.control_stat_HC <- subset(case.control_stat, GroupAssignment=="Healthy Control")
SelfReport_H <- subset(self_reports_scores, case.control_stat$GroupAssignment=="Healthy Control")
dim(SelfReport_H)
narratives_notes_H <- narratives_notes[which(narratives_notes$id%in%SelfReport_H$id), ]
MeasrdScore_H <- sentiment_scores[which(sentiment_scores$id%in%SelfReport_H$id), ]


# Comparing the number of total words in mood/anxiety and healthy/control groups
hist(MeasrdScore_M.A$total_words, col = "blue", main = "Histogram of total number of words",
     xlab = "Number of words")
hist(MeasrdScore_H$total_words, col = "purple", add = TRUE)
legend("topright", c("Mood and Anxiety", "Healthy Control"), col =c("blue","purple"), lwd = 10)

