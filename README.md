# LifeChart_SentAna: Association between sentiment scores obtained from life chart narrative notes and T-500 self-reported scores in Mood/Anxiety group

Analysis description: T-500 self-reported data base consists of various coded claim data associated with each subject with which it captures only some elements from clinical representation. We motivated to use Natural Language Processing methods to measure other benefits from narrative discharge summaries–lifechart narrative note that is a short story of subject’s life. Using Natural Language Toolkit (NLTK) library, we analyzed each narrative note to aggregated sentiment scores associated with each subject’s lifechart. Thus, we found four sentiment scores based on valence words, positive score (0 to 1), negative score (-1 to 0), objectivity score(0 to 1, i.e. this is also known as objectivity score, a 0 score mean subjective and 1 means objective), and polarity score (-1 to +1). Here is the definition of each score, beriefly:

-The compound score is computed by summing the valence scores of each word in the lexicon, adjusted according to the rules, and then normalized to be between -1 (most extreme negative) and +1 (most extreme positive)

-The positive, objectivity, and negative scores are ratios for proportions of text that fall in each category.

In this report, we investigate the association of these four scores, adjusted by total number of sentences, with self-reposted scores in T-500 data base for mood/anxitey group.
