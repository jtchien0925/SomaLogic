#install.packages("devtools")
#devtools::install_bitbucket("graumannlabtools/readat")
library(readat)
library(reshape2)
library(magrittr)
library(dplyr)

try<-readAdat("EMO-16-197.hybNorm.medNorm.20161025.adat")

#subsetting data to all Ex30
newdata<-try[c(4,7,8,9,11,17)]

seqData<-getSequenceData(newdata)

meldata<-melt(newdata)

#the parameters are (your dataset, Top number of the variation, grouping)
insterestingSeqs<- getSequencesWithLargestBetweenGroupVariation(meldata,n = 1, group= ~SeqId,Target)

interestingData<- merge(meldata,insterestingSeqs) %>%
mutate_(SeqName= ~paste(SeqId, Target,sep =","))

library(ggplot)
figure1<- interestingData %>%
  ggplot(aes(TimePoint, Intensity, color= Subject_ID)) + 
  geom_point(size = 6) +
  scale_y_log10() + 
  facet_wrap(~ SeqName) +
  theme_bw() +
  theme (legend.position = "top") +
  labs (color = "Monkey" , x= "Time Points")

#if you want to see figrue1, just call "figrue1"#

#MonkeyExpression<- soma2eset(newdata)
#MonkeyExpression$
