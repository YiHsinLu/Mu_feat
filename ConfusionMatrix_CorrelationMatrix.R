#packages
library(knitr)
library(dplyr)
library(flextable)
library(magrittr)
library(kableExtra)
library(tidytext)
library(tidyverse)
library(plot.matrix)
library(stringr)
library(micropan)
library(ggpubr)
library(highcharter)
library(ggpubr)
library(tm)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(wordcloud2)
library(e1071)
library(nnet)
library(rpart)
library(randomForest)

#ConMatrix = confusionMatrix
table <- data.frame(ConMatrix$table)

plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = prop)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "green", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference)))+
  ggtitle("Accuracy : 0.64")
#ggsave('D:/music_feature/fig/confusionMatrix_ohencoding_std.png')


# df = dataMatrix/dataFrame
res2<-rcorr(as.matrix(df))
flattenCorrMatrix(res2$r, res2$P)


col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res2$r, col = col, symm = TRUE)
