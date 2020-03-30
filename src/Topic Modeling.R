library(topicmodels)

data("AssociatedPress")

# set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#Notice that this has turned the model into a one-topic-per-term-per-row format. 
#For each combination, the model computes the probability of that term being generated from that topic. For example, the term “aaron” has a  
#1.686917×10^-12 probability of being generated from topic 1, but a  
#3.8959408×10^-5 probability of being generated from topic 2.

#We could use dplyr’s top_n() to find the 10 terms that are most common within each topic. 
#As a tidy data frame, this lends itself well to a ggplot2 visualization (Figure 6.2).

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#As an alternative, we could consider the terms that had the greatest difference in  
#β between topic 1 and topic 2.
library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
#Besides estimating each topic as a mixture of words, LDA also models each document as a mixture of topics.
#We can examine the per-document-per-topic probabilities, called (“gamma”), with the matrix = "gamma" argument to tidy().

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#Each of these values is an estimated proportion of words from that document that are generated from that topic.
#For example, the model estimates that only about 25% of the words in document 1 were generated from topic 1.


