library(tidyverse)
df <- read.csv('https://gist.githubusercontent.com/Bhargav-Rao/e9d6eb46bd1254cf78283db9239f09d7/raw/160cb8413b2feed0c52386b03a64ce93079be139/AskUbuntuLogs.csv', header = TRUE)

df <- df %>% 
  mutate(Details = gsub('.$', '', Details)) %>% 
  separate_rows(Details, sep = ';') %>%
  mutate(Details2 = sapply(strsplit(Details, ' - '), `[`, 1),
         Details2 = if_else(substr(Details2, 1, 6) == 'User @', 
                            'User mentioned', Details2))

counts <- count(ungroup(df), feedback, Details2)

ggplot(counts, aes(y = n, x = 1, fill = feedback)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ Details2, nrow = 5, scales = 'free') +
  coord_flip() +
  scale_fill_discrete(name = 'feedback') +
  ylab('count') +
  theme(legend.position = 'bottom',
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("1.png", dpi = 300, w = 7, h = 5)

ggplot(counts, aes(y = n, x = 1, fill = Details2)) + 
  geom_bar(stat = 'identity', col = 1) + 
  facet_wrap(~ feedback, nrow = 5, scales = 'free') +
  coord_flip() +
  scale_fill_discrete(name = 'feedback') +
  ylab('count') +
  guides(fill = guide_legend(ncol = 3)) +
  theme(legend.position = 'bottom',
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave("2.png", dpi = 300, w = 6, h = 5)

df %>% group_by(Details2) %>% 
  summarise(power = sum(feedback == "tp") / sum(feedback == "fp"),
            n = n()) %>% 
  ggplot(., aes(power, Details2, size = n)) + geom_point() +
  xlab("number of true positives / number of false positives") + ylab("") +
  expand_limits(x = 0)
ggsave("3.png", dpi = 300, w = 6, h = 6)

df %>% group_by(post) %>% summarise(feedback = first(feedback), n = n()) %>% 
  ggplot(., aes(feedback, n)) + geom_count() + ylab('number of flags')
ggsave("4.png", w = 3, h = 4, dpi = 300)

df %>% group_by(post) %>% summarise(feedback = first(feedback), NaaValue = first(NaaValue)) %>% 
  ggplot(., aes(feedback, NaaValue)) +
  geom_violin()
ggsave("5.png", w = 3, h = 4, dpi = 300)

df %>% group_by(post) %>% summarise(feedback = first(feedback), NaaValue = first(NaaValue)) %>% 
  ggplot(., aes(feedback, NaaValue)) +
  geom_count(aes(size = ..prop..))
ggsave("6.png", w = 3, h = 4, dpi = 300)
