tib_comments=readr::read_csv("https://raw.githubusercontent.com/lvaudor/textR/c291e5cd0c0656ea7e2b8bf6c0485ba80b69b0d7/datasets/tib_comments.csv")

library(tidytext)  
tib_comments_words <- unnest_tokens(tib_comments,
                                    output="word",
                                    input="commentext")
library(proustr)

tib_comments_mainwords <- anti_join(tib_comments_words,
                                    proust_stopwords()) 

tib_wordcloud=tib_comments_mainwords %>% 
  group_by(word) %>% 
  summarise(freq=n())%>% 
  filter(freq>=100)

library(wordcloud)  
wordcloud(tib_wordcloud$word,
          tib_wordcloud$freq)

###########♦



tib_comments <- readr::read_csv("https://raw.githubusercontent.com/lvaudor/textR/c291e5cd0c0656ea7e2b8bf6c0485ba80b69b0d7/datasets/tib_comments.csv")
tib_comments=tib_comments %>%
  filter(!is.na(note))

library(tidytext)  
tib_comments_words <- unnest_tokens(tib_comments,
                                    output="word",
                                    input="commentext")
library(proustr)
library(dplyr)
tib_comments_mainwords <- filter(tib_comments_words,
                                 !(word %in% proust_stopwords()$word))  



mnote <- tib_comments %>%
  filter(!is.na(note)) %>% 
  summarise(mnote=mean(note)) %>%
  pull()

tib_comments_mainwords=tib_comments_mainwords %>% 
  mutate(bonne_note=note>mean(note)) %>% 
  group_by(word) %>% 
  mutate(freq_tot=n(),
         freq_bonne_note=length(which(bonne_note))) %>% 
  mutate(prop_bonne_note=freq_bonne_note/freq_tot)


library(ggplot2)


ggplot(filter(tib_comments_mainwords,freq_tot>200),
       aes(x=forcats::fct_reorder(word,prop_bonne_note),
           fill=bonne_note)) + 
  geom_bar(position="fill")+
  coord_flip()
