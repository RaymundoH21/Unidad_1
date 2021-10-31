#Hirales Lazareno Raymundo - 17212339

install.packages("tidyverse")
install.packages("dplyr") 

#Librerias requeridas
library( dplyr )
library( tidytext )
library( textdata )
library( ggplot2 )
library( RColorBrewer )
library( wordcloud )
library( reshape2 )
library( tidyverse )

# El archivo esta subido a un github para facilitar su carga dentro de RStudio
mlpURL <- "https://raw.githubusercontent.com/SmilodonCub/DATA607/master/my-little-pony-transcript/clean_dialog.csv"
mlp_df <- read.csv( mlpURL )
dim( mlp_df )

#
colnames( mlp_df )

#En este apartado exploraremos los sentimientos como una funcion de la narrativa de los subsecuentes episodios de MLP.
# La pregunta es como los sentimientos varian a lo largo del dialogo del episodio?
episodeLines <- mlp_df %>%
  group_by( title ) %>% #with respect to episode title:
  mutate( id = row_number()) %>% #add a new feature 'id' to enumerate each row of text
  group_by( title, id) %>% #with respect to episode title & line of text(id):
  rowwise() %>% 
  summarise( lines = paste(dialog, collapse = "&&")) %>% 
  #paste all episode lines together delimited by '&&'
  mutate( lines = str_split( lines, "&&") ) %>%
  #mutate lines to a list of lines 
  unnest( lines ) %>% #unnest list of lines to one line per row
  unnest_tokens(word, lines) #one token/word per line
head(episodeLines)

#list of first 6 episode title
AllEpisodes <- unique(episodeLines$title)[1:20]

#perform an inner join with the bing lexicon
pony_sentiment <- episodeLines %>%
  filter( title %in% AllEpisodes ) %>% #subset for the first 8 episodes
  inner_join(get_sentiments("bing")) %>% #inner join with 'bing' lexicon
  #for each title, tally the sentiment score of the 
  #tokens in increments of 10 lines
  count(title, index = id %/% 20, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
head( pony_sentiment )

#Visualize sentiment over the course of episode narative:

colourCount = length(unique(pony_sentiment$title))
mycolors = colorRampPalette(brewer.pal(50, "PuRd"))(colourCount)

ggplot(pony_sentiment, aes(index, sentiment,color='black', fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x") +
  scale_fill_manual( values = mycolors) +
  labs( title = 'Sentiment Analysis', subtitle="Sentiment across episode trajectory for the first 6 episodes")
