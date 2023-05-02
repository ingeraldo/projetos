library(spotifyr)
library(lubridate)
library(syuzhet)
library(RColorBrewer)
library(ggplot2)
library(forcats)
library(dplyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'SPOTIFY_CLIENT_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'SPOTIFY_CLIENT_SECRET')
access_token <- get_spotify_access_token()

tracks <- get_playlist("PLAYLIST_ID")$tracks$items
ids <- c()
genre <- c()
for (i in 1:nrow(tracks)) {
  ids[i] <- tracks[[27]][[i]]$id
  genre[i] <- get_artists(ids = ids[i])$genre
  shell("cls")
  print(paste("Lendo musica",i,"de",nrow(tracks)))
}

generos <- get_tokens(genre, pattern = "\"")
generos_trat <- generos[generos != "list()" & generos != ", " & generos != "c(" & generos != ")"]

generosdf <- as.data.frame(generos_trat) %>% 
  dplyr::group_by(generos_trat) %>% 
  dplyr::summarise(QUANTIDADE = n()) %>% 
  dplyr::arrange(desc(QUANTIDADE)) %>% 
  # slice(1:10) %>% 
  dplyr::arrange(QUANTIDADE)

plot_generos <- generosdf %>% 
  mutate(generos_trat = fct_reorder(generos_trat, QUANTIDADE)) %>% 
  ggplot( aes(x = generos_trat, y = QUANTIDADE)) +
    geom_bar(stat = "identity", fill= brewer.pal(n = 10, name = "Spectral")) +
    geom_text(aes(label = QUANTIDADE), vjust = 0.3, hjust = -0.5) +
    geom_text(aes(y = 1, label = toupper(generos_trat)), hjust = 0, position = position_fill(), fontface = "bold") +
    coord_flip() +
    xlab("") +
    ylab("Quantidade") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_blank())

plot_generos