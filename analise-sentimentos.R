### CARREGANDO PACOTES
library(syuzhet)
library(rlang)
library(vctrs)
library(tm)
library(dplyr)
library(openai)
library(RColorBrewer)

### API KEY
Sys.setenv(OPENAI_API_KEY = "API KEY AQUI")

analisePalavras <- function(texto) {
  ## baseado no artigo https://marcusnunes.me/posts/analise-de-sentimentos-com-r-bojack-horseman-vs-brooklyn-99/
  texto_corpus <- VCorpus(VectorSource(texto))
  texto_corpus <- tm_map(texto_corpus, content_transformer(tolower))
  texto_corpus <- tm_map(texto_corpus, removePunctuation)
  texto_corpus <- tm_map(texto_corpus, removeNumbers)
  texto_corpus <- tm_map(texto_corpus, removeWords, stopwords("portuguese"))
  texto_corpus <- tm_map(texto_corpus, stripWhitespace)
  texto_corpus <- TermDocumentMatrix(texto_corpus)
  texto_corpus_matrix <- as.matrix(texto_corpus)
  
  lemma_dic <- read.delim(file = "lemmatization-pt.txt", header = FALSE, stringsAsFactors = FALSE)
  names(lemma_dic) <- c("stem", "term")
  
  palavras <- row.names(texto_corpus_matrix)
  
  for (i in 1:length(palavras)){
    comparacao <- palavras[i] == lemma_dic$term
    if (sum(comparacao) == 1){
      palavras[i] <- as.character(lemma_dic$stem[comparacao])
    } else {
      palavras[i] <- palavras[i]
    }
  }
  
  palavras_texto <- unique(palavras)
  palavras_traduzir <- as.String(palavras_texto)
  ## TRADUZIR
  requisicao <- paste0("Traduza para o inglês as seguintes palavras (apenas uma por linha): ",palavras_traduzir)
  
  result <- create_completion(
    model = "text-davinci-003",
    prompt = requisicao,
    temperature = 0.9,
    max_tokens = 1000,
    top_p = 1,
    frequency_penalty = 0.0,
    presence_penalty = 0.6
  )
  
  words <- result$choices$text
  
  words <- get_tokens(words, pattern = "\n")
  sentimentos_df <- get_nrc_sentiment(words)
  
  row.names(sentimentos_df) <- palavras_texto
  colnames(sentimentos_df) <- c("Raiva","Antecipação","Aversão","Medo","Alegria","Tristeza","Surpresa","Confiança","Negativo","Positivo")
  num_sentimentos_df <- sentimentos_df
  
  sentimentos_df$Raiva[sentimentos_df$Raiva != 0] <- "remete a"
  sentimentos_df[,"Raiva"][sentimentos_df$Raiva == 0] <- "-"
  
  sentimentos_df$Antecipação[sentimentos_df$Antecipação != 0] <- "remete a"
  sentimentos_df[,"Antecipação"][sentimentos_df$Antecipação == 0] <- "-"
  
  sentimentos_df$Aversão[sentimentos_df$Aversão != 0] <- "remete a"
  sentimentos_df[,"Aversão"][sentimentos_df$Aversão == 0] <- "-"
  
  sentimentos_df$Medo[sentimentos_df$Medo != 0] <- "remete a"
  sentimentos_df[,"Medo"][sentimentos_df$Medo == 0] <- "-"
  
  sentimentos_df$Alegria[sentimentos_df$Alegria != 0] <- "remete a"
  sentimentos_df[,"Alegria"][sentimentos_df$Alegria == 0] <- "-"
  
  sentimentos_df$Tristeza[sentimentos_df$Tristeza != 0] <- "remete a"
  sentimentos_df[,"Tristeza"][sentimentos_df$Tristeza == 0] <- "-"
  
  sentimentos_df$Surpresa[sentimentos_df$Surpresa != 0] <- "remete a"
  sentimentos_df[,"Surpresa"][sentimentos_df$Surpresa == 0] <- "-"
  
  sentimentos_df$Confiança[sentimentos_df$Confiança != 0] <- "remete a"
  sentimentos_df[,"Confiança"][sentimentos_df$Confiança == 0] <- "-"
  
  sentimentos_df$Negativo[sentimentos_df$Negativo != 0] <- "é"
  sentimentos_df[,"Negativo"][sentimentos_df$Negativo == 0] <- "-"
  
  sentimentos_df$Positivo[sentimentos_df$Positivo != 0] <- "é"
  sentimentos_df[,"Positivo"][sentimentos_df$Positivo == 0] <- "-"
  
  print(sentimentos_df)
  
  analiseEmocoes <- barplot(
    colSums(prop.table(num_sentimentos_df[, 1:8])),
    space = 0.5,
    horiz = FALSE,
    las = 1,
    cex.names = 1,
    col = brewer.pal(n = 8, name = "Spectral"),
    main = "Análise de emoções por palavra",
    xlab="Emoções", ylab = "Frequência %")
}

analiseSentencas <- function(texto) {
  s_v <- get_sentences(texto)
  
  ## TRADUZIR
  requisicao <- paste0("Traduza para o inglês as textos: ",s_v)
  
  result <- create_completion(
    model = "text-davinci-003",
    prompt = requisicao,
    temperature = 0.9,
    max_tokens = 1000,
    top_p = 1,
    frequency_penalty = 0.0,
    presence_penalty = 0.6
  )
  
  sentences <- result$choices$text
  
  # TRAJETÓRIA SENTIMENTAL
  s_v_sentiment <- get_sentiment(sentences, method = "syuzhet", lang = "english")
  
  trajetoria <- plot(
    s_v_sentiment, 
    type="l", 
    main="Trajetória emocional", 
    xlab = "Andamento da texto", 
    ylab= "Valor emocional",
    col = "red"
  )
  
  print(s_v_sentiment)
}

## Texto de exemplo extraído do conto 'A Causa Secreta', de Machado de Assis
texto <- "Garcia inclinou-se ainda para beijar outra vez o cadáver; mas então não pôde mais. O beijo rebentou em soluços, e os olhos não puderam conter as lágrimas, que vieram em borbotões, lágrimas de amor calado, e irremediável desespero. Fortunato, à porta, onde ficara, saboreou tranqüilo essa explosão de dor moral que foi longa, muito longa, deliciosamente longa."
analisePalavras(texto)
analiseSentencas(texto)
