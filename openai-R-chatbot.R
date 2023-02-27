## INSTALAR PACOTE 'openai' - disponível apenas para versões acima da 4.2
#install.packages("openai")
library("openai")
shell('cls')

## SETAR A API KEY (crie uma conta no site da openai para gerar uma API Key)
Sys.setenv(OPENAI_API_KEY = "sk-...")

## FAZER A INSTRUÇÃO E REQUISIÇÃO
requisicao_completa <- 'Você é um consultor de musculação. Responda às perguntas educadamente. Dê respostas pequenas, mas informativas. Responda como "CONSULTOR: "'

message(requisicao_completa)

while (TRUE) {
  requisicao_atual <- paste0(requisicao_completa,"\n\nVOCÊ: ",readline(prompt = "\nVOCÊ: "))
  message("\nO robô está digitando...")
  resultado <- create_completion(
    model = "text-davinci-003",
    prompt = requisicao_atual,
    temperature = 0.9,
    max_tokens = 150,
    top_p = 1,
    frequency_penalty = 0.0,
    presence_penalty = 0.6
  )
  shell("cls")
  requisicao_completa <- paste0(requisicao_atual,resultado$choices$text)
  message(requisicao_completa)
}
