#install.packages("openai")
library("openai")
shell('cls')
Sys.setenv(OPENAI_API_KEY = "sk-...")

requisicao_completa <- 'Você é um consultor de musculação. Responda às perguntas educadamente. Dê respostas pequenas, mas informativas. Responda como "CONSULTOR: "'

message(requisicao_completa)

while (TRUE) {
  requisicao_atual <- paste0(requisicao_completa,"\n\nVOCÊ: ",readline(prompt = "\nVOCÊ: "))
  message("\nO robô está digitando...")
  result <- create_completion(
    model = "text-davinci-003",
    prompt = requisicao_atual,
    temperature = 0.9,
    max_tokens = 150,
    top_p = 1,
    frequency_penalty = 0.0,
    presence_penalty = 0.6
  )
  shell("cls")
  requisicao_completa <- paste0(requisicao_atual,result$choices$text)
  message(requisicao_completa)
}