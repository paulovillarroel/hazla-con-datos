library(mall)

ollamar::pull("llama3.2")

texts_diag <- c("carcinoma pulmonar LSI", "lesión por bala abdominal", "masa testicular de gran tamaño")
texts_diag <- as.data.frame(texts_diag)

my_prompt <- paste(
  "Answer a question.",
  "Return only the answer, no explanation",
  "Acceptable answers are ONLY 'SI', 'NO'",
  "Answer this about the following text, is an oncological diagnosis or can be related to it?:"
)

texts_diag |> 
  llm_custom(texts_diag, my_prompt)


