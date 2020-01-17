install.packages("googleLanguageR")

library(magrittr)
library(googleLanguageR)

gl_auth()


test_audio <- system.file("woman1_wb.wav", package = "googleLanguageR")
result <- gl_speech(test_audio)


gl_talk_languages(languageCode = "es")
gl_talk("This is my audio player") %>% gl_talk_player()
