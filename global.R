# stuko app
library(shinyEvents)
source("quiz.r")
app = make.fb.quiz.app(10)
appReadyToRun(app)

#viewApp(app)
