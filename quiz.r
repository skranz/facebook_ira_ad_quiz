# Data source: https://mith.umd.edu/irads/data/

example = function() {
  app = make.fb.quiz.app(3)
  viewApp(app)
}

make.fb.quiz.app = function(max.round = 10, log.file = "log.txt") {
  library(restorepoint)
  library(shinyEvents)
  library(shiny)
  restore.point("make.fb.quiz.app")
  app = eventsApp()
  app$glob$max.round = max.round
  app$glob$log.con = file(log.file, open="at")
  
  #ads = read.csv("items.csv", encoding="UTF-8",stringsAsFactors = FALSE)
  #saveRDS(ads, "ads.Rds")
  ads = readRDS("ads.Rds")
  ads$tag = gsub("|",", ",ads$tag, fixed=TRUE)
  ads$interest = gsub("|",", ",ads$interest, fixed=TRUE)
  keep = ads$impressions > 0
  ads = ads[keep,]
  
  ads$clicks_per_imp = ads$clicks / ads$impressions
  app$glob$ads = ads
  app$glob$n = NROW(ads)
  
  app$ui = main.ui()
  buttonHandler("nextBtn", show.random.ad)
  buttonHandler("resNextBtn", function(...) {
    show.random.ad()
    removeModal()
  })
  customEventHandler(eventId = "adClick",  css.locator = "#ad_img_1, #ad_img_2", event="click", fun = function(id=NULL,..., app=getApp()) {
    args = list(...)
    restore.point("ad_img_click")
    if (id == "ad_img_1") {
      click.ad(1)
    } else {
      click.ad(2)
    }
  })

  appInitHandler(function(..., app=getApp()) {
    app$id = random.string()
    app$points = 0
    app$round = 0
    show.random.ad()
  })
  app
}

click.ad = function(num, app=getApp()) {
  restore.point("click.ad")
  cat("\nClicked")
  
  ad1 = app$glob$ads[app$ind1,]
  ad2 = app$glob$ads[app$ind2,]
  
  try(writeLines(paste0(Sys.time(),",",app$id,",",ad1$identifier,",",ad2$identifier,",",num), app$glob$log.con))
  
  period.format = function(ad) {
    as.character(as.Date(ad$created))
  }
  
  tab.rows = c(
    paste0("<td style='margin-right: 3px; margin-bottom: 3px'>",c("Clicks per 100 impressions",round(ad1$clicks_per_imp*100,1),round(ad2$clicks_per_imp*100,1)),"</td>", collapse="\n"),
    paste0("<td style='margin-right: 3px; margin-bottom: 3px'>",c("Impressions (1000nds)",round(ad1$impressions/1000,1),round(ad2$impressions/1000,1)),"</td>", collapse="\n"),
    paste0("<td style='margin-right: 3px; margin-bottom: 3px'>",c("Start Date",period.format(ad1),period.format(ad2)),"</td>", collapse="\n"),
    paste0("<td style='margin-right: 3px; margin-bottom: 3px'>",c("Tags",ad1$tag,ad2$tag),"</td>", collapse="\n"),
    paste0("<td style='margin-right: 3px; margin-bottom: 3px'>",c("Interests",ad1$interest,ad2$interest),"</td>", collapse="\n")
    
    #paste0("<td>",c("Cost",paste0(ad1$cost, " ", ad1$currency),paste0(ad2$cost, " ", ad2$currency)),"</td>", collapse="\n")
    
  )
  tab = paste0("<table><tr><th td style='margin-right: 3px;'></th><th>Left Ad</th><th>Right Ad</th></tr>",
    paste0("<tr>", tab.rows,"</tr>", collapse="\n"),"</table>")
  
  better.ad = (ad2$clicks_per_imp >= ad1$clicks_per_imp)+1
  
  if (num == better.ad) {
    correct = TRUE
    app$points = app$points+1
  } else {
    correct = FALSE
  }
  
  showModal(modalDialog(
    footer = NULL,
    if (correct)
      p(paste0("Correct guess!"))
    else
      p(paste0("Wrong guess!")),
    p(paste0("Points: ", app$points, " Rounds: ", app$round, " / ", app$glob$max.round)),
    actionButton("resNextBtn", "Continue"),
    br(),br(),
    HTML(tab)
  ))
}

sample.ad = function(ads = app$glob$ads, weights="impressions", weight.fun = function(x) sqrt(x+1000), app=getApp()) {
  restore.point("sample.ad")
  
  sample(1:NROW(ads),1,prob = weight.fun(ads[[weights]]))
}

show.random.ad = function(..., ind1 = sample.ad(),ind2 = sample.ad(), app=getApp()) {
  restore.point("show.random.ad")
  
  app$round = app$round+1
  if (app$round > app$glob$max.round) {
    app$round = 1
    app$points = 0
  }
  if (ind1 == ind2)
    ind2 = ind1
  if (ind2 > app$glob$n)
    ind2 = 1
  
  app$ind1 = ind1
  app$ind2 = ind2
  
  ad1 = make.ad.ui(app$glob$ads[ind1,],1)
  ad2 = make.ad.ui(app$glob$ads[ind2,],2)
  setUI("ad1",ad1)
  setUI("ad2",ad2)
} 

main.ui = function() {
  html = paste0("<h3>Russian sponsored Facebook Ads before US elections</h3><p>
    Below you see 2 random Facebook ads bought by the Russian <a href='https://en.wikipedia.org/wiki/Internet_Research_Agency'>Internet Research Agency</a> mostly before the US presidential elections. Facebook provided the data as <a href='https://intelligence.house.gov/social-media-content/social-media-advertisements.htm'>PDF files</a> to US Congress. This page uses the extracted <a href='https://mith.umd.edu/irads/data/')>csv data</a> from <a href='https://mith.umd.edu/irads/about' target='_blank'>Daniel Pfister and other researchers at the University of Maryland</a>.<br>You can explore the Russian influence campaign with a little game. Guess the ad that had more clicks per impression (just click on it)!</p>")
  fluidPage(
    title="Russian (IRA) Facebook Ad Quiz",
    HTML(html),
    fluidRow(
      column(width=6,uiOutput("ad1")),
      column(width=6,uiOutput("ad2"))
    )
  )
  
  
}

make.ad.ui = function(ad, num=1) {
  tagList(
#    h3(ad$title),
    shiny::img(id=paste0("ad_img_",num), style="max-width: 95%", src=ad$image),
    p(ad$description)
  )
}

