library(shiny)
library(data.table)
library(recoder)
library(DT)
library(shinyalert)
library(shinycssloaders)
library(shinyjs)

#Define function to use images as radio buttons
radioImages <- function(inputId, images, values, labels){
  radios <- lapply(
    seq_along(images),
    function(i) {
      id <- paste0(inputId, i)
      tagList(
        tags$input(
          type = "radio",
          name = inputId,
          id = id,
          class = "input-hidden",
          value = as.character(values[i])
        ),
        tags$label(
          `for` = id,
          tags$img(
            src = images[i]#, width = 150*1.13, height = 150
          )
        )
      )
    }
  )
  do.call(
    function(...) div(..., class = "shiny-input-radiogroup", id = inputId),
    radios
  )
}

css <- HTML(
  ".input-hidden {",
  "  position: absolute;",
  "  left: -9999px;",
  "}",
  "input[type=radio] + label>img {",
  "  width: 400px;",
  "  height: 304px;",
  "  transition: 500ms all;",
  "}",
  "input[type=radio]:checked + label>img {",
  "  border: 10px solid #5aa832;",
#  "  box-shadow: 0 0 3px 3px #090;", #fff #32a852
  "  transform: rotateZ(-10deg) rotateX(10deg);",
  "}"

)

#### Server####
function(input, output, session) {



  pokereact <- reactiveValues(
    poke_dt = poketab_b, poke_gss_dt = pokeguesstab_b, endphotos = endphotos_b,
    CompScore = 0, PlayScore = 0, Question = 0, QuestInfo = list(), Wrong = 0,
    PlayMessage = "", enter = 0, warn = warn_b, welcome1 = welcome1_b, info = info_b,
    q = q_b, qnum_text2 = qnum_text2_b, qnum_text3 = qnum_text3_b
  )

  species <- reactiveVal(1)

  observeEvent(input$species, {
    speciesval <- input$species
    if(speciesval%%2==1) {
      species(2)
      reset_game(pokereact)
      pokereact$poke_dt <- poketab_m
      pokereact$poke_gss_dt <- pokeguesstab_m
      pokereact$endphotos <- endphotos_m
      pokereact$warn <- warn_m
      pokereact$welcome1 <- welcome1_m
      pokereact$info <- info_m
      pokereact$q <- q_m
      pokereact$qnum_text2 <- qnum_text2_m
    } else {
      species(1)
      reset_game(pokereact)
      pokereact$poke_dt <- poketab_b
      pokereact$poke_gss_dt <- pokeguesstab_b
      pokereact$endphotos <- endphotos_b
      pokereact$warn <- warn_b
      pokereact$welcome1 <- welcome1_b
      pokereact$info <- info_b
      pokereact$q <- q_b
      pokereact$qnum_text2 <- qnum_text2_b
      }
    session$sendCustomMessage("changeBackgroundImage", list())
    #print(pokereact$info[[language()]])
  })

  language <- reactiveVal(1)
  observeEvent(input$language, {
    langval <- input$language
    if(langval%%2==1) { language(2) } else {language(1)}
    reset_game(pokereact)
  })



  # observeEvent(input$language_choice, pokereact$Question <- pokereact$Question +1)
  # observeEvent(input$language_choice, language_val <- input$language_choice)

  output$info1 <- renderUI({
    pokereact$info[[language()]]
    #h1("info")
  })
  output$info2 <- renderUI({
    info2[[language()]]
    #h1("info2")
  })

  # observe({
  #   language_value <- language()
  #   info_text <- pokereact$info[[language_value]]
  #   print(info_text)  # Check what's being retrieved
  # })
  #

  # output$info1 <- renderUI({
  #   info_text <- pokereact$info[[language()]]
  #   # Wrap the text in appropriate UI elements
  #   tags$h2(info_text)
  # })
  #
  # output$info2 <- renderUI({
  #   info_text <- pokereact$info2[[language()]]
  #   # Wrap the text in appropriate UI elements
  #   tags$h2(info_text)
  # })

  # observeEvent(language(), {
  #   info_text <- pokereact$info[[language()]]
  #   if (is.null(info_text)) {
  #     message("Info text is null or not available for the selected language.")
  #     return(NULL)
  #   }
  #   print(info_text)  # Check what's being retrieved
  #   # Wrap the text in appropriate UI elements
  #   output$info1 <- renderUI({
  #     tags$p(info_text)
  #   })
  #
  #   output$info2 <- renderUI({
  #     tags$p(info_text)
  #   })
  # })




  output$targetimg <- renderImage(pokereact$QuestInfo$TargetImage, deleteFile = F)
  output$compscore <- renderText(pokereact$CompScore)
  output$playscore <- renderText(pokereact$PlayScore)
  #output$playmsg <- renderText(pokereact$PlayMessage)
  output$playertext <- renderText(player[language()])

  observeEvent(pokereact$Question, {
    # Load data locally
    pokeref <- pokereact$poke_dt
    pokeguess <- pokereact$poke_gss_dt
    if(language()==1) {
      pokeguess$info <- pokeguess$deutsch
    } else {
      pokeguess$info <- pokeguess$english
    }
    #print(pokeguess$info)

    if (pokereact$Question > 1) {
      correct_ID <- pokereact$QuestInfo$CorrectID
      decoy_ID <- pokereact$QuestInfo$DecoyID
      target <- pokereact$QuestInfo$TargetImage

      # Reduce the table down depending on type
      pAns <- PokeAnswer(correct_ID, pokeref, pokeguess)
      pokeref <- pAns[[1]]
      pokeguess <- pAns[[2]]
      #print(pAns)

      # Add back to reractive pokereact
      pokereact$poke_gss_dt <- pokeguess
      pokereact$poke_dt <- pokeref
    }
    pokereact$QuestInfo <- PokeQuestion(pokeguess, pokeref)
    #print(pokereact$QuestInfo)
    #pokereact$debuginfo <- PokeAnswer(pokereact$QuestInfo$CorrectID, pokeref, pokeguess)[[1]]
  })

  # restart_ui <- tagList(
  #   id = "restart_button",
  #   class = "left floated circular ui info icon button", style = "margin: 10px;",
  #   tags$i(class = "reset icon")
  # )
  #
  # output$restart <- renderUI({
  #   restart_ui
  # })

  # choose_language <- tagList(
  #   wellPanel(
  #     radioImages("language_choice",
  #               images = c("German_button.jpg", "uk flag.png"),
  #               values = c(1, 2)
  #     )
  #   )
  # )
  #
  # observeEvent(input$language_choice, language_val <- input$language_choice)


  output$welcomeimg <- renderUI({
    img(src = welcome_img[language()])
  })
  output$welcome1 <- renderUI({
    h1(pokereact$welcome1[language()])
  })
  output$welcome2 <- renderUI({
    h1(welcome2[language()])
  })
  output$info_s <- renderUI({
    h3(pokereact$info[[language()]])
  })
  output$info2_s <- renderUI({
    h3(info2[[language()]])
  })



  zero_guess_ui <- tagList(
    tags$button(
      id = "zero_guess_button",
      class = "ui massive circular icon button action-button",
      style = "font-size: 4em;",
      tags$i(class = "play icon")
    ),
    uiOutput("welcomeimg"),
    uiOutput("welcome1"),
    uiOutput("welcome2"),
    br(),
    uiOutput("info_s"),
    uiOutput("info2_s")
  )

  #observeEvent(input$language, change_language(pokereact))
  observeEvent(input$restart, reset_game(pokereact))
  observeEvent(input$shinyend, reset_game(pokereact))
  observeEvent(input$zero_guess_button, pokereact$Question <- 1)
  observeEvent(input$ynenter,
               if(length(input$yncheck>0)) {pokereact$Question <- pokereact$Question + 1})

  output$din_css=renderUI({
    tags$head(
      tags$style(HTML(css))
    )
  })

  output$qnumber <- renderUI({
    if(pokereact$Question == 0){
      h1()
    } else if(pokereact$Question < turns){
      h1(paste(qnum_text1[language()], turns+1 - pokereact$Question, pokereact$qnum_text2[language()]),
         style = "margin-left:2px; color: white")
    } else if(pokereact$Question == turns) {
      h1(paste(qnum_text1[language()], turns+1 - pokereact$Question, pokereact$qnum_text3[language()]),
         style = "margin-left:2px; color: white")
    }
    else {
      h1()
    }

  })

  output$pokeguess <- renderUI({
    #if (pokereact$Question == -1) return(choose_language)
    if (pokereact$Question == 0) return(zero_guess_ui)
    if (pokereact$Question == turns+1){
      endpic <- sample(pokereact$endphotos, 1)
      if (pokereact$PlayScore>pokereact$CompScore) {
        return(shinyalert(paste(end_win1[language()], end_win2[language()], end_restart[language()]),
                          imageUrl = endpic, imageHeight = 500, imageWidth = 500,
                          type = "success", inputId = "shinyend", size = "l",
                          confirmButtonText = replay_text[language()]))
      } else if (pokereact$PlayScore<pokereact$CompScore) {
          return(shinyalert(paste(end_lose[language()], end_restart[language()]),
                                  imageUrl = endpic, imageHeight = 500, imageWidth = 500,
                            type = "error", inputId = "shinyend", size = "l",
                            confirmButtonText = replay_text[language()]))
      } else {
        return(shinyalert(paste(end_draw[language()], end_restart[language()]),
                                imageUrl = endpic, imageHeight = 500, imageWidth = 500,
                          type = "info", inputId = "shinyend", size = "l",
                          confirmButtonText = replay_text[language()]))
      }
    }

    pokeref <- pokereact$poke_dt
    pokeguess <- pokereact$poke_gss_dt

    answervalues <- sample(c(pokereact$QuestInfo$CorrectID,pokereact$QuestInfo$DecoyID))
    answername1 <- paste(pokeguess$name[pokeguess$id==answervalues[1]])
    answername2 <- paste(pokeguess$name[pokeguess$id==answervalues[2]])
    answernames <- c(answername1, answername2)
    answerimages1 <- pokeguess$idphoto[pokeguess$id==answervalues[1]]
    answerimages2 <- pokeguess$idphoto[pokeguess$id==answervalues[2]]
    answerimages1 <- gsub("jpg", "JPG", answerimages1)
    answerimages2 <- gsub("jpg", "JPG", answerimages2)
    answerimages <- c(answerimages1, answerimages2)


    tagList(
      img(src = pokereact$QuestInfo$TargetImage, height = 494, width = 650),
      #h4("Which baboon is in the image?"),
      h4(pokereact$q[language()]),
      wellPanel(radioImages("yncheck",
                       images = answerimages,
                       values = answervalues,
                       labels = answernames
                       ),
                # p(paste(answername1, answername2,
                #         sep = "                                                                     "),
                #   div(style="float: none; display: block;"))
                ),
       tags$button(id = "ynenter", style = "text-align: center;",
                   class = "ui big button action-button",
                   enter[language()], #enter[language_val],
                   tags$i(class = "play icon"))
    )


  })

  # observeEvent(input$ynenter, {
  #   pokereact$enter <- pokereact$enter + 1
  # })

  # observeEvent(input$ynenter, {shinyjs::disable("ynenter")})

  # observeEvent(input$shinyenter, {
  #   enable(id = "ynenter")
  # })


   alertsize <- "l"
   observeEvent(input$ynenter, {
     #print(input$yncheck)
  # observeEvent(pokereact$enter, {
  #   if(pokereact$enter>0){
    if(length(input$yncheck)==0) {
      shinyalert(pokereact$warn[language()], type = "warning", inputId = "shinywarning")
      #shinyalert(type = "warning")
      } else {

    correct <- pokereact$QuestInfo$CorrectID
    Computerguess <- sample(c(pokereact$QuestInfo$CorrectID, pokereact$QuestInfo$DecoyID), size = 1,
                            prob = c(0.75, 0.25))
    correctname <- pokereact$QuestInfo$CorrectName
    correctinfo <- pokereact$QuestInfo$CorrectInfo
    correctimg <- pokereact$QuestInfo$TargetImage
    #correctimg <- sub("/Padded_Images", "", correctimg)
    #print(correctinfo)
    #player correct, computer correct
    if(input$yncheck==correct & Computerguess==correct){
      pokereact$PlayScore <- pokereact$PlayScore + 1
      pokereact$CompScore <- pokereact$CompScore + 1
      print(correctimg)
      shinyalert(paste(right_ans[language()], correctname, "\n\n",
                       correctinfo, "\n",comp_right_ans[language()]),
                 type = "success", size = alertsize,
                 imageUrl = pokereact$QuestInfo$TargetImage,
                 imageWidth = 500,
                 imageHeight = 500,
                 inputId = "shinyanswer")
      #shinyalert(paste(right_ans[language_val], correctname, "\n\n",correctinfo), type = "success")
      #player correct, computer wrong
    } else if (input$yncheck==correct & Computerguess!=correct){
      pokereact$PlayScore <- pokereact$PlayScore + 1
      print(correctimg)
      shinyalert(paste(right_ans[language()], correctname, "\n\n",
                       correctinfo, "\n", comp_wrong_ans[language()]),
                 type = "success", size = alertsize,
                 imageUrl = pokereact$QuestInfo$TargetImage,
                 imageWidth = 500,
                 imageHeight = 500,
                 inputId = "shinyanswer")
      #shinyalert(paste(right_ans[language_val], correctname, "\n\n",correctinfo), type = "success")
      #player wrong, computer correct
    } else if (input$yncheck!=correct & Computerguess==correct){
      pokereact$Wrong <- pokereact$Wrong+1
      pokereact$CompScore <- pokereact$CompScore + 1
      wrongindex <- pokereact$Wrong %% length(model_info[[language()]])
      modinfo <- model_info[[language()]][wrongindex]
      print(correctimg)
      shinyalert(paste(wrong_ans[language()], correctname, "\n",
                       comp_right_ans2[language()], "\n", modinfo),
                 type = "error", size = alertsize,
                 imageUrl = pokereact$QuestInfo$TargetImage,
                 imageWidth = 500,
                 imageHeight = 500,
                 inputId = "shinyanswer")
      #player wrong, computer wrong
    } else if (input$yncheck!=correct & Computerguess!=correct){
      pokereact$Wrong <- pokereact$Wrong+1
      wrongindex <- pokereact$Wrong %% length(model_info[[language()]])
      modinfo <- model_info[[language()]][wrongindex]
      print(correctimg)
      shinyalert(paste(wrong_ans[language()], correctname,"\n",
                       comp_wrong_ans2[language()],"\n",modinfo),
                 type = "error", size = alertsize,
                 imageUrl = pokereact$QuestInfo$TargetImage,
                 imageWidth = 500,
                 imageHeight = 500,
                 inputId = "shinyanswer")
      }
    }
  # }
  })


  observeEvent(input$shinyanswer, {
    if(pokereact$Question<=turns){
  # observeEvent(input$ynenter, {
    #progress bar for model
    output$spinner <- renderUI({
      Sys.sleep(0.5)
      withSpinner(uiOutput("plot"), color="white", size = 2.5)
    })

    output$plot <- renderUI({
      computing(compute_time)
    })
    }
  })

}
