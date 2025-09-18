library(shiny)
library(shiny.semantic)
library(data.table)
library(recoder)
library(DT)
library(shinyalert)
library(shinyjs)
library(bslib)



#### UI ####
semanticPage(
  title = "Identify the baboon",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js"),
    #tags$script(src = "jquery-3.7.1.min.js"),  # Include jQuery library from CDN
    #tags$script(src = "semantic.min.js"),
  ),
  suppressDependencies("bootstrap"),

  #### Header ####
  div(img(src = "DPZ.jpg", width = "12%", height = "18%"),
    style = "text-align: left;",

   #### Info ####
    span(
      tags$button(
        class = "right floated circular ui massive info icon button", style = "margin: 10px;",
        tags$i(class = "info icon",
               title = "Info")
      ),
      div(
        class = "ui info modal",
        div(class = "header", "Info"),
        div(
          class = "content",
          fluidRow(
            img(src = "DPZ.jpg", height = "20%", width = "40%", style = "margin-right: 30px"),
            img(src = "logo.png", height = "20%", width = "40%", style = "margin-left: 100px"),
          ),
          uiOutput("info1"),
          #"Test info",
          br(),
          uiOutput("info2")
          #"Test info2"
        )
      ),
      tags$script("$('.info.modal').modal('attach events', '.info.button', 'show');"),
      tags$button(
        id = "restart",
        class = "right floated circular ui massive info icon button", style = "margin: 10px;",
        tags$i(class = "refresh icon",
               title = "Neustart/Restart")
      ),
      tags$button(
        id = "language",
        class = "right floated circular ui massive info icon button", style = "margin: 10px;",
        tags$i(class = "globe icon",
               title = "Sprache/Language")
      ),
      tags$button(
        id = "species",
        class = "right floated circular ui massive info icon button", style = "margin: 10px;",
        tags$i(class = "paw icon",
               title = "Spezies/Species")
      )
    )
  ),


  #### Main ####
  div(
    class = "ui center aligned grid",
    div(
      class = "row",

      #### User Score ####
      div(class = "one wide column"),
      div(
        class = "two wide column",
        div(
          class = "ui inverted circular segment", #style = "padding: 50px 60px;",
          style = "min-width: 250px; min-height: 260px;",
          h2(class = "ui header", style = "text-align: center;", textOutput("playertext")),
          h1(class = "sub header", style = "text-align: center;", textOutput("playscore")),
        ),
        uiOutput("qnumber")
      ),
      div(class = "one wide column"),

      #### Quiz ####
      div(
        class = "eight wide column",
        div(
          class = "ui clearing segment",
           uiOutput("din_css"),
           uiOutput("pokeguess")

          #tags$script("$('.reference.modal').modal('attach events', '.reference.button', 'show');"),
        )
      ),

      #### Computer Score ####
      div(class = "one wide column"),
      div(
        class = "two wide column",
        div(
          class = "ui circular segment", #style = "padding: 50px 40px;",
          style = "min-width: 250px; min-height: 260px;",
          h2(class = "ui header", style = "text-align: center;", "Computer"),
          h1(class = "sub header", style = "text-align: center;", textOutput("compscore"))
        ),
        fluidRow(
          tags$style(HTML(
          ".shiny-notification {
              background-color:#112446;
              color:#FFFFFF;
              height: 100px;
              width: 400px;
              position:fixed;
              top: calc(100% - 600px);;
              left: calc(100% - 500px);;
              font-size: 20px}"
              )),
          uiOutput("spinner")
        ),
      ),
      div(class = "one wide column")
    )
  )
)
