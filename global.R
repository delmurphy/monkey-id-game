library(shiny)
library(data.table)
library(recoder)
library(DT)

#languages: 1  = deutsch, 2 = english
language_val <- 1

#how many questions before the game ends?
turns <- 12

#how long for the model computation
compute_time <- 2

####WWW Tables####
#poketab1 <- data.table::fread('./www/babinfo.csv', na.strings = '')
poketab_b <- data.table::fread('./www/baboontab.csv', na.strings = '', header = TRUE, drop = 1, encoding = 'Latin-1')
poketab_b$chosen <- 0
poketab_m <- data.table::fread('./www/macaquetab.csv', na.strings = '', header = TRUE, drop = 1, encoding = 'Latin-1')
poketab_m$chosen <- 0
poketab_m$orig_targetphoto <- poketab_m$targetphoto
poketab_m$targetphoto <- paste0("monkeys/macaque_targetphotos/Padded_Images/",
                                sub('.*\\/', '', poketab_m$target))
#pokeguesstab1 <- data.table::fread('./www/babids_reduced.csv', na.strings = '')
pokeguesstab_b <- data.table::fread('./www/Baboon_Facts.csv', na.strings = '', encoding = 'Latin-1')
pokeguesstab_b$idphoto <- paste0("monkeys/baboon_idphotos/", pokeguesstab_b$idphoto, sep = "")
pokeguesstab_b <- pokeguesstab_b[pokeguesstab_b$Use!="no",]
pokeguesstab_b$avail <- 1
pokeguesstab_m <- data.table::fread('./www/Macaque_Facts.csv', na.strings = '', encoding = 'Latin-1')
pokeguesstab_m$idphoto <- paste0("monkeys/macaque_idphotos/Cropped_Images/", pokeguesstab_m$idphoto, sep = "")
pokeguesstab_m <- pokeguesstab_m[pokeguesstab_m$Use!="no",]
pokeguesstab_m$avail <- 1
#baboon_info<-list(pokeguesstab$deutsch, pokeguesstab$english)
#pokeguesstab$info <- baboon_info[language_val]

#remove unused photos
unused <- c("ORN.IMG_6234.JPG", "ZZU.IMG_6472.JPG", "KHL.IMG_7153.JPG",
            "TNK.TNK_20210411_PLP1_IMG_0885.JPG", "TNK.TNK_20210814_PLP1_IMG_4769.JPG")
unused <- paste0("monkeys/baboon_targetphotos/", unused)
poketab_b <- poketab_b[!poketab_b$targetphoto %in% unused,]


endphotos_b <- c("BUY.IMG_4944.JPG", "EFF.EFF_20210804_PLP1_IMG_1748.JPG",
               "PLR.IMG_7019.JPG", "PLR.PLR_20210804_PLP1_IMG_1927.JPG",
               "PLR_20210607_PLP1_IMG_5823.JPG", "SCK.IMG_0008.JPG",
               "PLR_20210804_PLP1_IMG_2173.JPG", "CNL.CNL_20210506_PLP1_IMG_3441.JPG")
endphotos_b <- paste0("/monkeys/baboon_targetphotos/", endphotos_b)

endphotos_m <- c("AbuDhabi_3.JPG", "Amsterdam_2.JPG", "FelizDSC2257.JPG", "hinhoiDSC_0337.JPG",
                 "Lluvia_Food_interest.JPG", "NewYearDSC_0912.JPG", "PastelandNeneh_2.JPG", "Peru_9.JPG",
                 "Peru_Apisit.JPG", "wasabi_bunchonumbers.JPG")
endphotos_m <- paste0("/monkeys/macaque_targetphotos/Padded_Images/", endphotos_m)


# Reduce possibilities to choose from (not repeating same correct id in 10 guesses)
#
# correctID - the correct id of the target image
# pokeref - Data.table of the image filepaths
PokeAnswer <- function(correctID, pokeref, pokeguess) {
  pokeref$chosen[pokeref$chosen>0] <- pokeref$chosen[pokeref$chosen>0]+1
  pokeref$chosen[pokeref$id==correctID] <- pokeref$chosen[pokeref$id==correctID]+1
  pokeref$chosen[pokeref$chosen>=10] <- 0
  availids <- unique(pokeref$id[pokeref$chosen==0])
  unavailids <- unique(pokeref$id[pokeref$chosen>0])
  pokeguess$avail[pokeguess$id %in% unavailids] <- 0
  pokeguess$avail[pokeguess$id %in% availids] <- 1
  return(list(pokeref, pokeguess))
}


# Create New Question
#
# pokeguess - Data.table of the available ids
# pokeref - Data.table of image files
PokeQuestion <- function(pokeguess, pokeref) {

  correctID <- sample(pokeguess$id[pokeguess$avail==1], 1)
  decoyID <- sample(pokeguess$id[pokeguess$id != correctID], 1)
  pokeQ <- sample(pokeref$targetphoto[pokeref$id==correctID & pokeref$chosen==0], 1)
  correctName <- pokeguess$name[pokeguess$id==correctID]
  correctInfo <- pokeguess$info[pokeguess$id==correctID]

  return(list(TargetImage = pokeQ, CorrectID = correctID, DecoyID = decoyID,
              CorrectName = correctName, CorrectInfo = correctInfo))
}


#Generate psuedo plot for progress bar:
computing <- function(compute_time) {
  #req(output$main_image)
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  if(language_val == 1){
    progress$set(message = "Der Computer berechnet die Antwort....", value = 0)
  } else {
    progress$set(message = "The model is computing its answer...", value = 0)
  }

  # Number of times we'll go through the loop
  for (i in 1:compute_time) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
    # Increment the progress bar, and update the detail text.
    progress$inc(1/compute_time)
    # Pause for 0.1 seconds to simulate a long computation.
    # Sys.sleep(abs(rnorm(1, 0.05)))
    Sys.sleep(0.5)
  }
}

reset_game <- function(pokereact){
  pokereact$enter <- 0
  pokereact$Question <- 0
  pokereact$PlayScore <- 0
  pokereact$CompScore <- 0
  pokereact$QuestInfo <- list()
}




########################################
# Text

#model_info
model_info_english <- c(
  "Our computer model is based on a machine learning algorithm called a 'convolutional neural network'. It's not always correct, but we are continuing to train it and improve it.",
  "The computer model works by scanning groups of pixels in each image and looking for familiar patterns.",
  "We are currently training the model to recognise the baboons you see in this game. We need hundreds of images of each baboon, taken from all angles, to train the model.",
  "The images come from camera traps placed all over our field site in Senegal.",
  "More than 400 wild baboons live at our field site. We are training the model to recognise about 200 of them to begin with.",
  "As you can see, it is not always easy to recognise individual baboons! The appearance of the baboons can change dramatically as they grow oldergrow older. For adult females, their appearance changes too, according to their reproductive state (e.g. pregnancy)!",
  "We hope to use the model to help with our research, observing wild baboons over the course of their lives and learning about their social relationships."
  )
model_info_deutsch <- c(
  "Unserem Computermodell liegt ein Algorithmus für maschinelles Lernen zugrunde welcher als 'convolutional neural network' bezeichnet wird. Die Ergebnisse sind nicht immer richtig, aber wir trainieren und verbessern das Modell weiter.",
  "Das Computermodell scannt Gruppen von Pixeln in jedem Bild und sucht nach bekannten Mustern.",
  "Derzeit trainieren wir das Modell, um die Paviane zu erkennen, die Sie in diesem Spiel sehen. Wir benötigen Hunderte von Bildern von jedem Pavian, die aus allen Winkeln aufgenommen wurden, um das Modell zu trainieren.",
  "Die Bilder stammen von Kamerafallen, die überall im Gebiet unserer Forschungsstation im Senegal aufgestellt sind.",
  "An unserer Forschungsstation leben mehr als 400 Paviane. Wir trainieren das Modell erstmal zumindest 200 von ihnen zu erkennen.",
  "Wie Du siehst, ist es nicht immer einfach, einzelne Paviane zu erkennen! Das Aussehen der Paviane kann sich stark verändern, wenn sie älter werden. Bei erwachsenen Weibchen ändert sich ihr Aussehen natürlich auch, je nach ihren Reproduktionszustand (z.B. wenn sie schwanger sind)!",
  "Wir hoffen, das Modell für unsere Forschung nutzen zu können, indem wir im Laufe ihres Lebens Daten von den Pavianen sammeln und so mehr über ihre sozialen Beziehungen erfahren."
)
model_info <- list(model_info_deutsch, model_info_english)


#info
info_deutsch <- c(
  "Unsere Forscher entwickeln ein Modell
  für maschinelles Lernen, um einzelne Paviane anhand von Kamerafallenbildern zu identifizieren.
  Die Bilder sind an unserer Forschungsstation im Senegal aufgenommen worden.
  Aber wie einfach oder schwer ist es, einen Pavian von einem anderen zu unterscheiden?
  Nimm die Herausforderung an und teste, ob du besser bist als der Computer!"
  )
info_english <- c(
  "Our researchers are developing a machine learning
  model to help identify individual baboons from camera trap images taken at our field site
  in Senegal. But how easy is it to tell one baboon from another? Take up the challenge and
  see if you can beat the computer!"
  )
info_b <- list(info_deutsch, info_english)

info_deutsch <- c(
  "Forscher aus der Abteilung für Verhaltensökologie beobachten Makaken an unserer Forschungsstation im Thailand. Sie müssen wissen, wie sie jeden Makaken erkennen können
  Aber wie einfach oder schwer ist es, einen Pavian von einem anderen zu unterscheiden?
  Nimm die Herausforderung an und teste, ob du besser bist als der Computer!"
)
info_english <- c(
  "Researchers from the Department of Behavioral Ecology observe macaques at our field site in Thailand. They need to know how to recognise each macaque.
  But how easy is it to tell one macaque from another? Take up the challenge and
  see if you can beat the computer!"
)
info_m <- list(info_deutsch, info_english)

info_deutsch2 <- c("Wählen Sie die Art (Guinea-Paviane oder Assam-Makaken) und die Sprache (Englisch oder Deutsch), setzen Sie das Spiel zurück,
 oder erhalten Sie weitere Informationen über die Schaltflächen in der oberen rechten Ecke. Wenn Du mehr über die Paviane und Makaken, oder unsere Forschung wissen möchtest, frag uns bitte!")
info_english2 <- c("Choose the species (Guinea baboons or Assamese macaques) and language (English or German), reset the game,
  or get more information using the buttons in the top right corner. If you want to know more about the baboons and macaques, or our research, please ask!")
info2 <- list(info_deutsch2, info_english2)

#Rightwrong answers
right_ans <- c("Richtig! \n\nDies ist", "Correct! \n\nThis is")
wrong_ans <- c("Oh nein! Du hast dich vertan! \n\nDies ist", "Oh no! You got it wrong! \n\nThis is")
comp_right_ans <-c("\nAuch der Computer hat richtig geraten!\n", "\nThe computer also guessed right!\n")
comp_right_ans2 <-c("\nDer Computer hat richtig geraten!\n", "\nThe computer guessed right!\n")
comp_wrong_ans <-c("\nDer Computer hat sich vertan!\n", "\nThe computer got it wrong!\n")
comp_wrong_ans2 <-c("\nAuch der Computer hat sich vertan!\n", "\nThe computer also got it wrong!\n")

#warning
warn_b <- c("Bitte wähle einen Pavian", "Please select a baboon")
warn_m <- c("Bitte wähle einen Makaken", "Please select a macaque")

#player icon
player<-c("Spieler", "Player")
enter<-c("Bestätigen", "Enter")

#welcome
welcome1_b <- c("Willkommen zum Pavian-Erkennungsspiel!", "Welcome to the baboon ID game!")
welcome1_m <- c("Willkommen zum Makaken-Erkennungsspiel!", "Welcome to the macaque ID game!")
welcome2 <- c("Kannst du gegen den Computer gewinnen?", "Can you beat our computer model?")
welcome_img <- c("logo-ke.png", "logo-ke-en.png")

#question
q_b <- c("Welcher Pavian ist das?", " Which baboon is in the image above?")
q_m <- c("Welcher Makake ist das?", " Which macaque is in the image above?")

#end message
end_win1 <- c("Herzlichen Glückwunsch!\n", "Congratulations!\n")
end_win2 <- c("\nDu hast gegen den Computer gewonnen!\nWir trainieren das Modell noch.
              Vielleicht wird er eines Tages so gut werden wie du!\n",
              "\nYou beat the computer!\nWe are still training our computer model.
              Hopefully one day it will be as good as you!\n")
end_lose <- c("Diesmal hat der Computer gewonnen, aber das sind gute Nachrichten für uns!
              \nDas sagt uns, dass unser Modell gut funktioniert!\n
              Spiel erneut, um zu sehen, ob du gewinnen kannst\n",
               "The computer beat you this time, but that's good news for us!\nIt tells us our model is working well!
              \nPlay again to see if you can win next time.\n")
end_draw <- c("Der Computer hat genau so gut gespielt wie du! Das sind gute Nachrichten für uns!
              \nDas sagt uns, dass unser Modell gut funktioniert!\n
              Spiel erneut, um zu sehen, ob du gewinnen kannst\n",
              "The computer scored the same as you! That's good news for us!\nIt tells us our model is working well!
              \nPlay again to see if you can win next time.\n")
end_restart <- c("\nUm erneut zu spielen, drücke die Neustart-Taste",
                 "\nTo play again, press the replay button")

replay_text <- c("Neustart", "Play again")

qnum_text1 <- c("Noch", "")
qnum_text2_b <- c("Paviane", "baboons left")
qnum_text3_b <- c("Pavian", "baboon left")
qnum_text2_m <- c("Makaken", "macaques left")
qnum_text3_m <- c("Makake", "macaque left")
