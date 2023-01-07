library(shiny)

compatibility <- function(personality1, personality2, return_fascination = TRUE){
  personality_traits <- c("Bookish",   #1
                          "Coward",    #2
                          "Goofball",  #3
                          "Greedy",    #4
                          "Healer",    #5
                          "Hothead",   #6
                          "Leader",    #7
                          "Loner",     #8
                          "Poet",      #9
                          "Romantic",  #10
                          "Snark")     #11
                         #matrix of compatibilities
                         #B, C,Gf,Gr,Hl,Hh,Ld,Ln, P, R, S   
  compat_mat <- matrix(c( 1, 0,-1,-1, 1, 0, 0, 0, 0, 0, 1, #Bookish
                          0, 1, 0, 0, 1,-1, 1,-1, 0, 0, 0, #Coward
                         -1, 0, 1, 1, 0, 0, 0, 0,-1, 0, 1, #Goofball
                         -1, 0, 1, 1, 0, 0, 0, 0, 1,-1, 0, #Greedy
                          1, 1, 0, 0, 1,-1, 0, 0, 0,-1, 0, #Healer
                          0,-1, 0, 0,-1, 1, 1, 0, 0, 1, 0, #Hothead
                          0, 1, 0, 0, 0, 1, 1,-1, 0, 0,-1, #Leader
                          0,-1, 0, 0, 0, 0,-1, 1, 1, 1, 0, #Loner
                          0, 0,-1, 1, 0, 0, 0, 1, 1, 0,-1, #Poet
                          0, 0, 0,-1,-1, 1, 0, 1, 0, 1, 0, #Romantic
                          1, 0, 1, 0, 0, 0,-1, 0,-1, 0, 1  #Snark
  ),  
  nrow = 11)
  #change from character to numeric personality traits if needed
  if(is.numeric(personality1)){
    p1n <- personality1
  } else if(is.character(personality1)){
    p1n <- which(personality_traits %in% personality1)
  } 
  if(is.numeric(personality2)){
    p2n <- personality2
  } else if(is.character(personality2)){
    p2n <- which(personality_traits %in% personality2)
  }
  #initialize
  compatibility <- 0
  for(i in 1:length(p1n)){
    #add effects of hero 1's ith personality stat on hero 2's personality stats
    compatibility <- compatibility + sum(compat_mat[p1n[i], p2n])
  }
  #return compatibility if fascination is not needed
  if(!return_fascination){
    return(compatibility)
  } else{
    fascination <- round(1 + abs(compatibility)/3, digits = 2)
    return(list(compatibility = compatibility, fascination = fascination))
  }
}

matchfinder <- function(personality, n_top = 5){
  personality_traits <- c("Bookish",   #1
                          "Coward",    #2
                          "Goofball",  #3
                          "Greedy",    #4
                          "Healer",    #5
                          "Hothead",   #6
                          "Leader",    #7
                          "Loner",     #8
                          "Poet",      #9
                          "Romantic",  #10
                          "Snark")     #11
  
  n_traits <- length(personality_traits)
  
  #change from character to numeric personality traits if needed
  if(is.numeric(personality)){
    p_n <- personality
  } else if(is.character(personality)){
    p_n <- which(personality_traits %in% personality)
  } 
  
  #list of matrices with all personalities
  all_p <- list()
  #personalities with 2, 3, or 4 traits 50 or higher
  for(i in 1:3){
    all_p[[i]] <- t(utils::combn(n_traits, i+1))
  }
  #find compatibility 
  compat_score <- list()
  for(i in seq_along(all_p)){
    #compatibility scores with all personality combinations of length 2, 3, or 4
    compat_score[[i]] <- apply(X = all_p[[i]], MARGIN = 1, FUN = compatibility, personality2 = p_n, return_fascination = FALSE)
  }
  top_n <- vector(mode = "list", length = length(all_p))
  bot_n <- vector(mode = "list", length = length(all_p))
  #highest and lowest compatibility scores
  top_compat <- vector(mode = "list", length = length(all_p))
  bot_compat <- vector(mode = "list", length = length(all_p))
  #personalities with highest and lowest compatibility scores
  top_person <- vector(mode = "list", length = length(all_p))
  bot_person <- vector(mode = "list", length = length(all_p))
  for(i in seq_along(all_p)){
    ordered <- order(compat_score[[i]], decreasing = TRUE)
    top_n[[i]] <- ordered[1:n_top]
    bot_n[[i]] <- rev(tail(ordered, n_top))
    
    top_compat[[i]] <- compat_score[[i]][top_n[[i]]]
    bot_compat[[i]] <- compat_score[[i]][bot_n[[i]]]
    
    top_person_mat <- matrix(personality_traits[all_p[[i]][top_n[[i]],]], ncol = i+1)
    bot_person_mat <- matrix(personality_traits[all_p[[i]][bot_n[[i]],]], ncol = i+1)
    
    top_person[[i]] <- apply(X = top_person_mat, MARGIN = 1, FUN = paste, collapse = ", ")
    bot_person[[i]] <- apply(X = bot_person_mat, MARGIN = 1, FUN = paste, collapse = ", ")
    
  }
  
  #find fascination
  top_fascin <- lapply(top_compat, FUN = function(x) round(1 + abs(x)/3, digits = 2))
  bot_fascin <- lapply(bot_compat, FUN = function(x) round(1 + abs(x)/3, digits = 2))
  
  #data frames for potential lovers and rivals with 2, 3, and 4 personality stats 50 or higher
  lovers2 <-  data.frame(Compatibility = top_compat[[1]], Fascination = top_fascin[[1]], Personality = top_person[[1]])
  lovers3 <-  data.frame(Compatibility = top_compat[[2]], Fascination = top_fascin[[2]], Personality = top_person[[2]])
  lovers4 <-  data.frame(Compatibility = top_compat[[3]], Fascination = top_fascin[[3]], Personality = top_person[[3]])
  rivals2 <-  data.frame(Compatibility = bot_compat[[1]], Fascination = bot_fascin[[1]], Personality = bot_person[[1]])
  rivals3 <-  data.frame(Compatibility = bot_compat[[2]], Fascination = bot_fascin[[2]], Personality = bot_person[[2]])
  rivals4 <-  data.frame(Compatibility = bot_compat[[3]], Fascination = bot_fascin[[3]], Personality = bot_person[[3]])
  
  return(list(lovers2 = lovers2, lovers3 = lovers3, lovers4 = lovers4,
              rivals2 = rivals2, rivals3 = rivals3, rivals4 = rivals4))
}

# Define UI ----
ui <- fluidPage(
  titlePanel(title = div("Wildermyth Hero Compatibility Calculator", img(src = "psionusoid_logo2_small.png", width = "25px", height = "25px"))),
  navbarPage("Calculator Type",
             tabPanel("2 Hero Compatibility",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Please select all personality stats 50 or greater for each hero:"),
                          checkboxGroupInput("personality1",
                                             h3("Hero 1 Personality"),
                                             choices = list("Bookish" = 1,
                                                            "Coward" = 2,
                                                            "Goofball" = 3,
                                                            "Greedy" = 4,
                                                            "Healer" = 5,
                                                            "Hothead" = 6,
                                                            "Leader" = 7,
                                                            "Loner" = 8,
                                                            "Poet" = 9,    
                                                            "Romantic" = 10, 
                                                            "Snark" = 11),
                                             #selected = c(1,2)
                          ),
                          checkboxGroupInput("personality2",
                                             h3("Hero 2 Personality"),
                                             choices = list("Bookish" = 1,
                                                            "Coward" = 2,
                                                            "Goofball" = 3,
                                                            "Greedy" = 4,
                                                            "Healer" = 5,
                                                            "Hothead" = 6,
                                                            "Leader" = 7,
                                                            "Loner" = 8,
                                                            "Poet" = 9,
                                                            "Romantic" = 10,
                                                            "Snark" = 11), 
                                             #selected = c(5,3)
                          ),
                          width = 4
                        ),
                        
                        mainPanel(
                          htmlOutput(outputId = "compatibility")
                        )
                      )
             ), 
             tabPanel("Matchfinder", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Please select all personality stats 50 or greater for each hero:"),
                          checkboxGroupInput("personality3",
                                             h3("Hero Personality"),
                                             choices = list("Bookish" = 1,
                                                            "Coward" = 2,
                                                            "Goofball" = 3,
                                                            "Greedy" = 4,
                                                            "Healer" = 5,
                                                            "Hothead" = 6,
                                                            "Leader" = 7,
                                                            "Loner" = 8,
                                                            "Poet" = 9,    
                                                            "Romantic" = 10, 
                                                            "Snark" = 11),
                                             #selected = c(1,2)
                          ),
                          sliderInput("n_top", 
                                      label = "Number of Potential Matches in Each Category",
                                      min = 1,
                                      max = 50, 
                                      value = 5
                                      ),
                          width = 3
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column("Best Lovers (2 Traits 50 or higher)", 
                                   #tableOutput("lovers2"), 
                                   reactable::reactableOutput("lovers2"),
                                   width = 6
                            ), 
                            column("Best Rivals  (2 Traits 50 or higher)", 
                                   #tableOutput("rivals2"), 
                                   reactable::reactableOutput("rivals2"),
                                   width = 6
                            )
                          ),
                          fluidRow(
                            column("Best Lovers (3 Traits 50 or higher)", 
                                   #tableOutput("lovers3"), 
                                   reactable::reactableOutput("lovers3"),
                                   width = 6
                            ), 
                            column("Best Rivals  (3 Traits 50 or higher)", 
                                   #tableOutput("rivals3"),
                                   reactable::reactableOutput("rivals3"),
                                   width = 6
                            )
                          ),
                          fluidRow(
                            column("Best Lovers (4 Traits 50 or higher)", 
                                   #tableOutput("lovers4"), 
                                   reactable::reactableOutput("lovers4"),
                                   width = 6
                            ), 
                            column("Best Rivals  (4 Traits 50 or higher)", 
                                   #tableOutput("rivals4"), 
                                   reactable::reactableOutput("rivals4"),
                                   width = 6
                            )
                          )
                        )
                      )
             )
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$compatibility <- renderUI({
    compat <- compatibility(as.numeric(input$personality1), as.numeric(input$personality2), return_fascination = TRUE)
    
    HTML(paste0("Compatibility: ", compat$compatibility, tags$br(),
                "Fascination: ", compat$fascination, tags$br(), tags$br(),
                "Heroes with compatibility >= 4 will likely become lovers while adventuring together." , tags$br(), 
                "Heroes with compatibility <= -2 will likely become rivals while adventuring together." , tags$br(),
                "Fascination scales how quickly relationships build. It is 1 at 0 (neutral) compatibility and increases as compatibility becomes further from 0."))
  })
  
  matches <- reactive(matchfinder(as.numeric(input$personality3), n_top = 50))

  #output$lovers2 <- renderTable({matches()$lovers2[1:input$n_top,]})
  #output$rivals2 <- renderTable({matches()$rivals2[1:input$n_top,]})
  #output$lovers3 <- renderTable({matches()$lovers3[1:input$n_top,]})
  #output$rivals3 <- renderTable({matches()$rivals3[1:input$n_top,]})
  #output$lovers4 <- renderTable({matches()$lovers4[1:input$n_top,]})
  #output$rivals4 <- renderTable({matches()$rivals4[1:input$n_top,]})
  output$lovers2 <- reactable::renderReactable({reactable::reactable(matches()$lovers2[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$lovers2[index,"Compatibility"] >= 4){
                                                                         list(color = "#0069B3")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  output$lovers3 <- reactable::renderReactable({reactable::reactable(matches()$lovers3[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$lovers3[index,"Compatibility"] >= 4){
                                                                         list(color = "#0069B3")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  output$lovers4 <- reactable::renderReactable({reactable::reactable(matches()$lovers4[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$lovers4[index,"Compatibility"] >= 4){
                                                                         list(color = "#0069B3")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  output$rivals2 <- reactable::renderReactable({reactable::reactable(matches()$rivals2[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$rivals2[index,"Compatibility"] <= -2){
                                                                         list(color = "#FD9934")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  output$rivals3 <- reactable::renderReactable({reactable::reactable(matches()$rivals3[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$rivals3[index,"Compatibility"] <= -2){
                                                                         list(color = "#FD9934")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  output$rivals4 <- reactable::renderReactable({reactable::reactable(matches()$rivals4[1:input$n_top,], 
                                                                     #columns = list(Personality = reactable::colDef(minWidth = 150)),
                                                                     rowStyle = function(index){
                                                                       if(matches()$rivals4[index,"Compatibility"] <= -2){
                                                                         list(color = "#FD9934")
                                                                       }
                                                                     },
                                                                     defaultPageSize = 50)})
  
}

#run the app ----
shinyApp(ui = ui, server = server)