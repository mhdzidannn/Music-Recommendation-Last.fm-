rm(list = ls())

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(data.table)
library(shinyWidgets)
library(markdown)
library(tm)
library(wordcloud)
library(memoise)
library(SnowballC)
library(RColorBrewer)

# ----------------------------------------------------------------------------------------------------------

#tag <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tags.csv", 
#                    header=FALSE, sep = ",", stringsAsFactors = FALSE)

#user_ta <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/user_taggedartists.csv", 
#                header=FALSE, sep = ",", stringsAsFactors = FALSE)


#tag_user <- merge(x=user_ta, y=tag, by="tagID")
#print(tag_user)
#write.csv(tag_user,'tag_user.csv')


tag_user_github <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tag_user.csv", 
                            header=FALSE, stringsAsFactors = FALSE)

year <- read_delim("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tag_user.csv", delim = ",") %>% select(year)

year <- unique(year)


# ----------------------------------------------------------------------------------------------------------

lfm_art <- read_delim("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/artists.dat", delim = "\t") %>% select(id, name)


lfm_art$name <- iconv(lfm_art$name, from = "UTF-8", to = "ASCII//TRANSLIT")

lfm_tags <- read_delim("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tags.dat", delim = "\t")


ag_mat <- as.matrix(read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/ag_mat.csv", check.names = FALSE,
                             header=TRUE, sep = ",", stringsAsFactors = FALSE) )


row.names(ag_mat) <- as.numeric(ag_mat[,1])

ag_mat <- ag_mat[,2:ncol(ag_mat)]
# ----------------------------------------------------------------------------------------------------------

art_sim <- as.matrix(read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/art_sim.csv", check.names = FALSE,
                              header=TRUE, sep = ",", stringsAsFactors = FALSE) )

row.names(art_sim) <- as.numeric(art_sim[,1])

art_sim <- art_sim[,2:ncol(art_sim)]
# -------------------------------------------------------------------------------------------------------------

last_sm <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/last_sm.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)

tenrecs <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/user_tenrecs.csv", 
                    header=TRUE, sep = ",", stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------------------------------------------------------------------


artistIDs <- as.numeric(rownames(art_sim) )


a_names <- lfm_art[lfm_art$id %in% artistIDs,]$name
#-----------------------------------------------------------------------------------


a_names <- a_names[a_names != '????']
a_names <- a_names[a_names != '?????']
a_names <- a_names[a_names != '??????']

# Get the list of genre names 

tagIDs <- as.numeric(colnames(ag_mat))

g_names <- lfm_tags[lfm_tags$tagID %in% tagIDs,]$tagValue
# --------------------------------------------------

# Get a list of distinct userID
userIDs <- unique(last_sm$userID)

u_hdr <- paste("Select a User ID (", min(userIDs), " - ", max(userIDs), " )" )


# --------------------------------------------------------------------------------------------------

ui <- shinyUI(fluidPage(
  
  
  tags$style(type="text/css", "#table th {
             display: none;}"), 
  tags$style(type="text/css", "#table2 th {
             display: none;}"), 
  tags$style(type="text/css", "#profileTable th {
             display: none;"), 
  tags$style(type="text/css", "#profileTable {
             max-height: 240px; 
             overflow-y: auto;}"),
  HTML('<footer>Arni, Fauzan & Zaidan</footer>'),
  tags$style(type="text/css", "footer{
             position:absolute;
             bottom:0;
             width:100%;
             height:40px; /* Height of the footer */
             color: white;
             padding: 10px;
             background-color: black;
             z-index: 1000;}"),
  tags$style(type="text/css", "#table tbody td {
             padding: 10px;}"),
  tags$style(type="text/css", "#table2 tbody td {
             padding: 10px;}"),
  tags$style(type="text/css", "#table {
             border-collapse: collapse;
             border-spacing: 10;
             font: normal 14px Century Gothic, sans-serif;}"),
  tags$style(type="text/css", "#table2 {
             border-collapse: collapse;
             border-spacing: 10;
             font: normal 14px Century Gothic, sans-serif;}"),
  
  titlePanel(
    
    h1(style = "font-family: Comic Sans MS; color: whitesmoke; text-align: center ", "~OnJE.fm~ (2005 - 2011)")),  
  
  hr(),
  
  navbarPage("Navbar",
             
             tabPanel("Artist Recommendation",
                      
                      sidebarLayout(
                        sidebarPanel(style = "background-color: orange",
                                     
                                     radioButtons("Rec_Choices", label=strong("Select A Recommendation Method:"),
                                                  choices = list("By Similar Artists (Top 5)" = "art_sim", 
                                                                 "By Genre (Top 5)" = "ag_mat", 
                                                                 "10 Artists Recommended based by the User Preferences" = "tenrecs"),
                                                  selected = "tenrecs"),
                                     
                                     selectInput("d_userID", u_hdr,
                                                 choices = c(Enter_User_ID='', userIDs )),
                                     
                                     strong("Artists The User Have Previously Listened To:"),
                                     DT::dataTableOutput("profileTable")
                                     
                        ), # end sidebarPanel
                        
                        mainPanel(style = "color: black; background-color: orange",
                                  setBackgroundColor("black"),
                                  h3(textOutput("text")),
                                  br(),
                                  uiOutput("selectedItem"),
                                  br(),
                                  tableOutput("table"),
                                  tags$div(id = 'placeholder')
                        ) # end mainPanel
                        
                      )
             ),
             
             
             
             tabPanel("Word Cloud",
                      sidebarLayout(
                        # Sidebar with a slider and selection inputs
                        sidebarPanel(
                          selectInput("selection", "Choose a year:",
                                      choices = year),
                          actionButton("update", "Change"),
                          hr(),
                          sliderInput("freq",
                                      "Minimum Frequency:",
                                      min =5000,  max = 60000, value = 5000),
                          sliderInput("max",
                                      "Maximum Number of Words:",
                                      min = 10,  max = 150,  value = 10)
                        ),
                        
                        # Show Word Cloud
                        mainPanel(
                          plotOutput("plot")
                        )
                      )

                      
             )
  )
  
) # end sidebarLayout
)


# ____server____

server <- shinyServer(function(input, output) {
  
  #############################################
  # Function to create dynamic drop down containing appropriate list to choose from
  
  output$selectedItem <- renderUI({
    if(values$show=='panel1'){
      if (input$Rec_Choices == "ag_mat") {
        selectInput("d_genre", "Select Genre:",
                    choices = c("Select a Genre", sort(g_names) ) )
        
      } else if (input$Rec_Choices == "art_sim") {
        selectInput("d_artsim", "Select Artist:",
                    choices = c("Select an Artist", sort(a_names) ) )
        
      }
    } 
  })
  
  ##############################################
  # Function to generate heading for main panel
  
  output$text<- renderText({
    if(values$show=='panel2'){
      paste("Top 5 Artists Similar to Selected Artist")
    }else{
      if (input$Rec_Choices == "ag_mat") {
        paste("Top 5 Artists in Selected Genre")
        
      } else if (input$Rec_Choices == "art_sim") {
        paste("Top 5 Artists Similar to Selected Artist", input$selectedItem)
        
      } else if (input$Rec_Choices == "tenrecs") {
        paste("10 Artists You May Like")
        
      } # end if
    }
  })
  
  ##############################################    
  # function to generate list of recommended artists depending on
  # the method selected by the user
  
  output$table <- renderTable({
    if (input$Rec_Choices == "ag_mat") {
      # Top 5 Artists in Selected Genre
      
      # set number of artists to recommend
      n_recommended <- 5
      
      # get tagID of genre
      g_tag <- lfm_tags[lfm_tags$tagValue == input$d_genre,]$tagID
      
      # fetch the top N artists:
      # the names of the items are the artist IDs
      g_arecs <- sort(ag_mat[,as.character(g_tag)], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      g_arecs_IDs <- as.numeric(names(g_arecs))
      
      # create list of artist names from artist ID's in list
      g_arec_names <- lfm_art[lfm_art$id %in% g_arecs_IDs,]$name
      
      return(g_arec_names)
      
      ############################################
      
    } else if (input$Rec_Choices == "art_sim") {
      # Top 5 Artists Similar to Selected Artist
      
      n_recommended <- 5
      
      # get name of artist from artist list
      a_val <- lfm_art[lfm_art$name == input$d_artsim,]$id
      
      a_val <- as.numeric(sort(a_val))
      
      # fetch their recommendations: this returns a named vector sorted by similarity
      # the names of the items are the artist IDs
      arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
      
      # extract the artist IDs and convert to numeric
      arecs_IDs <- as.numeric(names(arecs))
      
      # create list of artist names from artist ID's in list
      arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
      
      return(arec_names)
      
      #############################################
      
    } else if (input$Rec_Choices == "tenrecs") {
      # Get 10 Artists You May Like based on similar users
      
      # fetch their recommendations
      urecs <- sort(as.vector(subset(tenrecs, userID == input$d_userID)[2:11]) )
      
      # create list of artist names from artist ID's in list
      rec_names <- subset(lfm_art, id %in% urecs)$name
      
      return(rec_names)
      
    } # end if
    
  }) # end renderTable
  
  #render the profile table
  output$profileTable <- DT::renderDataTable({
    datatable(
      filteredTable_data(), 
      rownames = FALSE, 
      colnames = NULL, 
      selection = "single", 
      options = list(pageLength = -1,dom = 't'))
  })
  
  #Outputs the user datatable selected query
  output$table2 <- renderTable({
    if(length(input$profileTable_rows_selected>0)){
      # rerun the artists top n results
      n_recommended <- 5
      a_val <- lfm_art[lfm_art$name == filteredTable_selected()[[1]][1],]$id
      a_val <- as.numeric(sort(a_val))
      arecs <- sort(art_sim[as.character(a_val),], decreasing = TRUE)[1:n_recommended]
      arecs_IDs <- as.numeric(names(arecs))
      arec_names <- lfm_art[lfm_art$id %in% arecs_IDs,]$name
      return (arec_names)
    }else{
      return("Nothing Selected")
    }
  })
  
  #Keep track of which query system is being applied
  values <- reactiveValues()
  values$show <- 'panel1'
  
  #Grabs the artist value from the data table
  filteredTable_selected <- reactive({
    id <- input$profileTable_rows_selected
    filteredTable_data()[id,]
  })
  
  filteredTable_data <- reactive({
    # get list of previously listened artists for userID
    user_arts <- last_sm$artistID[last_sm$userID == input$d_userID]
    
    # create list of artist names from artist ID's in list
    ul_names <- lfm_art[lfm_art$id %in% user_arts,]$name
    
    # remove any artists that start with ? character
    ul_names <- ul_names[ul_names != '????']
    ul_names <- ul_names[ul_names != '?????']
    ul_names <- ul_names[ul_names != '??????']
    ret <- data.table(sort(ul_names))
  })
  
  # if the table is clicked, perform this UI upheaval
  observeEvent(input$profileTable_rows_selected, {
    values$show <- 'panel2' # toggle observable between 1 and 2
    removeUI(selector = '#table')
    removeUI(selector = '#table2')
    insertUI(selector = '#placeholder',ui = tableOutput('table2'))
  })
  
  # if the radio buttons are clicked, perform this UI upheaval
  observeEvent(input$Rec_Choices, {
    values$show <- 'panel1' # toggle observable between 1 and 2
    removeUI(selector = '#table2')
    removeUI(selector = '#table')
    insertUI(selector = '#placeholder',ui = tableOutput('table'))
  })
  
  # ----------------------------------------------------------------------------------
  
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    #v <- terms()
    #wordcloud_rep(names(v), v, scale=c(4,0.5),
    #              min.freq = input$freq, max.words=input$max,
    #              colors=brewer.pal(8, "Dark2"))
    
    tag_user_github <- read.csv("https://raw.githubusercontent.com/mhdzidannn/Music-Recommendation-Last.fm-/master/tag_user.csv", 
                                header=FALSE, stringsAsFactors = FALSE)
    
    
    #head(tag_user_github)
    
    tag_user_github <- as.character(tag_user_github)
    
    tag_user_github.corpus<-Corpus(VectorSource(tag_user_github))
    
    tag_user_github.corpus <- tag_user_github.corpus%>%
      tm_map(removePunctuation)%>% ##eliminate punctuation
      tm_map(removeNumbers)%>% #no numbers
      tm_map(stripWhitespace)#white spaces
    
    tag_user_github.corpus <- tag_user_github.corpus%>%
      tm_map(tolower)%>% ##make all words lowercase
      tm_map(removeWords, stopwords("english"))
    
    tag_user_github.corpus <- tm_map(tag_user_github.corpus, stemDocument)
    tag_user_github.corpus <- tm_map(tag_user_github.corpus, removeWords, c("progress", "music industry", "singersongwrit", "vocalist", "fuck", "song")) 
    
    tag_user_github.counts<-as.matrix(TermDocumentMatrix(tag_user_github.corpus))
    tag_user_github.freq<-sort(rowSums(tag_user_github.counts), decreasing=TRUE)
    #head(tag_user_github.freq)##what are the top words?
    
    
    set.seed(32) #be sure to set the seed if you want to reproduce the same again
    
    wordcloud(words=names(tag_user_github.freq), freq=tag_user_github.freq, scale=c(3,.5),max.words = input$max, random.order = TRUE, colors=brewer.pal(8, "Dark2"))
    
    
  })
  
}) # end server

shinyApp(ui, server)