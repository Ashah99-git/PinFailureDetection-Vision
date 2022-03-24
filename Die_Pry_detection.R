############################################################################################
#                   installing all the required libraries first                       ######
############################################################################################


library(remotes)

remotes::install_github("dreamRs/capture")

##########################################################################################
#                       Loading the required Libraries                                ####
##########################################################################################

library(remotes)

remotes::install_github("dreamRs/capture")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(odbc)
library(DBI)
library(stringr)
library(capture)
library(reticulate)
library(base64enc)
library(shinyjs)



##########################################################################################
#                      Sourcing the Python file in the system                         ####
##########################################################################################


source_python("python_func.py")


options(shiny.maxRequestSize = 30*1024^2)

time1 <- Sys.time()

time1 <- str_replace(time1, " ", "_")
time1 <- str_remove_all(time1, "[:-]")


##########################################################################################
#    Connects to the PostgreSQL database, changes might be needed to the DBI driver   ####
##########################################################################################


con <- DBI::dbConnect(odbc::odbc(), "postgres")

names <- dbGetQuery(con , "SELECT table_name
  FROM information_schema.tables
 WHERE table_schema='public'
   AND table_type='BASE TABLE'
")
colour <- c('P'="yellow",'0'="#A9A9A9",'1'="purple",'x'="#D3D3D3")

img_path <- c('')

ui <- fluidPage( titlePanel("Automatic Evaluation and Reporting of Dye & Pry"),  shinyjs::useShinyjs(),
                 
 ##########################################################################################
 #                     UI interface of the Home Tab                                    ####
 ##########################################################################################
                 
                 
  navbarPage("", id = "inTabset", theme = shinytheme("sandstone"),
                           tabPanel(value = "panel0", icon("home"),
                           fluidRow(column(width=12, height=12,
                                      br(),
                                      p("Dye and Pry testing is a failure-analysis and quality-control technique performed on solder joints on printed circuit board assemblies (PCBA) to identify defects unique to solder joints, such as: cracks, head-in-pillow defects, and other joint separations.", style="text-align:justify;
                                                                               color:black;
                                                                                   padding:5px;
                                                                              "),

p("This application will help you bifurcate the pins on your PCBs as good or bad. It also has an option to do this process autonomously using machine vision and detect faulty soldered pins. If you are not familiar with the usage of this application, please read and follow the below mentioned steps in order to successfully complete the task.",  style="text-align:justify;
                                                                               color:black;
                                                                                   padding:5px;
                                                                            
                                                                               "),

p("1.	Under the process selection tab, From the drop-down menu select the required BGA grid.",  style="text-align:justify;
                                                                               color:black;
                                                                                 padding:5px;
                                                                               "),
p("2.	After selecting you will get two buttons to choose from. If you want to use machine vision, click on machine vision. If you want to manually define pins, select manual button.",  style="text-align:justify;
                                                                               color:black;
                                                                                 padding:5px;
                                                                               "), 

strong("Manual Button : "),

p("a)	After selecting manual button an editable grid will be displayed. In that you can change status of all the pins as good or bad. By default all the pins will be marked as good.",  style="text-align:justify;
                                                                               color:black;
                                                                              
                                                                               padding:5px;
                                                                               "),
p("b)	After you are done with defining all the pins, you can go ahead and save it on your local machine as a pdf or export it as a xls file.", style="text-align:justify;
                                                                               color:black;
                                                                                 padding:5px;
                                                                              "),

strong("Machine Vision : "),

p("a)	After selecting machine vision button you will be presented with a browse button in which you have to upload the image of respective BGA.", style="text-align:justify;
                                                                               color:black;
                                                                                 padding:5px;
                                                                               "),
p("b)	Click on proceed, and you will be presented with a grid of the selected BGA. You can hover over all the boxes and a preview of that pin will pop up.", style="text-align:justify;
                                                                               color:black;
                                                                                padding:5px;
                                                                               "),
p("c)	Click on the box to change its value as a good or bad pin individually.",  style="text-align:justify;
                                                                               color:black;
                                                                                padding:5px;
                                                                              "),
p("d)	After you are done with defining all the pins, you can go ahead and save it on your local machine as a pdf or export it as a xls file.",  
                                                                        style="text-align:justify;
                                                                               color:black;
                                                                                 padding:5px;
                                                                               "),
                                      actionButton('jumpToP1', 'Proceed')
                                    ))),
                           

##########################################################################################
#                     UI interface for the selection tab                              ####
##########################################################################################



                           tabPanel(value = "panel1", title = "Process Selection",
                                      p("Process Selection"),
                                    fluidPage(
                                    sidebarPanel(selectInput('layout', 'Choose the layout and click select', names), width = 3 , actionButton(inputId = "go", 
                                                                                                                                             label = " Select"),
                                                 actionButton('Clear_input', 'Reset'),
                                      actionButton('jumpToP0', 'BACK'),
                                     
                                      p(""),
                                      br(),
                                    
                                      
                                      shinyjs::hidden(
                                       
                                        div(id = "conditional_button",
                                                       actionButton('jumpToP2', 'Manual Faulty Pin Selection'),
                                                       p(""),
                                                       br(),
                                                       actionButton('cvstart', 'Automatic Faulty Pin Selection'))

                                      )
                                      ),
                                    

        
                                    
                                    mainPanel(
                                      column(width = 9, 
                                             
                                             shinyjs::hidden(
                                               div(
                                               
                                                 id="conditional_upload",
                                                              fluidRow( 
                                                                fileInput("cdmyFile", "Choose a .jpg file", accept = "image/jpg"),
                                                                
                                                                actionButton("yolo", "Proceed"),
                                                                p(""),
                                                                br()
                                                              ))),
                                             
                                             div(id = "image-container", style = "display:flexbox", uiOutput("uploadedimage")))
                                      
                                     ),
                                    
                                   
                                    )
                                    ),


##########################################################################################
#                     UI interface for the Modification and saving tab                ####
##########################################################################################

                          
                           tabPanel(value = "panel2", title = "Modifying and Saving", 
                                      p("Modifying and Saving BGA grid array"),
                                    
                                    sidebarPanel(width = 3 , actionButton('reset_input', 'RESET'), actionButton ("exportR", "Export Results"), p(""), p(strong("Last Selected point")), textOutput("text"),
                                                 verbatimTextOutput("verb"),
                                                 p(""),
                                                 shinyjs::hidden(tags$div(id = 'hovered_image', imageOutput("hoveredimage"))), 
                                                
                                                 p(strong("Selected points on plot")),
                                                 tableOutput("table"),), 
                                    mainPanel(
                                      column(width = 9,br(), br(), br(),br(), br(), br(), 
                                             plotOutput("boxPlot", click = "boxPlot_click", hover = "plot_hover", height = 1200, width = 1200)), 
                                      
                                        
                                      )
                                    ),
                                  

##########################################################################################
#                      UI interface for the report tab                                ####
##########################################################################################
             
                          tabPanel(value = "panel3", title = "Export", sidebarPanel( width = 3 ,downloadButton('downloadtable', 'Export results as csv'),
                            capture_pdf(
                            selector = "#result-block",
                            filename = "results",
                            icon("camera"), "Save as PDF"
                          ),),
                          column(
                            width = 9,
                          tags$div(id = "result-block",
                                   tags$b("Results :"),
                                   plotOutput("finalboxPlot", height = 1200, width = 1200))
                                   
                                   ),
                          
                       
                          
                          )
             
             
                                    )
)

server <- function(input, output, session) {

  
  ##########################################################################################
  #                     Observe events for checking the button clicks                   ####
  ##########################################################################################
  
  observeEvent(input$jumpToP0, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel0")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$exportR, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  observeEvent(input$yolo, {
    showModal(modalDialog("Creating the Plot", footer=NULL))
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
    removeModal()
  })
  
  
  observeEvent(input$Clear_input, { 
    inFile <- NULL
    shinyjs::reset("go")
    shinyjs::reset("cdmyFile")
    shinyjs::hide("conditional_upload")
    shinyjs::hide("conditional_button")
    
    removeUI(
      selector = "#image-container > *",
    )
    
   })
  
  
  
  observeEvent(input$reset_input, { 
    inFile <- NULL
    shinyjs::reset("go")
    shinyjs::reset("cdmyFile")
    shinyjs::hide("conditional_upload")
    shinyjs::hide("conditional_button")
    shinyjs::hide("hovered_image")
    
    removeUI(
      selector = "#image-container > *",
       )
    
    rollback() })
  
  observeEvent(input$cvstart, {
    shinyjs::show("conditional_upload")
    shinyjs::show("image-container")
  })
  
  observeEvent(input$go, {
    shinyjs::show("conditional_button")
  })
  
 
  ##########################################################################################
  #                     Observe event for checking the input image file                 ####
  ##########################################################################################
  
  observeEvent(input$cdmyFile, {
    inFile <- input$cdmyFile
    if (is.null(inFile))
      return()
    
    b64 <- base64enc::dataURI(file = inFile$datapath, mime = "image/png")
    insertUI(
      selector = "#image-container",
      where = "afterBegin",
      ui = img(src = b64, width = 1050, height = 600)
    )
  })
 
  vals <- reactiveValues(x=0,y=0, cap_x =0, cap_y = 0)
  
  observeEvent(input$boxPlot_click, {
    vals$x <- c(vals$x,input$boxPlot_click$x)
    vals$y <- c(vals$y,input$boxPlot_click$y)
    
    
  })
  
  observeEvent(input$go, {
    vals$x <- c(0)
    vals$y <- c(0)
  })
  
   
  ac_pos_x1 <- 0
  ac_pos_y1 <- 0
  
  
  ##########################################################################################
  #                     Render Image function for displaying the hovered pin image      ####
  ##########################################################################################
  
  output$hoveredimage <- renderImage({
    
    inFile <- NULL
    len <- length(vals$x)

    
    ac_pos_x1 <- input$plot_hover$x
    ac_pos_y1 <- input$plot_hover$y
    
    if(is.null(ac_pos_x1)){
      ac_pos_x1 <- 0
      ac_pos_y1 <- 0

    }
    
    
    inFile <- input[["cdmyFile"]]     ###inFile consists the temporary file location of the input image, usually stored in the system temp folder(for windows)
   
    
    if(!is.null(inFile)){
      
    
      path2 <- inFile$datapath
      
      path3 <- gsub('.{5}$', '', path2)
      
      list(
        
        src = file.path(path3, paste0(ceiling(ac_pos_x1),"_",ceiling(ac_pos_y1),".jpg")),   ## sources the pin images from the temporary folder
        
        contentType = "image/jpeg",
        width = 300,
        height = 300
      )
    }

  }, deleteFile = FALSE)
  

  ##########################################################################################
  #                     Triggers the Pin detection object model                         ####
  ##########################################################################################
  
  
  
  observeEvent(input$yolo, {
    inFile <- input[["cdmyFile"]]
    shinyjs::show("hovered_image")
    showModal(modalDialog("Creating the Plot", footer=NULL))
    
    cnr_cord_1 <-yolo_detection(inFile$datapath)       ## Calls the Python function for Pin detection             
 
    cnr_cord <- as.numeric(unlist(cnr_cord_1))
    
    # Looping through the detected pins
    
    query1 <- paste0("CREATE SCHEMA IF NOT EXISTS ",data())
    dbGetQuery(con, query1)
    query2 <- paste0("CREATE TABLE IF NOT EXISTS ",data(),".d",time1," AS TABLE ",data())
    dbGetQuery(con, query2)

    query <- paste0("SELECT well_position_x,well_position_y FROM ",data()," ORDER BY well_position DESC LIMIT 1" )
    row_col <- (dbGetQuery(con, query))
    a <- (row_col$well_position_x)
    b <- (row_col$well_position_y)
    
    dis_x = ((cnr_cord[2] - cnr_cord[1]) / a)
    dis_y = ((cnr_cord[4] - cnr_cord[3] )/b)
    
    y_cord = cnr_cord[3]
    for (i in 1:a)
    {
      x_cord = cnr_cord[1]
      for (j in 1:b)
      {
        query <- paste0("SELECT * FROM ",data()," where well_position_x = ",i," and well_position_y = ",j)
        draw_c1 <- dbGetQuery(con, query)
        draw_c = draw_c1[, c("draw_colour")]
        if (draw_c == '0' | draw_c == '1')
        {
          bbox_p1 <- well_pos(x_cord,y_cord)
          bbox_p <- as.numeric(unlist(bbox_p1))
          if (bbox_p[1] != 0)
          {
            yolov4_m2(bbox_p[1], bbox_p[2], bbox_p[3], bbox_p[4],inFile$datapath, j ,i)
          }
        } 
        x_cord=x_cord + dis_x
      }
      y_cord=y_cord+dis_y
    }
  
    removeModal()
  })

  
  ##########################################################################################
  #                    Download Handler for downloading the table in form of CSV        ####
  ##########################################################################################
  
  
  output$downloadtable<-downloadHandler(
    #### called from UI
    
    filename = function() {paste(data(), '.csv', sep='')},
  
    content =  function(file) {
      
      table <- paste0("SELECT * FROM ",data(),".d",time1," ORDER BY well_position ASC")
      
      data1 <- dbGetQuery(con, table)
      
      file1 <- paste(data(), '.csv', sep='')
      
      write.csv(data1,file)

    }
  )
  
  observeEvent(input$go, {
    data <- eventReactive(input$go,  {
      (input$layout) 
    })
  })
  
  

  vals <- reactiveValues(x=0,y=0, cap_x =0, cap_y = 0)
  
  vals$Dat <- data.frame(x = numeric(),
                         y = c())
  
  ac_pos_x=0 
  ac_pos_y=0
  postn=0
  len=0
  
  cap_x = 0
  cap_y = 0
  
  z <- c()
  ac_pos_x_1 <- c()
  ac_pos_y_1 <- c()
  
  
  data <- eventReactive(input$go,  {
    (input$layout) 
  })
  
  observeEvent(input$go, {
    vals$x <- c(0)
    vals$y <- c(0)
  })
  
  observeEvent(input$boxPlot_click, {
    vals$x <- c(vals$x,input$boxPlot_click$x)
    vals$y <- c(vals$y,input$boxPlot_click$y)
    
    
  })
  
  ##########################################################################################
  #                     Reset Function                                                  ####
  ##########################################################################################
  
  
  rollback <- reactive({
    
    {
      inFile <- input[["cdmyFile"]]
      if(!is.null(input[["cdmyFile"]])){
      reset_files(inFile$datapath)
      }
      
      updateTabsetPanel(session, "inTabset",
                        selected = "panel1")}
    
    query <- paste0("SELECT well_position_x,well_position_y FROM ",data()," ORDER BY well_position DESC LIMIT 1" )
    row_col <- (dbGetQuery(con, query))
    
    
    len <- length(vals$x)
    ac_pos_x <- vals$x[len]
    ac_pos_y <- vals$y[len]
    
    a <- (row_col$well_position_x)
    b <- (row_col$well_position_y)
    
    postn = ceiling(ac_pos_x) +  a*floor(ac_pos_y)
    
    list = a*b
    
    for (i in 1:list)
    {
    query5 <- paste0("update ",data(),".d",time1," set draw_colour = 
                       CASE WHEN draw_colour = '1' THEN '0' ELSE draw_colour END where well_position = ",i)
    row_col6 <- (dbGetQuery(con, query5))
    }
   
  })
  
  ##########################################################################################
  #                    Creates the final Report i.e. the plot                           ####
  ##########################################################################################
  
  Report <- reactive({
    
    table <- paste0("SELECT * FROM ",data(),".d",time1," ORDER BY well_position ASC")
    
    data1 <- dbGetQuery(con, table)
    
    pos = data1[, c("draw_colour")]
    
    
    query <- paste0("SELECT well_position_x,well_position_y FROM ",data(),".d",time1," ORDER BY well_position DESC LIMIT 1" )
    row_col1 <- (dbGetQuery(con, query))
    
    
    len <- length(vals$x)
    ac_pos_x <- vals$x[len]
    ac_pos_y <- vals$y[len]
  
    
    a <- (row_col1$well_position_x)
    b <- (row_col1$well_position_y)
    
    list = a*b
    
    extend <- function(alphabet) function(i) {
      base10toA <- function(n, A) {
        stopifnot(n >= 0L)
        N <- length(A)
        j <- n %/% N 
        if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
      }   
      vapply(i-1L, base10toA, character(1L), alphabet)
    }
    MORELETTERS <- extend(LETTERS)  
    
    if (a > b)
    {
      d = a
    }
    else
    {
      d = b
    }
    #par(mai=c(0,0,0,0))
    plot(1,ylim=c(b,0),xlim=c(0,a),type='n',yaxs='i',xaxs='i',ylab='', xlab='')
    axis(3, at = 0 : (a-1), labels = MORELETTERS(1:a), pos = 0)
    for (i in 0:d) 
    {
      abline(v=i)
      abline(h=i)
    }
    postn = 0
    posn = 0
    
    for (k in 1 : b)
    { 
      
      for (n in 1 : a)
      {
        posn = posn + 1
        rect(floor(n - 0.5),floor(k - 0.1),ceiling(n - 0.5),ceiling(k - 0.1),col= as.character(colour[pos[posn]]), border = "white")
      }
    }
    
  })
  
  
  ##########################################################################################
  #                     Creates the plot for modification                               ####
  ##########################################################################################
  
  
  thePlot <- reactive({
    
   
    
    len <- length(vals$x)
    ac_pos_x <- vals$x[len]
    ac_pos_y <- vals$y[len]
    
    
    # create a copy of the schema selected
    
    query1 <- paste0("CREATE SCHEMA IF NOT EXISTS ",data())
    dbGetQuery(con, query1)
    query2 <- paste0("CREATE TABLE IF NOT EXISTS ",data(),".d",time1," AS TABLE ",data())
    dbGetQuery(con, query2)
    
    query <- paste0("SELECT well_position_x,well_position_y FROM ",data()," ORDER BY well_position DESC LIMIT 1" )
    row_col <- (dbGetQuery(con, query))
    
    
    table <- paste0("SELECT * FROM ",data(),".d",time1," ORDER BY well_position ASC")
    
    data1 <- dbGetQuery(con, table)

    pos = data1[, c("draw_colour")]
    
    a <- (row_col$well_position_x)
    b <- (row_col$well_position_y)
    
    extend <- function(alphabet) function(i) {
      base10toA <- function(n, A) {
        stopifnot(n >= 0L)
        N <- length(A)
        j <- n %/% N 
        if (j == 0L) A[n + 1L] else paste0(Recall(j - 1L, A), A[n %% N + 1L])
      }   
      vapply(i-1L, base10toA, character(1L), alphabet)
    }
    MORELETTERS <- extend(LETTERS)  
    
    
    list = a*b
    
    if (a > b)
    {
      d = a
    }
    else
    {
      d = b
    }
   
    plot(1,ylim=c(b,0),xlim=c(0,a),type='n',yaxs='i',xaxs='i',ylab='', xlab='')
    axis(3, at = 0 : (a-1), labels =  MORELETTERS(1:a), pos = 0)
    for (i in 0:d) 
    {
      abline(v=i)
      abline(h=i)
    }
    postn = 0
    posn = 0
  
    for (k in 1 : b)
    { 
      
      for (n in 1 : a)
      {
        posn = posn + 1
        postn = ceiling(ac_pos_x) +  a*floor(ac_pos_y)
        rect(floor(ac_pos_x - 0.1),floor(ac_pos_y - 0.1),ceiling(ac_pos_x - 0.1),ceiling(ac_pos_y - 0.1),col='purple', border = "white")
        rect(floor(n - 0.5),floor(k - 0.1),ceiling(n - 0.5),ceiling(k - 0.1),col= as.character(colour[pos[posn]]), border = "white")
      }
    }
    # Query to update the Database with User Value
    query3 <- paste0("update ",data(),".d",time1," set draw_colour = 
                       CASE WHEN draw_colour = '1' THEN '0' WHEN draw_colour = '0' 
                       THEN '1' ELSE draw_colour END where well_position = ",postn)
    
    dbGetQuery(con, query3)
    
    # Fetch from database again
    data1 <- dbGetQuery(con, table)
    pos = data1[, c("draw_colour")]
    posn = 0
    #  update the ui with latest edits
    for (k in 1 : b)
    { 
      for (n in 1 : a)
      {
        posn = posn + 1
        rect(floor(n - 0.5),floor(k - 0.1),ceiling(n - 0.5),ceiling(k - 0.1),col= as.character(colour[pos[posn]]), border = "white")
      }
    }
    
    query4 <- paste0("SELECT well_position_x,well_position_y FROM ",data(),".d",time1," WHERE draw_colour = '1' ORDER BY well_position" )
    row_col5 <- (dbGetQuery(con, query4))

    # add row to the data.frame
    vals$Dat <- rbind(vals$DT, row_col5)
    
    ## 5. render a table of the growing dataframe ##
    output$table <- renderTable({
      vals$Dat})
    
    output$text <- renderText({paste0(ceiling(ac_pos_x), ', ' , ceiling(ac_pos_y), '\n') })
    
  }
  )
  
  
  ##########################################################################################
  #                     Renders both the plot                                           ####
  ##########################################################################################
  
  output$boxPlot  <- renderPlot({
    thePlot()
    },  width = 1000,
  height = 1000)
  
  output$finalboxPlot  <- renderPlot({
    Report()
  },  width = 1000,
  height = 1000)
  
  
  
}

shinyApp(ui, server)