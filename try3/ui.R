#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

library(shinydashboard)


ui <- dashboardPage( skin = "red",
  dashboardHeader(title = "YouTube Data Analysis",  titleWidth = 350,
                  dropdownMenu( type= "message",messageItem(from = "Analyst",message = "data update"))),
 
  dashboardSidebar(
    sidebarMenu( 
      sidebarSearchForm("searchText","buttonSearch","Search"),
      menuItem("Home", tabName = "dashboard", icon = icon("fas fa-home")),
      menuItem("Plots", tabName = "charts", icon = icon("bar-chart")),
      menuItem("Trending",tabName = "trend",icon = icon("far fa-fire")),
      menuItem("Upload dataset",tabName = "Upload",icon=icon("fas fa-upload")),
     
      menuItem("Sentiment Analysis",tabName="senti",icon = icon("far fa-paper-plane"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",h2("DATASETS PRESENT FOR ANALYSIS"),
              fluidRow(valueBoxOutput("likes",width= 3),valueBoxOutput("category",width = 4)
                
              ),
              fluidRow(
                box(selectInput("va1","Select a suitable dataset",choices = c("Canada"=1,"USA"=2,"Germany"=3,"France"=4)
                              
                        
                ),#submitButton("Enter"), 
                status = "success", solidHeader=TRUE
                 
                )
              ),
              fluidRow(
                box(selectInput("var3","Select a variable from the dataset",
                                choices = c("views"=8,"likes"=9,"dislikes"=10,"comment_count"=11)
                                
                                
                                
                ),background="red")
              ),
              fluidRow(
                box(
                  tableOutput("summary")
                )
              )
             
      ),
      
      # Second tab content
      tabItem(tabName = "charts",
              h2("Visualized data"),
              fluidRow(
                box(selectInput("var1","Select a variable from the dataset",
                                choices = c("views"=8,"likes"=9,"dislikes"=10,"comment_count"=11)
      
                              
                                
                                ),background="red"#submitButton("Enter") 
                    )
              ),
              fluidRow(
                tabBox(width = 13,
                  tabPanel("Barplot",plotOutput("myhist"), title = "Bar plot based on the different datasets",background ="red"),
                  tabPanel("categories", tableOutput("descpt")))
                
              ),fluidRow(
                box(downloadButton(outputId="downloadData", label = "Download the plot"),background = "yellow")
              )
      ),
      #3rd tab content
     
      tabItem(tabName = "Upload",multiple=T,
              fluidRow(
               box(
                  fileInput("file1","upload the file"),
                  radioButtons("sep","Seperator", choices = c(Comma=',',Period='.',Tilde="~",minus="-")),
                  checkboxInput("header","Header?")
                 # 
                )
               
              ),
              fluidRow(box(tableOutput("input_file")))
              ),
     
     #trending panel
     tabItem(
       tabName = "trend",fluidRow(
         box(selectInput("var2","Select a variable from the dataset",
                         choices = c("views and likes"=8)
                         
                         
                         
         ),background="red" 
         )),fluidRow(box(tableOutput("trending"),width = 10))
     )
     ,
     #sentiment panel
      tabItem(tabName = "senti",
              fluidRow(
                box(
                 textInput("api","Insert your api key"),
                  textInput("video_id","insert a video id")
                 
                )
               
              ), fluidRow(
                box(
                  plotOutput("sentiment"), title = "Sentiment scores of the Youtube comments "
                  ),
                box(plotOutput("word"))
              ),
              fluidRow(
                box(plotOutput("chat"))
              )
              )
    ))
)

