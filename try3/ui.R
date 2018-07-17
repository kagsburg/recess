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
  dashboardHeader(title = "YouTube Analysis",  titleWidth = 350,
                  dropdownMenu( type= "message",messageItem(from = "Analyst",message = "data update"))),
 
  dashboardSidebar(
    sidebarMenu( 
      sidebarSearchForm("searchText","buttonSearch","Search"),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuSubItem("Extract data", tabName = "extract"),
      menuItem("Barcharts", tabName = "charts", icon = icon("bar-chart")),
      menuSubItem("Upload dataset",tabName = "Upload")
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",h2("Datasets basing on regions"),
              fluidRow(valueBoxOutput("likes",width= 3)
                
              ),
              fluidRow(
                box(selectInput("va","Select a suitable dataset",
                                choices = c("Canada"=1,"USA"=2,"Germany"=3,"France"=4)
                        
                ), status = "success", solidHeader=TRUE
                 
                )
              )
             
      ),
      
      # Second tab content
      tabItem(tabName = "charts",
              h2("Visualized data"),
              fluidRow(
                box(selectInput("var","Select a variable from the dataset",
                                choices = c("views"=8,"likes"=9,"dislikes"=10,"comment_count"=11)
      
                              
                                
                                ),background="red" )
              ),
              fluidRow(
                box(plotOutput("myhist"),width = 400, title = "Bar plot based on the different datasets",background ="green")
              ),fluidRow(
                box(downloadButton(outputId="downloadData", label = "Download the plot"),background = "yellow")
              )
      ),
      #3rd tab content
      tabItem(tabName ="extract",h1("One is able to extract YouTube Data"),
              fluidRow(
               # box(selectInput("category_id",
                                #"Select a category_id of the videos",
                                #choices = "CAvideos$category_id")),
                #box(tableOutput("videosout"))
              )
              ),
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
              )
    ))
)

