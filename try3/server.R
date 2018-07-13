#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
dat1 <-read.csv(file = "CAvideos.csv", nrows= 40000, header= T)
dat2 <-read.csv(file = "USvideos.csv", nrows= 40000, header= T)
dat3 <-read.csv(file = "DEvideos.csv", nrows= 40000, header= T)
dat4 <-read.csv(file = "FRvideos.csv", nrows= 40000, header= T)
library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
options(shiny.maxRequestSize= 60^1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
  output$videosout<- renderTable({
    category_idFilter <- subset("videos",videosout$category_id == input$category_id)
   
  })
  output$input_file <- renderTable({
    file_to_read = input$file1
    if(is.null(file_to_read)){
      return(NULL)
    }
    read.csv(file_to_read$datapath, sep = input$sep, header=input$header)
  })
  #output$summary <- renderTable({
    #mydata<- read.csv("~/Desktop/VisionGroupCiMJuly.csv", header = TRUE)
    #mydata <- input$file1
   # read.csv( mydata$datapath, header = input$header)
    #summary(mydata)
   # })
  output$myhist <- renderPlot({
    second <- input$va
    first <- input$var
    if(second==1){
    if(first==8){
  dat1 %>% select(category_id,views) %>% group_by(category_id) %>%ggplot(
      aes(category_id,views,fill=views ))+geom_col()+ggtitle("The Number of Views in the different categories of videos")+
         labs(x ='Category id', y ='Views')
    } 
    else if(first==9){
      dat1 %>% select(category_id,likes) %>% group_by(category_id) %>%ggplot(
        aes(category_id,likes))+geom_col()+ggtitle("The Number of Likes in the different categories of videos")+
        labs(x ='Category id', y ='Likes')
      
    } 
   else if(first==10){
      dat1 %>% select(category_id, dislikes) %>% group_by(category_id) %>%ggplot(
        aes(category_id,dislikes, fill=dislikes ))+geom_col()+ggtitle("The Number of dislikes in the different categories of videos")+
       labs(x ='Category id', y ='Dislikes')
    }
    else if(first==11){
      dat1 %>% select(category_id,comment_count) %>% group_by(category_id) %>%ggplot(
        aes(category_id,comment_count,fill= comment_count ))+geom_col()+ggtitle("The Number of Comments in the different categories of videos")+
        labs(x ='Category id', y ='Comments')
    }}
    else if(second==2){
      if(first==8){
        dat2 %>% select(category_id,views) %>% group_by(category_id) %>%ggplot(
          aes(category_id,views,fill=views ))+geom_col()+ggtitle("The Number of Views in the different categories of videos")+
          labs(x ='Category id', y ='Views')
      } 
      else if(first==9){
        dat2 %>% select(category_id,likes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,likes))+geom_col()+ggtitle("The Number of Likes in the different categories of videos")+
          labs(x ='Category id', y ='Likes')
        
      } 
      else if(first==10){
        dat2 %>% select(category_id, dislikes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,dislikes, fill=dislikes ))+geom_col()+ggtitle("The Number of dislikes in the different categories of videos")+
          labs(x ='Category id', y ='Dislikes')
      }
      else if(first==11){
        dat2 %>% select(category_id,comment_count) %>% group_by(category_id) %>%ggplot(
          aes(category_id,comment_count,fill= comment_count ))+geom_col()+ggtitle("The Number of Comments in the different categories of videos")+
          labs(x ='Category id', y ='Comments')
      }}
    else if(second==3){
      if(first==8){
        dat3 %>% select(category_id,views) %>% group_by(category_id) %>%ggplot(
          aes(category_id,views,fill=views ))+geom_col()+ggtitle("The Number of Views in the different categories of videos")+
          labs(x ='Category id', y ='Views')
      } 
      else if(first==9){
        dat3 %>% select(category_id,likes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,likes))+geom_col()+ggtitle("The Number of Likes in the different categories of videos")+
          labs(x ='Category id', y ='Likes')
        
      } 
      else if(first==10){
        dat3 %>% select(category_id, dislikes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,dislikes, fill=dislikes ))+geom_col()+ggtitle("The Number of dislikes in the different categories of videos")+
          labs(x ='Category id', y ='Dislikes')
      }
      else if(first==11){
        dat3 %>% select(category_id,comment_count) %>% group_by(category_id) %>%ggplot(
          aes(category_id,comment_count,fill= comment_count ))+geom_col()+ggtitle("The Number of Comments in the different categories of videos")+
          labs(x ='Category id', y ='Comments')
      }}
    else  if(second==4){
      if(first==8){
        dat4 %>% select(category_id,views) %>% group_by(category_id) %>%ggplot(
          aes(category_id,views,fill=views ))+geom_col()+ggtitle("The Number of Views in the different categories of videos")+
          labs(x ='Category id', y ='Views')
      } 
      else if(first==9){
        dat4 %>% select(category_id,likes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,likes))+geom_col()+ggtitle("The Number of Likes in the different categories of videos")+
          labs(x ='Category id', y ='Likes')
        
      } 
      else if(first==10){
        dat4 %>% select(category_id, dislikes) %>% group_by(category_id) %>%ggplot(
          aes(category_id,dislikes, fill=dislikes ))+geom_col()+ggtitle("The Number of dislikes in the different categories of videos")+
          labs(x ='Category id', y ='Dislikes')
      }
      else if(first==11){
        dat4 %>% select(category_id,comment_count) %>% group_by(category_id) %>%ggplot(
          aes(category_id,comment_count,fill= comment_count ))+geom_col()+ggtitle("The Number of Comments in the different categories of videos")+
          labs(x ='Category id', y ='Comments')
      }}
    
  })
  output$likes <- renderInfoBox({
  second <- input$va
  if(second==1){
    my <- summarise_all(dat1[c("likes","category_id")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
               )
    
  }
  else if(second==2){
    my <- summarise_all(dat2[c("likes","category_id")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
    )
  }
  else if(second==3){
    my <- summarise_all(dat3[c("likes","category_id")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
    )
  }
  else if(second==4){
    my <- summarise_all(dat4[c("likes","category_id")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
    )
  }
  })
  
})
