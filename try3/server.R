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
dt1<- read.csv(file = "Book1.csv", header = T)
library(shiny)
library(SocialMediaLab)
library(syuzhet)
library(igraph)
library(ggplot2)
library(dplyr)
library(ggthemes)
options(shiny.maxRequestSize= 60^1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
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
    second <- input$va1
    first <- input$var1
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
  output$likes <- renderValueBox({
  second2 <- input$va1
  if(second2==1){
    my <- summarise_all(dat1[c("likes")],funs(max(likes)))
    
    valueBox(value= my,
             subtitle = "Highest likes of a video "
               )
    #valueBox(value = "hello",color = "yellow",
     #        subtitle = "string of hell")
    
  }
  else if(second2==2){
    my <- summarise_all(dat2[c("likes")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
    )
  }
  else if(second2==3){
    my <- summarise_all(dat3[c("likes")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Video with the highest likes  "
    )
  }
  else if(second2==4){
    my <- summarise_all(dat4[c("likes")],funs(max(likes)))
    valueBox(value = my,
             subtitle = "Highest likes of a video "
    )
  }
  })
 output$category<- renderValueBox({
   second3 <- input$va1
   if(second3==1){
     my <- summarise_all(dat1[c("likes")],funs(max(likes)))
     for(row in 1:nrow(dat1)){
       li <- dat1[row , "likes"]
       
       cate <- dat1[row , "category_id"]
       if (li==my){
         
         
         w<-cate
       }
     }
     valueBox(value= w,
              subtitle = "The category in which the video belongs to",color = "yellow"
     )
   }
   else if(second3==2){
     my <- summarise_all(dat2[c("likes")],funs(max(likes)))
     for(row in 1:nrow(dat2)){
       li <- dat2[row , "likes"]
       
       cate <- dat2[row , "category_id"]
       if (li==my){
         a<-cate
         
       }
     }
     valueBox(value= a,
              subtitle = "The category in which the video belongs to",color = "yellow"
     )
   }
   else if(second3==3){
     my <- summarise_all(dat3[c("likes")],funs(max(likes)))
     for(row in 1:nrow(dat3)){
       li <- dat3[row , "likes"]
       
       cate <- dat3[row , "category_id"]
       if (li==my){
        
         a<-cate
         
       }
     }
     valueBox(value= a,
              subtitle = "The category in which the video belongs to",color = "yellow"
     )
   }
   else if(second3==4){
     my <- summarise_all(dat4[c("likes")],funs(max(likes)))
     for(row in 1:nrow(dat4)){
       li <- dat4[row , "likes"]
       
       cate <- dat4[row , "category_id"]
       if (li==my){
         a<-cate
         
       }
     }
     valueBox(value= a,
              subtitle = "The category in which the video belongs to",color = "yellow"
     )
   }
 })
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep=".")
    },
    content = function(file) {
      write.csv(data, file)
    })
  output$descpt<- renderTable({ dt1[1:32,]
    } )
  output$sentiment<- renderPlot({
  api1<- input$api
  vid<-input$video_id
   key <- AuthenticateWithYoutubeAPI(api1)
  Video <- c(vid)
  ytdata1 <- CollectDataYoutube(videoIDs = Video,apiKeyYoutube = key,writeToFile = FALSE)
  Comments1<- iconv(ytdata1$Comment)
  s <- get_nrc_sentiment(Comments1)
  s$neutral <- ifelse(s$negative+s$positive==0,1,0)
  barplot(100*colSums(s)/sum(s),
        las = 2,
      col = rainbow(10),
      ylab = 'percentage',
     main = 'Sentiment scores'
  )
  
  })
  output$summary <- renderTable({
    summ1<- input$var3
    sec3 <- input$va1
    if(sec3==1){
      if(summ1==8){
        as.array(summary(dat1$views))
        
      } 
      else if(summ1==9){
        as.array(summary(dat1$likes))
        
      } 
      else if(summ1==10){
       as.array(summary(dat1$dislikes))  
      }
      else if(summ1==11){
       as.array(summary(dat1$comment_count))  
      }}
    else if(sec3==2){
      if(summ1==8){
        as.array(summary(dat2$views))
      } 
      else if(summ1==9){
        as.array(summary(dat2$likes))
        
      } 
      else if(summ1==10){
        as.array(summary(dat2$dislikes)) 
      }
      else if(summ1==11){
        as.array(summary(dat2$comment_count)) 
      }}
    else if(sec3==3){
      if(summ1==8){
        as.array(summary(dat3$views)) 
      } 
      else if(summ1==9){
        as.array(summary(dat3$likes)) 
        
      } 
      else if(summ1==10){
        as.array(summary(dat3$dislikes)) 
      }
      else if(summ1==11){
        as.array(summary(dat3$comment_count)) 
      }}
    else  if(sec3==4){
      if(summ1==8){
        as.array(summary(dat4$views)) 
      } 
      else if(summ1==9){
        as.array(summary(dat4$likes)) 
        
      } 
      else if(summ1==10){
        as.array(summary(dat4$dislikes)) 
      }
      else if(summ1==11){
        as.array(summary(dat4$comment_count)) 
      }}
    
    
    
  })
  output$trending<- renderTable({
    sec1<-input$var2
    fir1<-input$va1
    if(fir1==1){
      if (sec1==8){
        dat1%>%select(video_id,title,views)%>%arrange(desc(views))%>%slice(1:10)
      }
      else if(sec1==9){
        dat1%>%select(video_id,title,likes)%>%arrange(desc(likes))%>%slice(1:10)
      }
    }
    else if(fir1==2){
      if (sec1==8){
        dat2%>%select(video_id,title,views)%>%arrange(desc(views))%>%slice(1:10)
      }
      else if(sec1==9){
        dat2%>%select(video_id,title,likes)%>%arrange(desc(likes))%>%slice(1:10)
      }
    }
    else if(fir1==3){
      if (sec1==8){
        dat3%>%select(video_id,title,views)%>%arrange(desc(views))%>%slice(1:10)
      }
      else if(sec1==9){
        dat3%>%select(video_id,title,likes)%>%arrange(desc(likes))%>%slice(1:10)
      }
    }
    else if(fir1==4){
      if (sec1==8){
        dat4%>%select(video_id,title,views)%>%arrange(desc(views))%>%slice(1:10)
      }
      else if(sec1==9){
        dat4%>%select(video_id,title,likes)%>%arrange(desc(likes))%>%slice(1:10)
      }
    }
  })
  
  
  
  
})
