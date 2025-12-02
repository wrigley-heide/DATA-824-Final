function(input, output, session) {
  
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(ggthemes)
library(dplyr)
library(readr)
library(vtable)
library(psych)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggpmisc)
library(pheatmap)
library(cluster)
library(NbClust)
  
bb <- read_csv("NBA_Rookies.csv")
bb_vars <- bb[6:40]
bb_vars_s <- scale(bb_vars)
  
var_names <- c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                 "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                 "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")


          output$chosen_hist <- renderPlot({
            variable = NULL
            bins = 15
            variable <- input$variable
            bins <- input$bins
            ggplot(bb, aes_string(x = variable)) +
              geom_histogram(fill="seagreen3", color="black", bins = bins) +
              labs(x = variable, title = paste("Histogram of", variable,"for Rookies 1979-2019")) +
              theme_stata()
          })   
          
          
          selected_row_data <- reactive({
            req(input$row)
            bb %>%
              filter(ID == input$row)
          })

          filteredData <- eventReactive(input$searchButton, {
            req(input$player)
            match_index <- grep(input$player, bb$Player, ignore.case = TRUE)
            if (length(match_index) > 0) {
              return(bb[1:40][match_index, , drop = FALSE])
            } 
            else {
              return(data.frame(Note = "No such player in Dataset", stringsAsFactors = FALSE))
            }
          })
          

          output$result <- renderDT({
            datatable(filteredData(), 
                      options = list(pageLength = 1,
                                     scrollX = TRUE,
                                     searching = FALSE, 
                                     lengthChange = FALSE,
                                     info = FALSE))
          })
          
          
            output$chosen_box <- renderPlot({
              variable <- input$variable
              ggplot(bb, aes_string(x = variable)) +
                geom_boxplot(color="cadetblue") +
                labs(x = variable, title = paste("Boxplot of", variable,"for Rookies 1979-2019"))+
                theme_stata()+
                theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())

          })
            y <- describe(bb[6:40])
            y
            y[,1]<- c("Age", "Career", "Games","MP","PTS","FG","FGA","FGperc","thrP","thrPA","thrPpg","FT","FTA","FTperc",
                      "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","MPpg","PTSpg","FGpg","thrPpg","FTpg","ORBpg","DRBpg",
                      "TRBpg","ASTpg","STLpg","BLKpg","TOVpg","PFpg")
            output$var_sum <- renderTable(y)
            
        variable_y = NULL
        variable_x = NULL
        output$chosen_scatterplot <- renderPlot({
          variable_y <- input$variable_y
          variable_x <- input$variable_x
          ggplot(bb, aes_string(x = variable_x, y = variable_y)) +
            geom_point() +
            labs(x = variable_x, y = variable_y, title = paste(variable_x ,"vs", variable_y," for Rookies 1979-2019")) +
            theme_stata()+
            stat_poly_line(color="lightsalmon") +
            stat_poly_eq(use_label(c("eq", "R2")))
            
        })
    
        output$comp_box <- renderPlot({
          variable <- input$variable
          ggplot(bb, aes_string(x=factor(bb$Team), y = variable)) +
            geom_boxplot(color="darkolivegreen2") +
            labs(x = variable, title = paste("Boxplots of", variable,"for Rookies by Team 1979-2019"))+
            theme_stata()+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
          
        })
        output$corrs <- renderPlot({
          corrplot(cor(bb_vars), type="upper",tl.cex = .9, 
                   tl.col="red",)
        })
        output$pca_bars <- renderPlot({
          fviz_eig(pca_m)
        })
        output$pca_2d <- renderPlot({
          pca_m <- PCA(bb_vars, graph = TRUE)
          pca_m
        })
        output$loadings <- renderTable({
          cbind(var_names,round(pca_m$var$coord, digits = 6))
        })
        output$dim1 <- renderPlot({
          fviz_contrib(pca_m, choice = "var", axes = 2, top = 10)
        })
        output$dim2 <- renderPlot({
          fviz_contrib(pca_m, choice = "var", axes = 2, top = 10)
        })
        output$dim3 <- renderPlot({
          fviz_contrib(pca_m, choice = "var", axes = 3, top = 10)
        })
        
        output$two_dims_plot <- renderPlot({
          fviz_contrib(pca_m, choice = "ind", axes = 1:2, top = 20)
        })
        
        output$clust <- renderPlot({
          method <- input$method
          year <- input$year
          bb_vars_year <- filter(bb, Year == 1979)
          bb_vars_s_year <- scale(bb_vars_year[6:40])
        c1 <- agnes(bb_vars_s_year, method= toString(method), metric = "euclidian")
        pltree(c1, cex = 0.6, hang = -1, main = "Dendrogram", xlab = "Player")
        })
        
        output$cut <- renderTable({
          cutpoint <- input$cutpoint
        clusters <- cutree(as.hclust(c1),h = as.double(cutpoint))
        table(clusters)
        })
}


