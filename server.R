rm(list = ls())
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session){
  # Matrix table input 
  matrix_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- input$matrix_file$datapath %>%
            read.csv(sep = "\t",
                     row.names = 1,
                     check.names = F) %>% 
            as.matrix()
          
          cat("Imported matrix file checked\n")
        }else{
          mt <- NULL
          cat("No matrix file imported\n")
        }
        mt
      },
      warning = function(war){
        print(war)
        mt <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        mt <- NULL
      }
    )
  })
  ann_c_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$col_annot_file$datapath)){
          ann_c.df <- input$col_annot_file$datapath %>%
            read.csv(sep = "\t",
                     row.names = 1,
                     check.names = F) %>% 
            mutate("blank" = NA)
          cat("Imported column annotation file checked\n")
        }else{
          ann_c.df <- NULL
          cat("No column annotation file imported\n")
        }
        ann_c.df
      },
      warning = function(war){
        print(war)
        cat("Error in column annotation file\n")
        ann_c.df <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ann_c.df <- NULL
      }
    )
  })
  ann_r_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$row_annot_file$datapath)){
          ann_r.df <- input$row_annot_file$datapath %>%
            read.csv(sep = "\t",
                     row.names = 1,
                     check.names = F) %>% 
            mutate("blank" = NA)
          cat("Imported row annotation file checked\n")
        }else{
          ann_r.df <- NULL
          cat("No row annotation file imported\n")
        }
        ann_r.df
      },
      warning = function(war){
        print(war)
        ann_r.df <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ann_r.df <- NULL
      }
    )
  })
  
  # color setting
  ann_c_term <-  reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath) & !is.null(input$col_annot_file$datapath)){
          ann_c <- ann_c_table()
          ann_c.term <- ann_c[[1]] %>% unique() %>% paste(.,collapse = ", ") %>% paste0("Term: ",.)
        }else{
          ann_c.term <- NULL
        }
        ann_c.term
      },
      warning = function(war){
        print(war)
        ann_c.term <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ann_c.term <- NULL
      }
    )
  })
  ann_r_term <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath) & !is.null(input$row_annot_file$datapath)){
          ann_r <- ann_r_table()
          ann_r.term <- ann_r[[1]] %>% unique() %>% paste(.,collapse = ", ") %>% paste0("Term: ",.)
        }else{
          ann_r.term <- NULL
        }
        ann_r.term
      },
      warning = function(war){
        print(war)
        ann_r.term <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ann_r.term <- NULL
      }
    )
  })
  
  color_m <- reactive({
    tryCatch(
      {
        if(nchar(input$point_m)>0 & nchar(input$color_m)>0){
          color_m <- colorRamp2(
            input$point_m %>% strsplit(.,split = ',') %>% unlist %>% as.numeric,
            input$color_m %>% strsplit(.,split = ',') %>% unlist
          )
        }else{
          color_m <- NULL
        }
        color_m
      },
      warning = function(war){
        print(war)
        color_m <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        color_m <- NULL
      }
    )
  })
  color_c <- reactive({
    tryCatch(
      {
        if(!is.null(input$color_c)){
          ann_c <- ann_c_table()
          ann_c.term <- ann_c[[1]] %>% unique()
          color_c <- c(input$color_c %>% strsplit(.,split = ',') %>% unlist)
          names(color_c) <- ann_c.term 
        }else{
          color_c <- NULL
        }
        color_c
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        color_c <- NULL
      }
    )
  })
  color_r <- reactive({
    tryCatch(
      {
        if(!is.null(input$color_r)){
          ann_r <- ann_r_table()
          ann_r.term <- ann_r[[1]] %>% unique()
          color_r <- c(input$color_r %>% strsplit(.,split = ',') %>% unlist)
          names(color_r) <- ann_r.term
        }else{
          color_r <- NULL
        }
        color_r
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        color_r <- NULL
      }
    )
  })

  # generate annotation
  ann_c_ha <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath) & !is.null(input$col_annot_file$datapath)){
          mt <- matrix_table()
          ann_c <- ann_c_table()
          ann_c <- ann_c[colnames(mt),]
          col_c <- color_c()
          if(input$doTranspose){
            whichAnno = "row"
          }else{
            whichAnno = "column"
          }
          
          if(length(col_c)>0){
            col = list(
              c_label = col_c
            )
            ha_c <- HeatmapAnnotation(
              c_label = ann_c[[1]],
              col = col,
              show_legend = T,
              which = whichAnno
            )
            
          }else{
            ha_c <- HeatmapAnnotation(
              c_label = ann_c[[1]],
              show_legend = T,
              which = whichAnno
              
            )
          }
          
         
          
          ha_c@anno_list[["c_label"]]@label <- colnames(ann_c)[1]
          ha_c@anno_list[["c_label"]]@color_mapping@name <- colnames(ann_c)[1]
        }else{
          ha_c <- NULL
        }
        ha_c
      },
      warning = function(war){
        print(war)
        ha_c <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ha_c <- NULL
      }
    )
  })
  ann_r_ha <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath) & !is.null(input$row_annot_file$datapath)){
          mt <- matrix_table()
          ann_r <- ann_r_table()
          ann_r <- ann_r[row.names(mt),]
          col_r <- color_r()
          if(input$doTranspose){
            whichAnno = "column"
          }else{
            whichAnno = "row"
          }
          
          if(length(col_r)>0){
            col = list(
              r_label = col_r
            )
            ha_r <- HeatmapAnnotation(
              r_label = ann_r[[1]],
              col = col,
              show_legend = T,
              which = whichAnno
            )
          }else{
            ha_r <- HeatmapAnnotation(
              r_label = ann_r[[1]],
              show_legend = T,
              which = whichAnno
            )
          }

          ha_r@anno_list[["r_label"]]@label <- colnames(ann_r)[1]
          ha_r@anno_list[["r_label"]]@color_mapping@name <- colnames(ann_r)[1]
          
        }else{
          ha_r <- NULL
        }
        ha_r
      },
      warning = function(war){
        print(war)
        ha_r <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        ha_r <- NULL
      }
    )
  })
  
  # plotting
  plot_heatmap <- eventReactive(c(input$doPlot), {
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          
          if(input$doTranspose){
            mt <- matrix_table() %>% t()
            ha_r <- ann_c_ha()
            ha_c <- ann_r_ha()
            col_m <- color_m()
          }else{
            mt <- matrix_table()
            ha_c <- ann_c_ha()
            ha_r <- ann_r_ha()
            col_m <- color_m()
          }
          
          if(length(col_m)>0){
            hp <- Heatmap(
              mt,
              col = col_m,
              cluster_rows = as.logical(input$doRowClustering),
              cluster_columns = as.logical(input$doColClustering),
              show_column_names = as.logical(input$showColName),
              show_row_names = as.logical(input$showRowName),
              name = "Value",
              show_heatmap_legend = T,
              top_annotation = ha_c,
              left_annotation = ha_r
            )
          }else{
            hp <- Heatmap(
              mt,
              cluster_rows = as.logical(input$doRowClustering),
              cluster_columns = as.logical(input$doColClustering),
              show_column_names = as.logical(input$showColName),
              show_row_names = as.logical(input$showRowName),
              name = "Value",
              show_heatmap_legend = T,
              top_annotation = ha_c,
              left_annotation = ha_r
            )
          }
          
          
        }else{
          hp <- NULL
        }
        hp
      },
      warning = function(war){
        print(war)
        hp <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        hp <- NULL
      }
    )
    
  })
  plot_pure_heatmap <- eventReactive(c(input$doPlot), {
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          if(input$doTranspose){
            mt <- matrix_table() %>% t()
            ha_r <- ann_c_ha()
            ha_c <- ann_r_ha()
            col_m <- color_m()
            
            if(!is.null(input$col_annot_file$datapath)){
              ha_c@anno_list[["r_label"]]@show_legend <- F
              ha_c@anno_list[["r_label"]]@name_param[["show"]] <- F
            }
            if(!is.null(input$row_annot_file$datapath)){
              ha_r@anno_list[["c_label"]]@show_legend <- F
              ha_r@anno_list[["c_label"]]@name_param[["show"]] <- F
            }
            
          }else{
            mt <- matrix_table()
            ha_c <- ann_c_ha()
            ha_r <- ann_r_ha()
            col_m <- color_m()
            
            if(!is.null(input$col_annot_file$datapath)){
              ha_c@anno_list[["c_label"]]@show_legend <- F
              ha_c@anno_list[["c_label"]]@name_param[["show"]] <- F
            }
            if(!is.null(input$row_annot_file$datapath)){
              ha_r@anno_list[["r_label"]]@show_legend <- F
              ha_r@anno_list[["r_label"]]@name_param[["show"]] <- F
            }
            
          }
          
          
          
          if(length(col_m)>0){
            hp <- Heatmap(
              mt,
              col = col_m,
              cluster_rows = as.logical(input$doRowClustering),
              cluster_columns = as.logical(input$doColClustering),
              show_column_names = F,
              show_row_names = F,
              name = "Value",
              show_heatmap_legend = F,
              top_annotation = ha_c,
              left_annotation = ha_r
            )
          }else{
            hp <- Heatmap(
              mt,
              cluster_rows = as.logical(input$doRowClustering),
              cluster_columns = as.logical(input$doColClustering),
              show_column_names = F,
              show_row_names = F,
              name = "Value",
              show_heatmap_legend = F,
              top_annotation = ha_c,
              left_annotation = ha_r
            )
          }
          
        }else{
          hp <- NULL
        }
        hp
      },
      warning = function(war){
        print(war)
        hp <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        hp <- NULL
      }
    )
  })
  
  # output
 
  output$hp <- renderPlot(plot_heatmap())
  output$hp_little <- renderPlot(plot_pure_heatmap())
  output$col_term <- renderText(ann_c_term())
  output$row_term <- renderText(ann_r_term())
  output$col_term <- renderText(ann_c_term())
  output$heatmap.pdf <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_heatmap", ".pdf")
    },
    content = function(file) {
      pdf(file)
      print(plot_heatmap())
      print(plot_pure_heatmap())
      graphics.off()
    }
  )
}

