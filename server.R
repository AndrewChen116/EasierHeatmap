rm(list = ls())
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session){
  # Matrix table input 
  matrix_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          matrix.df <- input$matrix_file$datapath %>%
            read.csv(sep = "\t",
                     row.names = 1,
                     check.names = F) %>% 
            as.matrix()
          cat("Imported matrix file checked\n")
        }else{
          matrix.df <- NULL
          cat("No matrix file imported\n")
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        matrix.df <- NULL
      }
    )
    matrix.df
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
      },
      warning = function(war){
        print(war)
        cat("Error in column annotation file\n")
      },
      error = function(err){
        print(conditionMessage(err));
        ann_c.df <- NULL
      }
    )
    ann_c.df
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
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        ann_r.df <- NULL
      }
    )
    ann_r.df
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
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        ann_c.term <- NULL
      }
    )
    ann_c.term
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
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        ann_r.term <- NULL
      }
    )
    ann_r.term
  })
  
  color_m <- reactive({
    tryCatch(
      {
        if(!is.null(input$point_m) & !is.null(input$color_m)){
          color_m <- colorRamp2(
            input$point_m %>% strsplit(.,split = ',') %>% unlist %>% as.numeric,
            input$color_m %>% strsplit(.,split = ',') %>% unlist
          )
        }else{
          color_m <- NULL
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        color_m <- NULL
      }
    )
    color_m
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
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        color_c <- NULL
      }
    )
    color_c
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
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        color_r <- NULL
      }
    )
    color_r
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
          if(length(col_c)>0){
            col = list(
              c_label = col_c
            )
            ha_c <- HeatmapAnnotation(
              c_label = ann_c[[1]],
              col = col,
              show_legend = T
            )
            
          }else{
            ha_c <- HeatmapAnnotation(
              c_label = ann_c[[1]],
              show_legend = T
            )
          }
          
         
          
          ha_c@anno_list[["c_label"]]@label <- colnames(ann_c)[1]
          ha_c@anno_list[["c_label"]]@color_mapping@name <- colnames(ann_c)[1]
        }else{
          ha_c <- NULL
        }
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
    ha_c
  })
  ann_r_ha <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath) & !is.null(input$row_annot_file$datapath)){
          mt <- matrix_table()
          
          ann_r <- ann_r_table()
          ann_r <- ann_r[row.names(mt),]
          
          col_r <- color_r()
          if(length(col_r)>0){
            col = list(
              r_label = col_r
            )
            ha_r <- rowAnnotation(
              r_label = ann_r[[1]],
              col = col,
              show_legend = T
            )
          }else{
            ha_r <- rowAnnotation(
              r_label = ann_r[[1]],
              show_legend = T
            )
          }

          ha_r@anno_list[["r_label"]]@label <- colnames(ann_r)[1]
          ha_r@anno_list[["r_label"]]@color_mapping@name <- colnames(ann_r)[1]
          
        }else{
          ha_r <- NULL
        }
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
    ha_r
  })
  
  # plotting
  plot_heatmap <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- matrix_table()
          
          ha_c <- ann_c_ha()
          ha_r <- ann_r_ha()
          col_m <- color_m()
          
          if(!is.null(col_m)){col = col_m}
          
          hp <- Heatmap(
            mt,
            col,
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
          hp <- NULL
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        hp <- NULL
      }
    )
    hp
  })
  plot_pure_heatmap <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
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
          
          if(!is.null(col_m)){col = col_m}
          
          hp <- Heatmap(
            mt,
            col,
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
          hp <- NULL
        }
      },
      warning = function(war){
        print(war)
      },
      error = function(err){
        print(conditionMessage(err));
        hp <- NULL
      }
    )
    hp
  })
  
  # output
 
  output$hp <- renderPlot(plot_heatmap())
  output$hp_little <- renderPlot(plot_heatmap())
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

