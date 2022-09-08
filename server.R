rm(list = ls())
options(shiny.maxRequestSize=30*1024^2) 

server <- function(input, output, session){
  # Matrix & annotation input 
  matrix_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          df <- input$matrix_file$datapath %>%
            read.csv(sep = "\t",
                     row.names = 1,
                     check.names = F)
            
          format_check <- colnames(df) %>% lapply(
            function(col_id){
              df[[col_id]] %>% class
            }
          ) %>% unlist
          
          if(sum(format_check=="character")){
            mt <- NULL
          }else{
            mt <- df %>% as.matrix
          }
            
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
  
  # Matrix properties
  table_estimate <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- matrix_table()
          n_row <- nrow(mt)
          n_col <- ncol(mt)

          if(!is.null(n_row)&!is.null(n_col)){
            txt <- paste0('<h6><p style="color:#97CBFF"><b>ROW</b>( ',n_row,
                          ' ) X <b>COL</b>( ',n_col,' )</p></h6>')
          }else{
            txt <- '<h6><p style="color:#CE0000">Character detected, please check format! </p></h6>'
          }
          
        }else{
          txt <- NULL
        }
        txt
      },
      warning = function(war){
        print(war)
        txt <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        txt <- NULL
      }
    )
  })
  example_table <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- matrix_table()
          n_row <- nrow(mt)
          n_col <- ncol(mt)
          
          if(n_row<10 & n_col<10){
            mt <- mt[c(1:n_row),c(1:n_col)]
          }else if(n_row>=10 & n_col<10){
            mt <- mt[c(1:10),c(1:n_col)]
          }else if(n_row<10 & n_col>=10){
            mt <- mt[c(1:n_row),c(1:10)]
          }else{
            mt <- mt[c(1:10),c(1:10)]
          }
          
          cat("Example table generated\n")
        }else{
          mt <- NULL
          cat("No Example table\n")
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
  max_exp <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- matrix_table()
          exp <- max(mt)
          
        }else{
          exp <- 1
        }
        exp
      },
      warning = function(war){
        print(war)
        exp <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        exp <- NULL
      }
    )
  })
  min_exp <- reactive({
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- matrix_table()
          exp <- min(mt)
          
        }else{
          exp <- 0
        }
        exp
      },
      warning = function(war){
        print(war)
        exp <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        exp <- NULL
      }
    )
  })
  
  # clustering to figure out markers
  clustered_matrix <- eventReactive(
    c(input$doEELClustering,input$doEBClustering, input$EEL_cutoff,input$EBC_cutoff,
      input$showColName_m,input$showRowName_m,input$doTranspose_m),
    {
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          if(input$doEELClustering|input$doEBClustering){
            ## import matrix
            mt <- matrix_table()
            if(input$doTranspose_m){
              mt <- t(mt)
            }
            
            ## estimate the diff between 1st and 2nd value in hypo- and hyper-expression set
            diff.lt <- row.names(mt) %>% lapply(
              function(row_id){
                hyper.set <- mt[row_id,] %>% sort(decreasing = T)
                hypo.set <- mt[row_id,] %>% sort()
                
                hyper.diff <- hyper.set[1]-hyper.set[2]
                hypo.diff <- hypo.set[1]-hypo.set[2]
                
                c(hyper.diff,hypo.diff)
              }
            )
            names(diff.lt) <- row.names(mt)
            
            ## filter out the samples according to the threshold
            exam.set <- names(diff.lt) %>% lapply(
              function(row_id){
                diff.set <- diff.lt[[row_id]]
                if((diff.set[1]>=input$EEL_cutoff)|(abs(diff.set[2])>=input$EEL_cutoff)){
                  T
                }else{
                  F
                }
              }
            ) %>% unlist
            
            mt <- mt[exam.set,]
            diff.lt <- diff.lt[exam.set]
            names(diff.lt) <- row.names(mt)
            
            ## merge annotation and filtered matrix
            annot.df <- mt %>% as.data.frame %>%  mutate(
              "hyper_exp" = rep(NA,length(diff.lt)),
              "hypo_exp" = rep(NA,length(diff.lt)),
              "hyper_id" = rep(NA,length(diff.lt)),
              "hypo_id" = rep(NA,length(diff.lt)),
            )
            
            ## merge annotation and filtered matrix
            if(length(diff.lt)>=1){
              for(i in 1:length(diff.lt)){
                annot.df$hyper_exp[i] <- diff.lt[[i]][1]
                annot.df$hypo_exp[i] <- diff.lt[[i]][2]
                annot.df$hyper_id[i] <- names(diff.lt[[i]][1])
                annot.df$hypo_id[i] <- names(diff.lt[[i]][2])
              }
            }else{
              annot.df <- NULL
            }
            
            ## matrix arrange 
            annot.df$hyper_id <- annot.df$hyper_id %>% factor(levels = colnames(mt))
            annot.df$hypo_id <- annot.df$hypo_id %>% factor(levels = colnames(mt))
            annot.df <- annot.df %>% arrange(hyper_id,-hyper_exp,hypo_id,hypo_exp)
            
            
            ## entropy-based method to enhance arrange
            if(input$doEBClustering){
              mt <- annot.df[,1:ncol(mt)] %>% as.matrix
              SUM_E <- row.names(mt) %>% sapply(
                function(row_id){
                  mt[row_id,] %>% sum
                }
              )
              R.mt<- log2(mt/SUM_E)*(mt/SUM_E)
              H.mt <- R.mt %>% row.names %>% sapply(
                function(row_id){
                  -R.mt[row_id,] %>% sum(na.rm = T)
                }
              )
              annot.df <- annot.df %>% mutate("entropy" = H.mt) %>% 
                filter(entropy<=input$EBC_cutoff) %>% 
                arrange(hyper_id,hypo_id,entropy)
            }
            
            ## clean annotation and return the matrix
            mt <- annot.df[,1:ncol(mt)] %>% as.matrix
            
          }else{
            mt <- NULL
          }
          cat("Marker matrix generated\n")
        }else{
          mt <- NULL
          cat("No Marker matrix!\n")
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
  
  marker_estimate <- eventReactive(
    c(input$doEELClustering,input$doEBClustering, input$EEL_cutoff,input$EBC_cutoff,
      input$showColName_m,input$showRowName_m,input$doTranspose_m),
    {
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          mt <- clustered_matrix()
          n_row <- nrow(mt)
          n_col <- ncol(mt)
          
          if(!is.null(n_row)&!is.null(n_col)){
            txt <- paste0('<h6><p style="color:#97CBFF"><b>ROW</b>( ',n_row,
                          ' ) X <b>COL</b>( ',n_col,' )</p></h6>')
          }else if(input$doEELClustering|input$doEBClustering){
            txt <- '<h6><p style="color:#CE0000">No marker identified, please check setting! </p></h6>'
          }else{
            txt <- NULL
          }
          
        }else{
          txt <- NULL
        }
        txt
      },
      warning = function(war){
        print(war)
        txt <- NULL
      },
      error = function(err){
        print(conditionMessage(err));
        txt <- NULL
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
  plot_marker_heatmap <- eventReactive(
    c(input$doEELClustering,input$doEBClustering, input$EEL_cutoff,input$EBC_cutoff,
      input$showColName_m,input$showRowName_m,input$doTranspose_m),
    {
    tryCatch(
      {
        if(!is.null(input$matrix_file$datapath)){
          
          mt <- clustered_matrix()
          col_m <- color_m()
          
          if(length(col_m)>0){
            hp <- Heatmap(
              mt,
              col = col_m,
              cluster_rows = F,
              cluster_columns = F,
              show_column_names = input$showColName_m,
              show_row_names = input$showRowName_m,
              name = "Value",
              show_heatmap_legend = F,
            )
          }else{
            hp <- Heatmap(
              mt,
              cluster_rows = F,
              cluster_columns = F,
              show_column_names = input$showColName_m,
              show_row_names = input$showRowName_m,
              name = "Value",
              show_heatmap_legend = F,
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
  
  # observe
  observe({
    check <- (!input$doEELClustering)&(input$doEBClustering)
    if(check)updateSliderInput(session, "EEL_cutoff", value = 0)
  })
  observe({
    maxExp <- max_exp()
    minExp <- min_exp()
    updateSliderInput(session, "EEL_cutoff", min = minExp, max = maxExp,
                      value = minExp, step = (maxExp-minExp)/10)
  })
  observe({
    mt <- matrix_table()
    if(!is.null(mt)){
      n_col <- ncol(mt)
    }else{
      n_col <- 2
    }
    updateSliderInput(session, "EBC_cutoff", max = floor(log2(n_col)),
                      value = floor(log2(n_col)))
  })
  
  # output
  output$hp <- renderPlot(plot_heatmap())
  output$hp_little <- renderPlot(plot_pure_heatmap())
  output$hp_marker <- renderPlot(plot_marker_heatmap())
  
  output$tbEstimate <- renderText(table_estimate())
  output$mkEstimate <- renderText(marker_estimate())
  output$exampleTb <- renderDataTable(example_table())
  
  output$col_term <- renderText(ann_c_term())
  output$row_term <- renderText(ann_r_term())
  
  output$heatmap.pdf <- downloadHandler(
    filename = function(x) {
      paste0(Sys.Date(), "_heatmap", ".pdf")
    },
    content = function(file) {
      pdf(file)
      print(plot_heatmap())
      print(plot_pure_heatmap())
      print(plot_marker_heatmap())
      graphics.off()
    }
  )
  
  output$marker.tsv <- downloadHandler(
    filename = function(x) {
      paste0(Sys.Date(), "_marker.tsv")
    },
    content = function(file) {
      write.table(
        clustered_matrix() %>% as.data.frame %>% rownames_to_column,
        file,
        sep = "\t",
        quote = F
      )
    }
  )
}

