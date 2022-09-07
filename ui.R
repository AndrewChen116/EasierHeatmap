##### Description #####
  # aim: easily apply "ComplexHeatmap" package to plot heatmap of genome data
  # maintainer: AndrewChen116 
##### version info #####
  # _                           
  # platform       x86_64-w64-mingw32          
  # arch           x86_64                      
  # os             mingw32                     
  # system         x86_64, mingw32             
  # status                                     
  # major          4                           
  # minor          0.5                         
  # year           2021                        
  # month          03                          
  # day            31                          
  # svn rev        80133                       
  # language       R                           
  # version.string R version 4.0.5 (2021-03-31)
  # nickname       Shake and Throw
##### package ##### 
options(repos = BiocManager::repositories())
getOption("repos")

library(shiny)
library(ComplexHeatmap)
library(circlize)
library(tidyverse)
library(parallel)
library(bslib)
library(DT)


##### ui #####
css <- "mark{
  padding: 0;
  background-color:#FFFF93;
  color:#930000;
}"

ui <- navbarPage(
  # Title 
  titlePanel(
    h3("Easier Heatmap"),
    windowTitle = "Easier_Heatmap"
  ),
  theme = bs_theme(
    bootswatch = "darkly"
  ),
  
  sidebarLayout(
    # side panel
    sidebarPanel(
      ## file input
      fileInput(
        "matrix_file", h6(strong("Matrix"), style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      fileInput(
        "row_annot_file", h6(strong("Row annotation"),style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      fileInput(
        "col_annot_file", h6(strong("Col annotation"),style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      ## output setting
      fluidRow(
        column(
          actionButton(inputId="doPlot", label=" Plot",
                       icon=icon(name = "palette")),
          width = 3
        ),
        column(
          downloadButton("heatmap.pdf", "PDF"),
          width = 3
        ),
        column(
          downloadButton("marker.tsv", "TSV"),
          width = 3
        )
      ),
      ## print execution time
      htmlOutput("print_time") %>% h6(.,align="center",style = "color:#B3D9D9"),
      plotOutput("hp_little", height = "200px"),
      br(),
      br(),
      br(),
      ## version info
      h6("20220907_KLC_v0.2.0",align="right",style = "color:#6C6C6C"),
      h6("Powered by ComplexHeatmap",align="right",style = "color:#6C6C6C"),
      ## width of sidebarPanel
      width = 3
      
    ),
    # main panel
    mainPanel(
      ## tag setting
      tags$head(tags$style(HTML(css))),
      
      ## output table
      tabsetPanel(
        tabPanel(
          h5("Preview",style = "color:#97CBFF"),
          h6("Here presents example table from raw data",style = "color:#97CBFF"),
          textOutput('tbEstimate'),
          h5("   ",style = "color:#97CBFF"),
          dataTableOutput("exampleTb")
        ),
        tabPanel(
          h5("Setting",style = "color:#97CBFF"),
          h6("Hierarchical Clustering",style = "color:#97CBFF"),
          fluidRow(
            column(
              checkboxInput(
                "doColClustering",
                "Column clustering",
                c(T)
              ),
              width = 3
            ),
            column(
              checkboxInput(
                "doRowClustering",
                "Row clustering",
                c(T)
              ),
              width = 3
            )
          ),
          h6("Annotation",style = "color:#97CBFF"),
          fluidRow(
            column(
              checkboxInput(
                "showColName",
                "Show coloumn name",
                c(T)
              ),
              width = 3
            ),
            column(
              checkboxInput(
                "showRowName",
                "Show row name",
                c(T)
              ),
              width = 3
            )
          ),
          h6("Transpose",style = "color:#97CBFF"),
          fluidRow(
            column(
              checkboxInput(
                "doTranspose",
                "Transpose the matrix",
                c(F)
              ),
              width = 3
            )
          ),
          
          width = 1
        ),
        tabPanel(
          h5("Palette",style = "color:#97CBFF"),
          h6("set the point of main legend",style = "color:#97CBFF"),
          textInput(
            "point_m",
            NULL,
            "0,0.5,1"
          ),
          h6("set the color of main legend",style = "color:#97CBFF"),
          textInput(
            "color_m",
            NULL,
            "black,orange,gold"
          ),
          br(),
          h6("set the color of column annotation legend",style = "color:#97CBFF"),
          textInput(
            "color_c",
            ""
          ),
          textOutput('col_term'),
          br(),
          h6("set the color of row annotation legend",style = "color:#97CBFF"),
          textInput(
            "color_r",
            ""
          ),
          textOutput('row_term'),
          br(),
          width = 1
        ),
        tabPanel(
          h5("Heatmap",style = "color:#97CBFF"),
          plotOutput("hp",
                     width = "700px",
                     height = "550px"),
          width = 1
        ),
        tabPanel(
          h5("Marker",style = "color:#97CBFF"),
          h6(" ",style = "color:#97CBFF"),
          fluidRow(
            column(
              checkboxInput(
                "doEELClustering",
                "EEL-based selection",
                c(F)
              ),
              width = 3
            ),
            column(width = 1),
            column(
              checkboxInput(
                "doEBClustering",
                "Entropy-based selection",
                c(F)
              ),
              width = 3
            )
          ),
          fluidRow(
            column(
              sliderInput(
                "EEL_cutoff",
                NULL,
                min = 0, max = 1, value = 0
              ),
              width = 3
            ),
            column(width = 1),
            column(
              sliderInput(
                "EBC_cutoff",
                NULL,
                min = 0, max = 4, step = 0.1, value = 4
              ),
              width = 3
            ),
            column(
              plotOutput("hp_marker",
                         width = "700px",
                         height = "550px"),
              width = 9
            )
          ),
         
        ),
      ),
      ## width of mainPanel
      width = 9
    )
  )
)
