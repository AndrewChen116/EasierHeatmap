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
    h3("  Easier Heatmap"),
    windowTitle = "Easier_Heatmap"
  ),
  theme = bs_theme(
    bootswatch = "darkly"
  ),
  
  sidebarLayout(
    # side panel
    sidebarPanel(
      HTML('<h5><b><p style="color:#00E3E3">Data manager</p></b></h5>'),
      
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
          downloadButton("heatmap.pdf", "Heatmap"),
          width = 4
        ),
        column(
          downloadButton("marker.tsv", "Marker"),
          width = 4
        ),
        column(
          actionButton(inputId='_', label="Source",
                       icon = icon("github"), 
                       onclick ="window.open('https://github.com/AndrewChen116/EasierHeatmap')"),
          width = 4
        )
      ),
      ## print execution time
      htmlOutput("print_time") %>% h6(.,align="center",style = "color:#B3D9D9"),
      plotOutput("hp_little", height = "200px"),
      
      ## version info
      h6("20220907_KLC_v0.2.0",align="right",style = "color:#6C6C6C"),
      br(),
      br(),
      br(),
      br(),
      "Powered by ",
      tags$a(href="'https://github.com/jokergoo/ComplexHeatmap", "ComplexHeatmap"),
      
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
          HTML('<h5><b><p style="color:#97CBFF">Preview</p></b></h5>'),
          htmlOutput('tbEstimate'),
          h5("   ",style = "color:#97CBFF"),
          dataTableOutput("exampleTb")
        ),
        tabPanel(
          HTML('<h5><b><p style="color:#97CBFF">Heatmap</p></b></h5>'),
          fluidRow(
            column(
              HTML('<h5><b><p style="color:#00E3E3">Plot</p></b></h5>'),
              plotOutput("hp",
                         width = "700px",
                         height = "550px"),
              width = 8
            ),
            column(
              fluidRow(
                column(
                  HTML('<h5><b><p style="color:#00E3E3">Dashboard</p></b></h5>'),
                  width = 8
                ),
                column(
                  h6(""),
                  actionButton(inputId="doPlot", label=" Plot",
                               icon=icon(name = "palette")),
                  width = 4
                )
              ),
              h6("Hierarchical Clustering",style = "color:#97CBFF"),
              fluidRow(
                column(
                  checkboxInput(
                    "doRowClustering",
                    "ROW clustering",
                    c(T)
                  ),
                  width = 6
                ),
                column(
                  checkboxInput(
                    "doColClustering",
                    "COL clustering",
                    c(T)
                  ),
                  width = 6
                )
              ),
              h6("Label",style = "color:#97CBFF"),
              fluidRow(
                column(
                  checkboxInput(
                    "showRowName",
                    "show ROW name",
                    c(T)
                  ),
                  width = 6
                ),
                column(
                  checkboxInput(
                    "showColName",
                    "show COL name",
                    c(T)
                  ),
                  width = 6
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
                  width = 12
                )
              ),
              HTML('<h5><b><p style="color:#00E3E3">Palette</p></b></h5>'),
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
              h6("set the color of ROW annotation legend",style = "color:#97CBFF"),
              textInput(
                "color_c",
                NULL
              ),
              textOutput('col_term'),
              h6("set the color of COL annotation legend",style = "color:#97CBFF"),
              textInput(
                "color_r",
                NULL
              ),
              textOutput('row_term'),
              width = 4
            )
          ),
          
          width = 1
        ),
        tabPanel(
          HTML('<h5><b><p style="color:#97CBFF">Marker</p></b></h5>'),
          fluidRow(
            column(
              HTML('<h5><b><p style="color:#00E3E3">Stairs plot</p></b></h5>'),
              HTML('<h6><p style="color:grey">Suggestion: original number of ROWs > 50</p></h6>'),
              htmlOutput('mkEstimate'),
              plotOutput("hp_marker",
                         width = "700px",
                         height = "550px"),
              width = 8
            ),
            
            column(
              HTML('<h5><b><p style="color:#00E3E3">Dashboard</p></b></h5>'),
              checkboxInput(
                "doEELClustering",
                "EEL-based selection",
                c(F)
              ),
              sliderInput(
                "EEL_cutoff",
                NULL,
                min = 0, max = 1, value = 0
              ),
              checkboxInput(
                "doEBClustering",
                "Entropy-based selection",
                c(F)
              ),
              sliderInput(
                "EBC_cutoff",
                NULL,
                min = 0, max = 4, step = 0.1, value = 4
              ),
              h6("Label",style = "color:#97CBFF"),
              fluidRow(
                column(
                  checkboxInput(
                    "showRowName_m",
                    "show ROW name",
                    c(T)
                  ),
                  width = 6
                ),
                column(
                  checkboxInput(
                    "showColName_m",
                    "show COL name",
                    c(T)
                  ),
                  width = 6
                )
              ),
              h6("Transpose",style = "color:#97CBFF"),
              fluidRow(
                column(
                  checkboxInput(
                    "doTranspose_m",
                    "Transpose the matrix",
                    c(F)
                  ),
                  width = 12
                )
              ),

              width = 4
            )
          ),
        ),
      ),
      ## width of mainPanel
      width = 9
    )
  )
)
