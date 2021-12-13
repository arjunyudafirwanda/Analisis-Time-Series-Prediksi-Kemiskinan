#setwd("D:/KULIAH/DATA TINGKAT 4/INTERNSHIP 1/APLIKASI_ARJUN/APLIKASI PENGOLAHAN DATA KEMISKINAN/Aplikasi_Pengolahan_Data_Kemiskinan")
#getwd()
#https://www.sqlite.org/download.html
#https://cran.r-project.org/web/packages/shinymanager/readme/README.html
#https://ndholkondho.wordpress.com/2018/01/12/koneksi-mysql-dengan-r-studio/


#Fungsi Login
credentials <- data.frame(
    user = c("user"), # mandatory
    password = c("user"), # mandatory
    user = c(TRUE),
    stringsAsFactors = FALSE
)

library(shiny)
library(shinymanager)
library(rsconnect)
library(shinythemes)
library(ggplot2)
library(tidyverse)


#Ui
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Aplikasi Pengolahan Data Kemiskinan",
                           tabPanel("Dataset",
                                    sidebarLayout(
                                        sidebarPanel(
                                            
                                            # Input: Select a file ----
                                            fileInput("file1", "Choose CSV File",
                                                      multiple = FALSE,
                                                      accept = c("text/csv",
                                                                 "text/comma-separated-values,text/plain",
                                                                 ".csv")),
                                            
                                            # Input: Checkbox if file has header ----
                                            checkboxInput("header", "Header", TRUE),
                                            
                                            # Input: Select separator ----
                                            radioButtons("sep", "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";"),
                                                         selected = ";"),
                                            
                                            # Input: Select number of rows to display ----
                                            radioButtons("disp", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "all")
                                        ),
                                        mainPanel(
                                            tableOutput("contents")
                                        )
                                    )
                           ),
                           
                           tabPanel("Summary",
                                    verbatimTextOutput("summ")
                           ),
                           
                           tabPanel("Visualisasi",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("x", "Masukkan Variabel Independen", c("Indeks_Kedalaman_X1",
                                                                                               "Indeks_Keparahan_X2",
                                                                                               "Presentase_Kemiskinan_X3")),
                                            selectInput("y", "Masukkan Variabel Dependen", c("Data_Penduduk_Miskin_Y"))
                                        ),
                                        
                                        mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Scatterplot", plotOutput("scatterplot")),
                                                        
                                                        tabPanel("Barplot",
                                                                 fluidRow(
                                                                     column(6, plotOutput("barplot1")),
                                                                     column(6, plotOutput("barplot2")),
                                                                 )
                                                        ),
                                                        
                                                        
                                                        tabPanel("Boxplot",
                                                                 fluidRow(
                                                                     column(6, plotOutput("boxplot1")),
                                                                     column(6, plotOutput("boxplot2")),
                                                                 )
                                                        ),
                                                        
                                                        tabPanel("Histogram",
                                                                 fluidRow(
                                                                     column(6, plotOutput("histogram1")),
                                                                     column(6, plotOutput("histogram2")),
                                                                 )
                                                        )
                                            )
                                        )
                                    )
                           ),
                           
                           tabPanel("Uji Asumsi Klasik", verbatimTextOutput("summary")),
                           
                           tabPanel("Perhitungan Regresi", 
                                    sidebarLayout(
                                        
                                        sidebarPanel(tags$h3('Perhitungan Variabel'), width = 250,
                                                     textInput("satu", "Masukan Nilai a/konstanta/intercept"),
                                                     textInput("dua", "Masukan Nilai X1"),
                                                     textInput("tiga", "Masukan Nilai b1"),
                                                     textInput("empat", "Masukan Nilai X2"),
                                                     textInput("lima", "Masukan Nilai b2"),
                                                     textInput("enam", "Masukan Nilai X3"),
                                                     textInput("tujuh", "Masukan Nilai b3"),
                                                     actionButton("hitung", "Proses")
                                        ),
                                        mainPanel(
                                            tags$h4('Hasil Perhitungan Prediksi Kemiskinan:'),
                                            verbatimTextOutput("less")
                                        )
                                    )
                           )
                           
                )
)


# Wrap your UI with secure_app
ui <- secure_app(ui)


#Server
server <- function(input, output, session) {
    
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
    
    #Output Menampilkan 6 Data Teratas
    output$contents <- renderTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        if( input$disp == "head"){
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    
    #Output Menampilkan Tabel Data
    output$summ<-renderPrint({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        summary(df)
    })
    
    
    #Output Summary Uji Asumsi Klasik
    output$summary <- renderPrint({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header =  input$header,
                               sep = input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        tryCatch(
            {
                model <- lm(Data_Penduduk_Miskin_Y ~ Indeks_Kedalaman_X1 + Indeks_Keparahan_X2 + Presentase_Kemiskinan_X3, df)
                prediksi <- predict(model, df)
                prediksi
                summary(model)
            },
            error = function(e){
                print("Masukan Variabel Terlebih Dahulu") 
            }
        )
    })
    
    
    #Output Grafik Scatterplot
    output$scatterplot <- renderPlot({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                plot(df[,input$x], df[,input$y], main= "Scatterplot",
                     xlab= input$x, ylab= input$y, pch = 15)
                abline(lm(df[,input$y] ~ df[,input$x]), col="red")
                lines(lowess(df[,input$x],df[,input$y]), col="blue")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Grafik barplot 1
    output$barplot1 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                barplot(df[,input$y], main="Barplot Variabel Dependen", xlab=input$y, col = "brown")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Grafik barplot 2
    output$barplot2 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                barplot(df[,input$x], main="Barplot Variabel Independen", xlab=input$x, col = "blue")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    
    #Output Grafik boxplot 1
    output$boxplot1 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                boxplot(df[,input$y], main="Boxplot Variabel Dependen", xlab=input$y, col = "gold")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Grafik boxplot 2
    output$boxplot2 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                boxplot(df[,input$x], main="Boxplot Variabel Independen", xlab=input$x, col = "gray")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Grafik Histogram 1
    output$histogram1 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                hist(df[,input$y], main="", xlab=input$y, col = "wheat")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Grafik Histogram 2
    output$histogram2 <- renderPlot({
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep =  input$sep)
            },
            error = function(e){
                stop(safeError(e))
            }
        )
        
        tryCatch(
            {
                hist(df[,input$x], main="", xlab=input$x, col = "turquoise")
            },
            error = function(e){
                print("")
            }
        )
    })
    
    
    #Output Perhitungan Regresi
    observeEvent(input$hitung,{
        a <- as.numeric(input$satu)
        x1 <- as.numeric(input$dua)
        b1 <- as.numeric(input$tiga)
        x2 <- as.numeric(input$empat)
        b2 <- as.numeric(input$lima)
        x3 <- as.numeric(input$enam)
        b3 <- as.numeric(input$tujuh)
        
        
        #Rumus Regresi Linear Berganda
        y <- a + (x1*b1) + (x2*b2) + (x3*b3)
        
        output$less <- renderPrint(y)
        
    })
}

#Run Apps
shinyApp(ui, server)
