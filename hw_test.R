options(warn=-1)
library(shiny)
library(tidyverse)
  
ui <- fluidPage(
  titlePanel("Test for Hardy_Weinberg Equilibrium"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = 'dominant', label = 'Homo_dominant', value = 0),
      numericInput(inputId = 'recessive', label = 'Homo_recessive', value = 0),
      numericInput(inputId = 'heterzygous', label = 'Heterozygous', value = 0)
    ),
  
  # Main panel for displaying outputs ----
  mainPanel(uiOutput("tb",width = "100%")) # control the pane size)
  )
)

server <-function(input,output) {
  dat <- reactive({
    # observed
    dom <- input$dominant
    rec <- input$recessive
    het <- input$heterzygous
    tot <- sum(dom, rec, het)
    
    # observed percentage
    allele_p <- (2*dom + het)/(tot*2)
    allele_q <- (2*rec + het)/(tot*2)
    
    # hardy_weinberg equilibrium expected
    het_hw <- 2*allele_p*allele_q
    
    # setup dataframe
    observed <- c(dom, het, rec, allele_p, allele_q)
    hardy_weinberg_expected <- c(round(allele_p^2 * tot), round(het_hw * tot), round(allele_q^2 * tot),
             allele_p, allele_q)
    rowNames <- c('dom', 'het', 'rec', 'allele_p', 'allele_q')
    df <- as.data.frame(cbind(rowNames, observed, hardy_weinberg_expected))
    df
  })
  
  # to show the data file as dataset in main pane
  output$filedf <- renderTable({
    dat()
  })
  
  # chisq good of fit test 
  output$chisqTest <- renderPrint({
    dat1 <- as.numeric(as.character(dat()[1:5, 2]))  
    p2 = (dat1[4])^2;
    q2 = (dat1[5])^2;
    h2 = 1 - p2 - q2;  
    
    print(chisq.test(dat1[1:3], p =c(p2, h2, q2)))
  })
  
  # Dynamic UI
  output$tb <- renderUI({
      tabsetPanel(
        tabPanel("Organized Dataset ", tableOutput("filedf")),
        tabPanel("Chi Square Test", verbatimTextOutput("chisqTest"))
      )
  })
  }

# Return a Shiny app object
shinyApp(ui = ui, server = server)