#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries ---------------------------------------------------------------


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(zoo)

# Data --------------------------------------------------------------------

load(url("https://www.dropbox.com/s/cdap0dvgv78urz9/ICZPL_KVART.RData?dl=1"))
load(url("https://www.dropbox.com/s/3lf31d6watfdy3j/NEPL.RData?dl=1"))
load(url("https://www.dropbox.com/s/wvhoo79ltf0worp/PAYL_KVART.RData?dl=1"))

ICZPL_KVART$Življenjske_Potrebščine <- gsub("^SKUPAJ", "Življenjske potrebščine - SKUPAJ", ICZPL_KVART$Življenjske_Potrebščine)

Nepr <- unique(NEPL$STANOVANJSKE_NEPREMIČNINE)
Dejavnosti <- unique(PAYL_KVART$SKD_DEJAVNOST)
Cene <- unique(ICZPL_KVART$Življenjske_Potrebščine)

# Create empty data frame
Data_ggplot <- data.frame(
  matrix(ncol=6,
         nrow=0, 
         dimnames=list(NULL, c("Datum", "Info", "Skupina", "Podskupina", "Indeks", "Vrsta_Plače")))) %>% 
  mutate(
    Datum = as.yearqtr(Datum),
    Info = as.character(Info),
    Skupina = as.character(Skupina),
    Podskupina = as.character(Podskupina),
    Indeks = as.numeric(Indeks),
    Vrsta_Plače = as.character(Vrsta_Plače)
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pregled in primerjava indeksov"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("obdobje",
                      "Izberi obdobje:",
                      choices = seq(as.yearqtr("2008-01"), as.yearqtr("2020-04"), by = 1/4),
                      selected = c(as.yearqtr("2008-01"), as.yearqtr("2020-04"))
      ),
      
      hr(),
      
      selectInput("Tip",
                  "Izberi tip indeksa",
                  choices = c(
                    "Četrtletje / povprečje četrtletij leta 2015",
                    "Četrtletje / prejšnje četrtletje",
                    "Četrtletje / isto četrtletje prejšnjega leta",
                    "Četrtletje / zadnje četrtletje prejšnjega leta"
                  )
      ),
      
      hr(style = "border-color: black"),
      
      awesomeCheckbox(
        inputId = "NeprB",
        label = "Indeks nepremičnin", 
        value = TRUE,
        status = "danger"
      ),
      selectInput(
        inputId = "VrstaN",
        label = "Izberi vrsto nepremičnin",
        choices = Nepr,
        selected = Nepr[1],
        multiple = TRUE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      
      hr(style = "border-color: black"),
      
      awesomeCheckbox(
        inputId = "PlačaB",
        label = "Indeks plač", 
        value = TRUE,
        status = "danger"
      ),
      radioGroupButtons(
        inputId = "VrstaPlače",
        label = "Izberi vrsto plače:",
        choices = c("Bruto", 
                    "Neto"),
        justified = TRUE
      ),
      selectizeInput(
        inputId = "VrstaD",
        label = "Izberi dejavnost", 
        choices = Dejavnosti,
        multiple = TRUE,
        selected = last(Dejavnosti)
      ),
      # selectInput(
      #   inputId = "VrstaD",
      #   label = "Izberi dejavnost",
      #   choices = Dejavnosti,
      #   selected = last(Dejavnosti),
      #   multiple = TRUE,
      #   selectize = TRUE,
      #   width = NULL,
      #   size = NULL
      # ),
      
      hr(style = "border-color: black"),
      
      awesomeCheckbox(
        inputId = "CeneB",
        label = "Indeks cen življenjskih potrebščin", 
        value = FALSE,
        status = "danger"
      ),
      selectInput(
        inputId = "VrstaC",
        label = "Izberi življenjske potrebščine",
        choices = Cene,
        selected = "Življenjske potrebščine - SKUPAJ",
        multiple = TRUE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs", id = "BigT",
                  #tabPanel("Namen in uporaba", textOutput("Navodila")),
                  tabPanel("Graf", 
                           br(),
                           switchInput(
                             inputId = "Point",
                             label = "Prikaži točke na grafu",
                             labelWidth = "150px",
                             value = FALSE
                           ),
                           numericInput(
                             inputId = "FontSize",
                             label = "Velikost črk na grafu:",
                             value = 11,
                             min = 5,
                             max = 20,
                             step = 1
                           ),
                           plotOutput(outputId = "graf"),
                           downloadButton('downloadData1',"Prenos slike")),
                  tabPanel("Tabele", 
                           tabsetPanel(id = "Tabs", type = "tabs"
                           )),
                  tabPanel("Namen in uporaba", 
                           p(""),
                           p(""),
                           p("Aplikacija je namenjena prikazu in primerjavi indeksov cen nepremičnin, 
                           plač in življenskih potrebščin.
                           Izberete lahko željeno obdobje (od l.2008 do
                           l.2020, kvartalno), tip indeksa (željena primerjava četrtletij) ter vrsto indeksa,
                           ki ga želite imeti predstavljenega v tabeli in grafu."),
                           p(""),
                           p("Posamezne vrste indeksa lahko podrobneje specificirate glede na to kaj
                           vas zanima. Npr.: Bruto ali neto plača, nova stanovanja oz. rabljena, ipd."),
                           p(""),
                           p("Rezultati se prikažejo v zavihkih GRAF in TABELE, ki si jih lahko tudi 
                           prenesete na svoj računalnik."),
                           p("Vir podatkov je podatkovna baza SiStat (https://pxweb.stat.si/SiStat/sl).")
                  )
      )
      
      

      
    )
  )
)


server <- function(input, output) {

  
  NEP <- reactive({
    NEPL %>% 
      filter(
        Datum >= as.yearqtr(input$obdobje[1]),
        Datum <= as.yearqtr(input$obdobje[2]),
        Info == input$Tip,
        STANOVANJSKE_NEPREMIČNINE %in% input$VrstaN
      )
  })
  
  ICZP <- reactive({
    ICZPL_KVART %>% 
      filter(
        Datum >= as.yearqtr(input$obdobje[1]),
        Datum <= as.yearqtr(input$obdobje[2]),
        Info == input$Tip,
        Življenjske_Potrebščine %in% input$VrstaC
      )  
    
  })
  
  PAY <- reactive({
    PAYL_KVART %>% 
      filter(
        Datum >= as.yearqtr(input$obdobje[1]),
        Datum <= as.yearqtr(input$obdobje[2]),
        Info == input$Tip,
        Vrsta_Plače == input$VrstaPlače,
        SKD_DEJAVNOST %in% input$VrstaD
      )
  })
  
  ######################
  ##  Add / Remove tabs!
  ######################
  
  # Nepremičnine
  observeEvent(input$NeprB, {
    if(input$NeprB == TRUE){
    appendTab(inputId = "Tabs", tabPanel("Nepremičnine", 
                                         downloadButton('downloadDataNEP',"Prenos tabele"),
                                         tableOutput(outputId = "tabNEP")), select = T)
    }
  })
  
  observeEvent(input$NeprB, {
    if(input$NeprB == FALSE){
      removeTab(inputId = "Tabs", target = "Nepremičnine")
    }
  })
  
  output$tabNEP <- 
    renderTable({
      if(input$NeprB == T){
        NEP() %>% 
          mutate(Datum = as.character(Datum))}
      else{NULL}
    })
  
  
  # Plače
  observeEvent(input$PlačaB, {
    if(input$PlačaB == TRUE){
      appendTab(inputId = "Tabs", tabPanel("Plače", 
                                           downloadButton('downloadDataPAY',"Prenos tabele"),
                                           tableOutput(outputId = "tabPAY")), select = T)
    }
  })
  
  observeEvent(input$PlačaB, {
    if(input$PlačaB == FALSE){
      removeTab(inputId = "Tabs", target = "Plače")
    }
  })
  
  output$tabPAY <- renderTable({
    if(input$PlačaB == T){
      PAY()%>% 
        mutate(Datum = as.character(Datum))}
    else{NULL}        
  })
  
  # Cene
  observeEvent(input$CeneB, {
    if(input$CeneB == TRUE){
      appendTab(inputId = "Tabs", tabPanel("ICŽP",
                                           downloadButton('downloadDataICZP',"Prenos tabele"),
                                           tableOutput(outputId = "tabICZP")), select = T)
    }
  })
  
  observeEvent(input$CeneB, {
    if(input$CeneB == FALSE){
      removeTab(inputId = "Tabs", target = "ICŽP")
    }
  })
  
  
  output$tabICZP <- renderTable({
    if(input$CeneB == T){
      ICZP()%>% 
        mutate(Datum = as.character(Datum))}
    else{NULL}
  })
  
  #######################
  ### ggplot data
########################

  ICZP_G <- reactive({
    ICZP() %>%
      mutate(
        Skupina = "Cene življenjskih potrebščin"
      ) %>% 
      rename("Podskupina" = Življenjske_Potrebščine)
  })

  PAY_G <- reactive({
    PAY() %>% mutate(
      Skupina = ifelse(input$VrstaPlače == "Bruto", "Plače (bruto)", "Plače (neto)")
    ) %>% 
      rename("Podskupina" = SKD_DEJAVNOST)
  })
  
  NEP_G <- reactive({
    NEP() %>% 
      mutate(
        Skupina = "Nepremičnine"
      ) %>% 
      rename("Podskupina" = STANOVANJSKE_NEPREMIČNINE)
  })
  
  #######################
  ### ggplot ukazi
  ########################
  
  geomL_NEP <- reactive({
    if(input$NeprB){
    geom_line(data = NEP_G())
    } else NULL
  })
  
  geomP_NEP <- reactive({
    if(input$NeprB & input$Point){
      geom_point(data = NEP_G())
    } else NULL
  })
  
  
  
  geomL_ICZP <- reactive({
    if(input$CeneB){
    geom_line(data = ICZP_G())
    } else NULL
  })
  
  geomP_ICZP <- reactive({
    if(input$CeneB & input$Point){
      geom_point(data = ICZP_G())
    } else NULL
  })
  
  
  geomL_PAY <- reactive({
    if(input$PlačaB){
    geom_line(data = PAY_G())
    } else NULL
  })
  
  geomP_PAY <- reactive({
    if(input$PlačaB & input$Point){
      geom_point(data = PAY_G())
    } else NULL
  })
  
  #######################
  ### Grafi
  #######################
  
  Graf <- reactive({
    if(any(input$CeneB, input$PlačaB, input$NeprB)){
      ggplot(Data_ggplot, aes(x = Datum, y = Indeks, 
                              group = interaction(Skupina, Podskupina), 
                              linetype = Skupina, 
                              col = Podskupina)) +
        geomL_NEP() +
        geomP_NEP() +
        geomL_ICZP() +
        geomP_ICZP() +
        geomL_PAY() +
        geomP_PAY() +
        labs(
          y = "Indeks, %",
          title = paste("Primerjava indeksov", input$Tip)
        ) +
        scale_x_yearqtr(format ="%Y Q%q") + 
        theme_bw(base_size = input$FontSize)
        #theme(legend.text=element_text(size=rel(input$FontSize)))
    } else NULL
  })
  
  
  output$graf <- renderPlot({
    Graf()
  })
  
  
  ###########################
  ### Prenos grafa ali tabele
  ###########################
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("graf", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = Graf(), device = 'png', width = 11, height = 7, units = "in")
    }
  )
  
  output$downloadDataICZP <- downloadHandler(
    filename = function() {
      "Indeksi_Cen_Življenjskih_Potrebščin.txt"
    },
    content = function(file) {
      write.csv(ICZP(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataNEP <- downloadHandler(
    filename = function() {
      "Indeksi_Nepremičnin.txt"
    },
    content = function(file) {
      write.csv(NEP(), file, row.names = FALSE)
    }
  )
  
  output$downloadDataPAY <- downloadHandler(
    filename = function() {
      "Indeksi_Plač.txt"
    },
    content = function(file) {
      write.csv(PAY(), file, row.names = FALSE)
    }
  )
  

  
  
  # output$graf <- renderPlot({
  #   if(input$CeneB == T & input$PlačaB ==T){
  #     ICZP() %>% 
  #       full_join(PAY(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste("Življenjske potrebščine -", input$VrstaC), 
  #                                     paste0(input$VrstaPlače, " plača (", input$VrstaD, ")")), 
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #   }
  #   else if(input$NeprB == T & input$PlačaB ==T){
  #     NEP() %>% 
  #       full_join(PAY(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste("Stanovanjske nepremičnine -", input$VrstaN), 
  #                                     paste0(input$VrstaPlače, " plača (", input$VrstaD, ")")), 
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #     
  #   }
  #   else if(input$CeneB == T & input$NeprB ==T){
  #     ICZP() %>% 
  #       full_join(NEP(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste("Življenjske potrebščine -", input$VrstaC), 
  #                                     paste0("Stanovanjske nepremičnine -", input$VrstaN)), 
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #     
  #   }
  #   else if(input$CeneB == T){
  #     ICZP() %>% 
  #       full_join(ICZP(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste("Življenjske potrebščine -", input$VrstaC)),
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #     
  #   }
  #   else if(input$PlačaB == T){
  #     PAY() %>% 
  #       full_join(PAY(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste0(input$VrstaPlače, " plača (", input$VrstaD, ")")),
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #     
  #   }
  #   else if(input$NeprB == T){
  #     NEP() %>% 
  #       full_join(NEP(), by = c("Datum" = "Datum", "Info" = "Info")) %>% 
  #       pivot_longer(cols = c("Indeks.x", "Indeks.y")) %>%
  #       ggplot() +
  #       geom_line(aes(x = Datum, y = value, color = name), size = 1) +
  #       labs(
  #         title = paste("Primerjava indeksov", input$Tip),
  #         x = "Datum",
  #         y = "Indeks, %",
  #         color = ""
  #       ) +
  #       scale_color_manual(labels = c(paste("Stanovanjske nepremičnine -", input$VrstaN)),
  #                          values = c("#00BFC4", "#F8766D") ) +
  #       theme_bw()
  #     
  #   }
  # })
  # 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)



# Deploy ------------------------------------------------------------------

# options(encoding = "UTF-8") # Sicer ne prebere dependencies pravilno!
# deployApp(appName = "PrimerjavaIndeksov")
