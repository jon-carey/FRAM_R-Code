library(shiny)
library(plotly)
library(tidyr)
library(dplyr)

CWT_data <- read.csv("FRAMBuilder_CWT_comparison.csv")
Stk_LUT <- read.csv("StockLUT.csv", stringsAsFactors = FALSE)
Fish_LUT <- read.csv("FishLUT.csv", stringsAsFactors = FALSE)
CWT_data$StockNum <- CWT_data$StockNum /2
CWT_data <- merge(CWT_data, Stk_LUT)
CWT_data$StockTitle <- factor(CWT_data$StockTitle, levels = c(Stk_LUT$StockTitle), ordered = TRUE)
CWT_data$FisheryName <- factor(CWT_data$FisheryName, levels = c(Fish_LUT$FisheryTitle), ordered = TRUE)
colnames(CWT_data)[13:14] <- c("Expanded", "Nominal")
CWT_data_long <- CWT_data %>% 
  gather(RecType, Recs, 13:14) %>%
  filter(Notes == "") %>%
  group_by(Version, StockTitle, FisheryName, RecType) %>%
  summarize(Recoveries = sum(Recs))

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("FRAMBuilder CWTs by Stock"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 8,

      # Input: stock selector ----
      selectInput(inputId = "stock",
                  label = "Select stock:",
                  choices = as.character(unique(Stk_LUT$StockTitle)))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: 
      plotlyOutput(outputId = "Plot", width = "120%", height = "600px")

    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  CWT_stock <- reactive({
    CWT_data_long %>%
      filter(StockTitle == input$stock & !(FisheryName == "Escapement"))
  })
  
  output$Plot <- renderPlotly({

    print(
      ggplotly(
        ggplot(data = CWT_stock(), aes(FisheryName, Recoveries, fill = Version)) +
          geom_bar(width=0.7, size=0.2, color = "black", alpha=1, position="dodge", stat="identity") +
          scale_fill_manual(values = c("#66CDAA","#6CA6CD")) +
          facet_grid(RecType ~ ., scales = "free_y") +
          ggtitle(paste(input$stock)) +
          xlab("") +
          ylab("Recoveries") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          theme(legend.position = "top", legend.title = element_blank())
      )
    ) %>%
      layout(legend = list(orientation = "h", x = 0.8, y = 1.1))
    
    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
