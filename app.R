library(shiny)
library(tmap)
library(tmaptools)
library(plyr)
library(readr)
library(broom)
library(dplyr)
library(rgdal)
library(ggplot2)
library(maptools)
library(GISTools)
library(ggrepel)
library(gridExtra)
library(grid)
library(png)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Генератор мапи заборгованості за газ"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,

      # Input: Select a file ----
      fileInput("file1", "Оберіть csv-файл з даними",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
      
      tags$hr(),
      downloadButton('downloadData', 'Завантажити мапу')


  
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 6,

      # Output: Data file ----
      plotOutput("ukraine")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$ukraine <- renderPlot({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        debt <- read_delim(input$file1$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "Windows-1251"))
        names(debt) <-  c('date', 'region_code', 'region', 'goal', 'debt_total', 'debt_2018')
        debt$date <- as.Date(debt$date, format = "%d.%m.%Y")
        # # Підраховуємо зміну з переостанньої дати
        debt <- debt %>% 
          dplyr::group_by(region_code) %>% 
          dplyr::arrange(region_code, date) %>% 
          dplyr::mutate(changes = c(0,diff(debt_total))) %>% 
          dplyr::mutate(changes_percent = paste(round(100 * (changes / debt_total), 2), "%", sep="")) %>% 
          data.frame()
        debt_plus <- paste0("+", debt$changes_percent)
        debt$changes_percent[debt$changes > 0] <- debt_plus[debt$changes > 0]
        debt$color <- sapply(debt$changes, function(x) {ifelse(x >= 0, "red", "green")})
        second_date <- sort(unique(debt$date), decreasing = TRUE)[2]
        debt <- filter(debt, date == max(date))
        date_string <- as.character(max(debt$date), format = "%d.%m.%Y")
        second_date_string <- as.character(second_date, format = "%d.%m.%Y")
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ukr_adm2 <- readOGR("./ukraine_shapefiles/UKR_adm1.shp")
    ukrainian_names <- read.csv("map_correspondence.csv")
    map_extended <- append_data(ukr_adm2, ukrainian_names,  key.shp = "ID_1", key.data = "ID_1")
    map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
    map_extended@data$id <-  rownames(map_extended@data)
    
    # Шукаємо центроїди для тексту
    cents <- coordinates(map_extended)
    cents <- SpatialPointsDataFrame(coords=cents, data=map_extended@data)
    cents_table <- broom::tidy(cents, region = "id")
    # Корегуємо розташування центроїдів для Київської та Одеської області
    cents_table$coords.x2[cents_table$region_code == "10"] <- cents_table$coords.x2[cents_table$region_code == "10"] - 0.5
    cents_table$coords.x2[cents_table$region_code == "16"] <- cents_table$coords.x2[cents_table$region_code == "16"] + 0.25
    cents_table$coords.x1[cents_table$region_code == "16"] <- cents_table$coords.x1[cents_table$region_code == "16"] + 0.4
    cents_table$coords.x1[cents_table$region_code == "18"] <- cents_table$coords.x1[cents_table$region_code == "18"] + 0.2
    cents_table$coords.x2[cents_table$region_code == "18"] <- cents_table$coords.x2[cents_table$region_code == "18"] + 0.3
    cents_table$coords.x1[cents_table$region_code == "19"] <- cents_table$coords.x1[cents_table$region_code == "19"] - 0.1
    
    
    
    #green pallete
    #sc <- scale_fill_continuous(low = "#e5f5f9", high = "#2ca25f", breaks = map_extended@data$debt_total) 
    #red pallete
    #sc <- scale_fill_continuous(low = "#fee8c8", high = "#e34a33", breaks = map_extended@data$debt_total) 
    # red single
    #sc <- scale_fill_continuous(low = "#fee0d2", high = "#de2d26", breaks = map_extended@data$debt_total) 
    # green single
    #sc <- scale_fill_continuous(low = "#edf8e9", high = "#006d2c", breaks = map_extended@data$debt_total) 
    # orange palette
    sc <- scale_fill_continuous(low = "#fff7bc", high = "#d95f0e", breaks = map_extended@data$debt_total) 
    # blue single
    #sc <- scale_fill_continuous(low = "#deebf7", high = "#3182bd", breaks = map_extended@data$debt_total) 
    # diverging palette
    #sc <- scale_fill_continuous(low = "#1a9641", high = "#d7191c", breaks = map_extended@data$debt_total) 
    map_extended@data$color <- sc$palette(sc$rescaler(map_extended@data$debt_total))
    scale_value <- map_extended@data$color
    names(scale_value) <- as.character(map_extended@data$debt_total)
    ukr_df <- broom::tidy(map_extended, region = "id")
    ukr_df <- plyr::join(ukr_df, map_extended@data, by = "id") %>% 
      dplyr::arrange(desc(debt_total))
    ukr_df$label <- mapply(function(d,r){
      billions <- format(round(d / 1000000, 1), decimal.mark = ",", big.mark = " ")
      paste(r, "–", billions, "")
    }, ukr_df$debt_total, ukr_df$UA_NAME)
    subtitle <- paste0('Сумарний накопичений борг перед НАК "Нафтогаз України" в розрізі регіонів\n', "Дані станом на ", 
                      date_string, ". Відсотки у порівнянні із ", second_date_string, ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
    #subtitle_gr <- grid.text(subtitle, gpar(color = "dimgrey", size = 9)) 
    ukr_df$debt_total <- factor(as.character(ukr_df$debt_total), 
                                levels = as.character(unique(ukr_df$debt_total)), ordered = TRUE)
    legend_labels <- factor(unique(ukr_df$label), levels = unique(ukr_df$label), ordered = TRUE)
    logo <- readPNG("dl.png")
    pl <<- ggplot(ukr_df, aes(x = long, y= lat, group = group, fill = debt_total)) + 
      ggtitle("Борги виробників тепла за газ") +
      geom_polygon() +
      geom_polygon(data = subset(ukr_df, !(id %in% c(10,19)))) + 
      geom_polygon(data = subset(ukr_df, (id %in% c(10,19)))) + 
      geom_path(color = "black", size = 0.05) +
      geom_text(data = dplyr::filter(cents_table, region_code != "09", region_code != "19"), 
                aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), inherit.aes = FALSE, size = 2.5, 
                family = "Bliss Pro Light") +
      geom_text_repel(data = dplyr::filter(cents_table, region_code == "09"), 
                      aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), 
                      inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.3, size = 2.5, segment.size = 0.15,
                      family = "Bliss Pro Light") +
      geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                      aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), 
                      inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, size = 2.5, segment.size = 0.15,
                      family = "Bliss Pro Light"  ) +
      #scale_fill_continuous(low = "#e5f5f9", high = "#2ca25f", breaks = map_extended@data$debt_total) +
      scale_fill_manual(name = "Борг, млн грн", values = scale_value, labels = legend_labels) +
      #scale_color_manual(values = c(green = "#4daf4a", red = "#e41a1c"), guide=FALSE) +
      scale_color_manual(values = c(green = "darkgreen", red = "darkred"), guide=FALSE) +
      #theme(legend.position = "bottom") +
      theme(legend.key.size = unit(0.14, units = "inches")) +
      #theme(legend.spacing = unit(3, units = "pt")) +
      guides(fill = guide_legend(ncol = 1)) +
      theme(axis.line = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank(), axis.title = element_blank(),
            rect = element_blank(),
            #legend.text = element_text(size = 11))
            text = element_text(family = "Bliss Pro Light"),
            legend.text = element_text(size = 8.5),
            plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
            legend.title = element_text(face = "bold"),
            plot.subtitle = element_blank(),
            #plot.subtitle = element_text(color = "dimgrey", size = 9),
            plot.caption = element_text(color = "dimgrey", size = 8)) +
      labs(caption = subtitle) +
      coord_map() 
      #coord_fixed(111/39.8) +
      #annotation_custom(rasterGrob(logo), xmin = 22.2, xmax = 23.8, ymin = 43.230, ymax = 43.830)
    
    pl <<- ggplot_gtable(ggplot_build(pl))
    pl <<- gtable_add_grob(pl, rasterGrob(logo), 9, 4, 9, 9, name = "logo")
    #pl$grobs[[which(pl$layout$name == "logo")]]$just <<- "left"
    pl$grobs[[which(pl$layout$name == "logo")]]$hjust <<- 8
    #pl$layout$clip[pl$layout$name=="panel"] <<- "off"
    grid.draw(pl)
    #pl
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      return("map.png") 
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {

      
      # Write to a file specified by the 'file' argument
      #print(file)
      #png(file = "myplot.png", bg = "transparent")
      ggsave(file, pl, width = 8, height = 5.66)
    }
  )

}

# Create Shiny app ----
shinyApp(ui, server)
