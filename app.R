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
library(gtable)
library(shinyjs)
library(geofacet)
require(mapproj)
library(showtext)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Генератор мапи заборгованості за газ"),
  
  # Sidebar layout with input and output definitions ----
  useShinyjs(),
  column(7,
         
         fluidRow( 
           
           sidebarPanel(
             
             # Input: Select a file ----
             fileInput("file1", "Оберіть csv-файл з даними. Розділювач - крапка с комою, кодування - Windows-1251",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             tags$br(),
             disabled(sliderInput("date_range", "Часовий проміжок. Якщо даних на точну дату немає, буде обрано найближчу з наявних", 
                                  min = as.Date("2018-01-01"), 
                                  max = as.Date("2018-12-31"), 
                                  value = c(as.Date("2018-09-17"), as.Date("2018-10-17")),
                                  timeFormat = "%d.%m.%Y")),
             tags$br(),
             radioButtons("map_type", "Тип графіка: ", choices = c("борг", "борг і зміни", "зміни кольором", "зміни графіком")),
             helpText("Мапа генерується і змінюється тільки після натискання кнопки знизу:"),
             fluidRow(disabled(actionButton("generate_button", "Згенерувати мапу", width = "100%")))
             
           ),
           
           column(width = 8,
                  plotOutput("ukraine", width = "750px",height =  "500px" ))
         ),
         wellPanel(
           fluidRow( #tags$hr(),
             helpText("Після того, як мапа згенерувалася, ви можете завантажити її на диск."),
             column(1, 
                    radioButtons("extension", "Формат: ", choices = c("png", "pdf"))),
             column(2,
                    radioButtons("goal", "Призначення: ", choices = c("facebook", "презентація"))),
             column(4, 
                   textInput("customtitle", label = "Заголовок на мапі у файлі")),
             column(2,
                    disabled(downloadButton('downloadData', 'Завантажити мапу')))))
         
  )
)

# Define server logic to read selected file ----
server <- function(session, input, output) {
  options(warn=-1)
  source("geofacet_functions.R")
  logo <- readPNG("qr_code_small.png")
  system("mkdir ~/.fonts")
  download.file(url = "https://github.com/localizator/ukrainian-fonts-pack/raw/master/BlissPro-Light%20-%20Bliss%20Pro%20-%20Light.ttf",
                destfile = "~/.fonts/Bliss Pro Light.ttf")
  system('fc-cache -fEV ~/.fonts') 
  cents_table <- read.csv("cents_table.csv")
  find_closest_date <- function(d, dates) {
    absolutes <- abs(dates - d)
    dates[which(absolutes == min(absolutes))]
  }
  

  
  upload_react <- observeEvent(input$file1, {
    req(input$file1)
    tryCatch(
      {
        debt <- read_delim(input$file1$datapath, delim = ";", locale = locale(decimal_mark = ",", encoding = "Windows-1251"))
        names(debt) <-  c('date', 'region_code', 'region', 'goal', 'debt_total', 'debt_2018')
        debt$date <- as.Date(debt$date, format = "%d.%m.%Y")
        # активуємо деактивовані елементи вводу
        enable("generate_button")
        enable("date_range")
        # змінюємо значення слайдера виходячи з наявних дат
        updateSliderInput(session = session, "date_range", value = c(max(debt$date) - 30, max(debt$date)), max = max(debt$date), min = min(debt$date))
        ukr_adm2 <- readOGR("./simplified_shapefiles/UKR_adm1-2.shp")
        ukrainian_names <- read.csv("map_correspondence.csv")
        map_extended <- append_data(ukr_adm2, ukrainian_names,  key.shp = "ID_1", key.data = "ID_1")
        # переносимо змінні в глобальне середовище
        debt <<- debt
        map_extended <<- map_extended
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  
  generate_react <- observeEvent(input$generate_button, {
    output$ukraine <- renderPlot ({
      date1 <- find_closest_date(isolate(input$date_range[1]), unique(debt$date))
      date2 <- find_closest_date(isolate(input$date_range[2]), unique(debt$date))
      map_type <- isolate(input$map_type)
      date_string <- as.character(date2, format = "%d.%m.%Y")
      second_date_string <- as.character(date1, format = "%d.%m.%Y")
      if (map_type == "борг і зміни") {
        debt <- debt %>%
          dplyr::filter(date %in% c(date1, date2)) %>% 
          dplyr::group_by(region_code) %>% 
          dplyr::arrange(region_code, date) %>% 
          dplyr::mutate(changes = c(0,diff(debt_total))) %>% 
          dplyr::mutate(changes_percent = paste(round(100 * (changes / debt_total), 2), "%", sep="")) %>% 
          data.frame()
        debt_plus <- paste0("+", debt$changes_percent)
        debt$changes_percent[debt$changes > 0] <- debt_plus[debt$changes > 0]
        debt$color <- sapply(debt$changes, function(x) {ifelse(x >= 0, "red", "green")})
        debt <- filter(debt, date == date2)
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
        # orange palette
        sc <- scale_fill_continuous(low = "#fff7bc", high = "#d95f0e", breaks = map_extended@data$debt_total) 
        map_extended@data$color <- sc$palette(sc$rescaler(map_extended@data$debt_total))
        scale_value <- map_extended@data$color
        names(scale_value) <- as.character(map_extended@data$debt_total)
        ukr_df <- broom::tidy(map_extended, region = "id")
        ukr_df <- plyr::join(ukr_df, map_extended@data, by = "id") %>% 
          dplyr::arrange(desc(debt_total))
        millions <- as.character(round(ukr_df$debt_total / 1000000, 1))
        ukr_df$label <- paste(ukr_df$UA_NAME, "–", millions, "")
        # ukr_df$label <- mapply(function(d,r){
        #   billions <- format(round(d / 1000000, 1), decimal.mark = ",", big.mark = " ")
        #   paste(r, "–", billions, "")
        # }, ukr_df$debt_total, ukr_df$UA_NAME)
        subtitle <- paste0('Сумарний накопичений борг перед НАК "Нафтогаз України" в розрізі регіонів\n', "Дані станом на ", 
                           date_string, ". Відсотки у порівнянні із ", second_date_string, ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
        ukr_df$debt_total <- factor(as.character(ukr_df$debt_total), 
                                    levels = as.character(unique(ukr_df$debt_total)), ordered = TRUE)
        legend_labels <- factor(unique(ukr_df$label), levels = unique(ukr_df$label), ordered = TRUE)
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
                          family = "Bliss Pro Light", min.segment.length = 0.2) +
          geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                          aes(x = coords.x1, y = coords.x2, label = changes_percent, color = color), 
                          inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, size = 2.5, segment.size = 0.15,
                          family = "Bliss Pro Light", min.segment.length = 0.2) +
          scale_fill_manual(name = "Борг, млн грн", values = scale_value, labels = legend_labels) +
          scale_color_manual(values = c(green = "darkgreen", red = "darkred"), guide=FALSE) +
          theme(legend.key.size = unit(0.14, units = "inches")) +
          guides(fill = guide_legend(ncol = 1)) +
          theme(axis.line = element_blank(), 
                axis.text = element_blank(),
                axis.ticks = element_blank(), axis.title = element_blank(),
                rect = element_blank(),
                text = element_text(family = "Bliss Pro Light"),
                legend.text = element_text(size = 8.5),
                plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                legend.title = element_text(face = "bold"),
                plot.subtitle = element_blank(),
                plot.caption = element_text(color = "dimgrey", size = 8)) +
          labs(caption = subtitle) +
          coord_map()
        updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
        pl_local <- ggplot_gtable(ggplot_build(pl))
        leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
        caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
        pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 3
        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
        pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
        enable("downloadData")
        grid.draw(pl_local)
      } else { if (map_type == "зміни графіком") {
        debt <- debt %>% 
          filter(date >= date1, date <= date2)
        ukraine_grid <- read.csv("grid.csv")
        names(debt)[names(debt) == "region"] <- "name"
        debt$name[which(debt$name == "місто Київ")] <- "Київ"
        debt$name[which(debt$name == "місто Севастополь")] <- "Севастополь"
        debt$year <- format(debt$date, format = "%Y")
        debt_last <- dplyr::filter(debt, date == max(date))
        subtitle <- paste0('Зміни сумарного накопиченого боргу перед НАК "Нафтогаз України" з ', 
                           second_date_string, " до ", date_string, ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
        dates_diff <- as.numeric(max(debt$date) - min(debt$date))
        date_breaks <-  c(min(debt$date) + round(dates_diff / 10), mean(debt$date), max(debt$date) - round(dates_diff / 10))
        pl <<- ggplot(debt, aes(x = date, y = debt_total, col = year)) +
          geom_smooth(alpha = 0.5, se = FALSE, size = 0.7) +
          ggtitle("Борги виробників тепла за газ: зміни по регіонам") + 
          facet_geo(~name, grid = ukraine_grid, scales = "fixed") +
          scale_x_date(breaks = date_breaks, date_labels = "%d.%m") +
          scale_y_continuous(trans = "sqrt", minor_breaks = NULL,
                             labels = function(br){
                               all_labels <- sapply(br, function(d) {
                                 billions <- format(round(d / 1000000000, 1), decimal.mark = ",", big.mark = "")
                                 millions <- format(round(d / 1000000, 1), decimal.mark = ",", big.mark = "")
                                 ifelse(billions >= 1, paste0(billions, " млрд"), paste0(millions, " млн"))})
                               first_non_na <- which(!is.na(br))[1]
                               labels <- all_labels[(first_non_na + 1):(length(all_labels)-1)]
                               reversed_elements <- rev(seq_along(labels))
                               labels_to_exclude <- reversed_elements[seq_along(reversed_elements) %% 2 == 1]
                               labels[labels_to_exclude] <- ""
                               c(all_labels[1:first_non_na], labels, all_labels[length(all_labels)])
                             }) +
          theme(
            plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
            axis.ticks = element_blank(), axis.title = element_blank(),
            rect = element_rect(fill = "white"),
            legend.position = "bottom",
            text = element_text(family = "Bliss Pro Light"),
            axis.text.y = element_text(size = 8, vjust = 1),
            axis.text.x = element_text(vjust = 0),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.background = element_rect(fill =  "#deebf7"),
            strip.background = element_rect(fill = "#c6dbef"),
            legend.title = element_blank()) +
          labs(caption = subtitle)
        updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
        pl_local <- print.facet_geo(pl)
        leftest_panel <- min(pl_local$layout$l[grepl("panel-", pl_local$layout$name)])
        caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
        pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2 , leftest_panel, caption_row, leftest_panel, name = "logo")
        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
        pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
        enable("downloadData")
        grid.draw(pl_local)
      } else {
        if (map_type == "зміни кольором") {
          debt <- debt %>%
            dplyr::filter(date %in% c(date1, date2)) %>% 
            dplyr::group_by(region_code) %>% 
            dplyr::arrange(region_code, date) %>% 
            dplyr::mutate(changes = c(0,diff(debt_total))) %>% 
            dplyr::mutate(changes_percent = paste(round(100 * (changes / debt_total), 2), "%", sep="")) %>% 
            data.frame()
          debt <- filter(debt, date == date2)
          map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
          map_extended@data$id <-  rownames(map_extended@data)
          ukr_df <- broom::tidy(map_extended, region = "id")
          total_df <- join(ukr_df, map_extended@data, by = "id") %>% 
            dplyr::arrange(desc(debt_total))
          changes_df <- join(ukr_df, map_extended@data, by = "id") %>% 
            dplyr::arrange(desc(changes))
          sc_changes <- scale_colour_gradient2(low = "#2c7bb6", high = "#d7191c", breaks = changes_df$changes) 
          changes_df$color_changes <- sc_changes$palette(sc_changes$rescaler(changes_df$changes))
          scale_changes_value <- changes_df$color_changes
          names(scale_changes_value) <- as.character(changes_df$changes)
          changes_millions <- as.character(round(changes_df$changes / 1000000, 1))
          changes_df$label <-  paste(changes_df$UA_NAME, "–", ifelse(changes_millions > 0, paste0("+", changes_millions), changes_millions), "")
          changes_df$changes <- factor(as.character(changes_df$changes), 
                                       levels = as.character(unique(changes_df$changes)), ordered = TRUE)
          changes_labels <- factor(unique(changes_df$label), levels = unique(changes_df$label), ordered = TRUE)
          subtitle <- paste0('\nЗміни сумарного накопиченого боргу перед НАК "Нафтогаз України з ', 
                             second_date_string, " до ", date_string, 
                             ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
          pl <<- ggplot(changes_df, aes(x = long, y= lat, group = group, fill = changes)) + 
            ggtitle("Зміна боргу за поставки газу для виробництва тепла") +
            geom_polygon() +
            geom_polygon(data = subset(changes_df, !(id %in% c(10,19)))) + 
            geom_polygon(data = subset(changes_df, (id %in% c(10,19)))) +
            geom_path(color = "black", size = 0.05) +
            scale_fill_manual(name = "Зміна боргу, млн грн", values = scale_changes_value, labels = changes_labels) +
            theme(legend.key.size = unit(0.14, units = "inches")) +
            geom_text(data = dplyr::filter(cents_table, region_code != "09", region_code != "9", region_code != "19"), 
                      aes(x = coords.x1, y = coords.x2, label = UA_NAME), inherit.aes = FALSE, family = "Bliss Pro Light",
                      size = 2) +
            geom_text_repel(data = dplyr::filter(cents_table, region_code == "9"), 
                            aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                            inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.5, segment.size = 0.15,
                            family = "Bliss Pro Light", min.segment.length = 0.2, size = 2) +
            geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                            aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                            inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, segment.size = 0.15,
                            family = "Bliss Pro Light", min.segment.length = 0.2, size = 2) +
            guides(fill = guide_legend(ncol = 1)) +
            theme(axis.line = element_blank(), 
                  axis.text = element_blank(),
                  axis.ticks = element_blank(), axis.title = element_blank(),
                  rect = element_blank(),
                  text = element_text(family = "Bliss Pro Light"),
                  legend.text = element_text(size = 8.5),
                  plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                  legend.title = element_text(face = "bold"),
                  plot.subtitle = element_blank(),
                  plot.caption = element_text(color = "dimgrey", size = 8)) +
            labs(caption = subtitle)
          updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
          pl_local <- ggplot_gtable(ggplot_build(pl))
          leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
          caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
          pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
          pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 5
          pl_local$grobs[[which(pl_local$layout$name == "logo")]]$vjust <- 0
          pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(0.8, "in")
          pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(0.8, "in")
          pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
          enable("downloadData")
          grid.draw(pl_local)
        } else {
          if ((map_type == "борг")) {
            debt <- filter(debt, date == date2)
            map_extended <- append_data(map_extended, debt,  key.shp = "UA_NAME", key.data = "region")
            map_extended@data$id <-  rownames(map_extended@data)
            sc <- scale_fill_continuous(low = "#fff7bc", high = "#d95f0e", breaks = map_extended@data$debt_total) 
            map_extended@data$color <- sc$palette(sc$rescaler(map_extended@data$debt_total))
            scale_value <- map_extended@data$color
            names(scale_value) <- as.character(map_extended@data$debt_total)
            ukr_df <- broom::tidy(map_extended, region = "id")
            ukr_df <- plyr::join(ukr_df, map_extended@data, by = "id") %>% 
              dplyr::arrange(desc(debt_total))
            millions <- as.character(round(ukr_df$debt_total / 1000000, 1))
            ukr_df$label <- paste(ukr_df$UA_NAME, "–", millions, "")
            subtitle <- paste0('Сумарний накопичений борг перед НАК "Нафтогаз України" в розрізі регіонів\n', "Дані станом на ", 
                               date_string, ".", ".\nЗавантажити дані: https://data.gov.ua/dataset/75140072-160d-4f87-ac08-a75a1d3557e8")
            ukr_df$debt_total <- factor(as.character(ukr_df$debt_total), 
                                        levels = as.character(unique(ukr_df$debt_total)), ordered = TRUE)
            legend_labels <- factor(unique(ukr_df$label), levels = unique(ukr_df$label), ordered = TRUE)
            pl <<- ggplot(ukr_df, aes(x = long, y= lat, group = group, fill = debt_total)) + 
              ggtitle("Борги виробників тепла за газ") +
              geom_polygon() +
              geom_polygon(data = subset(ukr_df, !(id %in% c(10,19)))) + 
              geom_polygon(data = subset(ukr_df, (id %in% c(10,19)))) + 
              geom_path(color = "black", size = 0.05) +
              geom_text(data = dplyr::filter(cents_table, region_code != "9", region_code != "19"), 
                        aes(x = coords.x1, y = coords.x2, label = UA_NAME), inherit.aes = FALSE, size = 2, 
                        family = "Bliss Pro Light") +
              geom_text_repel(data = dplyr::filter(cents_table, region_code == "9"), 
                              aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                              inherit.aes = FALSE, nudge_y = 0.5, nudge_x = -0.3, size = 2, segment.size = 0.15,
                              family = "Bliss Pro Light", min.segment.length = 0.2) +
              geom_text_repel(data = dplyr::filter(cents_table, region_code == "19"), 
                              aes(x = coords.x1, y = coords.x2, label = UA_NAME), 
                              inherit.aes = FALSE, nudge_x = -0.5, nudge_y = 0.3, size = 2, segment.size = 0.15,
                              family = "Bliss Pro Light", min.segment.length = 0.2) +
              scale_fill_manual(name = "Борг, млн грн", values = scale_value, labels = legend_labels) +
              scale_color_manual(values = c(green = "darkgreen", red = "darkred"), guide=FALSE) +
              theme(legend.key.size = unit(0.14, units = "inches")) +
              guides(fill = guide_legend(ncol = 1)) +
              theme(axis.line = element_blank(), 
                    axis.text = element_blank(),
                    axis.ticks = element_blank(), axis.title = element_blank(),
                    rect = element_blank(),
                    text = element_text(family = "Bliss Pro Light"),
                    legend.text = element_text(size = 8.5),
                    plot.title = element_text(size = 10, family = "SourceSansPro", face = "bold"),
                    legend.title = element_text(face = "bold"),
                    plot.subtitle = element_blank(),
                    plot.caption = element_text(color = "dimgrey", size = 8)) +
              labs(caption = subtitle) +
              coord_map()
            updateTextInput(session, inputId = "customtitle", value = pl$labels$title)
            pl_local <- ggplot_gtable(ggplot_build(pl))
            leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
            caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
            pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- 3
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(1, "in")
            pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(1, "in")
            pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
            enable("downloadData")
            grid.draw(pl_local)
          }
        }
      }
      }
    })
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      return(paste("map", input$extension, sep = "."))
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      pl <- pl + ggtitle(isolate(input$customtitle))
      if (input$goal == "facebook") {
        w = 8.5
        h = 4.85
        dp = 130
        if (is.null(pl$data$facet_col)) {
          pl <- pl + theme(legend.text = element_text(size = 8.5),
                           legend.title = element_text(face = "bold", size = 9),
                           legend.box.spacing = unit(1.2, "cm"),
                           plot.title = element_text(size = 12)) +
            scale_y_continuous(expand = c(0,0)) + 
            scale_x_continuous(expand = c(0,0))
          hjust_cor <- -1.3
          vjust <- 0.28
          pl_local <- ggplot_gtable(ggplot_build(pl))
        } else {
          hjust_cor <- -3.6
          vjust <- 0.4
          pl <- pl +
            theme(axis.text = element_text(size = 6),
                  axis.text.y = element_text(size = 6),
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  strip.text = element_text(size = 6, lineheight = 0.1),
                  legend.box.spacing = unit(0.0, "cm"),
                  panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0.2, "lines"),
                  plot.title = element_text(hjust = 0.5),
                  plot.caption = element_text(size = 8, family = "Bliss Pro Light"))
          pl_local <- print.facet_geo(pl)
          
          strips <- which(grepl("strip-", pl_local$layout$name))
          tops <- unique(pl_local$layout$t[strips])
          for (i in strips) {
            un <- attr(pl_local$grobs[[i]]$heights, "unit")
            n <- as.numeric(pl_local$grobs[[i]]$heights)
            pl_local$grobs[[i]]$heights <- unit(n/2, un)  
          }
          
          for (i in tops) {
            un <- attr(pl_local$heights[[i]], "unit")
            n <- as.numeric(pl_local$heights[[i]])
            pl_local$heights[[i]] <- unit(n/2, un)  
          }
        }
      } else {if (input$goal == "презентація") {
        w = 8
        h = 5.66
        dp = 300
        if (is.null(pl$data$facet_col)) {
          pl <- pl + 
            scale_y_continuous(expand = c(0,1)) + 
            scale_x_continuous(expand = c(0,0)) +
            theme(legend.text = element_text(size = 8.5),
                  legend.title = element_text(face = "bold", size = 9),
                  plot.title = element_text(size = 13, hjust = 0.5))
          hjust_cor <- -1
          vjust <- 0.25
          pl_local <- ggplot_gtable(ggplot_build(pl))
        } else {
          hjust_cor <- -2.9
          vjust <- 0.35
          pl <- pl +
            theme(axis.text = element_text(size = 6),
                  axis.text.y = element_text(size = 6),
                  panel.grid.major = element_line(size = 0.1),
                  panel.grid.minor = element_line(size = 0.1),
                  strip.text = element_text(size = 6, lineheight = 0.1),
                  legend.box.spacing = unit(0.0, "cm"),
                  panel.spacing.x = unit(0.2, "lines"),
                  panel.spacing.y = unit(0.2, "lines"),
                  plot.title = element_text(hjust = 0.5),
                  plot.caption = element_text(size = 8, family = "Bliss Pro Light"))
          pl_local <- print.facet_geo(pl)
          strips <- which(grepl("strip-", pl_local$layout$name))
          tops <- unique(pl_local$layout$t[strips])
          for (i in strips) {
            un <- attr(pl_local$grobs[[i]]$heights, "unit")
            n <- as.numeric(pl_local$grobs[[i]]$heights)
            pl_local$grobs[[i]]$heights <- unit(n/2, un)  
          }
          for (i in tops) {
            un <- attr(pl_local$heights[[i]], "unit")
            n <- as.numeric(pl_local$heights[[i]])
            pl_local$heights[[i]] <- unit(n/2, un)  
          }
          
        }
      }}
      
      leftest_panel <- min(pl_local$layout$l[grepl("panel", pl_local$layout$name)])
      caption_row  <- pl_local$layout$b[pl_local$layout$name == "caption"]
      pl_local <- gtable_add_grob(pl_local, rasterGrob(logo), caption_row - 2, leftest_panel, caption_row, leftest_panel, name = "logo")
      logo_side <- h / 5 
      pl_local$grobs[[which(pl_local$layout$name == "logo")]]$height <- unit(logo_side, "in")
      pl_local$grobs[[which(pl_local$layout$name == "logo")]]$width <-  unit(logo_side, "in")
      
      hjust <- (w / 2) / logo_side + hjust_cor
      pl_local$grobs[[which(pl_local$layout$name == "logo")]]$hjust <- hjust
      pl_local$grobs[[which(pl_local$layout$name == "logo")]]$vjust <- vjust
      
      # Write to a file specified by the 'file' argument
      #print(file)
      #png(file = "myplot.png", bg = "transparent")
      if (input$extension == "pdf") {
        dev <- cairo_pdf
      } else {
        dev <- "png"
      }
      pl_local$layout$clip[which(pl_local$layout$name == "logo")] <- "off"
      ggsave(file, pl_local, width = w, height = h, dpi = dp, device = dev)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
