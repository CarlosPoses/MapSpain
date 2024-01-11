# Libraries
library(shiny)
library(shinyWidgets)
library(sf)
library(tidyverse)
library(showtext)
library(ggsflabel)

# Map data
europe <- readRDS("data/europe.rds")
municipios <- readRDS("data/municipios.rds") 
trens <- readRDS("data/trens.rds")

# Setting seed for reproducibility
set.seed(122)

# Municipalities of cataluña
municipios_cat <- municipios %>% filter(ine.ccaa.name == "Cataluña")

# Create synthetic dataframe by sampling municipalities
NAMEUNIT <- c(
  sample(municipios_cat$name, 90, replace = FALSE), 
  sample(municipios$name, 90, replace = FALSE))
cor <- c(rep("Pa", 2), rep("Ma", 1), rep("Ambos", 2), rep("Visita", 175))
en_mapa <- rep("SI", 180)

lugares <- data.frame(NAMEUNIT = NAMEUNIT,
                      cor = cor,
                      en_mapa = en_mapa)

# Join dataframes
municipios <- municipios %>% 
  left_join(lugares, by = c("name" = "NAMEUNIT")) %>% 
  mutate(cor = ifelse(is.na(en_mapa), "Ningun", cor)) 

# Plotting parameters
showtext::showtext.auto()
annotation_size <- 3.75
fonte <- "Playfair Display"
font_add_google(fonte)
# estetica "#FAD510FF"
background <- "white"
pa <- "#92140C"
ma <- "#F1F500"
ambos <- "#F7C4A5"
visita <- "#0031B8"  
liñas <- "grey20"
text_colour <- "grey20"
custom_colors <- c(Pa = pa, Ma = ma, Ningun = background, Ambos = ambos, Visita = visita)



# Define UI for application
ui <- fluidPage(
  fluidRow(column(9,wellPanel(h1("A personalized map"),
           p("This is a map of the municipalities of Spain where Person 1 and Person 2 have lived
             separetely (in yellow and red, and labelled), lived together (in pink and labelled), and visited (in blue, unlabelled).
             Person 1 and Person 1 are not moving anymore in the foreseeable future,
             so where they have lived is not customizable.
             However, they are likely to still visit more places. They can update the 
             new municipalities they visit, as well as unselect municipalities wrongly marked as visited,
             in the right-side panel"),
          p("All the data is synthetic, simulating two people that lived in and mostly visited Catalonia.
            This project started as a private project for two real people, using real data.
            This online version is a showcase and does not include any real data"),
          p("The grey lines indicate the train lines, included only for aesthetic purpose.
            Map data comes from Eurostat and the Spanish National Geographic Institute.")),
  )),
  fluidRow(
    column(9,
             plotOutput("Plot",
                        height = "700px")
    ),  # Add a closing parenthesis for the column function
    
    column(3,
           wellPanel("Visited Concellos",
                        multiInput(inputId = "concellos",
                                   label = "Concellos",
                                   choices = municipios$name,
                                   selected = lugares$NAMEUNIT)
    )
    
  )))




# Define server
server <- function(input, output) {
  plot <-
    ggplot() +
    geom_sf(
      data = trens,
      aes(color = "State railways network"),
      fill = NA,
      linewidth = 0.5) +
    scale_color_manual(values = alpha(liñas, 0.06),
                       name = NULL) +
    scale_size_identity() +
    geom_sf(data = europe,
            color = liñas,
            fill = NA,
            key_glyph = "rect") +
    coord_sf(ylim = c(35.5,45.5),
             xlim = c(-10,4),
             clip = "on") +
    theme(legend.text = element_text(family = fonte, colour = text_colour, size = 11,
                                     face = "plain"),
          legend.title = element_text(family = fonte, colour = text_colour, size = 14,
                                      face = "plain"),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = background, color = NA),
          panel.background = element_rect(fill = background, color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(0,0,0,0), "cm"),
          panel.spacing = unit(c(0,0,0,0), "cm"),
          #  legend.title = element_blank(),
          legend.position = c(0.91, 0.06),
          legend.key = element_blank(),
          legend.key.width = unit(1, "cm"),  
          legend.key.height = unit(1, "cm"),
          legend.spacing.y = unit(.1, "cm"),
          legend.margin = margin(-0.25,0,0,0, unit="cm"),
          legend.title.align=0.5) +
    scale_fill_manual(name = "Municipalities",
                      values = custom_colors, 
                      breaks = c("Pa", "Ma", "Ambos", "Visita"),
                      labels = c("Person 1", "Person 2", "Both", "Visited")) +
    # annotation as title in the middle top
    annotate("text", x = -10.3, y = 45, 
             label = "Personalized \n Cartography",
             lineheight = 1,
             size = 12, color = text_colour, family = fonte,
             hjust = 0) +
    guides(fill = guide_legend(override.aes = list(color = "white"), nrow = 2, order = 1),
           color = guide_legend(order = 2, label.hjust = 1, title = NULL,
                                label.position = "right",
                                override.aes = list(color = "grey70",
                                                    linewidth = 1.75))) 
  output$Plot <- renderPlot({
    
    # Modify the cor column in municipios dataset
    municipios_modified <- municipios %>%
      mutate(cor = ifelse(cor == "Ningun" & name %in% input$concellos, "Visita", cor))
    
    plot +
      # municipios, con non en mapa en cor
      geom_sf(data = municipios_modified %>% filter(name %in% input$concellos),
              aes(fill = cor),
              color= alpha(text_colour,0)) +
      geom_sf_text_repel(data = municipios_modified %>% filter(cor %in% c("Pa", "Ma", "Ambos")),
                         aes(label = name),
                         family = fonte,
                         color = text_colour,
                         size = annotation_size,
                         min.segment.length = 0.1,
                         segment.color = NA,
                         segment.size = 0.1,
                         nudge_x = 0.4,
                         nudge_y = -0.7) +
      coord_sf(ylim = c(35.5,45.5),
               xlim = c(-10,4),
               clip = "on") })
}

# Run the application 
shinyApp(ui = ui, server = server)
