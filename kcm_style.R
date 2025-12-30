library(sysfonts)
library(showtext)
library(scales)

sysfonts::font_add_google("inter", "inter")
showtext::showtext_auto()

kcm_style <- function(textsize = NULL) {
  
  font <- 'inter'
  
  if(is.null(textsize)) {
    font_size <- 16
    title_size <- 20
    subtitle_size <- 18
    legend_size <- 15
  } else if(textsize == 'markdown') {
    font_size <- 28
    title_size <- 36
    subtitle_size <- 32
    legend_size <- 27
  }
  
  list(ggplot2::theme(
         text = element_text(size = font_size, family = font),
         plot.title = ggplot2::element_text(size = title_size, family = font, face = "bold"),
         plot.subtitle = ggplot2::element_text(size = subtitle_size, family = font, color="#242424"),
         plot.caption = ggplot2::element_text(size = font_size, hjust = 0.02, vjust = 2, family = font, color="#585860"),
    
         plot.title.position = "plot",
         plot.caption.position = "plot",
         
         legend.position = 'top',
         legend.text.align = 0,
         legend.background = ggplot2::element_blank(),
         legend.title = ggplot2::element_blank(),
         legend.justification='left',
         legend.margin = margin(t=0, b=0, l=0, r=0),
         legend.key = ggplot2::element_blank(),
         legend.text = ggplot2::element_text(family = font, size=legend_size),
         
         axis.title = ggplot2::element_blank(),
         axis.text = ggplot2::element_text(size = font_size, family = font, color = "black"),
         axis.ticks = ggplot2::element_blank(),
         axis.line = element_line(linewidth = 0.6, linetype = 'solid', color="black"),
         
         panel.grid.minor = ggplot2::element_blank(),
         panel.grid.major.y = ggplot2::element_line(color = '#585860', size = 0.35, linetype = 2),
         panel.grid.major.x = ggplot2::element_blank(),
         
         panel.background = ggplot2::element_rect(fill = "white"),
         panel.border = element_blank(),
         
         panel.spacing.x = unit(1.5, "line"),
         
         strip.background = ggplot2::element_rect(fill = 'white'),
         strip.text = ggplot2::element_text(size = title_size, family = font, hjust = 0),
    
  ))
}


kcm_style_flip <- function(textsize = NuLL) {
  
  font <- 'inter'
  
  if(is.null(textsize)) {
    font_size <- 16
    title_size <- 20
    subtitle_size <- 18
    legend_size <- 15
  } else if(textsize == 'markdown') {
    font_size <- 28
    title_size <- 36
    subtitle_size <- 32
    legend_size <- 27
  }
  
  list(ggplot2::theme(
         text = element_text(size = font_size, family = font),
         plot.title = ggplot2::element_text(size = title_size, family = font, face = "bold"),
         plot.subtitle = ggplot2::element_text(size = subtitle_size, family = font, color="#242424"),
         plot.caption = ggplot2::element_text(size = font_size, hjust = 0.02, vjust = 2, family = font, color="#585860"),
         
         plot.title.position = "plot",
         plot.caption.position = "plot",
         
         legend.position = 'top',
         legend.text.align = 0,
         legend.background = ggplot2::element_blank(),
         legend.title = ggplot2::element_blank(),
         legend.justification='left',
         legend.margin = margin(t=0, b=0, l=0, r=0),
         legend.key = ggplot2::element_blank(),
         legend.text = ggplot2::element_text(family = font, size=legend_size),
         
         axis.title = ggplot2::element_blank(),
         axis.text = ggplot2::element_text(size = font_size, family = font, color = "black"),
         axis.ticks = ggplot2::element_blank(),
         axis.line = element_line(linewidth = 0.6, linetype = 'solid', color="black"),
         
         panel.grid.minor = ggplot2::element_blank(),
         panel.grid.major.x = ggplot2::element_line(color = '#585860', size = 0.35, linetype = 2),
         panel.grid.major.y = ggplot2::element_blank(),
         
         panel.background = ggplot2::element_rect(fill = "white"),
         panel.border = element_blank(),
         
         strip.background = ggplot2::element_rect(fill = 'white'),
         strip.text = ggplot2::element_text(size = 20, family = font, hjust = 0),
         
       ))
}

# Pre-Definied Color Schemes for Common Values
kcm_color_palette <- function() {
  
  kcm_custom_colors <- c(
    # Day Type
    "Weekday" = "#FDB71A","Saturday" = "#31859F","Sunday" = "#006633"
    
    # Period
    ,"AM" = "#31859F","MID" = "#FF7B21","PM" = "#390854", "XEV" = "#FDB71A"  , "XNT"=  "#006633", "OWL" = "#FFE089"
    
    # Productivity Period
    ,"Peak" = "#FDB71A", "Off-Peak" = "#006633", "Night" = "#390854"
    
    # Day Period
    
    , "AM" = "#FDB71A", "MID" = "#F57F29", "PM" = "#31859F", "XEV" = "#006633",
      "XNT" = "#390854"
    
    # Before/Post
    ,"Before" = "#31859F", "Post" = "#F57F29"
    
    # Inbound/Outbound
    ,"Inbound" = "#FDB71A", "Outbound" = "#31859F"
    ,"I" = "#FDB71A", "O" = "#31859F"
    
    
    # Service Family
    ,"Urban" = "#006848", "Suburban" = "#D67619", "Rural and DART" = "#4B2884", "DART/Shuttle" = "#4B2884", "ST" = "#264d5e", "Other" = "#784885", "Entire Network" = "#FDB71A")
  
  
  list(ggplot2::scale_color_manual(values = kcm_custom_colors),
       ggplot2::scale_fill_manual(values = kcm_custom_colors))
}

kcm_colors_list <- c ("#FDB71A",
                          "#FF7B21",
                           "#31859F",
                          "#006633",
                        "#390854",
                          "#FFE089")

kcm_colors_default <- function(){
  kcm_colors_list <- c ("#FDB71A",
                        "#FF7B21",
                        "#31859F",
                        "#006633",
                        "#390854",
                        "#FFE089")
  list(ggplot2::scale_color_manual(values = kcm_colors_list),
       ggplot2::scale_fill_manual(values = kcm_colors_list))
}
