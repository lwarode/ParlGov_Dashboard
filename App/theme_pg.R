library(ggplot2)

# ParlGov ggplot2 theme ---------------------------------------------------

theme_pg <- function() {
  
  # font
  font <- "Helvetica"
  
  # base theme
  theme_linedraw() %+replace%
    
    theme(
      
      # plot grid line
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = alpha("grey40", 0.2)),
      
      # faceting title background
      strip.background = element_blank(),
      
      strip.text = element_text(color = "black"),
      
      # plot text properties
      plot.title = element_text(            
        family = font,            
        size = 18,                        
        hjust = 0.5,
        margin = margin(0, 0, 10, 0)),
    
      plot.subtitle = element_text(          
        family = font,            
        size = 14),               
      
      plot.caption = element_text(
        family = font,            
        size = 8,                 
        hjust = 1),               
      
      axis.title = element_text(            
        family = font,            
        size = 10),               
      
      axis.text.x = element_text(   
        family = font, 
        # angle = 45,
        size = 8), 
      
      axis.text.y = element_text(   
        family = font, 
        size = 8),  
      
      legend.position = "top"
      
    )
  
}



