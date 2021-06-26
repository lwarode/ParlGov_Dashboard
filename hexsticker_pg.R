library(hexSticker)
library(magick)
# library(grImport2)

pg_logo_url <- "http://www.parlgov.org/static/images/parlgov-logo.svg" 

pg_logo <- image_read_svg(pg_logo_url, width = 2000, height = 2000/2.55)
pg_logo
# pg_logo <- download.file(pg_logo_url, destfile = "pg_logo.svg")
# pg_logo <- system.file("pg_logo.svg")
# pg_logo <- system.file("SVG", "pg_logo.svg", package="grImport2")
# readPicture(pg_logo)

sticker(pg_logo, 
        s_x = 1,
        s_y = 1.2,
        s_height = 1.5,
        s_width = 1.5,
        package = "Dashboard", 
        p_size = 7.5,
        p_y = 0.6,
        p_color = "#2076B6",
        h_fill = "white",
        h_color = "#474749",
        # h_color = "#2076B6",
        filename = "pg_dashboard_sticker.png",
        dpi = 600)

