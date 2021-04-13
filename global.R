pg_logo_url <- "http://www.parlgov.org/static/images/parlgov-logo.svg"

pg_logo_colors <- colorfindr::get_colors(pg_logo_url, 
                                         exclude_col = "white", 
                                         top_n = 5) %>% 
  pull(col_hex)

pg_color_1 <- pg_logo_colors %>% pluck(1) # "#474749"
pg_color_2 <- pg_logo_colors %>% pluck(2) # "#2076B6"
