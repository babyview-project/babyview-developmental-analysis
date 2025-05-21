library(extrafont)
# extrafont::font_import(prompt = FALSE)
theme_set(theme_bw() +
            theme(strip.background = element_blank(),
                  panel.grid = element_blank(),
                  text = element_text(family = "Source Sans 3")))
