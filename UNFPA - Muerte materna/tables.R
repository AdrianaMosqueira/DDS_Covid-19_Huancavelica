library(kableExtra)
library(readxl)
library(magrittr)

table1 <- read_excel("data/intermediate/tablas.xlsx", sheet = 1)
table2 <- read_excel("data/intermediate/tablas.xlsx", sheet = 2)
table3 <- read_excel("data/intermediate/tablas.xlsx", sheet = 3)

kabtab1 <- kable(table1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                latex_options = "hold_position", html_font = "Roboto",
                font_size = 13) %>%
  collapse_rows(columns = 1, valign = "top")


kabtab2 <- kable(table2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                latex_options = "hold_position", html_font = "Roboto",
                font_size = 13)  %>%
  collapse_rows(columns = 1, valign = "top")


kabtab3 <- kable(table3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                latex_options = "hold_position", html_font = "Roboto",
                font_size = 13) %>%
  collapse_rows(columns = 2, valign = "top") %>%
  pack_rows("Estrategia 1: Afrontar los limitados ingresos asignados a programas de planificación familiar",
            1, 3, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Estrategia 2: Potenciar las sinergias entre entidades públicas y el rol del sector privado",
            4, 6, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Estrategia 3: Mejorar la eficiencia en la asignación y uso de los recursos de planificación familiar",
            7, 10, label_row_css = "background-color: #666; color: #fff;") %>%
  pack_rows("Estrategia 4. Incorporar innovaciones en las fuentes de financiamiento para la planificación familiar",
            11, 12, label_row_css = "background-color: #666; color: #fff;")





