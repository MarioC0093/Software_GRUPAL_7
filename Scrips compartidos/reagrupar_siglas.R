
library(stringi) # stri_trans_general

siglas2 <- siglas |> 
  mutate(siglas2=case_when(str_detect(str_to_lower(denominacion), "partido popular|populares|pp") ~ "PP",
                           # P.P-E.U. nos interesa como PP
                           # listas conjuntas con el Partido Popular en las elecciones municipales y autonómicas de 2007
                           # en las elecciones generales de 2011 se presentó en coalición con el Partido Popular
                           str_detect(str_to_lower(denominacion), "^part.*socialist|^socialist.*teruel") ~ "PSOE",
                           str_detect(str_to_lower(denominacion), "^ciudadanos-") ~ "C's",
                           str_detect(str_to_lower(denominacion), "vasco") ~ "PNV",
                           str_detect(str_to_lower(siglas), "bng") ~ "BNG",
                           str_detect(str_to_lower(siglas), "comprom") &
                             !str_detect(siglas,"^0-9")~ "COMPROMÍS", # para evitar COMPROMÍS 2
                           str_detect(str_to_lower(siglas), "ciu") ~ "CiU",
                           str_detect(str_to_upper(denominacion), "VERDES|PODEM|EZKER|COMUNISTA|ZQUIERDA REPUBLICANA
                                     |VERDS|IZQUIERDA UNIDA|EN MAREA|UNIDAD POPULAR|EQUO") & # importante que el | esté en esta línea y no al final de la anterior
                             !str_detect(str_to_upper(stri_trans_general(denominacion, id = "Latin-ASCII")), "MAS PAIS") ~ "UP",
                           str_detect(str_to_lower(siglas), "erc") ~ "ERC",
                           # FRONT PEL PAIS VALENCIA-ERC --> ERC
                           # https://www.vilaweb.cat/noticia/1142933/20000710/front-pais-valencia-fusionara-erc.pdf
                           str_detect(str_to_upper(denominacion), "EH|EUSKO|ARALAR") | 
                             str_detect(str_to_upper(siglas), "EH|EUSKO|ARALAR")~ "EH - BILDU",
                           str_detect(str_to_upper(stri_trans_general(denominacion, id = "Latin-ASCII")), "MAS PAIS") ~ "MÁS PAÍS",
                           str_detect(str_to_upper(denominacion), "VOX") ~ "VOX",
                           TRUE ~ "Otros"))