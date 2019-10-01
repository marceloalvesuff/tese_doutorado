# --------------------------- Script Coleta Facebook ---------------------------
# --------------------------- Marcelo Alves ---------------------------



######## pacote
library(Rfacebook)


token <- "token"

#gerar data.frame
posts <- data.frame()


# Rotina de extração
for (x in 1:nrow(paginas)) {
 
   try(posts <-
          rbind(
             posts,
             getPage(
                page = paginas$from_id[x],
                token = token,
                reactions =
                   F,
                since = "2013-03-01",
                until =  "2018-11-30",
                n = 10000
             ))

   print(paste(x, " Coleta = ", nrow(posts)))
   
   
   
}


# tratamento de data
posts$dia <- as.Date(gsub("T.*", "", posts$created_time))
posts$mes_dia <- format(posts$dia , format = "%y-%m")
posts$mes_dia <-
   as.Date(paste0(posts$mes_dia, "-01"), "%y-%m-%d")
posts$ano <- lubridate::year(posts$dia)





#### Carregar em SQLITE

library("sqldf")

# estabelecendo conexão

db <- dbConnect(SQLite(), dbname = "tese_doutorado.sqlite")

# fazer carga
dbWriteTable(
   conn = db,
   name = "posts",
   value = posts,
   row.names = FALSE,
   append = T
)
