## Tese Final ----------------------------------------------------------
  
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(formattable)
library(flextable)
library(reshape2)
library(scales)
library(ggrepel)
Sys.setlocale("LC_TIME", 'portuguese')
options(scipen = 999)


### dados
load(file = "Tese_Dados_consolidado_2019.Rdata")

crowdtangle$from_name[crowdtangle$from_name == "UOL Notícias"] <- "UOL"
posts$caption <- gsub("www1.folha.uol.com.br", "folha.uol", posts$caption)

# back
back_c <-crowdtangle
back <- posts


# mescla
posts <- left_join(posts, select(paginas, from_id, NATUREZA, modularity_class, esq_dir))
crowdtangle <- left_join(crowdtangle, select(paginas, NATUREZA, from_id, modularity_class, esq_dir))




# tema
tema <- theme(plot.title = element_text(colour="#2d2d2d",size=28, face = "bold", family = "Roboto Condensed"),
              plot.caption =  element_text(colour="grey20",size=10, face = "plain", family = "Roboto Condensed"),
              plot.subtitle = element_text(colour="grey20",size=20, face = "plain", family = "Roboto Condensed"),
              axis.text.y = element_text(colour="grey20",size=18),
              axis.text.x = element_text(colour="grey20",size=18),
              strip.text = element_text(size = 12, face = "bold", family = "Arial Narrow"))+
  theme(
    panel.grid.minor.x=element_blank(), 
   # panel.grid.major.x=element_blank(),
    panel.grid.minor.y=element_blank())

ideo.colors <- c("Centro" = "#656565", "Centro-Direita" = "#3A49BD", "Centro-Esquerda" ="#EC5469", "Direita" = "#A1E796", "Esquerda" = "#F2AD68")


posts$included <- posts$from_id %in% crowdtangle$from_id

## export


tmp <- paginas %>% select(from_name, NATUREZA, modularity_class, shares_soma_c, likes) %>%  arrange(-shares_soma_c)

write.csv2(tmp, "git/paginas_tese_marcelo_final.csv")
ft <- flextable(tmp)
ft <- autofit(ft)
print(ft, preview = "docx")

# Cpt1 --------------------------------------------------------------------

# Figura XX – Percentual de compartilhamentos de páginas posicionadas em cada segmento ideológico de rede por dia

p <- posts %>% 
  filter(mes_dia == "2013-06-01")  %>% # retirar ou nao imprensa 
  group_by(from_id) %>% 
  summarise(s = sum(shares_count, na.rm = T)) %>% 
  inner_join(select(paginas, from_id, from_name, modularity_class)) %>% 
  ungroup %>% 
  group_by(modularity_class) %>% 
  top_n(20, s)


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(mes_dia == "2013-06-01", from_id %in% p$from_id) %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


tmp <- posts %>% 
  filter(mes_dia == "2013-06-01",included == F, from_id %in% p$from_id)  %>% 
  group_by(from_id, dia) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  inner_join(select(p, from_id, modularity_class))

d <-  tmp %>%  
  group_by( dia) %>% 
  summarise(d_n = sum(n),
            d_shares = sum(shares, na.rm = T)) %>% 
  na.omit()


            
tmp <- tmp %>% 
  group_by(modularity_class, dia) %>% 
  summarise(n = n(),
            shares = sum(shares, na.rm = T)) %>% 
  inner_join(d) %>% 
  mutate(norm_dia = shares/d_shares)


tmp <- tmp %>% group_by(modularity_class) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
  mutate(label = paste(modularity_class, "total =", shares)) %>% select(-shares) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- modularity_class)


# grafico 2
names(ideo.colors) = unique(tmp$label)

tmp %>%
  ggplot( aes(x=dia, y=norm_dia)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line( aes(color=label, group = label), size=3.5 )+
  geom_point(aes(color=label), fill = "white", size = 2.5, shape = 21) +
  labs(x = "", y = "Compartilhamentos", title = "Alcance de esquerda e direita em junho de 2013", 
       subtitle = "Porcentagem de compartilhamentos diários \ndas 20 páginas com mais alcance de cada categoria", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle
       Nota: Gráfico considera apenas os compartilhamentos das 20 páginas de cada grupo. Normalizado pelo total de compartilhamentos dessas páginas no dia") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, norm_dia), aes(label=paste(percent(norm_dia), "dia =",
                                                                                                 prettyNum(shares, "."))), 
                           color = "black", size = 5,direction = "both" , nudge_y=.1,
                           alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 16) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  theme(legend.position = 'none') + scale_y_percent() +
  scale_color_manual(values=ideo.colors) 


                             
ggsave("figuras/cpt1_casos_2013_evo_share_ideob.png", width = 12, height = 8)



# Hashtags

### Preparação 
library(tidytext)


text_df <- data_frame(id = posts$id, text  = as.character(stringi::stri_enc_toutf8(posts$message)),
                      modularity_class =posts$modularity_class,
                      shares_count = posts$shares_count,
                      from_id = posts$from_id,
                      mes_dia = posts$mes_dia,
                      dia = posts$dia) %>% na.omit()




hash_tokens <- text_df %>%
  filter(mes_dia == "2013-06-01")  %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  anti_join(stop_words, by = "word")  %>%
  #anti_join(mystopwords, by = "word")  %>%
  filter(grepl("#", word)) %>%
  filter(str_count(word) > 2)

hash_count <- hash_tokens %>% 
  group_by(word) %>% 
  summarise(n = n(),
            pg = n_distinct(from_id),
            cluster = n_distinct(modularity_class), 
            shares = sum(shares_count, na.rm = T))

hash_count %>%
  top_n(15, n) %>% 
  ggplot(aes(reorder(word, n), n)) +
  geom_col(fill = "#314256", show.legend = FALSE, alpha = .9) +
  coord_flip()  +
  labs(x = "", y = "", title = "Hashtags mais utilizadas em Junho de 2013", 
       subtitle = "15 hastags publicadas na mensagem dos posts", 
       caption = "Dados extraídos da Facebook Graph API")+ 
  theme_ipsum_rc(grid="X") +
  geom_text(aes(label=prettyNum(n, ".")), hjust=0, nudge_y=7, size = 5, color = "#314256") + tema +
  expand_limits(y = 2050)

ggsave("figuras/cpt1_casos_2013_hash.png", width = 12, height = 8)


# Evolução no tempo


tmp <- hash_tokens %>% 
  filter(word %in% c("#passelivre", "#vemprarua", "#chupadilma", "#grevegeral", "#golpecomunista","#foradilma", "#pec37nao", "#primaverabrasileira", "#forapt")) %>% 
  group_by(word, dia) %>% 
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T)) 



tmp <- tmp %>% group_by(word) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
  mutate(label = paste(word, "total =", shares)) %>% select(-shares) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- word)


# grafico 2


tmp %>%
  ggplot( aes(x=dia, y=shares)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line(aes(group = label), color = "#4b82db", size=3.5 ) +
  geom_point(color = "#4b82db", fill = "white", size = 2.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  
  labs(x = "", y = "Compartilhamentos", title = "Evolução das hashtags mais compartilhadas em Junho de 2013", 
       subtitle = "Total de compartilhamentos por dia de posts mencionando as hashtags", 
       caption = "Dados extraídos pela Facebook Graph API") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, shares), aes(label=prettyNum(shares, ".")), 
                           color = "black", size = 5,direction = "y" , nudge_y=20000,
                           alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 4) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  theme(legend.position = 'none') + expand_limits(y = 150000)


ggsave("figuras/cpt1_casos_2013_hash_evo.png", width = 12, height = 8)

# Tabela XX – Sumário descritivo das páginas com maior alcance em Junho de 2013



tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(mes_dia == "2013-06-01") %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


  
  
tmp <- posts %>% 
  filter(mes_dia == "2013-06-01",included == F)  %>% 
  group_by(from_id) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  left_join(select(paginas, from_id,from_name)) %>% 
  mutate(Media = round(shares/n)) %>% 
  select(from_name, from_id, n, shares, Media) %>% arrange(-shares) %>% head(100)

ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares", "Media"),
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")



# Figura XX – Série temporal das páginas com maior compartilhamento em Junho de 2013

p <- paginas %>%  
  filter(from_name %in% c("Anonymous Brasil", "Movimento Brasil Consciente", "Estadão", "Folha de S.Paulo",
                          "Mídia Ninja", "FORA PT", "Folha Política", "TV Revolta", "Brasil 247"))

tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(mes_dia == "2013-06-01", from_id %in% p$from_id) %>% 
  group_by(from_id, dia) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


tmp <- posts %>% 
  filter(mes_dia == "2013-06-01",included == F, from_id %in% p$from_id)  %>% 
  group_by(from_id, dia) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  inner_join(select(p, from_id, from_name))


tmp <- tmp %>% group_by(from_name) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
  mutate(label = paste(from_name, "n =", shares)) %>% select(-shares) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- from_name)


tmp %>%
  ggplot( aes(x=dia, y=shares)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line(aes(group = label), color = "#4b82db", size=3.5 ) +
  geom_point(color = "#4b82db", fill = "white", size = 2.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  
  labs(x = "", y = "Compartilhamentos", title = "Páginas mais compartilhadas em Junho de 2013", 
       subtitle = "Total de compartilhamentos por dia", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, shares), aes(label=prettyNum(shares, ".")), 
                           color = "black", size = 5,direction = "y" , nudge_y=20000,
                           alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 4) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  theme(legend.position = 'none') + expand_limits(y = 150000)


ggsave("figuras/cpt1_pag_evo.png", width = 16, height = 10)


# links mais compartilhados

tmp <- posts %>% 
  filter(mes_dia == "2013-06-01", !grepl("facebook", link))  %>% 
  group_by(link) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% arrange(-shares) %>% na.omit %>% 
  head(20)


ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares",
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")

# Tabela XX – Estatísticas descritivas das 20 páginas mais compartilhadas em 2014

tmp <- crowdtangle %>% 
  filter(dia > "2014-07-01" & dia < "2014-12-01") %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()




tmp <- posts %>% 
  filter(dia > "2014-07-01" & dia < "2014-12-01",included == F)  %>% 
  group_by(from_id) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  left_join(select(paginas, from_id,from_name)) %>% 
  mutate(Media = round(shares/n)) %>% 
  select(from_name, from_id, n, shares, Media) %>% arrange(-shares) %>% head(100)

pt <- posts %>% 
  filter(!duplicated(from_id),dia > "2014-07-01" & dia < "2014-12-01") %>% 
  select(from_id, from_name)


ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares", "Media"),
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")

# Detalhamento Franciscini

tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(ano > 2012 & ano < 2016,
    from_name == "Delegado Francischini") %>% 
  group_by(from_name, mes_dia) %>% 
  summarise(shares = sum(Shares))


tmp %>% 
  ggplot(aes(x=mes_dia, y=shares, group=1)) +
  geom_line(color = "#4b82db", size = 2.5) +
  geom_point( fill = "white", size = 3, shape = 21)+                         # Plot stock price
  #tidyquant::geom_ma(ma_fun = SMA, n = 30)+   # Plot 50-day EVWMA+
  labs(x = "", y = "posts", title = "Francischini tem pico durante eleição", 
       subtitle = "Página volta a ter compartilhamentos somente no segundo semestre de 2015", 
       caption = "Dados extraídos pelo Crowdtangle") +
  ggrepel::geom_text_repel(data=top_n(tmp, 1, shares), aes(label=paste(str_to_title(format(mes_dia, "%B")), prettyNum(shares, "."))), 
                           color = "black", size = 6,direction = "x" , alpha = .9, nudge_x = 9.16, fontface = "italic",
                           family = "Roboto Condensed"
  )  +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  )


ggsave("figuras/cpt1_casos_2014_francischini.png", width = 16, height = 10)

# diario


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(ano == 2014,
         from_name %in% c("Delegado Francischini", "Aécio Neves"))


tmp %>% 
  ggplot(aes(x=dia, y=Shares, group=1)) +
  geom_line(color="grey", size=2, alpha=0.3) +
  #geom_point( fill = "white", size = 3, shape = 21)+                         # Plot stock price
  tidyquant::geom_ma(ma_fun = SMA, n = 30, size = 2, 
                     linetype = "solid", color = "#4b82db")+   # Plot 50-day EVWMA+
  labs(x = "", y = "posts", title = "Comparação de compartilhamentos por dia", 
       subtitle = "Padrões diferentes entre Aécio Neves e Francischini", 
       caption = "Dados extraídos pelo Crowdtangle \n Linha azul referente à média móvel de 30 dias") +
  ggrepel::geom_text_repel(data=top_n(tmp, 2, Shares), aes(label=paste(str_to_title(format(dia, "%d-%m")), prettyNum(Shares, "."))), 
                           color = "black", size = 6,direction = "x" , alpha = .9, nudge_x = 9.16, fontface = "italic",
                           family = "Roboto Condensed"
  )  +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  ) + facet_grid(~ from_name)

ggsave("figuras/cpt1_casos_2014_francischini_aecio.png", width = 16, height = 10)

# Figura XX – Páginas mais compartilhadas entre junho e novembro de 2014


p <- paginas %>%  
  filter(from_name %in% c("Alvaro Dias", "Delegado Francischini", "Aécio Neves", "Folha de S.Paulo",
                          "Mídia Ninja", "Dilma Rousseff", "Folha Política", "TV Revolta", "Brasil 247",
                          "Revoltados ON LINE", "MCC - Movimento Contra Corrupção", "Marco Feliciano"))

tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia > "2014-07-01" & dia < "2014-12-01", from_id %in% p$from_id) %>% 
  group_by(from_id, mes_dia) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


tmp <- posts %>% 
  filter(dia > "2014-07-01" & dia < "2014-12-01",included == F, from_id %in% p$from_id)  %>% 
  group_by(from_id, mes_dia) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  inner_join(select(p, from_id, from_name))


tmp <- tmp %>% group_by(from_name) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
  mutate(label = paste(from_name, "\n n =", shares)) %>% select(-shares) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- from_name)

ylab <- c(0, 2, 4, 6)

tmp %>%
  ggplot( aes(x=mes_dia, y=shares))  + geom_hline(yintercept = 0, color = "darkgrey")+
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line(aes(group = label), color = "#4b82db", size=3.5 ) +
  geom_point(color = "#4b82db", fill = "white", size = 2.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  
  labs(x = "", y = "Compartilhamentos", title = "Páginas mais compartilhadas na eleição de 2014", 
       subtitle = "Total de compartilhamentos por dia", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, shares), aes(label=prettyNum(shares, ".")), 
                           color = "black", size = 5,direction = "y" , nudge_y=400000,
                           alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 2) + tema +
  theme(legend.position = 'none')  +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  )


ggsave("figuras/cpt1_casos_2014_pag_evo.png", width = 16, height = 10)

# Figura XX – Compartilhamento de MBL e Mídia Ninja em 2015




tmp <- crowdtangle %>% 
  filter(from_name %in% c("MBL - Movimento Brasil Livre", "Mídia Ninja"), 
         ano == 2015) %>% 
  group_by(from_name, mes_dia) %>% 
  summarise(n = sum(Post.Count),
            shares = sum(Shares))

tmp %>% 
  ggplot(aes(x=mes_dia, y=shares, group=from_name, color = from_name)) +
  geom_line(size = 3.5) +
  geom_point( fill = "white", size = 4, shape = 21)+
  labs(x = "", y = "posts", title = "Comparação MBL e Mídia Ninja em 2015", 
       subtitle = "MBL foi mais compartilhado em todos os meses", 
       caption = "Dados extraídos pelo Crowdtangle") + 
  geom_label_repel(data = filter(tmp, mes_dia == "2015-12-01"),aes(label=paste(from_name, prettyNum(shares, big.mark=".")), 
                                                                   fill = from_name), color = "white",
                   size = 6,direction = "y" , alpha = .9,
                   fontface = "italic",
                           family = "Roboto Condensed")  +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema  + 
  scale_color_manual(values =c("#436685", "#BF2F24"))  + 
  scale_fill_manual(values =c("#436685", "#BF2F24")) +
  scale_x_date(date_breaks  = "2 month", labels=date_format("%b"))



ggsave("figuras/cpt1_casos_2015_mbl_ninja.png", width = 12, height = 10)


#Tabela XX – Ranking das 20 páginas mais compartilhadas na amostra coleta sobre 2016


tmp <- crowdtangle %>% 
  filter(ano == 2016, dia %in% dt$dia) %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()




tmp <- posts %>% 
  filter(ano == 2016, dia %in% dt$dia,included == F)  %>% 
  group_by(from_id) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  left_join(select(paginas, from_id,from_name)) %>% 
  mutate(Media = round(shares/n)) %>% 
  select(from_name, from_id, n, shares, Media) %>% arrange(-shares) %>% head(100)



ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares", "Media"),
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")


#Tabela XX – Ranking das 20 páginas mais compartilhadas na amostra coleta sobre 2017


tmp <- crowdtangle %>% 
  filter(ano == 2017, dia %in% dt$dia) %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()




tmp <- posts %>% 
  filter(ano == 2017, dia %in% dt$dia,included == F)  %>% 
  group_by(from_id) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  left_join(select(paginas, from_id,from_name)) %>% 
  mutate(Media = round(shares/n)) %>% 
  select(from_name, from_id, n, shares, Media) %>% arrange(-shares) %>% head(100)


ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares", "Media"),
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")


# Figura XX – Comparativo de compartilhamentos entre páginas de esquerda e de direita em 2017

tmp <- crowdtangle %>% 
  filter(ano == 2017, dia %in% dt$dia) %>% 
  group_by(mes_dia, esq_dir) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


tmp <- posts %>% 
  filter(ano == 2017, dia %in% dt$dia,included == F)  %>% 
  group_by(mes_dia, esq_dir) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  ungroup %>% 
  group_by(mes_dia,esq_dir) %>% 
  summarise(n = sum(n),
            shares = sum(shares, na.rm = T)) %>% 
  mutate(p = shares/sum(shares)) %>% 
  droplevels()

sem<- sd(tmp$p)/sqrt(length(tmp$p))

tmp %>% 
  ggplot(aes(x=mes_dia, y=p, group=esq_dir, color = esq_dir, fill = esq_dir)) +
  geom_line( size = 4)+
  #geom_ribbon(aes(ymin=p-2*sem, ymax=p+2*sem), alpha=0.2) +
  geom_point( size = 4.5, shape=21, fill="white")+
  labs(x = "", y = "", title = "Esquerda quase iguala alcance em 2017", 
       subtitle = "Porcentagem dos compartilhamentos por mês", 
       caption = "Dados extraídos pela Facebook Graph API") + scale_y_continuous(labels = percent_format(), limits=c(0,0.9)) + 
  geom_label_repel(data = filter(tmp, mes_dia == "2017-08-011"),aes(label=paste(esq_dir, percent(p)), fill = esq_dir), color = "white",
                   size = 6,direction = "y" , alpha = .9
  )   + 
  scale_color_manual(values =c("#436685", "#BF2F24"))  + 
  scale_fill_manual(values =c("#436685", "#BF2F24"))  +
  scale_x_date(date_breaks  = "1 month", labels=date_format("%B"))+
  theme_ipsum(grid = "Y") +
  tema+
  theme(legend.position = 'none')



ggsave("figuras/cpt1_casos_2017_ev_shares.png", width = 12, height = 8)



#Tabela XX – Ranking das 20 páginas mais compartilhadas na amostra coleta sobre 2018


tmp <- crowdtangle %>% 
  filter(ano == 2018, dia %in% dt$dia) %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()




tmp <- posts %>% 
  filter(ano == 2018, dia %in% dt$dia,included == F)  %>% 
  group_by(from_id) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  left_join(select(paginas, from_id,from_name)) %>% 
  mutate(Media = round(shares/n)) %>% 
  select(from_name, from_id, n, shares, Media) %>% arrange(-shares) %>% head(100)


ft <- flextable(head(tmp, 20))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares", "Media"),
  big.mark=".", digits = 0)
ft <- autofit(ft)
print(ft, preview = "docx")


# links mais compartilhados

tmp <- posts %>% 
  filter(ano == 2018,  !grepl("facebook", link))    %>% 
  group_by(link) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% arrange(-shares) %>% na.omit


ft <- flextable(head(tmp, 30))
ft <- colformat_num(
  x = ft, col_keys = c("n", "shares",
                       big.mark=".", digits = 0)
  ft <- autofit(ft)
  print(ft, preview = "docx")
  
  
  # Figura XX – Páginas mais compartilhadas em 2018
  

p <- paginas %>%  
    filter(from_name %in% c("Quebrando o Tabu", "Delegado Francischini", "Jair Messias Bolsonaro", "Movimento Do POVO Brasileiro",
                            "Mídia Ninja", "VEJA", "Fernando Haddad", "Brasil 247", "Brasil 247",
                            "O Antagonista", "MBL - Movimento Brasil Livre", "Folha Política", "Joice Hasselmann"))
  
tmp <- crowdtangle %>% 
    droplevels() %>% 
    filter(ano == 2018, from_id %in% p$from_id, dia %in% dt$dia) %>% 
    group_by(from_id, mes_dia) %>% 
    summarise(n = sum(Post.Count, na.rm = T),
              shares = sum(Shares, na.rm = T)) %>% 
    na.omit()
  
  
tmp <- posts %>% 
    filter(ano == 2018,included == F, from_id %in% p$from_id, dia %in% dt$dia)  %>% 
    group_by(from_id, mes_dia) %>% 
    summarise(n = n(),
              shares = sum(shares_count, na.rm = T)) %>% 
    na.omit()%>% 
    bind_rows(tmp) %>% 
    inner_join(select(p, from_id, from_name))
  
  
tmp <- tmp %>% group_by(from_name) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
    mutate(label = paste(from_name, "\n n =", shares)) %>% select(-shares) %>% 
    inner_join(tmp)
  
tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- from_name)
  
ylab <- c(0, 2, 4, 6, 8)
  
tmp %>%
    ggplot( aes(x=mes_dia, y=shares))  + geom_hline(yintercept = 0, color = "darkgrey")+
    geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
    geom_line(aes(group = label), color = "#4b82db", size=3.5 ) +
    geom_point(color = "#4b82db", fill = "white", size = 2.5, shape = 21) +
    #viridis::scale_color_viridis(discrete = TRUE) +
    
    labs(x = "", y = "Compartilhamentos", title = "Páginas mais compartilhadas na eleição de 2018", 
         subtitle = "Total de compartilhamentos por mês", 
         caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle") +
    ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, shares), aes(label=prettyNum(shares, ".")), 
                             color = "black", size = 5,direction = "y" , nudge_y=400000,
                             alpha = .9,  fontface = "italic", segment.alpha = .0,
                             family = "Roboto Condensed"
    )  +
    facet_wrap(~label) +
    theme_ipsum_rc(grid = "Y", base_size = 2) + tema +
    theme(legend.position = 'none')  +
    scale_y_continuous(labels = paste0(ylab, "M"),
                       breaks = 10^6 * ylab
    ) +
  scale_x_date(date_breaks  = "1 month", labels=date_format("%b"))
  
  
ggsave("figuras/cpt1_casos_2018_pag_evo.png", width = 16, height = 10)

### Evolução ideologia


p <- posts %>% 
  filter(ano == 2018, dia %in% dt$dia)  %>% # retirar ou nao imprensa 
  group_by(from_id) %>% 
  summarise(n = n(),
    s = sum(shares_count, na.rm = T)) %>% 
  inner_join(select(paginas, from_id, from_name, modularity_class)) %>% 
  ungroup %>% 
  group_by(modularity_class) %>% 
  top_n(20, s)


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(ano == 2018, from_id %in% p$from_id, dia %in% dt$dia) %>% 
  group_by(from_id) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()



tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(ano == 2018, from_id %in% p$from_id, dia %in% dt$dia) %>% 
  group_by(from_name) %>% 
  summarise(n = sum(Post.Count, na.rm = T),
            shares = sum(Shares, na.rm = T)) %>% 
  na.omit()


tmp <- posts %>% 
  filter(ano == 2018,included == F, from_id %in% p$from_id, dia %in% dt$dia)  %>% 
  group_by(from_id, mes_dia) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  inner_join(select(p, from_id, modularity_class))

d <-  tmp %>%  
  group_by(mes_dia) %>% 
  summarise(d_n = sum(n),
            d_shares = sum(shares, na.rm = T)) %>% 
  na.omit()



tmp <- tmp %>% 
  group_by(modularity_class, mes_dia) %>% 
  summarise(n = n(),
            shares = sum(shares, na.rm = T)) %>% 
  inner_join(d) %>% 
  mutate(norm_dia = shares/d_shares)


tmp <- tmp %>% group_by(modularity_class) %>% summarise(shares = prettyNum(sum(shares), ".")) %>% 
  mutate(label = paste(modularity_class, "total =", shares)) %>% select(-shares) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- modularity_class)


# grafico 2
names(ideo.colors) = unique(tmp$label)

tmp %>%
  ggplot( aes(x=mes_dia, y=norm_dia)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line( aes(color=label, group = label), size=3.5 )+
  geom_point(aes(color=label), fill = "white", size = 2.5, shape = 21) +
  labs(x = "", y = "Compartilhamentos", title = "Alcance dos grupos ideológicos na eleição de 2018", 
       subtitle = "Porcentagem de compartilhamentos diários \ndas 20 páginas com mais alcance de cada categoria", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle
       Nota: Gráfico considera apenas os compartilhamentos das 20 páginas de cada grupo. Normalizado pelo total de compartilhamentos dessas páginas no mês") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, norm_dia), aes(label=paste(percent(norm_dia), "mês =",
                                                                                                 prettyNum(shares, "."))), 
                           color = "black", size = 5,direction = "both" , nudge_y=.1,
                           alpha = .9,  fontface = "italic", segment.alpha = .3,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 16) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  theme(legend.position = 'none') + scale_y_percent() +
  scale_color_manual(values=ideo.colors)  +
  scale_x_date(date_breaks  = "1 month", labels=date_format("%b"))




ggsave("figuras/cpt1_casos_2018_evo_share_ideob.png", width = 12, height = 8)


# Cpt2 --------------------------------------------------------------------

# Tabela XX – Categorias dos atores

tmp <- paginas %>% 
  tabyl(NATUREZA, show_na = F) %>% 
  adorn_pct_formatting()

ft <- flextable(tmp)
ft <- autofit(ft)
print(ft, preview = "docx")

#data_summary <- function(data, varname, groupnames){
require(plyr)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

paginas$indice <- as.numeric(as.character(paginas$END_FISICO))*2 + as.numeric(as.character(paginas$CONTATO)) + as.numeric(as.character(paginas$QUEM_SOMOS)) + 
  as.numeric(as.character(paginas$ESTRUTURA_NOMES))*2 + as.numeric(as.character(paginas$WIKI))*2 + as.numeric(as.character(paginas$SITE))*2

df2 <- data_summary(paginas, varname="indice", 
                    groupnames=c("NATUREZA")) %>% na.omit()

ggplot(df2, aes(x=NATUREZA, y=indice, group=NATUREZA, color=NATUREZA)) + 
  geom_line() +
  geom_point()+
  geom_errorbar(aes(ymin=indice-sd, ymax=indice+sd), width=.2,
                position=position_dodge(0.05)) + coord_flip()

# Cpt 4 shares --------------------------------------------------------------



## Tabela XX – Publicações por posicionamento de rede


tmp <- paginas %>% 
  group_by(modularity_class)  %>% 
  summarise(n = sum(n_c, na.rm = T), 
            n_dis = n_distinct(from_id), 
            Média = mean(n_c, na.rm = T)) %>% 
  mutate(Percentual = percent(n/sum(n)))  %>% 
  arrange(-n) 
sum(tmp$n_dis)
# Tabela

ft <- flextable(tmp)
ft <- colformat_num(
  x = ft, col_keys = c("n", "n_dis", "Média"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")




## Tabela XX – Publicações por tipologia de atores

tmp <- paginas %>% 
  group_by(NATUREZA)  %>% 
  summarise(n = sum(n_c, na.rm = T), 
            n_dis = n_distinct(from_id), 
            Média = mean(n_c, na.rm = T)) %>% 
  mutate(Percentual = percent(n/sum(n)))  %>% 
  arrange(-n) 

sum(tmp$n_dis)

# Tabela

ft <- flextable(tmp)
ft <- colformat_num(
  x = ft, col_keys = c("n", "n_dis", "Média"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")


# Figura XX – Páginas que mais publicaram por posicionamento de rede


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia, NATUREZA != "Imprensa") %>% 
  group_by(from_name) %>% 
  summarise(n = sum(Post.Count, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F, NATUREZA != "Imprensa") %>% 
  droplevels() %>% 
  group_by(from_name) %>% 
  summarise(n = n())  %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  ungroup() %>%
  inner_join(select(paginas,  from_name, modularity_class)) %>%
  group_by(modularity_class) %>% 
  top_n(10, n)  %>%
  ungroup() %>%
  na.omit %>% 
  arrange(modularity_class,  n) %>%
  mutate(order = row_number())

# grafico

tmp %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  filter(modularity_class %in% c("Centro", "Direita", "Centro-Direita")) %>% 
  ggplot(aes(order, n)) + 
  geom_bar(stat = "identity",  color = "#314256", fill = "#314256",width = .7, alpha = .7) +
  theme_minimal() + coord_flip() +
  labs(x = "", y = "", title = "Páginas que mais postaram de acordo com posicionamento na rede", 
       subtitle = "10 fan-pages com maior total de posts na amostra", 
       caption = "Dados extraídos pela Facebook Graph API \n Complementação dos dados pelo Crowdtangle") +
  facet_wrap(~ modularity_class, ncol=2, scales = "free_y")  + 
  scale_x_continuous(
    breaks = tmp$order,
    labels = tmp$from_name,
    expand = c(0,0)
  ) + 
  theme_ipsum_rc(grid="X")+ tema+
  theme(strip.text.x =  element_text(size = 18, colour = "black", face = "bold")) +
  theme(axis.text.y = element_text(colour="grey20",size=18))


ggsave("figuras/cpt6_id_top1.png", width = 16, height = 10)

# grafico centro esquerda

tmp %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  filter(modularity_class %in% c("Esquerda", "Centro-Esquerda")) %>% 
  ggplot(aes(order, n)) + 
  geom_bar(stat = "identity",  color = "#314256", fill = "#314256",width = .7, alpha = .7) +
  theme_minimal() + coord_flip() +
  labs(x = "", y = "", title = "Páginas que mais postaram de acordo com posicionamento na rede", 
       subtitle = "10 fan-pages com maior total de posts na amostra", 
       caption = "Dados extraídos pela Facebook Graph API \n Complementação dos dados pelo Crowdtangle") +
  facet_wrap(~ modularity_class, ncol=2, scales = "free_y")  + 
  scale_x_continuous(
    breaks = tmp$order,
    labels = tmp$from_name,
    expand = c(0,0)
  ) + 
  theme_ipsum_rc(grid="X")+ tema+
  theme(strip.text.x =  element_text(size = 18, colour = "black", face = "bold")) +
  theme(axis.text.y = element_text(colour="grey20",size=18))


ggsave("figuras/cpt6_id_top1ce.png", width = 16, height = 10)

# grafico imprensa


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia, NATUREZA == "Imprensa") %>% 
  group_by(from_name) %>% 
  summarise(n = sum(Post.Count, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F, NATUREZA == "Imprensa") %>% 
  droplevels() %>% 
  group_by(from_name) %>% 
  summarise(n = n())  %>% 
  na.omit()%>% 
  bind_rows(tmp) %>% 
  ungroup() %>%
  inner_join(select(paginas,  from_name, NATUREZA)) %>%
  group_by(NATUREZA) %>% 
  top_n(10, n)  %>%
  ungroup() %>%
  na.omit %>% 
  arrange(NATUREZA,  n) %>%
  mutate(order = row_number())

# grafico

tmp %>%
  # 3. Add order column of row numbers
  mutate(order = row_number()) %>%
  ggplot(aes(order, n)) + 
  geom_bar(stat = "identity",  color = "#314256", fill = "#314256",width = .7, alpha = .7) +
  theme_minimal() + coord_flip() +
  labs(x = "", y = "", title = "Páginas que mais postaram da imprensa", 
       subtitle = "10 fan-pages com maior total de posts na amostra", 
       caption = "Dados extraídos pela Facebook Graph API \n Complementação dos dados pelo Crowdtangle") +
  facet_wrap(~ NATUREZA, ncol=2, scales = "free_y")  + 
  scale_x_continuous(
    breaks = tmp$order,
    labels = tmp$from_name,
    expand = c(0,0)
  ) +
  geom_text(aes(label= prettyNum(n, big.mark=".")), hjust=1.5,size = 6, color = "white",fontface = "bold") + 
  theme_ipsum_rc(grid="X")+ tema+
  theme(strip.text.x =  element_text(size = 18, colour = "black", face = "bold")) +
  theme(axis.text.y = element_text(colour="grey20",size=18))


ggsave("figuras/cpt6_id_top1imp.png", width = 16, height = 10)

# grafico
paginas %>% 
  filter(!is.na(NATUREZA)) %>% 
  ggplot(aes(x=reorder(NATUREZA, log(n_c), FUN = median, na.rm=T), y=log(n_c), alpha = .99)) + 
  geom_boxplot(outlier.colour = NULL, 
               fill ="#314256", colour="#314256") + geom_jitter(alpha = .1) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", 
               fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  labs(x = "", y = "Total de Publicações (log)", title = "Distribuição do total de publicações das páginas", 
       subtitle =  "Medidas de posição por tipo de atores", 
       caption =  "Logaritmo da soma de compartilhamento das páginas \n Fonte de dados: Graph API com complementação dos dados pelo Crowdtangle")  + 
  coord_flip() + theme_ipsum_rc(grid="X") +  tema  +
  theme(legend.position="none") 


ggsave("figuras/cpt6_posts_total_top.png", width = 12, height = 8)


# Tabela XX – Sumário descritivo do total de compartilhamentos por categoria


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() + tmp$n

t1 <- paginas %>%
  group_by(NATUREZA) %>%
  summarise(Mediana = round(median(shares_soma_c, na.rm = T),2),
            Total = sum(shares_soma_c, na.rm = T),
            Minimo = round(min(shares_soma_c, na.rm = T),2),
            Maximo = round(max(shares_soma_c, na.rm = T),2),
            Paginas = n_distinct(from_id))  %>%
  mutate(shares_percent = (Total/tmp$n) *100) %>% 
  rename(Categoria = NATUREZA) %>%
  arrange(-Mediana) %>% na.omit()


ft <- flextable(t1)
ft <- colformat_num(
  x = ft, col_keys = c("Mediana", "Total", "Minimo", "Maximo"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")


# Figura XX – Medidas de posição do total de compartilhamentos por categoria
max <- paginas %>%  filter(NATUREZA == "Imprensa")


# boxplot - todos
paginas %>% 
  filter(!is.na(NATUREZA)) %>% 
  ggplot(aes(x=reorder(NATUREZA, log(shares_soma_c), FUN = median, na.rm=T), y=log(shares_soma_c), alpha = .99)) + 
  geom_boxplot(outlier.colour = NULL, 
               fill ="#314256", colour="#314256") + geom_jitter(alpha = .1) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  labs(x = "", y = "Total de Compartilhamentos (log)", title = "Distribuição do total de compartilhamento das páginas", 
       subtitle =  "Medidas de posição por tipo de atores", 
       caption =  "Logaritmo da soma de compartilhamento das páginas \n Fonte de dados: Graph API com complementação dos dados pelo Crowdtangle")  + 
  coord_flip() + theme_ipsum_rc(grid="X") +  tema  +
  theme(legend.position="none") +
  geom_hline(yintercept=c(log(max(max$shares_soma_c))), linetype="dotted")+
  annotate("text", x = 5, y = 19, family = "Roboto Condensed", size = 5, color = "gray20",
           label = "Imprensa tradicional\n máximo de compartilhamento")+ 
  expand_limits(y = 21) +
  geom_curve(aes(x = 4.8, y = 16.7, xend = 5, yend = 18),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "gray20", curvature = -0.3)


ggsave("figuras/cpt6_shares_total.png", width = 12, height = 8)



# Tabela XX – Sumário descritivo do total de compartilhamentos por categoria das 50 páginas mais compartilhadas por categoria

# tabela - top 50
p <- paginas %>% filter(!is.na(NATUREZA)) %>%   group_by(NATUREZA) %>% top_n(50, shares_soma_c) 

tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() + tmp$n

t1 <- p %>%
  group_by(NATUREZA) %>%
  summarise(Mediana = round(median(shares_soma_c, na.rm = T),2),
            Total = sum(shares_soma_c, na.rm = T),
            Minimo = round(min(shares_soma_c, na.rm = T),2),
            Maximo = round(max(shares_soma_c, na.rm = T),2),
            Paginas = n_distinct(from_id))  %>%
  mutate(shares_percent = (Total/tmp$n) *100) %>% 
  rename(Categoria = NATUREZA) %>%
  arrange(-Total)


ft <- flextable(t1)
ft <- colformat_num(
  x = ft, col_keys = c("Mediana", "Total", "Minimo", "Maximo"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")

# grafico
p %>% 
  filter(!is.na(NATUREZA)) %>% 
  ggplot(aes(x=reorder(NATUREZA, log(shares_soma_c), FUN = median, na.rm=T), y=log(shares_soma_c), alpha = .99)) + 
  geom_boxplot(outlier.colour = NULL, 
               fill ="#314256", colour="#314256") + geom_jitter() +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  labs(x = "", y = "Total de Compartilhamentos (log)", title = "Distribuição do total de compartilhamento das páginas", 
       subtitle =  "Medidas de posição por tipo de atores - Top 50", 
       caption =  "Logaritmo da soma de compartilhamento das páginas \n Fonte de dados: Graph API com complementação dos dados pelo Crowdtangle")  + 
  coord_flip() + theme_ipsum_rc(grid="X") +  tema  +
  theme(legend.position="none") 


ggsave("figuras/cpt6_shares_total_top.png", width = 12, height = 8)


# tipo 2

g <- ggplot(p, aes(x=reorder(NATUREZA, log(shares_media_c), FUN = median, na.rm=T), y=log(shares_media_c), color = NATUREZA)) +
  coord_flip() +
  ggsci::scale_color_uchicago() + theme_ipsum_rc(grid = "none")+ tema

tmp <- p %>%  group_by(NATUREZA) %>%  summarise(md = median( log(shares_media_c)))


g + 
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = median, geom = "point", size = 5)+
  geom_hline(aes(yintercept = median(log(p$shares_media_c))), color = "gray70", size = 0.6) +
  geom_segment(data = tmp, aes(x = NATUREZA, xend = NATUREZA,
                   y = median(log(p$shares_media_c)), yend = md),
               size = 0.8)+
  labs(x = "", y = "Média de Compartilhamentos (log)", title = "Distribuição da média de compartilhamento das páginas", 
       subtitle =  "Medidas de posição por tipo de atores - Top 50", 
       caption =  "Logaritmo da soma de compartilhamento das páginas \n Fonte de dados: Graph API com complementação dos dados pelo Crowdtangle")  +
  theme(legend.position="none") +
  annotate("text", x = 5.7, y = median(log(p$shares_media_c)-.82), family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Mediana de compartilhamentos:\n{round(median( log(p$shares_media_c)), 1)}"))+
  annotate("text", x = 5.5, y = 8, family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Políticos com maior mediana:\n{round(max(tmp$md), 2)}")) +
  annotate("text", x = 1.5, y = 4, family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Imprensa tem a menor mediana:\n{round(min(tmp$md), 2)}")) + expand_limits(x = 6)


ggsave("figuras/cpt6_shares_media_top.png", width = 12, height = 8)



# Regressão 
paginas$log_Shares_geral_soma <- log(paginas$shares_soma_c +1)

#relevel
paginas$NATUREZA <- as.factor(paginas$NATUREZA)
paginas <- paginas %>%
  mutate(NATUREZA = relevel(NATUREZA, ref = "Imprensa"))

m1<- lm(log_Shares_geral_soma ~ log(likes +1), data = paginas)
mb<- lm(log_Shares_geral_soma ~ NATUREZA, data = paginas)
m2<- lm(log_Shares_geral_soma ~ NATUREZA + log(likes +1), data = paginas)
m3<- lm(log_Shares_geral_soma ~ NATUREZA + log(likes +1) + log(n_c+1), data = paginas)
        
        
        
        
stargazer::stargazer(m1, mb, m2,m3, type = "html", 
                             out = "figuras/regressao.html", title = "Regressão Múltipla", dep.var.caption = "Variável dependente",
                             dep.var.labels = "Compartilhamentos (log)",
                             covariate.labels=c("Seguidores (log)", 
                                                "posts (log)",
                                                "Criadores Digitais", 
                                                "Mídia Alternativa", 
                                                "Políticos/partidos", "Sociedade Civil"))

# Evolução imprensa


p_ano <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(ano) %>% 
  summarise(n = sum(Post.Count, na.rm = T)) %>% 
  na.omit()
  
p_ano <- posts %>% 
  filter(included == F) %>% 
  droplevels() %>% 
  group_by(ano) %>% 
  summarise(n = n()) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(p_ano) %>% 
  group_by(ano) %>% 
  summarise(total = sum(n, na.rm = T)) %>% 
  na.omit()

# total


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(NATUREZA, ano) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F) %>% 
  droplevels() %>% 
  group_by(NATUREZA, ano) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(tmp) %>% 
  group_by(NATUREZA, ano) %>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  na.omit() %>% 
  inner_join(p_ano) %>% 
  mutate(m = n/total)

tmp %>% 
  ggplot(aes(x=ano, y=m, group=NATUREZA)) +
  geom_line(aes(color = NATUREZA), size = 1.2)+
  geom_point(aes(color = NATUREZA), size = 3)+
  labs(x = "", y = "", title = "Média de compartilhamentos por ano", 
       subtitle = "Média do compartilhamento das fan-pages de acordo com a partição de modularidade", 
       caption = "Dados extraídos pela Facebook Graph API")  +
  ggsci::scale_color_uchicago() + theme_ipsum_rc(grid = "Y")+ tema+
  ggrepel::geom_text_repel(data=top_n(tmp, 1, ano), aes(label=paste(NATUREZA, round(m))), 
                           color = "black", size = 6,direction = "x" , alpha = .9, nudge_x = .5, fontface = "italic",
                           family = "Roboto Condensed"
  ) + expand_limits(x = 8)+ geom_hline(yintercept = 0, color = "darkgrey")+
  theme(legend.position = 'none')


ggsave("figuras/cpt5_shares_media_cat.png", width = 12, height = 8)




# Evolução imprensa - top 20

p <- paginas %>%  group_by(NATUREZA) %>% top_n(20, shares_soma_c) 

p_ano <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(ano) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

p_ano <- posts %>% 
  filter(included == F) %>% 
  droplevels() %>% 
  group_by(ano) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(p_ano) %>% 
  group_by(ano) %>% 
  summarise(total = sum(n, na.rm = T)) %>% 
  na.omit()

# total


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia, from_id %in% p$from_id) %>% 
  group_by(NATUREZA, ano) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F, from_id %in% p$from_id) %>% 
  droplevels() %>% 
  group_by(NATUREZA, ano) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(tmp) %>% 
  group_by(NATUREZA, ano)%>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  na.omit() %>% 
  inner_join(p_ano) %>% 
  mutate(p = n/total)



# Top 30

tmp %>% 
  ggplot(aes(x=ano, y=p, group = NATUREZA, color = NATUREZA)) +
  geom_line( size = 4) +
  geom_point( fill = "white", size = 4.5, shape = 21) +
  facet_wrap(~ NATUREZA) +
  labs(x = "", y = "Compartilhamentos", title = "Comparação de compartilhamento", 
       subtitle = "Porcentagem de compartilhamentos anuais \ndas 20 páginas com mais alcance de cada categoria", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle")+
  ggrepel::geom_text_repel(data=tmp, aes(label=paste(percent(p))), 
                           color = "black", size = 5,direction = "y" , alpha = .9, nudge_x = -.66, fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 16) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  ggsci::scale_color_uchicago()  +
  theme(legend.position = 'none') + scale_y_percent()

# grafico 2


tmp <- tmp %>% group_by(NATUREZA) %>% summarise(n = prettyNum(sum(n), ".")) %>% 
  mutate(label = paste(NATUREZA, "\n total =", n)) %>% select(-n) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- NATUREZA)


tmp %>%
  ggplot( aes(x=ano, y=p)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=2, alpha=0.3) +
  geom_line( aes(color=label, group = label), color="#4b82db", size=3.2 )+
  geom_point(color="#4b82db", fill = "white", size = 4.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "Compartilhamentos", title = "Comparação de compartilhamento", 
       subtitle = "Porcentagem de compartilhamentos anuais \ndas 20 páginas com mais alcance de cada categoria", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle")+
  facet_wrap(~label) +
  ggrepel::geom_text_repel(data=tmp, aes(label=paste(percent(p))), 
                           color = "black", size = 5,direction = "y" , alpha = .9, nudge_x = -.66, fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 16) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  ggsci::scale_color_uchicago()  +
  theme(legend.position = 'none') + scale_y_percent()


ggsave("figuras/cpt5_cat_evo_2.png", width = 12, height = 8)


# A imprensa cai?

tmp <- crowdtangle %>% 
  filter(NATUREZA == "Imprensa", ano > 2012 & ano < 2019) %>% 
  group_by(ano) %>% 
  summarise(n = sum(Post.Count), 
            shares = sum(Shares)) %>% 
  mutate(media = round(shares/n, 2)) 

tmp %>% 
  ggplot(aes(x=ano, y=media, group=1)) +
  geom_line(color = "#4b82db",size = 4)+
  geom_point(color="#4b82db", fill = "white", size = 4.5, shape = 21)+
  ggrepel::geom_text_repel(data=tmp, aes(label=paste(media)), 
                           color = "black", size = 6,direction = "y", 
                           nudge_y = 12, alpha = .9, 
                           fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  ) +
  labs(x = "", y = "Média de Compartilhamentos", title = "News feed desfavorece a imprensa?", 
       subtitle = "Média de compartilhamentos da imprensa cai depois de 2015", 
       caption = "Dados extraídos pelo Crowdtangle")+
  
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema 



ggsave("figuras/cpt5_imp_evo_.png", width = 13, height = 8)

# imprensa perde seguidores?


crowdtangle %>% 
  filter(from_name == c("O Globo", "VEJA", "Estadão", "UOL Notícias"), dia > "2016-01-01", dia < "2019-01-01") %>% 
  ggplot(aes(x=dia, y=Page.Likes, group=1)) +
  geom_line(color = "#4b82db",size = 4)+
  labs(x = "", y = "Seguidores", title = "Seguidores crescem, média cai", 
       subtitle = "Indício do efeito de controle algorítimico pelo Facebook", 
       caption = "Dados extraídos pelo Crowdtangle")+
  
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema + facet_grid(~ from_name)



ggsave("figuras/cpt5_impseg_evo_.png", width = 13, height = 8)

# Comparação paginas

# facetado


tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia, from_name %in% c("Delegado Francischini",
                                           "Quebrando o Tabu",
                                           "MBL - Movimento Brasil Livre",
                                           "VEJA",
                                           "MCC - Movimento Contra Corrupção",
                                           "Jair Messias Bolsonaro",
                                           "Brasil 247",
                                           "Mídia Ninja", 
                                           "O Antagonista")) %>% 
  group_by(from_name, ano) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F, from_name %in% c("Delegado Francischini",
                                         "Quebrando o Tabu",
                                         "MBL - Movimento Brasil Livre",
                                         "VEJA",
                                         "MCC - Movimento Contra Corrupção",
                                         "Jair Messias Bolsonaro",
                                         "Brasil 247",
                                         "Mídia Ninja", 
                                         "O Antagonista")) %>% 
  droplevels() %>% 
  group_by(from_name, ano) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(tmp) %>% 
  group_by(from_name, ano)%>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  na.omit() %>% 
  inner_join(p_ano) %>% 
  mutate(p = n/total)



tmp <- tmp %>% group_by(from_name) %>% summarise(n = prettyNum(sum(n), ".")) %>% 
  mutate(label = paste(from_name, "\nTotal =", n)) %>% select(-n) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- from_name)


tmp %>%
  ggplot( aes(x=ano, y=p)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=2, alpha=0.3) +
  geom_line( aes(color=label, group = label), color="#4b82db", size=3.2 )+
  geom_point(color="#4b82db", fill = "white", size = 4.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "Compartilhamentos", title = "Comparação de compartilhamento das páginas", 
       subtitle = "Porcentagem de compartilhamentos anuais", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle")+
  facet_wrap(~label) +
  ggrepel::geom_text_repel(data=tmp, aes(label=paste(percent(p))), 
                           color = "black", size = 5,direction = "y" , alpha = .9,
                           nudge_x = -.0, fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 2) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  ggsci::scale_color_uchicago()  +
  theme(legend.position = 'none') + scale_y_percent()


ggsave("figuras/cpt5_pag_evo_1.png", width = 12, height = 8)

# grafico


tmp <- crowdtangle %>% 
  filter(dia %in% dt$dia, from_name %in% c("UOL Notícias", "MBL - Movimento Brasil Livre")) %>% 
  group_by(from_name, ano) %>% 
  summarise(n = sum(Post.Count),
            shares = sum(Shares))

ylab <- c(0, 2, 4, 6, 8)

tmp %>% 
  ggplot(aes(x=ano, y=shares, group=from_name, color = from_name)) +
  geom_line(size = 4) +
  geom_point( fill = "white", size = 4.5, shape = 21)+
  labs(x = "", y = "posts", title = "Comparação de compartilhamento", 
       subtitle = "MBL passa a ter mais alcance que o UOl Notícias depois de 2016", 
       caption = "Dados extraídos pela Facebook Graph API \n Complementação dos dados pelo Crowdtangle")+
  ggrepel::geom_text_repel(data=top_n(tmp, 1, shares), aes(label=paste(from_name, 
                                                                       round(shares/1000000, 0), "milhões")), 
                           color = "black", size = 5,direction = "x" , alpha = .9, nudge_x = -.66, fontface = "italic",
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  ggsci::scale_color_uchicago()  +
  expand_limits(x = c(0, NA), y = c(0, NA))  +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  )

ggsave("figuras/cpt5_tuol_mbl.png", width = 12, height = 8)


# Tabela top 30

tmp <- paginas %>% 
  select(from_name, n_c,  shares_soma_c, shares_media_c) %>% 
  arrange(-shares_soma_c) %>% 
  mutate(shares_media_c = round(shares_media_c, 2),
         Porcentagem = percent(shares_soma_c/sum(shares_soma_c))) %>% 
  head(30)



ft <- flextable(tmp)
ft <- colformat_num(
  x = ft, col_keys = c("n_c", "shares_soma_c", "shares_media_c"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")

# desenho curtidas

crowdtangle %>% 
  filter(from_name == "Rachel Sheherazade", dia > "2016-06-01", dia < "2019-01-01") %>% 
  ggplot(aes(x=dia, y=Page.Likes, group=1)) +
  geom_line(color = "#4b82db",size = 4)+
  labs(x = "", y = "Seguidores", title = "Rachel Sheherazade perde seguidores depois de 2016", 
       subtitle = "Movimentos bruscos sugerem coordenação na rede para descurtir a página", 
       caption = "Dados extraídos pelo Crowdtangle")+

  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema 

ggsave("figuras/cpt5_sheherazade.png", width = 12, height = 8)

# Direita tabela



# tabela - top 50
p <- paginas %>%  group_by(modularity_class) %>% top_n(50, shares_soma_c) 

tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() + tmp$n

t1 <- p %>%
  group_by(modularity_class) %>%
  summarise(posts = sum(n_c, na.rm = T), 
                Total = sum(shares_soma_c, na.rm = T),
            Minimo = round(min(shares_soma_c, na.rm = T),2),
            Maximo = round(max(shares_soma_c, na.rm = T),2),
            Paginas = n_distinct(from_id))  %>%
  mutate(Média = round(Total/posts), 
    shares_percent = percent(Total/tmp$n)) %>% 
  rename(Posicionamento = modularity_class) %>%
  arrange(-Total)


ft <- flextable(t1)
ft <- colformat_num(
  x = ft, col_keys = c("posts","Média", "Total", "Minimo", "Maximo"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")

# Direita Evolução




tmp <- crowdtangle %>% 
  droplevels() %>% 
  filter(dia %in% dt$dia, from_id %in% p$from_id) %>% 
  group_by(modularity_class, ano) %>% 
  summarise(n = sum(Shares, na.rm = T)) %>% 
  na.omit()

tmp <- posts %>% 
  filter(included == F, from_id %in% p$from_id) %>% 
  droplevels() %>% 
  group_by(modularity_class, ano) %>% 
  summarise(n = sum(shares_count, na.rm = T)) %>% 
  na.omit() %>% 
  ungroup%>% 
  bind_rows(tmp) %>% 
  group_by(modularity_class, ano)%>% 
  summarise(n = sum(n, na.rm = T)) %>% 
  na.omit() %>% 
  inner_join(p_ano) %>% 
  mutate(p = n/total)


# grafico

tmp <- tmp %>% group_by(modularity_class) %>% summarise(n = prettyNum(sum(n), ".")) %>% 
  mutate(label = paste(modularity_class, "\nTotal =", n)) %>% select(-n) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- modularity_class)
names(ideo.colors) = unique(tmp$label)

tmp %>%
  ggplot( aes(x=ano, y=p)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=2, alpha=0.3) +
  geom_line( aes(color=label, group = label), size=3.2 )+
  geom_point(aes(color=label), fill = "white", size = 4.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  labs(x = "", y = "Compartilhamentos", title = "Comparação de compartilhamento", 
       subtitle = "Porcentagem de compartilhamentos anuais \ndas 50 páginas com mais alcance de cada categoria", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle")+
  facet_wrap(~label) +
  ggrepel::geom_text_repel(data=tmp, aes(label=paste(percent(p))), 
                           color = "black", size = 5,direction = "y" , alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )   +
  theme_ipsum_rc(grid = "Y", base_size = 16) + tema + geom_hline(yintercept = 0, color = "darkgrey") +
  theme(legend.position = 'none') + scale_y_percent() +
  scale_color_manual(values=ideo.colors) 


ggsave("figuras/cpt5_mod_evo.png", width = 12, height = 8)


# dispersao

g <- ggplot(p, aes(x=reorder(modularity_class, log(shares_soma_c), FUN = median, na.rm=T), y=log(shares_soma_c), color = modularity_class)) +
  coord_flip() +
  scale_color_manual(values=ideo.colors)  + theme_ipsum_rc(grid = "none")+ tema

tmp <- p %>%  group_by(modularity_class) %>%  summarise(md = median( log(shares_soma_c)))


g + 
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun.y = median, geom = "point", size = 5)+
  geom_hline(aes(yintercept = median(log(p$shares_soma_c))), color = "gray70", size = 0.6) +
  geom_segment(data = tmp, aes(x = modularity_class, xend = modularity_class,
                               y = median(log(p$shares_soma_c)), yend = md),
               size = 0.8)+
  labs(x = "", y = "Soma de Compartilhamentos (log)", title = "Distribuição do total de compartilhamento das páginas", 
       subtitle =  "Medidas de posição das 50 páginas com mais alcance de cada grupo", 
       caption =  "Logaritmo da soma de compartilhamento das páginas \n Fonte de dados: Graph API com complementação dos dados pelo Crowdtangle")  +
  theme(legend.position="none") +
  annotate("text", x = 5.7, y = median(log(p$shares_soma_c)-.82), family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Mediana de compartilhamentos:\n{round(median( log(p$shares_soma_c)), 1)}"))+
  annotate("text", x = 5.5, y = 15, family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Direita tem maior mediana:\n{round(max(tmp$md), 2)}")) +
  annotate("text", x = 1.5, y = 12, family = "Roboto Condensed", size = 4, color = "gray20",
           label = glue::glue("Centro-Direita tem a menor mediana:\n{round(min(tmp$md), 2)}")) + expand_limits(x = 6)


ggsave("figuras/cpt6_shares_soma_ideo_top.png", width = 12, height = 8)


# Tabela imprensa, shares

t1 <- paginas %>% 
  select(from_name, FSP, shares_soma_c) %>% 
  arrange(-FSP) %>% head(10)



ft <- flextable(t1)
ft <- colformat_num(
  x = ft, col_keys = c("FSP","shares_soma_c"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")


# comparacao paginas


tmp <- crowdtangle %>% 
  filter(from_name %in% c("TV Revolta", "O Globo"), 
         mes_dia %in% as.Date(c("2014-07-01", "2014-08-01", "2014-09-01", "2014-10-01")), 
         dia > "2014-07-14") %>% 
  group_by(from_name, dia) %>% 
  summarise(n = sum(Post.Count),
            shares = sum(Shares)) 

# Média móvel


tmp %>% 
  ggplot(aes(x=dia, y=n, group=1)) +
  geom_line(color = "grey", alpha = .3, size = 1) +                         # Plot stock price
  tidyquant::geom_ma(ma_fun = SMA, n = 10)+   # Plot 50-day EVWMA
  scale_x_date(limits = c(min(tmp$dia + 30), max(tmp$dia))) + theme_minimal()
# }




# Cpt 4 links -------------------------------------------------------------

# Tabela XX – Tipos de publicações

t1 <- posts %>% 
  group_by(type) %>% 
  summarise(n = n()) %>% 
  filter(n > 10) %>% 
  mutate(percent = percent(n / sum(n))) %>% 
  arrange(-n)


ft <- flextable(t1)
ft <- colformat_num(
  x = ft, col_keys = c("n"),
  big.mark=".", digits = 0, na_str = "N/A")
ft <- autofit(ft)
print(ft, preview = "docx")

# Tabela XX – Tipos de postagem por categorização de atores

tmp <- posts %>%
  tabyl(NATUREZA, type, show_na = F) %>%
  adorn_crosstab(denom = "row") %>% select(NATUREZA, link, status, video, photo) 
ft <- autofit(flextable(tmp))
print(ft, preview = "docx")


# Figura XX – Compartilhamento de tipos de links no Facebook

# grafico
tmp <- posts %>% 
  filter(!grepl("facebook", link)) %>% 
  group_by(caption) %>% 
  summarise(shares =sum(shares_count, na.rm = T), 
            n = n(),
            n_ds = n_distinct(from_id),
            n_cs = n_distinct(modularity_class))  %>%
  mutate(percent_p = round((shares / sum(shares)) * 100,2),
         media = shares/n)%>% 
  na.omit %>% 
  arrange(-shares) %>%
  #head(100) %>%
  select(caption, shares, n, n_ds, percent_p, n_cs, media) 

# tipo

f <- posts %>% 
  filter(NATUREZA == "Imprensa") %>% 
  group_by(caption) %>% 
  summarise(n = n()) %>% 
  filter(n > 80 & caption != "youtube.com")

tmp$tipo <-  tmp$caption %in% f$caption
tmp$tipo <- ifelse(tmp$tipo == "TRUE", "Imprensa", "Outro")

#teste t
with(subset(tmp, tipo %in% c("Imprensa", "Outro")),
     t.test(shares ~ factor(tipo)))

# geral 
ggplot(tmp, aes(x=reorder(tipo, log(shares+1), FUN = median, na.rm=T), y=log(shares+1), alpha = .99)) + 
  geom_boxplot(outlier.colour = NULL, 
               fill ="#314256", colour="#314256") + 
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  labs(x = "", y = "Total de Compartilhamentos (log)", title = "Tipos de links mais compartilhados", 
       subtitle =  "Fontes da imprensa são muito mais compartilhadas que outros", 
       caption =  "Fonte: Facebook Graph API")  + 
  coord_flip() + theme_ipsum_rc(grid = "none") +  tema  +
  theme(legend.position="none") 


ggsave("figuras/cpt4_links_tipo.png", width = 12, height = 8)


# top 50


p <- tmp %>%  group_by(tipo) %>% top_n(50, shares) 

with(subset(p, tipo %in% c("Imprensa", "Outro")),
     t.test(shares ~ factor(tipo)))

ggplot(p, aes(x=reorder(tipo, log(shares+1), FUN = median, na.rm=T), y=log(shares+1), alpha = .99)) + 
  geom_boxplot(outlier.colour = NULL, 
               fill ="#314256", colour="#314256") + 
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) }) +
  labs(x = "", y = "Total de Compartilhamentos (log)", title = "Tipos de links mais compartilhados", 
       subtitle =  "Não há diferença signifiativa entre os 50 principais links de cada grupo", 
       caption =  "Fonte: Facebook Graph API")  + 
  coord_flip() + theme_ipsum_rc(grid = "none") +  tema  +
  theme(legend.position="none") 



ggsave("figuras/cpt4_links_tipo_top50.png", width = 12, height = 8)


# Tabela XX – Sumário descritivo da visibilidade de links



tmp <- posts %>% 
  filter(!grepl("facebook", link), dia %in% dt$dia) %>% 
  group_by(caption) %>% 
  summarise(n = n(),
    sum = sum(shares_count, na.rm = T))  %>%
  arrange(-sum) %>%
  #select(caption,n,  sum) %>% 
  mutate(percent = paste(round((sum / sum(sum)) * 100,2), "%"),
         media = round(sum/n, 2))%>% 
  na.omit %>% 
  head(30) 
  

ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n","media", "sum"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")

# Comparação Veja e Folha Política

tmp <- posts %>% 
  filter(caption %in% c("veja.abril.com.br", "folhapolitica.org"), dia %in% dt$dia) %>% 
  group_by(caption, ano) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  mutate(ano = as.Date(paste0(ano, "-01-01")))

tmp %>% 
  ggplot(aes(x=ano, y=shares, group=caption, color = caption)) +
  geom_line(size = 4) +
  geom_point( fill = "white", size = 4.5, shape = 21)+
  labs(x = "", y = "Compartilhamentos", title = "Fontes de desinformação cresceram durante impeachment", 
       subtitle = "Série temporal do total de compartilhamentos da Folha Política e da Veja", 
       caption = "Dados extraídos pela Facebook Graph API") +
  ggrepel::geom_text_repel(data=top_n(tmp, 1, shares), aes(label=paste(caption, 
                                                                       prettyNum(shares, "."))), 
                           color = "black", size = 6,direction = "x" , alpha = .9, nudge_x = 9.16, fontface = "italic",
                           family = "Roboto Condensed"
  )  +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema +
  scale_color_manual(values = c("indianred", "skyblue"),
                     name = NULL) +
  scale_fill_manual(values = c("indianred",  "skyblue"),
                    name = NULL)  +
  geom_hline(yintercept = 0, color = "darkgrey") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggsci::scale_color_uchicago()



ggsave("figuras/cpt4_links_fp_veja.png", width = 12, height = 8)


# Quem compartilha


tmp <- posts %>% 
  filter(caption %in% c( "folhapolitica.org"), dia %in% dt$dia) %>% 
  group_by(from_name) %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T)) %>% 
  arrange(-n)


ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n", "shares"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")




#  Cpt5 -------------------------------------------------------------------

# Figura XX – Série temporal de compartilhamentos da Revoltados Online



p <- posts %>%  filter(from_name %in% c("Revoltados ON LINE"))



tmp <- p %>% 
  group_by(from_name, mes_dia)  %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T),
            media = mean(shares_count, na.rm = T)) %>% 
  ungroup %>%  
  droplevels() 

ylab <- c(0, 2, 4, 6)
tmp %>% 
  ggplot(aes(x=mes_dia, y=shares, group=from_name)) +
  geom_line(color = "#314256", size = 4) +
  geom_point(color = "#314256", size = 4.5, shape=21, fill="white")+
  labs(x = "", y = "", title = "Crescimento abrupto da Revoltados Online", 
       subtitle = "Total de compartilhamentos por mês passa de 74 mil para 3,2 milhões em dois meses", 
       caption = "Dados extraídos pela Facebook Graph API")+
  theme_ipsum(grid = "Y") +
  tema +
  geom_label_repel(data = filter(tmp, mes_dia == "2015-02-01"), aes(label=paste(prettyNum(shares, big.mark=".")), fill = "#314256"), 
                   color = "white", size = 6,direction = "x" , alpha = .9
  )  +
  theme(legend.position = 'none') + 
  scale_color_manual(values =c("#436685", "#BF2F24"))  + 
  scale_fill_manual(values =c("#436685", "#BF2F24")) +
  scale_x_date(date_breaks  = "2 month", labels=date_format("%b %y"))+ 
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  )


ggsave("figuras/cpt5_evo_revoltados.png", width = 12, height = 8)

# Figura XX – Análise comparativa de compartilhamentos de MBL, Vem Pra Rua e Revoltados Online

# Revoltados MBLVPR 


p <- posts %>%  filter(from_name %in% c("Revoltados ON LINE"))


tmp <- p %>% 
  group_by(from_name, ano)  %>% 
  summarise(n = n(),
            shares = sum(shares_count, na.rm = T),
            media = mean(shares_count, na.rm = T)) %>% 
  ungroup %>%  
  droplevels() 


tmp <- crowdtangle %>% 
  filter(from_name %in% c("Vem Pra Rua Brasil", "MBL - Movimento Brasil Livre", "O Globo"), ano != 2019) %>% 
  group_by(from_name, ano) %>% 
  summarise(n = n(),
            shares = sum(Shares, na.rm = T),
            media = mean(Shares, na.rm = T)) %>% bind_rows(tmp)


ylab <- c(0, 10, 20, 30, 40)

tmp %>%
  ggplot(aes(x=ano, y=shares, group=from_name, color = from_name)) +
  geom_line(size = 4) +
  geom_point( fill = "white", size = 4.5, shape = 21)+
  labs(x = "", y = "Compartilhamentos", title = "MBL atinge maior visibilidade", 
       subtitle = "Grupos pró-impeachment batem recordes de compartilhamentos", 
       caption = "Dados extraídos pela Facebook Graph API \n Complementação dos dados pelo Crowdtangle") +
  ggrepel::geom_text_repel(data=top_n(tmp, 1, shares), aes(label=paste(from_name, 
                                                                       round(shares/1000000, 0), "milhões")), 
                           color = "black", size = 5,direction = "x" , alpha = .9, nudge_x = -1.16, fontface = "italic",
                           family = "Roboto Condensed"
  )  +
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  theme(legend.position = 'none') + tema +
  scale_color_manual(values = c("#0072b2", "#cfcfcf",  "#D55E00", "#009e73"),
                     name = NULL) +
  scale_fill_manual(values = c("#0072b2",  "#cfcfcf", "#D55E00", "#009e73"),
                    name = NULL) +
  expand_limits(x = c(0, NA), y = c(0, NA))  +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab
  )


ggsave("figuras/cpt5_evo_gbmblrovpr.png", width = 12, height = 8)

# Tabela XX – Sumário descritivo dos dados das páginas da Rede RFA
back_paginas <- paginas



paginas$rfa <- NA
paginas$rfa[paginas$from_name == "MCC - Movimento Contra Corrupção"] <- TRUE
paginas$rfa[paginas$from_name == "TV Revolta"] <- TRUE
paginas$rfa[paginas$from_name == "Correio do Poder"] <- TRUE
paginas$rfa[paginas$from_name == "Política na Rede"] <- TRUE
paginas$rfa[paginas$from_name == "Portal Curió"] <- TRUE
paginas$rfa[paginas$from_name == "Humor 13"] <- TRUE
paginas$rfa[paginas$from_name == "Folha Política"] <- TRUE
paginas$rfa[paginas$from_name == "Gazeta Social"] <- TRUE
paginas$rfa[paginas$from_name == "Ficha Social"] <- TRUE
paginas$rfa[paginas$from_name == "Juventude Contra Corrupção"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Rio de Janeiro"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Distrito Federal"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Espírito Santo"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Santa Catarina"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - São Paulo - Capital"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Minas Gerais"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - Paraná"] <- TRUE
paginas$rfa[paginas$from_name == "Movimento Contra Corrupção - São Paulo"] <- TRUE

p <- subset(paginas, paginas$rfa == T) %>% select(from_id, rfa)


#posts <- posts[,c(1:21)]


# mescla
posts_rfa <- inner_join(back, p)


# Engajamento
sum(posts$shares_count, na.rm = T) /sum(back$shares_count, na.rm = T)

t <- back %>% 
  filter(dia %in% dt$dia)%>% 
  group_by(ano) %>%
  summarise(posts_rfa = n(), 
            total = sum(shares_count, na.rm = T))
  

tmp <- posts %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(ano) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            media = round(shares/n, 2)) %>%
  inner_join(t) %>% 
  mutate(p = shares/total) %>% 
  na.omit


## Figura XX – Compartilhamentos da RFA subiram significativamente durante impeachment

tmp %>% 
  ggplot(aes(x=ano, y=p)) +
  geom_col(fill = "#4b82db", alpha = .8)+
  labs(x = "", y = "", title = "RFA teve papel importante em 2016", 
       subtitle = "Rede apócrifa foi responsável por 11,4% dos compartilhamentos da amostra", 
       caption = "Dados extraídos pela Facebook Graph API")+
  geom_text(aes(label= paste(percent(p), "\n n =", prettyNum(shares,big.mark =  "."))), 
            hjust=.5, vjust=-.1, size = 6, fontface = "bold", 
            color = "#3d3d3d") +
  #scale_y_comma(limits=c(0,150000))+ 
  theme_ipsum_rc(grid="Y") + tema + 
  expand_limits(y = .14) +
  scale_y_percent()



ggsave("figuras/cpt7_ev_rede_rfa.png", width = 12, height = 8)



# Tabela XX – Sumário descritivo dos dados das páginas da Rede RFA

tmp <- posts %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(from_name) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  #inner_join(a) %>% 
  mutate(shares_m = shares/n) %>% 
  filter(n > 100) %>% 
  arrange(-shares) %>% 
  na.omit() %>% 
  head(20)



ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")




# Tabela XX – Sumário descritivo dos dados das páginas da Rede RFA



tmp <- posts %>% 
  filter(caption != "facebook.com") %>% 
  group_by(caption) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  arrange(-shares) %>% 
  mutate(shares_n = shares/n) %>% 
  na.omit() %>% 
  filter(n > 15 & caption != "youtube.com")

ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")


# rfa_entity --------------------------------------------------------------
library(spacyr)
library(tm)

posts_rfa$text <- paste(posts_rfa$message, posts_rfa$name)


posts_rfa$text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", posts_rfa$text )
posts_rfa$text <- gsub("Lava Jato|Lava-Jato", "Lavajato", posts_rfa$text )
posts_rfa$text <- gsub("Tv Revolta|Movimento Contra a Corrupção|Juventude Contra a Corrupção|Folha Política|MCC|NA", "", posts_rfa$text )

posts_rfa$n_words <- str_count(posts_rfa$text, '\\w+')
# manter apenas acima de 1
posts_rfa <- posts_rfa[posts_rfa$n_words > 5,] # limpar vazias
#pub_filtro = posts_rfa %>% 
#  mutate(text = clean_tweets(text) %>% 
#           enc2native()) %>% 
#  filter(!is.na(text))
posts_rfa$texto <- str_replace_all(posts_rfa$text, "http\\S+\\s*","")
posts_rfa$texto <- removePunctuation(posts_rfa$texto)
posts_rfa$texto <- removeNumbers(posts_rfa$texto) #: Remove numbers
posts_rfa$texto <- stripWhitespace(posts_rfa$texto)
posts_rfa <- posts_rfa %>% 
  filter(!is.na(texto), dia %in% dt$dia)

## spacy
spacy_initialize(model = "pt")



# entity extraction
sample <- sample_n(posts_rfa, 25000)

parsed <- spacy_parse(sample$texto, entity = TRUE, multithread =T)
entity_a <- entity_extract(parsed)


ent_u <- subset(entity_a, !duplicated(entity))

tmp <- entity_a %>% 
  #filter(entity_type == "PER")  %>% 
  mutate(entity = entity) %>% 
  group_by(entity) %>% 
  summarise(n = n()) %>% 
  left_join(select(ent_u, entity, entity_type)) %>% 
  arrange(-n)


entopwords <- data_frame(word = c("Voce", "Ja", "Entao", "timeline_photos",
                                  "Peco", "Articulacao", "CNH", "Ta", "Concordo", "Parabens",
                                  "So", "Tenho", "Quero", "Mudou", "Acho", "Vejam", "Repasse", "Pessoal",
                                  "R$", "Vamos", "Traga", "Repassem", "Compartilhem", "Estao", "Extrema", "Constituicao",
                                  "Hang_huaaa_kiyaaa", "Proibido", "Senhor", "Psol", "Hang_huaaa_kiyaaa_mobile", "Sera", "Sou", 
                                  "Jonathas_pereira_de_souza", "Ninguem", "Educao", "Educacao", "Jesus", "Cristo", "Deus", "Jesus_cristo", 
                                  "Atencao", "Vou", "Olha", "Franco_timoteo", "Gilvan_da_rede", "Joice_ferreira", "Lava_jato", "Leia", 
                                  "Pai", "Mundo", "Homeschooling", "Presidente_bolsonaro", "manifeste", "repúdio", "manifestantes", 
                                  "Justiça", "\\“", "NA", "luciana_peres_timeline_photos", "impeachment_já", "vídeo", "venha", "notícias"))

entity_a$entity <- gsub("juiz_sergio_moro|sergio_moro|sérgio_moro", "moro", tolower(entity_a$entity))
entity_a$entity <- gsub("dilma_rousseff|dilma_leia", "dilma", tolower(entity_a$entity))
entity_a$entity <- gsub("renan_calheiros", "renan", tolower(entity_a$entity))
entity_a$entity <- gsub("jair_bolsonaro", "bolsonaro", tolower(entity_a$entity))
entity_a$entity <- gsub("fernando_haddad", "haddad", tolower(entity_a$entity))
entity_a$entity <- gsub("eduardo_cunha", "cunha", tolower(entity_a$entity))
entity_a$entity <- gsub("aécio_neves", "aécio", tolower(entity_a$entity))
entity_a$entity <- gsub("rodrigo_maia", "maia", tolower(entity_a$entity))
entity_a$entity <- gsub("michel_temer", "temer", tolower(entity_a$entity))
entity_a$entity <- gsub("geraldo_alckmin", "alckmin", tolower(entity_a$entity))
entity_a$entity <- gsub("gleisi_hoffman", "gleisi", tolower(entity_a$entity))
entity_a$entity <- gsub("jair_messias_bolsonaro", "bolsonaro", tolower(entity_a$entity))
entity_a$entity <- gsub("curta_moro", "moro", tolower(entity_a$entity))
entity_a$entity <- gsub("ciro_gomes", "ciro", tolower(entity_a$entity))
entity_a$entity <- gsub("olavo_de_carvalho", "olavo", tolower(entity_a$entity))
entity_a$entity <- gsub("na_lula", "lula", tolower(entity_a$entity))

# recode
entity_a[entity_a$entity %in% c("Dilma", "dilma"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("bolsonaro", "Bolsonaro"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Cunha", "cunha"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("General_Mourão", "mourão"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Mariana_Camargo"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Celso_Daniel", "celso_daniel"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Haddad", "haddad"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Temer", "temer"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Maia", "maia"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Kajuru", "kajuru"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Maluf", "maluf"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Sarney", "sarney"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Toffoli", "toffoli"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Marina", "marina"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Vaccari","vaccari"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Maia", "maia"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Lula", "lula"), "entity_type"] <- "PER"
entity_a[entity_a$entity %in% c("Moro","moro"), "entity_type"] <- "PER"

# Organizações
orgwords <- data_frame(word = c("PARAR", "paulista","VEZ", "NOVAMENTE", "pec", "constituição", 
                                "acompanhe", "dilma", "brasil", "comissão", "direito",
                                "maré", "paulista", "atenção", "google", "bolsa_família", "petição", "ps",
                                "MOSTRAR", "TODOS", "ISSO", "VENENO", "FAKE", "PODER",
                                "REALIDADE", "COMPARTILHE", "SAIBA", "MAIORIA", "REALIDADE", "COMBATIDO", "COMPETENCIA", "Saiba",
                                "ACABOU", "NOSSO", "Nosso", "FORMAO", "VDEO", "VIDEO", "Video", 
                                "SIM_!", "Sim", "Competencia", "Competncia", "Honestidade", "Presidente", 'Grupo', "Favor", 
                                "Ordem", "Ditadura", "Esse", "Nacao", "Pornografia", "Pais", "R$", "Governo_bolsonaro", "Adm", 
                                "Movimento_cultural_duna_", "Somos", "Pai", "Amamos_bolsonaro", "narrativa", "arrepiar", "mas", "voc", 
                                "fim", "olha", "possamos", "roubar", "claro", "sua_parte", "faça", 'imagem_reprodução_redes_sociais',
                                "fora_pt_impeachment_já", "fora_pt"))


# correções
entity_a[entity_a$entity %in% c("STF", "stf", "Stf"), "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "senado", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "judiciario", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "imprensa", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "midia", "entity_type"]  <- "ORG"
entity_a[entity_a$entity %in% "folha", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "estadao", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "MDB", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "exercito", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "odebrecht", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "câmara", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "senado", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "congresso", "entity_type"] <- "ORG"
entity_a[entity_a$entity %in% "ministério_público", "entity_type"] <- "ORG"


entity_a[grepl("UF", entity_a$entity), "entity_type"] <- "ORG"

entity_a$entity <- gsub("movimento_passe_livre|passe_livre", "mpl", tolower(entity_a$entity))
entity_a$entity <- gsub("rede_globo", "globo", tolower(entity_a$entity))


# Figura XX – Personagens e organizações mais citados nas publicações da Rede RFA

tmp <- entity_a %>% 
  filter(entity_type == "PER")  %>% 
  mutate(entity = tolower(entity)) %>% 
  group_by(entity) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!entity %in% tolower(entopwords$word),
         !grepl("kk|\\.", entity)) %>% 
  head(20) %>% mutate(type = "Pessoa")

tmp <- entity_a %>% 
  filter(entity_type == "ORG")  %>% 
  mutate(entity = tolower(entity)) %>% 
  group_by(entity) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!entity %in% tolower(orgwords$word),
         !grepl("kk|\\.", entity)) %>% 
  head(20) %>% mutate(type = "Organização") %>%  bind_rows(tmp)

#grafico

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}


scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

tmp %>% 
  ggplot(aes(x=reorder_within(entity, n, n), y=n)) +
  geom_col(fill = "#4b82db", alpha = .8)+
  labs(x = "", y = "", title = "Foco principal no antipetismo", 
       subtitle = "PT e lideranças são mais mencionadas; STF aparece em seguida", 
       caption = "Dados extraídos pela Facebook Graph API \nNota: Identificação de pessoas e organizaçõesautomática pelo algoritmo de NLP Spacy")+
  geom_text(aes(label= paste(prettyNum(n,big.mark =  "."))), 
            hjust=-.1, vjust=.4, size = 6, fontface = "bold", 
            color = "#3d3d3d") +
  theme_ipsum_rc(grid="X") + tema + 
  coord_flip() + 
  scale_x_reordered() +facet_wrap(~ type, scales = "free") + expand_limits(y = c(0,6000))


ggsave("figuras/cpt5_ev_rede_rfa_ent.png", width = 12, height = 8)

# Figura XX – Série temporal das citações nas publicações da Rede RFA


e <- entity_a %>% 
  filter(entity_type == "PER")  %>% 
  mutate(entity = tolower(entity)) %>% 
  group_by(entity) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  filter(!entity %in% tolower(entopwords$word)) %>% 
  head(6) 

e <- inner_join(e, entity_a)
e$doc_id <- as.character(gsub("text","",e$doc_id))
sample$doc_id <- as.character(1:nrow(sample))
e <- inner_join(e, select(sample, doc_id, ano))

a <- sample %>% 
  group_by(ano) %>% 
  summarise(total = n())

tmp <- e %>% 
  group_by(entity,  ano) %>% 
  summarise(n = n())  %>%
  inner_join(a) %>% 
  mutate(p = n/total) %>% 
  na.omit 



tmp <- tmp %>% group_by(entity) %>% summarise(n = prettyNum(sum(n), ".")) %>% 
  mutate(label = paste(str_to_title(entity), "\nTotal =", n)) %>% select(-n) %>% 
  inner_join(tmp)

tmp <- tmp %>%mutate(name2 = label) %>%   as.data.frame() %>% select(- entity)



tmp %>%
  ggplot( aes(x=ano, y=p)) +
  geom_line( data=tmp %>% dplyr::select(-label), aes(group=name2), color="grey", size=3, alpha=0.3) +
  geom_line(aes(group = label), color = "#4b82db", size=3.5 ) +
  geom_point(color = "#4b82db", fill = "white", size = 2.5, shape = 21) +
  #viridis::scale_color_viridis(discrete = TRUE) +
  
  labs(x = "", y = "Compartilhamentos", title = "Bolsonaro domina menções em 2018", 
       subtitle = "Lula, Dilma e Moro são mais citados em 2016", 
       caption = "Dados extraídos pela Facebook Graph API \nComplementação dos dados pelo Crowdtangle") +
  ggrepel::geom_text_repel(data=tmp %>%  group_by(label) %>% top_n(1, p), aes(label=percent(p)), 
                           color = "black", size = 5,direction = "y" , nudge_y=.03,
                           alpha = .9,  fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )  +
  facet_wrap(~label) +
  theme_ipsum_rc(grid = "Y", base_size = 4) + tema  + expand_limits(y = .45) +
  theme(legend.position = 'none') + scale_y_percent()




ggsave("figuras/cpt5_entity_per_ano.png", width = 12, height = 8)

# MBL


mbl <- read.csv("rede_mbl_derrubada.csv")
mbl$from_id <- NULL
mbl$from_name <- trimws(mbl$from_name)
p <- inner_join(paginas, mbl)


paginas$mbl <- NA
paginas$mbl[paginas$from_name == "MBL - Movimento Brasil Livre"] <- TRUE
paginas$mbl[paginas$from_name == "Fernando Holiday"] <- TRUE
paginas$mbl[paginas$from_name == "Kim Kataguiri"] <- TRUE
paginas$mbl[paginas$from_name == "Movimento Brasil Livre - Brasília"] <- TRUE
paginas$mbl[paginas$from_name == "Movimento Brasil Livre - Belo Horizonte MG"] <- TRUE
paginas$mbl[paginas$from_name == "MBL - Movimento Brasil Livre RS"] <- TRUE
paginas$mbl[paginas$from_name == "MBL - Movimento Brasil Livre - RJ"] <- TRUE
paginas$mbl[paginas$from_name == "Movimento Brasil Livre - São Paulo"] <- TRUE
paginas$mbl[paginas$from_name == "Movimento Brasil Livre - GO"] <- TRUE
paginas$mbl[paginas$from_name == "MBL - Movimento Brasil Livre Curitiba"] <- TRUE
paginas$mbl[paginas$from_name == "MBL - Movimento Brasil Livre PB"] <- TRUE
paginas$mbl[paginas$from_name == "Movimento Brasil Livre - Paraná"] <- TRUE
paginas$mbl[paginas$from_name == "Socialista de iPhone"] <- TRUE
paginas$mbl[paginas$from_name == "Mamaefalei"] <- TRUE

p$mbl = TRUE
p <- rbind(p, subset(paginas, paginas$mbl == T))#[, -72])



# mescla
posts <- inner_join(back, p[, -2])

# Tabela XX – Sumário descritivo das fontes da Rede MBL

tmp <- posts %>% 
  filter(dia %in% dt$dia) %>% 
  group_by(from_name) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  #inner_join(a) %>% 
  mutate(shares_n = shares/n) %>% 
  filter(n > 100) %>% 
  arrange(-shares) %>% 
  na.omit() %>% 
  head(20)

sum(posts$shares_count, na.rm = T) /sum(back$shares_count, na.rm = T)

ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")

# Tabela XX – Sumário descritivo das fontes da Rede MBL



tmp <- posts %>% 
  filter(caption != "facebook.com") %>% 
  group_by(caption) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  arrange(-shares) %>% 
  mutate(shares_n = shares/n) %>% 
  na.omit() %>% 
  filter(n > 15 & caption != "youtube.com")

ft <- flextable(head(tmp, 20))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")


# BOlsonaro e moro


p <- paginas %>% 
  select(from_name, about, description, modularity_class, ESTRUTURA_NOMES)


pattern <- c("bolso|Moro|jato")

p <-  paginas[with(paginas, grep(pattern, paste(from_name), ignore.case = T)),]

p <- p %>% 
  filter(ESTRUTURA_NOMES != "1" | is.na(ESTRUTURA_NOMES), from_name != "Jair Bolsoneca")


# mescla
posts <- inner_join(back, p[, -2])

# Tabela XX – Canais de apoio a Jair Bolsonaro e Sérgio Moro

tmp <- posts %>% 
  filter(dia %in% dt$dia, included == F) %>% 
  group_by(from_name) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  #inner_join(a) %>% 
  mutate(shares_n = shares/n) %>% 
  filter(n > 100) %>% 
  arrange(-shares) %>% 
  na.omit() %>% 
  head(20)


tmp <- crowdtangle %>% 
  filter(from_id %in% p$from_id, dia %in% dt$dia) %>% 
  group_by(from_name) %>%
  summarise(n = sum(Post.Count, na.rm = T), 
            shares = sum(Shares, na.rm = T), 
            shares_m = median(Shares, na.rm = T))  %>% 
  #inner_join(a) %>% 
  mutate(shares_n = shares/n) %>% 
  filter(n > 100) %>% 
  arrange(-shares) %>% 
  bind_rows(tmp) %>% 
  na.omit()



sum(posts$shares_count, na.rm = T) /sum(back$shares_count, na.rm = T)

ft <- flextable(head(tmp, 30))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")


# fontes




tmp <- posts %>% 
  filter(caption != "facebook.com") %>% 
  group_by(caption) %>%
  summarise(n = n(), 
            shares = sum(shares_count, na.rm = T), 
            shares_m = median(shares_count, na.rm = T))  %>% 
  arrange(-shares) %>% 
  mutate(shares_n = shares/n) %>% 
  na.omit() %>% 
  filter(n > 15 & caption != "youtube.com")

ft <- flextable(head(tmp, 20))
ft <- colformat_num(x = ft, col_keys = c("n", "shares", "shares_m", "shares_n"), big.mark = ".", digits =0, na_str = "missing")
ft <- autofit(ft)
print(ft, preview = "docx")



# Nicho midiático de direita
posts = back
posts <- left_join(posts, select(paginas, from_id, NATUREZA, modularity_class, esq_dir))

posts <- posts %>% 
  filter(NATUREZA != "Imprensa", modularity_class == "Direita")

library(tidytext)
library(tm)
# Figura XX – Comparação de menções a Bolsonaro e Aécio Neves


posts$text <-tolower(iconv(posts$message, "UTF-8", to='ASCII//TRANSLIT'))
#tw <- tweets$text
text_df <- data_frame(id = as.character(posts$id),
                      text = as.character(stringi::stri_enc_toutf8(posts$text)),
                      group = as.character(posts$esq_dir), 
                      ano = posts$ano, 
                      dia = posts$dia,
                      mes_dia = posts$mes_dia, 
                      categoria = posts$NATUREZA, 
                      from_id = posts$from_id, 
                      shares_count = posts$shares_count) %>% 
  na.omit


# Obtendo numero de palavras
tokens <- text_df %>%
  unnest_tokens(word, text, token = "tweets") 


t_words <- posts %>% 
  group_by(ano) %>% 
  summarise(total = n(), 
            total_shares = sum(shares_count, na.rm = T))




# Filtrar Bolsonaro
#names(tokens)[3] <- "mes_dia"

b_words <- tokens %>% 
  dplyr::filter(word %in% c("bolsonaro","aécio")) %>% 
  group_by(word, ano) %>% 
  summarise(n = n(), 
            shares = sum(shares_count, na.rm= T)) %>% 
  inner_join(t_words) %>% 
  mutate(p = n/total, 
         p_shares = shares/total_shares)

b_words <- b_words %>%
  ungroup %>% 
  mutate(word = str_to_title(word)) %>% 
  droplevels()

sem<- sd(b_words$p)/sqrt(length(b_words$p))
Sys.setlocale("LC_TIME", 'portuguese')

b_words %>%
  ggplot(aes(x=ano, y=p_shares, group=word)) +
  geom_line(aes(color =word),size = 4) +
  geom_point( aes(color =word),fill = "white", size = 4.5, shape = 21) +
  ggrepel::geom_text_repel(data=filter(b_words, p_shares > 0.15), aes(label=paste(str_to_title(word), percent(p_shares))), 
                           color = "black", size = 6,direction = "y" , alpha = .9, nudge_x = -.36,
                           fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  ) +
  labs(x = "", y = "% de compartilhamentos", title = "Aécio aparece na eleição de 2014 e em escândalos", 
       subtitle = "Porcentagem de compartilhamentos em posts que mencionam Aécio e Bolsonaro", 
       caption = "Dados extraídos pela Facebook Graph API")+
  theme_ipsum(grid = "Y") +
  tema + 
  theme(legend.position = 'none') + 
  scale_color_manual(values =c("#436685", "#73bf87"))  + 
  scale_fill_manual(values =c("#436685", "#73bf87"))   +
  tema + scale_y_continuous(labels = percent_format()) 




ggsave("figuras/cpt5_aecio_bolso.png", width = 12, height = 8)


# Figura XX – Hashtags mais compartilhadas em postagens de páginas do grupo de direita


hash_tokens <- tokens %>% 
  filter(grepl("#", word),group == "Direita" ) %>%
  filter(str_count(word) > 2)

hash_count <- hash_tokens %>% 
  group_by(word) %>% 
  summarise(n = n(),
            pg = n_distinct(from_id),
            shares = sum(shares_count, na.rm = T))

hash_count %>%
  filter(!word %in% c("#hoje", "#th")) %>%
  top_n(20, shares) %>% 
  ggplot(aes(reorder(word, shares), shares, label = shares))+
  geom_col(fill = "#4b82db", alpha = .8) +
  labs(x = "", y = "Total de Compartilhamentos", title = "Antipetismo, impeachment e Bolsonaro", 
       subtitle = "Hashtags mais compartilhadas na direita", 
       caption = "Dados extraídos da Facebook Graph API")+
  geom_text(aes(label= paste(prettyNum(shares,big.mark =  "."))), 
            hjust=-.1, vjust=.3, size = 6, fontface = "italic", 
            color = "#3d3d3d") +
  theme_ipsum_rc(grid="X") + tema + 
  expand_limits(y = c(0, 2660000)) + coord_flip()


ggsave("figuras/cpt_5casos_direita_hash.png", width = 12, height = 8)


# desenho curtidas bolsonaro
ylab <- c(2.5, 5.0, 7.5)

tmp <- crowdtangle %>% 
  filter(from_name %in% c("Fernando Haddad", "Jair Messias Bolsonaro"), dia > "2015-09-01", dia < "2019-01-01")

tmp %>% 
  ggplot(aes(x=dia, y=Page.Likes, group=from_name))+ 
  geom_vline(xintercept = as.Date("2018-08-25"), linetype = "dashed")+ 
  geom_vline(xintercept = as.Date("2018-10-30"), linetype = "dashed") +
  geom_line(aes(color = from_name),size = 3)+
  labs(x = "", y = "Seguidores", title = "Bolsonaro cresce a partir do final de agosto", 
       subtitle = "Ganho significativo de seguidores antes da facada", 
       caption = "Dados extraídos pelo Crowdtangle")+
  
  theme_ipsum_rc(grid = "Y", base_size = 16)+
  ggrepel::geom_text_repel(data=filter(tmp, dia %in% as.Date(c("2018-12-31"))), aes(label=paste(from_name, prettyNum(Page.Likes, "."))), 
                           color = "black", size = 6,direction = "y" , alpha = .9, nudge_y = 500000,
                           fontface = "italic", segment.alpha = .0,
                           family = "Roboto Condensed"
  )+
  theme(legend.position = 'none') + tema + 
  scale_color_manual(values =c("#BF2F24", "#436685"))  + 
  scale_fill_manual(values =c("#BF2F24", "#436685"))   +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab )



ggsave("figuras/cpt5_seg_bolso_haddad.png", width = 12, height = 8)


