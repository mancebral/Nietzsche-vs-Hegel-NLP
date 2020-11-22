#### librerías ####
library(udpipe)
library(tidyverse)

#primero necesitamos descargar el modelo de trabajo sobre el que lemmatizaremos
udmodel <- udpipe_download_model(language = "spanish")
#y cargar el modelo 
udmodel <- udpipe_load_model(udmodel$file_model)

#ahora ya con ese modelo podemos anotar el texto que deseemos
library(pdftools)
all_pdfs <- list.files(pattern = ".pdf$")
corpus <- map_df(all_pdfs, ~ tibble(txt = pdf_text(.x)) %>%
                   mutate(documento = .x) %>%
                   mutate (documento = str_remove_all(documento, ".pdf")))

#separamos las dos obras
corpus1 <- corpus %>% 
  filter(documento == "Fenomenología del espíritu")%>%
  mutate (txt = str_remove_all(txt, "Derrida en castellano"), 
          txt= str_remove_all(txt, "http://www.jacquesderrida.com.ar"))

fenomenologia <- udpipe_annotate(udmodel, x = corpus1$txt)
fenomenologia <- as_tibble (fenomenologia)
View(fenomenologia)

#vamos con la segunda
corpus2 <- corpus %>% 
  filter(documento == "Así habló Zaratustra")

zaratustra <- udpipe_annotate(udmodel, x = corpus2$txt)
zaratustra <- as_tibble (zaratustra, detailed = TRUE)
View(zaratustra)

#### comparar resultados ####
anno_corpus <- bind_rows(zaratustra, fenomenologia, .id="id") 
anno_corpus$id <- as_factor(anno_corpus$id)
levels(anno_corpus$id) <-  c("Así habló Zaratustra", "Fenomenología del espíritu")
View(anno_corpus)

#### sentence length ####
sentence_length <- anno_corpus %>% 
  group_by(id) %>% 
  count(sentence)%>%
  ggplot(aes(n, fill=id))+
  geom_histogram(stat="bin", show.legend=FALSE)+
  facet_wrap(~id, scales = "fixed")+
  coord_cartesian(xlim = c(0, 150))+
  xlab("sentence length")+
  ylab(NULL)+
  theme (plot.background = element_rect(fill="#ebebeb"), 
         panel.background = element_rect(fill="#ebebeb"),
         legend.background = element_rect(fill = "#ebebeb"),
         strip.background =element_rect(fill="#ebebeb"),
         strip.text = element_text(colour = 'black'))

ggsave("sentence_length.png", sentence_length, width = 4, height = 2.5, dpi = 300)

# sentence mean
anno_corpus %>%
  filter(id == "Fenomenología del espíritu")%>%
  count(sentence)%>%
  mutate(mean=mean(n))%>%
  distinct(mean)

anno_corpus %>%
  filter(id == "Así habló Zaratustra")%>%
  count(sentence)%>%
  mutate(mean=mean(n))%>%
  distinct(mean)

#### tipos de palabras ####
upos_count <- anno_corpus %>% 
  group_by(id)%>%
  count(id, upos, sort = TRUE)%>%
  summarise(id, upos, prom=n/sum(n))%>%
  ungroup()%>%
  na.omit()%>%
  filter(!upos %in% c("X", "SYM", "PART", "NUM", "INTJ"))%>%
  ggplot(aes(prom, fct_reorder(upos, prom)))+
  geom_col(aes(fill=id), position = "dodge", show.legend = FALSE)+
  labs(x = "Frequency", y = NULL)+
  theme (plot.background = element_rect(fill="#ebebeb"), 
         panel.background = element_rect(fill="#ebebeb"),
         legend.background = element_rect(fill = "#ebebeb"))

ggsave("upos_count.png", upos_count, width = 4, height = 2.5, dpi = 300)

#### tipos de verbos ####
tipos_verbos <- anno_corpus %>% 
  filter (upos == "VERB") %>%
  mutate(feats=str_remove_all(feats, "Gender=Fem|"),
         feats=str_remove_all(feats, "Gender=Masc|"))%>%
  group_by (id)%>%
  count(feats, sort=TRUE)%>%
  summarise(id, feats, prom=n/sum(n)) %>%
  top_n(10,prom) %>%
  ggplot(aes(prom, as.factor(reorder(feats, prom)), fill=feats))+
  geom_col()+
  facet_wrap(~id, scales = "free_y")+
  xlab("")+
  ylab("")+
  scale_fill_discrete(labels = c("Singular Pasado Participio",
                                 "Singular Finito",
                                 "Singular Participio",
                                 "Indivativo Plural 1°Pers Presente",
                                 "Indivativo Plural 3°Pers Presente",
                                 "Indivativo Singular 1°Pers Presente",
                                 "Indivativo Singular 3°Pers Imperfecto",
                                 "Indivativo Singular 3°Pers Pasado",
                                 "Indivativo Singular 3°Pers Presente",
                                 "Subjuntivo Singular 3°Pers Presente",
                                 "Gerundio",
                                 "Infinitivo"))+
  theme (axis.text.y  = element_blank())+
  theme (axis.ticks.y  = element_blank())+
  theme (legend.title = element_blank(),
         #legend.position="bottom",
         legend.text = element_text(size=8),
         plot.background = element_rect(fill="#ebebeb"), 
         panel.background = element_rect(fill="#ebebeb"),
         legend.background = element_rect(fill = "#ebebeb"),
         strip.background =element_rect(fill="#ebebeb"),
         strip.text = element_text(colour = 'black'))

ggsave("tipos_verbos.png", tipos_verbos, width = 8, height = 3.5, dpi = 300)

### exploraciones más concretas por etiquetas ####
anno_corpus %>%
  mutate(lemma= gsub("^hombre$", "humano", lemma))%>%
  filter(upos %in% c("VERB"))%>%
  group_by(id, upos) %>%
  count(lemma, sort = TRUE) %>%
  summarise(id, upos, lemma, prom=n/sum(n))%>%
  top_n(15, prom)%>%
  ggplot(aes(prom, fct_reorder(lemma, prom)))+
  geom_col(aes(fill=id), show.legend = FALSE)+
  labs(x = "Frequency", y = NULL)+
  facet_wrap(~id, scales = "free")+
  theme(legend.position="bottom")

mis_stopwords <- tm::stopwords("es")

verbos <- anno_corpus %>%
  filter(upos == "VERB")%>%
  mutate(sentence=paste(doc_id, sentence_id,sep="-"))%>%
  select(id, sentence, verbos=lemma)

nombres <- anno_corpus %>%
  filter(upos == "NOUN")%>%
  mutate(sentence=paste(doc_id, sentence_id, sep="-"))%>%
  select(id, sentence, nombres=lemma)  

gram_corpus <- verbos %>%
  left_join(nombres)

View(gram_corpus)

verbos_top <- verbos %>% 
  group_by(id)%>%
  summarise(verbos, total_verb= n())%>%
  count (total_verb, verbos, sort = TRUE)%>%
  mutate(prom= n/total_verb)%>%
  top_n(2, prom)

library(ggraph)
library(igraph)
library(tidygraph)
library(ggthemes)

graph_corpus <- gram_corpus %>%
  mutate(nombres= gsub("^hombre$", "humano", nombres))%>%
  filter(verbos %in% verbos_top$verbos)%>%
  select(id, verbos, nombres)%>%
  group_by (id)%>%
  summarise(nombres, verbos, total_verb=n())%>%
  ungroup()%>%
  group_by(total_verb)%>%
  count(verbos, nombres, sort=TRUE)%>%
  ungroup()%>%
  mutate (prom=n/total_verb) %>%
  mutate (id= as.character(total_verb),
          id= gsub("23443", "Fenomenología del espíritu", id),
          id= gsub("6378", "Así habló Zaratustra", id)) %>%
  select(verbos, nombres, prom, id)%>%
  top_n(30, prom)%>%
  as_tbl_graph()

set.seed(1234)
red_verbos <- graph_corpus%>%
  mutate(centralidad=centrality_authority())%>%
  activate(nodes)%>%
  filter(!name == "NA")%>%
  mutate(tipo=ifelse(name %in% verbos_top$verbos, "verbos", "nombres"))%>%
  activate(edges)%>%
  #filter(id=="Fenomenología del espíritu")%>%
  ggraph (layout = "with_kk") + 
  geom_edge_link(aes(width=prom, alpha=prom), color="#00BFC4", show.legend = FALSE) +
  #geom_node_point(color = "lightblue", size = 3) +
  geom_node_text (aes(label = name, color=tipo, size=centralidad), 
                  vjust = -0.1, hjust = 0.4)+
  scale_color_manual (values = c("#F8766D", "black"))+
  scale_size(range=c(1.9,3.3))+
  facet_wrap(~id)+
  theme(legend.position="bottom")+
  theme (plot.background = element_rect(fill="#ebebeb"), 
         panel.background = element_rect(fill="#ebebeb"),
         strip.background =element_rect(fill="#ebebeb"),
         legend.background = element_rect(fill = "#ebebeb"))

ggsave("red_verbos.png", red_verbos, width = 9, height = 3.5, dpi = 300)

gram_corpus %>% filter(verbos%in% c("naturaleza", "esencia"))
anno_corpus %>% filter (lemma=="naturaleza", upos=="VERB")

#### tipos de etiquetas comparativas ####
conversion <- anno_corpus %>% 
  mutate(lemma = gsub('[[:punct:] ]+','',lemma)) %>%
  filter(!lemma == "") %>%
  select(id, lemma, upos) %>%
  group_by(id, upos)%>%
  count(lemma, sort = TRUE)%>%
  mutate(proportion=n/sum(n))%>%
  select(-n)%>%
  spread(id, proportion)%>%
  gather(id, proportion, `Fenomenología del espíritu`)

ggplot(conversion, aes(x=proportion, y=`Así habló Zaratustra`))+
  geom_abline (color= "gray40", lty = 2)+
  geom_text(aes(label=lemma))

stop_spain <- tidytext::get_stopwords("es", source = "snowball")
conversion2 <- conversion %>% 
  filter(!lemma %in% stop_spain$word,
         !upos %in% c("NUM", "PUNCT"))

library(scales)
ggplot(conversion2, aes(x=proportion, y=`Así habló Zaratustra`))+
  geom_abline (color= "red", lty = 2)+
  geom_jitter(alpha=0.1, size=2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label=lemma), check_overlap = TRUE, vjust=0.1)+
  scale_x_log10(labels= percent_format())+
  scale_y_log10(labels=percent_format())+
  scale_color_gradient(limits=c(0, 0.0001), 
                       low= "darkslategray4", high = "gray75")+
  labs(x="Fenomenología del espíritu")+
  theme_light()

#colores upos
conversion %>%
  filter (upos=="CCONJ") %>%
  ggplot(aes(x=proportion, y=`Así habló Zaratustra`))+
  geom_abline (color= "red", lty = 2)+
  geom_jitter(alpha=0.1, size=2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label=lemma, color=upos), check_overlap = TRUE, vjust=0.1)+
  scale_x_log10(labels= percent_format())+
  scale_y_log10(labels=percent_format())+
  labs(x="Fenomenología del espíritu")+
  theme_light()

#facet_wrap upos
xy_graph <- conversion %>%
  filter(!upos %in% c("INTJ", "PART", "SYM", "NUM", "PUNCT")) %>%
  filter(!lemma %in% c("baer", "eche", "ambo", "ia")) %>%
  mutate(lemma= gsub("^hombre$", "humano", lemma))%>%
  mutate (upos= gsub("X", "VERB", upos)) %>%
  ggplot(aes(x=proportion, y=`Así habló Zaratustra`))+
  geom_abline (color= "red", lty = 2)+
  geom_jitter(alpha=0.1, size=2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label=lemma, color=upos), size=3, check_overlap = TRUE, vjust=0.1,
            show.legend = FALSE)+
  scale_x_log10(labels= percent_format())+
  scale_y_log10(labels=percent_format())+
  #scale_color_gradient(limits=c(0, 0.0001), 
  #                     low= "darkslategray4", high = "gray75")+
  labs(x="Fenomenología del espíritu")+
  theme_light()+
  facet_wrap(~upos)+
  theme (plot.background = element_rect(fill="#ebebeb"), 
         panel.background = element_rect(fill="#ebebeb"),
         legend.background = element_rect(fill = "#ebebeb"))

ggsave("xy_graph.jpg", xy_graph, width = 10, height = 6, dpi = 300)
