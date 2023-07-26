# Author: Alicia Franco
# Maintainer: MP, IS
# Copyright: 2023, Data Cívica
# blog-dificultad-diversidad/src/descriptives.R
# =========================================================================

#### PAQUETES ####
if(!require(pacman)) install.packages("pacman")
p_load(tidyverse, janitor, here, data.table, ggnewscale, survey)

source(here("src/tema.R"))
options(survey.lonely.psu = "adjust")

#### DIRECTORIOS ####
dir <- list(folder = here("input/"),
            grafs = here("output/"))

#### Quiero ver su deseo vs accion según como se identifican ####
sdem <- fread(paste0(dir$folder, "TSDEM.csv")) %>% 
  clean_names()

geo <- fread(paste0(dir$folder, "geo_catalog.csv")) %>% 
  clean_names() %>% 
  distinct(ent, name_ent, name_ent_short) %>% 
  filter(ent != 0)

endiseg <- fread(paste0(dir$folder, "TMODULO.csv")) %>% 
  clean_names() %>% 
  select(viv_sel, hogar, ent, n_ren, folio, est_dis, upm_dis, factor, niv, p4_2, starts_with("p7"), starts_with("p8"),starts_with("p9"), starts_with("filtro"), starts_with("p5_3_"), starts_with("p5_4_"), p11_3,starts_with("p11_5_"),  starts_with("p11_4_")) %>% 
  mutate(
    trans = as.integer(filtro_9_2 == 1), #
    lgb = case_when(p8_1a == 1 ~ "Lesbiana", #
                    p8_1a == 2 ~ "Gay",
                    p8_1a == 3 ~ "Bisexual",
                    p8_1a == 4 ~ "+",
                    p8_1 == 4 | p8_1 == 5 ~ "Hetere",
                    T ~ NA_character_),
    id_genero = case_when(p9_1 == 1 & trans == 0 ~ "Hombre", #
                          p9_1 == 2 & trans == 0 ~ "Mujer",
                          p9_1 == 1 & trans == 1 ~ "Hombre trans",
                          p9_1 == 2 & trans == 1 ~ "Mujer trans",
                          p9_1 %in% 3:5 ~ "No binarie/Género fluido"),
    hetere = as.integer(p8_1 == 4 | p8_1 == 5), #
    cis = as.integer(p9_1 == 1 & trans == 0 |p9_1 == 2 & trans == 0), #
    id = paste(viv_sel, hogar, n_ren,folio,  sep = ".")) %>% 
  left_join(geo)

#### Traslape: orientación e identidad ####
tempo <- endiseg %>% 
  group_by(lgb, id_genero) %>% 
  summarise(tot = n(),
            label = "Encuestadas") %>% 
  ungroup() %>% 
  mutate(porc = round(100*tot/sum(tot),1)) %>% 
  rbind(
    endiseg %>% 
      group_by(lgb, id_genero) %>% 
      summarise(tot = sum(factor),
                label = "Representa") %>% 
      ungroup() %>% 
      mutate(porc = round(100*tot/sum(tot),2))
  ) 

ggplot() +
  geom_tile(aes(x = lgb, y = id_genero, fill = tot),
            data = tempo %>% filter(label == "Encuestadas") %>% droplevels(),
            colour="white") +
  scale_fill_distiller(palette = "Purples", direction = 1) +
  geom_text(aes(label = paste0(porc,"%\n", tot), x = lgb, y = id_genero),
            data = tempo %>% filter(label == "Encuestadas") %>% droplevels(),
            color = "black", family = "Barlow Condensed") +
  new_scale_fill() +
  geom_tile(aes(x = lgb, y = id_genero, fill = tot),
            data = tempo %>% filter(label == "Representa") %>% droplevels(),
            colour="white") +
  scale_fill_distiller(palette = "Oranges", direction = 1) +
  geom_text(aes(label = paste0(porc,"%\n", tot), x = lgb, y = id_genero),
            data = tempo %>% filter(label == "Representa") %>% droplevels(),
            color = "black", family = "Barlow Condensed") +
  facet_wrap(~ label, scales = "free") +
  labs(title = "Composición de encuestadas y número de personas que representan",
       subtitle = "Según su orientación sexual y su identidad de género",
       x = "Orientación sexual", y = "Identidad de género",
       caption = caption) +
  tema +
  theme(legend.position = "none")

ggsave(paste0(dir$grafs, "tile-repre-cruce-ident-orient.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "tile-repre-cruce-ident-orient.svg"), width = 10, height = 6)

#### COMING OUT ####
tempo <- endiseg %>% 
  filter(filtro_10_5 == 1) 
dis <- distinct(tempo, viv_sel, hogar, ent, n_ren, folio, est_dis, upm_dis, factor)
design <- svydesign(id=~folio, strata=~est_dis, data=dis, weights=~factor, nest=TRUE)

ic_bars <- function(num, denom, preg, orient_id){
  
  if(orient_id == "orient"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(lgb == denom & !is.na(eval(parse(text=num)))),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & lgb == denom),0),
             dif = preg)
  }
  if(orient_id == "id"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(id_genero == denom & !is.na(eval(parse(text=num)))),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & id_genero == denom),0),
             dif = preg)
  }
 
  
  rat <- svyratio(~tab_svy$preg, denominator=~tab_svy$cond, design, na.rm = TRUE)
  ic <- confint(rat)
  final <- tibble(group = denom, 
                  dif = preg,
                  class = orient_id,
                  porc = rat[[1]][[1]], lci = ic[1], uci = ic[2])
  return(final)
}

args <- tibble(num = c(rep(c("p8_4_2","p8_4_3"),4), rep(c("p9_5_2","p9_5_3"),3)),
               denom = c(rep("Lesbiana",2), rep("Gay",2),rep("Bisexual",2), rep("+",2),rep("Mujer trans",2), rep("Hombre trans",2), rep("No binarie/Género fluido",2)),
               preg = rep(c("¿alguno de elles quiso corregirle?", 
                            "¿alguno de elles le agredió?"),7),
               orient_id = append(rep("orient", 8),rep("id",6))) 

final <- bind_rows(args %>% pmap(ic_bars)) %>% 
  mutate(across(porc:uci, ~.x*100),
         group = factor(group, levels = c("Lesbiana", "Gay", "Bisexual", "+", "Mujer", "Mujer trans", "Hombre", "Hombre trans", "No binarie/Género fluido"))) 

# Orientación
ggplot(final %>% filter(class == "orient"), aes(group = group, y = porc, x = dif, fill = group)) +
  geom_bar(aes(color = group), stat = "identity", position = position_dodge(width=0.9), alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), position = position_dodge(width=0.9), width = 0.25) +
  geom_label(aes(label = paste0(round(porc), "%")), 
             position = position_dodge(width = 0.9), 
             show.legend = FALSE, family = "Barlow Condensed") +
  labs(title = "Cuando sus padres se enteraron de su orientación sexual, ¿alguno de ellos...", x = "", y = "",
       subtitle = "Según su orientación sexual",
       caption = caption) +
  scale_fill_manual(values = pal_orient_sex) +
  scale_color_manual(values = pal_orient_sex) +
  tema + 
  theme(legend.position = "top", axis.text.y = element_blank(), legend.title = element_blank()) 

ggsave(paste0(dir$grafs, "bar-correccion-coming-out-orientacion.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "bar-correccion-coming-out-orientacion.svg"), width = 10, height = 6)

# Identidad
ggplot(final %>% filter(class == "id"), aes(group = group, y = porc, x = dif, fill = group)) +
  geom_bar(aes(color = group), stat = "identity", position = position_dodge(width=0.9), alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), position = position_dodge(width=0.9), width = 0.25) +
  geom_label(aes(label = paste0(round(porc), "%")), 
             position = position_dodge(width = 0.9), 
             show.legend = FALSE, family = "Barlow Condensed") +
  labs(title = "Cuando sus padres se enteraron de su identidad de género, ¿algun de ellos...", x = "", y = "",
       subtitle = "Según su identidad de género",
       caption = caption) +
  scale_fill_manual(values = c(pal_id_gen[2], pal_id_gen[4],pal_id_gen[5])) +
  scale_color_manual(values = c(pal_id_gen[2], pal_id_gen[4],pal_id_gen[5])) +
  tema + 
  theme(legend.position = "top", axis.text.y = element_blank(), legend.title = element_blank()) 

ggsave(paste0(dir$grafs, "bar-correccion-coming-out-ident-gen.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "bar-correccion-coming-out-ident-gen.svg"), width = 10, height = 6)

#### Violencia en la infancia ####
tempo <- endiseg
dis <- distinct(endiseg, viv_sel, hogar, ent, n_ren, folio, est_dis, upm_dis, factor)
design <- svydesign(id=~folio, strata=~est_dis, data=dis, weights=~factor, nest=TRUE)
# Molestaron o hicieron sentir mal 

ic_bars <- function(num, denom, preg, orient_id){
  
  if(orient_id == "orient"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(lgb == denom),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & lgb == denom),0),
             dif = preg)
  }
  if(orient_id == "id"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(id_genero == denom),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & id_genero == denom),0),
             dif = preg) 
  }
  
  
  rat <- svyratio(~tab_svy$preg, denominator=~tab_svy$cond, design, na.rm = TRUE)
  ic <- confint(rat)
  final <- tibble(group = denom, 
                  dif = preg,
                  class = orient_id,
                  porc = rat[[1]][[1]], lci = ic[1], uci = ic[2])
  return(final)
}

args <- tibble(num = rep(paste0("p5_4_",1:5),10),
               denom = c(rep("Lesbiana",5),rep("Gay",5), rep("Bisexual",5), rep("+",5), rep("Hetere",5),
                         rep("Hombre",5),rep("Hombre trans",5),rep("Mujer",5), rep("Mujer trans",5), rep("No binarie/Género fluido",5)),
               preg = rep(c("le excluyeron", "le insultaron", "le robaron pertenencias", "le amenazaron", "le agredieron"),
                          10),
               orient_id = append(rep("orient", 25),rep("id",25)))

final <- bind_rows(args %>% pmap(ic_bars)) %>% 
  mutate(across(porc:uci, ~.x*100),
         group = factor(group, levels = c("Lesbiana", "Gay", "Bisexual", "+", "Hetere", "Mujer", "Mujer trans", "Hombre", "Hombre trans", "No binarie/Género fluido")),
         dif = factor(dif, levels = c("le excluyeron", "le robaron pertenencias","le insultaron","le amenazaron", "le agredieron"))) 

# Orientación
ggplot(final %>% filter(class == "orient"), aes(group = group, y = porc, x = dif, fill = group)) +
  geom_bar(aes(color = group), stat = "identity", position = position_dodge(width=0.9), alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), position = position_dodge(width=0.9), width = 0.25) +
  geom_label(aes(label = paste0(round(porc), "%")), 
             position = position_dodge(width = 0.9), hjust = 1.25,
             show.legend = FALSE, family = "Barlow Condensed") +
  labs(title = "Durante su infancia (hasta los 11 años), por molestarle, ¿alguna vez ... ?", x = "", y = "",
       subtitle = "Según su orientación sexual",
       caption = caption) +
  scale_fill_manual(values = pal_orient_sex) +
  scale_color_manual(values = pal_orient_sex) +
  tema + 
  theme(legend.position = "top", axis.text.y = element_blank(), legend.title = element_blank()) 

ggsave(paste0(dir$grafs, "bar-molestaron-orint.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "bar-molestaron-orint.svg"), width = 10, height = 6)
# Identidad
ggplot(final %>% filter(class == "id"), aes(group = group, y = porc, x = dif, fill = group)) +
  geom_bar(aes(color = group), stat = "identity", position = position_dodge(width=0.9), alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), position = position_dodge(width=0.9), width = 0.25) +
  geom_label(aes(label = paste0(round(porc), "%")),  hjust = 1.25, 
             position = position_dodge(width = 0.9), show.legend = FALSE, family = "Barlow Condensed") +
  labs(title = "Durante su infancia (hasta los 11 años), por molestarle, ¿alguna vez ... ?", x = "", y = "",
       subtitle = "Según su identidad de género",
       caption = caption) +
  scale_fill_manual(values = pal_id_gen) +
  scale_color_manual(values = pal_id_gen) +
  tema + 
  theme(legend.position = "top", axis.text.y = element_blank(), legend.title = element_blank()) 

ggsave(paste0(dir$grafs, "bar-molestaron-id.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "bar-molestaron-id.svg"), width = 10, height = 6)
# Binario 
ic_bars <- function(num, denom, preg, orient_id){
  
  if(orient_id == "orient"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(hetere == denom),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & hetere == denom),0),
             dif = preg) 
  }
  if(orient_id == "id"){
    tab_svy <-  tempo %>% 
      mutate(cond = as.integer(cis == denom),
             preg = replace_na(as.integer(eval(parse(text=num)) == 1 & cis == denom),0),
             dif = preg) 
  }
  
  
  rat <- svyratio(~tab_svy$preg, denominator=~tab_svy$cond, design, na.rm = TRUE)
  ic <- confint(rat)
  final <- tibble( group = case_when(orient_id == "orient" & denom == 1 ~ "Hetere",
                                     orient_id == "orient" & denom == 0 ~ "Diversidad sexual",
                                     orient_id == "id" & denom == 1 ~ "CIS",
                                     orient_id == "id" & denom == 0 ~ "Diversidad de género"),
                  dif = preg,
                  class = orient_id,
                  porc = rat[[1]][[1]], lci = ic[1], uci = ic[2])
  return(final)
}

args <- tibble(num = rep(paste0("p5_4_",1:5),4),
               denom = rep(c(rep(1,5), rep(0,5)),2),
               preg = rep(c("le excluyeron", "le insultaron", "le robaron pertenencias", "le amenazaron", "le agredieron"),
                          4),
               orient_id = c(rep("orient", 10),rep("id",10)))

final <- bind_rows(args %>% pmap(ic_bars)) %>% 
  mutate(across(porc:uci, ~.x*100),
         group = factor(group, levels = c("Diversidad sexual", "Hetere", "Diversidad de género", "CIS")),
         dif = factor(dif, levels = c("le excluyeron", "le robaron pertenencias","le insultaron","le amenazaron", "le agredieron"))) 


ggplot(final, 
       aes(x = dif, y = porc, fill = group)) +
  geom_bar(aes(color = group), stat = "identity", position = position_dodge(width = 0.9), alpha = 0.5) +
  geom_errorbar(aes(ymin = lci, ymax = uci, color = group), position = position_dodge(width=0.9), width = 0.25) +
  geom_label(aes(label = paste0(round(porc), "%")), hjust = 1.25,
             position = position_dodge(width = 0.9), family = "Barlow Condensed", show.legend = F) +
  scale_fill_manual(values = pal_id_gen) +
  scale_color_manual(values = pal_id_gen) +
  labs(title = "Durante su infancia (hasta los 11 años), por molestarle, ¿alguna vez ... ?", x = "", y = "",
       subtitle = "Según su pertenencia a la diversidad sexual o de género",
       caption = caption) +
  tema +
  theme(legend.title = element_blank(),
        axis.text.y = element_blank())

ggsave(paste0(dir$grafs, "bar-molestaron-binar.jpg"), width = 10, height = 6)
ggsave(paste0(dir$grafs, "bar-molestaron-binar.svg"), width = 10, height = 6)

# done