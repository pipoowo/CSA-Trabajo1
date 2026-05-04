
#---- 1. Cargar paquetes ----
pacman::p_load(lme4,reghelper,haven,stargazer,ggplot2,dplyr, patchwork,
               texreg,ggeffects,sjmisc,statar,summarytools,psych,sjPlot,
               plm,lmtest,foreign, tidyverse)



#---- 2. Cargar bases de datos ----

datos_lb_proc<- readRDS(file = "input/data/proc/datos_lb_proc.rds")

Latinobarometro18_20_LARR<- readRDS(file = "input/data/original/Latinobarometro18_20_LARR.rds")

Latinobarometro18_20_LARR <- as.data.frame(Latinobarometro18_20_LARR)  

#---- Asignar valores de referencia ----
datos_lb_proc <- datos_lb_proc %>%
  mutate(
    clase_social = relevel(as.factor(clase_social), ref = "1. Gran empleador"),
    posicion_pol_ord = relevel(as.factor(posicion_pol_ord), ref = "Izquierda"),
    pais = relevel(as.factor(pais), ref = "ARG"),
    ano = relevel(as.factor(ano), ref = "2018"))

#---- 3. Figura 3: Interaccion clase /año (a y b)----

log1a <- glm(conf_sindicato_dic ~ clase_social + sexo + edad + pais + ano +
              conf_instituciones + posicion_pol_ord + clase_social*ano,
            data = datos_lb_proc,
            family = "binomial")

log1b <- glm(trustunions_life_dummy ~ class3+female+edad+pais+ano+
               trust_pol_institutions+pol_pos+class3*ano,
             data = Latinobarometro18_20_LARR,
             family = "binomial") 

pred1a <- ggpredict(log1a, terms = c("clase_social", "ano")) %>% 
  as.data.frame() %>% 
  mutate(origen = "datos_lb_proc")

pred1b <- ggpredict(log1b, terms = c("class3", "ano")) %>% 
  as.data.frame() %>% 
  mutate(origen = "Latinobarometro18_20_LARR")

predtot1 <- bind_rows(pred1a, pred1b)

Int_Clase_Ano <- ggplot(pred1a, aes(x = x, y = predicted, shape = group, color = group, linetype = group)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = group), size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "a) Reproducción: Probabilidad de confiar en un sindicato por clase social y año", x = "Clase social", y = "% de confiar en sindicato") +
  scale_x_discrete(name = "") +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año",
                        limits = c("2018", "2020"),
                        labels = c("2018", "2020"),
                        values = c("solid", "dashed")) +
  scale_color_manual(name = "Año",
                     limits = c("2018", "2020"),
                     labels = c("2018", "2020"),
                     values = c("#1f77b4", "#1f77b4")) + 
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

Int_Clase_Ano_Junto <- ggplot(predtot1, aes(x = x, 
                                           y = predicted, 
                                           color = origen, 
                                           shape = group, 
                                           linetype = group,
                                           group = interaction(group, origen))) + 
  geom_point(size = 2.5) +
  geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "b) Comparación: Probabilidad de confiar en un sindicato por clase social y año", x = "Clase social", y = "% de confiar en sindicato") +
  scale_x_discrete() +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año", values = c("solid", "dashed")) +
  scale_color_manual(name = "Base de datos",
                     labels = c("datos_lb_proc"="Reproducción: Elaboración propia",
                            "Latinobarometro18_20_LARR"="Original: Pérez & Carrasco (2024)"),
                     values = c("#1f77b4", "#d62728")) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#---- 4. Figura 3: Interaccion pol position /año (c y d) ----
log2a <- glm(conf_sindicato_dic ~ clase_social + sexo + edad + pais + ano +
              conf_instituciones + posicion_pol_ord + posicion_pol_ord*ano,
            data = datos_lb_proc,
            family = "binomial")

log2b <- glm(trustunions_life_dummy ~ class3+female+edad+pais+ano+
               trust_pol_institutions+pol_pos+pol_pos*ano,
             data = Latinobarometro18_20_LARR,
             family = "binomial") 

pred2a <- ggpredict(log2a, terms = c("posicion_pol_ord", "ano")) %>% 
  as.data.frame() %>% 
  mutate(origen = "datos_lb_proc")

pred2b <- ggpredict(log2b, terms = c("pol_pos", "ano" )) %>% 
  as.data.frame() %>% 
  mutate(origen = "Latinobarometro18_20_LARR")

predtot2 <- bind_rows(pred2a, pred2b)

Int_PolPos_Ano <- ggplot(pred2a, aes(x = x, y = predicted, shape = group, color = group, linetype = group)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = group), size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "c) Reproducción: Probabilidad de confiar en un sindicato por posición política y año", x = "Posición política", y = "% de confiar en sindicato") +
  scale_x_discrete(name = "") +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año",
                        limits = c("2018", "2020"),
                        labels = c("2018", "2020"),
                        values = c("solid", "dashed")) +
  scale_color_manual(name = "Año",
                     limits = c("2018", "2020"),
                     labels = c("2018", "2020"),
                     values = c("#1f77b4", "#1f77b4")) + 
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

Int_PolPos_Ano_Junto <- ggplot(predtot2, aes(x = x, 
                                           y = predicted, 
                                           color = origen, 
                                           shape = group, 
                                           linetype = group,
                                           group = interaction(group, origen))) + 
  geom_point(size = 2.5) +
  geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "d) Comparación: Probabilidad de confiar en un sindicato por posición política y año", x = "Posición política", y = "% de confiar en sindicato") +
  scale_x_discrete() +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año", values = c("solid", "dashed")) +
  scale_color_manual(name = "Base de datos",
                     labels = c("datos_lb_proc"="Reproducción: Elaboración propia",
                            "Latinobarometro18_20_LARR"="Original: Pérez & Carrasco (2024)"),
                     values = c("#1f77b4", "#d62728")) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

figura3 <- (Int_Clase_Ano + Int_Clase_Ano_Junto) / (Int_PolPos_Ano + Int_PolPos_Ano_Junto)

ggsave(figura3, filename = "output/graphs/figura3.png",
       device = "png",dpi = "retina", units = "cm",
       width = 36,height = 24)


#---- 5. Figura 4: Interaccion pais*año (ordenado de menor a mayor confianza) ----
log3a<- glm(conf_sindicato_dic ~ clase_social+sexo+edad+pais+ano+
             conf_instituciones+posicion_pol_ord+pais*ano,
           data = datos_lb_proc,
           family = "binomial") 

log3b<- glm(trustunions_life_dummy ~ class3+female+edad+pais+ano+
             trust_pol_institutions+pol_pos+pais*ano,
           data = Latinobarometro18_20_LARR,
           family = "binomial")

pred3a <- ggpredict(log3a, terms = c("pais", "ano")) %>% 
  as.data.frame() %>% 
  mutate(origen = "datos_lb_proc") %>%
  mutate(x = fct_reorder2(x, group, predicted, .desc = FALSE))

pred3b <- ggpredict(log3b, terms = c("pais", "ano")) %>% 
  as.data.frame() %>% 
  mutate(origen = "Latinobarometro18_20_LARR") 

predtot3 <- bind_rows(pred3a, pred3b) %>%
  mutate(x = factor(x, levels = levels(pred3a$x)))

Int_Pais_Ano <- ggplot(pred3a, aes(x = x, y = predicted, shape = group, color = group, linetype = group)) +
  geom_point(size = 2.5) +
  geom_line(aes(group = group), size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "a) Recreación: Probabilidad de confiar en un sindicato por país y año", x = "País", y = "% de confiar en sindicato") +
  scale_x_discrete(name = "") +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año",
                        limits = c("2018", "2020"),
                        labels = c("2018", "2020"),
                        values = c("solid", "dashed")) +
  scale_color_manual(name = "Año",
                     limits = c("2018", "2020"),
                     labels = c("2018", "2020"),
                     values = c("#1f77b4", "#1f77b4")) + 
  scale_y_continuous(limits = c(0, 0.50), breaks = seq(0, 0.50, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

Int_Pais_Ano_Junto <- ggplot(predtot3, aes(x = x, 
                                           y = predicted, 
                                           color = origen, 
                                           shape = group, 
                                           linetype = group,
                                           group = interaction(group, origen))) + 
  geom_point(size = 2.5) +
  geom_line(size = 0.8) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, alpha = 0.7) +
  labs(title = "b) Comparación: Probabilidad de confiar en un sindicato por país y año", x = "País", y = "% de confiar en sindicato") +
  scale_x_discrete() +
  scale_shape_manual(name = "Año", values = c(16, 17)) +
  scale_linetype_manual(name = "Año", values = c("solid", "dashed")) +
  scale_color_manual(name = "Base de datos",
                     labels = c("datos_lb_proc"="Recreación: Calderón et al. (2026)",
                            "Latinobarometro18_20_LARR"="Original: Pérez & Carrasco (2024)"),
                     values = c("#1f77b4", "#d62728")) +
  scale_y_continuous(limits = c(0, 0.50), breaks = seq(0, 0.50, by = 0.05),
                     labels = scales::percent_format(accuracy = 1L)) +
  theme_bw() +
  theme(plot.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

  
#---- 6. Guardar figuras ----

figura4<- Int_Pais_Ano / Int_Pais_Ano_Junto 

ggsave(figura4, filename = "output/graphs/figura4.png",
       device = "png",dpi = "retina", units = "cm",
       width = 30,height = 24)
