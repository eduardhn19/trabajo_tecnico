


### Librerias 
library(reshape)
library(readxl)
library(dplyr)
library(openxlsx)
library(skimr)
library(car)
library(corrplot)
library(ggplot2)
library(extrafont) 
library(sjPlot)



### Nueva BBDD
BBDD2 <- read_xlsx("BBDD_mod_2025_03_24.xlsx")
BBDD2 <- select(BBDD2, Pais, part_voto, PIB_USAct, Gini, Persxkm, representation_est, rights_est, participation_est, rule_law_est, JRV, Tipoeleccion, Eleccion)

BBDD2$Eleccion <- as.factor(BBDD2$Eleccion)
BBDD2$Tipoeleccion <- as.factor(BBDD2$Tipoeleccion)

BBDD2$representation_est <- BBDD2$representation_est*100
BBDD2$rights_est <- BBDD2$rights_est*100
BBDD2$rule_law_est <- BBDD2$rule_law_est*100
BBDD2$participation_est <- BBDD2$participation_est*100

# Usando ggplot
ggplot(BBDD2, aes(x=PIB_USAct, y=part_voto)) +
  geom_point() + # agrega los puntos de las observaciones
  geom_smooth(method=lm, aes(fill=PIB_USAct)) + # calcula la línea de regresión con base en la variable "age"
  theme_minimal() # tema de formato del gráfico




ml1 <- lm(formula=part_voto ~ PIB_USAct + Gini + Persxkm + representation_est + rights_est + participation_est + rule_law_est + JRV + Tipoeleccion + Eleccion, data=BBDD2) 
summary(ml1)
vif(ml1)

### Nuevas pruebas según cambios

### Modelo

ml0 <- lm(formula=part_voto ~ PIB_USAct + Gini + Persxkm + representation_est + rights_est + JRV + Tipoeleccion + Eleccion, data=BBDD2) 

summary(ml0)


### Por presencia de outliers se identifican y eliminan
ml0_sin_outliers <- lm(formula(ml0), data = BBDD2[-c(120, 158, 250), ])
shapiro.test(residuals(ml0_sin_outliers))
qqnorm(residuals(ml0_sin_outliers)); qqline(residuals(ml0_sin_outliers), col = "red")
hist(residuals(ml0_sin_outliers), breaks = 30, main = "Histograma de residuos", col = "lightblue")


summary(ml0_sin_outliers)


# Independencia de errores (BIEN)

res2 <-residuals(ml0) #Entre paréntesis colocamos el nombre del modelo que estimamos previamente
plot(res2)

restan2 <-rstandard(ml0)
plot(restan2)


durbinWatsonTest(ml0_sin_outliers)


## Varianza es constante (BIEN)

lmtest::bptest(ml0_sin_outliers)


## Residuos normales (No se cumple)

par(mfrow=c(1,2))
qqnorm(restan2)
qqline(restan2)
hist(restan2)

shapiro.test(restan2)

# Instalar si no lo tenés
install.packages("nortest")
library(nortest)

# Prueba de Anderson-Darling
ad.test(residuals(ml0))

qqnorm(residuals(ml0))
qqline(residuals(ml0), col = "red", lwd = 2)

hist(residuals(ml0), breaks = 30, main = "Histograma de residuos", col = "lightblue")


which(residuals(ml0) > 40)


BBDD2[c(120, 158, 250), ]  # Verifica los datos originales de esos casos


BBDD2 <- BBDD2[-c(120, 158, 250), ]









## Multicolinealidad (BIEN)
vif(ml0_sin_outliers)



# Mostrar tabla
tab_model(ml0_sin_outliers,
          title = "Modelo voto exterior",
          show.se = T,
          show.stat = T,
          show.fstat = T,
          show.ci = F,
          file = "modelo_sin_outliers.html")


####### GRAFICO BASE BIEN

# --- Cargar fuentes (solo necesario una vez) ---
# font_import()  # Ejecutar una vez para importar fuentes
loadfonts(device = "win")  # Cargar fuentes en Windows

# --- Gráfico académico ---
plot_final <- plot_model(
  ml0_sin_outliers,
  show.values = TRUE,
  value.offset = 0.5,
  axis.labels = c("Elección [2022]", "Elección [2018]", "Primera ronda", "JRV", 
                  "Derechos y Equidad", "Gob. Representativo", 
                  "Densidad Pob.", "Gini", "PIB"),
  colors = c("#404040", "#8D8D8D"),
  dot.size = 3,
  line.size = 1.2
) +
  # Línea vertical en cero (sin recorte)
  geom_segment(
    aes(y = 0, yend = 0, x = -Inf, xend = Inf),
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  # Eliminar título y etiqueta del eje X
  labs(
    title = NULL,
    x = NULL,  # Elimina texto del eje X
    y = "Efecto estimado (β) [IC 95%]"
  ) +
  # Tema con Times New Roman
  theme_classic(base_family = "serif") +  
  theme(
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Márgenes balanceados
    text = element_text(family = "serif")  # Asegurar fuentes en todos los textos
  )

plot_final <- plot_model(
  ml0_sin_outliers,
  show.values = TRUE,
  value.offset = 0.5,
  axis.labels = c("Elección [2022]", "Elección [2018]", "Balotaje", "JRV", 
                  "Derechos y Equidad", "Gob. Representativo", 
                  "Densidad Pob.", "Gini", "PIB"),
  colors = c("#2C5C8A", "#B22222"),
  dot.size = 3,
  line.size = 1.2
) +
  # Línea vertical en cero (sin recorte)
  geom_segment(
    aes(y = 0, yend = 0, x = -Inf, xend = Inf),
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  # Eliminar título y etiqueta del eje X
  labs(
    title = NULL,
    x = NULL,  # Elimina texto del eje X
    y = "Efecto estimado (con IC de 95%)"
  ) +
  # Tema con Times New Roman
  theme_classic(base_family = "serif") +  
  theme(
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Márgenes balanceados
    text = element_text(family = "serif")  # Asegurar fuentes en todos los textos
  )


ggsave("mi_grafico.tiff",
       plot = plot_final,
       width = 8,
       height = 10,     # Misma proporción
       units = "in",
       dpi = 300,       # Alta resolución
       compression = "lzw")



plot_final <- plot_model(
  ml0_sin_outliers,
  show.values = TRUE,
  value.offset = 0.5,
  axis.labels = c("Elección [2022]", "Elección [2018]", "Balotaje", "JRV", 
                  "Derechos y Equidad", "Gob. Representativo", 
                  "Densidad Pob.", "Gini", "PIB"),
  colors = c("#3C3C3C", "#7C7C7C"),
  dot.size = 3,
  line.size = 1.2
) +
  # Línea vertical en cero (sin recorte)
  geom_segment(
    aes(y = 0, yend = 0, x = -Inf, xend = Inf),
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  # Eliminar título y etiqueta del eje X
  labs(
    title = NULL,
    x = NULL,  # Elimina texto del eje X
    y = "Efecto estimado (con IC de 95%)"
  ) +
  # Tema con Times New Roman
  theme_classic(base_family = "serif") +  
  theme(
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Márgenes balanceados
    text = element_text(family = "serif")  # Asegurar fuentes en todos los textos
  )



summary(BBDD2$part_voto)
hist(BBDD2$part_voto)



print(plot_final)



# Exportar en alta calidad (opcional)
ggsave("plot_final.png", plot_final,  width = 8, height = 6, dpi = 300)




