# # # # Trabajo Final  
# # Curso Diamante:
# # "Topicos especiales en modelos lineales generalizados"


# # # # # # # # # # # # # # # # # # # # # # # #

# 1) Carga de librerias y de datos #### 

# # # # # # # # # # # # # # # # # # # # # # # #

library(glmmADMB)
library(gridExtra)


data <- read.csv("C:\\Users\\solre\\Desktop\\ModelosMixtos\\varil.csv", sep = ";", dec=",")

str(data)
data$exinido <- factor(data$exinido, labels = c("Fracasos", "Exitos"))  
#data$vis <- factor(data$vis)



# # # # # # # # # # # # # # # # # # # # # # # #

# 2) Analisis de la base de datos  ####

# # # # # # # # # # # # # # # # # # # # # # # #

# Para ver el numero de muestras por tipo de soporte

replications(exinido ~ soporte, data)

# Se observa que nos faltan muestras para muchos de los soportes.
# Se decide omitir esta variable en el análisis

data <- data[, c(1:4, 6:7, 9, 12)]

summary(data)


### PLOT 1
g1 <- ggplot(data) + 
  geom_bar(aes( x = exinido, fill = exinido),
           show.legend = FALSE) + 
  theme_bw() +
  labs( title = "N exitos y fracasos", y= "", fill = "", x = "")


## PLOT 2
g2 <- ggplot(data) + 
  geom_boxplot(aes( x = exinido, y = long, fill = exinido), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes( x = exinido, y = long, col = exinido), 
             size = 2,
             show.legend = FALSE,
             position = position_jitterdodge(dodge.width = 0.1, jitter.width = 0.2)) + 
  theme_bw() +
  labs( title = "", y = "Longitud", col = "", x = "")


## PLOT 3
g3 <- ggplot(data) + 
  geom_boxplot(aes( x = exinido, y = ancho, fill = exinido), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes( x = exinido, y = ancho, col = exinido), 
             size = 2,
             show.legend = FALSE,
             position = position_jitterdodge(dodge.width = 0.1, jitter.width = 0.2)) + 
  theme_bw() +
  labs( title = "", y = "Ancho", col = "", x = "")


## PLOT 4
g4 <- ggplot(data) + 
  geom_boxplot(aes( x = exinido, y = prof, fill = exinido), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes( x = exinido, y = prof, col = exinido),
             size = 2,
             show.legend = FALSE,
             position = position_jitterdodge(dodge.width = 0.1, jitter.width = 0.2)) + 
  theme_bw() +
  labs( title = "", y = "Profundidad", col = "", x = "")

grid.arrange(g2, g3, g4, ncol = 3)



## PLOT 5
g5 <- ggplot(data) + 
  geom_boxplot(aes( x = exinido, y = alt, fill = exinido), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes( x = exinido, y = alt, col = exinido), 
             size = 2, 
             show.legend = FALSE,
             position = position_jitterdodge(dodge.width = 0.1, jitter.width = 0.2)) + 
  theme_bw() +
  labs( title = "", y = " Altura", col = "", x = "")


### PLOT 6
g6 <- ggplot(data) + 
  geom_boxplot(aes( x = exinido, y = vis, fill = exinido), alpha = 0.1, show.legend = FALSE) + 
  geom_point(aes( x = exinido, y = vis, col = exinido), 
             size = 2,
             show.legend = FALSE,
             position = position_jitterdodge(dodge.width = 0.1, jitter.width = 0.2)) + 
  theme_bw() +
  labs( title = "", y = "Visibilidad", col = "", x = "")


grid.arrange(g1, g5, g6, ncol = 3)

## PLOT 7
ggplot(data) + 
  geom_bar(aes( x = exinido, fill = exinido),
           show.legend = FALSE) + 
  facet_grid( ~huevos ) + 
  theme_bw() +
  labs( title = " Numero de exitos y fracasos por huevos", y= "", fill = "", x = "")




#ggplot(data) + geom_freqpoly(aes( x = alt, col = exinido)) +  theme_bw() 






# # # # # # # # # # # # # # # # # # # # # # # #

# 3) Definimos el modelo general  ####

# # # # # # # # # # # # # # # # # # # # # # # #

# Por el diseño experimental tenemos un MGLMM binomial, con variable aleatoria "nido"


data.escala <- scale(data[,2:6])
data.escala <- as.data.frame(data.escala)
names(data.escala) <- c("long.esc",  "ancho.esc", "prof.esc", "alt.esc", "vis.esc"  )
data <- cbind(data, data.escala)

#data$huevos <- factor(data$huevos) <<< poner huevos como factor tira warnings
#data$nido <- factor(data$nido)

modelo.1 <- lme4::glmer(exinido ~ prof.esc + alt.esc + vis.esc + long.esc + ancho.esc + huevos + (1| nido) , 
      data = data, 
      family = "binomial",
      na.action=na.fail)

summary(modelo.1)
plot(modelo.1)

# ¿como sabemos si ajusta?

logLik(modelo.1)
Discrep.Pear1 <- sum( resid(modelo.1, type = "pearson")^2)
dp1 <- Discrep.Pear1/(118 - 8)  #numero de obs - grados
dp1  # 0.6661579 

# >>> NO tenemos una sobredispersion. 
# (No hay funcion en R con quasibinomial + mixto)


# # # # # # # # # # # # # # # # # # # # # # # #

# 4) Analizado por teoria de la info las variables fijas####

# # # # # # # # # # # # # # # # # # # # # # # #

#MM.1 <- dredge (modelo.1, rank="QAICc", chat=dp1)  #para cuando hay sobredispersion

# Hicimos prueba para ver todos los modelos posibles:
MM.1 <- dredge(modelo.1, rank = "AICc")

MM1.sel <- model.avg(MM.1, revised.var = TRUE)
summary(MM1.sel)
coef(MM1.sel)
confint(MM1.sel)  #tenemos q ver si queda el 0 excluido! eso nos marca q es significativo

mod.0 <- glm(exinido ~ 1, 
             data = data,
             family ="binomial")

mod.1 <- lme4::glmer(exinido ~ prof.esc + alt.esc + vis.esc + long.esc + ancho.esc + huevos + (1| nido) , 
                        data = data,
                        family = "binomial")

mod.2 <- lme4::glmer(exinido ~  prof.esc + long.esc + ancho.esc + alt.esc + huevos + (1| nido) , 
                        data = data, 
                        family = "binomial")

mod.3 <- lme4::glmer(exinido ~ alt.esc+ vis.esc + huevos + (1| nido) , 
                        data = data, 
                        family = "binomial")

mod.4 <- lme4::glmer(exinido ~ prof.esc + vis.esc +long.esc + ancho.esc + huevos + (1| nido) , 
                     data = data, 
                     family = "binomial")

mod.5 <- lme4::glmer(exinido ~ prof.esc + huevos + (1| nido) , 
                     data = data, 
                     family = "binomial")


model.sel(mod.0, mod.1, mod.2, mod.3, mod.4, mod.5, rank = "AICc")  


# # # # # # # # # # # # # # # # # # # # # # # #

### 4) Inferencia de modelos  ####

# # # # # # # # # # # # # # # # # # # # # # # #

MA <- model.avg(mod.3, mod.2, mod.5, beta = F, revised.var = TRUE)
summary(MA)
coef(MA)
confint(MA)



# # # # # # # # # # # # # # # # # # # # # # # #

# 5) Modelo mínimo adecuado ####  <<<<<< ¿Hay q ponerlo?

# # # # # # # # # # # # # # # # # # # # # # # #

mod.5final <- lme4::glmer(exinido ~ huevos + (1| nido) , 
                     data = data, 
                     family = "binomial")

summary(mod.5final)







# # # # # # # # # # # # # # # # # # # # # # # #

##### OJO! Si huevos fuera categorico podría seguir el análisis:

library(multcomp)
levels(data$huevos)
Cont1 <- glht(mod.5final, linfct = mcp(orden = "Tukey"))
summary(Cont1)

