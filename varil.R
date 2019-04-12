# # # # Trabajo Final  
# # Curso Diamante:
# # "Topicos especiales en modelos lineales generalizados"


# # # # # # # # # # # # # # # # # # # # # # # #

# 1) Carga de librerias y de datos #### 

# # # # # # # # # # # # # # # # # # # # # # # #

library(glmmADMB)

data <- read.csv("C:\\Users\\solre\\Desktop\\ModelosMixtos\\varil.csv", sep = ";", dec=",")

str(data)
data$exinido <- as.factor(data$exinido)  
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


ggplot(data) + 
  geom_bar(aes( x = exinido)) + 
  theme_bw() +
  labs( title = " N de exitos - fracasos")

ggplot(data) + 
  geom_bar(aes( x = exinido)) + 
  facet_grid(~ huevos ) + 
  theme_bw() +
  labs( title = " N de exitos - fracasos por huevos")


ggplot(data) + 
  geom_freqpoly(aes( x = alt, col = exinido)) + 
  theme_bw() 

# # # # # # # # # # # # # # # # # # # # # # # #

# 3) Definimos el modelo general  ####

# # # # # # # # # # # # # # # # # # # # # # # #

# Por el diseño experimental tenemos un MGLMM binomial, con variable aleatoria "nido"


data.escala <- scale(data[,2:6])
data.escala <- as.data.frame(data.escala)
names(data.escala) <- c("long.esc",  "ancho.esc", "prof.esc", "alt.esc", "vis.esc"  )
data <- cbind(data, data.escala)

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

mod.2 <- lme4::glmer(exinido ~  alt.esc + vis.esc + huevos + (1| nido) , 
                        data = data, 
                        family = "binomial")

mod.3 <- lme4::glmer(exinido ~ prof.esc+ vis.esc + long.esc + ancho.esc + huevos + (1| nido) , 
                        data = data, 
                        family = "binomial")

mod.4 <- lme4::glmer(exinido ~ prof.esc + alt.esc + long.esc + ancho.esc + huevos + (1| nido) , 
                     data = data, 
                     family = "binomial")



model.sel(mod.0, mod.1, mod.2, mod.3, mod.4, rank = "AICc")  


# # # # # # # # # # # # # # # # # # # # # # # #

### 4) Inferencia de modelos  ####

# # # # # # # # # # # # # # # # # # # # # # # #

MA <- model.avg(mod.4, mod.2, beta = F, revised.var = TRUE)
summary(MA)
coef(MA)
confint(MA)




#  Modelo mínimo adecuado  ####

