data <- read.table("C:/Users/Windows/Documents/College/8th semester/SimS/Informe/Registro_bd.csv", header=TRUE, sep=",")

x<-data$tiempo_de_viaje
plotdist(x,histo=TRUE,demp=TRUE)
summary(x)

descdist(x,boot=10000)


fit_x_n<-fitdist(x,"norm")
plot(fit_x_n)
summary(fit_x_n)



> fit_x_u<-fitdist(x,"unif")
> plot(fit_x_u)
> gofstat(list(fit_x_n,fit_x_u))

par(mfrow=c(2,2))
> plot.legend<-c("Normal","Uniforme")
> denscomp(list(fit_x_n,fit_x_u),legendtext = plot.legend)
> cdfcomp(list(fit_x_n,fit_x_u),legendtext = plot.legend)
> qqcomp(list(fit_x_n,fit_x_u),legendtext = plot.legend)
> ppcomp(list(fit_x_n,fit_x_u),legendtext = plot.legend)
> summary(fit_x_u)

summary(fit_x_u)

y<-data$tiempo_entre_llegadas_de_estudiante
> plotdist(y,histo=TRUE,demp=TRUE)
> summary(y)

descdist(y,boot=1000)
fit_y_gamma <- fitdist(y, "gamma")
fit_y_logistica <- fitdist(y, "logis")
fit_y_lognormal <- fitdist(y, "lnorm")
plot.legend<-c("Gamma","Logistica","Lognormal")
> denscomp(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal),legendtext = plot.legend)
Error en plot.new(): figure margins too large
> par(mfrow=c(2,2))  # Configurar 4 gráficos en una ventana
> par(mar=c(4, 4, 2, 2))  # Ajustar márgenes (abajo, izquierda, arriba, derecha)
> 
  > par(mfrow=c(2,2))
> plot.legend<-c("Gamma","Logistica","Lognormal")
> denscomp(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal),legendtext = plot.legend)
> cdfcomp(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal),legendtext = plot.legend)
> qqcomp(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal),legendtext = plot.legend)
> ppcomp(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal),legendtext = plot.legend)
> gofstat(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal))


gofstat(list(fit_y_gamma,fit_y_logistica,fit_y_lognormal))

summary(fit_y_gamma)

w<-data$tiempo_entre_llegada_y_partida_del_bus
> plot(w)
> plotdist(w,histo=TRUE,demp=TRUE)
> summary(w)

descdist(w,boot=1000)
fit_w_gamma <- fitdist(w, "gamma")
> fit_w_unif <- fitdist(w, "unif")
fit_w_logn <- fitdist(w, "lnorm")
> fit_w_norm <- fitdist(w, "norm")
> fit_w_exp <- fitdist(w, "exp")
> plot(fit_w_logn)
> plot(fit_w_norm)
> plot(fit_w_exp)
> par(mfrow=c(2,2))
> plot.legend<-c("Gamma","Uniforme","Lognorma","Normal","Exponencial")
> denscomp(list(fit_w_gamma,fit_w_unif,fit_w_logn,fit_w_norm,fit_w_exp),legendtext = plot.legend)
> cdfcomp(list(fit_w_gamma,fit_w_unif,fit_w_logn,fit_w_norm,fit_w_exp),legendtext = plot.legend)
> qqcomp(list(fit_w_gamma,fit_w_unif,fit_w_logn,fit_w_norm,fit_w_exp),legendtext = plot.legend)
> ppcomp(list(fit_w_gamma,fit_w_unif,fit_w_logn,fit_w_norm,fit_w_exp),legendtext = plot.legend)
> gofstat(list(fit_w_gamma,fit_w_unif,fit_w_logn,fit_w_norm,fit_w_exp))