# Configuración Inicial
# ======================
library(tidyverse)
library(dplyr)
Sys.setlocale("LC_TIME", "English")

# Lectura de data
# ================
data <- read.csv("../data/hotel_bookings_miss.csv", header=TRUE, stringsAsFactors=FALSE, sep=";")

# LIMPIEZA DE TABLA
# ==================
# Primero identificamos cuántos NAs existen en cada columna
length(data$hotel[is.na(data$hotel)])
length(data$is_canceled[is.na(data$is_canceled)])
length(data$lead_time[is.na(data$lead_time)])
length(data$arrival_date_year[is.na(data$arrival_date_year)])
length(data$arrival_date_month[is.na(data$arrival_date_month)])
length(data$arrival_date_week_number[is.na(data$arrival_date_week_number)])
length(data$arrival_date_day_of_month[is.na(data$arrival_date_day_of_month)])
length(data$stays_in_weekend_nights[is.na(data$stays_in_weekend_nights)])
length(data$stays_in_week_nights[is.na(data$stays_in_week_nights)])
length(data$adults[is.na(data$adults)])
length(data$children[is.na(data$children)])
length(data$babies[is.na(data$babies)])
length(data$meal[is.na(data$meal)])
length(data$country[is.na(data$country)])
length(data$market_segment[is.na(data$market_segment)])
length(data$distribution_channel[is.na(data$distribution_channel)])
length(data$is_repeated_guest[is.na(data$is_repeated_guest)])
length(data$previous_cancellations[is.na(data$previous_cancellations)])
length(data$previous_bookings_not_canceled[is.na(data$previous_bookings_not_canceled)])
length(data$reserved_room_type[is.na(data$reserved_room_type)])
length(data$assigned_room_type[is.na(data$assigned_room_type)])
length(data$booking_changes[is.na(data$booking_changes)])
length(data$deposit_type[is.na(data$deposit_type)])
length(data$agent[is.na(data$agent)])
length(data$company[is.na(data$company)])
length(data$days_in_waiting_list[is.na(data$days_in_waiting_list)])
length(data$customer_type[is.na(data$customer_type)])
length(data$adr[is.na(data$adr)])
length(data$required_car_parking_spaces[is.na(data$required_car_parking_spaces)])
length(data$total_of_special_requests[is.na(data$total_of_special_requests)])
length(data$reservation_status[is.na(data$reservation_status)])
length(data$reservation_status_date[is.na(data$reservation_status_date)])

# CRITERIOS DE LIMPIEZA:
# =======================
# 1. Aquellas filas con columnas relacionadas a la fechas que estén incompletas se eliminarán,
#    puesto que no se puede determinar una fecha por defecto
#    - arrival_date_[year/month/week_number/day_of_month]
#    - reservation_status_date
data.limpia <- data[!is.na(data$arrival_date_year),]
data.limpia <- data.limpia[!is.na(data.limpia$arrival_date_month),]
data.limpia <- data.limpia[!is.na(data.limpia$arrival_date_week_number),]
data.limpia <- data.limpia[!is.na(data.limpia$arrival_date_day_of_month),]

# 2. Si faltan datos de las personas alojadas se tomará el siguiente criterio:
#    - Si faltan adultos, por defecto se reemplazará con 1
#    - Si faltan niños o bebés, por defecto se reemplazará con 0
data.limpia$adults[is.na(data.limpia$adults)] = 1
data.limpia$children[is.na(data.limpia$children)] = 0
data.limpia$babies[is.na(data.limpia$babies)] = 0

# 3. Con respecto a las reservaciones, en determinados casos se aplicará el uso de media como:
#    - Tiempo de espera
#    - Dias en la lista de espera
lean_time.mean <- mean(data.limpia$lead_time, na.rm=TRUE)
days_in_waiting_list.mean <- mean(data.limpia$days_in_waiting_list, na.rm=TRUE)
data.limpia$lead_time[is.na(data.limpia$lead_time)] = lean_time.mean
data.limpia$days_in_waiting_list[is.na(data.limpia$days_in_waiting_list)] = days_in_waiting_list.mean
#    En otros serán reemplazados con 0
data.limpia$booking_changes[is.na(data.limpia$booking_changes)] = 0
data.limpia$stays_in_week_nights[is.na(data.limpia$stays_in_week_nights)] = 0
data.limpia$stays_in_weekend_nights[is.na(data.limpia$stays_in_weekend_nights)] = 0

# CASO DE ANÁLISIS
# =================
# a. ¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?
pedidos_por_tipo_de_hotel<-data.limpia%>%group_by(data.limpia$hotel)%>%
  dplyr::summarise(cantidad=n())
barplot(pedidos_por_tipo_de_hotel$cantidad,main="Reservas por tipo de hotel",xlab="Tipo de Hotel",ylab="Cantidad de reservas",col = c("green","blue"))

# b. ¿Está aumentando la demanda con el tiempo?
pedidos_por_anio_y_mes<-data.limpia%>%group_by(data.limpia$arrival_date_year,data.limpia$arrival_date_month)%>%
  dplyr::summarise(cantidad=n())
summary(pedidos_por_anio_y_mes)
tiempo_cantidad<-unite(pedidos_por_anio_y_mes, tiempo,c(1:2),  sep = ", ", remove = TRUE)

grafico<-barplot(tiempo_cantidad$cantidad,main = "J",xlab ="x",ylab ="u",names.arg = tiempo_cantidad$tiempo,las=2,cex.names = 0.7)

# c. ¿Cuándo se producen las temporadas de reservas: alta, media y baja?
pedidos_fechas<-data.limpia%>%group_by(year=data.limpia$arrival_date_year,month=data.limpia$arrival_date_month,day=data.limpia$arrival_date_day_of_month)%>%dplyr::summarise(cantidad=n())
pedidos_fechas_con_formato<-unite(pedidos_fechas, fecha,c(1:3),  sep = " / ", remove = TRUE)

estaciones_pedidos<-pedidos_fechas%>%group_by(year,month,day)%>%
  dplyr::summarise(cantidad,estacion=ifelse(month=="March",ifelse(day>=21,"Primavera","Invierno"),ifelse(month=="April"||month=="May","Primavera",ifelse(month=="June",ifelse(day>=21,"Verano","Primavera"),ifelse(month=="July"||month=="August","Verano",ifelse(month=="September",ifelse(day>=21,"Otoño","Verano"),ifelse(month=="October"||month=="November","Otoño",ifelse(month=="December",ifelse(day>=21,"Invierno","Otoño"),ifelse(month=="February"||month=="January","Invierno","error")))))))))


estaciones_pedidos_agrupado<-estaciones_pedidos%>%group_by(estacion)%>%dplyr::summarise(cantidad=sum(cantidad))
barplot(estaciones_pedidos_agrupado$cantidad,main="Reservas por estacion",xlab="Estacion",ylab="Reservas",names.arg = estaciones_pedidos_agrupado$estacion)

# d. ¿Cuándo es menor la demanda de reservas?
pedidos_por_anio_y_mes%>%group_by(pedidos_por_anio_y_mes$`data.limpia$arrival_date_year`,pedidos_por_anio_y_mes$`data.limpia$arrival_date_month`)%>%
  dplyr::summarise(ifelse(cantidad==min(cantidad),cantidad,0))

menor<-pedidos_por_anio_y_mes[pedidos_por_anio_y_mes$cantidad==min(pedidos_por_anio_y_mes$cantidad),]
menor<-unite(menor,tiempo,c(1:2),sep=", ",remove = TRUE)
barplot(menor$cantidad,main="Menor reserva",xlab ="Tiempo",ylab="Reservas",names.arg = menor$tiempo)

# e. ¿Cuántas reservas incluyen niños y/o bebés?
reservas_children_babies<-data.limpia[data.limpia$children!=0 | data.limpia$babies!=0,]

df<-data.frame(tipo="Children or Babies reservation",cantidad=count(reservas_children_babies))
barplot(df$n,main="Reservas con niños o bebes",xlab="Tipo",ylab = "Reservas",names.arg = df$tipo)

# f. ¿Es importante contar con espacios de estacionamiento?
reservas_espacio<-data.limpia[data.limpia$required_car_parking_spaces!=0,]
reservas_sin_espacio<-data.limpia[data.limpia$required_car_parking_spaces==0,]
no_spaces<-count(reservas_sin_espacio)
spaces<-count(reservas_espacio)
df_d<-data.frame(tipo=c("Con espacio de estacionamiento","Sin espacio de estacionamiento"),cantidad=c(spaces$n,no_spaces$n))

barplot(df_d$cantidad,main="Reservas por espacio de estacionamiento",xlab="Tipo",ylab="Reservas",names.arg = df_d$tipo)

# g. ¿En qué meses del año se producen más cancelaciones de reservas?
rm(cancelaciones_por_anio_y_mes)
cancelados<-data.limpia[data.limpia$is_canceled==1,]
cancelaciones_por_anio_y_mes<-cancelados%>%group_by(cancelados$arrival_date_year,cancelados$arrival_date_month)%>%
  dplyr::summarise(cantidad=n())
mas_cancelaciones<-cancelaciones_por_anio_y_mes[cancelaciones_por_anio_y_mes$cantidad==max(cancelaciones_por_anio_y_mes$cantidad),]
mas_cancelaciones<-unite(mas_cancelaciones,Fecha,c(1:2),sep = " / ",remove=TRUE)
barplot(mas_cancelaciones$cantidad,main = "Mas cancelaciones",xlab = "Cancelados",ylab = "Cancelaciones",names.arg = mas_cancelaciones$Fecha)


##############SECTOR VS TIPO DE HABITACION

sector_tipo<-data.limpia%>%group_by(market_segment,reserved_room_type)%>%
  dplyr::summarise(cantidad=mean(lead_time))

ggplot(data = sector_tipo) +
  geom_point(mapping = aes(x = market_segment, y = cantidad, color = reserved_room_type,cex.names=0.5))


##################CANCELACION VS TIPO DE HABITACION
cancelado_por_habitacion<-cancelados%>%group_by(reserved_room_type)%>%
  dplyr::summarise(promedio_estadia=mean(lead_time))

barplot(cancelado_por_habitacion$promedio_estadia,main="Promedio estadia por tipo de habitación de reserva cancelada",xlab = "Tipo habitación",ylab="Promedio estadia",names.arg = cancelado_por_habitacion$reserved_room_type)

#####################RESERVAS CON NIÑOS EN RESORT VS CIUDAD 

reservas_children<-data.limpia[data.limpia$children!=0,]
reservas_por_hotel_children<-reservas_children%>%group_by(ï..hotel)%>%
  dplyr::summarise(cantidad=n())
barplot(reservas_por_hotel_children$cantidad,main="Reservas con niños por tipo de hotel",xlab = "Tipo de hotel",ylab="Cantidad de reservas",names.arg = reservas_por_hotel_children$ï..hotel)

###################TIEMPO ESTADIA CON NIÑOS VS SIN NIÑOS

reservas_sin_cb<-data.limpia[data.limpia$children==0|data.limpia$babies==0,]
sin_cb<-mean(reservas_sin_cb$lead_time)
con_cb<-mean(reservas_children_babies$lead_time)
df_vs_cb<-data.frame(tipo=c("Con niños o bebes","Sin niños ni bebes"),cantidad=c(con_cb,sin_cb))
barplot(df_vs_cb$cantidad,main = "Tiempo estadia con niños y/o bebes vs sin niños y/o bebes",ylab = "Estadia promedio",names.arg=df_vs_cb$tipo)