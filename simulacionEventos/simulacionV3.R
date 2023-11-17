set.seed(123)

# Precios de paquetes por evento (cambiando precios de trabajo escrito)
precioBoda <- c(6000000, 10000000, 17000000)
precioCumple <- c(4000000, 6000000, 12000000)
precioGraduacion <- c(0, 8000000, 15000000)

# Costos
costos <- c(2760000, 4629000, 10246000)
impuestos <- 3208333 + 4583333 + 6875000  # Se cambian los del trabajo

# Matrices de reservas
reservasBodas <- matrix(0,3,12)
reservasCumple <- matrix(0,3,12)
reservasGraduacion <- matrix(0,3,12)

inversionInicial <- 330000000
ganancias <- 0


simulacion <- function() {
  ganancias <<- inversionInicial * -1
  reservasBodas[] <<- 0
  reservasCumple[] <<- 0
  reservasGraduacion[] <<- 0
  
  for (año in 1:1) {
    simularAño()
  }
}


simularAño <- function() {
  for (mes in 1:12) {
    calcularGanancias(mes)

    num_reservas <- rpois(1,10)
    for (evento in 1:num_reservas) {
      reservar(mes)
    }
  }
}


# Calcula las ganancias de un mes, recibe el mes actual
calcularGanancias <- function(mes) {
  for (salon in 1:3) {
    ganancias <<- ganancias + ((precioBoda[salon] - costos[salon]) * reservasBodas[salon, mes])
    ganancias <<- ganancias + ((precioCumple[salon] - costos[salon]) * reservasCumple[salon, mes])
    ganancias <<- ganancias + ((precioGraduacion[salon] - costos[salon]) * reservasGraduacion[salon, mes])
    reservasBodas[salon, mes] <<- 0
    reservasCumple[salon, mes] <<- 0
    reservasGraduacion[salon, mes] <<- 0
  }
  
  ganancias <<- ganancias - impuestos
}


# Intenta reservar un evento, recibe el mes actual
reservar <- function(mes) {
  tamano_salon <- sample(c(1,2,3), 1, T, c(0.3, 0.4, 0.3))
  probEventos <- c(5,5,5)  # Boda, Cumple, Graduacion
  mesReserva <- 0
  
  if (tamano_salon == 1) {
    mesReserva <- sample(c(1,2), 1, T)
  } else if (tamano_salon == 2) {
    mesReserva <- sample(c(3,4,5,6), 1, T)
  } else {
    mesReserva <- sample(c(9,10,11,12), 1, T)
  }
  
  mesReserva <- ((mesReserva + mes) %% 12) + 1

  # Aquí se revisa si se puede reservar el salón en el mes solicitado
  
  # Aquí se ajustaría la proporción de cada tipo de evento según el mes

  # Se ajusta la proporción de cada tipo de evento según el tamaño del salón
  if (tamano_salon == 1) {
    probEventos[1] <- probEventos[1] + 1  # Más probable de que sea boda
    probEventos[2] <- probEventos[2] + 2  # Aún más probable de que sea cumpleaños
    probEventos[3] <- 0  # No puede ser graduación
  } else if (tamano_salon == 2) {
    probEventos[1] <- probEventos[1] + 2  # Aún más probable de que sea boda
    probEventos[2] <- probEventos[2] + 1  # Más probable de que sea cumpleaños
  } else {
    probEventos[1] <- probEventos[1] + 1  # Más probable de que sea boda
    probEventos[2] <- probEventos[2] - 1  # Menos probable de que sea cumpleaños
    probEventos[3] <- probEventos[3] + 2  # Aún más probable de que sea graduación
  }


  probEventos <- probEventos / sum(probEventos)
  tipoEvento <-  sample(c(1,2,3), 1, T, probEventos)
  
  # Se le suma 1 a la cantidad de reservas
  if (tipoEvento == 1) {
    reservasBodas[tamano_salon, mesReserva] <<- reservasBodas[tamano_salon, mesReserva] + 1
  } else if (tipoEvento == 2) {
    reservasCumple[tamano_salon, mesReserva] <<- reservasCumple[tamano_salon, mesReserva] + 1
  } else {
    reservasGraduacion[tamano_salon, mesReserva] <<- reservasGraduacion[tamano_salon, mesReserva] + 1
  }
}


simulacion()
