# Mariela Valerio Sáenz C18049
# Juan Carlos Sequeira Jiménez B68330
# Roy Muñoz Miranda B54911
# Isaac Madrigal Silva C14387

set.seed(123)

# Precios de paquetes por evento
precioBoda <- c(6000000, 10000000, 17000000)
precioCumple <- c(4000000, 6000000, 12000000)
precioGraduacion <- c(0, 8000000, 15000000)

# Costos
costos <- c(2760000, 4629000, 10246000)
impuestos <- 3208333 + 4583333 + 6875000

numLlegadas <- c(5,10,20)

# Matrices de reservas
reservasBodas <- matrix(0,3,12)
reservasCumple <- matrix(0,3,12)
reservasGraduacion <- matrix(0,3,12)

totalBodas <- matrix(0,3,12)
totalCumple <- matrix(0,3,12)
totalGraduacion <- matrix(0,3,12)

# Vectores de probabilidad por mes
ajusteBodas <- c(4,4,0,0,0,0,0,0,-4,-4,0,4)
ajusteGraduacion <- c(-2,-4,-4,-4,-4,0,0,-2,-4,-4,-2,4)

inversionInicial <- 330000000
ganancias <- 0


simulacion <- function(numClientes) {
  # Se inicializan los datos
  ganancias <<- inversionInicial * -1
  reservasBodas[] <<- 0
  reservasCumple[] <<- 0
  reservasGraduacion[] <<- 0
  totalBodas[] <<- 0
  totalCumple[] <<- 0
  totalGraduacion[] <<- 0
  
  # Se calculan las ganancias de 3 años
  for (año in 1:3) {
    simularAño(numClientes)
  }
}


# Calcula las ganancias por 12 meses
simularAño <- function(numClientes) {
  for (mes in 1:12) {
    calcularGanancias(mes)

    num_reservas <- rpois(1,numClientes)
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
  
  # Cada mes, se le restan los impuestos a las ganancias
  ganancias <<- ganancias - impuestos
}


# Intenta reservar un evento, recibe el mes actual
reservar <- function(mes) {
  pesoEventos <- c(5,5,5)  # Boda, Cumple, Graduacion
  mesReserva <- 0
  # Se determina el tamño del salón que será reservado
  tamano_salon <- sample(c(1,2,3), 1, T, c(0.3, 0.4, 0.3))
  
  # Se determina el mes en el que el salón será reservado
  if (tamano_salon == 1) {
    mesReserva <- sample(c(1,2), 1, T)
  } else if (tamano_salon == 2) {
    mesReserva <- sample(c(3,4,5,6), 1, T)
  } else {
    mesReserva <- sample(c(9,10,11,12), 1, T)
  }
  mesReserva <- ((mesReserva + mes) %% 12) + 1

  # Se revisa si se puede reservar el salón en el mes solicitado
  # Cada salón puede ser reservado un máximo de 4 veces por mes
  if (reservasBodas[tamano_salon, mesReserva] +
      reservasGraduacion[tamano_salon, mesReserva] +
      reservasCumple[tamano_salon, mesReserva] == 4) {  
    return (NULL)  # Si no se puede reservar, se pierde el cliente
  }

  # Se ajusta la proporción de cada tipo de evento según el mes
  pesoEventos[1] <- pesoEventos[1] + ajusteBodas[mesReserva]
  pesoEventos[3] <- pesoEventos[3] + ajusteGraduacion[mesReserva]

  # Se ajusta la proporción de cada tipo de evento según el tamaño del salón
  if (tamano_salon == 1) {
    pesoEventos[2] <- pesoEventos[2] + 3  # Más probable de que sea cumpleaños
    pesoEventos[3] <- 0                   # No puede ser graduación
  } else if (tamano_salon == 2) {
    pesoEventos[1] <- pesoEventos[1] + 3  # Aún más probable de que sea boda
    pesoEventos[2] <- pesoEventos[2] + 1  # Más probable de que sea cumpleaños
  } else {
    pesoEventos[1] <- pesoEventos[1] + 1  # Más probable de que sea boda
    pesoEventos[2] <- pesoEventos[2] - 1  # Menos probable de que sea cumpleaños
    pesoEventos[3] <- pesoEventos[3] + 2  # Aún más probable de que sea graduación
  }

  # Se determina el tipo de evento
  tipoEvento <-  sample(c(1,2,3), 1, T, pesoEventos)
  
  # Se le suma 1 a la cantidad de reservas
  if (tipoEvento == 1) {
    reservasBodas[tamano_salon, mesReserva] <<- reservasBodas[tamano_salon, mesReserva] + 1
    totalBodas[tamano_salon, mesReserva] <<- totalBodas[tamano_salon, mesReserva] + 1
  } else if (tipoEvento == 2) {
    reservasCumple[tamano_salon, mesReserva] <<- reservasCumple[tamano_salon, mesReserva] + 1
    totalCumple[tamano_salon, mesReserva] <<- totalCumple[tamano_salon, mesReserva] + 1
  } else {
    reservasGraduacion[tamano_salon, mesReserva] <<- reservasGraduacion[tamano_salon, mesReserva] + 1
    totalGraduacion[tamano_salon, mesReserva] <<- totalGraduacion[tamano_salon, mesReserva] + 1
  }
}

hist(totalBodas[1])


simulacion(numLlegadas[3])
