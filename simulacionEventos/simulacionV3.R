# Matrices de reservas
reservasBodas <- matrix(0,3,12)
reservasCumple <- matrix(0,3,12)
reservasGraduacion <- matrix(0,3,12)

# Precios de paquetes por evento (cambiando precios de trabajo escrito)
precioBoda <- c(6000000, 10000000, 17000000)
precioCumple <- c(4000000, 6000000, 12000000)
precioGraduacion <- c(0, 8000000, 15000000)

# Costos
costos <- c(2760000, 4629000, 10246000)
impuestos <- 3208333+4583333+6875000
inversionInicial <- 330000000

ganancias <- 0
ganancias <- ganancias - inversionInicial

for (mes in 1:12) {
  num_reservas <- rpois(1,10)
  print(num_reservas)
  for (evento in 1:num_reservas) {
    tamano_salon <- sample(c(1,2,3), 1, T, c(0.3, 0.4, 0.3))
    # Boda, Cumple, Graduacion
    probabilidadesDeEvento <- c(5,5,5)
    mesReserva <- 0
    if (tamano_salon == 1) {
      mesReserva <- sample(c(1,2), 1, T)
      probabilidadesDeEvento[2] <- probabilidadesDeEvento[2] + 2
      probabilidadesDeEvento[1] <- probabilidadesDeEvento[1] + 1
      probabilidadesDeEvento[3] <- 0
      # Aquí se ajustaría la proporción de cada tipo de evento según el mes
    } else if (tamano_salon == 2) {
      mesReserva <- sample(c(3,4,5,6), 1, T)
      probabilidadesDeEvento[1] <- probabilidadesDeEvento[1] + 2
      probabilidadesDeEvento[2] <- probabilidadesDeEvento[2] + 1
      # Aquí se ajustaría la proporción de cada tipo de evento según el mes
    } else {
      mesReserva <- sample(c(9,10,11), 1, T)
      probabilidadesDeEvento[1] <- probabilidadesDeEvento[1] + 1
      probabilidadesDeEvento[2] <- probabilidadesDeEvento[2] - 1
      probabilidadesDeEvento[3] <- probabilidadesDeEvento[3] + 2
      # Aquí se ajustaría la proporción de cada tipo de evento según el mes
    }
    
    mesReserva <- ((mesReserva + mes) %% 12) + 1
    
    # Aquí se revisa si se puede reservar el salón en el mes solicitado
    
    probabilidadesDeEvento <- probabilidadesDeEvento / sum(probabilidadesDeEvento)
    tipoEvento <-  sample(c(1,2,3), 1, T, probabilidadesDeEvento)
    
    if (tipoEvento == 1) {
      reservasBodas[tamano_salon, mesReserva] <- reservasBodas[tamano_salon, mesReserva] + 1
    } else if (tipoEvento == 2) {
      reservasCumple[tamano_salon, mesReserva] <- reservasCumple[tamano_salon, mesReserva] + 1
    } else {
      reservasGraduacion[tamano_salon, mesReserva] <- reservasGraduacion[tamano_salon, mesReserva] + 1
    }
    
    ganancias <- ganancias + ((precioBoda[1] - costos[1]) * reservasBodas[1, mes])
    ganancias <- ganancias + ((precioBoda[2] - costos[2]) * reservasBodas[2, mes])
    ganancias <- ganancias + ((precioBoda[3] - costos[3]) * reservasBodas[3, mes])
    ganancias <- ganancias + ((precioCumple[1] - costos[1]) * reservasCumple[1, mes])
    ganancias <- ganancias + ((precioCumple[2] - costos[2]) * reservasCumple[2, mes])
    ganancias <- ganancias + ((precioCumple[3] - costos[3]) * reservasCumple[3, mes])
    ganancias <- ganancias + ((precioGraduacion[1] - costos[1]) * reservasGraduacion[1, mes])
    ganancias <- ganancias + ((precioGraduacion[2] - costos[2]) * reservasGraduacion[2, mes])
    ganancias <- ganancias + ((precioGraduacion[3] - costos[3]) * reservasGraduacion[3, mes])
    
  }
  ganancias <- ganancias - impuestos
  print(ganancias)
}

