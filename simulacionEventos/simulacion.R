set.seed(123)

# Demandas
demandaBoda <- c(7,6,4,5,4,4,3,3,1,1,4,9)
demandaCumple <- c(5,5,4,4,3,3,4,4,3,3,3,5)
demandaGraduacion <- c(1,0,0,0,0,3,3,2,0,0,1,10)

# Precios de paquetes por evento
precioBoda <- c(6000000, 12000000, 30000000)
precioCumple <- c(4000000, 8000000, 20000000)
precioGraduacion <- c(0, 10000000, 25000000)

# Costos
costos <- c(2760000, 4629000, 10246000)
impuestos <- 6666666
inversionInicial <- 330000000

ganancias <- 0
ganancias <- ganancias - inversionInicial

for (mes in 1:12) {
  for (evento in 1:demandaBoda[mes]) {
    salon <- sample(c(1,2,3), 1, T, c(0.2, 0.5, 0.3))
    ganancias <- ganancias + (precioBoda[salon] - costos[salon])
  }
  print(ganancias)
  for (evento in 1:demandaCumple[mes]) {
    salon <- sample(c(1,2,3), 1, T, c(0.6, 0.3, 0.1))
    ganancias <- ganancias + (precioCumple[salon] - costos[salon])
  }
  print(ganancias)
  for (evento in 1:demandaGraduacion[mes]) {
    salon <- sample(c(2,3), 1, T, c(0.3, 0.7))
    ganancias <- ganancias + (precioGraduacion[salon] - costos[salon])
  }
  print(ganancias)
  
  ganancias <- ganancias - impuestos
}

