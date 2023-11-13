set.seed(123)

#Calcula la ganancia de un mes para un tipo de evento en especifico
MostrarDatos <- function(tipoEvento, demanda, precios, costos, salonProb) {
  cat("Tipo de evento:", tipoEvento, "\n")
  cat("Demanda:", demanda, "\n")
  cat("Precios:", precios, "\n")
  cat("Costos:", costos, "\n")
  
  gananciaPorMes <- CalcularGanancias(demanda, precios, costos, salonProb)
  
  cat("Ganancias:", gananciaPorMes, "\n\n")
  return(gananciaPorMes)
}

#Calcula qué salón se utlizará para un evento y retorna la ganancia restando los costos
CalcularGanancias <- function(demanda, precios, costos, salonProb) {
  ganancia <- 0
  for (evento in 1:demanda) {
    #Utiliza la distribución recibida por parametro para determinar el salon
    salon <- sample(1:length(precios), 1, TRUE, prob = salonProb)
    
    ganancia <- ganancia + (precios[salon] - costos[salon])
  }
  return(ganancia)
}

#Abre un archivo recibido por parametro y asigna los datos


#Extrae los datos de un archivo .csv de: demanda, precios y probabilidades 
#de usar cada salon segun el tipo de evento
demandaData <- read.table("demanda_data.csv", header = TRUE, sep = ",")
preciosData <- read.table("precios_data.csv", header = TRUE, sep = ",")
salonProbData <- read.table("salon_prob_data.csv", header = TRUE, sep = ",")


demandaBoda <- demandaData$demandaBoda
demandaCumple <- demandaData$demandaCumple
demandaGraduacion <- demandaData$demandaGraduacion

# Precios de paquetes por evento (cambiando precios de trabajo escrito)
precioBoda <- preciosData$precioBoda
precioCumple <- preciosData$precioCumple
precioGraduacion <- preciosData$precioGraduacion

#Distribucion de probabilidad de utilizar un salon segun el evento
salonProbBoda <- salonProbData$salonProbBoda
salonProbCumple <- salonProbData$salonProbCumple
salonProbGraduacion <- salonProbData$salonProbGraduacion


# Costos
costos <- c(2760000, 4629000, 10246000)
impuestos <- 3208333+4583333+6875000
inversionInicial <- 330000000

gananciaTotal <- 0
gananciaTotal <- gananciaTotal - inversionInicial

for (mes in 1:12) {
  
  cat("----Simulando datos para mes ", mes, "----\n")
  
  gananciasBoda <- MostrarDatos("Boda",demandaBoda[mes], precioBoda, costos, salonProbBoda)
  gananciasCumple <- MostrarDatos("Cumpleanos",demandaCumple[mes], precioCumple, costos, salonProbCumple)
  gananciasGraduacion <- MostrarDatos("Graduacion",demandaGraduacion[mes], precioGraduacion, costos, salonProbGraduacion)
  
  gananciaTotal <- gananciaTotal + gananciasBoda + gananciasCumple + gananciasGraduacion - impuestos
}
