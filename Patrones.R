# Patrón de Moire - Ale
library(grid)
library(TurtleGraphics)
patron <- function(longitud) {
  if (longitud > 0) {
    turtle_forward(longitud)
    turtle_right(90)
    patron(longitud-3)
  }
  invisible(NULL)
}
turtle_init(500, 500, mode="cycle")
turtle_do({
  turtle_setpos(x=0, y=0)
  turtle_param(col="blue", lwd=2)
  patron(490)
  turtle_setpos(x=1, y=1)
  turtle_param(col="green",lwd=2)
  patron(500)
})

# Patrón en espiral - Ale

library(grid)
library("TurtleGraphics")

espiral <- function(longitud_linea) {
  if (longitud_linea > 0) {
    turtle_forward(longitud_linea)
    turtle_right(50)
    espiral(longitud_linea-5)
  }
  invisible(NULL)
}
turtle_init(500, 500, mode="clip")
turtle_do({
  turtle_setpos(x=0, y=0)
  turtle_param(col="orange", lwd=3)
  espiral(300)
})


#Eduardo Arturo Aguilar Herrera
#Estadistica General en R


###patrón espiral###

install.packages("turtleGraphics")
library(TurtleGraphics)


turtle_init()
for(i in 1:360) {
  turtle_forward(i)
  turtle_right(45)
}


###patrón de Moire###

turtle_init()

for(i in 1:500) {
  turtle_forward(i)
  turtle_right(75)
}

##########################################
# Santiago Garcia Rios

# Taller de estadística general usando R
# Tarea Módulo 2 - escribir un programita que genere un patrón espiral y otro que genere un patrón de Moire

# ---- Librerías ----
library(TurtleGraphics)

# ---- Función Espiral ----

espiral_tortuga <- function(n_iteraciones, paleta_colores) {
  # Crear plot nuevo con tortuga en el centro
  turtle_init(width = 1000, height = 1000, mode = "clip")
  # Colores para la espiral con n_iteraciones de colores en el gradiente
  colores <- colorRampPalette(paleta_colores)(n_iteraciones)
  # Turtle_do esconde la tortuga hasta que termina el plot (eficiencia)
  turtle_do({
    for (i in 1:n_iteraciones) {
      # Mover adelante para extender la espirál
      turtle_forward(5 + i/10)
      # reduce el ángulo de rotación para crear espirál
      turtle_right(30 - i / 36)
      # usar color distinto en cada iteración
      turtle_col(colores[i])
      # Aumenta el grosor del trazo con cada iteración
      turtle_lwd(lwd = 1 + i/180)
    }
  })
}

# Uso de función
espiral_tortuga(720, c("#070F2B", "#1B1A55", "#535C91", "#9AD0C2"))
espiral_tortuga(320, c("#240A34", "#891652", "#EABE6C", "#FFEDD8"))
espiral_tortuga(800, c("#08D9D6", "#252A34", "#FF2E63", "#71C9CE", "#3F72AF", "#112D4E", "#222831"))


# ---- Función Moiré ----

tortuga_moire <- function(n_iteraciones, incremento_angulo) {
  # Validar que número de iteraciones esté entre 1000&7000 para crear un patrón de moiré bonito
  if (!is.numeric(n_iteraciones) || n_iteraciones < 1000 || n_iteraciones > 7000) {
    stop("n_iteraciones debe ser un valor entre 1000 y 7000.")
  }
  # Validar que incremento de ángulo esté entre 0.3 & 0.9 para crear un patrón de moiré bonito
  if (!is.numeric(incremento_angulo) || incremento_angulo < 0.3 || incremento_angulo > 0.9) {
    stop("incremento_angulo debe ser un valor entre 0.3 y 0.9.")
  }
  
  turtle_init(100, 100, mode = "clip")
  
  turtle_do({
    for (i in 1:n_iteraciones) {
      turtle_setpos(0, 0)
      # Gradiente de colores cambia cada iteración
      turtle_col(col = rainbow(n_iteraciones)[i])
      turtle_forward(100 + i/70) # Mover adelante ligeramente con cada iteración
      turtle_left(incremento_angulo) # Cambiar ligeramente ángulo
    }
  })
}


# Uso de función
tortuga_moire(7000, 0.33)
tortuga_moire(7000, 0.8)
tortuga_moire(1000, .9)
tortuga_moire(300, .4) # no permitido
tortuga_moire(2000, 12) # no permitido

#############################################
