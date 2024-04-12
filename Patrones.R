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

### M O I R E ### Lorena Miranda

lineas.paralelas <- function(num_lineas, linea_largo, distancia_entre_lineas, angulo) {
  for (i in 1:num_lineas) {
    turtle_forward(linea_largo)
    
    turtle_up()
    turtle_backward(linea_largo)
    turtle_right(angulo)
    turtle_forward(distancia_entre_lineas)
    turtle_left(angulo)
    turtle_down()
  }
}

turtle_init(mode = "clip")
turtle_do({
  turtle_setpos(x = 1, y = 1)
  turtle_col("black")
  turtle_lwd(7)
  lineas.paralelas(250, 200, 2, 90)
  turtle_setpos(x = 1, y = 1)
  turtle_setangle(355)
  turtle_lwd(5)
  lineas.paralelas(500, 200, 2, 120)
})

#####PATRÓN DE MOIRÉ#######
frente <- 2
ángulo <- 1

turtle_do(for(j in 1:46) {
  turtle_setpos(1, 1)
  turtle_forward(frente)
  turtle_setangle(ángulo)
  frente = frente + 3
  ángulo = ángulo + 1
  if(frente > 138) {
    frente <- 2
    ángulo <- 90
    turtle_setpos(1, 1)
    turtle_setangle(90)
  }
})

turtle_do(for(k in 1:47) {
  turtle_setpos(1, 1)
  turtle_forward(frente)
  turtle_setangle(ángulo)
  frente = frente + 3
  ángulo = ángulo - 1
})#####PATRÓN DE MOIRÉ#######
frente <- 2
ángulo <- 1

turtle_do(for(j in 1:46) {
  turtle_setpos(1, 1)
  turtle_forward(frente)
  turtle_setangle(ángulo)
  frente = frente + 3
  ángulo = ángulo + 1
  if(frente > 138) {
    frente <- 2
    ángulo <- 90
    turtle_setpos(1, 1)
    turtle_setangle(90)
  }
})

turtle_do(for(k in 1:47) {
  turtle_setpos(1, 1)
  turtle_forward(frente)
  turtle_setangle(ángulo)
  frente = frente + 3
  ángulo = ángulo - 1
})

