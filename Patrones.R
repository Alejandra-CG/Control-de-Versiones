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