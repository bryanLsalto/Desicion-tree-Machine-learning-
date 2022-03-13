# dts

### Descripción
_El siguiente programa te permite leer una matriz de datos qualquiera y te calcula su arbol de decisión. Después también puedes classificar un nuevo elemento según el arbol obtenido por la base de datos._


### Pre-requisitos

Que cosas necesitas para ejecutar el siguiente codigo


_1. Una matriz con las columnas separadas por espacios y las filas por salto de pàgina._

_2. Conocer que analiza cada columna y que opciones tiene cada columa, para después entender el arbol y poder classifcar un nuevo dato._


### Instalación

_Para ejecutar el programa con un conjuto de tados en especifico se  tiene que modificar el archivo de entrada en el main del codigo, la primera linea del main_


Modificar la siguiente parte en el codigo:

```
text <- readFile "DatosqueInsertar"
```

_Para compilar_

```
ghc dts.hs
```
## Uso

La mera de utilizar el progra es:

_1. Ejecutar el programa_

_2. Esperar a que se caulcule el arbol, puede tardar unos pocos minutos, depende de la entrada.(Futura optimización)_ 


_3. Una vez se ha calculado todo el arbol se mostrara en pantalla, despues pudedes clasificar un nuevo dato._

_4. Seguir las instrucciones que te indica la terminal._

## Construido con 🛠️

_Haskell_

## Autores

* **Bryan Leonardo Salto Salao** - *FIB IPC* - *Barcelona*

## Creditos

* **Para calcular la heuristical** - *Articulo* - [Machine Learning Basics: Decision Tree From Scratch](https://towardsdatascience.com/machine-learning-basics-descision-tree-from-scratch-part-ii-dee664d46831)



---
_Bryan Leonardo Salto Salao_
