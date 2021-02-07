# Detección de Fraudes en Seguros de Autóviles
Proyecto de la fase dos de BEDU - Data Science

## Objetivo del Proyecto
Se busca investigar y crear diferentes modelos de aprendizaje máquina para predecir fraudes en automóviles y así poder prevenirlos y lograr una reducción en los costos de operación de las aseguradoras, misma que debería de reflejarse en una reducción de las primas, beneficiando así a los asegurados.

El fraude es un problema en la actualidad. Ha mantenido su ritmo de crecimiento constante en los últimos años y a pesar de todas las tecnologías existentes, no se ha podido disminuir su crecimiento y se hace cada vez más difícil su detección, pues los esquemas utilizados por los defraudadores siguen evolucionando.

## Modelos a evaluar 
En este proyecto, se utilizaron diferentes métodos de clasificación:
- MODELO C5.0: Este método es uno de los más utilizados en el ámbito de los árboles de clasificación. Este algoritmo crea modelos de árbol de clasificación, permitiendo sólo variables de salida categórica. Las variables de entrada pueden ser de naturaleza continua o categórica.
- REGRESIÓN LOGÍSTICA: Sabemos que, en este caso, la regresión logística tiene las ventajas de tiene bajo consumo de recursos de un ordenador, tiene una fácil interpretación y una buena eficiencia y simplicidad.
- Neural Networks Model: se refiere a un conjunto de redes neuronales que tienen como objetivo resolver problemas difíciles mediante algoritmos convencionales. Dentro de dicho conjunto, en cada una de ellas podemos distinguir una entrada de señal, un nodo y una salida o respuesta hacia otra neurona artificial.

## Notas del código
En la carpeta "Codigo" se encuentran el archivo principal "main.R" que se utilizó para la creación del proyecto, sin embargo, anteriormente se realizaron pruebas y código variado que nos llevó al principal, lo cual, se presenta en la carpeta "Testing". 

Por otro lado, el archivo "environment.RData" se presenta el ambiente que se utilizó para la creación del mismo, debido a que el modelo Neural Networks es muy caro computacionalmente.
