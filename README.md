# Piratas del Caribe

_**¡Un grupo de piratas ha tomado la facultad! Y nos piden hacer un programa funcional para planear sus ataques y saqueos.
Tesoros piratas
Lo más importante para un pirata son los tesoros. Ellos saquean ciudades y tienen aventuras en busca de tesoros cada vez más valiosos.
Cada pirata tiene un apodo y un botín (conjunto de tesoros que ya posee). De los tesoros solo les importa su nombre y valor.**_

![piratas](piratas1.jpg)

### Piratas y tesoros
Se quiere saber
1. La cantidad de tesoros de un pirata
2. Si un pirata es afortunado, lo cual es cierto si el valor total de su botín supera los 10000.
3. Si dos piratas tienen un mismo tesoro, pero de valor diferente
4. El valor del tesoro más valioso de un pirata.
5. Como queda el pirata luego de adquirir un nuevo tesoro
6. Como queda el pirata luego de perder todos los tesoros valiosos, que son los que tienen un valor mayor a 100.
7. Como queda el pirata luego de perder todos los tesoros con un nombre dado.

Definir por ejemplo a los siguientes piratas
* Jack Sparrow: Su botín está compuesto por una brújula que apunta lo que más deseas valuada en 10000 y un frasco de arena valuado en 0.
* David Jones: Su único tesoro es una cajita musical valuada en 1.
* Anne Bonny: Su botín son doblones que valen 100 y un frasco de arena que vale 1.

### Temporada de saqueos
Los piratas trabajan saqueando, pero no siempre lo hacen de la misma forma. Existen manera de particular de elegir los tesoros que quieren saquear, que puede ser alguna de las siguientes:
* Sólo los tesoros valiosos.
* Tesoros con objetos específicos, es decir, sólo tesoros cuyo nombre sea una palabra dada.
* Existen los piratas con corazón que no saquean nada.
* Existe una forma más compleja que consiste en una conjunción de las anteriores. Esto significa que se quedan con los tesoros que cumplan al menos una de entre un conjunto de maneras se saquear.
  
1. Modelar las maneras de saqueo mencionadas y hacer la función saquear, que dado un pirata, una forma de saqueo y un tesoro, devuelve al pirata con el nuevo tesoro, en caso que sea de su preferencia. 
2. Probar diferentes combinaciones de piratas, tesoros y formas de saquear. Por ejemplo, con un mismo tesoro de oro valuado en 100, ver qué pasa si lo quiere saquear:

* Anne Bonny, considerando una manera de saquear que con consiste en buscar “oro”.
* David Jones, asumiendo que no le interese saquear nada (solo le importa el amor).
* Jack Sparrow, aplicando una forma de saquear que es una combinación de cosas valiosas y tesoros con nombre “sombrero”.
 
### Navegando los siete mares
Los piratas se juntan para armar una tripulación que viaja en barco recorriendo los mares en busca de nuevos tesoros. Cada barco se caracteriza por una manera de saquear, que utilizan todos sus tripulantes.

Hacer funciones para representar que:
1. Un pirata se incorpora a la tripulación de un barco
2. Un pirata abandona la tripulación de un barco

Por ejemplo:
* La tripulación del Perla Negra está compuesta por Jack Sparrow y Anne Bonny
* La tripulación del Holandés Errante está compuesta por David Jones.
* Elizabeth Swann, que posee una moneda del cofre muerto valuada en 100 y una espada de hierro que vale 50, se suma a la tripulación del Perla Negra.  
* Will Turner, con un cuchillo que le regaló el padre valuado en 5, también se sube al Perla Negra, pero luego abandona el barco.

Mientras va navegando, a un barco le pueden pasar diferentes cosas y hay que averiguar cómo queda luego de cada una de ellas:
* Anclar en una isla deshabitada: cuando llega a una isla todos los miembros de la tripulación toman como tesoro un elemento típico de la isla, del que hay en abundancia para que todos los piratas puedan tener uno. Por ejemplo, puede estar la Isla Tortuga, que es muy pequeña y en la que todos los piratas se hacen de un frasco de arena de valor 1 y la Isla del Ron, donde alguna vez hubo un depósito de provisiones y todos los piratas de la tripulación se llevan una botella de ron valuada en 25. 
* Atacar una ciudad: Cuando el barco llega a una ciudad costera en la que hay muchos tesoros, luego de derrotar a las pobladores, los piratas saquean los tesoros existentes. Generalmente hay más tesoros que piratas. Por orden de llegada, a cada pirata le toca un tesoro. En caso que el tesoro que le toca a un pirata no sea acorde a la manera de saquear del barco, no pasa nada, simplemente ese pirata no acrecienta su botín. Seguramente van a quedar tesoros sin saquear, pero no es de nuestro interés modelar cómo queda la ciudad. 
A veces ocurre que la ciudad es muy pobre o la tripulación del barco muy numerosa, de manera que no hay suficientes tesoros para todos los piratas, lo cual es un problema. En ese caso, los primeros piratas saquean los tesoros existentes, siempre de acuerdo a la manera de saquear del barco, y los restantes piratas son bajados de la embarcación y abandonados en la ciudad.
* Abordar otro barco en alta mar. ¿Qué le pasa a un barco pirata cuando va navegando y se cruza con otro barco? Inventar una función que lo represente adecuadamente. 

### Hacete toda la película... 
Hacer una serie de pruebas sobre el argumento de la película. Se recomienda comenzar con escenas sueltas y luego irlas encadenando para representar el desarrollo de la saga.
A la vez ir analizando cómo van quedando los piratas.
Un desafío interesante es encontrar una serie de circunstancias que lleven a cierto objetivo, por ejemplo lograr que Jack Sparrow termine la película siendo un pirata afortunado.

Por ejemplo, algunos casos de prueba pueden ser:
* La tripulación del Perla Negra desembarca en la IslaDelRon y todos se llevan una botella.
* El Perla Negra ataca Port Royal, donde hay muchos tesoros.
* El Holandes Errante pasa por la Isla Tortuga y luego hace un largo viaje para atacar Carmen de Patagones, donde hay pocos tesoros.
* El Perla Negra aborda al Holandes Errante


## Segunda parte: Piratas de todas las épocas

*¡Los piratas han modernizado sus técnicas y van por más!*

![piratas](piratas2.jpg)

### Tesoros para todos 
Además de los tesoros mencionados, aquellos que ya tienen un valor y un nombre establecido, aparecen otros elementos que son codiciados por los piratas y forman parte de su botín:
* Bonos en dafault: Consta de un conjunto de cotizaciones históricas. Su valor se calcula como la diferencia entre la mínima y la máxima cotización, más un 50% de comisión. Su nombre es siempre "Bono".
* Letras de liquidez: El valor se calcula tomando como base el importe nominal y aplicándole una tasa de interes que depende del país emisor. Por ejemplo, si es argentino, la tasa es del 74%. Su nombre es "LeLiq", seguido de un espacio y el nombre del país.

### Saqueos sofisticados
Además de mantenerse las anteriores maneras de saquear, surgen otras
* Buitres: Permite elegir cualquier tesoro que sea un bono en dafault.
* Fobicos: Permite tomar cualquier tesoro, excepto los que su nombre sea una palabra dada, que representa la cosa a la que se le tiene fobia
Tener en cuenta que hay ahora diferentes formas de valorizar y denominar a los tesoros, y que hay más variantes para el modo de saqueo que es una conjunción de otros modos de saqueo.

### Universidad Pirata 
Existen universidades cuya misión es desarrollar las habilidades de saqueo piratas. Cuando un barco va al laboratorio de una universidad, su forma de saqueo se ve alterada de acuerdo al perfil académico de dicha institución. Se tiene conocimiento de los siguientes perfiles, pero podría haber más:
* Universidad Anti Dictaminante de Estilos: Provoca que el barco tenga la forma de saqueo inversa a la que tenía. Por ejemplo si era "de corazón", ahora saquea todo; si era de objeto específico, se vuelve fóbica de dicho objeto y así sucesivamente.
* Universidad de Buitres Alternativos: Hace que el barco quede con una forma de saqueo compleja, donde una de las alternativas es la que el barco ya tenía, y se le agrega la forma "buitre" y la de cosas valiosas. 
* Universidad Atlantica Inofensiva: No le afecta en absoluto.  

### Historias de barcos
Se quieren contar y analizar las historias de aventuras de los barcos piratas. En algún sentido el objetivo es similar a "Hacete la película", pero centrada en un barco y generalizando para considerar una numerosa cantidad de situaciones. 
1. Definir una función que reciba un barco y una serie de situaciones en las que interviene el barco y que permita averiguar cómo queda el barco luego de haberlas vivido. Por ejemplo, al Perla Negra se le sube Will Turner, luego aborda al Holandés Errante, ataca Port Royal, etc.  
2. Definir una función que recibiendo una historia formada por una serie de situaciones y un conjunto de barcos, obtenga todos los barcos para los cuales la historia es inofensiva, es decir, que luego de haberla vivido el barco queda igual. (Se asume  que es igual cuando los piratas que conforman la tripulación son los mismos, más allá que se hayan subido al barco en otro orden; no contemplar las formas de saqueo)
3. Definir una función que recibiendo una historia formada por una serie de situaciones y un conjunto de barcos, obtenga el barco que queda con la tripulación más numerosa, luego de haber vivido toda la historia. 

### Proliferación de piratas 
El éxito de los piratas hace que muchos quieran unirse a las tripulaciones de los barcos, tanto que se vuelven infinitas... 
Definir las funciones que permitan obtener un barco con tripulación infinita, donde cada pirata tenga algo diferente a los otros piratas con los que comparte tripulación. 
Analizar qué sucede con las anteriores funciones cuando reciben un barco de tripulación infinita. Detectar en particular aquellas funciones que:
* No devuelven nada por quedarse evaluando infinitamente. 
* Devuelven una respuesta infinita 
* Devuelven una respuesta finita 
* Segun cuál sea la otra información del barco o con qué otros parámetros se la invoque, pueden pasar diferentes cosas.

