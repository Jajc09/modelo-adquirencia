# modelo-adquirencia


Modelo de adquirencia CredibanCo.


Uso de la librería OSMAR para la extracción de información de la API de OpenStreetMap. https://www.openstreetmap.org/#map=18/4.65495/-74.05671&layers=D.


La información extraída corresponde a la ciudad de Bogotá.


Fecha de creación: 07/septiembre/2020.


## osm_variables.RDS: 
Contiene la base final del ejercicio luego de extraer la información de los poligonos de Bogotá usando la libreria osmar y la API de OpenStreetMap.


## osmar_library.R: 
Script de R con la extracción de los centroides de los poligonos de Bogotá, extracción de la información de OpenStreetMap y limpieza de la base de datos extraída.


## ubicaciones_hexagonos.RDS: 
Base de los centroides de los poligonos de Bogotá, estos poligonos son de tamaño 100. 


Fuente: https://journal.r-project.org/archive/2013/RJ-2013-005/RJ-2013-005.pdf
