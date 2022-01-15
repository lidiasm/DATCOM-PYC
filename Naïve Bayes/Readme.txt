Para la ejecución de los ficheros .Rmd sería recomendable pero no necesario seguir el orden cronológico que he seguido:


1. Preprocesamiento1.Rmd : Contiene una breve introducción y un preprocesamiento común en el grupo.
2. Preprocesamiento2. Rmd: Contiene otra variante del preprocesamiento1.Rmd
3. Preprocesamiento3. Rmd: Preprocesamiento y clasificación (incluyengo ensemble)
4. Seleccion_NaiveBayes.Rmd: Básicamente se realiza un estudio para seleccionar las características más importantes. Incluida clasificación
5. Preprocesamiento5yClasifica.Rmd
6. Preprocesamiento6yClasifica.Rmd 
7. Preprocesamiento7yClasifica.Rmd: 
8. Preprocesamiento8yMejorClasifica.Rmd: En ese fichero se encuentra la clasificación que obtiene la mejor tasa de acierto.
9. Prepro_PowersetLabel.Rmd: Se usa la técnica que transforma el dataset multietietiqueta en uno uno multiclase
10. Ensembles.Rmd: Un fichero únicamente para multiclasificación.


Nota: Se incluyen dos ficheros .csv pues la ejecución tardo unas horas. 
El fichero Ensembles.Rmd también tarda 4h en ejecutarse  