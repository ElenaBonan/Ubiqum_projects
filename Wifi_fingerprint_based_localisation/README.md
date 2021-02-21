# Wifi_fingerprint_base_localisation

Find the position of a person accurately in an indoor environment is still an open problem. A possibility is to look at the received signal strength (RSS) on the phone from the Wifi access points of the building. Having an accurate propagation model for the signals of WAPS in an indoor real environment is extremely difficult. Location fingerprints techniques use empirical data to approximate a location. We analyzed one of the most important datasets for this problem, which was created at the University Jaume I in Spain. The data involved three different buildings, where the RSS were captured by different users. For every building, We created a model to predict the floor, longitude and latitude given the RSS captured by a phone in the building. We carefully examined the metrics obtained, trying to discover which factors influenced the errors (problems of the phone, the shape of the building, missing zones in the radio map, etc.). Finally, We gave some suggestion for future improvement (more sophisticated algorithms, different positioning of the routers, etc.). <br>

<b> Problem </b> Classification (redict the building and the floor) and regression (predict the longitude and latitude).<br>
<b> Data source:</b> http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc. <br>
<b> Link to visualiza the notebook:</b> https://elenabonan.github.io/Ubiqum_projects/Wifi.html. <br>

# Content of the Repository

- <b> Data </b> A folder with the training and validation set.
- <b> Documents </b> This folder contains two documents. A detailed report with the description of the data analysed, the findings and the metrics of the chosen models. A powerpoint that we used to present the project during the course. 
- <b> Models </b> This folder contains the final models.
- <b> Modeling.Rmd </b> This is the code used to create the models. To visualize the code use this link: https://elenabonan.github.io/Ubiqum_projects/Wifi.html.
- <b> Modeling.nb.html </b> This is the html code produced by the previous code in R Markdown. 

# Structure of the code 
The code is divided in the following five parts:
- <b> Preprocessing </b> Some of the tasks done in the preprocessing are the followings: eliminate the variables with zero variance, eliminate the observations duplicated, find an appropriate value for the absence of signal and locate the Waps in a building looking at where its signal was received.
- <b> Model to predict the building </b> Using all the observation of the training we have predicted the building of the observation in the validations trying different models (knn, random forest and support vector machine).
- <b> Model for building 1 </b> Using the signal of the Waps that we have located in this building, we have predicted in parallel the floor, longitude and latitude. We have used three well-known models (knn, random forest and SVM) trying different hyperparameters.
- <b> Model for building 2 </b> We  predicted the position using the same approach of the previous building.
- <b> Model for building 3 </b> We repeat the same process of the previous buildings. 
