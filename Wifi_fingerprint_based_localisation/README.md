# Wifi_fingerprint_base_localisation

Find the position of a person accurately in an indoor environment is still an open problem. A possibility is to look at the received signal strength (RSS) on the phone from the Wifi access points of the building. Having an accurate propagation model for the signals of WAPS in an indoor real environment is extremely difficult. Location fingerprints techniques use empirical data to approximate a location. We analyzed one of the most important datasets for this problem, which was created at the University Jaume I in Spain. The data involved three different buildings, where the RSS were captured by different users. For every building, We created a model to predict the floor, longitude and latitude given the RSS captured by a phone in the building. We carefully examined the metrics obtained, trying to discover which factors influenced the errors (problems of the phone, the shape of the building, missing zones in the radio map, etc.). Finally, We gave some suggestion for future improvement (more sophisticated algorithms, different positioning of the routers, etc.). <br>

<b>Problem </b> Classification (redict the building and the floor) and regression (predict the longitude and latitude).<br>
<b> Data source:</b> http://archive.ics.uci.edu/ml/datasets/UJIIndoorLoc. <br>
<b> Link to visualiza the notebook:</b> https://elenabonan.github.io/Ubiqum_projects/Wifi.html. <br>

# Content of the Repository

- <b> Data </b> A folder with the training and validation set.
- <b> Documents </b> This folder contains two documents. A detailed report with the description of the data analysed, the findings and the metrics of the chosen models. A powerpoint presentations that we used to present the project during the course. 
- <b> Models </b> This folder contains the final models.
- <b> Modeling.Rmd </b> This is the code used to create the model. To visualize the code use this link: https://elenabonan.github.io/Ubiqum_projects/Wifi.html.
- <b> Modeling.nb.html </b> This is the html code produced by the previous code in R Markdown. 

# Structure of the code 
The code is divided in the following five parts:
- preprocessing 
