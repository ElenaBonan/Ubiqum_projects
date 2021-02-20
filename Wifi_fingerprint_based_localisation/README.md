# Wifi_fingerprint_base_localisation

Find the position of a person accurately in an indoor environment is still an open problem. A possibility is to look at the received signal strength (RSS) on the phone from the Wifi access points of the building. Having an accurate propagation model for the signals of WAPS in an indoor real environment is extremely difficult. Location fingerprints techniques use empirical data to approximate a location. I analyzed one of the most important datasets for this problem, which was created at the University Jaume I in Spain. The data involved three different buildings, where the RSS were captured by different users. For every building, I created a model to predict the floor, longitude and latitude given the RSS captured by a phone in the building. I carefully examined the metrics obtained, trying to discover which factors influenced the errors (problems of the phone, the shape of the building, missing zones in the radio map, etc.). Finally, I gave some suggestion for future improvement (more sophisticated algorithms, different positioning of the routers, etc.). 

# Content

- A detailed report with the description of the data analysed, the findings and the metrics of the chosen models.
- A PowerPoint with an introduction of the problem and the models used to predict the position.
- The training and validation set.
- The R script used to train the models.
- A folder with the final models chosen.

# Structure of the code 
