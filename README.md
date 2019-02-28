This repository can be used to impute travel mode for mobile data locations.

***Please pay attention to the License file before using this repository.***

--------------------------------------------------------------------
DIRECTORY STRUCTURE:
--------------------------------------------------------------------
The repository directory should be as follows:

--Codes
-------Attribute_Extraction: The R code in this folder takes the point file from the Inputs folder and creates a trip file with all trip attributes, in the Inputs folder. Make sure the point file is available in the Inputs folder before running this code.
-------Prediction: The RF_Prediction python code in this folder uses the output of Attribute_Extraction code, which is a trip file, and imputes the modes.The folder also includes model file and other dependencies.
--Inputs: The inputs are separately shared on DropBox. The folders inside the shared DropBox folder should be added to the Inputs folder. The Inputs folder should have the following structure (Otherwise directories in the codes need to be changed):
--Network_Data
------Bus_Network: Includes the bus network shapefiles, required for the attribute extraction.
------Commercial_Airports: Includes the airport locations shapefiles, required for the attribute extraction.
------Road_Network: Includes the road network shapefiles, required for the attribute extraction.
------Track_Network: Includes the rail and metro network shapefiles, required for the attribute extraction.
--Trip_Data
------AirSage_Data: This folder includes the point files and trip files. Point files are raw location data, with trip ID attribute added, required for attribute extraction code. trip files are outputs of the attribute extraction and input to the mode prediction code. The trip files include trips and their attributes. The trip and point files available in the shared DropBox folder are obtained from the sample data previously shared with MTI. The points data can be replaced with any new raw location data and the new trip files should be obtained by running the Attribute_Extraction code on the new point files.

--------------------------------------------------------------------
WORK FLOW:
--------------------------------------------------------------------
1) Start with the raw location point files (Trip ID should be added to the point file).
2) Have all the network files ready in the Inputs folder.
3) Run Attribute_Extraction R code to create a trip file. All trip attributes such as speed, time, distance, network distance etc. get extracted and saved in the trip file using this code.
4) Once the Attribute_Extraction code is finished running and the trip file is created, run the RF_Prediction python code to calculate the mode attribute for each trip.


