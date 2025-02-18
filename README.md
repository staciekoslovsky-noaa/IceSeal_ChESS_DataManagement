# ChESS Ice Seal Survey Data Management

This repository stores the code associated with managing ChESS survey data. Code numbered 0+ are intended to be run sequentially as the data are available for processing. Code numbered 99 are stored for longetivity, but are intended to only be run once to address a specific issue or run as needed, depending on the intent of the code.

The data management processing code is as follows:
* **CHESS_01_Import2DB.R** - code to import data into the DB (from CSV files on the network)
* **CHESS_01_ImportEffortLog.R** - code to import effort log data into the DB
* **CHESS_01_ImportFastIce.R** - code to import fast ice data into the DB
* **CHESS_02_CorrectHotspots2HiLo.R** - code to correct issues with hotspots being too high or low on the frame identified during processing
* **CHESS_02_ProcessOrphanImages.R** - code to process orphan images identified during processing
* **CHESS_03_QA_CompareFilt2Unfilt.R** - code to compare filtered and unfiltered data to check for alignment
* **CHESS_03_QA_DuplicateSeals.txt** - code to detect duplicate seals in the data; code to be run in PGAdmin
* **CHESS_03_QA_SpeciesID.txt** - code to quality check species ID data; code to be run in PGAdmin
* **CHESS_04_CreateTracklineByEffort.txt** - code to create a spatial dataset of trackline data by effort type; code to be run in PGAdmin
* **CHESS_04_CreateTracklineByFlight.txt** - code to create a spatial dataset of trackline data by flight; code to be run in PGAdmin

Other code in the repository includes:
* Code for creating a view in the DB for displaying effort data with processed sightings:
	* CHESS_99_CreateProcessWithEffort_4ELR.txt
* Code for creating datasets for machine learning purposes:
	* CHESS_99_Data4MachineLearning.txt
	* CHESS_99_Data4MachineLearning_Export2CSV.R
	* CHESS_99_Data4MachineLearning_Subsets.R
	* CHESS_99_Data4ViameTraining.R
* Code for comparing/reviewing detections:
	* CHESS_99_Detections_CompareProcessedSightings.R
	* CHESS_99_Detections_Original4Process.R
* Code for supporting polar bear work:
	* CHESS_99_PolarBear_Covariates4EEM.txt
	* CHESS_99_PolarBear_TrackEffort4PBC.R
	* CHESS_99_PolarBear_View.txt
* Code for processing detection rate data into a single table:
	* CHESS_99_ProcessDetectionRateData2SingleTable.R
* Code for preparing data for possible Kaggle competition:
	* CHESS_99_RandomSelection_4Kaggle.R
* Code for reviewing valid hotspots:
	* CHESS_99_ReviewValidFrames4HotspotIDs.R

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.