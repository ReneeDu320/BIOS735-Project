# Predicting the Success of Bank Telemarketing Using Client Information

### BIOS735 Team 6 "Robotcall" - SPRING 2022
#### Shuang Du, Michael Jetsupphasuk, Camille Liu and Kai Xia

### Project Description
Welcome to our UNC-BIOS735 project. In this repository, you will find a variety of scripts and folders. Telemarketing is a method of marketing directly to customers via phone or the Internet in order to sell goods or services. Telemarketing has increasingly been used in industrial sales organizations such as banks and insurance companies. Implementing this method successfully is difficult. To improve the performance in telemarketing, predictive analytics could play an essential role in the success of telemarketing operations. A data driven approach could be used in a decision support system to maximize the success rate of sales using key metrics from potential customers. A well-trained predictive model could potentially classify telemarketing targets using demographic information such as education, income, or marriage status. By training models on historical data including demographic information and success labels, a predictive classification model can be constructed using either parametric methods such as logistic regression or non-parametric methods such as support vector machines (SVM). In this project, we will evaluate the performance of both parametric and non-parametric statistical learning methods in predicting the success of telemarketing from bank data. Our primary outcome of interest is whether a client would agree to subscribe a term deposit after the targeted telemarketing campaign.

### Dataset
The data used in this project is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls.

* *bank-full.csv* - original version of this [dataset](https://archive.ics.uci.edu/ml/datasets/bank+marketing), with 17 inputs, ordered by date


### Data Processing and Report  

A copy of the initial project proposal 'Updated_proposal BIOS735 Team6.pdf' and final project report 'final_report.html' are kept in this folder. The presentation for the report can be found [here](https://docs.google.com/presentation/d/1EZ85oHhNHWCJr9l-fvAzzVQnlLNpCdSaWfzGy-WKoKk/edit?usp=sharing)

### Package 

Our R-package implementation is kept in 'final' folder. For more information about the functions please refer to the report.

To install the package, download the file `final_0.0.0.9000.tar.gz`, navigate to its directory, and run `install.packages("final_0.0.0.9000.tar.gz", repos=NULL)`. You can then load the package as usual using `library(final)`. 

