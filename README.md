# LabelledOpart

An implementation for change point detection of labelled time-series data.


[CostComparison.R](CostComparison.R) creates cost comparison of lopart and opart on simulated data as follows:

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/CostComparison.PNG)

[ModelComparison.R](ModelComparison.R) creates model comparison figures for opart and lopart on sample data for zero and infinite penalties as follows:

Lopart with infinite penalty:
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/ModelComparison1.PNG)

Lopart with zero penalty
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/ModelComparison4.PNG)

Opart with infinite penalty:
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/ModelComparison2.PNG)

Opart with zero penalty
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/ModelComparison3.PNG)


[TimingVsLabels.R](TimingVsLabels.R) creates timing comparison plot for lopart with number of labels for fixed langth dataset
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TimingVsLabels.PNG)

[CostVsSignalComparison.R](CostVsSignalComparison.R) creates plot which shows variation of cost vs position of last change point for lopart

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/CostVsSignal.PNG)

[TestErrors.R](TestErrors.R) compares the test errors of lopart and opart for 10 profiles as shown:

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TestErrors.PNG)

[TrainErrorsOpart.R](TrainErrorsOpart.R) creates a table of min and max train errors for opart on all profiles a few of which are as shown:
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TrainErrorsOpart.PNG)
