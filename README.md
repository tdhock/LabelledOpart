# LabelledOpart

An implementation for change point detection of labelled time-series data.


Make sure that `data-for-LOPART.rds` is present in the root folder before running these plots.

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
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TimingVsLabelsVsSize.PNG)

[CostVsSignalComparison.R](CostVsSignalComparison.R) creates plot which shows variation of cost vs position of last change point for lopart

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/SignalVsCost.PNG)

[TestErrors.R](TestErrors.R) compares the test errors of lopart and opart on 10 different penalty values for 10 profiles as shown:

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TestErrors.PNG)

[TrainErrorsOpart.R](TrainErrorsOpart.R) creates a table of min and max train errors for opart on all profiles a few of which are as shown:
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TrainErrorsOpart.PNG)

It also plots average test errors of opart and labelled opart on real data as shown
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/AverageTestErrors.PNG)

[591_1Segments.R](591_1Segments.R) shows segment means of profile 591.1 for both lopart and opart. This figure shows that lopart has no train error whereas opart detects a breakpoint in zero labelled region
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/591_1Segments.PNG)


[TimingVsSize.R](TimingVsSize.R) creates and compares time vs data size plot for opart, lopart and fpop. From the figure we can see that both opart and lopart have quadratic complexity whereas fpop is log linear.

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TimingVsSize.PNG)

[Comparison.R](Comparison.R) creates and compares models of labelled opart, fpop and pdpa on real dataset(profile 8.11).

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/Comparison.PNG)



