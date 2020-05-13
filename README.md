# LabelledOpart

An implementation for change point detection of labelled time-series data. This package provides a `labelled_opart_gaussian` interface to fits an optimal segmentation model given the data vector, labelled regions(data frame with 3 columns indicating start, end and number of change points in a labelled region sorted by start) and a non-negative penalty value. It outputs 2 vectors, `cost.vec` and `end.vec` indicating the cost values and segment ends of the optimal model respectively.


The following example illustrates how to use this interface.

```{r}
signal <- rnorm(100, 20)
labels <- data.frame("start" <- c(5, 20, 60), "end" <- c(10, 30, 80), "change" <- c(1,0,1))
penalty <- 5

labelled_fit <- LabelledOpart::labelled_opart_gaussian(signal, labels, penalty)
labelled_fit$cost.vec
 [1]   -375.2093   -744.6697  -1167.0363  -1609.0187  -2054.5544  -2474.2438  -2823.9470  -3220.7165  -3669.4748
 [10]  -4120.8629  -4456.2042  -4846.3210  -5318.3026  -5689.6070  -6023.6161  -6495.6699  -6887.3299  -7257.8869
 [19]  -7618.7644  -8021.0849      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000      0.0000
 [28]      0.0000      0.0000 -12204.9984 -12548.6820 -12947.9393 -13356.1328 -13797.0257 -14175.1365 -14622.7457
 [37] -15080.1473 -15420.2120 -15842.1656 -16261.5906 -16731.0878 -17182.7440 -17634.0862 -18095.1257 -18575.6211
 [46] -18930.9546 -19367.1551 -19765.1570 -20185.7667 -20624.6953 -21006.5681 -21336.7816 -21710.2891 -22139.7341
 [55] -22521.7704 -22919.5256 -23347.2869 -23797.8305 -24252.3843 -24695.1494 -25102.5598 -25450.3279 -25797.1192
 [64] -26241.2797 -26647.2577 -27062.3243 -27492.9771 -27938.6166 -28339.8505 -28745.6073 -29186.8117 -29656.6330
 [73] -30040.5093 -30471.2192 -30870.8495 -31306.9186 -31688.8745 -32037.7812 -32419.5178 -32874.3574 -33301.0884
 [82] -33767.0142 -34181.5102 -34533.8979 -34962.1599 -35326.4471 -35699.3224 -36039.6057 -36399.9699 -36884.5717
 [91] -37269.1346 -37641.2138 -38030.0482 -38464.4993 -38891.5298 -39266.2141 -39637.9553 -40024.6504 -40413.3157
[100] -40805.5440

labelled_fit$end.vec
[1]   6  30  76 100
```

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
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TiminVsLabelsVsSize.PNG)

[CostVsSignalComparison.R](CostVsSignalComparison.R) creates plot which shows variation of cost vs position of last change point for lopart

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/SignalVsCost.PNG)

[TestErrors.R](TestErrors.R) compares the test errors of lopart and opart on 10 different penalty values for 10 profiles as shown:

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TestErrors.PNG)

[TrainErrorsOpart.R](TrainErrorsOpart.R) creates a table of min and max train errors for opart on all profiles a few of which are as shown:
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TrainErrorsOpart.PNG)

It also plots average test errors of opart and labelled opart on real data as shown
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/AverageTestErrors.PNG)

[591_1Segments.R](591_1Segments.R) shows segment means of profile 591.1 for both lopart and opart. This figure shows that lopart has no train error whereas opart detects a breakpoint in zero labelled region
![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/591_Segments.PNG)


[TimingVsSize.R](TimingVsSize.R) creates and compares time vs data size plot for opart, lopart and fpop. From the figure we can see that both opart and lopart have quadratic complexity whereas fpop is log linear.

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/TimingVsSize.PNG)

[Comparison.R](Comparison.R) creates and compares models of labelled opart, fpop and pdpa(under Segmenor3IsBack) on real dataset(profile 8.11).

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/Comparison.PNG)

[gfpopComparison.R](gfpopComparison.R) creates and compares models of labelled opart with gfpop on real dataset(profile 614.2).

![fig](https://github.com/as4378/LabelledOpart/blob/master/figures/StdComp.PNG)



