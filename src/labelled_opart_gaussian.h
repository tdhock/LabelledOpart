#define NEGATIVE_PENALTY 1
#define NUM_OF_DATA_VALUES_LESS_THAN_ZERO 2

int labelled_opart_gaussian
(const int n_data, const int n_labels, const double *data_ptr, const double penalty,
 double *cost_ptr, const int* starts, const int* ends, const int* breaks,
 int* indicator, int* closest, double* sums, double* dp, double* vt, int *end_ptr,
 int* positions, int* vt_end);
