#include <R.h>
#include <R_ext/Rdynload.h>
#include "labelled_opart_gaussian.h"


void labelled_opart_gaussian_interface
  (const int* n_data, const int* n_labels, const double *data_ptr,
   const double* penalty, double *cost_ptr, const int* starts, const int* ends,
   const int* breaks, int* indicator, int* closest, double* sums, double* dp, double* vt,
   int *end_ptr, int* positions, int* vt_end){

  int status = labelled_opart_gaussian(n_data[0], n_labels[0], data_ptr, penalty[0],
                                       cost_ptr, starts, ends, breaks, indicator, closest,
                                       sums, dp, vt, end_ptr, positions, vt_end);

  if(status == NEGATIVE_PENALTY){
    error("penalty value must be greater than 0");
  }

  if(status == NUM_OF_DATA_VALUES_LESS_THAN_ZERO){
    error("the data vector must have more than 0 elements");
  }

  if(status != 0){
    error("error code %d", status);
  }
}

R_CMethodDef cMethods[] = {
  {"opart_gaussian_interface",
   (DL_FUNC) &labelled_opart_gaussian_interface, 16
    //,{int, int, double, double, double, int, int, int, int, int,
    //  double, double, double, int, int, int}
  },
  {NULL, NULL, 0}
};


void R_init_opart(DllInfo *info) {
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  //R_useDynamicSymbols call says the DLL is not to be searched for
  //entry points specified by character strings so .C etc calls will
  //only find registered symbols.
  R_useDynamicSymbols(info, FALSE);
}

