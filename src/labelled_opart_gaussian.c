#include <stdlib.h>
#include "labelled_opart_gaussian.h"


//Utility function for initializing the indicator list
void InitializeIndicator(int* indicator, const int* starts, const int* ends,
                          const int n_data, const int n_labels){
  int i = 0;
  int j = 0;
  int start = -1;
  int end = -1;

  //first initialize each value to 0, indicating false
  for(i = 0; i < n_data; i++){
    indicator[i] = 0;
  }

  for(i = 0; i < n_labels; i++){
    start = starts[i];
    end = ends[i];
    for(j = start; j < end; j++){
      indicator[j] = 1; //indicating true
    }
  }
}

//Utility function for initializing closest list
void InitializeClosest(int* closest, const int* starts, const int* ends,
                       const int n_data, const int n_labels){
  int previous = 1;
  int last = n_data;

  int i;
  int j;
  int current;

  closest[0] = 0;

  for(i = 0; i < n_data; i++){
    closest[i] = 0;
  }

  for(i = 0; i < n_labels; i++){
    current = starts[i];
    for(j = previous; j < current; j++){
      closest[j] = previous - 1;
    }
    previous = current;
  }
  for(i=previous; i < last; i++){
    closest[i] = previous - 1;
  }
}

//Utility function for storing cumulative sums of given list of values
void InitializeSums(const double* data, double* sums, const int n_data){
  double total = 0;
  int x;
  for(x = 0; x < n_data; x++){
    sums[x] = 0;
    total += data[x];
    sums[x] = total;
  }
}

//Utility function for getting mean of a segment starting at "initial" and ending at "final"
double GetMean(int initial, int final, double* sums){
  double mean = 0;

  int length = final - initial + 1;
  if(initial == 0)
    mean = sums[final] / length;
  else
    mean = (sums[final] - sums[initial - 1]) / length;

  return mean;
}

//Utility function for getting cost of segment starting at "initial" and ending at "final"
double GetSegmentCost(int initial, int final, double* sums){
  double mu = GetMean(initial, final, sums);
  int length = final - initial + 1;
  double total = mu * length;

  return ((-2 * mu * total) + (length * mu * mu));
}




//Recursive function for returning the optimal cost of segmentation upto each data point in the dataset
//This function essentially implements the optimal partitioning algorith described in section 3.1 in:
//https://link.springer.com/article/10.1007/s11222-016-9636-3
//The recursive function is:
//F(t) = min{F(s) + C(Ys+1:t) + B} for all 's' in [0,t) and 'B' is the penalty for segmentation
//We calculate F(t) for each data-point 't' in the given dataset to get the optimal cost
//F(0) = B
void FindOptimalSegments(const double* data_points, double* sums, double* dp,
                         double* vt, int* indicator, int* closest, int* positions,
                         int* vt_end, const double beta, const int n_data){

  //loop variables
  int i, s, t, start;

  //temporary variables used in recursive function
  double f_tau;
  double min;
  int pos;
  double maxCost = GetSegmentCost(0, n_data - 1, sums) + n_data*beta;


  //initialize the positions with -2 in the positions vector as initially no data-point is segment end
  for(i = 0; i < n_data; i++){
    positions[i] = -2;
  }

  //F(1) = F(0) + Cy1:1 + B
  //F(0) = B

  dp[0] = -1*beta + GetSegmentCost(0, 0, sums) + beta;
  vt[0] = dp[0];
  min = dp[0];
  positions[0] = -1;

  //Calculate F(t) for all t in 'data_points'
  //F(t) = min{F(s) + C(Ys+1:t) + B}
  for(t = 0; t < n_data; t++){
    start = closest[t];
    start = start == 0 ? -1 : start;
    min = maxCost;
    for(s = start; s < t; s++){
      if(s == -1){
        f_tau = -beta + GetSegmentCost(s, t, sums) + beta;
      }
      else{
        f_tau = vt[s] + GetSegmentCost(s + 1, t, sums) + beta;
      }
      if(f_tau <= min){
        min = f_tau;
        pos = s;
      }
    }
    //update the dynamic programming cost and position buffer with minimum cost
    //and associated position
    dp[t] = min;
    positions[t] = pos;

    //check if need to calculate vt value
    if(indicator[t] == 1){
      int prev_start = closest[start];
      prev_start = prev_start == 0 ? -1 : prev_start;
      min = maxCost;
      for(s = prev_start; s < start; s++){
        if(s == -1){
          f_tau = -beta + GetSegmentCost(s, t, sums) + beta;
        }
        else{
          f_tau = vt[s] + GetSegmentCost(s + 1, t, sums) + beta;
        }

        if(f_tau <= min){
          min = f_tau;
          pos = s;
        }
      }
      vt[t] = min;
      vt_end[t] = pos;
    }

    else{
      vt[t] = dp[t];
    }
  }
}



//The interface function that gets called through R
int labelled_opart_gaussian(const int n_data, const int n_labels, const double *data_ptr,
                            const double penalty, double *cost_ptr, const int* starts, const int* ends,
                            const int* breaks, int* indicator, int* closest, double* sums, double* dp,
                            double* vt, int *end_ptr, int* positions, int* vt_end){

  //test for boundary cases
  if(penalty < 0){
    return NEGATIVE_PENALTY;
  }

  if(n_data <= 0){
    return NUM_OF_DATA_VALUES_LESS_THAN_ZERO;
  }

  //loop variables
  int i;
  int j;

  //temporary variables used in tracing back segment ends
  int temp;
  int maxPos;

  //store cumulative sums for O(1) access to segment cost
  InitializeSums(data_ptr, sums, n_data);

  InitializeIndicator(indicator, starts, ends, n_data, n_labels);

  InitializeClosest(closest, starts, ends, n_data, n_labels);

  //Compute optimal cost values and segment ends
  FindOptimalSegments(data_ptr, sums, dp, vt, indicator, closest, positions, vt_end,
                      penalty, n_data);

  //Copy the optimal cost values to cost_ptr for return
  for(i = 0; i < n_data; i++){
    cost_ptr[i] = dp[i];
    end_ptr[i] = positions[i];
  }

  //set indicator of label ends to false
  for(i = 0; i < n_labels; i++){
    indicator[ends[i] - 1] = 0;
  }
  //Traceback the optimal segment ends and copy to end_ptr for return
  end_ptr[0] = n_data;
  i = n_data - 1;
  j = 1;
  while(1){
    if(i == -1){
      break;
    }
    if(indicator[i] == 1){
      if(vt_end[i] == -1)
        break;
      end_ptr[j] = vt_end[i] + 1;
      i = vt_end[i];
    }
    else {
      if(positions[i] == -1)
        break;
      end_ptr[j] = positions[i] + 1;
      i = positions[i];
    }
    if(i == 0){
      end_ptr[j] = positions[i] + 1;
      j++;
      break;
    }
    j++;
  }
  i = 0;
  j--;
  maxPos = j + 1;

  while(j > i){
    temp = end_ptr[i];
    end_ptr[i] = end_ptr[j];
    end_ptr[j] = temp;
    i++;
    j--;
  }

  //assigning -2 as a placeholder value indicating that segment is not used in the optimal model
  while(maxPos < n_data){
    end_ptr[maxPos] = -2;
    maxPos++;
  }

  //success
  return 0;
}
