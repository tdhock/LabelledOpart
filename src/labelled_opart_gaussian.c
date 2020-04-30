#include <stdlib.h>
#include <math.h>//for INFINITY
#include "labelled_opart_gaussian.h"


//Utility function for initializing the indicator list
void InitializeIndicator(int* indicator, const int* starts, const int* ends,
                         const int* breaks, const int n_data, const int n_labels){
  int i = 0;
  int j = 0;
  int start = -1;
  int end = -1;

  //first initialize each value to 0, indicating false
  for(i = 1; i <= n_data; i++){
    indicator[i] = 0;
  }

  for(i = 0; i < n_labels; i++){
    if(breaks[i] == 1){
      start = starts[i];
      end = ends[i];
      for(j = start + 1; j < end; j++){
        indicator[j] = 1; //indicating true
      }
    }
  }
}

//Utility function for initializing 0 labels list
void InitializeZeroLabels(int* zeros, const int* starts, const int* ends,
                          const int* breaks, const int n_data, const int n_labels){
  int i = 0;
  int j = 0;
  int start = -1;
  int end = -1;

  //first initialize each value to 0, indicating false
  for(i = 1; i <= n_data; i++){
    zeros[i] = 0;
  }

  for(i = 0; i < n_labels; i++){
    if(breaks[i] == 0){
      start = starts[i];
      end = ends[i];
      for(j = start + 1; j < end; j++){
        zeros[j] = 1;
      }
      zeros[start] = -2;
      zeros[end] = -1;
    }
  }
}

//Utility function for initializing closest list
void InitializeClosest(int* closest, const int* starts, const int* ends,
                       const int* breaks, const int n_data, const int n_labels){
  int previous = 0;
  int last = n_data;

  int i;
  int j;
  int current;

  for(i = 0; i <= n_data; i++){
    closest[i] = 0;
  }

  for(i = 0; i < n_labels; i++){
    if(breaks[i] == 1){
      current = starts[i];
      for(j = previous + 1; j <= current; j++){
        closest[j] = previous;
      }
      previous = current;
    }
  }
  for(i=previous + 1; i <= last; i++){
    closest[i] = previous;
  }
}

//Utility function for storing cumulative sums of given list of values
void InitializeSums(const double* data, double* sums, const int n_data){
  double total = 0;
  int x;
  sums[0] = 0;
  for(x = 1; x <= n_data; x++){
    sums[x] = data[x - 1] + sums[x - 1];
  }
}

//Utility function for getting mean of a segment starting at "initial" and ending at "final"
double GetMean(int initial, int final, double* sums){
  int length = final - initial + 1;

  double mean = (sums[final] - sums[initial - 1]) / length;

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
                         double* vt, int* indicator, int* zeros, int* closest, int* positions,
                         int* vt_end, const double beta, const int n_data, double* cand_cost){

  //loop variables
  int i, s, t, start;

  //temporary variables used in recursive function
  double f_tau;
  double min;
  int pos;
  double maxCost = INFINITY;


  //initialize the positions with -2 in the positions vector as initially no data-point is segment end
  for(i = 0; i <= n_data; i++){
    positions[i] = -2;
    cand_cost[i] = i < 95 ? maxCost : -1;
  }

  //F(1) = F(0) + Cy1:1 + B
  //F(0) = B

  dp[0] = -1 * beta;
  vt[0] = dp[0];
  positions[0] = -1;
  dp[1] = GetSegmentCost(1, 1, sums);
  vt[1] = dp[1];

  //Calculate F(t) for all t in 'data_points'
  //F(t) = min{F(s) + C(Ys+1:t) + B}
  for(t = 1; t <= n_data; t++){
    if (zeros[t] != 1){
      start = closest[t];
      min = maxCost;
      for(s = start; s < t; s++){
        if((zeros[s] == 1) || (zeros[s] == -2)){
          if(t == 95){
            cand_cost[s] = maxCost;
          }
          continue;
        }
        f_tau = vt[s] + GetSegmentCost(s + 1, t, sums) + beta;
        if(t == 95){
          cand_cost[s] = f_tau;
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
    }

    //check if need to calculate vt value
    if((indicator[t] == 1) || (zeros[t] == -1)){
      int prev_start = closest[start];
      if (prev_start == start){
        vt[t] = GetSegmentCost(start, t, sums);
        vt_end[t] = 0;
      }
      else{
        min = maxCost;
        for(s = prev_start; s < start; s++){

          f_tau = vt[s] + GetSegmentCost(s + 1, t, sums) + beta;

          if(f_tau <= min){
            min = f_tau;
            pos = s;
          }
        }
        vt[t] = min;
        vt_end[t] = pos;
      }
      if(zeros[t] != 0){
        dp[t] = vt[t];
      }
    }
    else{
      vt[t] = dp[t];
    }
  }
}



//The interface function that gets called through R
int labelled_opart_gaussian(const int n_data, const int n_labels, const double *data_ptr,
                            const double penalty, double *cost_ptr, const int* starts, const int* ends,
                            const int* breaks, int* indicator, int* zeros, int* closest, double* sums,
                            double* dp,double* vt, int *end_ptr, int* positions, int* vt_end, double* cand_cost){

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

  InitializeIndicator(indicator, starts, ends, breaks, n_data, n_labels);

  InitializeZeroLabels(zeros, starts, ends, breaks, n_data, n_labels);

  InitializeClosest(closest, starts, ends, breaks, n_data, n_labels);

  //Compute optimal cost values and segment ends
  FindOptimalSegments(data_ptr, sums, dp, vt, indicator, zeros, closest, positions, vt_end,
                      penalty, n_data, cand_cost);

  //Copy the optimal cost values to cost_ptr for return
  for(i = 0; i <= n_data; i++){
    cost_ptr[i] = dp[i];
    end_ptr[i] = positions[i];
  }

  //Traceback the optimal segment ends and copy to end_ptr for return
  i = n_data;
  end_ptr[0] = n_data;
  int index = 1;
  while(1){
    if (i <= 0){
      break;
    }
    if (indicator[i] == 1){
      end_ptr[index] = vt_end[i];
      i = vt_end[i];
    }
    else{
      end_ptr[index] = positions[i];
      i = positions[i];
    }
    index += 1;
  }
  maxPos = index;
  index--;
  i = 0;
  while(i < index){
    int temp = end_ptr[i];
    end_ptr[i] = end_ptr[index];
    end_ptr[index] = temp;
    i++;
    index--;
  }
  //assigning -2 as a placeholder value indicating that segment is not used in the optimal model
  while(maxPos <= n_data){
    end_ptr[maxPos] = -2;
    maxPos++;
  }

  //success
  return 0;
}
