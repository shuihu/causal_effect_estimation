/*
 * Run an observation down the tree, and return the prediction error,
 *    for several CP values at once.
 *
 */
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"
static double INFTY = 9999999999; // infinity

int findNeighbor(int obs, int k) { 
  // k is the starting index of the validation set
  int i, j, temp, neighbor;
  int obs2 = (obs < 0)? -(1 + obs) : obs;
  double dist, min = INFTY;
  int found = 0;
  
  for (i = k; i < rp.n; i++) {
    j = rp.sorts[0][i];
    temp = (j < 0)? -(1 + j) : j;
    if (rp.wt[temp] != rp.wt[obs2]) {
      found = 1;
      dist = measureDistance(obs2, temp);
      if (dist < min) {
        neighbor = j;
        min = dist;       
      } 
    }       
  }
  // for dubgging only:
  if(found == 0) Rprintf("There is only one group in validation set!");
  else Rprintf("two groups in validation! ");
  return neighbor;
}


double measureDistance(int i, int j) {
  int k;
  double distance = 0;
  for (k = 0; k < rp.nvar; k++) {
    distance += (rp.xdata[k][i] - rp.xdata[k][j]) * (rp.xdata[k][i] - rp.xdata[k][j]) / rp.xvar[k];   
  }
  return distance;
}