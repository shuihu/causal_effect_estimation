/*
 * Run an observation down the tree, and return the prediction error,
 *    for several CP values at once.
 *
 */
#include <math.h>
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void
rundown3(pNode tree, int obs, int neighbor, double *cp, double *xpred, double *xpred2, double *xtemp)
{
  int i, obs2 = (obs < 0)? -(1 + obs) : obs;
  int neighbor2 = (neighbor < 0)? -(1 + neighbor) : neighbor;
  pNode otree = tree; // record the last upper level of the tree
  pNode tree2 = tree; // record the root of the tree
  pNode otree2 = tree;
  double temp;
   
  for (i = 0; i < rp.num_unique_cp; i++) {
    
    while (cp[i] < tree->complexity) {
      tree = branch(tree, obs);
      if (tree == 0) {       
        if (rp.usesurrogate < 2) {  /* must have hit a missing value */
          for (; i < rp.num_unique_cp; i++)
	          xpred[i] = otree->response_est[0];
        } else
          goto oops;
      }  
	    otree = tree;
    }
    xpred[i] = tree->response_est[0];
   
    
    while (cp[i] < tree2->complexity) {
      tree2 = branch(tree2, neighbor);
      if (tree2 == 0) {
        if (rp.usesurrogate < 2) {  /* must have hit a missing value */
          for (; i < rp.num_unique_cp; i++)
            xpred2[i] = otree2->response_est[0];
        } else
          goto oops;  
      }
      otree2 = tree2;
    }
    xpred2[i] = tree2->response_est[0]; 
    
    //Rprintf("under cp = %f,the estimated value for obs %d is %f, and its neighbor %d's is %f.\n", cp[i], obs, xpred[i], neighbor, xpred2[i]);
    
    
    // matching function:
    temp = (2 * rp.wt[obs2] - 1) *(rp.ydata[obs2] - rp.ydata[neighbor2]);
    xtemp[i] = (temp - 0.5 * (xpred[i] + xpred2[i])) * (temp - 0.5 * (xpred[i] + xpred2[i]));
    //Rprintf("under cp = %f, the error is %f\n", cp[i], xtemp[i]);
  }
  return;
  
  oops:; 
  warning("Warning message--see rundown3.c");  
}


