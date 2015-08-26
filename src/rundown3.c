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
  int i, obs2 = (obs < 0)? -( 1 + obs) : obs;
  int neighbor2 = (neighbor < 0)? -( 1 + neighbor) : neighbor;
  
  pNode otree = tree; // record the last upper level of the tree
  pNode tree2 = tree; // record the root of the tree
  pNode otree2 = tree;
  double temp;
  //Rprintf("xtree->complexity = %f\n", tree->complexity);
   
  for (i = 0; i < rp.num_unique_cp; i++) {
    
    while (cp[i] < tree->complexity) {
      tree = branch(tree, obs);
      //Rprintf("cp[%d] = %f, tree->complexity = %f, ",i, cp[i], tree->complexity);
      //Rprintf("obs %d:%f -> ", obs+1, tree->response_est[0]);
      //if (tree == 0) {       
      //  if (rp.usesurrogate < 2) {  /* must have hit a missing value */
      //    for (; i < rp.num_unique_cp; i++)
	    //       xpred[i] = otree->response_est[0];
      //  } else
      //    goto oops;
     // }  
	    otree = tree;
    }
    //Rprintf("\n");
    xpred[i] = tree->response_est[0];
   
    
    while (cp[i] < tree2->complexity) {
      tree2 = branch(tree2, neighbor);
      //Rprintf("cp[%d] = %f, tree2->complexity = %f, ",i, cp[i], tree2->complexity);
      //Rprintf("obs %d: %f -> ", neighbor+1, tree2->response_est[0]);
      // This part is only to deal with missing values
      //if (tree2 == 0) { 
      //  if (rp.usesurrogate < 2) {  /* must have hit a missing value */
      //    for (; i < rp.num_unique_cp; i++)
      //      xpred2[i] = otree2->response_est[0];
      //  } else
      //    goto oops;  
      //}
      otree2 = tree2;
    }
    //Rprintf("\n");
    xpred2[i] = tree2->response_est[0]; 
    
    //Rprintf("under cp = %f, the estimation for obs %d is %f, neighbor %d's is %f.\n", cp[i], (obs + 1), xpred[i], neighbor+1, xpred2[i]);
    
    
    // matching function:
    temp = (2 * rp.wt[obs2] - 1) * (*rp.ydata[obs2] - *rp.ydata[neighbor2]);
    //Rprintf("neighbor2 = %d\n", neighbor2);
    //Rprintf("rp.wt[%d] = %f, rp.ydata[%d] = %f, rp.ydata[%d] = %f\n", obs2, rp.wt[obs2], 
    //  obs2, *rp.ydata[obs2], neighbor2, *rp.ydata[neighbor2]);
    
    xtemp[i] = (temp - 0.5 * (xpred[i] + xpred2[i])) * (temp - 0.5 * (xpred[i] + xpred2[i]));
    //Rprintf("temp = %f, the error is %f\n", temp, xtemp[i]);
  }
  return;
  
  oops:; 
  warning("Warning message--see rundown3.c");  
}


