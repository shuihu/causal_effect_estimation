#include <math.h>
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

#ifndef DEBUG
# define DEBUG 0
#endif
#if DEBUG > 1
static int debug = 0;           
/* if it is odd, print out every tree */
/* if >= 2, print out every risk value we see */
#endif

	void
//xval(int n_xval, CpTable cptable_head, int *x_grp,
//		int maxcat, char **errmsg, double *parms, int *savesort)
xval(int n_xval, CpTable cptable_head, int *x_grp,
   	int maxcat, char **errmsg, int parms, double p, int *savesort)
{
	int i, j, k, ii, jj;
	int last;
	int xgroup;
	double *xtemp, *xpred, *xpred2;
	int *savew;
	double *cp;
	double alphasave;
	pNode xtree;
	CpTable cplist;
	double temp, temp1;
	double old_wt, total_wt;
  int neighbor; // nearest neighbor number

	alphasave = rp.alpha;
  
  // only for debugging
  
  int cv_count = 0;
  //Rprintf("n_xval = %d\n", n_xval);

	/*
	 * Allocate a set of temporary arrays
	 */
	xtemp = (double *) CALLOC(4 * rp.num_unique_cp, sizeof(double));
	xpred = xtemp + rp.num_unique_cp;
  xpred2 = xpred + rp.num_unique_cp;
	cp = xpred2 + rp.num_unique_cp;
	savew = (int *) CALLOC(rp.n, sizeof(int));
	for (i = 0; i < rp.n; i++)
		savew[i] = rp.which[i]; /* restore at the end */

	/*
	 * Make the list of CPs that I will compare against
	 */
   // test for 
	// cp[0] = 10 * cptable_head->cp;      /* close enough to infinity */
  cp[0] = 10000 * cptable_head->cp;
	for (cplist = cptable_head, i = 1; i < rp.num_unique_cp;cplist = cplist->forward, i++) {  
    //Rprintf("old cp[%d] = %f\n", i, cplist->cp);
		cp[i] = sqrt(cplist->cp * (cplist->forward)->cp);
    //Rprintf("geometric cp[%d] = %f\n", i, cp[i]);
    //rescale the cp here:
    cp[i] = (n_xval - 1) * 1.0 / n_xval * cp[i];
    //Rprintf("scaled cp[%d] = %f\n", i, cp[i]);
	}
   //rescale alpha:
  rp.alpha *= (n_xval - 1) * 1.0 / n_xval;
 // Rprintf("rp.alpha = %f\n", rp.alpha);
  

	/* why we need to concern about wt> */
	total_wt = 0;
	for (i = 0; i < rp.n; i++)
		total_wt += rp.wt[i];
	old_wt = total_wt;

	/*
	 * do the validations
	 */
	k = 0;                      /* -Wall */
	for (xgroup = 0; xgroup < n_xval; xgroup++) {
		/*
		 * restore rp.sorts, with the data for this run at the top
		 * this requires one pass per variable
		 */
		for (j = 0; j < rp.nvar; j++) {
			k = 0;
			for (i = 0; i < rp.n; i++) {
				ii = savesort[j * rp.n + i];
				if (ii < 0)
					ii = -(1 + ii);     /* missings move too */
				if (x_grp[ii] != xgroup + 1) { 
					// samples not belong to the test fold:
					/*
					 * this obs is left in --
					 *  copy to the front half of rp.sorts
					 */
					rp.sorts[j][k] = savesort[j * rp.n + i]; // the reason to store in savesort
					k++;
				}
			} 
		}

		/*
		 *  Fix up the y vector, and save a list of "left out" obs *   in
		 * the tail, unused end of rp.sorts[0][i];
		 */
		last = k;

		k = 0;
		temp = 0;
		temp1 = 0;
		for (i = 0; i < rp.n; i++) {
			rp.which[i] = 1;    /* everyone starts in group 1 */
			if (x_grp[i] == xgroup + 1) {
        //Rprintf("validation data is %d\n", i + 1);
				rp.sorts[0][last] = i;
				last++;
			} else {
				rp.ytemp[k] = rp.ydata[i];
				rp.wtemp[k] = rp.wt[i];
				temp += rp.wt[i];
				temp1 += 1;
				k++;
			}
		}
  
    
    //for (j = 0; j < rp.num_unique_cp; j++) {
    //  cp[j] *= temp1 / rp.n;
    //  Rprintf("after: cp[%d] = %f\n", j, cp[j]);
    //}
       
    //rp.alpha *= temp1 / rp.n;
    // we choose not to rescale the cp in cross validation:
    old_wt = temp;


		/*
		 * partition the new tree
		 */
		xtree = (pNode) CALLOC(1, nodesize);
		xtree->num_obs = k;
		(*rp_init) (k, rp.ytemp, maxcat, errmsg, parms, &temp, 2, rp.wtemp);
		//(*rp_eval) (k, rp.ytemp, xtree->response_est, &(xtree->risk), rp.wtemp);
        (*rp_eval) (k, rp.ytemp, xtree->response_est, &(xtree->risk), rp.wtemp, rp.max_y);
		xtree->complexity = xtree->risk;
    //Rprintf("xtree->complexity = %f\n", xtree->complexity);
		//partition(1, xtree, &temp, 0, k);

    partition(1, xtree, &temp, 0, k, parms);
    
    //Rprintf("now, xtree->complexity = %f\n", xtree->complexity);
		//the complexity should be min(me, any-node-above-me). This routine fixes that.
		fix_cp(xtree, xtree->complexity);
    //Rprintf("after fixation, xtree->complexity = %f\n", xtree->complexity);
    //Rprintf("cv_count = %d\n\n", ++cv_count);
		
    /*
		 * run the extra data down the new tree
		 */
     
		for (i = k; i < rp.n; i++) {
      j = rp.sorts[0][i]; // left-out samples for testing
			//rundown(xtree, j, cp, xpred, xtemp); 
      // for testing only
      //Rprintf("validation %d ", j+1);
     // Rprintf("x1 variable %f ", rp.xdata[0][j]);
      
      if (p < 0) {
       // Rprintf("--matching: ");
        //matching method:
        neighbor = findNeighbor(j, k); 
        //Rprintf("k = %d.\n", k);
        
        //Rprintf("its neighbor %d\n", neighbor+1);
        rundown3(xtree, j, neighbor, cp, xpred, xpred2, xtemp);
        // Rprintf("the error is %f\n", xtemp[]);
        
       
      } else {
        // TOT method:
        //Rprintf("--TOT: ");
        rundown(xtree, j, cp, xpred, xtemp, p);
      }
     
#if DEBUG > 1
			if (debug > 1) {
				jj = j + 1;
				Rprintf("\nObs %d, y=%f \n", jj, rp.ydata[j][0]);
			}
#endif
			/* add it in to the risk */
			cplist = cptable_head;
			for (jj = 0; jj < rp.num_unique_cp; jj++) {
				//cplist->xrisk += xtemp[jj] * rp.wt[j];
				cplist->xrisk += xtemp[jj];
				//cplist->xstd += xtemp[jj] * xtemp[jj] * rp.wt[j];
        cplist->xstd += xtemp[jj] * xtemp[jj];
        
#if DEBUG > 1
			//	if (debug > 1)
					Rprintf("  cp=%f, pred=%f, xtemp=%f\n",
							cp[jj] , xpred[jj], xtemp[jj]);
#endif
				cplist = cplist->forward;
			}
      // debug only:
      //round ++;
		}
    //Rprintf("%d cv round!\n", round);
    //Rprintf("count = %d\n", count);
		free_tree(xtree, 1);    // Calloc-ed
		R_CheckUserInterrupt();
	}

	for (cplist = cptable_head; cplist; cplist = cplist->forward) {
		//cplist->xstd = sqrt(cplist->xstd -
		//		cplist->xrisk * cplist->xrisk / total_wt);
    cplist->xstd = sqrt(cplist->xstd -
  			cplist->xrisk * cplist->xrisk / rp.n);
	}
	rp.alpha = alphasave;
	for (i = 0; i < rp.n; i++)
		rp.which[i] = savew[i];
	Free(savew);
	Free(xtemp);
}
