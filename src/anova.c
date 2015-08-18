/*
* The four routines for anova splitting
*/
#include "rpart.h"
#include "rpartproto.h"

static double *mean, *sums, *wtsums;
static double *wts;
static int *countn;
static int *tsplit;

// upper bound for y_max, only for debugging:
// static int MAX = 61;
 // only for debugging
//static int min_node_size = 2;


int
anovainit(int n, double *y[], int maxcat, char **error,
	  double *parm, int *size, int who, double *wt)
{
  if (who == 1 && maxcat > 0) {
	graycode_init0(maxcat);
	countn = (int *) ALLOC(2 * maxcat, sizeof(int));
	tsplit = countn + maxcat;
  // mean = (double *) ALLOC(3 * maxcat, sizeof(double));
	mean = (double *) ALLOC(4 * maxcat, sizeof(double));
	wts = mean + maxcat;
	sums = wts + maxcat;
  // change part
  wtsums = sums + maxcat;
    }
    *size = 1;
    return 0;
}

/*
* The anova evaluation function.  Return the mean and the ss.
*/
void
anovass(int n, double *y[], double *value, double *risk, double *wt, double max_y)
{
    int i;
    double temp = 0., temp0 = 0., temp1 = 0., twt = 0.; /* sum of the weights */
    double effect;
    // double ss;

    for (i = 0; i < n; i++) {
      temp1 += *y[i] * wt[i];
      temp0 += *y[i] * (1 - wt[i]);
	    twt += wt[i];
    }
    //mean = temp / twt;
    effect = temp1 / twt - temp0 / (n - twt);

    /* 
    ss = 0;
    for (i = 0; i < n; i++) {
      temp = *y[i] - mean;
	    ss += temp * temp * wt[i];
    } 
    */

    *value = effect;
    //*risk = 4 * n * MAX * MAX - effect * effect * n;
    //*risk = n * MAX * MAX - effect * effect * n;
    //max_y = MAX;
    *risk = n * max_y * max_y - n * effect * effect ;
}

/*
 * The anova splitting function.  Find that split point in x such that
 *  the sum of squares of y within the two groups is decreased as much
 *  as possible.  It is not necessary to actually calculate the SS, the
 *  improvement involves only means in the two groups.
 */
 
void
//anova(int n, double *y[], double *x, int nclass,
//     int edge, double *improve, double *split, int *csplit,
//      double myrisk, double *wt)
anova(int n, double *y[], double *x, int nclass,
    int edge, double *improve, double *split, int *csplit,
     double myrisk, double *wt, int parm)
{
    int i, j;
    double temp;
    double left_sum, right_sum;
    double left_wt_sum, right_wt_sum;
    double left_wt, right_wt;
    int left_n, right_n;
    double grandmean, best;
    int direction = LEFT;
    int where = 0;
    double node_effect, left_effect, right_effect;
    double left_temp, right_temp;
   // double min_node_size = parm[0];
    int min_node_size = parm;
    
  

   /*
    * The improvement of a node is SS - (SS_L + SS_R), where
    *   SS = sum of squares in a node = \sum w_i (x_i - \bar x)^2, where
    * of course \bar x is a weighted mean \sum w_i x_i / \sum w_i
    * Using the identity
    *    \sum w_i(x_ - \bar x)^2 = \sum w_i (x_i-c)^2 - (\sum w_i)(c-\bar x)^2
    * the improvement = w_l*(left mean - grand mean)^2
    *                  +w_r*(right mean- grand mean)^2
    * where w_l is the sum of weights in the left node, w_r similarly.
    */
    right_wt = 0;
    right_n = n;
    right_sum = 0;
    right_wt_sum = 0;
    for (i = 0; i < n; i++) {
      right_sum += *y[i];
      right_wt_sum += *y[i] * wt[i];
	    right_wt += wt[i];
    }
    //grandmean = right_sum / right_wt;
    temp = (right_wt_sum - right_sum * right_wt / n) /
                 ((1 - right_wt / n) * right_wt);
                 
    node_effect = temp * temp * n;


    if (nclass == 0) {
      /* continuous predictor */
      left_sum = 0;           /* No data in left branch, to start */
	    left_wt_sum = 0;
      left_wt = 0;
	    left_n = 0;
	    right_sum = 0;          /* after subracting grand mean, it's zero */
      right_wt_sum = 0;
	    best = 0;
	    for (i = 0; right_n > edge; i++) {
	      left_wt += wt[i];
	      right_wt -= wt[i];
	      left_n++;
	      right_n--;
	      temp = *y[i] * wt[i];
	      left_wt_sum += temp;
	      right_wt_sum -= temp;
        left_sum += *y[i];
        right_sum -= *y[i];
        
	      if (x[i + 1] != x[i] && left_n >= edge &&
            left_wt >= min_node_size &&
            left_n - left_wt >= min_node_size &&
            right_wt >= min_node_size &&
            right_n - right_wt >= min_node_size) {
              
              temp = (left_wt_sum - left_sum * left_wt / left_n) /
                 ((1 - left_wt / left_n) * left_wt);
              left_effect = temp * temp * left_n;
              
              temp = (right_wt_sum - right_sum * right_wt / right_n) /
                 ((1 - right_wt / right_n) * right_wt);
              right_effect = temp * temp * right_n;
          
              temp = left_effect + right_effect - node_effect;
              

              //temp = left_sum * left_sum / left_wt +
		          //right_sum * right_sum / right_wt;
		          if (temp > best) {
		              best = temp;
		              where = i;
                  left_temp = left_wt_sum / left_wt - (left_sum - left_wt_sum) / (left_n - left_wt);
                  right_temp = right_wt_sum / right_wt- (right_sum - right_wt_sum) / (right_n - right_wt);
                  
		              if (left_temp < right_temp)
			              direction = LEFT;
		              else
			              direction = RIGHT;
		          }
	    }
	   }

	  *improve = best / myrisk;
	  if (best > 0) {         /* found something */
	      csplit[0] = direction;
        *split = (x[where] + x[where + 1]) / 2; /* where to split!!!!!!!!! */ 
        }
  }
    /*
     * Categorical predictor
     */
    else {
      for (i = 0; i < nclass; i++) {
	      sums[i] = 0;
	      countn[i] = 0;
	      wts[i] = 0;
	    }

       /* rank the classes by their mean y value */
       /* RANK THE CLASSES BY THEI */
	    for (i = 0; i < n; i++) {
	        j = (int) x[i] - 1;
	        countn[j]++;
	        wts[j] += wt[i];
	        sums[j] += *y[i];
          // adding part
          wtsums[j] += *y[i] * wt[i];
	    }
    	for (i = 0; i < nclass; i++) {
	        if (countn[i] > 0) {
            tsplit[i] = RIGHT;
		        // mean[i] = sums[i] / wts[i];
            mean[i] = sums[i] / countn[i];
	        } else
            tsplit[i] = 0;
	    }
	    graycode_init2(nclass, countn, mean);

	/*
	 * Now find the split that we want
	 */
	left_wt = 0;
	left_sum = 0;
  left_wt_sum = 0;
	right_sum = 0;
  right_wt_sum = 0;
	left_n = 0;
	best = 0;
	where = 0;
	while ((j = graycode()) < nclass) {
	    tsplit[j] = LEFT;
	    left_n += countn[j];
	    right_n -= countn[j];
	    left_wt += wts[j];
	    right_wt -= wts[j];
	    left_sum += sums[j];
      left_wt_sum += wtsums[j];
	    right_sum -= sums[j];
      right_wt_sum -= wtsums[j];
	    if (left_n >= edge && right_n >= edge &&
          left_wt >= min_node_size &&
          left_n - left_wt >= min_node_size &&
          right_wt >= min_node_size &&
          right_n - right_wt >= min_node_size) {
            
            temp = (left_wt_sum - left_sum * left_wt / left_n) /
                 ((1 - left_wt / left_n) * left_wt);
            left_effect = temp * temp * left_n;
              
            temp = (right_wt_sum - right_sum * right_wt / right_n) /
                 ((1 - right_wt / right_n) * right_wt);
            right_effect = temp * temp * right_n;
          
            temp = left_effect + right_effect - node_effect;
                    
            
            //temp = left_sum * left_sum / left_wt +
		        //right_sum * right_sum / right_wt;
		        if (temp > best) {
		            best = temp;
                left_temp = left_wt_sum / left_wt - (left_sum - left_wt_sum) / (left_n - left_wt);
                right_temp = right_wt_sum / right_wt- (right_sum - right_wt_sum) / (right_n - right_wt);
		            
                if (left_temp > right_temp)
                  for (i = 0; i < nclass; i++) csplit[i] = -tsplit[i];
                else
                  for (i = 0; i < nclass; i++) csplit[i] = tsplit[i];
		        }
	      }
	 }

	*improve = best / myrisk;       /* % improvement */
  }
}
