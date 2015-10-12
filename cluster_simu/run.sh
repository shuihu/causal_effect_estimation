for ((d=2;d<=8;d++)) 
do
  fnm="progress$d.out"
  echo $fnm
  R CMD BATCH --no-save --no-restore "--args $d" sigmoid_snow_run.R $fnm &
done
