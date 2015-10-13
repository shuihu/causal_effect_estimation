for ((d=2; d<=8 ;d++)) 
do
  if [ $d -eq 7 ]
  then
    d=8
  fi
  fnm="progress_step$d.out"
  echo $fnm
  nohup nice R CMD BATCH --no-save --no-restore "--args $d" step_simu.R $fnm &
done
