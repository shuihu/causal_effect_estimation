for ((d=2; d<=8 ;d++)) 
do
  if [ $d -eq 7 ]
  then
    d=8
  fi
  fnm="progress$d.out"
  echo $fnm
  nohup nice R CMD BATCH --no-save --no-restore "--args $d" sigmoid_serial_run.R $fnm &
done
