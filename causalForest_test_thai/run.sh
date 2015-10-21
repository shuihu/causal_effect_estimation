for ((Jsam=1; Jsam<=9; Jsam++))
do
    for ((d=2; d<=8 ; d++)) 
    do
        if [ $d -eq 7 ]
        then
            d=8
        fi
        fnm="/Test_Run_Results/progress_step$d$Jsam.out"
        echo $fnm
        nohup nice R CMD BATCH --no-save --no-restore "--args $d $Jsam" step_simu.R $fnm &
    done
done
