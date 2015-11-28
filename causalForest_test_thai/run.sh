for ((Jsam=4; Jsam<=8; Jsam++))
do
    for ((d=2; d<=8 ; d++)) 
    do
        if [ $d -eq 7 ]
        then
            d=8
        fi
        fnm="Test_Run_Results/Test_case_3_1128_step_0.5_$d$Jsam.out"
        echo $fnm
        nohup nice R CMD BATCH --no-save --no-restore "--args $d $Jsam" case_3_step_simu.R $fnm &
    done
done
