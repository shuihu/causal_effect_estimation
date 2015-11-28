for ((d=2; d<=8 ; d++)) 
do
    if [ $d -eq 7 ]
    then
        d=8
    fi
    fnm="Test_Two_Forests/Test_case_3_two_forests_1113_0.5_step_$d.out"
    echo $fnm
    nohup nice R CMD BATCH --no-save --no-restore "--args $d " two_forests_step_simu.R $fnm &
done
