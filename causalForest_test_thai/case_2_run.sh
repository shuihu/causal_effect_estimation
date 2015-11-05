for a in 6 12 15 20 30 40 
do
    for ((d=2; d<=8 ; d++)) 
    do
        if [ $d -eq 7 ]
        then
            d=8
        fi
        fnm="/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/Test_Run_Results/case_2_progress_10000_step$d$a.out"
        echo $fnm
        nohup nice R CMD BATCH --no-save --no-restore "--args $d $a" case_2_step_simu.R $fnm &
    done
done
