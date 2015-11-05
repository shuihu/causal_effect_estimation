for ((a=1; a<=9; a++))
do
    for d in 2 5 10 15 20 30
    do
        fnm="/farmshare/user_data/thaipham/R-Simulation/causal_effect_estimation/causalForest_test_thai/Test_Run_Results/case_1_progress_step$d$a.out"
        echo $fnm
        nohup nice R CMD BATCH --no-save --no-restore "--args $d $a" case_1_step.R $fnm &
    done
done
