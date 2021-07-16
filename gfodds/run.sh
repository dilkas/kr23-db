#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(empirical_max_reduction),asserta(unify_avg_var_at_iteration_boundary),asserta(bidirectional_reduction),asserta(bidirectional_reduction_high_first),planit([2],'\''Elevators/elevators2_policy_2_ve_max_unify_bi_high'\''), halt' > Elevators/results_2_ve_max_unify_bi_high

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_max_unify_bi_high'\''), halt' >> Elevators/results_2_ve_max_unify_bi_high

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(empirical_max_reduction),asserta(unify_avg_var_at_iteration_boundary),asserta(bidirectional_reduction),planit([2],'\''Elevators/elevators2_policy_2_ve_max_unify_bi_low'\''), halt' > Elevators/results_2_ve_max_unify_bi_low

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_max_unify_bi_low'\''), halt' >> Elevators/results_2_ve_max_unify_bi_low


#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(empirical_max_reduction),asserta(unify_avg_var_at_iteration_boundary),planit([2],'\''Elevators/elevators2_policy_2_ve_max_unify_uni'\''), halt' > Elevators/results_2_ve_max_unify_uni

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_max_unify_uni'\''), halt' >> Elevators/results_2_ve_max_unify_uni


#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(unify_avg_var_at_iteration_boundary),planit([2],'\''Elevators/elevators2_policy_2_ve_unify_uni'\''), halt' > Elevators/results_2_ve_unify_uni

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_unify_uni'\''), halt' >> Elevators/results_2_ve_unify_uni

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),planit([2],'\''Elevators/elevators2_policy_2_ve_uni'\''), halt' > Elevators/results_2_ve_uni

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_uni'\''), halt' >> Elevators/results_2_ve_uni

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(unify_avg_var_at_iteration_boundary),asserta(bidirectional_reduction),asserta(bidirectional_reduction_high_first),planit([2],'\''Elevators/elevators2_policy_2_ve_unify_bi_high'\''), halt' > Elevators/results_2_ve_unify_bi_high

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_unify_bi_high'\''), halt' >> Elevators/results_2_ve_unify_bi_high

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(unify_avg_var_at_iteration_boundary),asserta(bidirectional_reduction),planit([2],'\''Elevators/elevators2_policy_2_ve_unify_bi_low'\''), halt' > Elevators/results_2_ve_unify_bi_low

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Elevators/elevators2_policy_2_ve_unify_bi_low'\''), halt' >> Elevators/results_2_ve_unify_bi_low

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),planit([4],'\''Inventory/ic_4_1_policy_4_ve_new'\''), halt'

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),asserta(random_policy),testit('\''Inventory/ic_4_1_policy_4_ve'\''), halt'

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),planit([2],'\''Inventory_Advanced/ic_2_1_policy_4_ve_new'\''), halt'

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination),testit('\''Inventory_Advanced/ic_2_1_policy_4_ve'\''), halt'

#yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'asserta(variableElimination), asserta(ve_by_mdd), asserta(debug(ve)), doit, halt' > baghuya_ve_mdd2

yap -s 1500000 -h 1500000 -l 500000 -l foddYapADE.yap -g 'planit([4],'\''Inventory_Advanced/ic_2_1_policy_2_ve_no_reductions'\''), halt'

