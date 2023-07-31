This is a replication repository for the article *[A Note on the M6 Forecasting Competition: Rank Optimization](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4527154)*.
To replicate the analysis:

1. Install dependencies in `requirements.txt`
2. Run the analysis via either:
    -  If you are a DVC user, simply run `dvc exp run` in the console or run the `run.bat` file.
    -  If not, you can also run all the files manually in the following order:
        1. `src\process_data.R`
        2. `src\estimate_coefficients.R`
        3. `src\compute_strategy.R`
        4. `src\run_competition_simulated.R`
        5. `src\run_competition_bootstrapped.R`
        6. `src\auxiliary\leaderboard_comparison.R`
        7. `src\auxiliary\strategy_diagram.R`
        8. `src\auxiliary\scaling_effects.R`
        9. `src\auxiliary\position_effects.R`
        10. `src\auxiliary\competition_statistics_simulated.R`
        11. `src\auxiliary\competition_statistics_bootstrapped.R`
3. To create the pdf, compile the file `latex\article_ssrn\article_ssrn.tex`

