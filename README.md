# Replication Code

This repository holds the code for "Different signals, same message? Evidence of mainstream accommodation of the radical right on European Integration using parliamentary speeches", by Leandre Benoit and Duarte Amaro.

In the `final_regression` folder, the file `regression_prep.md` corresponds to the data processing pipeline taking the machine-coded parliamentary speeches and yielding the dataframe used in the regression analysis (`reg_data_tri`). The file `regression_final.md` then takes this processed data and conducts the regression analysis. It also creates descriptive statistics and, for the benchmarking of our measure against the Chapel Hill Expert Survey, uses yearly positional estimates, created in `regression_prep.md`. 

Three .CSV files are also included in the `final_regression` folder: `reg_data_tri.csv`, corresponding to the data used in the regression analysis, `data_trimester.csv`, corresponding to a lighter data set used for descriptive statistics, and `data_yearly.csv`, corresponding to the yearly data used for descriptive statistics and cross-validating the measure.

The fine-tuned LLMs can be found on https://huggingface.co/LBenoit and access can be requested at leandre.benoit@politics.ox.ac.uk. 

Both Markdown files render with corresponding tables and images. RMarkdown versions in raw code are also displayed.
