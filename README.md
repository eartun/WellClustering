# Well Clustering
The repository was created for the peer-reviewed publcation:

Lobut, B., Artun, E. (2024). Enhancing economic sustainability in mature oil fields: insights from the clustering approach to select candidate wells for extended shut-in. _Artificial Intelligence in Geosciences_ Under review.

Input Files:
- data.csv: Main data file which includes 179 oil wells with normalized coordinates, reservoir properties and performance characteristics
- Scnearios.xlsx: Results of the shut-in scenarios for economic evaluation

*R Session Information:*
R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
[3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] knitr_1.46       ggplotify_0.1.2  pheatmap_1.0.12  gridExtra_2.3    factoextra_1.0.7
 [6] cluster_2.1.6    ggpubr_0.6.0     pastecs_1.4.2    caret_6.0-94     lattice_0.20-45 
[11] readxl_1.4.3     lubridate_1.9.3  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
[16] purrr_1.0.2      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1     ggplot2_3.5.1   
[21] tidyverse_2.0.0 

loaded via a namespace (and not attached):
 [1] colorspace_2.1-0     ggsignif_0.6.4       class_7.3-20         htmlTable_2.4.2     
 [5] base64enc_0.1-3      fs_1.6.3             rstudioapi_0.16.0    listenv_0.9.1       
 [9] farver_2.1.1         ggrepel_0.9.5        prodlim_2023.08.28   fansi_1.0.6         
[13] codetools_0.2-18     splines_4.2.2        cachem_1.0.8         Formula_1.2-5       
[17] pROC_1.18.5          broom_1.0.5          compiler_4.2.2       backports_1.4.1     
[21] assertthat_0.2.1     Matrix_1.5-1         fastmap_1.1.1        cli_3.6.2           
[25] htmltools_0.5.8.1    tools_4.2.2          gtable_0.3.5         glue_1.7.0          
[29] reshape2_1.4.4       Rcpp_1.0.12          carData_3.0-5        cellranger_1.1.0    
[33] vctrs_0.6.5          nlme_3.1-160         iterators_1.0.14     timeDate_4032.109   
[37] gower_1.0.1          xfun_0.43            globals_0.16.3       timechange_0.3.0    
[41] lifecycle_1.0.4      rstatix_0.7.2        future_1.33.2        MASS_7.3-58.1       
[45] scales_1.3.0         ipred_0.9-14         ragg_1.3.0           hms_1.1.3           
[49] parallel_4.2.2       RColorBrewer_1.1-3   memoise_2.0.1        yulab.utils_0.1.4   
[53] rpart_4.1.23         stringi_1.8.3        foreach_1.5.2        checkmate_2.3.1     
[57] hardhat_1.3.1        boot_1.3-28          lava_1.8.0           rlang_1.1.3         
[61] pkgconfig_2.0.3      systemfonts_1.0.6    evaluate_0.23        recipes_1.0.10      
[65] htmlwidgets_1.6.4    labeling_0.4.3       cowplot_1.1.3        tidyselect_1.2.1    
[69] parallelly_1.37.1    plyr_1.8.9           magrittr_2.0.3       R6_2.5.1            
[73] generics_0.1.3       Hmisc_5.1-2          pillar_1.9.0         foreign_0.8-83      
[77] withr_3.0.0          survival_3.4-0       abind_1.4-5          nnet_7.3-18         
[81] future.apply_1.11.2  crayon_1.5.2         car_3.1-2            utf8_1.2.4          
[85] tzdb_0.4.0           rmarkdown_2.26       tornado_0.1.3        grid_4.2.2          
[89] data.table_1.15.4    ModelMetrics_1.2.2.2 digest_0.6.35        textshaping_0.3.7   
[93] gridGraphics_0.5-1   stats4_4.2.2         munsell_0.5.1   
