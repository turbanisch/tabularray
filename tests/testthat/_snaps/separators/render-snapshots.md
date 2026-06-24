# row_group_style presets

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {lrr}
              }
              \toprule
              country     & gdp   & population \\ 
              \midrule
              Germany     & 0.17  & 11.00      \\ 
              France      & 17.00 & 5.00       \\ 
              \midrule
              China       & 0.23  & 7.30       \\ 
              Afghanistan & 11.30 & 123.11     \\ 
              Taiwan      & 2.40  & 33.00      \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

