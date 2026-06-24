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
              Germany     & 0.17  & 11.0       \\ 
              France      & 17.00 & 5.0        \\ 
              \midrule
              China       & 0.23  & 7.3        \\ 
              Afghanistan & 11.30 & 123.1      \\ 
              Taiwan      & 2.40  & 33.0       \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

