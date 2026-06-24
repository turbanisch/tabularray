# README: simple table

    Code
      cat(as_latex(tblr(tatooine())))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {lrrlr}
              }
              \toprule
              name               & height & mass & sex    & birth\_year \\ 
              \midrule
              C\$PO              & 167    & 75   & none   & 112.00      \\ 
              Cliegg Lars        & 183    & NA   & male   & 82.00       \\ 
              Shmi Skywalker     & 163    & NA   & female & 72.00       \\ 
              Owen Lars          & 178    & 120  & male   & 52.00       \\ 
              Beru Whitesun Lars & 165    & 75   & female & 47.00       \\ 
              Darth Vader        & 202    & 136  & male   & 41.90       \\ 
              Anakin Skywalker   & 188    & 84   & male   & 41.90       \\ 
              Biggs Darklighter  & 183    & 84   & male   & 24.00       \\ 
              Luke Skywalker     & 172    & 77   & male   & 19.00       \\ 
              R5-D4              & 97     & 32   & none   & NA          \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

# README: marked-up grouped table

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{talltabs}[
              caption = {Starwars Creatures from Tatooine},
              remark{Note} = {Entry C3PO altered to test characters that have a special meaning in LaTeX.},
              remark{Source} = {R package \texttt{dplyr}}
              ]{
              colspec = {lX[r]X[r]X[r]},
              width = {0.7\linewidth}
              }
              \toprule
                                 & \SetCell[c=3]{c} All my vars     &      &                          \\ 
              \cmidrule{2-4}
                                 & \SetCell[c=2]{c} Group 1         &      & \SetCell[c=1]{c} Group 2 \\ 
              \cmidrule[r]{2-3} \cmidrule[l]{4}
                                 & Height                           & Mass & Birth Year               \\ 
              \midrule
                                 & \SetCell[c=3]{c} \textit{None}   &      &                          \\ 
              \cmidrule{2-4}
              C\$PO              & 167                              & 75   & 112.00                   \\ 
              R5-D4              & 97                               & 32   & NA                       \\ 
              \addlinespace
                                 & \SetCell[c=3]{c} \textit{Male}   &      &                          \\ 
              \cmidrule{2-4}
              Cliegg Lars        & 183                              & NA   & 82.00                    \\ 
              Owen Lars          & 178                              & 120  & 52.00                    \\ 
              Darth Vader        & 202                              & 136  & 41.90                    \\ 
              Anakin Skywalker   & 188                              & 84   & 41.90                    \\ 
              Biggs Darklighter  & 183                              & 84   & 24.00                    \\ 
              Luke Skywalker     & 172                              & 77   & 19.00                    \\ 
              \addlinespace
                                 & \SetCell[c=3]{c} \textit{Female} &      &                          \\ 
              \cmidrule{2-4}
              Shmi Skywalker     & 163                              & NA   & 72.00                    \\ 
              Beru Whitesun Lars & 165                              & 75   & 47.00                    \\ 
              \bottomrule
          \end{talltabs}
      \end{center}

# set_colspec example

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {ccX}
              }
              \toprule
              continent & country     & value \\ 
              \midrule
              Europe    & Germany     & 0.17  \\ 
              Asia      & China       & 0.23  \\ 
              Asia      & Afghanistan & 11.30 \\ 
              Europe    & France      & 17.00 \\ 
              Asia      & Taiwan      & 2.40  \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

# set_column_labels example

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {llr}
              }
              \toprule
              continent & COUNTRY     & \textbf{Value} \\ 
              \midrule
              Europe    & Germany     & 0.17           \\ 
              Asia      & China       & 0.23           \\ 
              Asia      & Afghanistan & 11.30          \\ 
              Europe    & France      & 17.00          \\ 
              Asia      & Taiwan      & 2.40           \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

# set_column_spanner example

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {llr}
              }
              \toprule
              \SetCell[c=2]{c} Region &             &       \\ 
              \cmidrule{1-2}
              continent               & country     & value \\ 
              \midrule
              Europe                  & Germany     & 0.17  \\ 
              Asia                    & China       & 0.23  \\ 
              Asia                    & Afghanistan & 11.30 \\ 
              Europe                  & France      & 17.00 \\ 
              Asia                    & Taiwan      & 2.40  \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

# set_source_notes example

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{talltabs}[
              caption = {A table with notes},
              remark{Source} = {Built-in R dataset},
              remark{Note} = {Comes with R.}
              ]{
              colspec = {llr}
              }
              \toprule
              continent & country     & value \\ 
              \midrule
              Europe    & Germany     & 0.17  \\ 
              Asia      & China       & 0.23  \\ 
              Asia      & Afghanistan & 11.30 \\ 
              Europe    & France      & 17.00 \\ 
              Asia      & Taiwan      & 2.40  \\ 
              \bottomrule
          \end{talltabs}
      \end{center}

# set_theme grouped example

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{booktabs}{
              colspec = {lrr}
              }
              \toprule
              country     & gdp                              & population \\ 
              \midrule
                          & \SetCell[c=2]{l} \textit{Europe} &            \\ 
              \cmidrule{2-3}
              Germany     & 0.17                             & 11.00      \\ 
              France      & 17.00                            & 5.00       \\ 
              \addlinespace
                          & \SetCell[c=2]{l} \textit{Asia}   &            \\ 
              \cmidrule{2-3}
              China       & 0.23                             & 7.30       \\ 
              Afghanistan & 11.30                            & 123.11     \\ 
              Taiwan      & 2.40                             & 33.00      \\ 
              \bottomrule
          \end{booktabs}
      \end{center}

# table types: float and break

    Code
      cat(as_latex(tblr(countries(), type = "float", caption = "Float")))
    Output
      \begin{center}
          \begin{talltabs}[
              caption = {Float}
              ]{
              colspec = {llr}
              }
              \toprule
              continent & country     & value \\ 
              \midrule
              Europe    & Germany     & 0.17  \\ 
              Asia      & China       & 0.23  \\ 
              Asia      & Afghanistan & 11.30 \\ 
              Europe    & France      & 17.00 \\ 
              Asia      & Taiwan      & 2.40  \\ 
              \bottomrule
          \end{talltabs}
      \end{center}

---

    Code
      cat(as_latex(tblr(countries(), type = "break", caption = "Break")))
    Output
      \begin{center}
          \begin{longtabs}[
              caption = {Break}
              ]{
              colspec = {llr}
              }
              \toprule
              continent & country     & value \\ 
              \midrule
              Europe    & Germany     & 0.17  \\ 
              Asia      & China       & 0.23  \\ 
              Asia      & Afghanistan & 11.30 \\ 
              Europe    & France      & 17.00 \\ 
              Asia      & Taiwan      & 2.40  \\ 
              \bottomrule
          \end{longtabs}
      \end{center}

# non-booktabs theme

    Code
      cat(as_latex(x))
    Output
      \begin{center}
          \begin{tblr}{
              colspec = {llr}
              }
              \hline
              continent & country     & value \\ 
              \hline
              Europe    & Germany     & 0.17  \\ 
              Asia      & China       & 0.23  \\ 
              Asia      & Afghanistan & 11.30 \\ 
              Europe    & France      & 17.00 \\ 
              Asia      & Taiwan      & 2.40  \\ 
              \hline
          \end{tblr}
      \end{center}

