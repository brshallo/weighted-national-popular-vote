# weighted-national-popular-vote

'weighted-popular-vote.R' script calculates both weighted and unweighted national popular vote totals. See script for details on data sources. 

These calculations were used to support notes in recent blog post [Compromise Between the Electoral College and a National Popular Vote](https://www.bryanshalloway.com/2020/09/11/compromise-between-the-electoral-college-and-a-national-popular-vote/). 

Note that calculations *do* include write-in votes -- hence vote totals can be *slightly* different than those reported in other election results reporting that frequently does not include these votes (e.g. on Wikipedia).

I also got lazy with reproducibility: 

* The relevant chart was manually saved from the Rstudio viewer
* There is a `knitr::kable()` output that I just copy and pasted into the article

:-( 
:-/