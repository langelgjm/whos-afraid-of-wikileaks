# Instructions

This repository includes the data and code used to produce the figures, tables, and statistics for the article "Who's Afraid of Wikileaks? Missed Opportunities in Political Science Research." It also includes the LaTeX code and bibliography file used to produce the article manuscript. You can use this repository to replicate the findings of the article.

If you want to reproduce the countries.csv file in this repository, you should obtain the August 30, 2013 text of the leaked intellectual property chapter of the Trans-Pacific Partnership, which is available at http://www.wikileaks.org/tpp. Save this text in a file, then run extract_country_codes.py and specify this file as the sole argument.

If you want to reproduce the figures and tables used in the article, you should run leaks_code.R. Note that it will overwrite files without warning, so you should probably run it in its own directory.

The files are heavily commented, so you should examine them for more details.
