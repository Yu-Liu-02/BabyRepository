.PHONY: clean

clean:
  rm derived_data/*
	rm logs/*
	rm figures/*

report: report.Rmd
	Rscript -e "rmarkdown::render('report.Rmd')"
