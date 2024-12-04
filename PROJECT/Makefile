.PHONY: clean

./report.pdf: ./report.tex \
		Figure/heatplot.png \
		Figure/intensityplot_ACA.png \
		Figure/intensityplot_CA3.png \
		Figure/intensityplot_DG.png \
		Figure/intensityplot_LS.png \
		Figure/ROC_logit.png \
		Figure/ROC_svm.png \
		Figure/ROC_xgb.png \
		Figure/spikeplot.png \
	pdflatex ./report.tex
# unable to render reference in the first run
	pdflatex ./report.tex

Figure/heatplot.png: heatplot.R
	Rscript r1variable.py

output/2gene_space_norm.pdf output/2mean_count_norm.pdf output/2mean_count.pdf: r2mean.py
	python3 r2mean.py

output/3gpca_params.pdf output/3pca_gpca_compare.pdf: r3cluster.py
	python3 r3cluster.py

clean: 
	rm output/*
