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

datsum.txt: datsum.R
	Rscript datsum.R

Figure/heatplot.png: heatplot.R
	Rscript heatplot.R

Figure/ROC_logit.png Figure/ROC_svm.png Figure/ROC_xgb.png: ROCcurve.R
	Rscript ROCcurve.R

Figure/intensityplot_ACA.png Figure/intensityplot_CA3.png Figure/intensityplot_DG.png Figure/intensityplot_LS.png: intensityplot.R
	Rscript intensityplot.R

Figure/spikeplot.R: spikeplot.R
	Rscript spikeplot.R

clean: 
	rm output/*
