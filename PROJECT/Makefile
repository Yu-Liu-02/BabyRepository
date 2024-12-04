.PHONY: clean

./report.pdf: ./main.tex \
	./heatplot.png \
	./intensityplot_ACA.png \
	./intensityplot_CA3.png \
	./intensityplot_DG.png \
	./intensityplot_LS.png \
	./ROC_logit.png \
	./ROC_svm.png \
	./ROC_xgb.png \
	./spikeplot.png 
	pdflatex ./main.tex
# unable to render reference in the first run
	pdflatex ./main.tex

datsum.txt: datsum.R
	Rscript datsum.R

./heatplot.png: heatplot.R
	Rscript heatplot.R

./ROC_logit.png ./ROC_svm.png ./ROC_xgb.png: ROCcurve.R
	Rscript ROCcurve.R

./intensityplot_ACA.png ./intensityplot_CA3.png ./intensityplot_DG.png ./intensityplot_LS.png: intensityplot.R
	Rscript intensityplot.R

./spikeplot.R: spikeplot.R
	Rscript spikeplot.R

clean: 
	rm output/*
