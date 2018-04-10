
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=merged2.pdf Figure* S*






ls Figure* | sed -e 's/Figure\([0-9a-z]*\).*/\1/'

ls * | sed -e 's/[A-Za-z]*\([0-9a-z]*\).*/\1/'

ls * | sed -e 's/\([A-Za-z]*\).*/\1/'

convert -size 560x85 xc:transparent -draw "text 10,55 'Linsdfuxsfd and Life'" -append linuxandlife.png

convert xc:transparent linuxandlife.png  -draw "text 10,10 'HeLLO'"  out.png



rm proper_* supp.pdf main.pdf merged.pdf 
file_ls=$(ls *)
for i in $file_ls; do
	echo +++++++++++++++++++++++++++++
	echo $i
	file_type=$(echo $i | sed -e 's/\([A-Za-z]*\).*/\1/')
	ver=$(echo $i | sed -e 's/[A-Za-z]*\([0-9a-z]*\).*/\1/')
	if [ $file_type = 'S' ]; then
		supp="Supplementary"
	else
		supp=""
	fi
	echo $supp Figure $ver | convert label:@- label.pdf
	#convert -quality 100% -size 1080x1080 $i label.pdf -append proper_$file_type\_$ver.pdf
	#convert $i label.pdf -coalesce proper_$file_type\_$ver.pdf
	convert linuxandlife.png xc:transparent -draw "text 10,10 'HeLLO'"  out.pdf
	rm label.pdf
done


pdfjam proper_Figure_3* --nup 2x1 --landscape --outfile temp.pdf
rm proper_Figure_3*
mv temp.pdf proper_Figure_3_Troph.pdf
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=merged.pdf $(ls -1 proper_* | sort -V)
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=main.pdf $(ls -1 proper_Figure* | sort -V)
gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=supp.pdf $(ls -1 proper_S* | sort -V)



