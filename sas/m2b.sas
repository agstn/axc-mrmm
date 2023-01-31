proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m2b.html' style=HTMLEncore;

proc glimmix data=tlc3_l noclprint = 10;
 class id week (ref = first)  trt (ref = last);
 model lead = trt week trt*week / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
 lsmeans week*trt / slicedff=week cl;
run;

ods html close;