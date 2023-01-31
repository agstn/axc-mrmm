proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m2c.html' style=HTMLEncore;

proc glimmix data=tlc3_l noclprint = 10;
 class id week (ref = first) trt (ref = last);
 model lead = follow follow*trt / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
run;

ods html close;