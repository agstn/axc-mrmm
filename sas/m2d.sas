proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m2d.html' style=HTMLEncore;

data tlc3_l;
 set tlc3_l;
 if week = "4" then week_4 = 1; else week_4 = 0;
 if week = "6" then week_6 = 1; else week_6 = 0;
run;

proc glimmix data=tlc3_l noclprint = 10;
 class id week (ref = first)  trt (ref = last);
 model lead = week_4 week_6 week_4*trt week_6*trt / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
run;

ods html close;