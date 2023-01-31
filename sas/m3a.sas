proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m3a.html' style=HTMLEncore;

proc glimmix data=tlc3_l noclprint = 10;
 where week ^= 0;
 class id week (ref = first) trt (ref = last);
 model diff = trt / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
 lsmeans trt / diff cl;
run;

ods html close;