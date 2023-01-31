proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m1b.html' style=HTMLEncore;

proc glimmix data=tlc3_l noclprint = 10;
 where week ^= 0;
 class id week (ref = first) trt (ref = last);
 model lead = trt base week week*trt / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
 lsmeans trt*week / slicediff=week cl;
run;

ods html close;