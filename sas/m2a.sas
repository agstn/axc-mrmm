proc import datafile='h:\tlc3_l.csv' out=tlc3_l replace; run;

ods html file='h:\m2a.html' style=HTMLEncore;

proc glimmix data=tlc3_l noclprint = 10;
 class id week (ref = first) follow (ref=first) trt (ref = last);
 model lead = trt follow follow*trt / solution ddfm=satterthwaite;
 random _residual_ / subject=id type=un;
 lsmeans trt*follow / slicediff=follow cl;
run;

ods html close;