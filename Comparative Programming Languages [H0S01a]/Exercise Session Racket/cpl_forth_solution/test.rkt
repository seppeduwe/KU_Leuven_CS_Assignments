#lang forth
: negate (n -- n')
   0 swap - 
;

123.6 negate .