type 'a proc = Par of 'a proc 
             |Sum of 'a sumproc 
and 'a parproc = ('a * 'a proc list [@seq])[@z]
and 'a sumproc = 'a parproc list [@z][@seq 2]
[@@spec]

