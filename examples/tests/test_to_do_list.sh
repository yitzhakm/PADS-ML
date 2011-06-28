darwin.i386/test_to_do_list  ../../TODO.txt | sed -e 's/\/\\n\[ \\t\]\*\\n\//\
\
/g' -e 's/\/ \*\//  /g' -e 's/\/---+ \*\//----- /g' -e 's/\/ \*-\*\///g'
