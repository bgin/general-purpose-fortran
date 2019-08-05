       program demo_process_readall use M_process ,only: process_readall use M_strings ,only: split implicit none

              integer
                     :: ierr

              integer
                     :: i character(len=:),allocatable :: string character(len=:),allocatable :: array(:) string=process_readall('ls',delim=NEW_LINE("A"),ierr=ierr) call
                     split(string,array,delimiters=NEW_LINE("A")) do i=1,size(array) write(*,'(i0,t10,"[",a,"]")')i,trim(array(i)) enddo end program demo_process_readall

       Results:

       1      [Articles]

              2      [LIBRARY]

              3      [PC]

              4      [SHIP]

              5      [SPEC]

              6      [crib.dat]

              7      [doc]

              8      [html]

              9      [index.html]

              10     [plan.txt]

              11     [questions]

              12     [scripts]

              13     [tmp]

   SEE ALSO
       M_process(3fm)

                                                                                        August 04, 2019                                                                     process_readall(3)
