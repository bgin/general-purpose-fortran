           program demo_atleast
            use M_strings, only : atleast
            implicit none
            character(len=10)            :: string='abcdefghij'
            character(len=:),allocatable :: answer
               answer=atleast(string,5)
               write(*,'("[",a,"]")') answer
               answer=atleast(string,20)
               write(*,'("[",a,"]")') answer
           end program demo_atleast
