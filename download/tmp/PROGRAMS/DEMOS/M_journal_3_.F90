              program demo_journal
              !! showing used for creating debug messages
              !! debug messages to a specified trail file
              use M_journal, only : journal
              implicit none
              ! produces no output because trail is not on
              call journal('D','*demo* DEBUG MESSAGE 001')
              call journal(.true.,'debug')
              ! produces output on stdout because debug mode is on but no named trail file
              call journal('D','*demo* DEBUG MESSAGE 002')
              ! open trail file
              call journal('O','mytrail.txt')
              ! debug messages now go to the trail file
              call journal('D','*demo* DEBUG MESSAGE 003')
              ! closed trail file so messages go to stdout again
              call journal('O','')
              call journal('D','*demo* DEBUG MESSAGE 004')
              call journal(.false.,'debug')
              ! back to no output from the next message
              call journal('D','*demo* DEBUG MESSAGE 005')
              end program demo_journal
