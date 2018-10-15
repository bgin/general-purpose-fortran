   program fifo
   implicit none
   use M_fixedform, only : fixedform, loaddata, icount_ptr, page_ptr, page_pd, icount_pd
   character(len=:),allocatable) :: tabs
   implicit none
      call make_data()
      page_ptr=>page_pd
      icount_ptr=>icount_pd
      call loaddata('test.dat')      ! fill the page(*) with user data
      call fixedform(tabs)
      write(*,*)tabs
   end program fifo
   subroutine make_data()
    open(unit=10,file='test.dat')
    write(10,'(a)')[character(len=80) ::
   '@    The simplest use of FIXEDFORM is when a text file is used to define a     @', &
   '@    form to be generated much like it could be drawn on paper:                @', &
   '################################################################################', &
   '#                                                                              #', &
   '#  ~ A basic form definition:         ~  ^ RED                                 #', &
   '#  ~ o Underlines become input fields ~  ^ WHITE                               #', &
   '#  ~ o Up-carets become menu options  ~  ^ BLUE                                #', &
   '#  ~ o Pound characters define boxes  ~                                        #', &
   '#  ~ o Text otherwise displays as-is  ~  Connected by pound characters or      #', &
   '#  ~   for the most part.             ~  adjacent to one another, up-carets    #', &
   '#  Name:  ___________________            form a radio button.                  #', &
   '#  Date:  ___________________            #######################               #', &
   '#  Value: ___________________            ^      ^       ^      ^               #', &
   '#                                       EAST   WEST   NORTH  SOUTH             #', &
   '#                                                                              #', &
   '# When the cursor is over a menu item it is toggled by pressing the space bar. #', &
   '# A tab character moves to the next selectable item. Typing in an input value  #', &
   '# changes the value. When the form is complete use the ctrl-S keys to submit.  #', &
   '################################################################################' ]
   end subroutine make_data
