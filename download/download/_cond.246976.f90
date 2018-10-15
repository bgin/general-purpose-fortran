program logic
!-----------------------------------------------------------------------------------------------------------------------------------
   use M_strings, only         : lower, delim, v2s  ! convert character case and split a string
   use M_logic, only           : nest_level, cond, write
   use M_debug, only           : unit_check, unit_check_good, unit_check_bad, unit_check_start
   use M_calculator_plus, only : inum0, rnum0
   ! WRITE         flag whether current data lines should be written
   ! NEST_level    nesting level for if/elseif/else/endif
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1024)              :: line                ! input line
   integer,parameter                :: max_words=2         ! maximum number of words allowed on a line, really only need two
   character(len=1024)              :: array(max_words)    ! working copy of input line
   integer                          :: ibegin(max_words), iterm(max_words) ! location where words start and end
   integer                          :: icount
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('cond')                           ! Change database entry to indicate changes have begun
   READLINE: do                                            ! read loop to read input file
      read(*,'(a)',iostat=ios) line
      if(ios.ne.0)then
         if (nest_level.ne.0) then                         ! check to make sure all if blocks are closed
            write(*,*)'*logic* error - IF BLOCK NOT CLOSED WHEN READiNG FILE WAS FINISHED.'
         endif
         exit READLINE
      endif
      ! just parsing the first word out and finding where second word starts although delim(3f) can do more
      array=' ' ! make sure array is initialized for when icount(number of words on line) is zero
      call delim(lower(line),array,max_words,icount,ibegin,iterm,ilen,' ')
      select case(array(1))
      case('if','else','elseif','endif')                            ! find conditional lines
         call cond(trim(array(1)),line(iterm(1)+1:),ierr_logic)     ! process conditional directive
      case default                                                  ! find input lines you want to use, skip others
         if (write) then                                            ! for example, if last conditional was true then write line
            if(index('!#',array(1)(1:1)).ne.0)then
               write(*,*)'COMMENT '//trim(line)
            else
               icount=icount+1
               value=rnum0(trim(line))
               write(*,'(i0.5,a)')icount, trim(line)//'===>'//v2s(value)           ! write data line
            endif
         else
            write(*,*)'SKIP '//trim(line)
         endif
      end select
   enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
!  assuming check_count is set to zero initially and  incremented only in the sections it should not be in get an error
!  if variable is not zero.
   icheck=inum0('check_count')
   call unit_check('cond',icheck.eq.0)
   write(*,*)'*QA*: CALCULATOR VARIABLE check_count is zero'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check('cond',nest_level.eq.0) ! unless input intentionally has an error, nesting level should be zero at end
   write(*,*)'*QA*: module variable nest_level is zero'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(line.eq.'! GOT TO END')then
      call unit_check_good('cond')  ! flag that got to end of test program
   else
      call unit_check_bad('cond') ! flag that got unexpected ending
   endif
   write(*,*)'*QA*: last line is '//trim(line)
end program logic
