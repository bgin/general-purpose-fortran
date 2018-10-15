!-----------------------------------------------------------------------------------------------------------------------------------
! USH must pass on page size;
! FILENAME must be stored by USH so last one can be restored

program help
   use M_journal, only: journal
   use M_kracken,   only: kracken, iget, sget, lget
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('help','&
      & -oo &
      & -t &
      & -f & 
      & -h .f. --help .f.    & 
      & -v .f. --version .f. &
      & -topics .false.      &
      & -all .false.         &
      & -summaries .false.   &
      &') ! crack command line
   !--------------------------------------------------------------------------------------------------------------------------------
   if( lget('help_h').or.lget('help_help'))then
      write(*,'(a)')'================================================================================'
      write(*,'(a)')' Display specially formatted help text files.'
      write(*,'(a)')' '
      write(*,'(a)')' [helpg [TOPIC |-t SEARCH_STRING] [-f INPUT_FILE]]|[-all|-topics|-summaries]'
      write(*,'(a)')'--------------------------------------------------------------------------------'
      write(*,'(a)')' -f INPUT_FILE'
      write(*,'(a)')' Set the filename to read descriptions from. The default input file is the value'
      write(*,'(a)')' of the environment variable $USHHELP if it is set. The built-in default is     '
      write(*,'(a)')' /usr/share/ush/help.txt'
      write(*,'(a)')'================================================================================'
      write(*,'(a)')' helpg -topics | more     # show all topic names                                '
      write(*,'(a)')' helpg -summaries | more  # Show all topics and short abstracts                 '
      write(*,'(a)')' helpg -all | more        # display entire help file                            '
      write(*,'(a)')'================================================================================'
      write(*,'(a)')'================================================================================'
      stop
   endif
   !--------------------------------------------------------------------------------------------------------------------------------
   if( lget('help_version').or.lget('help_v'))then
      write(*,'(a)')'================================================================================'
      write(*,'(a)')' helpg : version 3.0 : 20130818 '
      write(*,'(a)')'================================================================================'
      stop
   endif
   !--------------------------------------------------------------------------------------------------------------------------------
   ! make:
   ! option to stay in help utility in a loop when called interactively as a seperate command

   ! assumes JUN*() procedures will write to the trail file properly
   ! assumes output file was specified with a redirect when called
   ! topic to read from descriptions file
   ! topic to search descriptions for
   ! get filename to read descriptions from
   call helpg(sget('help_oo'),sget('help_t'),sget('help_f'))
   contains
!-----------------------------------------------------------------------------------------------------------------------------------
   subroutine jun(i,string)
      implicit none
      integer,intent(in)                    :: i
      character(len=*),intent(in)           :: string
      write(*,*)string
   end subroutine jun
!-----------------------------------------------------------------------------------------------------------------------------------
   subroutine helpg(topic0,search0,helpdat0)
!@(#) version of procedure help with pause and graphics
!
!     start at the beginning of the description file and read until the desired topic is reached;
!     then display help until another topic begins
!
!     format of file is TOPIC:topic
!                       TOPIC:another topic line(s)
!
!                       detailed topic lines for commands (assumed not to exist if
!                       first letter of topic is uppercase)
!
!     if topic is ? show all topic lines
!     if topic is ??
!     if topic is ??? show all lines
!     if topic is blank, show all lines with TOPIC: and detailed TOPIC lines
!
!     if topic is not null, find a match (abbreviations allowed, leading blanks
!     significant) and display text from matched topic line to next line
!     beginning with TOPIC: (or EOF).
!

!     John S. Urban  12/08/1984
!     John S. Urban  05/25/1992
!     John S. Urban  06/11/1995  Complete rewrite so help can be generated from HTML 2.0 only
!     John S. Urban  08/18/2013  Updated and made a seperate program instead of a subroutine
!
   use M_strings,only : upper
   character(len=*),intent(in)   :: topic0      ! topic name to location
   character(len=*),intent(in)   :: search0     ! search body of text for this string
   character(len=*),intent(in)   :: helpdat0    ! name of file containing help text. If blank use environment variable
                                                ! or "help.txt"

   character(len=250)            ::  s,topicx,topic,search

   character(len=79)             :: columns
   character(len=255)            :: helpdat=' '

   integer,parameter             :: IIN=1     ! unit number to use for reading
   integer,parameter             :: IMAX=240  ! it's time. Even 4010 emulators seem to support at least 80; Tektronix 4010 was  74
   integer                       :: i300
   integer                       :: icol
   integer                       :: istart
   integer                       :: iend
   integer                       :: ios
   integer                       :: ilen
   integer                       :: ishowed
   integer                       :: iwide
   integer                       :: j
!-----------------------------------------------------------------------------------------------------------------------------------
   topic=topic0(:min(len(topic),len(topic0)))                  ! topic can be changed by this procedure, so make copy
   search=search0(:min(len(search),len(search0)))              ! search can be changed by this procedure, so make copy
   helpdat=helpdat0(:min(len(helpdat),len(helpdat0)))          ! helpdat can be changed by this procedure, so make copy
!-----------------------------------------------------------------------------------------------------------------------------------
   iend=len_trim(helpdat)                                      ! find last non-blank character of filename
   if(iend.eq.0)then                                           ! if blank name, look in environment variable or use default name
      call get_environment_variable('USHHELP',helpdat)         ! set default name for helpfile
      if(helpdat.eq.' ')then
         helpdat='/usr/share/ush/help.txt'                     ! if default name if everything else resulted in blanks
      endif
      iend=len_trim(helpdat)                                   ! get length of filename
   endif
   close(IIN)
   open(unit=IIN,status='old',file=helpdat(:iend),iostat=ios)
   if(ios.ne.0)then
      call journal('sc','*help* unexpected i/o problems opening the help file')
      call journal('sc',helpdat)
      goto 999
   endif
   rewind IIN
!-----------------------------------------------------------------------------------------------------------------------------------
   if(topic.eq.' '.and.search.eq.' ')then
      topic='novice'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(search.ne.' ')then                                       ! show topics that have a certain string in them
      ilen=len_trim(search)
      search=upper(search)
      call journal('s','TOPIC')
      topicx=' '
      do
         read(IIN,'(a)',end=999,err=888)s
         if(s(1:6).eq.'TOPIC:'.and.s(7:9).ne.'...')topicx=s    ! hold  the topic line
         j=index(upper(s),search(:ilen))
         if(j.ne.0)then
            s(j:j+ilen-1)='#########################################'
            if(topicx.ne.' ')call journal('s',trim(topicx))
            call journal('s','      '//trim(s))
            topicx=' '
         endif
      enddo
   else
!-----------------------------------------------------------------------------------------------------------------------------------
      select case (topic)
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('???')                                             ! dump all of file
         do
            read(IIN,'(a)',end=999,err=888)s
            ilen=len_trim(s)
            ilen=min(ilen,IMAX)
            call journal('s',s(:ilen))
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('??')
         call journal('s','Available topics include:')
            READLINE: do
               read(IIN,'(a)',end=999,err=888)s                   ! look for topic line
               NEXT: do
                  if(s(1:6).eq.'TOPIC:')then
                     call journal('s','=========================================================================')
                     call journal('s',s(7:len_trim(s)))
                  else                                            ! show next paragraph after topic line
                     cycle READLINE
                  endif
                  do
                     read(IIN,'(a)',end=999,err=888)s             ! skip blank lines if any after TOPIC lines
                     if(s(1:6).eq.'TOPIC:')cycle NEXT             ! another TOPIC: line right after the one that began this block
                     if(s.eq.' ')then
                     elseif(s(1:4).eq.'====')then
                     else
                        exit
                     endif
                  enddo
                  do
                     ilen=len_trim(s)
                     ilen=min(ilen,IMAX)
                     call journal('s','-'//s(1:ilen))                   ! echo lines from detailed paragraph
                     read(IIN,'(a)',end=999,err=888)s             ! read and echo detailed description
                     if(s(1:6).eq.'TOPIC:')cycle NEXT             ! another TOPIC: line right after the one that began this block
                     if(s.eq.' ')then                             ! end of detailed paragraph, go get next TOPIC:
                        cycle READLINE
                     endif
                  enddo
                  exit NEXT
               enddo NEXT
            enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('?')                                                      ! dump all topic lines
         iwide=24
         OUTER: do
            columns='|'
            columns(79:79)='|'
            icol=3
            L300: do i300=1,3                                         ! look for topics in sets of up to three 
               L350: do
                  read(IIN,'(a)',iostat=ios)s                         ! read a line
                  if(is_iostat_end(ios))then                          ! if hit end (or error) exit
                     if(columns(2:78).ne.' ') call journal('s',columns)
                     call journal('s','*-----------------------------------------------------------------------------*')
                     goto 999
                  endif
                  if(ios.ne.0) goto 888                               ! some other error
                  if(s(1:6).eq.'TOPIC:')then                          ! found a topic line
                     if(upper(s).eq.s)then                            ! start a new line if topic is all capitals
                        if(columns(2:78).ne.' ') call journal('s',columns)
                        columns='| '//s(7:7+79-1)
                        columns(79:79)='|'
                        call journal('s','*-----------------------------------------------------------------------------*')
                        call journal('s',columns)
                        cycle OUTER
                     endif
                     columns(icol:icol+iwide)=s(7:7+iwide)            ! a TOPIC: line that is not all uppercase letters
                     icol=icol+iwide
                     columns(icol:icol)='|'
                     icol=icol+2
                     cycle L300                                       ! see about adding another topic
                  endif
               enddo L350
            enddo L300
            if(columns(2:78).ne.' ') call journal('s',columns)
         enddo OUTER
!-----------------------------------------------------------------------------------------------------------------------------------
      case default                                             ! display help text for a topic
         ilen=len_trim(topic)
         do
            read(IIN,'(a)',iostat=ios)s
            if(ios.ne.0)then                                            ! reached end of file or error on reading
               write(s,'(3a)')'*help* no entry for /',topic(:ilen),'/'
               ilen=len_trim(s)
               ilen=min(ilen,IMAX)
               call journal('s',s(:ilen))
               call journal('s','*help* maybe try help -t TOPIC?')
               goto 999
            endif
            if(s(7:ilen+6).eq.topic(:ilen).and.s(1:6).eq.'TOPIC:')then  ! found the topic line you were searching for
               call journal('s',trim(s(7:)))                  ! echo topic without TOPIC: field
               ishowed=0                                ! number of lines showed (if 0, its an adjacent ? line, so show it)
               do
                  read(IIN,'(a)',end=999,err=888)s      ! read a potential description, end of description, or continued topic line
                  if(s(1:6).eq.'TOPIC:'.and.ishowed.ne.0)then  ! end of description
                     goto 999
                  elseif(s(1:6).ne.'TOPIC:')then               ! regular line of text
                     ishowed=ishowed+1
                     istart=1
                  else
                     istart=7
                  endif
                  iend=istart+IMAX-1
                  iend=min(iend,IMAX)
                  iend=len_trim(s(:iend))
                  call journal('s',s(istart:iend))
               enddo
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call journal('sc','*help* ended without finding end')
   goto 999
 888  continue
   call journal('sc','*help* unexpected i/o problems on the help file')
   call journal('sc',helpdat)
   goto 999
!-----------------------------------------------------------------------------------------------------------------------------------
 999  continue
   close(IIN)
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine helpg
!-----------------------------------------------------------------------------------------------------------------------------------
end program help
!-----------------------------------------------------------------------------------------------------------------------------------
