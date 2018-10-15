program main

!*****************************************************************************80
!
!! MAIN is the main program for HELP.
!
!  Discussion:
!
!    HELP reads and navigates a VMS-style help file.
!
!  Usage:
!
!    help file_name
!
!    where FILE_NAME is a help file, that is, a text file marked up
!    using the VMS help file format.  In this format, a number in column
!    1 is a label, and indicates the beginning of a help topic.  Label
!    1 is reserved for the main help topic; subtopics of the main topic
!    have a label of 2, and so on.  
!
!  Example
!
!    1 Help_File_Format
!
!    This is a brief suggestion of how a help file is laid out.  A line
!    that begins with a number marks the beginning of a topic.  The text
!    on that line is the label for the topic.  
!
!    2 What_the_User_Sees
!
!    On issuing the help command, the user sees the main topic printed out.
!    Following this, a list of the immediate subtopics is presented.
!    The user can proceed to a subtopic by typing its label.
!
!    3 Sub_Sub_Topics
!
!    The arrangement of topics is a little like the grouping of parentheses.
!    
!    2 Suggestions
!
!    You might want to keep each subtopic short, certainly no more than 
!    a page in length.
!
!  Licensing:
!  This code is distributed under the GNU LGPL license. 
!  Modified: 06 March 2001
!  Author:   John Burkardt
!
  implicit none

  character ( len=80 ) help_file
  integer iarg
  !integer ierror
  !integer ilen
  !integer ipxfargc
  integer numarg

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELP'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  A VMS-style help facility.'
!
  numarg = command_argument_count()    !  Count the number of command line arguments.
!
!  Get the input file name.
  if ( numarg >= 1 ) then
    iarg = 1
    call get_command_argument(iarg, help_file)
  else
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'What is the name of the help file to be read?'
    read ( *, '(a)' ) help_file
    if ( help_file == ' ' ) then
      stop
    end if
  end if
!
!  Say hello.
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELP, version 1.06'
  write ( *, '(a)' ) ' '

  call hlpvms ( help_file )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'HELP'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end
function ch_is_digit ( c )

!*****************************************************************************80
!
!! CH_IS_DIGIT returns .TRUE. if a character is a decimal digit.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be analyzed.
!
!    Output, logical CH_IS_DIGIT, .TRUE. if C is a digit, .FALSE. otherwise.
!
  implicit none

  character c
  logical ch_is_digit

  if ( lge ( c, '0' ) .and. lle ( c, '9' ) ) then
    ch_is_digit = .true.
  else
    ch_is_digit = .false.
  end if

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is an integer between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    02 March 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5 and 6).
!
!    Otherwise, IUNIT is an integer between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
  implicit none

  integer i
  integer ios
  integer iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine hlpvms ( help_file )

!*****************************************************************************80
!
!! HLPVMS provides extensive help from a VMS help file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len=* ) HELP_FILE, the name of the help file.
!
  implicit none

  integer, parameter :: maxtop = 100

  logical ch_is_digit
  character ( len=80 ) choice
  character ( len=* ) help_file
  integer i
  integer ierror
  integer iline
  character ( len=80 ) inline
  integer ios
  integer itop
  integer jerror
  character lab
  integer lenc
  integer level
  character ( len=80 ) levelc(maxtop)
  integer levelm(10)
  integer levelo
  integer levelt(maxtop)
  integer lhunit
  integer move
  integer nline
  integer ntop
  integer num
  character ( len=80 ) output
  character ( len=80 ) prompt
  logical s_eqi

  ierror = 0
  nline = 0

  call get_unit ( lhunit )

  if ( lhunit == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HLPVMS - Fatal error!'
    write ( *, '(a)' ) '  Could not get a free FORTRAN unit.'
    ierror = 1
    return
  end if

! call setlin ( 0 )
!
!  Open help file
!
  open ( unit = lhunit, file = help_file, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HLPVMS - Fatal error!'
    write ( *, '(a)' ) '  Could not open the help file.'
    ierror = 2
    return
  end if

  levelo = 0
  level = 1
  iline = 1
!
!  Move to beginning of current topic by reading MOVE lines from
!  the top of the file.  Record this position, corresponding to
!  the current LEVEL, in LEVELM, in case we later want to back up.
!
!  Print out the heading line of this topic.
!
  do

    jerror = 0
    move = iline
    levelm(level) = iline

    do i = 1, move-1
      read ( lhunit, '(1x)', iostat = ios )
      if ( ios /= 0 ) then
        ierror = 4
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'HLPVMS - Fatal error!'
        write ( *, '(a)' ) '  Unexpected end of file, or other I/O error.'
        return
      end if
    end do

    read ( lhunit,'(a1,a75)', iostat = ios ) lab, inline

    if ( ios /= 0 ) then
      ierror = 4
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HLPVMS - Fatal error!'
      write ( *, '(a)' ) '  Unexpected end of file, or other I/O error.'
      return
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( inline )
    write ( *, '(a)' ) ' '

    nline = 3
!
!  If 'going down' or redisplaying, (as opposed to backing up),
!  display information available under the current topic.
!
!  We stop printing when we hit a numeric label.
!
!  If this label is less than or equal to current level, there are
!  no subtopics.
!
!  Otherwise, we now move ahead to print out the list of subtopics
!  available for this topic.
!
    if ( level >= levelo ) then

      ntop = -1

      do

        read ( lhunit, '(a1,a75)', end = 50 ) lab, inline
        move = move + 1

        if ( ch_is_digit ( lab ) ) then
          read ( lab, '(i1)' ) num
          if ( num <= level ) then
            go to 50
          end if
          ntop = 0
          exit
        end if

        if ( nline >= 24 ) then
          read ( *, * )
          nline = 0
        end if

        nline = nline + 1
        write ( *, '(a)' ) trim ( inline )

      end do

    else

      ntop = 0
      inline = ' '
      lab = ' '

    end if
!
!  Locate each subtopic by examining column 1, searching for
!  integer label.
!
!  Assuming we are at level LEVEL, we are searching for labels
!  equal to LEVEL+1.  As we encounter each such label, we want to
!  store the rest of the line as a subtopic.  We ignore labels
!  greater than LEVEL+1 because these are sub-subtopics, and we
!  cease our search when we reach a label less than or equal to
!  LEVEL.
!
    do

      if ( ch_is_digit ( lab ) ) then

        read ( lab, '(i1)' ) num

        if ( num <= level ) then
          exit
        end if

        if ( num == level+1 ) then

          if ( ntop == maxtop+1 ) then

            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) 'HELP - Warning!'
            write ( *, '(a)' ) '  Maximum number of topics reached!'

          else if ( ntop > maxtop ) then

          else

            ntop = ntop + 1

            if ( ntop == 1 ) then
              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'Help is available on:'
              write ( *, '(a)' ) ' '
            end if

            call s_blanks_delete ( inline )

            write ( *, '(a)' ) trim ( inline )

            levelt(ntop) = move
            levelc(ntop) = inline

          end if

        end if
      end if

      read ( lhunit,'(a1,a75)', iostat = ios ) lab, inline

      if ( ios /= 0 ) then
        exit
      end if

      move = move + 1

    end do

50  continue
!
!  Display subtopics.
!
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'RETURN to back up, ? to redisplay.'
!
!  Prompt for user choice of new topic, exit, or back up.
!
60  continue

    nline = 0

    if ( ntop > 0 ) then
      prompt = 'Enter topic you want help on, or RETURN or ?.'
    else
      prompt = 'Enter RETURN or ?.'
    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( prompt )

    nline = 0
    read ( *, '(a)', iostat = ios ) choice

    if ( ios /= 0 ) then
      ierror = 3
      close ( unit = lhunit )
      return
    end if

    call s_blanks_delete ( choice )
    lenc = len_trim ( choice )

    if ( lenc <= 0 ) then
      choice = '!'
    end if
!
!  Consider ending this help session.
!
    if ( choice == '!' .and. level == 1 ) then
      close ( unit = lhunit )
      return
    end if
!
!  Two errors in a row, OK, but three suggests that something is wrong.
!
    if ( ierror /= 0 ) then

      jerror = jerror + 1

      if ( jerror <= 4 ) then
        go to 60
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'HLPVMS - Fatal error!'
      write ( *, '(a)' ) '  Too many input errors in a row!'
      close ( unit = lhunit )
      return

    end if
!
!  User wants to back up to a supertopic.  We must rewind.
!
    rewind lhunit
    levelo = level

    if ( choice == '!' ) then

      level = level - 1
      iline = levelm(level)
!
!  Redisplay current topic.
!
    else if ( choice == '?' ) then
!
!  User wants to go down to a subtopic.
!
    else

      itop = 0

      do i = 1, ntop
        if ( s_eqi ( trim ( choice ), trim ( levelc(i) ) ) ) then
          itop = i
          exit
        end if
      end do

      if ( itop == 0 ) then
        output = 'Sorry, no help available on "'// trim ( choice ) // '".'
        call s_blanks_delete ( output )
        write ( *, '(a)' ) trim ( output )
        jerror = jerror + 1
        go to 60
      else
        level = level + 1
        iline = levelt(itop)
      end if

    end if

  end do

  return
end
subroutine s_blanks_delete ( s )

!*****************************************************************************80
!
!! S_BLANKS_DELETE replaces consecutive blanks by one blank.
!
!  Discussion:
!
!    The remaining characters are left justified and right padded with blanks.
!    TAB characters are converted to spaces.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len=* ) S, the string to be transformed.
!
  implicit none

  integer i
  integer j
  character newchr
  character oldchr
  character ( len=* ) s
  character, parameter :: TAB = char ( 9 )

  j = 0
  newchr = ' '

  do i = 1, len ( s )

    oldchr = newchr
    newchr = s(i:i)

    if ( newchr == TAB ) then
      newchr = ' '
    end if

    s(i:i) = ' '

    if ( oldchr /= ' ' .or. newchr /= ' ' ) then
      j = j + 1
      s(j:j) = newchr
    end if

  end do

  return
end
subroutine s_cap ( s )

!*****************************************************************************80
!
!! S_CAP replaces any lowercase letters by uppercase ones in a string.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len=* ) S, the string to be transformed.
!
  implicit none

  character c
  integer i
  integer nchar
  character ( len=* ) s

  nchar = len_trim ( s )

  do i = 1, nchar

    c = s(i:i)
    call ch_cap ( c )
    s(i:i) = c

  end do

  return
end
subroutine s_chop ( s, ilo, ihi )

!*****************************************************************************80
!
!! S_CHOP "chops out" a portion of a string, and closes up the hole.
!
!  Example:
!
!    S = 'Fred is not a jerk!'
!
!    call s_chop ( S, 9, 12 )
!
!    S = 'Fred is a jerk!    '
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len=* ) S, the string to be transformed.
!
!    Input, integer ILO, IHI, the locations of the first and last
!    characters to be removed.
!
  implicit none

  integer ihi
  integer ihi2
  integer ilo
  integer ilo2
  integer lens
  character ( len=* ) s

  lens = len ( s )

  ilo2 = max ( ilo, 1 )
  ihi2 = min ( ihi, lens )

  if ( ilo2 > ihi2 ) then
    return
  end if

  s(ilo2:lens+ilo2-ihi2-1) = s(ihi2+1:lens)
  s(lens+ilo2-ihi2:lens) = ' '

  return
end
function s_eqi ( s1, s2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!  Example:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is .TRUE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len=* ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none

  character c1
  character c2
  integer i
  integer len1
  integer len2
  integer lenc
  logical s_eqi
  character ( len=* ) s1
  character ( len=* ) s2

  len1 = len ( s1 )
  len2 = len ( s2 )
  lenc = min ( len1, len2 )

  s_eqi = .false.

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do

  do i = lenc + 1, len1
    if ( s1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc + 1, len2
    if ( s2(i:i) /= ' ' ) then
      return
    end if
  end do

  s_eqi = .true.

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len=8  ) ampm
  integer   ( kind = 4 ) d
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len=9  ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
