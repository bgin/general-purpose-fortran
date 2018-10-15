program test_m_html ! test the M_html procedures
   use M_html
   use M_debug, only: unit_check_start, unit_check, unit_check_bad, unit_check_good

   call unit_check_start('M_html'  )
   call unit_check_start('h_open'  )
   call unit_check_start('h_close' )
   call unit_check_start('h_array' )
!-----------------------------------------------------------------
!  call unit_check('h_open',  m_html(10,30)           .eq. 25 )
!  call unit_check('h_close', m_html()                .eq. 25 )
!  call unit_check('h_array', m_html(start=12,end=30) .eq. 25 )
!  call unit_check_bad('M_html')
!-----------------------------------------------------------------
!  if got here without being stopped assume passed test
   call unit_check_good('M_html')

end program test_m_html
