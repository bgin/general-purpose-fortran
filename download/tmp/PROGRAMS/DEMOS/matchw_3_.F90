          program demo_matchw
          implicit none
          ! This main() routine passes a bunch of test strings into the above code.
          ! In performance comparison mode, it does that over and over.  Otherwise,
          ! it does it just once.  Either way, it outputs a passed/failed result.
          !
             integer :: nReps
             logical :: allpassed
             integer :: i
             allpassed = .true.

             nReps = 10000
             nReps = 1     ! Can choose as many repetitions as you're expecting in the real world.

             do i=1,nReps
                ! Cases with repeating character sequences.
                allpassed=and(allpassed, test("a*abab", "a*b", .true.))
                !!cycle
                allpassed=and(allpassed, test("ab", "*?", .true.))
                allpassed=and(allpassed, test("abc", "*?", .true.))
                allpassed=and(allpassed, test("abcccd", "*ccd", .true.))
                allpassed=and(allpassed, test("bLah", "bLaH", .false.))
                allpassed=and(allpassed, test("mississippi", "*sip*", .true.))
                allpassed=and(allpassed, test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.))
                allpassed=and(allpassed, test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.))
                allpassed=and(allpassed, test("mississipissippi", "*issip*ss*", .true.))
                allpassed=and(allpassed, test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.))
                allpassed=and(allpassed, test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.))
                allpassed=and(allpassed, test("xyxyxyzyxyz", "xy*z*xyz", .true.))
                allpassed=and(allpassed, test("xyxyxyxyz", "xy*xyz", .true.))
                allpassed=and(allpassed, test("mississippi", "mi*sip*", .true.))
                allpassed=and(allpassed, test("ababac", "*abac*", .true.))
                allpassed=and(allpassed, test("ababac", "*abac*", .true.))
                allpassed=and(allpassed, test("aaazz", "a*zz*", .true.))
                allpassed=and(allpassed, test("a12b12", "*12*23", .false.))
                allpassed=and(allpassed, test("a12b12", "a12b", .false.))
                allpassed=and(allpassed, test("a12b12", "*12*12*", .true.))

                ! Additional cases where the '*' char appears in the tame string.
                allpassed=and(allpassed, test("*", "*", .true.))
                allpassed=and(allpassed, test("a*r", "a*", .true.))
                allpassed=and(allpassed, test("a*ar", "a*aar", .false.))

                ! More double wildcard scenarios.
                allpassed=and(allpassed, test("XYXYXYZYXYz", "XY*Z*XYz", .true.))
                allpassed=and(allpassed, test("missisSIPpi", "*SIP*", .true.))
                allpassed=and(allpassed, test("mississipPI", "*issip*PI", .true.))
                allpassed=and(allpassed, test("xyxyxyxyz", "xy*xyz", .true.))
                allpassed=and(allpassed, test("miSsissippi", "mi*sip*", .true.))
                allpassed=and(allpassed, test("miSsissippi", "mi*Sip*", .false.))
                allpassed=and(allpassed, test("abAbac", "*Abac*", .true.))
                allpassed=and(allpassed, test("abAbac", "*Abac*", .true.))
                allpassed=and(allpassed, test("aAazz", "a*zz*", .true.))
                allpassed=and(allpassed, test("A12b12", "*12*23", .false.))
                allpassed=and(allpassed, test("a12B12", "*12*12*", .true.))
                allpassed=and(allpassed, test("oWn", "*oWn*", .true.))

                ! Completely tame (no wildcards) cases.
                allpassed=and(allpassed, test("bLah", "bLah", .true.))

                ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
                allpassed=and(allpassed, test("a", "*?", .true.))

                ! More mixed wildcard tests including coverage for false positives.
                allpassed=and(allpassed, test("a", "??", .false.))
                allpassed=and(allpassed, test("ab", "?*?", .true.))
                allpassed=and(allpassed, test("ab", "*?*?*", .true.))
                allpassed=and(allpassed, test("abc", "?**?*?", .true.))
                allpassed=and(allpassed, test("abc", "?**?*&?", .false.))
                allpassed=and(allpassed, test("abcd", "?b*??", .true.))
                allpassed=and(allpassed, test("abcd", "?a*??", .false.))
                allpassed=and(allpassed, test("abcd", "?**?c?", .true.))
                allpassed=and(allpassed, test("abcd", "?**?d?", .false.))
                allpassed=and(allpassed, test("abcde", "?*b*?*d*?", .true.))

                ! Single-character-match cases.
                allpassed=and(allpassed, test("bLah", "bL?h", .true.))
                allpassed=and(allpassed, test("bLaaa", "bLa?", .false.))
                allpassed=and(allpassed, test("bLah", "bLa?", .true.))
                allpassed=and(allpassed, test("bLaH", "?Lah", .false.))
                allpassed=and(allpassed, test("bLaH", "?LaH", .true.))

                ! Many-wildcard scenarios.
                allpassed=and(allpassed, test(&
                &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
                &"a*a*a*a*a*a*aa*aaa*a*a*b",&
                &.true.))
                allpassed=and(allpassed, test(&
                &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
                &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
                &.true.))
                allpassed=and(allpassed, test(&
                &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
                &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
                &.false.))
                allpassed=and(allpassed, test(&
                &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
                &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
                &.false.))
                allpassed=and(allpassed, test(&
                &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
                &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
                &.true.))
                allpassed=and(allpassed, test("aaabbaabbaab", "*aabbaa*a*", .true.))
                allpassed=and(allpassed, test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", "a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.))
                allpassed=and(allpassed, test("aaaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.))
                allpassed=and(allpassed, test("aaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.))
                allpassed=and(allpassed, test(&
                &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
                & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
                &.false.))
                allpassed=and(allpassed, test(&
                &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
                &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
                &.true.))
                allpassed=and(allpassed, test("abc*abcd*abcd*abc*abcd", "abc*abc*abc*abc*abc", .false.))
                allpassed=and(allpassed, test( "abc*abcd*abcd*abc*abcd*abcd*abc*abcd*abc*abc*abcd", &
                &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
                &.true.))
                allpassed=and(allpassed, test("abc", "********a********b********c********", .true.))
                allpassed=and(allpassed, test("********a********b********c********", "abc", .false.))
                allpassed=and(allpassed, test("abc", "********a********b********b********", .false.))
                allpassed=and(allpassed, test("*abc*", "***a*b*c***", .true.))

                ! A case-insensitive algorithm test.
                ! allpassed=and(allpassed, test("mississippi", "*issip*PI", .true.))
             enddo

             if (allpassed)then
                write(*,'(a)')"Passed",nReps
             else
                write(*,'(a)')"Failed"
             endif
       !===================================================================================================================================
       contains
       !===================================================================================================================================

              ! This is a test program for wildcard matching routines.
                     It can be used ! either to test a single routine for correctness, or to compare the timings ! of two (or
                     more) different wildcard matching routines.  !  function test(tame, wild, bExpectedResult) result(bpassed)
                     use M_strings, only : matchw character(len=*) :: tame character(len=*) :: wild

                     logical
                            :: bExpectedResult

                     logical
                            :: bResult

                     logical
                            :: bPassed

                     bResult = .true.
                            ! We'll do "&=" cumulative checking.

                     bPassed = .false.
                            ! Assume the worst.  write(*,*)repeat('=',79) bResult = matchw(tame, wild) ! Call a wildcard
                            matching routine.

                     ! To assist correctness checking, output the two strings in any failing scenarios.  if (bExpectedResult
                     .eqv. bResult) then bPassed = .true.  if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild else
                     if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild endif

              end function test end program demo_matchw

       Expected output

AUTHOR
       John S. Urban

       Based on the article "Matching Wildcards: An Empirical Way to Tame an Algorithm" in Dr Dobb's Journal, By Kirk J. Krauss,
       October 07, 2014

LICENSE
       Public Domain

                                                        November 13, 2019                                              matchw(3)
