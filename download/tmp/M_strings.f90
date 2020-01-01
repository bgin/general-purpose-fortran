!>
!!##NAME
!!    M_strings(3f) - [M_strings:INTRO] Fortran string module
!!##DESCRIPTION
!!
!!    The M_strings(3fm) module is a collection of Fortran procedures
!!    that supplement the built-in intrinsic string routines. Routines
!!    for parsing, tokenizing, changing case, substituting new strings for
!!    substrings, locating strings with simple wildcard expressions, removing
!!    tabs and line terminators and other string manipulations are included.
!!
!!    M_strings_oop(3fm) is a companion module that provides an OOP interface
!!    to the M_strings module.
!!
!!    As newer Fortran features become more widely available a significant
!!    amount of the code (much of which originated as pre-Fortran90 routines)
!!    is subject to updating so new versions of this module are not expected
!!    to be compatible with older versions.
!!##SYNOPSIS
!!
!!  public entities:
!!
!!    use M_strings, only : split,delim,chomp
!!    use M_strings, only : substitute,change,modif,transliterate,reverse,replace,join
!!    use M_strings, only : upper,lower,upper_quoted
!!    use M_strings, only : rotate13
!!    use M_strings, only : adjustc,compact,nospace,indent,crop,unquote,quote
!!    use M_strings, only : len_white,atleast,stretch,lenset,merge_str
!!    use M_strings, only : switch,s2c,c2s
!!    use M_strings, only : noesc,notabs,expand,visible
!!    use M_strings, only : string_to_value,string_to_values,s2v,s2vs,value_to_string,v2s,msg
!!    use M_strings, only : listout,getvals
!!    use M_strings, only : matchw
!!    use M_strings, only : fmt
!!    use M_strings, only : base, decodebase, codebase
!!    use M_strings, only : isalnum, isalpha, iscntrl, isdigit, isgraph, islower,
!!                          isprint, ispunct, isspace, isupper, isascii, isblank, isxdigit
!!
!!    TOKENS
!!
!!    split  subroutine parses string using specified delimiter characters and stores tokens into an array
!!    delim  subroutine parses string using specified delimiter characters and store tokens into an array
!!    chomp  function consumes input line as it returns next token in a string using specified delimiters
!!    fmt    convert a string into a paragraph
!!
!!    EDITING
!!
!!    substitute     subroutine non-recursively globally replaces old substring
!!                   with new substring
!!    replace        function non-recursively globally replaces old substring
!!                   with new substring using allocatable string (version of
!!                   substitute(3f) without limitation on length of output string)
!!    change         subroutine non-recursively globally replaces old substring
!!                   with new substring with a directive like line editor
!!    modif          subroutine modifies a string with a directive like the XEDIT
!!                   line editor MODIFY command
!!    transliterate  replace characters found in set one with characters from set two
!!    reverse        reverse character order in a string
!!    join           join an array of CHARACTER variables with specified separator
!!    rotate13       apply trivial encryption algorithm ROT13 to a string
!!
!!    CASE
!!
!!    upper  function converts string to uppercase
!!    lower  function converts string to miniscule
!!    upper  function converts string to uppercase skipping strings quoted per Fortran rules
!!
!!    WHITE SPACE
!!
!!    adjustc  elemental function centers text within the length of the input string
!!    compact  left justify string and replace duplicate whitespace with single characters or nothing
!!    nospace  function replaces whitespace with nothing
!!    indent   find number of leading spaces
!!    crop     function trims leading and trailing spaces
!!
!!    QUOTES
!!
!!    unquote  remove quotes from string as if read with list-directed input
!!    quote    add quotes to string as if written with list-directed input
!!
!!    STRING LENGTH
!!
!!    len_white  find location of last non-whitespace character
!!    lenset     return a string of specified length
!!    atleast    return a string of at least specified length
!!    stretch    return a string of at least specified length with suffix
!!    merge_str  make strings of equal length and then call MERGE(3f) intrinsic
!!
!!    CHARACTER ARRAY VERSUS STRING
!!
!!    switch  switch between a string and an array of single characters
!!    s2c     convert string to array of single characters and add null terminator for passing to C
!!    c2s     convert null-terminated array of single characters to string for converting strings returned from C
!!
!!    NONALPHA
!!
!!    noesc    convert non-printable ASCII8 characters to a space
!!    notabs   convert tabs to spaces while maintaining columns, assuming tabs are set every 8 characters
!!    expand   expand escape sequences in a string
!!    visible  expand escape sequences in a string to control and meta-control representations
!!
!!    NUMERIC STRINGS
!!
!!    string_to_value   generic subroutine returns numeric value (REAL, DOUBLEPRECISION, INTEGER)  from string
!!    string_to_values  subroutine reads an array of numbers from a string
!!    getvals           subroutine reads a relatively arbitrary number of values from a string using list-directed read
!!    s2v               function returns DOUBLEPRECISION numeric value from string
!!    s2vs              function returns a DOUBLEPRECISION array of numbers from a string
!!    msg               append the values of up to nine values into a string
!!
!!    value_to_string   generic subroutine returns string given numeric value (REAL, DOUBLEPRECISION, INTEGER, LOGICAL )
!!    v2s               generic function returns string from numeric value (REAL, DOUBLEPRECISION, INTEGER )
!!    trimzeros         delete trailing zeros from numeric decimal string
!!    listout           expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!!    isnumber          determine if string represents a number
!!
!!    CHARACTER TESTS
!!
!!    matchw  compares given string for match to pattern which may contain wildcard characters
!!
!!    o isalnum   returns .true. if character is a letter or digit
!!    o isalpha   returns .true. if character is a letter and .false. otherwise
!!    o iscntrl   returns .true. if character is a delete character or ordinary control character
!!    o isdigit   returns .true. if character is a digit (0,1,...,9) and .false. otherwise
!!    o isgraph   returns .true. if character is a printable character except a space is considered non-printable
!!    o islower   returns .true. if character is a miniscule letter (a-z)
!!    o isprint   returns .true. if character is an ASCII printable character
!!    o ispunct   returns .true. if character is a printable punctuation character
!!    o isspace   returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed
!!    o isupper   returns .true. if character is an uppercase letter (A-Z)
!!    o isascii   returns .true. if the character is in the range char(0) to char(127)
!!    o isblank   returns .true. if character is a blank character (space or horizontal tab.
!!    o isxdigit  returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).
!!
!!    BASE CONVERSION
!!
!!    base       convert whole number string in base [2-36] to string in alternate base [2-36]
!!    codebase   convert whole number string in base [2-36] to base 10 number
!!    decodebase convert whole number in base 10 to string in base [2-36]
!!
!!    MISCELLANEOUS
!!
!!    describe   returns a string describing the name of a single character
!!
!!
!!    INTRINSICS
!!
!!    The M_strings(3fm) module supplements and works in combination with
!!    the Fortran built-in intrinsics. Stand-alone
!!    Fortran lets you access the characters in a string using ranges
!!    much like they are character arrays, assignment, comparisons with
!!    standard operators, supports dynamically allocatable strings and
!!    supports concatenation using the // operator, as well as a number
!!    of intrinsic string routines:
!!
!!        adjustl   Left adjust a string
!!        adjustr   Right adjust a string
!!        index     Position of a substring within a string
!!        repeat    Repeated string concatenation
!!        scan      Scan a string for the presence of a set of characters
!!        trim      Remove trailing blank characters of a string
!!        verify    Scan a string for the absence of a set of characters
!!        len       It returns the length of a character string
!!        achar     converts an integer into a character
!!        iachar    converts a character into an integer
!!        len_trim  finds length of string with trailing spaces ignored
!!        new_line  Newline character
!!        selected_char_kind  Choose character kind
!!        lge       Lexical greater than or equal
!!        lgt       Lexical greater than
!!        lle       Lexical less than or equal
!!        llt       Lexical less than
!!
!!
!!
!!    OOPS INTERFACE
!!
!!    The M_strings_oop(3fm) module (included with the M_strings(3fm)
!!    module) provides an OOP (Object-Oriented Programming) interface
!!    to the M_strings(3fm) module; as described in the example program
!!    OBJECT_ORIENTED shown below...
!!
!!##SEE ALSO
!!
!!    There are additional routines in other GPF modules for working with
!!    expressions (M_calculator), time strings (M_time), random strings
!!    (M_random, M_uuid), lists (M_list), and interfacing with the C regular
!!    expression library (M_regex).
!!
!!##EXAMPLES
!!
!!
!! Each of the procedural functions includes an example program in the corresponding man(1) page for the function.
!! The object-oriented interface does not have individual man(1) pages, but is instead demonstrated using the following
!! example program:
!!
!!  program demo_M_strings
!!  !
!!  ! This is an example using the object-oriented class/type model
!!  ! defined in M_strings_oop
!!  ! This is essentially the same functionality as the procedures
!!  ! combined with several Fortran intrinsics and overloaded operators
!!  !
!!  use M_strings_oop,only : string, p
!!  implicit none
!!  TYPE(string) :: str1
!!  TYPE(string) :: str2
!!  TYPE(string) :: str3
!!  TYPE(string) :: str4
!!  !==============================================================================
!!    write(*,*)'exercise the M_STRING_OOP module interface'
!!    ! draw a break line in the output
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Call methods of type(STRING)'
!!    ! define TYPE(STRING) with constructor
!!    str2=string('   This  is  a  String!       ')
!!    str4=string(' a  String ')
!!    write(*,*)repeat('=',78)
!!    ! print members of type
!!    write(*,101)'str2%str is ................ ',str2%str
!!    ! same as intrinsic LEN()
!!    write(*,202)'len ........................ ',str2%len()
!!    ! same as intrinsic INDEX()
!!    write(*,202)'len_trim ................... ',str2%len_trim()
!!    ! same as intrinsic INDEX()
!!    write(*,202)'index("is")................. ',str2%index("is")
!!    ! same as intrinsic INDEX()
!!    write(*,202)'index("is",back=.T.) ....... ',str2%index("is",back=.TRUE.)
!!    ! output TYPE(STRING) with %str all uppercase
!!    write(*,101)'upper ...................... ',p(str2%upper())
!!    ! output TYPE(STRING) with %str all miniscule
!!    write(*,101)'lower ...................... ',p(str2%lower())
!!    ! output TYPE(STRING) with %str reversed
!!    write(*,101)'reverse .................... ',p(str2%reverse())
!!    ! same as intrinsic ADJUSTL()
!!    write(*,101)'adjustl .................... ',p(str2%adjustl())
!!    ! same as intrinsic ADJUSTR()
!!    write(*,101)'adjustr .................... ',p(str2%adjustr())
!!    ! center string in current string length
!!    write(*,101)'adjustc .................... ',p(str2%adjustc())
!!    ! center string in string length of NN
!!    write(*,101)'adjustc(49) ................ ',p(str2%adjustc(49))
!!    ! force %str to be NN characters long
!!    write(*,101)'lenset(49) ................. ',p(str2%lenset(49))
!!    ! same as intrinsic TRIM()
!!    write(*,101)'trim ....................... ',p(str2%trim())
!!    ! trim leading and trailing spaces
!!    write(*,101)'crop ....................... ',p(str2%crop())
!!    ! calls M_strings procedure SUBSTITUTE()
!!    write(*,101)'substitute("This","Here") .. ',p(str2%substitute("This","Here"))
!!    ! calls M_strings procedure COMPACT()
!!    write(*,101)'compact .................... ',p(str2%compact())
!!    write(*,101)'compact("") ................ ',p(str2%compact(""))
!!    write(*,101)'compact(":") ............... ',p(str2%compact(":"))
!!    ! calls M_strings procedure TRANSLITERATE()
!!    write(*,101)'transliterate("aei","VWX") . ',p(str2%transliterate("aei","VWX"))
!!    write(*,101)'transliterate("aeiou"," ") . ',p(str2%transliterate("aeiou"," "))
!!    write(*,101)'transliterate("aeiou","") .. ',p(str2%transliterate("aeiou",""))
!!    write(*,101)'transliterate(" aeiou","") . ',p(str2%transliterate(" aeiou",""))
!!    ! calls M_strings procedure SWITCH()
!!    write(*,404)'chars .................... . ',str4%chars()
!!
!!    write(*,*)repeat('=',78)
!!    str2%str='\t\tSome tabs\t   x\bX '
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,101)'expand ..................... ',p(str2%expand())
!!    str2=str2%expand()
!!    ! calls M_strings procedure NOTABS()
!!    write(*,101)'notabs ..................... ',p(str2%notabs())
!!    ! calls M_strings procedure NOESC()
!!    write(*,101)'noesc ...................... ',p(str2%noesc())
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Casting to numeric variables'
!!    str3=string('   12.345678901234567e1        ')
!!    write(*,101)'str3%str ................... ',str3%str
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'int  ....................... ', str3%int()
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'real ....................... ', str3%real()
!!    ! calls M_strings procedure STRING_TO_VALUE()
!!    write(*,*)'dble ....................... ', str3%dble()
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'Matching simple globbing patterns'
!!    str3=string('   12.345678901234567e1        ')
!!    str3=string('Four score and seven years ago')
!!    write(*,101)'str3%str ................... ',str3%str
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("Fo*") ............... ', str3%match("Fo*")
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("and") ............... ', str3%match("and")
!!    ! calls M_strings procedure MATCHW
!!    write(*,*)'match("*and*") ............. ', str3%match("*and*")
!!
!!    101 format(1x,a,"[",a,"]")
!!    202 format(1x,a,i0)
!!    303 format(1x,*(l3))
!!    404 format(1x,a,*("[",a1,"]":))
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (add and subtract,return TYPE(STRING))'
!!    str1%str='123.456'
!!    str2%str='AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)'str1 + str2 ................ ',p(str1 + str2)
!!    ! a string that looks like a numeric value can have a value added
!!    write(*,*)'str1 + 20000 ............... ',p(str1 +20000)
!!    write(*,*)'str1 - 20.0 ................ ',p(str1 -20.0)
!!    write(*,*)'str2 - "Aa" (removes ALL) .. ',p(str2 - 'Aa')
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (multiply,return TYPE(STRING))'
!!    str1%str='AaBbCcDdEeFfGgHhIiJj'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,*)'str1 * 3 ................... ',p(str1 * 3)
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (//,return TYPE(STRING))'
!!    str1%str='String one:'
!!    str2%str='String two:'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)'str1 // str2 ................ ',p(str1 // str2)
!!    ! numeric values are converted to strings
!!    write(*,*)'str1 // 20000 ............... ',p(str1 // 20000)
!!    write(*,*)'str1 // 20.0 ................ ',p(str1 // 20.0)
!!
!!    write(*,*)repeat('=',78)
!!    write(*,*)'OVERLOADED OPERATORS (logical comparisons,return logical)'
!!    ! NOTE: comparisons are performed on the character variable members
!!    !       of the type(string)
!!    str1%str='abcdefghij'
!!    str2%str='klmnopqrst'
!!    write(*,101)'str1%str ................... ',str1%str
!!    write(*,101)'str2%str ................... ',str2%str
!!    write(*,*)': EQ LT GT LE GE NE'
!!    write(*,*)'compare str1 to str1'
!!    write(*,303)str1.eq.str1  ,str1.lt.str1  ,str1.gt.str1  ,str1.le.str1 &
!!               & ,str1.ge.str1  ,str1.ne.str1
!!    write(*,*)'compare str1 to str2'
!!    write(*,303)str1.eq.str2  ,str1.lt.str2  ,str1.gt.str2  ,str1.le.str2 &
!!               & ,str1.ge.str2  ,str1.ne.str2
!!    write(*,*)'compare str2 to str1'
!!    write(*,303)str2.eq.str1  ,str2.lt.str1  ,str2.gt.str1  ,str2.le.str1 &
!!               & ,str2.ge.str1  ,str2.ne.str1
!!
!!    write(*,*)repeat('=',78)
!!
!!  end program demo_M_strings
!!
!!  Expected output
!!
!!   exercise the M_STRING_OOP module interface
!!   =============================================================================
!!   Call methods of type(STRING)
!!   =============================================================================
!!   str2%str is ................ [   This  is  a  String!             ]
!!   len ........................ 36
!!   len_trim ................... 23
!!   index("is")................. 6
!!   index("is",back=.T.) ....... 10
!!   upper ...................... [   THIS  IS  A  STRING!             ]
!!   lower ...................... [   this  is  a  string!             ]
!!   reverse .................... [             !gnirtS  a  si  sihT   ]
!!   adjustl .................... [This  is  a  String!                ]
!!   adjustr .................... [                This  is  a  String!]
!!   adjustc .................... [        This  is  a  String!        ]
!!   adjustc(49) ................ [              This  is  a  String!               ]
!!   lenset(49) ................. [   This  is  a  String!                          ]
!!   trim ....................... [   This  is  a  String!]
!!   crop ....................... [This  is  a  String!]
!!   substitute("This","Here") .. [   Here  is  a  String!             ]
!!   compact .................... [This is a String!]
!!   compact("") ................ [ThisisaString!]
!!   compact(":") ............... [This:is:a:String!]
!!   transliterate("aei","VWX") . [   ThXs  Xs  V  StrXng!             ]
!!   transliterate("aeiou"," ") . [   Th s   s     Str ng!             ]
!!   transliterate("aeiou","") .. [   Ths  s    Strng!                 ]
!!   transliterate(" aeiou","") . [ThssStrng!                          ]
!!   chars .................... . [ ][a][ ][s][t][r][i][n][g][ ]
!!   =============================================================================
!!   str2%str ................... [\t\tSome tabs\t   x\bX ]
!!   expand ..................... [         Some tabs          x   X]
!!   notabs ..................... [                Some tabs          x    X]
!!   noesc ...................... [  Some tabs    x X]
!!   =============================================================================
!!   Casting to numeric variables
!!   str3%str ................... [   12.345678901234567e1        ]
!!   int  .......................          123
!!   real .......................    123.456787
!!   dble .......................    123.45678901234567
!!   =============================================================================
!!   Matching simple globbing patterns
!!   str3%str ................... [Four score and seven years ago]
!!   match("Fo*") ...............  T
!!   match("and") ...............  F
!!   match("*and*") .............  T
!!   ==============================================================================
!!   OVERLOADED OPERATORS (add and subtract, return TYPE(STRING))
!!   str1%str ................... [123.456]
!!   str2%str ................... [AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj]
!!   str1 + str2 ................ 123.456 AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj
!!   str1 + 20000 ............... 20123.455999999998
!!   str1 - 20.0 ................ -103.456
!!   str2 - "Aa" (removes ALL) .. BbCcDdEeFfGgHhIiJj BbCcDdEeFfGgHhIiJj
!!   =============================================================================
!!   OVERLOADED OPERATORS (multiply, return TYPE(STRING))
!!   str1%str ................... [AaBbCcDdEeFfGgHhIiJj]
!!   str1 * 3 ................... AaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJjAaBbCcDdEeFfGgHhIiJj
!!   =============================================================================
!!   OVERLOADED OPERATORS (//, return TYPE(STRING))
!!   str1%str ................... [String one:]
!!   str2%str ................... [String two:]
!!   str1 // str2 ................ String one:String two:
!!   str1 // 20000 ............... String one:20000
!!   str1 // 20.0 ................ String one:20.0
!!   =============================================================================
!!   OVERLOADED OPERATORS (logical comparisons, return logical)
!!   str1%str ................... [abcdefghij]
!!   str2%str ................... [klmnopqrst]
!!   : EQ LT GT LE GE NE
!!   compare str1 to str1
!!   :  T  F  F  T  T  F
!!   compare str1 to str2
!!   :  F  T  F  T  F  T
!!   compare str2 to str1
!!   :  F  F  T  F  T  T
!!   =============================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
MODULE M_strings !
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use M_journal,       only : journal
use M_debug, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_debug, only : unit_check_level
implicit none    ! change default for every procedure contained in the module

character(len=*),parameter::ident_1="@(#)M_strings(3f): Fortran module containing routines that deal with character strings"

!-----------------------------------------------------------------------------------------------------------------------------------
PRIVATE

!----------------------# TOKENS
PUBLIC split           !  subroutine parses a string using specified delimiter characters and store tokens into an allocatable array
PUBLIC chomp           !  function consumes input line as it returns next token in a string using specified delimiters
PUBLIC delim           !  subroutine parses a string using specified delimiter characters and store tokens into an array
PUBLIC strtok          !  gets next token. Used by change(3f)
PUBLIC fmt             !  convert a long string into a paragraph
!----------------------# EDITING
PUBLIC substitute      !  subroutine non-recursively globally replaces old substring with new substring in string
PUBLIC replace         !  function non-recursively globally replaces old substring with new substring in string
PUBLIC change          !  replaces old substring with new substring in string with a directive like a line editor
PUBLIC modif           !  change string using a directive using rules similar to XEDIT line editor MODIFY command
PUBLIC transliterate   !  when characters in set one are found replace them with characters from set two
PUBLIC reverse         !  elemental function reverses character order in a string
PUBLIC join            !  append an array of character variables with specified separator into a single CHARACTER variable
PUBLIC rotate13        !  apply trivial encryption algorithm ROT13 to string
!----------------------# CHARACTER ARRAY VERSUS STRING
PUBLIC switch          !  generic switch between a string and an array of single characters (a2s,s2a)
PRIVATE a2s            !  function to copy char array to string
PRIVATE s2a            !  function to copy string(1:Clen(string)) to char array
PUBLIC s2c             !  convert character variable to array of character(len=1) with null terminator for C compatibility
PUBLIC c2s             !  convert null-terminated array of character(len=1) to string for strings returned by C
!----------------------# CASE
PUBLIC upper           !  elemental function converts string to uppercase
PUBLIC lower           !  elemental function converts string to miniscule
PUBLIC upper_quoted          !  elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!----------------------# WHITE SPACE
PUBLIC adjustc         !  elemental function centers string within the length of the input string
PUBLIC compact         !  left justify string and replace duplicate whitespace with single characters or nothing
PUBLIC nospace         !  function replaces whitespace with nothing
PUBLIC indent          !  count number of leading spaces
PUBLIC crop            !  function trims leading and trailing spaces
!----------------------# QUOTES
PUBLIC unquote         !  remove quotes from string as if read with list-directed input
PUBLIC quote           !  add quotes to string as if written with list-directed input
!----------------------# STRING LENGTH
PUBLIC lenset          !  return a string as specified length
PUBLIC atleast         !  return a string of at least specified length
PUBLIC stretch         !  return a string of at least specified length with suffix
PUBLIC merge_str       !  make strings of equal length and then call MERGE(3f) intrinsic
PUBLIC len_white       !  find location of last non-whitespace character
!----------------------# NONALPHA
PUBLIC noesc           !  elemental function converts non-printable ASCII8 characters to a space
PUBLIC notabs          !  convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters
PUBLIC expand          !  expand escape sequences in a string
PUBLIC visible         !  expand escape sequences in a string to control and meta-control representations
!----------------------# NUMERIC STRINGS
PUBLIC string_to_value !  generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)
 PRIVATE a2d           !  subroutine returns double value from string
 PRIVATE a2r           !  subroutine returns real value from string
 PRIVATE a2i           !  subroutine returns integer value from string
PUBLIC string_to_values!  subroutine returns values from a string
PUBLIC getvals         !  subroutine returns values from a string
PUBLIC s2v             !  function returns doubleprecision value from string
PUBLIC s2vs            !  function returns a doubleprecision array of numbers from a string
PUBLIC msg             !  function returns a string representing up to nine scalar intrinsic values
                       !------------------------------------------------------------------------------------------------------------
PUBLIC value_to_string !  generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
PUBLIC v2s             !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER|LOGICAL value
 PRIVATE d2s           !  function returns string from doubleprecision value
 PRIVATE r2s           !  function returns string from real value
 PRIVATE i2s           !  function returns string from integer value
 PRIVATE l2s           !  function returns string from logical value
PUBLIC v2s_bug         !  generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value
PUBLIC isnumber        !  determine if string represents a number
 PRIVATE trimzeros     !  Delete trailing zeros from numeric decimal string
PUBLIC listout         !  expand a list of numbers where  negative numbers denote range ends (1 -10 means 1 thru 10)
!-----------------------------------------------------------------------------------------------------------------------------------
!
! extend intrinsics to accept CHARACTER values
!
PUBLIC int, real, dble

interface int;     module procedure int_s2v;           end interface
interface real;    module procedure real_s2v;          end interface
interface dble;    module procedure dble_s2v;          end interface

interface int;     module procedure ints_s2v;          end interface
interface real;    module procedure reals_s2v;         end interface
interface dble;    module procedure dbles_s2v;         end interface

!-----------------------------------------------------------------------------------------------------------------------------------
!----------------------# BASE CONVERSION
PUBLIC base            !  convert whole number string in base [2-36] to string in alternate base [2-36]
PUBLIC codebase        !  convert whole number string in base [2-36] to base 10 number
PUBLIC decodebase      !  convert whole number in base 10 to string in base [2-36]
!----------------------# LOGICAL TESTS
PUBLIC matchw          !  compares given string for match to pattern which may contain wildcard characters
PUBLIC isalnum         !  elemental function returns .true. if CHR is a letter or digit
PUBLIC isalpha         !  elemental function returns .true. if CHR is a letter and .false. otherwise
PUBLIC isascii         !  elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)
PUBLIC isblank         !  elemental function returns .true. if CHR is a blank character (space or horizontal tab.
PUBLIC iscntrl         !  elemental function returns .true. if CHR is a delete character or ordinary control character
PUBLIC isdigit         !  elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise
PUBLIC isgraph         !  elemental function true if CHR is an ASCII printable character except considers a space non-printable
PUBLIC islower         !  elemental function returns .true. if CHR is a miniscule letter (a-z)
PUBLIC isprint         !  elemental function determines if CHR is an ASCII printable character
PUBLIC ispunct         !  elemental function returns .true. if CHR is a printable punctuation character
PUBLIC isspace         !  elemental function true if CHR is a null, space, tab, carriage return, new line, vertical tab, or formfeed
PUBLIC isupper         !  elemental function returns .true. if CHR is an uppercase letter (A-Z)
PUBLIC isxdigit        !  elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).
!----------------------#
PUBLIC describe        !  returns a string describing character
!----------------------#

public test_suite_m_strings

!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_2="@(#)M_strings::switch(3f): toggle between string and array of characters"

interface switch
   module procedure a2s, s2a
end interface switch
! note how returned result is "created" by the function
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_3="@(#)M_strings::msg(3f): {msg_scalar,msg_one}"

interface msg
   module procedure msg_scalar, msg_one
end interface msg
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_4="&
&@(#)M_strings::string_to_value(3f): Generic subroutine converts numeric string to a number (a2d,a2r,a2i)"

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_5="&
&@(#)M_strings::v2s(3f): Generic function returns string given REAL|INTEGER|DOUBLEPRECISION value(d2s,r2s,i2s)"

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
! ASCII character constants
character, public, parameter :: ascii_nul = char(0)   ! null
character, public, parameter :: ascii_bel = char(7)   ! bell
character, public, parameter :: ascii_bs  = char(8)   ! backspace
character, public, parameter :: ascii_ht  = char(9)   ! horizontal tab
character, public, parameter :: ascii_lf  = char(10)  ! line feed or newline
character, public, parameter :: ascii_ff  = char(12)  ! form feed or newpage
character, public, parameter :: ascii_cr  = char(13)  ! carriage return
character, public, parameter :: ascii_esc = char(27)  ! escape
!-----------------------------------------------------------------------------------------------------------------------------------
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    matchw(3f) - [M_strings:COMPARE] compare given string for match to pattern which may contain wildcard characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    logical function matchw(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!##DESCRIPTION
!!
!!    matchw(3f) compares given string for match to pattern which may
!!    contain wildcard characters.
!!
!!    In this version to get a match entire string must be described by pattern.
!!    Trailing whitespace is significant, so trim the input string to have
!!    trailing whitespace ignored.
!!
!!##OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!             o  "?" matching any one character
!!             o  "*" matching zero or more characters. Do NOT use adjacent asterisks.
!!             o  Both strings may have trailing spaces which are ignored.
!!             o  There is no escape character, so matching strings with literal
!!                question mark and asterisk is problematic.
!!
!!##EXAMPLES
!!
!!   Example program
!!
!!    program demo_matchw
!!    implicit none
!!    ! This main() routine passes a bunch of test strings into the above code.
!!    ! In performance comparison mode, it does that over and over.  Otherwise,
!!    ! it does it just once.  Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     nReps = 1     ! Can choose as many repetitions as you're expecting in the real world.
!!
!!     do i=1,nReps
!!      ! Cases with repeating character sequences.
!!      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
!!      !!cycle
!!      allpassed=allpassed .and. test("ab", "*?", .true.)
!!      allpassed=allpassed .and. test("abc", "*?", .true.)
!!      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
!!      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
!!      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
!!      allpassed=allpassed .and. test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. test("mississipissippi", "*issip*ss*", .true.)
!!      allpassed=allpassed .and. test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
!!      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
!!      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)
!!
!!      ! Additional cases where the '*' char appears in the tame string.
!!      allpassed=allpassed .and. test("*", "*", .true.)
!!      allpassed=allpassed .and. test("a*r", "a*", .true.)
!!      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)
!!
!!      ! More double wildcard scenarios.
!!      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
!!      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
!!      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
!!      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
!!      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
!!      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)
!!
!!      ! Completely tame (no wildcards) cases.
!!      allpassed=allpassed .and. test("bLah", "bLah", .true.)
!!
!!      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!      allpassed=allpassed .and. test("a", "*?", .true.)
!!
!!      ! More mixed wildcard tests including coverage for false positives.
!!      allpassed=allpassed .and. test("a", "??", .false.)
!!      allpassed=allpassed .and. test("ab", "?*?", .true.)
!!      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
!!      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
!!      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
!!      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
!!      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
!!      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)
!!
!!      ! Single-character-match cases.
!!      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
!!      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
!!      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
!!      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
!!      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)
!!
!!      ! Many-wildcard scenarios.
!!      allpassed=allpassed .and. test(&
!!      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
!!      allpassed=allpassed .and. test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", "a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd", "abc*abc*abc*abc*abc", .false.)
!!      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd*abc*abcd*abc*abc*abcd", &
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc", "********a********b********c********", .true.)
!!      allpassed=allpassed .and. test("********a********b********c********", "abc", .false.)
!!      allpassed=allpassed .and. test("abc", "********a********b********b********", .false.)
!!      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)
!!
!!      ! A case-insensitive algorithm test.
!!      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(a)')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.  It can be used
!!    ! either to test a single routine for correctness, or to compare the timings
!!    ! of two (or more) different wildcard matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bpassed)
!!    use M_strings, only : matchw
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = matchw(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_matchw
!!
!!   Expected output
!!
!!##AUTHOR
!!   John S. Urban
!!
!!##REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
!!##LICENSE
!!   Public Domain
function matchw(tame,wild)

character(len=*),parameter::ident_6="&
&@(#)M_strings::matchw(3f): function compares text strings, one of which can have wildcards ('*' or '?')."

logical                    :: matchw
character(len=*)           :: tame       ! A string without wildcards
character(len=*)           :: wild       ! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character.  They
! represent the locations, in the two strings, from which we start once we've observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi).eq.'*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi).eq.NULL) then        ! "x" matches "*"
            matchw=.true.
            return
         endif
         if(wildtext(wi:wi) .ne. '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti) .ne. wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti).eq.NULL)then
                  matchw=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti) .ne. wildtext(wi:wi) .and. wildtext(wi:wi) .ne. '?') then
         ! Got a non-match.  If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark.ne.NULL) then
            if(wildtext(wi:).ne. wbookmark) then
               wildtext = wbookmark;
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti) .ne. wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti).ne.NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         matchw=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (tametext(ti:ti).eq.NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi).ne.NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi).eq.NULL)exit
            enddo
         endif
         if (wildtext(wi:wi).eq.NULL)then
            matchw=.true.
            return                               ! "x" matches "x"
         endif
         matchw=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function matchw
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_matchw()
! This main() routine passes a bunch of test strings into the above code.
! In performance comparison mode, it does that over and over.  Otherwise,
! it does it just once.  Either way, it outputs a passed/failed result.
!
integer :: nReps
logical :: allpassed
integer :: i
   call unit_check_start('matchw',' &
      & -description ''match string with a pattern containing * and ? wildcard characters'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
  allpassed = .true.

  nReps = 1000000
  nReps = 10    ! Can choose as many repetitions as you're expecting in the real world.

  do i=1,nReps
      ! Cases with repeating character sequences.
      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
      !!cycle
      allpassed=allpassed .and. test("ab", "*?", .true.)
      allpassed=allpassed .and. test("abc", "*?", .true.)
      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
      allpassed=allpassed .and. test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
      allpassed=allpassed .and. test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
      allpassed=allpassed .and. test("mississipissippi", "*issip*ss*", .true.)
      allpassed=allpassed .and. test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
      allpassed=allpassed .and. test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)

      ! Additional cases where the '*' char appears in the tame string.
      allpassed=allpassed .and. test("*", "*", .true.)
      allpassed=allpassed .and. test("a*r", "a*", .true.)
      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)

      ! More double wildcard scenarios.
      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)

      ! Completely tame (no wildcards) cases.
      allpassed=allpassed .and. test("bLah", "bLah", .true.)

      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
      allpassed=allpassed .and. test("a", "*?", .true.)

      ! More mixed wildcard tests including coverage for false positives.
      allpassed=allpassed .and. test("a", "??", .false.)
      allpassed=allpassed .and. test("ab", "?*?", .true.)
      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)

      ! Single-character-match cases.
      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)

      ! Many-wildcard scenarios.
      allpassed=allpassed .and. test(&
      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
      &.true.)
      allpassed=allpassed .and. test(&
      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
      &.true.)
      allpassed=allpassed .and. test(&
      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
      &.false.)
      allpassed=allpassed .and. test(&
      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
      &.false.)
      allpassed=allpassed .and. test(&
      &"abababababababababababababababababababaacacacacacacacadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
      &.true.)
      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
      allpassed=allpassed .and. test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", "a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa", "*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
      allpassed=allpassed .and. test(&
      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
      &.false.)
      allpassed=allpassed .and. test(&
      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
      &.true.)
      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd", "abc*abc*abc*abc*abc", .false.)
      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd*abc*abcd*abc*abc*abcd", &
      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
      &.true.)
      allpassed=allpassed .and. test("abc", "********a********b********c********", .true.)
      allpassed=allpassed .and. test("********a********b********c********", "abc", .false.)
      allpassed=allpassed .and. test("abc", "********a********b********b********", .false.)
      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)

      ! A case-insensitive algorithm test.
      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
   enddo

   call unit_check('matchw',allpassed,msg='')
   call unit_check_done('matchw')
!===================================================================================================================================
   contains
!===================================================================================================================================
   ! This is a test program for wildcard matching routines.  It can be used
   ! either to test a single routine for correctness, or to compare the timings
   ! of two (or more) different wildcard matching routines.
   !
   function test(tame, wild, bExpectedResult) result(bpassed)
   !!use M_strings, only : matchw
      character(len=*) :: tame
      character(len=*) :: wild
      logical          :: bExpectedResult
      logical          :: bResult
      logical          :: bPassed
      bResult = .true.    ! We'll do "&=" cumulative checking.
      bPassed = .false.   ! Assume the worst.
      bResult = matchw(tame, wild) ! Call a wildcard matching routine.

      ! To assist correctness checking, output the two strings in any failing scenarios.
      if (bExpectedResult .eqv. bResult) then
         bPassed = .true.
         if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
      else
         if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
      endif

   end function test
end subroutine test_matchw
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_strings:TOKENS] parse string into an array using specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!##DESCRIPTION
!!     SPLIT(3f) parses a string using specified delimiter characters and
!!     store tokens into an allocatable array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_split
!!    use M_strings, only: split
!!    character(len=*),parameter     :: &
!!    & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!    character(len=:),allocatable :: array(:) ! output array of tokens
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,'(80("="))')
!!       write(*,*)'typical call:'
!!       CALL split(line,array)
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(80("-"))')
!!       write(*,*)'custom list of delimiters (colon and vertical line):'
!!       CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(80("-"))')
!!       write(*,*)&
!!     &'custom list of delimiters, reverse array order and count null fields:'
!!       CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!       write(*,'(80("-"))')
!!       write(*,*)'INPUT LINE:['//LINE//']'
!!       write(*,*)&
!!       &'default delimiters and reverse array order and return null fields:'
!!       CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!       write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!       write(*,*)'SIZE:',SIZE(array)
!!    end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_7="&
&@(#)M_strings::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: ilen                   ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (:0)                                                      ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
   case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_split
!!  split: parse a string using specified delimiter characters and store tokens into an array
!!$SYSTEM  goodbad split start -library libGPF -filename `pwd`/M_strings.FF --section 3
!!   USE M_strings, ONLY: split
INTRINSIC SIZE
CHARACTER(LEN=:),ALLOCATABLE    :: line
CHARACTER(LEN=:),ALLOCATABLE    :: order
CHARACTER(LEN=:),ALLOCATABLE    :: dlm
CHARACTER(LEN=:),ALLOCATABLE    :: array(:)
character(len=10)               :: orders(3)=['sequential', '          ', 'reverse   ' ]
! return strings composed of delimiters or not IGNORE|RETURN|IGNOREEND
character(len=10)               :: nulls(3)=['ignore    ', 'return    ', 'ignoreend ' ]
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('split',' &
      & -description ''subroutine parses string on delimiters and store tokens into an array'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   dlm=''
   LINE='abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   order=orders(3)
   CALL testit()
   CALL split(line,array,dlm,order,nulls(1))
   if(unit_check_level.gt.0)then
      write(*,*)size(array)
   endif
   order=orders(2)
   CALL split(line,array,dlm,order,nulls(2))
   if(unit_check_level.gt.0)then
      write(*,*)size(array)
   endif
   order=orders(1)
   CALL split(line,array,dlm,order,nulls(3))
   if(unit_check_level.gt.0)then
      write(*,*)size(array)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE='        abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' aABCDEF  ; b;;c d e;  ;  '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm='; '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   LINE=';;;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;;'
   CALL testit()
   LINE=';;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE=';abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE='abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   line='a b c d e f g h i j k l m n o p q r s t u v w x y z'
   CALL split(line,array)
   call unit_check('split',size(array).eq.26,msg='test delimiter')
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=' '
   CALL split(line,array,dlm)
   call unit_check('split',size(array).eq.26,msg='test delimiter')
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_done('split')
!-----------------------------------------------------------------------------------------------------------------------------------
   CONTAINS
!-----------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE testit()
   integer :: i
   if(unit_check_level.gt.0)then
      WRITE(*,'(80("="))')
      WRITE(*,'(A)')'parsing ['//TRIM(line)//']'//'with delimiters set to ['//dlm//'] and order '//trim(order)//''
   endif
   CALL split(line,array,dlm,order)
   if(unit_check_level.gt.0)then
      WRITE(*,'("number of tokens found=",i0)')SIZE(array)
      WRITE(*,'(I0,T10,A)')(i,TRIM(array(i)),i=1,SIZE(array))
   endif
   END SUBROUTINE testit
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    chomp(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function chomp(source_string,token[,delimiters])
!!
!!     character(len=*)                     :: source_string
!!     character(len=:),intent(out),token   :: token
!!     character(len=:),intent(in),optional :: delimiters
!!     integer                              :: chomp
!!##DESCRIPTION
!!    The CHOMP(3f) function is used to isolate sequential tokens in a
!!    string, SOURCE_STRING. These tokens are delimited in the string by at
!!    least one of the characters in DELIMITERS. This routine consumes the
!!    source_string one token per call. It returns -1 when complete. The
!!    default delimiter list is "space,tab,carriage return,newline".
!!
!!##OPTIONS
!!     SOURCE_STRING  string to tokenize
!!     DELIMITERS     list of separator characters
!!
!!##RETURNS
!!     TOKEN          returned token
!!     CHOMP          status flag. 0 = success, -1 = no tokens remain
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!    program demo_chomp
!!
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=100)            :: inline
!!    character(len=:),allocatable  :: token
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer                       :: ios
!!    integer                       :: icount
!!    integer                       :: itoken
!!       icount=0
!!       do        ! read lines from stdin until end-of-file or error
!!          read (unit=*,fmt="(a)",iostat=ios) inline
!!          if(ios.ne.0)stop
!!          icount=icount+1
!!          itoken=0
!!          write(*,*)'INLINE ',trim(inline)
!!          do while ( chomp(inline,token,delimiters).ge. 0)
!!             itoken=itoken+1
!!             print *, itoken,'TOKEN=['//trim(token)//']'
!!          enddo
!!       enddo
!!
!!    end program demo_chomp
!!
!!   sample input file
!!
!!     this is a test of chomp; A:B :;,C;;
!!
!!   sample output file
!!
!!     INLINE     this is a test of chomp; A:B :;,C;;
!!               1 TOKEN=[this]
!!               2 TOKEN=[is]
!!               3 TOKEN=[a]
!!               4 TOKEN=[test]
!!               5 TOKEN=[of]
!!               6 TOKEN=[chomp]
!!               7 TOKEN=[A:B]
!!               8 TOKEN=[:]
!!               9 TOKEN=[C]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
FUNCTION chomp(source_string,token,delimiters)

character(len=*),parameter::ident_8="@(#)M_strings::chomp(3f): Tokenize a string : JSU- 20151030"

character(len=*)                         :: source_string    ! string to tokenize
character(len=:),allocatable,intent(out) :: token            ! returned token
character(len=*),intent(in),optional     :: delimiters       ! list of separator characters
integer                                  :: chomp            ! returns copy of shifted source_string
character(len=:),allocatable             :: delimiters_local
integer                                  :: token_start      ! beginning of token found if function result is .true.
integer                                  :: token_end        ! end of token found if function result is .true.
integer                                  :: isource_len
!-----------------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(present(delimiters))then
      delimiters_local=delimiters
   else                                          ! increment start to previous end + 1
      delimiters_local=char(32)//char(09)//char(10)//char(13) ! space,horizontal tab, newline, carriage return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   token_start=1
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)                         ! step thru each character to find next delimiter, if any
      if(index(delimiters_local,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
   !write(*,*)'TOKEN_START ',token_start
   !write(*,*)'TOKEN_END   ',token_end
   chomp=isource_len-token_end
   if(chomp.ge.0)then
      token=source_string(token_start:token_end)
      source_string=source_string(token_end+1:)
   else
      token=''
      source_string=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function chomp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chomp()
!!use M_strings, only : chomp
character(len=:),allocatable  :: str
character(len=:),allocatable  :: token
character(len=66),allocatable :: delimiters
integer                       :: ipass
   call unit_check_start('chomp',' &
      & -description ''function consumes input line and returns next token using delimiters'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   str = 'a b ccc ddd x12#$)$*#@Z1!( ab cd ef'
   delimiters=' #@$)*!('
   ipass=0
   do while ( chomp(str,token,delimiters) .ge. 0 )
      ipass=ipass+1
      if(unit_check_level.gt.0)then
         print *, ipass,'TOKEN=['//trim(token)//']'
      endif
      select case(ipass)
      case(1); call unit_check('chomp', token.eq.'a'   ,msg=msg('token=',token))
      case(2); call unit_check('chomp', token.eq.'b'   ,msg=msg('token=',token))
      case(3); call unit_check('chomp', token.eq.'ccc' ,msg=msg('token=',token))
      case(4); call unit_check('chomp', token.eq.'ddd' ,msg=msg('token=',token))
      case(5); call unit_check('chomp', token.eq.'x12' ,msg=msg('token=',token))
      case(6); call unit_check('chomp', token.eq.'Z1'  ,msg=msg('token=',token))
      case(7); call unit_check('chomp', token.eq.'ab'  ,msg=msg('token=',token))
      case(8); call unit_check('chomp', token.eq.'cd'  ,msg=msg('token=',token))
      case(9); call unit_check('chomp', token.eq.'ef'  ,msg=msg('token=',token))
      end select
   enddo

   call unit_check_done('chomp')
end subroutine test_chomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      delim(3f) - [M_strings:TOKENS] parse a string and store tokens into an array
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)
!!
!!     character(len=*),intent(in)  :: line
!!     integer,integer(in)          :: n
!!     integer,intent(out)          :: icount
!!     character(len=*)             :: array(n)
!!     integer,intent(out)          :: ibegin(n)
!!     integer,intent(out)          :: iterm(n)
!!     integer,intent(out)          :: ilen
!!     character(len=*)             :: dlim
!!##DESCRIPTION
!!
!!      Given a LINE of structure " par1 par2 par3 ... parn "
!!      store each par(n) into a separate variable in ARRAY (UNLESS
!!      ARRAY(1).eq.'#N#')
!!
!!      Also set ICOUNT to number of elements of array initialized, and
!!      return beginning and ending positions for each element in IBEGIN(N)
!!      and ITERM(N).
!!
!!      Return position of last non-blank character (even if more
!!      than N elements were found) in ILEN
!!
!!      No quoting or escaping of delimiter is allowed, so the delimiter
!!      character can not be placed in a token.
!!
!!      No checking for more than N parameters; If any more they are ignored.
!!
!!##OPTIONS
!!    LINE      input string to parse into tokens
!!    ARRAY(N)  array that receives tokens
!!    N         size of arrays ARRAY, IBEGIN, ITERM
!!    ICOUNT    number of tokens found
!!    IBEGIN(N) starting columns of tokens found
!!    ITERM(N)  ending columns of tokens found
!!    ILEN      position of last non-blank character in input string LINE
!!    DLIM      delimiter characters
!!
!!##EXAMPLES
!!
!!  Sample program:
!!
!!     program demo_delim
!!
!!     use M_strings, only: delim
!!     character(len=80) :: line
!!     character(len=80) :: dlm
!!     integer,parameter :: n=10
!!     character(len=20) :: array(n)=' '
!!     integer           :: ibegin(n),iterm(n)
!!     line=' first  second 10.3 words_of_stuff  '
!!     do i20=1,4
!!        ! change delimiter list and what is calculated or parsed
!!        if(i20.eq.1)dlm=' '
!!        if(i20.eq.2)dlm='o'
!!        if(i20.eq.3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
!!        if(i20.eq.3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
!!        if(i20.eq.4)line='AAAaBBBBBBbIIIIIi  J K L'
!!
!!        ! write out a break line composed of =========== ..
!!        write(*,'(57("="))')
!!        ! show line being parsed
!!        write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
!!        ! call parsing procedure
!!        call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)
!!        write(*,*)'number of tokens found=',icount
!!        write(*,*)'last character in column ',ilen
!!        if(icount.gt.0)then
!!           if(ilen.ne.iterm(icount))then
!!              write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
!!           endif
!!           do i10=1,icount
!!              ! check flag to see if ARRAY() was set
!!              if(array(1).ne.'#N#')then
!!                 ! from returned array
!!                 write(*,'(a,a,a)',advance='no')&
!!                 &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
!!              endif
!!           enddo
!!           ! using start and end positions in IBEGIN() and ITERM()
!!           write(*,*)
!!           do i10=1,icount
!!              ! from positions in original line
!!              write(*,'(a,a,a)',advance='no')&
!!              &'[',line(ibegin(i10):iterm(i10)),']'
!!           enddo
!!           write(*,*)
!!        endif
!!     enddo
!!     end program demo_delim
!!
!!  Expected output
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine delim(line,array,n,icount,ibegin,iterm,ilen,dlim)

character(len=*),parameter::ident_9="@(#)M_strings::delim(3f): parse a string and store tokens into an array"

!
!     given a line of structure " par1 par2 par3 ... parn "
!     store each par(n) into a separate variable in array.
!
!     IF ARRAY(1) == '#N#' do not store into string array  (KLUDGE))
!
!     also count number of elements of array initialized, and
!     return beginning and ending positions for each element.
!     also return position of last non-blank character (even if more
!     than n elements were found).
!
!     no quoting of delimiter is allowed
!     no checking for more than n parameters, if any more they are ignored
!
character(len=*),intent(in)    :: line
integer,intent(in)             :: n
character(len=*)               :: array(n)
integer,intent(out)            :: icount
integer,intent(out)            :: ibegin(n)
integer,intent(out)            :: iterm(n)
integer,intent(out)            :: ilen
character(len=*),intent(in)    :: dlim
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(line)):: line_local
logical             :: lstore
integer             :: i10
integer             :: iarray
integer             :: icol
integer             :: idlim
integer             :: iend
integer             :: ifound
integer             :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
      icount=0
      ilen=len_trim(line)
      line_local=line

      idlim=len(dlim)
      if(idlim > 5)then
         idlim=len_trim(dlim)      ! dlim a lot of blanks on some machines if dlim is a big string
         if(idlim == 0)then
            idlim=1     ! blank string
         endif
      endif

      if(ilen == 0)then                                        ! command was totally blank
         return
      endif
!
!     there is at least one non-blank character in the command
!     ilen is the column position of the last non-blank character
!     find next non-delimiter
      icol=1

      if(array(1) == '#N#')then                                ! special flag to not store into character array
         lstore=.false.
      else
         lstore=.true.
      endif

      do iarray=1,n,1                                          ! store into each array element until done or too many words
         NOINCREMENT: do
            if(index(dlim(1:idlim),line_local(icol:icol)) == 0)then  ! if current character is not a delimiter
               istart=icol                                     ! start new token on the non-delimiter character
               ibegin(iarray)=icol
               iend=ilen-istart+1+1                            ! assume no delimiters so put past end of line
               do i10=1,idlim
                  ifound=index(line_local(istart:ilen),dlim(i10:i10))
                  if(ifound > 0)then
                     iend=min(iend,ifound)
                  endif
               enddo
               if(iend <= 0)then                               ! no remaining delimiters
                 iterm(iarray)=ilen
                 if(lstore)then
                    array(iarray)=line_local(istart:ilen)
                 endif
                 icount=iarray
                 return
               else
                 iend=iend+istart-2
                 iterm(iarray)=iend
                 if(lstore)then
                    array(iarray)=line_local(istart:iend)
                 endif
               endif
               icol=iend+2
               exit NOINCREMENT
            endif
            icol=icol+1
         enddo NOINCREMENT
!        last character in line was a delimiter, so no text left
!        (should not happen where blank=delimiter)
         if(icol > ilen)then
           icount=iarray
           if( (iterm(icount)-ibegin(icount)) < 0)then         ! last token was all delimiters
              icount=icount-1
           endif
           return
         endif
      enddo
      icount=n  ! more than n elements
end subroutine delim
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_delim()
character(len=80) :: line
character(len=80) :: dlm
integer,parameter :: n=10
character(len=20) :: array(n)=' '
integer           :: ibegin(n),iterm(n)
integer           :: icount
integer           :: ilen
   call unit_check_start('delim',' &
      & -description ''subroutine parses string using delimiters and stores tokens into array'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   line=' first  second 10.3 words_of_stuff  '
   dlm=' '

   call testit()
   call unit_check('delim',icount.eq.4,msg=' check number of tokens')
   call unit_check('delim',ilen.eq.34,msg=' check position of last character')
   call unit_check('delim',all(array(:icount).eq.[character(len=20) :: 'first','second','10.3','words_of_stuff']),msg='tokens')

   ! change delimiter list and what is calculated or parsed
   dlm=' aeiou'    ! NOTE SPACE IS FIRST
   call testit()
   call unit_check('delim',all(array(:icount).eq.&
           [character(len=10) :: 'f','rst','s','c','nd','10.3','w','rds_','f_st','ff']),msg='delims')

  call unit_check_done('delim')
contains
subroutine testit()
integer                 :: i10
   ! show line being parsed
   ! call parsing procedure
   call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)

   if(unit_check_level.gt.0)then
      write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
      write(*,*)'number of tokens found=',icount
      write(*,*)'last character in column ',ilen
      if(icount.gt.0)then
         if(ilen.ne.iterm(icount))then
            write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
         endif
         do i10=1,icount
            ! check flag to see if ARRAY() was set
            if(array(1).ne.'#N#')then
               ! from returned array
               write(*,'(a,a,a)',advance='no')'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
            endif
         enddo
         ! using start and end positions in IBEGIN() and ITERM()
         write(*,*)
         do i10=1,icount
            ! from positions in original line
            write(*,'(a,a,a)',advance='no') '[',line(ibegin(i10):iterm(i10)),']'
         enddo
         write(*,*)
      endif
   endif
end subroutine testit
end subroutine test_delim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_strings:EDITING] function globally replaces one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace(targetline[,old,new|cmd],range,ierr) result (newline)
!!
!!     character(len=*)                       :: targetline
!!     character(len=*),intent(in),optional   :: old
!!     character(len=*),intent(in),optional   :: new
!!     character(len=*),intent(in),optional   :: cmd
!!     integer,intent(in),optional            :: range(2)
!!     integer,intent(out),optional           :: ierr
!!     logical,intent(in),optional            :: clip
!!     character(len=:),allocatable           :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new"
!!     range       if present, only change range(1) to range(2) of occurrences of old string
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!     clip        whether to return trailing spaces or not. Defaults to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_replace
!!    use M_strings, only : replace
!!    implicit none
!!    character(len=:),allocatable :: targetline
!!
!!    targetline='this is the input string'
!!
!!    call testit('th','TH','THis is THe input string')
!!
!!    ! a null old substring means "at beginning of line"
!!    call testit('','BEFORE:', 'BEFORE:THis is THe input string')
!!
!!    ! a null new string deletes occurrences of the old substring
!!    call testit('i','', 'BEFORE:THs s THe nput strng')
!!
!!    write(*,*)'Examples of the use of RANGE='
!!
!!    targetline=replace('a b ab baaa aaaa','a','A')
!!    write(*,*)'replace a with A ['//targetline//']'
!!
!!    targetline=replace('a b ab baaa aaaa','a','A',range=[3,5])
!!    write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!    targetline=replace('a b ab baaa aaaa','a','',range=[3,5])
!!    write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!    targetline=replace('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',range=[3,5])
!!    write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'
!!
!!    contains
!!    subroutine testit(old,new,expected)
!!    character(len=*),intent(in) :: old,new,expected
!!    write(*,*)repeat('=',79)
!!    write(*,*)'STARTED ['//targetline//']'
!!    write(*,*)'OLD['//old//']', ' NEW['//new//']'
!!    targetline=replace(targetline,old,new)
!!    write(*,*)'GOT     ['//targetline//']'
!!    write(*,*)'EXPECTED['//expected//']'
!!    write(*,*)'TEST    [',targetline.eq.expected,']'
!!    end subroutine testit
!!
!!    end program demo_replace
!!
!!   Expected output
!!
!!     ===============================================================================
!!     STARTED [this is the input string]
!!     OLD[th] NEW[TH]
!!     GOT     [THis is THe input string]
!!     EXPECTED[THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [THis is THe input string]
!!     OLD[] NEW[BEFORE:]
!!     GOT     [BEFORE:THis is THe input string]
!!     EXPECTED[BEFORE:THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [BEFORE:THis is THe input string]
!!     OLD[i] NEW[]
!!     GOT     [BEFORE:THs s THe nput strng]
!!     EXPECTED[BEFORE:THs s THe nput strng]
!!     TEST    [ T ]
!!     Examples of the use of RANGE=
!!     replace a with A [A b Ab bAAA AAAA]
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax.ge.4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace(targetline,old,new,ierr,cmd,range) result (newline)

character(len=*),parameter::ident_10="@(#)M_strings::replace(3f): Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: ichar
integer                       :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2.ne.0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace* must specify OLD and NEW or CMD')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(range))then
      range_local=range
   else
      range_local=[1,original_input_length]
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(len_new.gt.0)then
         newline=new_local(:len_new)//targetline(left_margin:original_input_length)
      else
         newline=targetline(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=left_margin                                   ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old_local(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.right_margin)then          ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:ichar-1)//targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(icount.ge.range_local(1).and.icount.le.range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new.ne.0)then                                          ! put in new string
            newline=newline(:ichar-1)//new_local(:len_new)
            ichar=ichar+len_new
         endif
      else
         if(len_old.ne.0)then                                          ! put in copy of old string
            newline=newline(:ichar-1)//old_local(:len_old)
            ichar=ichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline                               ! if no changes made output should be input
   case default
      if(ic.lt.len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:ichar-1)//targetline(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replace()
character(len=:),allocatable :: targetline

   call unit_check_start('replace',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   targetline='this is the input string'

   call testit('th','TH','THis is THe input string')

   ! a null old substring means "at beginning of line"
   call testit('','BEFORE:', 'BEFORE:THis is THe input string')

   ! a null new string deletes occurrences of the old substring
   call testit('i','', 'BEFORE:THs s THe nput strng')

   targetline=replace('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',range=[3,5])
   call unit_check('replace',targetline.eq.'a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa','example of using RANGE=')
   call unit_check_done('replace')

contains
subroutine testit(old,new,expected)
character(len=*),intent(in) :: old,new,expected

   if(unit_check_level.gt.0)then
      write(*,*)repeat('=',79)
      write(*,*)'STARTED ['//targetline//']'
      write(*,*)'OLD['//old//']', ' NEW['//new//']'
   endif

   targetline=replace(targetline,old,new)

   if(unit_check_level.gt.0)then
      write(*,*)'GOT     ['//targetline//']'
      write(*,*)'EXPECTED['//expected//']'
   endif
   call unit_check('replace',targetline.eq.expected,msg='')
   call unit_check_done('replace',msg='finished test of replacing substrings')
end subroutine testit
end subroutine test_replace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_strings:EDITING] subroutine globally substitutes one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_substitute
!!    use M_strings, only : substitute
!!    implicit none
!!    ! must be long enough to hold changed line
!!    character(len=80) :: targetline
!!
!!    targetline='this is the input string'
!!    write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!    ! changes the input to 'THis is THe input string'
!!    call substitute(targetline,'th','TH')
!!    write(*,*)'th => TH    : '//trim(targetline)
!!
!!    ! a null old substring means "at beginning of line"
!!    ! changes the input to 'BEFORE:this is the input string'
!!    call substitute(targetline,'','BEFORE:')
!!    write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!    ! a null new string deletes occurrences of the old substring
!!    ! changes the input to 'ths s the nput strng'
!!    call substitute(targetline,'i','')
!!    write(*,*)'i => ""     : '//trim(targetline)
!!
!!    end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

character(len=*),parameter::ident_11="@(#)M_strings::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      ichar=len_new + original_input_length
      if(ichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ichar=il                                            ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(ichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichar:)=targetline(ic:ind-1)
         ichar=ichar+ladd
      endif
      if(ichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(ichar:)=new(:len_new)
         ichar=ichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(ichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_substitute
!!use M_strings, only : substitute
implicit none
character(len=:),allocatable    :: targetline   ! input line to be changed
character(len=:),allocatable    :: old          ! old substring to replace
character(len=:),allocatable    :: new          ! new substring
integer                         :: ml           ! ml sets the left  margin
integer                         :: mr           ! mr sets the right margin
integer                         :: ier          ! error code. if ier = -1 bad directive, >= 0then ier changes made
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('substitute','-description ''subroutine globally replaces old substring with new string'' ')
   targetline='This an that and any other '
   old='an'
   new='##'
   ml=1
   mr=len(targetline)
   if(unit_check_level.gt.0)then
      write(*,*)'ORIGINAL: '//targetline
   endif
   call substitute(targetline,old,new,ier,ml,mr) !Globally substitute one substring for another in string
   if(unit_check_level.gt.0)then
      write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ier.ne.3)then
      if(unit_check_level.gt.0)then
         write(*,*)ier,targetline
      endif
      call unit_check_bad('substitute')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(targetline.ne.'This ## that ##d ##y other ')then
      call unit_check_bad('substitute')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   targetline='This and that, This and that,                               '
   if(unit_check_level.gt.0)then
      write(*,*)'ORIGINAL: '//targetline
   endif

   old=''
   new='BEGINNING: '
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level.gt.0)then
      write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='This'
   new='THIS'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level.gt.0)then
      write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='that'
   new='LONGER STRING'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level.gt.0)then
      write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='LONGER STRING'
   new=''
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level.gt.0)then
      write(*,*)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   if ( targetline .ne. 'BEGINNING: THIS and , THIS and ,')then
      call unit_check_bad('substitute')
      stop 3
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_good('substitute')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    change(3f) - [M_strings:EDITING] change old string to new string with a directive like a line editor
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine change(target_string,cmd,ierr)
!!
!!     character(len=*),intent(inout) :: target_string
!!     character(len=*),intent(in)    :: cmd
!!     integer                        :: ierr
!!##DESCRIPTION
!!    change an old substring into a new substring in a character variable
!!    like a line editor. Primarily used to create interactive utilities
!!    such as input history editors for interactive line-mode programs. The
!!    output string is assumed long enough to accommodate the change.
!!    a directive resembles a line editor directive of the form
!!
!!       C/old_string/new_string/
!!
!!    where / may be any character which is not included in old_string
!!    or new_string.
!!
!!    a null old_string implies "beginning of string".
!!
!!##OPTIONS
!!    target_string  line to be changed
!!    cmd            contains instructions to change the string
!!    ierr           error code.
!!
!!       o =-1 bad directive
!!       o =0 no changes made
!!       o >0 count of changes made
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_change
!!
!!     use M_strings, only : change
!!     implicit none
!!     character(len=132) :: line='This is a test string to change'
!!     integer            :: ierr
!!        write(*,*)trim(line)
!!        ! change miniscule a to uppercase A
!!        call change(line,'c/a/A/',ierr)
!!        write(*,*)trim(line)
!!        ! put string at beginning of line
!!        call change(line,'c//prefix: /',ierr)
!!        write(*,*)trim(line)
!!        ! remove blanks
!!        call change(line,'c/ //',ierr)
!!        write(*,*)trim(line)
!!    end program demo_change
!!
!!   Expected output
!!
!!     This is a test string to change
!!     This is A test string to chAnge
!!     prefix: This is A test string to chAnge
!!     prefix:ThisisAteststringtochAnge
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine change(target_string,cmd,ierr)
! Change a string assumed long enough to accommodate the change, with a directive that resembles a line editor directive of the form
!    C/old_string/new_string/
! where / may be any character which is not included in old_string or new_string.
! a null old_string implies "beginning of string"
!===================================================================================================================================

character(len=*),parameter::ident_12="@(#)M_strings::change(3f): change a character string like a line editor"

character(len=*),intent(inout)   :: target_string          ! line to be changed
character(len=*),intent(in)      :: cmd                    ! contains the instructions changing the string
character(len=1)                 :: delimiters
integer                          :: ierr                   ! error code. ier=-1 bad directive;=0 no changes made;>0 ier changes made
integer                          :: itoken
integer,parameter                :: id=2                   ! expected location of delimiter
character(len=:),allocatable     :: old,new                ! scratch string buffers
logical                          :: ifok
integer                          :: lmax                   ! length of target string
integer                          :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   lmax=len_trim(cmd)                                                          ! significant length of change directive
   if(lmax.ge.4)then                         ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)                                                    ! find delimiter in expected location
      itoken=0                                                                 ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))    ! change old substrings to new substrings
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_change
!!use M_strings, only : change
!character(len=132) :: direc
character(len=132)  :: line=' The rain in Spain falls mainly on the plain. '
integer             :: ier
   if(unit_check_level.gt.0)then
      write(*,*)' LINE='//trim(line)
   endif
   ! indicate test of change(3f) has begun
   call unit_check_start('change',' &
      & -description ''replace substring with new string with a directive like line editor'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call change(line, 'c/ain/AIN'     ,ier)
   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier
   endif
   call unit_check('change',ier.eq.4,'without trailing slash')

   call change(line, 'c/ The r/R/'   ,ier)
   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier
   endif
   call unit_check('change',ier.eq.1,'with trailing slash')

   call change(line, 'c/ /'          ,ier)
   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier
   endif
   call unit_check('change',ier.ge.7,'no new string') ! remove spaces

   call change(line, 'c//PREFIX:'    ,ier)
   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier
   endif
   call unit_check('change',ier.eq.1,'null new string')

   call change(line, 'c/XYZ/xxxxxx:' ,ier)
   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier
   endif
   call unit_check('change',ier.eq.0,'no matching old string')

   if(unit_check_level.gt.0)then
      write(*,*)'IER=',ier,' LINE='//trim(line)
   endif
   call unit_check('change','PREFIX:RAINinSpAINfallsmAINlyontheplAIN.' .eq. line,'check cumulative changes')

   call unit_check_done('change') ! indicate test of change(3f) passed
end subroutine test_change
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     strtok(3f) - [M_strings:TOKENS] Tokenize a string
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!       function strtok(source_string,itoken,token_start,token_end,delimiters)
!!                 result(strtok_status)
!!
!!        logical                      :: strtok_status    ! returned value
!!        character(len=*),intent(in)  :: source_string    ! string to tokenize
!!        integer,intent(inout)        :: itoken           ! token count since started
!!        integer,intent(out)          :: token_start      ! beginning of token
!!        integer,intent(inout)        :: token_end        ! end of token
!!        character(len=*),intent(in)  :: delimiters       ! list of separator characters
!!
!!##DESCRIPTION
!!     The STRTOK(3f) function is used to isolate sequential tokens in a string,
!!     SOURCE_STRING. These tokens are delimited in the string by at least one of
!!     the characters in DELIMITERS. The first time that STRTOK(3f) is called,
!!     ITOKEN should be specified as zero. Subsequent calls, wishing to obtain
!!     further tokens from the same string, should pass back in TOKEN_END  and
!!     ITOKEN until the function result returns .false.
!!
!!     This routine assumes no other calls are made to it using any other input
!!     string while it is processing an input line.
!!
!!##OPTIONS
!!     source_string   input string to parse
!!     itoken          token count should be set to zero for a new string
!!     delimiters      characters used to determine the end of tokens
!!##RETURN
!!     token_start     beginning position in SOURCE_STRING where token was found
!!     token_end       ending position in SOURCE_STRING where token was found
!!     strtok_status
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     !===============================================================================
!!     program demo_strtok
!!     use M_strings, only : strtok
!!     character(len=264)          :: inline
!!     character(len=*),parameter  :: delimiters=' ;,'
!!     integer                     :: ios
!!     !-------------------------------------------------------------------------------
!!        do                        ! read lines from stdin until end-of-file or error
!!           read (unit=*,fmt="(a)",iostat=ios) inline
!!           if(ios.ne.0)stop
!!           itoken=0 ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
!!           do while ( strtok(inline,itoken,istart,iend,delimiters) )
!!              print *, itoken,'TOKEN=['//(inline(istart:iend))//']',istart,iend
!!           enddo
!!        enddo
!!     end program demo_strtok
!!     !===============================================================================
!!
!!     sample input file
!!
!!      this is a test of strtok; A:B :;,C;;
!!
!!     sample output file
!!
!!     1  TOKEN=[this]    2   5
!!     2  TOKEN=[is]      7   8
!!     3  TOKEN=[a]       10  10
!!     4  TOKEN=[test]    12  15
!!     5  TOKEN=[of]      17  18
!!     6  TOKEN=[strtok]  20  25
!!     7  TOKEN=[A:B]     28  30
!!     8  TOKEN=[:]       32  32
!!     9  TOKEN=[C]       35  35
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

character(len=*),parameter::ident_13="@(#)M_strings::strtok(3f): Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer,save                 :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strtok()
integer,parameter             :: length=264
character(len=length)         :: inline
integer                       :: istart(length/2+1)
integer                       :: iend(length/2+1)
integer,allocatable           :: istart_expected(:)
integer,allocatable           :: iend_expected(:)
character(len=:),allocatable  :: words_expected(:)
character(len=*),parameter    :: delimiters=' ;,'
integer                       :: is,ie
integer                       :: itoken
   call unit_check_start('strtok',' &
   & -section 3  &
   & -library libGPF  &
   & -filename `pwd`/M_strings.FF &
   & -documentation y &
   & -ufpp         y &
   & -ccall        n &
   & -archive      GPF.a &
   & ')
   istart_expected=[ 2,  7,  10,  12,  17,  20,  28,  32,  35 ]
   iend_expected=[ 5,  8,  10,  15,  18,  25,  30,  32,  35 ]
   words_expected=[ character(len=10) :: 'this', 'is', 'a', 'test', 'of', 'strtok', 'A:B', ':', 'C']

   inline=' this is a test of strtok; A:B :;,C;;'

   itoken=0 ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
   if(unit_check_level.gt.0)then
      write(*,*)trim(inline)
   endif
   do while ( strtok(inline,itoken,is,ie,delimiters) )
      istart(itoken)=is
      iend(itoken)=ie
      if(unit_check_level.gt.0)then
         print *, itoken,'TOKEN=['//(inline(istart(itoken):iend(itoken)))//']',istart(itoken),iend(itoken)
      endif
   enddo
   call unit_check('strtok',all(istart(:itoken).eq.istart_expected) .and. &
      all(iend(:itoken).eq.iend_expected), &
      msg='parse a line into tokens with strtok(3f)')
   call unit_check_done('strtok')
end subroutine test_strtok
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    modif(3f) - [M_strings:EDITING] emulate the MODIFY command from the line editor XEDIT
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine modif(cline,cmod)
!!
!!     character(len=*) :: cline ! input string to change
!!     character(len=*) :: cmod  ! directive provides directions on changing string
!!##DESCRIPTION
!!
!!   MODIF(3f) Modifies the line currently pointed at using a directive
!!   that acts much like a line editor directive.
!!   Primarily used to create interactive utilities such as input history
!!   editors for interactive line-mode programs.
!!
!!   the modify directives are as follows-
!!
!!    DIRECTIVE EXPLANATION
!!
!!    ^STRING#   Causes the string of characters between the ^ and the
!!               next # to be inserted before the characters pointed to
!!               by the ^. an ^ or & within the string is treated as a
!!               regular character. If the closing # is not specified,
!!               MODIF(3f) inserts the remainder of the line as if a # was
!!               specified after the last nonblank character.
!!
!!               There are two exceptions. the combination ^# causes a #
!!               to be inserted before the character pointed to by the
!!               ^, and an ^ as the last character of the directives
!!               causes a blank to be inserted.
!!
!!    #          (When not the first # after an ^) causes the character
!!               above it to be deleted.
!!
!!    &          Replaces the character above it with a space.
!!
!!    (SPACE)    A space below a character leaves it unchanged.
!!
!!    Any other character replaces the character above it.
!!
!!##EXAMPLES
!!
!!   Example input/output:
!!
!!    THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
!!    THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
!!    ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!!
!!   Sample program:
!!
!!    program demo_modif
!!    use M_strings, only : modif
!!    implicit none
!!    character(len=256)           :: line
!!    integer                      :: ios
!!    integer                      :: count
!!    integer                      :: COMMAND_LINE_LENGTH
!!    character(len=:),allocatable :: COMMAND_LINE
!!       ! get command name length
!!       call get_command_argument(0,length=count)
!!       ! get command line length
!!       call get_command(length=COMMAND_LINE_LENGTH)
!!       ! allocate string big enough to hold command line
!!       allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
!!       ! get command line as a string
!!       call get_command(command=COMMAND_LINE)
!!       ! trim leading spaces just in case
!!       COMMAND_LINE=adjustl(COMMAND_LINE)
!!       ! remove command name
!!       COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios)line
!!          if(ios.ne.0)exit
!!          call modif(line,COMMAND_LINE)
!!          write(*,'(a)')trim(line)
!!       enddo INFINITE
!!    end program demo_modif
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
SUBROUTINE MODIF(CLINE,MOD)

!$@(#) M_strings::modif(3f): Emulate the MODIFY command from the line editor XEDIT

!
! MODIF
! =====
! ACTION- MODIFIES THE LINE CURRENTLY POINTED AT. THE INPUT STRING CLINE IS ASSUMED TO BE LONG ENOUGH TO ACCOMMODATE THE CHANGES
!         THE MODIFY DIRECTIVES ARE AS FOLLOWS-
!
!   DIRECTIVE                       EXPLANATION
!   ---------                       ------------
!   ^STRING#   CAUSES THE STRING OF CHARACTERS BETWEEN THE ^ AND THE
!              NEXT  # TO BE INSERTED BEFORE THE CHARACTERS POINTED TO
!              BY THE ^.  AN ^ OR & WITHIN THE STRING IS TREATED AS A
!              REGULAR CHARACTER.  IF THE CLOSING # IS NOT SPECIFIED,
!              MODIF(3f) INSERTS THE REMAINDER OFTHELINE AS IF A # WAS
!              SPECIFIED AFTER THE LAST NONBLANK CHARACTER.
!
!              THERE ARE TWO EXCEPTIONS. THE COMBINATION ^# CAUSES A #
!              TO BE INSERTED BEFORE THE CHARACTER POINTED TO BY THE
!              ^,  AND AN ^ AS THE LAST CHARACTER OF THE DIRECTIVES
!              CAUSES A BLANK TO BE INSERTED.
!
!   #          (WHEN NOT THE FIRST # AFTER AN ^) CAUSES THE CHARACTER
!              ABOVE IT TO BE DELETED.
!
!   &          REPLACES THE CHARACTER ABOVE IT WITH A SPACE.
!
!   (SPACE)    A SPACE BELOW A CHARACTER LEAVES IT UNCHANGED.
!
!   ANY OTHER CHARACTER REPLACES THE CHARACTER ABOVE IT.
!
! EXAMPLE-
! THE INPUT LINE........ 10 THIS STRING  TO BE MORTIFD
! THE DIRECTIVES LINE...        ^ IS THE#        D#  ^IE
! ALTERED INPUT LINE.... 10 THIS IS THE STRING  TO BE MODIFIED
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
character(len=*)            :: cline        !STRING TO BE MODIFIED
character(len=*),intent(in) :: mod          !STRING TO DIRECT MODIFICATION
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'      !ASSIGN DEFAULT EDIT CHARACTERS
integer                     :: maxscra      !LENGTH OF SCRATCH BUFFER
character(len=len(cline))   :: dum2         !SCRATCH CHARACTER BUFFER
logical                     :: linsrt       !FLAG FOR INSERTING DATA ON LINE
integer :: i, j, ic, ichar, iend, lmax, lmx1
maxscra=len(cline)
   CMOD=TRIM(MOD)
   LMAX=MIN0(LEN(CLINE),MAXSCRA)         !DETERMINE MAXIMUM LINE LENGTH
   LMX1=LMAX-1                           !MAX LINE LENGTH -1
   DUM2=' '                              !INITIALIZE NEW LINE
   LINSRT=.FALSE.                        !INITIALIZE INSERT MODE
   IEND=len_trim(CMOD)                   !DETERMINE END OF MODS
   I=0                                   !CHAR COUNTER FOR MOD LINE CMOD
   IC=0                                  !CHAR COUNTER FOR CURRENT LINE CLINE
   ICHAR=0                               !CHAR COUNTER NEW LINE DUM2
11 CONTINUE
   I=I+1                                 !NEXT CHAR IN MOD LINE
   IF(ICHAR.GT.LMX1)GOTO 999             !IF TOO MANY CHARS IN NEW LINE
   IF(LINSRT) THEN                       !IF INSERTING NEW CHARS
      IF(I.GT.IEND) CMOD(I:I)=C(1:1)     !FORCE END OF INSERT MODE
      IF(CMOD(I:I).EQ.C(1:1))THEN        !IF END OF INSERT MODE
         LINSRT=.FALSE.                  !RESET INSERT MODE FLAG
         IF(IC+1.EQ.I)THEN               !NULL INSERT STRING
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            DUM2(ICHAR:ICHAR)=C(1:1)     !INSERT INSERT MODE TERMINATOR
         ENDIF
         DO J=IC,I                       !LOOP OF NUMBER OF CHARS INSERTED
            ICHAR=ICHAR+1                !INCREMENT COUNTER FOR NEW LINE
            IF(ICHAR.GT.LMAX)GOTO 999    !IF AT BUFFER LIMIT, QUIT
            DUM2(ICHAR:ICHAR)=CLINE(J:J) !APPEND CHARS FROM ORIG LINE
         ENDDO                           !...WHICH ALIGN WITH INSERTED CHARS
         IC=I                            !RESET CHAR COUNT TO END OF INSERT
         GOTO 1                          !CHECK NEW LINE LENGTH AND CYCLE
      ENDIF                              !END OF TERMINATED INSERT LOGIC
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNT
      DUM2(ICHAR:ICHAR)=CMOD(I:I)        !SET NEWLINE CHAR TO INSERTED CHAR
   ELSE                                  !IF NOT INSERTING CHARACTERS
      IC=IC+1                            !INCREMENT ORIGINAL LINE COUNTER
      IF(CMOD(I:I).EQ.C(1:1))GOTO 1      !IF DELETE CHAR. NO COPY AND CYCLE
      IF(CMOD(I:I).EQ.C(3:3))THEN        !IF BEGIN INSERT MODE
         LINSRT=.TRUE.                   !SET INSERT FLAG TRUE
         GOTO 1                          !CHECK LINE LENGTH AND CONTINUE
      ENDIF                              !IF NOT BEGINNING INSERT MODE
      ICHAR=ICHAR+1                      !INCREMENT NEW LINE COUNTER
      IF(CMOD(I:I).EQ.C(2:2))THEN        !IF REPLACE WITH BLANK
         DUM2(ICHAR:ICHAR)=' '           !SET NEWLINE CHAR TO BLANK
         GOTO 1                          !CHECK LINE LENGTH AND CYCLE
      ENDIF                              !IF NOT REPLACE WITH BLANK
      IF(CMOD(I:I).EQ.' ')THEN           !IF BLANK, KEEP ORIGINAL CHARACTER
         DUM2(ICHAR:ICHAR)=CLINE(IC:IC)  !SET NEW CHAR TO ORIGINAL CHAR
      ELSE                               !IF NOT KEEPING OLD CHAR
         DUM2(ICHAR:ICHAR)=CMOD(I:I)     !REPLACE ORIGINAL CHAR WITH NEW
      ENDIF                              !END CHAR KEEP OR REPLACE
   ENDIF                                 !END INSERT OR NO-INSERT
1  CONTINUE
   IF(I.LT.LMAX)GOTO 11                  !CHECK FOR END OF LINE REACHED
                                         !AND CYCLE IF OK
999   CONTINUE
   CLINE=DUM2                            !SET ORIGINAL CHARS TO NEW CHARS
END SUBROUTINE MODIF                     !RETURN
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modif()
character(len=256)           :: line
character(len=:),allocatable :: COMMAND_LINE
   call unit_check_start('modif',' &
      & -description ''change string using a directive similar to XEDIT editor MODIFY command'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   line='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_check('modif',line.eq.'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=line)
   call unit_check('modif',line.eq.'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=command_line)
   call modif(line,COMMAND_LINE)
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_check('modif',line.eq.'%aaaaJ1L2N3 QRupSTUVWXYZ',msg=line)
   call unit_check_done('modif')
end subroutine test_modif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      len_white(3f) - [M_strings:LENGTH] get length of string trimmed of whitespace.
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    integer function len_white(string)
!!
!!     character(len=*) :: string
!!##DESCRIPTION
!!      len_white(3f) returns the position of the last character in
!!      string that is not a whitespace character. The Fortran90 intrinsic
!!      LEN_TRIM() should be used when trailing whitespace can be assumed
!!      to always be spaces.
!!
!!      This procedure was heavily used in the past because ANSI FORTRAN
!!      77 character objects are fixed length and blank padded and the
!!      LEN_TRIM() intrinsic did not exist. It should now be used only when
!!      whitespace characters other than blanks are likely.
!!##OPTIONS
!!      string     input string whose trimmed length is being calculated
!!                 ignoring all trailing whitespace characters.
!!##RETURNS
!!      len_white  the number of characters in the trimmed string
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_len_white
!!
!!      use M_strings, only : len_white
!!      character(len=80) ::  s
!!      intrinsic len
!!
!!      s=' ABCDEFG abcdefg '
!!      ilen = len(s)
!!      lastnb = len_white(s)
!!
!!      write(*,*) 'total length of variable is ',ilen
!!      write(*,*) 'trimmed length of variable is ',lastnb
!!      write(*,*) 'trimmed string=[',s(:lastnb),']'
!!
!!     end program demo_len_white
!!
!!##NOTES
!!
!! o len_white
!!
!!      is a resource-intensive routine. Once the end of
!!      the string is found, it is probably best to keep track of it in
!!      order to avoid repeated calls to len_white. Because they
!!      might be more efficient, consider looking for vendor-supplied or
!!      system-optimized equivalents. For example:
!!
!!         o lnblnk - Solaris f77
!!         o len_trim - FORTRAN 90
!!
!! o
!!      Some compilers seem to have trouble passing a string of variable
!!      length properly. To be safe, use something like this:
!!
!!       subroutine message(s)
!!        character(len=*) :: s ! s is of variable length
!!           ilen=len(s)        ! get total length of variable
!!           ! explicitly specify a substring instead of just variable name
!!           lastnb = len_white(s(:ilen))
!!           write(*,*)'error:[',s(:lastnb),']'
!!       end subroutine messages
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental integer function len_white(string)
!  DEPRECATED. Use len_trim(3f),trim(3f) unless you might have trailing nulls (common when interacting with C procedures)"
!  John S. Urban, 1984, 1997-12-31
!  Note that if the string is blank, a length of 0 is returned; which is not a legal string length in Fortran77.
!  this routine used to return one instead of zero.
!   - mod 1:     1994
!                added null (char(0)) because HP and some Suns not padding
!                strings with blank, but with null characters; 1994 JSU
!   - mod 2:     1999
!                update syntax with INTENT(), ENDDO, no RETURN
!                still need instead of LEN_TRIM() because some systems stil pad CHARACTER with NULL
!-----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_14="@(#)M_strings::len_white(3f): return position of last non-blank/non-null character in string"

character(len=*),intent(in):: string ! input string to determine length of
integer                    :: i10
intrinsic len
   len_white=0
   do i10=len(string),1,-1
      select case(string(i10:i10))
      case(' ')                 ! space(32)
      case(char(0))             ! null(0)
      case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13)
      case default
         len_white=i10
         exit
      end select
   enddo
end function len_white
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_len_white()
   call unit_check_start('len_white',' &
      & -description ''find location of last non-whitespace character'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('len_white',len_white('A b c  '//char(9)//char(10)//char(11)//char(12)//char(13)).eq.5,msg='')
   call unit_check_done('len_white',msg='len_white(3f) tests completed')
end subroutine test_len_white
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    crop(3f) - [M_strings:WHITESPACE] trim leading blanks and trailing blanks from a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function crop(strin) result (strout)
!!
!!     character(len=*),intent(in)  :: strin
!!     character(len=:),allocatable :: strout
!!##DESCRIPTION
!!    trim leading blanks from a string and return position of last
!!    non-blank character in the string.
!!##OPTIONS
!!    strin   input string to trim leading and trailing space from
!!##RETURNS
!!    strout  cropped version of input string
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_crop
!!    use M_strings, only: crop
!!    implicit none
!!    character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
!!       write(*,*) 'untrimmed string=[',untrimmed,']'
!!       write(*,*) 'cropped string=[',crop(untrimmed),']'
!!    end program demo_crop
!!
!!   Expected output
!!
!!      untrimmed string=[   ABCDEFG abcdefg                      ]
!!      cropped string=[ABCDEFG abcdefg]
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function crop(strin) result (strout)
use M_journal, only : journal

character(len=*),parameter::ident_15="@(#)M_strings::crop(3f): trim leading and trailings blanks from string"

character(len=*),intent(in)  :: strin
character(len=:),allocatable :: strout
   strout=trim(adjustl(strin))
end function crop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_crop()
   call unit_check_start('crop',' &
      & -description ''function trims leading and trailing spaces'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('crop',crop('    A B CC D      ').eq.'A B CC D',msg='crop string test 1')
   call unit_check('crop',crop('A B CC D').eq.'A B CC D',msg='crop string test 2')
   call unit_check('crop',crop('A B CC D    ').eq.'A B CC D',msg='crop string test 3')
   call unit_check('crop',crop('     A B CC D    ').eq.'A B CC D',msg='crop string test 4')
   call unit_check_done('crop')
end subroutine test_crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    transliterate(3f) - [M_strings:EDITING] replace characters from old set with new set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function transliterate(instr,old_set,new_set) result(outstr)
!!
!!     character(len=*),intent(in)  :: instr
!!     character(len=*),intent(in)  :: old_set
!!     character(len=*),intent(in)  :: new_set
!!     character(len=len(instr))    :: outstr
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from the input string.
!!
!!##OPTIONS
!!    instr    input string to change
!!    old_set  list of letters to change in INSTR if found
!!
!!             Each character in the input string that matches a character in
!!             the old set is replaced.
!!    new_set  list of letters to replace letters in OLD_SET with.
!!
!!             If the new_set is the empty set the matched characters are deleted.
!!
!!             If the new_set is shorter than the old set the last character in the
!!             new set is used to replace the remaining characters in the new set.
!!##RETURNS
!!    outstr   instr with substitutions applied
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_transliterate
!!
!!     use M_strings, only : transliterate
!!     implicit none
!!     character(len=80)   :: STRING
!!
!!     STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
!!     write(*,'(a)') STRING
!!
!!     ! convert a string to uppercase:
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
!!
!!     ! change all miniscule letters to a colon (":"):
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz',':')
!!
!!     ! delete all miniscule letters
!!     write(*,*) TRANSLITERATE(STRING,'abcdefghijklmnopqrstuvwxyz','')
!!
!!    end program demo_transliterate
!!
!!    Expected output
!!
!!     > aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ
!!     > AABBCCDDEEFFGGHHIIJJKKLLMMNNOOPPQQRRSSTTUUVVWWXXYYZZ
!!     > :A:B:C:D:E:F:G:H:I:J:K:L:M:N:O:P:Q:R:S:T:U:V:W:X:Y:Z
!!     > ABCDEFGHIJKLMNOPQRSTUVWXYZ
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
PURE FUNCTION transliterate(instr,old_set,new_set) RESULT(outstr)

character(len=*),parameter::ident_16="@(#)M_strings::transliterate(3f): replace characters from old set with new set"

!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=*),INTENT(IN)  :: instr                             ! input string to change
CHARACTER(LEN=*),intent(in)  :: old_set
CHARACTER(LEN=*),intent(in)  :: new_set
!-----------------------------------------------------------------------------------------------------------------------------------
CHARACTER(LEN=LEN(instr))    :: outstr                            ! output string to generate
!-----------------------------------------------------------------------------------------------------------------------------------
INTEGER                      :: i10                               ! loop counter for stepping thru string
INTEGER                      :: ii,jj
!-----------------------------------------------------------------------------------------------------------------------------------
   jj=LEN(new_set)
   IF(jj.NE.0)THEN
      outstr=instr                                                ! initially assume output string equals input string
      stepthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.NE.0)THEN
            if(ii.le.jj)then                                      ! use corresponding character in new_set
               outstr(i10:i10) = new_set(ii:ii)
            else
               outstr(i10:i10) = new_set(jj:jj)                   ! new_set not as long as old_set; use last character in new_set
            endif
         ENDIF
      ENDDO stepthru
   else                                                           ! new_set is null string so delete characters in old_set
      outstr=' '
      hopthru: DO i10 = 1, LEN(instr)
         ii=iNDEX(old_set,instr(i10:i10))                         ! see if current character is in old_set
         IF (ii.EQ.0)THEN                                         ! only keep characters not in old_set
            jj=jj+1
            outstr(jj:jj) = instr(i10:i10)
         ENDIF
      ENDDO hopthru
   endif
END FUNCTION transliterate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_transliterate
!!use M_strings, only: transliterate
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
   call unit_check_start('transliterate',' &
      & -description ''when characters in set one are found replace them with characters from set two'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
call unit_check('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc).eq.uc(1:26),msg='transliterate to uppercase')
call unit_check('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',uc,lc).eq.lc(1:26),msg='transliterate to lowercase')
call unit_check_done('transliterate')
end subroutine test_transliterate
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    rotate13(3f) - [M_strings] apply trivial ROT13 encryption to a string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    rotate13(input) result(output)
!!
!!     character(len=*),intent(in) :: input
!!     character(len=len(input))   :: output
!!
!!##DESCRIPTION
!!    ROT13 ("rotate by 13 places", sometimes hyphenated ROT-13) is a simple
!!    letter substitution cipher that replaces a letter with the 13th letter
!!    after it in the alphabet; wrapping around if necessary.
!!
!!    The transformation can be done using a lookup table, such as the
!!    following:
!!
!!       Input  ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
!!       Output NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm
!!
!!    ROT13 is used in online forums as a means of hiding spoilers, punchlines,
!!    puzzle solutions, and offensive materials from the casual glance. ROT13
!!    has inspired a variety of letter and word games on-line, and is frequently
!!    mentioned in newsgroup conversations.
!!
!!    The algorithm provides virtually no cryptographic security, and is
!!    often cited as a canonical example of weak encryption.
!!
!!    ROT13 is a special case of the Caesar cipher which was developed in
!!    ancient Rome.
!!
!!    ALGORITHM
!!
!!    Applying ROT13 to a piece of text merely requires examining its alphabetic
!!    characters and replacing each one by the letter 13 places further along in
!!    the alphabet, wrapping back to the beginning if necessary. A becomes
!!    N, B becomes O, and so on up to M, which becomes Z, then the sequence
!!    continues at the beginning of the alphabet: N becomes A, O becomes B,
!!    and so on to Z, which becomes M. Only those letters which occur in the
!!    English alphabet are affected; numbers, symbols, whitespace, and all other
!!    characters are left unchanged.
!!
!!    SAME ALGORITHM FOR ENCODING AND DECODING
!!
!!    Because there are 26 letters in the English alphabet and 26 = 2 x 13,
!!    the ROT13 function is its own inverse: so the same action can be used
!!    for encoding and decoding. In other words, two successive applications
!!    of ROT13 restore the original text (in mathematics, this is sometimes
!!    called an involution; in cryptography, a reciprocal cipher).
!!
!!    TRIVIAL SECURITY
!!
!!    the use of a constant shift means that the encryption effectively
!!    has no key, and decryption requires no more knowledge than the fact
!!    that ROT13 is in use. Even without this knowledge, the algorithm is
!!    easily broken through frequency analysis.
!!
!!    In encrypted normal English-language text of any significant size,
!!    ROT13 is recognizable from some letter/word patterns. The words "n",
!!    "V" (capitalized only), and "gur" (ROT13 for "a", "I", and "the"),
!!    and words ending in "yl" ("ly") are examples.
!!
!!##REFERENCES
!!    Wikipedia, the free encyclopedia
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rotate13
!!    use M_strings, only : rotate13
!!    implicit none
!!    character(len=256) :: line
!!    integer            :: ios
!!    do
!!       read(*,'(a)',iostat=ios)line
!!       if(ios.ne.0)exit
!!       write(*,'(a)')rotate13(line)
!!    enddo
!!    end program demo_rotate13
!!
!!  Sample usage:
!!    demo_rotate13
!!    United we stand, divided we fall.
!!    Havgrq jr fgnaq, qvivqrq jr snyy.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function rotate13 (input)
implicit none

character(len=*),parameter::ident_17="&
&@(#)M_strings::rotate13(3f): converts a character to its ROT13 equivalent, which is a trivial encryption."

character(len=*),intent(in) :: input
character(len=len(input))   :: rotate13
integer                     :: itemp
integer                     :: i
   rotate13=' '
   do i=1,len_trim(input)
      itemp = ichar (input(i:i))
      select case(itemp)
       case(65:77,97:109)
         itemp = itemp + 13
       case(78:90,110:122)
         itemp = itemp - 13
      end select
      rotate13(i:i) = char ( itemp )
   enddo

end function rotate13
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rotate13()
character(len=:),allocatable  :: s
character(len=:),allocatable  :: e
   call unit_check_start('rotate13',' &
      & -description ''apply trivial encryption algorithm ROT13 to a string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   s='United we stand, divided we fall.'
   e='Havgrq jr fgnaq, qvivqrq jr snyy.'
   call unit_check('rotate13',rotate13(s).eq.e,msg=msg(s,'==>',rotate13(s)))
   call unit_check_done('rotate13',msg='')
end subroutine test_rotate13
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=:),allocatable         :: string
!!##DESCRIPTION
!!      JOIN(3f) appends the elements of a CHARACTER array into a single CHARACTER variable,
!!      with elements 1 to N joined from left to right.
!!      By default each element is trimmed of trailing spaces and the default separator is
!!      a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      TRM     option to trim each element of STR of trailing spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR() appended together
!!              with the optional separator SEP placed between the elements.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_join
!!       use M_strings, only: join
!!       implicit none
!!       character(len=:),allocatable  :: s(:)
!!       character(len=:),allocatable  :: out
!!       integer                       :: i
!!          s=[character(len=10) :: 'United',' we',' stand,',' divided',' we fall.']
!!          out=join(s)
!!          write(*,'(a)') out
!!          write(*,'(a)') join(s,trm=.false.)
!!          write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!          write(*,'(a)') join(s,sep='<>')
!!          write(*,'(a)') join(s,sep=';',left='[',right=']')
!!          write(*,'(a)') join(s,left='[',right=']')
!!          write(*,'(a)') join(s,left='>>')
!!       end program demo_join
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function join(str,sep,trm,left,right) result (string)

character(len=*),parameter::ident_18="&
&@(#)M_strings::join(3f): append an array of character variables with specified separator into a single CHARACTER variable"

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep
character(len=*),intent(in),optional :: right
character(len=*),intent(in),optional :: left
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: string
integer                              :: i
logical                              :: trm_local
character(len=:),allocatable         :: sep_local
character(len=:),allocatable         :: left_local
character(len=:),allocatable         :: right_local

   if(present(sep))then    ;  sep_local=sep      ;  else  ;  sep_local=''      ;  endif
   if(present(trm))then    ;  trm_local=trm      ;  else  ;  trm_local=.true.  ;  endif
   if(present(left))then   ;  left_local=left    ;  else  ;  left_local=''     ;  endif
   if(present(right))then  ;  right_local=right  ;  else  ;  right_local=''    ;  endif

   string=''
   do i = 1,size(str)
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local//sep_local
      else
         string=string//left_local//str(i)//right_local//sep_local
      endif
   enddo
end function join
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_join()
character(len=:),allocatable  :: s(:)
   call unit_check_start('join',' &
      & -description ''append an array of character variables with specified separator into a single CHARACTER variable'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   s=[character(len=10) :: 'United',' we',' stand,',' divided',' we fall.']

   call testit( join(s),                            'United we stand, divided we fall.')
   call testit( join(s,trm=.false.),                'United     we        stand,    divided   we fall.')
   call testit( join(s,trm=.false.,sep='|'),        'United    | we       | stand,   | divided  | we fall. |')
   call testit( join(s,sep='<>'),                   'United<> we<> stand,<> divided<> we fall.<>')
   call testit( join(s,sep=';',left='[',right=']'), '[United];[ we];[ stand,];[ divided];[ we fall.];')
   call testit( join(s,left='[',right=']'),         '[United][ we][ stand,][ divided][ we fall.]')
   call testit( join(s,left='>>'),                  '>>United>> we>> stand,>> divided>> we fall.')
   call unit_check_done('join',msg='join array of strings into a single string controlling separators and white space')
contains
subroutine testit(generated,expected)
character(len=*),intent(in) :: generated
character(len=*),intent(in) :: expected
   if(unit_check_level.gt.0)then
      write(*,*)'JOIN(3F) TEST'
      write(*,*)'INPUT       ','['//s//']'
      write(*,*)'GENERATED   ',generated
      write(*,*)'EXPECTED    ',expected
   endif
   call unit_check('join',generated.eq.expected,msg='output is '//generated)
end subroutine testit
end subroutine test_join
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      reverse(3f) - [M_strings:EDITING] Return a string reversed
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function reverse(str) result (string)
!!
!!     character(*), intent(in) :: str
!!     character(len(str))      :: string
!!##DESCRIPTION
!!      reverse(string) returns a copy of the input string with
!!      all characters reversed from right to left.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_reverse
!!       use M_strings, only: reverse
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
!!          s='abcdefghijklmnopqrstuvwxyz'
!!          write(*,*) 'original input string is ....',s
!!          write(*,*) 'reversed output string is ...',reverse(s)
!!       end program demo_reverse
!!
!!    Expected output
!!
!!      original input string is ....abcdefghijklmnopqrstuvwxyz
!!      reversed output string is ...zyxwvutsrqponmlkjihgfedcba
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function reverse(string ) result (rev)

character(len=*),parameter::ident_19="@(#)M_strings::reverse(3f): Return a string reversed"

character(len=*),intent(in)    :: string   ! string to reverse
character(len=len(string))     :: rev      ! return value (reversed string)
integer                        :: length
integer                        :: i
   length = len(string)
   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!! test reverse(3f)
subroutine test_reverse
!!use M_strings, only: reverse
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('reverse',' &
      & -description ''elemental function reverses character order in a string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
if(reverse(lc).eq.'9876543210zyxwvutsrqponmlkjihgfedcba')then
   call unit_check_good('reverse')
else
   if(unit_check_level.gt.0)then
      write(*,*)'error: reverse '
      write(*,*)'iN:  ['//lc//']'
      write(*,*)'OUT: ['//reverse(lc)//']'
   endif
   call unit_check_bad('reverse')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper_quoted(3f) - [M_strings:CASE] elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper_quoted(str) result (string)
!!
!!     character(*), intent(in)    :: str
!!     character(len(str))         :: string  ! output string
!!##DESCRIPTION
!!    upper_quoted(string) returns a copy of the input string with all not-quoted
!!    characters converted to uppercase, assuming ASCII character sets
!!    are being used. The quoting rules are the same as for Fortran source.
!!    Either a single or double quote starts a quoted string, and a quote
!!    character of the same type is doubled when it appears internally in
!!    the quoted string. If a double quote quotes the string single quotes
!!    may appear in the quoted string as single characters, and vice-versa
!!    for single quotes.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all unquoted characters converted
!!           to uppercase
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper_quoted
!!     use M_strings, only: upper_quoted
!!     implicit none
!!     character(len=:),allocatable  :: s
!!     s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with "" Quote" everything else'
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper_quoted(s)
!!        write(*,*) 'make first character uppercase  ... ',upper_quoted('this is a sentence.')
!!        write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>',upper_quoted(["abc","def","ghi"])
!!     end program demo_upper_quoted
!!
!!    Expected output:
!!
!!     mixed-case input string is .... ABCDEFG abcdefg "Double-Quoted" 'Single-Quoted' "with "" Quote" everything else
!!     upper-case output string is ... ABCDEFG ABCDEFG "Double-Quoted" 'Single-Quoted' "with "" Quote" EVERYTHING ELSE
!!     make first character uppercase  ... THIS IS A SENTENCE.
!!     upper_quoted(3f) is elemental ==>ABC+DEF+GHI
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental pure function upper_quoted(str) result (string)

character(len=*),parameter::ident_20="&
&@(#)M_strings::upper_quoted(3f): elemental function converts string to miniscule skipping strings quoted per Fortran syntax rules"

character(len=*), intent(in)   :: str     ! The input string
character(len=len(str))        :: string  ! The output string
logical                        :: toggle
character(len=1)               :: togglechar
integer                        :: irnk
integer                        :: i
character(len=26), parameter   :: large="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
character(len=26), parameter   :: small="abcdefghijklmnopqrstuvwxyz"

   string=str
   toggle = .TRUE.
   do i = 1, len_trim(string)
      if(toggle) then
         if(string(i:i) == '"' .or. string(i:i) == "'") then
            toggle = .not. toggle
            togglechar = string(i:i)
         endif
         irnk = index(small, string(i:i))
         if(irnk > 0) then
            string(i:i) = large(irnk:irnk)
         endif
      else
         if(string(i:i) == togglechar) toggle = .not. toggle
      endif
   enddo
end function upper_quoted
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! upper(3f) - [M_strings:CASE] changes a string to uppercase
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function upper(str,begin,end) result (string)
!!
!!     character(*), intent(in)    :: str
!!     integer,optional,intent(in) :: begin,end
!!     character(len(str))         :: string  ! output string
!!##DESCRIPTION
!!      upper(string) returns a copy of the input string with all characters
!!      converted in the optionally specified range to uppercase, assuming
!!      ASCII character sets are being used. If no range is specified the
!!      entire string is converted to uppercase.
!!
!!##OPTIONS
!!    str    string to convert to uppercase
!!    begin  optional starting position in "str" to begin converting to uppercase
!!    end    optional ending position in "str" to stop converting to uppercase
!!
!!##RESULTS
!!    upper  copy of the input string with all characters converted to uppercase
!!           over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_upper
!!     use M_strings, only: upper
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s=' ABCDEFG abcdefg '
!!        write(*,*) 'mixed-case input string is ....',s
!!        write(*,*) 'upper-case output string is ...',upper(s)
!!        write(*,*) 'make first character uppercase  ... ',upper('this is a sentence.',1,1)
!!        write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',upper(["abc","def","ghi"])
!!     end program demo_upper
!!
!!    Expected output
!!
!!     mixed-case input string is .... ABCDEFG abcdefg
!!     upper-case output string is ... ABCDEFG ABCDEFG
!!     make first character uppercase  ... This is a sentence.
!!     UPPER(3f) is elemental ==>ABC+DEF+GHI
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
! Timing
!
!    Several different methods have been proposed for changing case.
!    A simple program that copies a large file and converts it to
!    uppercase was timed and compared to a simple copy. This was used
!    to select the default function.
!
! NULL:    83.41user  9.25system 1:37.94elapsed 94%CPU
! upper:  101.44user 10.89system 1:58.36elapsed 94%CPU
! upper2: 105.04user 10.69system 2:04.17elapsed 93%CPU
! upper3: 267.21user 11.69system 4:49.21elapsed 96%CPU
elemental pure function upper(str,begin,end) result (string)

character(len=*),parameter::ident_21="@(#)M_strings::upper(3f): Changes a string to uppercase"

character(*), intent(In)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
   string = str                                      ! initialize output string to input string

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))-32)    ! change miniscule letter to uppercase
       end select
   end do

end function upper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_upper
!!use M_strings, only: upper
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('upper',' &
      & -description ''elemental function converts string to uppercase'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check_start('upper')
   call unit_check('upper',upper(lc).eq.uc)
   call unit_check_done('upper')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lower(3f) - [M_strings:CASE] changes a string to lowercase over specified range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental pure function lower(str,begin,end) result (string)
!!
!!     character(*), intent(in) :: str
!!     integer,optional         :: begin, end
!!     character(len(str))      :: string  ! output string
!!##DESCRIPTION
!!      lower(string) returns a copy of the input string with all characters
!!      converted to miniscule over the specified range, assuming ASCII
!!      character sets are being used. If no range is specified the entire
!!      string is converted to miniscule.
!!
!!##OPTIONS
!!    str    string to convert to miniscule
!!    begin  optional starting position in "str" to begin converting to miniscule
!!    end    optional ending position in "str" to stop converting to miniscule
!!
!!##RESULTS
!!    lower  copy of the input string with all characters converted to miniscule
!!           over optionally specified range.
!!
!!##TRIVIA
!!    The terms "uppercase" and "lowercase" date back to the early days of
!!    the mechanical printing press. Individual metal alloy casts of each
!!    needed letter, or punctuation symbol, were meticulously added to a
!!    press block, by hand, before rolling out copies of a page. These
!!    metal casts were stored and organized in wooden cases. The more
!!    often needed miniscule letters were placed closer to hand, in the
!!    lower cases of the work bench. The less often needed, capitalized,
!!    majuscule letters, ended up in the harder to reach upper cases.
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_lower
!!       use M_strings, only: lower
!!       implicit none
!!       character(len=:),allocatable  :: s
!!          s=' ABCDEFG abcdefg '
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',lower(s)
!!       end program demo_lower
!!
!!    Expected output
!!
!!       mixed-case input string is .... ABCDEFG abcdefg
!!       lower-case output string is ... abcdefg abcdefg
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental pure function lower(str,begin,end) result (string)

character(len=*),parameter::ident_22="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
   string = str

   ibegin = 1
   if (present(begin))then
      ibegin = max(ibegin,begin)
   endif

   iend = len_trim(str)
   if (present(end))then
      iend= min(iend,end)
   endif

   do i = ibegin, iend                               ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
      case default
      end select
   end do

end function lower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lower
!!use M_strings, only: lower
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('lower',' &
      & -description ''elemental function converts string to miniscule'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check_start('lower')
   call unit_check('lower',lower(uc).eq.lc,'lower')
   call unit_check_done('lower')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    switch(3f) - [M_strings:ARRAY] converts between CHARACTER scalar and array of single characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    pure function switch(array) result (string)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!
!!      or
!!
!!    pure function switch(string) result (array)
!!
!!     character(len=1),intent(in) :: array(:)
!!     character(len=SIZE(array))  :: string
!!##DESCRIPTION
!!
!!    SWITCH(3f): generic function that switches CHARACTER string to an array
!!    of single characters or an array of single characters to a CHARACTER
!!    string. Useful in passing strings to C. New Fortran features may
!!    supersede these routines.
!!
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!    program demo_switch
!!    use M_strings, only : switch, isalpha, islower, nospace
!!    character(len=*),parameter :: dashes='-----------------------------------'
!!    character(len=*),parameter :: string='This is a string of letters'
!!    character(len=1024)        :: line
!!
!!    ! First, examples of standard Fortran features
!!    write(*,*)['A','=','=','=','=','='].eq.'='      ! returns array [F,T,T,T,T,T]
!!    write(*,*)all(['=','=','=','=','=','='].eq.'=') ! this would return T
!!    write(*,*)all(['A','=','=','=','=','='].eq.'=') ! this would return F
!!
!!    ! so to test if the string DASHES is all dashes using SWITCH(3f) is
!!    if(all(switch(dashes).eq.'-'))then
!!       write(*,*)'DASHES is all dashes'
!!    endif
!!
!!    ! so to test is a string is all letters
!!    ! isalpha(3f) returns .true. only if character is a letter
!!    write(*,*) all(isalpha(switch(dashes)))  ! false because dashes are not a letter
!!    write(*,*) all(isalpha(switch(string)))  ! false because of spaces
!!    write(*,*) all(isalpha(switch(nospace(string))))  ! true because removed whitespace
!!
!!    ! to see if a string is all uppercase
!!    write(*,*) string                           ! show the string
!!    write(*,'(1x,*("[",a,"]":))') switch(string)   ! converted to character array
!!    write(*,'(*(l3))') islower(switch(string))
!!
!!    line=nospace(string)                        ! we need a string that is all letters
!!    write(*,*)'LINE=',trim(line)
!!    write(*,*) islower(switch(nospace(string))) ! all true except first character
!!    write(*,*) all(islower(switch(nospace(string))))      ! should be false
!!    write(*,*) all(islower(switch(nospace(string(2:)))))  ! should be true
!!
!!    end program demo_switch
!!
!!  Expected output
!!
!!    >  F T T T T T
!!    >  T
!!    >  F
!!    >  DASHES is all dashes
!!    >  F
!!    >  F
!!    >  T
!!    >  This is a string of letters
!!    >  [T][h][i][s][ ][i][s][ ][a][ ][s][t][r][i][n][g][ ][o][f][ ][l][e][t][t][e][r][s]
!!    >   F  T  T  T  F  T  T  F  T  F  T  T  T  T  T  T  F  T  T  F  T  T  T  T  T  T  T
!!    >  LINE=Thisisastringofletters
!!    >  F T T T T T T T T T T T T T T T T T T T T T
!!    >  F
!!    >  T
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function a2s(array)  result (string)

character(len=*),parameter::ident_23="@(#)M_strings::a2s(3fp): function to copy char array to string"

character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall( i = 1:size(array)) string(i:i) = array(i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function a2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function s2a(string)  RESULT (array)

character(len=*),parameter::ident_24="@(#)M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array"

character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
! ----------------------------------------------------------------------------------------------------------------------------------
   forall(i=1:len(string)) array(i) = string(i:i)
! ----------------------------------------------------------------------------------------------------------------------------------
end function s2a
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_switch
!!use M_switch, only: reverse
!!use M_switch, only: switch
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=1)            :: chars(36)
integer :: i
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level.gt.0)then
   write(*,*)'switch:' ! switch: switch between single string and an array of single characters; generic name for {a2s,s2a}
   write(*,*)'switch LC string to an array'
   write(*,'(i0,1x,*(a,1x))') size(switch(lc)),switch(lc)
   write(*,*)'switch UC string to an array'
   write(*,'(i0,1x,*(a,1x))') size(switch(uc)),switch(uc)
endif
   call unit_check_start('switch',' &
      & -description ''generic switch between a string and an array of single characters (a2s,s2a)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_switch.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
if(size(switch(uc)).ne.36)then
   call unit_check_bad('switch')
endif
chars=switch(uc)
do i=1,size(chars)
   if(chars(i).ne.uc(i:i))then
      call unit_check_bad('switch')
   endif
enddo

if(unit_check_level.gt.0)then
   write(*,*)'put string UC into array CHARS'
endif
chars=switch(uc)
if(unit_check_level.gt.0)then
   write(*,*)'put CHARS array into CHARS array in reverse order like reverse'
endif
chars=chars(36:1:-1)
if(unit_check_level.gt.0)then
   write(*,*)'put CHARS array into string reversed and compare to original UC string'
endif
if( uc .ne. switch(chars(36:1:-1)) )then
   if(unit_check_level.gt.0)then
      write(*,*)switch(chars(36:1:-1))
   endif
   call unit_check_bad('switch')
endif
call unit_check_good('switch')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_switch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2c(3f) - [M_strings:ARRAY] convert character variable to array of characters with last element set to null
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2c(string)
!!
!!     character(len=*),intent=(in)  :: string
!!     character(len=1),allocatable  :: s2c(:)
!!##DESCRIPTION
!!    Given a character variable convert it to an array of single-character
!!    character variables with the last element set to a null character.
!!    This is generally used to pass character variables to C procedures.
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_s2c
!!     use M_strings, only : s2c
!!     implicit none
!!     character(len=*),parameter   :: string="single string"
!!     character(len=3),allocatable :: array(:)
!!        write(*,*)'INPUT STRING ',trim(string)
!!        ! put one character into each 3-character element of array
!!        array=s2c(string)
!!        ! write array with ASCII Decimal Equivalent below it except show
!!        ! unprintable characters like NULL as "XXX"
!!        write(*,'(1x,*("[",a3,"]":))')&
!!             & merge('XXX',array,ichar(array(:)(1:1)).lt.32)
!!        write(*,'(1x,*("[",i3,"]":))')&
!!             & ichar(array(:)(1:1))
!!     end program demo_s2c
!!
!!   Expected output:
!!
!!    INPUT STRING single string
!!    [s  ][i  ][n  ][g  ][l  ][e  ][   ][s  ][t  ][r  ][i  ][n  ][g  ][XXX]
!!    [115][105][110][103][108][101][ 32][115][116][114][105][110][103][  0]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function s2c(string)  RESULT (array)
use,intrinsic :: ISO_C_BINDING, only : C_CHAR

character(len=*),parameter::ident_25="@(#)M_strings::s2c(3f): copy string(1:Clen(string)) to char array with null terminator"

character(len=*),intent(in)     :: string

! This is changing, but currently the most portable way to pass a CHARACTER variable to C is to convert it to an array of
! character variables with length one and add a null character to the end of the array. The s2c(3f) function helps do this.
character(kind=C_CHAR,len=1)    :: array(len_trim(string)+1)
integer                         :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2c()
   call unit_check_start('s2c',' &
      & -description ''convert character variable to array of character(len=1) with null terminator for C compatibility'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check_done('s2c',msg='UNTESTED')
end subroutine test_s2c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      c2s(3f) - [M_strings:ARRAY] convert C string pointer to Fortran character string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function c2s(c_string_pointer) result(f_string)
!!
!!     type(c_ptr), intent(in)       :: c_string_pointer
!!     character(len=:), allocatable :: f_string
!!##DESCRIPTION
!!    Given a C pointer to a character string return a Fortran character string.
!!##OPTIONS
!!    c_string_pointer  C pointer to convert
!!##RETURNS
!!    f_string          Fortran character variable to return
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function c2s(c_string_pointer) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns "NULL", similar to C's "(null)" printed in similar cases:
use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char

character(len=*),parameter::ident_26="&
&@(#)M_strings::c2s(3f): copy pointer to C char array till a null is encountered to a Fortran string up to 4096 characters"

integer,parameter                             :: max_length=4096
type(c_ptr), intent(in)                       :: c_string_pointer
character(len=:), allocatable                 :: f_string
character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
character(len=max_length)                            :: aux_string
integer                                       :: i,length=0

   call c_f_pointer(c_string_pointer,char_array_pointer,[max_length])
   if (.not.associated(char_array_pointer)) then
     allocate(character(len=4)::f_string)
     f_string="NULL"
     return
   endif
   aux_string=" "
   do i=1,max_length
     if (char_array_pointer(i)==c_null_char) then
       length=i-1
       exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)

end function c2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2s()
   call unit_check_start('c2s',' &
      & -description ''convert null-terminated array of character(len=1) to string for strings returned by C '' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check_done('c2s',msg='UNTESTED')
end subroutine test_c2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      indent(3f) - [M_strings:WHITESPACE] count number of leading spaces in a string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function indent(line)
!!
!!     integer                        :: indent
!!     character(len=*),intent(in)    :: line
!!##DESCRIPTION
!!    Count number of leading spaces in a CHARACTER variable.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_indent
!!     !  test filter to count leading spaces in a character variable
!!     !  might want to call notabs(3f) to expand tab characters
!!     use M_strings, only : indent
!!     implicit none
!!     character(len=1024) :: in
!!     integer             :: ios
!!        READFILE: do
!!           read(*,'(A)',iostat=ios)in
!!           if(ios /= 0) exit READFILE
!!           write(*,'(i3,"",a)')indent(in),trim(in)
!!        enddo READFILE
!!     end program demo_indent
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function indent(line)
implicit none

character(len=*),parameter::ident_27="@(#)M_strings::indent(3f): find number of leading spaces in a string"

integer                        :: indent
character(len=*),intent(in)    :: line
integer                        :: i
   indent=0
   NOTSPACE: block
      SCAN: do i=1,len(line)
         if(line(i:i).ne.' ')then
            indent=i-1
            exit NOTSPACE
         endif
      enddo SCAN
      indent=len(line)
   endblock NOTSPACE
end function indent
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_indent()
character(len=1024) :: in
   call unit_check_start('indent',' &
      & -description ''count number of leading spaces'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   in='    should be four'
   call unit_check('indent',indent(in).eq.4,msg=trim(in))

   in='should be zero'
   call unit_check('indent',indent(in).eq.0,msg=trim(in))

   in='   should be three'
   call unit_check('indent',indent(trim(in)).eq.3,msg=trim(in))

   call unit_check_done('indent')
end subroutine test_indent
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    visible(3f) - [M_strings:NONALPHA] expand a string to control and meta-control representations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function visible(input) result(output)
!!
!!     character(len=*),intent(in)           :: input
!!     character(len=:),allocatable          :: output
!!##DESCRIPTION
!!
!!     visible(3f) expands characters to commonly used sequences used to represent the characters
!!     as control sequences or meta-control sequences.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_visible
!!     use M_strings, only : visible
!!     integer :: i
!!        do i=0,255
!!           write(*,'(i0,1x,a)')i,visible(char(i))
!!        enddo
!!     end program demo_visible
!!##BUGS
!!     The expansion is not reversible, as input sequences such as "M-" or "^a"
!!     will look like expanded sequences.
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function visible(input) result(output)
character(len=*),intent(in)  :: input
character(len=:),allocatable :: output

character(len=*),parameter::ident_28="&
&@(#)M_strings::visible(3f) expand escape sequences in a string to control and meta-control representations"

integer                      :: i
character(len=1)             :: c

character(len=*),parameter :: chars(0:255)= [ &
'^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
'^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
'^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
'^^  ', '^_  ', '    ', '!   ', '"   ', '#   ', '$   ', '%   ', '&   ', '''   ', &
'(   ', ')   ', '*   ', '+   ', ',   ', '-   ', '.   ', '/   ', '0   ', '1   ', &
'2   ', '3   ', '4   ', '5   ', '6   ', '7   ', '8   ', '9   ', ':   ', ';   ', &
'<   ', '=   ', '>   ', '?   ', '@   ', 'A   ', 'B   ', 'C   ', 'D   ', 'E   ', &
'F   ', 'G   ', 'H   ', 'I   ', 'J   ', 'K   ', 'L   ', 'M   ', 'N   ', 'O   ', &
'P   ', 'Q   ', 'R   ', 'S   ', 'T   ', 'U   ', 'V   ', 'W   ', 'X   ', 'Y   ', &
'Z   ', '[   ', '\   ', ']   ', '^   ', '_   ', '`   ', 'a   ', 'b   ', 'c   ', &
'd   ', 'e   ', 'f   ', 'g   ', 'h   ', 'i   ', 'j   ', 'k   ', 'l   ', 'm   ', &
'n   ', 'o   ', 'p   ', 'q   ', 'r   ', 's   ', 't   ', 'u   ', 'v   ', 'w   ', &
'x   ', 'y   ', 'z   ', '{   ', '|   ', '}   ', '~   ', '^?  ', 'M-^@', 'M-^A', &
'M-^B', 'M-^C', 'M-^D', 'M-^E', 'M-^F', 'M-^G', 'M-^H', 'M-^I', 'M-^J', 'M-^K', &
'M-^L', 'M-^M', 'M-^N', 'M-^O', 'M-^P', 'M-^Q', 'M-^R', 'M-^S', 'M-^T', 'M-^U', &
'M-^V', 'M-^W', 'M-^X', 'M-^Y', 'M-^Z', 'M-^[', 'M-^\', 'M-^]', 'M-^^', 'M-^_', &
'M-  ', 'M-! ', 'M-" ', 'M-# ', 'M-$ ', 'M-% ', 'M-& ', 'M-'' ', 'M-( ', 'M-) ', &
'M-* ', 'M-+ ', 'M-, ', 'M-- ', 'M-. ', 'M-/ ', 'M-0 ', 'M-1 ', 'M-2 ', 'M-3 ', &
'M-4 ', 'M-5 ', 'M-6 ', 'M-7 ', 'M-8 ', 'M-9 ', 'M-: ', 'M-; ', 'M-< ', 'M-= ', &
'M-> ', 'M-? ', 'M-@ ', 'M-A ', 'M-B ', 'M-C ', 'M-D ', 'M-E ', 'M-F ', 'M-G ', &
'M-H ', 'M-I ', 'M-J ', 'M-K ', 'M-L ', 'M-M ', 'M-N ', 'M-O ', 'M-P ', 'M-Q ', &
'M-R ', 'M-S ', 'M-T ', 'M-U ', 'M-V ', 'M-W ', 'M-X ', 'M-Y ', 'M-Z ', 'M-[ ', &
'M-\ ', 'M-] ', 'M-^ ', 'M-_ ', 'M-` ', 'M-a ', 'M-b ', 'M-c ', 'M-d ', 'M-e ', &
'M-f ', 'M-g ', 'M-h ', 'M-i ', 'M-j ', 'M-k ', 'M-l ', 'M-m ', 'M-n ', 'M-o ', &
'M-p ', 'M-q ', 'M-r ', 'M-s ', 'M-t ', 'M-u ', 'M-v ', 'M-w ', 'M-x ', 'M-y ', &
'M-z ', 'M-{ ', 'M-| ', 'M-} ', 'M-~ ', 'M-^?']
output=''
do i=1,len(input)
   c=input(i:i)
   if(c.eq.' ')then
      output=output//' '
   else
      output=output//trim(chars(ichar(c)))
   endif
enddo
end function visible
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_visible()
integer :: i
character(len=2) :: controls(0:31)
   call unit_check_start('visible',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=32,126
      call unit_check('visible',visible(char(i)).eq.char(i))
   enddo
   controls=['^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
             '^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
             '^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
             '^^  ', '^_  ']
   do i=0,31
      call unit_check('visible',visible(char(i)).eq.controls(i))
   enddo
   call unit_check('visible',visible(char(127)).eq.'^?')
   if(unit_check_level.gt.0)then
      do i=0,255
         write(*,'(i0,1x,a)')i,visible(char(i))
      enddo
   endif
   do i=32,126
      call unit_check('visible',char(i).eq.visible(char(i)))
   enddo
   call unit_check_done('visible')
end subroutine test_visible
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    expand(3f) - [M_strings:NONALPHA] expand C-like escape sequences
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function expand(line,escape) result(lineout)
!!
!!    character(len=*)                      :: line
!!    character(len=1),intent(in),optional  :: escape
!!    character(len=:),allocatable          :: lineout
!!##DESCRIPTION
!!
!!     EXPAND() expands sequences used to represent commonly used escape sequences
!!     or control characters. By default ...
!!
!!     Escape sequences
!!       \\     backslash
!!       \a     alert (BEL) -- g is an alias for a
!!       \b     backspace
!!       \c     suppress further output
!!       \e     escape
!!       \f     form feed
!!       \n     new line
!!       \r     carriage return
!!       \t     horizontal tab
!!       \v     vertical tab
!!       \oNNN  byte with octal value NNN (3 digits)
!!       \dNNN  byte with decimal value NNN (3 digits)
!!       \xHH   byte with hexadecimal value HH (2 digits) -- h is an alias for x
!!
!!     The default escape character is the backslash, but this may be changed using
!!     the optional parameter ESCAPE.
!!
!!##EXAMPLES
!!
!!    Sample Program:
!!
!!     program demo_expand
!!     !  test filter to expand escape sequences in input lines
!!     use M_strings, only : expand
!!     character(len=1024) :: line
!!     integer             :: ios
!!        READFILE: block
!!           do
!!              read(*,'(A)',iostat=ios)line
!!              if(ios /= 0) exit READFILE
!!              write(*,'(a)')trim(expand(line))
!!           enddo
!!        endblock READFILE
!!     end program demo_expand
!!
!!    Sample input:
!!
!!      \e[2J
!!      \tABC\tabc
!!      \tA\a
!!      \nONE\nTWO\nTHREE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function expand(line,escape) result(lineout)
USE ISO_C_BINDING ,ONLY: c_horizontal_tab
implicit none

character(len=*),parameter::ident_29="@(#)M_strings::expand(3f): return string with escape sequences expanded"

character(len=*)                      :: line
character(len=1),intent(in),optional  :: escape ! escape character. Default is backslash
! expand escape sequences found in input string
! Escape sequences
!    %%     escape character           %a     alert (BEL) -- gi is an alias for a
!    %b     backspace                  %c     suppress further output
!    %e     escape                     %E     escape
!    %f     form feed                  %n     new line
!    %r     carriage return            %t     horizontal tab
!    %v     vertical tab
!    %oNNN  byte with octal value NNN (3 digits)
!    %dNNN  byte with decimal value NNN (3 digits)
!    %xHH   byte with hexadecimal value HH (2 digits) -- h is an alias for x
character(len=1)                      :: esc    ! escape character. Default is %
character(len=:),allocatable          :: lineout
integer                               :: i
integer                               :: ilen
character(len=3)                      :: thr
integer                               :: xxx
integer                               :: ios
   i=0 ! pointer into input

   ilen=len_trim(line)
   lineout=''

   if(ilen.eq.0)return

   if (present(escape))then
      esc=escape
   else
      esc=char(92)
   endif

   EXP: do
      i=i+1
      if(i.gt.ilen)exit
      if(line(i:i).eq.esc)then
         i=i+1
         if(i.gt.ilen)exit
         if(line(i:i).ne.esc)then
            BACKSLASH: select case(line(i:i))
            case('a','A','g','G');lineout=lineout//char(  7) ! %a     alert (BEL)
            case('b','B');lineout=lineout//char(  8)         ! %b     backspace
            case('c','C');exit EXP                           ! %c     suppress further output
            case('d','D')                                    ! %d     Dnnn decimal value
                      thr=line(i+1:)
                   read(thr,'(i3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('e','E');lineout=lineout//char( 27)         ! %e     escape
            case('f','F');lineout=lineout//char( 12)         ! %f     form feed
            case('n','N');lineout=lineout//char( 10)         ! %n     new line
          !!case('n','N');lineout=lineout//new_line('A')     ! %n     new line
            case('o','O')
                      thr=line(i+1:)
                   read(thr,'(o3)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+3
            case('r','R');lineout=lineout//char( 13)         ! %r     carriage return
            case('t','T');lineout=lineout//char(  9)         ! %t     horizontal tab
          !!case('t','T');lineout=lineout//c_horizontal_tab  ! %t     horizontal tab
            case('v','V');lineout=lineout//char( 11)         ! %v     vertical tab
            case('x','X','h','H')                            ! %x     xHH  byte with hexadecimal value HH (1 to 2 digits)
                      thr=line(i+1:)
                   read(thr,'(z2)',iostat=ios)xxx
                      lineout=lineout//char(xxx)
                   i=i+2
            end select BACKSLASH
         else
            lineout=lineout//esc                             ! escape character, defaults to backslash
         endif
      else
         lineout=lineout//line(i:i)
      endif
      if(i.ge.ilen)exit EXP
   enddo EXP

end function expand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expand()

character(len=*),parameter::ident_30="@(#)M_strings::test_expand(3f): test filter to expand escape sequences in input lines"

integer :: i
character(len=80) :: line
   call unit_check_start('expand',' &
      & -description ''expand escape sequences in a string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('expand',expand('\e\d0912J').eq.char(27)//'[2J','a vt102 sequence to clear the screen')
   call unit_check('expand',expand('this is a test').eq.'this is a test',msg='test plain text')

   !check all ASCII values
   do i=0,127
       write(line,'("%d",i3.3)')i
       call unit_check('expand',expand(line,'%').eq.char(i),msg='check all valid decimal values')
       write(line,'("%o",o3.3)')i
       call unit_check('expand',expand(line,'%').eq.char(i),msg='check all valid octal values')
       write(line,'("%x",z2.2)')i
       call unit_check('expand',expand(line,'%').eq.char(i),msg='check all hexadecimal values')
   enddo

   call unit_check('expand',expand('%d008%d027%d013%d011%d007%d009','%').eq. &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test decimal escape characters')
   call unit_check('expand',expand('%b%e%r%v%a%t','%').eq. &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test escape characters')
   call unit_check_done('expand')

end subroutine test_expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notabs(3f) - [M_strings:NONALPHA] expand tab characters
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!##DESCRIPTION
!!     NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!     columns. It assumes a tab is set every 8 characters. Trailing spaces
!!     are removed.
!!
!!     In addition, trailing carriage returns and line feeds are removed
!!     (they are usually a problem created by going to and from MSWindows).
!!
!!     What are some reasons for removing tab characters from an input line?
!!     Some Fortran compilers have problems with tabs, as tabs are not
!!     part of the Fortran character set.  Some editors and printers will
!!     have problems with tabs.  It is often useful to expand tabs in input
!!     files to simplify further processing such as tokenizing an input line.
!!
!!##OPTIONS
!!     instr     Input line to remove tabs from
!!
!!##RESULTS
!!     outstr    Output string with tabs expanded.
!!     ilen      Significant length of returned string
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use M_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       READFILE: block
!!          do
!!             read(*,'(A)',iostat=ios)in
!!             if(ios /= 0) exit READFILE
!!             call notabs(in,out,iout)
!!             write(*,'(a)')out(:iout)
!!          enddo
!!       endblock READFILE
!!
!!    end program demo_notabs
!!
!!##SEE ALSO
!!     GNU/Unix commands expand(1) and unexpand(1)
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!    Public Domain
subroutine notabs(INSTR,OUTSTR,ILEN)

character(len=*),parameter::ident_31="&
&@(#)M_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

CHARACTER(LEN=*),INTENT(IN)   :: instr        ! input line to scan for tab characters
CHARACTER(LEN=*),INTENT(OUT)  :: outstr       ! tab-expanded version of INSTR produced
INTEGER,INTENT(OUT)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR
!===================================================================================================================================
INTEGER,PARAMETER             :: tabsize=8    ! assume a tab stop is set every 8th column
INTEGER                       :: ipos         ! position in OUTSTR to put next character of INSTR
INTEGER                       :: lenin        ! length of input string trimmed of trailing spaces
INTEGER                       :: lenout       ! number of characters output string can hold
INTEGER                       :: istep        ! counter that advances thru input string INSTR one character at a time
CHARACTER(LEN=1)              :: c            ! character in input line being processed
INTEGER                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE notabs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notabs()
character(len=:),allocatable :: inline
character(len=:),allocatable :: expected
character(len=1024)          :: outline
integer                      :: iout
   call unit_check_start('notabs',' &
      & -description ''convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   inline= 'one '//char(9)//'and'//repeat(char(9),3)//'two'
   expected='one     and                     two'
   call notabs(inline,outline,iout)
   if(unit_check_level.ne.0)then
      write(*,*)'*test_notabs*',inline
      write(*,*)'*test_notabs*',outline
      write(*,*)'*test_notabs*',len_trim(outline),iout
   endif
   call unit_check('notabs',outline.eq.expected.and.iout.eq.35,msg='expand a line with tabs in it')
   call unit_check_done('notabs',msg='')
end subroutine test_notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!       adjustc(3f) - [M_strings:WHITESPACE] center text
!!       (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   pure function adjustc(string[,length])
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in),optional  :: length
!!    character(len=:),allocatable :: adjustc
!!##DESCRIPTION
!!     Centers input text in a string of the length specified. Returns a
!!     string of length LENGTH if LENGTH is present. Otherwise returns a
!!     string of the length of the input string.
!!##OPTIONS
!!     string  input string to trim and center
!!     length  line length to center text in, optional.
!!##RETURNS
!!     adjustc  centered output string
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_adjustc
!!    use M_strings, only : adjustc
!!    !  using length of the input string
!!       write(*,'(a)')       '================================'
!!       write(*,'(a)')adjustc('centered string                 ')
!!       write(*,'(a)')adjustc('                 centered string')
!!       write(*,'(a)')adjustc('  centered string               ')
!!    !  using explicit output string length
!!       write(*,'(a)')repeat('=',50)
!!       write(*,'(a)')adjustc('this is a centered string',50)
!!       write(*,'(a)')repeat('=',50)
!!    end program demo_adjustc
!!
!!   Expected output
!!
!!    ================================
!!            centered string
!!            centered string
!!            centered string
!!    ==================================================
!!                this is a centered string
!!    ==================================================
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function adjustc(string,length)

character(len=*),parameter::ident_32="@(#)M_strings::adjustc(3f): center text"

!>
!! PROCEDURE   adjustc(3f)
!! DESCRIPTION center text using implicit or explicit length
!!##VERSION     2.0, 20160711
!! AUTHOR      John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: string         ! input string to trim and center
integer,intent(in),optional  :: length         ! line length to center text in
character(len=:),allocatable :: adjustc        ! output string
integer                      :: inlen
integer                      :: ileft          ! left edge of string if it is centered
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(length))then                     ! optional length
      inlen=length                             ! length will be requested length
      if(inlen.le.0)then                       ! bad input length
         inlen=len(string)                     ! could not use input value, fall back to length of input string
      endif
   else                                        ! output length was not explicitly specified, use input string length
      inlen=len(string)
   endif
   allocate(character(len=inlen):: adjustc)    ! create output at requested length
   adjustc(1:inlen)=' '                        ! initialize output string to all blanks
!-----------------------------------------------------------------------------------------------------------------------------------
   ileft =(inlen-len_trim(adjustl(string)))/2  ! find starting point to start input string to center it
   if(ileft.gt.0)then                          ! if string will fit centered in output
      adjustc(ileft+1:inlen)=adjustl(string)   ! center the input text in the output string
   else                                        ! input string will not fit centered in output string
      adjustc(1:inlen)=adjustl(string)         ! copy as much of input to output as can
   endif
end function adjustc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_adjustc
character(len=80),allocatable :: expected(:)
character(len=80),allocatable :: left(:)
character(len=80),allocatable :: input(:)
integer                       :: i
   call unit_check_start('adjustc',' &
      & -description ''elemental function centers string within the length of the input string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   expected=[ character(len=80) ::                                                     &
   '12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   '                            An Ode to Centered Text                             ', &
   '                                                                                ', &
   '       Centered text is acceptable when used for short phrases or titles,       ', &
   '              like the name on your BUSINESS CARDS or LETTERHEAD.               ', &
   '              In documents, you can center major section headings               ', &
   '                  like "Introduction" and "Table of Contents."                  ', &
   '                     But if you enjoy centering text, then                      ', &
   '                          you should learn to use the                           ', &
   '                                HARD LINE BREAK                                 ', &
   '                              so your lines start                               ', &
   '                                  in sensible                                   ', &
   '                                    places.                                     ', &
   '                                      OK?                                       ', &
   '                                                                                ']
   left=expected
   input=expected
   ! make copy with all strings left-justified
   do i=1,size(left)
      left(i)=adjustl(left(i))
   enddo
   if(unit_check_level.gt.0)write(*,'(a)')left

   ! now center the left-justified copy
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   ! check against expected output
   call unit_check('adjustc',all(expected.eq.input),msg='text centering')

   ! indent lines different amounts
   do i=1,size(left)
      input(i)=repeat(' ',i-1)//left(i)
   enddo
   if(unit_check_level.gt.0)write(*,'(a)')input

   ! recenter it again
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   if(unit_check_level.gt.0)write(*,'(a)')input
   call unit_check('adjustc',all(expected.eq.input),msg='text centering')

   call unit_check_done('adjustc')
end subroutine test_adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    nospace(3f) - [M_strings:WHITESPACE] remove all whitespace from input string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function nospace(str) - remove all whitespace from input string
!!
!!     character(len=*),intent(in)          :: str
!!     character(len=:),allocatable         :: nospace
!!##DESCRIPTION
!!
!!    nospace(3f) removes space, tab, carriage return, new line, vertical
!!    tab, formfeed and null characters (called "whitespace"). The output
!!    is returned trimmed.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_nospace
!!     use M_strings, only: nospace
!!     implicit none
!!     character(len=:),allocatable  :: s
!!        s='  This     is      a     test  '
!!        write(*,*) 'original input string is ....',s
!!        write(*,*) 'processed output string is ...',nospace(s)
!!        if(nospace(s).eq.'Thisisatest')then
!!           write(*,*)'nospace test passed'
!!        else
!!           write(*,*)'nospace test error'
!!        endif
!!     end program demo_nospace
!!
!!   Expected output
!!
!!     original input string is ....  This     is      a     test
!!     processed output string is ...Thisisatest
!!     nospace test passed
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function nospace(line)

character(len=*),parameter::ident_33="@(#)M_strings::nospace(3f): remove all whitespace from input string"

character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace          ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(nospace,mold=line)                      ! initially make output line length of input line
   nospace(:len_trim(nospace))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace=trim(nospace)                            ! blank out unpacked part of line
end function nospace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nospace
!!use M_strings, only: nospace
implicit none
   character(len=:),allocatable :: string
   string='  This     is      a     test  '
   string=nospace(string)
   call unit_check_start('nospace',' &
      & -description ''function replaces whitespace with nothing'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   if (string .ne. 'Thisisatest')then
      call unit_check_bad('nospace')
   endif
   call unit_check_good('nospace')
end subroutine test_nospace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    stretch(3f) - [M_strings:LENGTH] return string padded to at least specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function stretch(str,length,pattern,suffix) result(strout)
!!
!!     character(len=*),intent(in)         :: str
!!     integer,intent(in)                  :: length
!!     character(len=*)intent(in),optional :: pattern
!!     character(len=*)intent(in),optional :: suffix
!!     character(len=:),allocatable        :: strout
!!##DESCRIPTION
!!    stretch(3f) pads a string with spaces to at least the specified
!!    length. If the trimmed input string is longer than the requested
!!    length the original string is returned trimmed of trailing spaces.
!!##OPTIONS
!!    str      the input string to return trimmed, but then padded to
!!             the specified length if shorter than length
!!    length   The minimum string length to return
!!    pattern  optional string to use as padding. Defaults to a space.
!!    suffix   optional string to append to output string
!!##RETURNS
!!    strout  The input string padded to the requested length or
!!            the trimmed input string if the input string is
!!            longer than the requested length.
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_stretch
!!      use M_strings, only : stretch
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!      integer                      :: i
!!         answer=stretch(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=stretch(string,20)
!!         write(*,'("[",a,"]")') answer
!!         i=30
!!         write(*,*)
!!         write(*,'(1x,a,i0)') stretch('CHAPTER 1 : The beginning ',i,'.'), 1
!!         write(*,'(1x,a,i0)') stretch('CHAPTER 2 : The end ',i,'.'),       1234
!!         write(*,'(1x,a,i0)') stretch('APPENDIX ',i,'.'),                  1235
!!         write(*,*)
!!         write(*,'(1x,a,i7)') stretch('CHAPTER 1 : The beginning ',i,'.'), 1
!!         write(*,'(1x,a,i7)') stretch('CHAPTER 2 : The end ',i,'.'),       1234
!!         write(*,'(1x,a,i7)') stretch('APPENDIX ',i,'.'),                  1235
!!         write(*,*)
!!         write(*,*) stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
!!         write(*,*) stretch('CHAPTER 2 : The end ',i,suffix=': '),       1234
!!         write(*,*) stretch('APPENDIX ',i,suffix=': '),                  1235
!!     end program demo_stretch
!!
!!   Results:
!!
!!    [abcdefghij]
!!    [abcdefghij          ]
!!
!!     CHAPTER 1 : The beginning ....1
!!     CHAPTER 2 : The end ..........1234
!!     APPENDIX .....................1235
!!
!!     CHAPTER 1 : The beginning ....      1
!!     CHAPTER 2 : The end ..........   1234
!!     APPENDIX .....................   1235
!!
!!     CHAPTER 1 : The beginning     :            1
!!     CHAPTER 2 : The end           :         1234
!!     APPENDIX                      :         1235
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function stretch(line,length,pattern,suffix) result(strout)

character(len=*),parameter::ident_34="@(#)M_strings::stretch(3f): return string padded to at least specified length"

character(len=*),intent(in)                  :: line
integer,intent(in)                           :: length
character(len=*),intent(in),optional         :: pattern
character(len=*),intent(in),optional         :: suffix
!!character(len=max(length,len(trim(line)))) :: strout
character(len=:),allocatable                 :: strout
   if(present(pattern))then
      strout=atleast(line,length,pattern)
   else
      strout=atleast(line,length)
   endif
   if(present(suffix))then
      strout=strout//suffix
   endif
end function stretch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stretch()
   call unit_check_start('stretch',' &
      & -description ''return a string of at least specified length'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('stretch',stretch('Hello World',20)//'!'.eq.'Hello World         !',msg='check if padded')
   call unit_check('stretch',len(stretch('Hello World',20)).eq.20,msg='check padded length')
   call unit_check('stretch',len(stretch('Hello World',2)).eq.11 &
           .and.stretch('Hello World',2).eq.'Hello World', &
           msg='check not truncated')
   call unit_check_done('stretch',msg='tests completed')
end subroutine test_stretch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   atleast(3f) - [M_strings:LENGTH] return string padded to at least specified length
!!   (LICENSE:PD)
!! !!
!!##SYNOPSIS
!!
!! !!
!!   function atleast(str,length,pattern) result(strout)
!! !!
!!    character(len=*)                           :: str
!!    integer,intent(in)                         :: length
!!    character(len=max(length,len(trim(line)))) ::  strout
!!    character(len=*),optional                  ::  pattern
!!##DESCRIPTION
!!   atleast(3f) pads a string with spaces to at least the specified
!!   length. If the trimmed input string is longer than the requested
!!   length the trimmed string is returned.
!!##OPTIONS
!!   str      the input string to return trimmed, but then padded to
!!            the specified length if shorter than length
!!   length   The minimum string length to return
!!   pattern  optional string to use as padding. Defaults to a space.
!!##RETURNS
!!   strout  The input string padded to the requested length or
!!           the trimmed input string if the input string is
!!           longer than the requested length.
!! !!
!!##EXAMPLE
!!
!! !!
!!   Sample Program:
!! !!
!!    program demo_atleast
!!     use M_strings, only : atleast
!!     implicit none
!!     character(len=10)            :: string='abcdefghij'
!!     character(len=:),allocatable :: answer
!!     integer                      :: i
!!        answer=atleast(string,5)
!!        write(*,'("[",a,"]")') answer
!!        answer=atleast(string,20)
!!        write(*,'("[",a,"]")') answer
!!        i=30
!!        write(*,*)
!!        write(*,'(1x,a,i0)') atleast('CHAPTER 1 : The beginning ',i,'.'), 1
!!        write(*,'(1x,a,i0)') atleast('CHAPTER 2 : The end ',i,'.'),       1234
!!        write(*,'(1x,a,i0)') atleast('APPENDIX ',i,'.'),                  1235
!!        write(*,*)
!!        write(*,'(1x,a,i7)') atleast('CHAPTER 1 : The beginning ',i,'.'), 1
!!        write(*,'(1x,a,i7)') atleast('CHAPTER 2 : The end ',i,'.'),       1234
!!        write(*,'(1x,a,i7)') atleast('APPENDIX ',i,'.'),                  1235
!!    end program demo_atleast
!!
!!  Results:
!!
!!   [abcdefghij]
!!   [abcdefghij          ]
!!
!!    CHAPTER 1 : The beginning ....1
!!    CHAPTER 2 : The end ..........1234
!!    APPENDIX .....................1235
!!
!!    CHAPTER 1 : The beginning ....      1
!!    CHAPTER 2 : The end ..........   1234
!!    APPENDIX .....................   1235
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

character(len=*),parameter::ident_35="@(#)M_strings::atleast(3f): return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_atleast()
   call unit_check_start('atleast',' &
      & -description ''return a string of at least specified length'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('atleast',atleast('Hello World',20)//'!'.eq.'Hello World         !',msg='check if padded')
   call unit_check('atleast',len(atleast('Hello World',20)).eq.20,msg='check padded length')
   call unit_check('atleast',len(atleast('Hello World',2)).eq.11 &
           .and.atleast('Hello World',2).eq.'Hello World', &
           msg='check not truncated')
   call unit_check_done('atleast',msg='tests completed')
end subroutine test_atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_strings:LENGTH] return string trimmed or padded to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!##RESULTS
!!    strout  output string
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!     program demo_lenset
!!      use M_strings, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

character(len=*),parameter::ident_36="@(#)M_strings::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lenset()
character(len=10)            :: string='abcdefghij'
   call unit_check_start('lenset',' &
      & -description ''return a string as specified length'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

        call unit_check('lenset',len(lenset(string, 5)).eq.5)
        call unit_check('lenset',len(lenset(string,20)).eq.20)
        call unit_check('lenset',lenset(string,20).eq.'abcdefghij')
        call unit_check('lenset',lenset(string, 5).eq.'abcde')
   call unit_check_done('lenset')
end subroutine test_lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_strings:LENGTH] pads strings to same length and then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in)     :: str1
!!     character(len=*),intent(in)     :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!     program demo_merge_str
!!     use M_strings, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!    Expected output
!!
!!     [first string]
!!     [second string is longer]
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

character(len=*),parameter::ident_37="@(#)M_strings::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in)     :: str1
character(len=*),intent(in)     :: str2
logical,intent(in)              :: expr
character(len=:),allocatable    :: strout
integer                         :: big
   big=max(len(str1),len(str2))
   strout=trim(merge(lenset(str1,big),lenset(str2,big),expr))
end function merge_str
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_merge_str()
character(len=:), allocatable :: answer
   call unit_check_start('merge_str',' &
      & -description ''make strings of equal length and then call MERGE(3f) intrinsic'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   answer=merge_str('first string', 'second string is longer',10.eq.10)
   if(unit_check_level.gt.0)then
      write(*,*)'['//answer//']',len(answer)
   endif
   call unit_check('merge_str',answer.eq.'first string'.and.len(answer).eq.12,msg='check true value ')

   answer=merge_str('first string', 'second string is longer',10.ne.10)
   if(unit_check_level.gt.0)then
      write(*,*)'['//answer//']',len(answer)
   endif
   call unit_check('merge_str',answer.eq.'second string is longer'.and.len(answer).eq.23,msg='check false value')

   call unit_check_done('merge_str')
end subroutine test_merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    compact(3f) - [M_strings:WHITESPACE] converts contiguous whitespace to a single character (or nothing)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function compact(STR,CHAR) result (OUTSTR)
!!
!!     character(len=*),intent(in)          :: STR
!!     character(len=*),intent(in),optional :: CHAR
!!     character(len=len(str))              :: OUTSTR
!!##DESCRIPTION
!!    COMPACT(3f) converts multiple spaces, tabs and control characters
!!    (called "whitespace") to a single character or nothing. Leading
!!    whitespace is removed.
!!
!!##OPTIONS
!!    STR     input string to reduce or remove whitespace from
!!    CHAR    By default the character that replaces adjacent
!!            whitespace is a space. If the optional CHAR parameter is supplied
!!            it will be used to replace the whitespace. If a null character is
!!            supplied for CHAR whitespace is removed.
!!##RETURNS
!!    OUTSTR  string of same length as input string but with all contiguous whitespace
!!            reduced to a single space and leading whitespace removed
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_compact
!!     use M_strings, only : compact
!!     implicit none
!!     ! produces 'This is a test               '
!!     write(*,*)compact('  This     is      a     test  ')
!!     ! produces 'Thisisatest                  '
!!     write(*,*)compact('  This     is      a     test  ',char='')
!!     ! produces 'This:is:a:test               '
!!     write(*,*)compact('  This     is      a     test  ',char=':')
!!     ! note CHAR is used to replace the whitespace, but if CHAR is
!!     ! in the original string it is just copied
!!     write(*,*)compact('A  AA    A   AAAAA',char='A')
!!     ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
!!     ! not 'A'
!!    end program demo_compact
!!
!!    Expected output
!!
!!     >This is a test
!!     >Thisisatest
!!     >This:is:a:test
!!     >AAAAAAAAAAAA
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

character(len=*),parameter::ident_38="@(#)M_strings::compact(3f): Converts white-space to single spaces"

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(ichar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   end do IFSPACE

end function compact
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compact
!!use M_strings, only: compact
implicit none
   call unit_check_start('compact',' &
      & -description ''left justify string and replace duplicate whitespace with single characters or nothing'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   if (compact('  This  is     a    test  ') .ne. 'This is a test')then
      call unit_check_bad('compact')
      stop 1
   endif
   if (compact('This is a test') .ne. 'This is a test')then
      call unit_check_bad('compact')
      stop 2
   endif
   if (compact('This-is-a-test') .ne. 'This-is-a-test')then
      call unit_check_bad('compact')
      stop 3
   endif
   if (compact('  This  is     a    test  ',char='') .ne. 'Thisisatest')then
      call unit_check_bad('compact')
      stop 4
   endif
   if (compact('  This  is     a    test  ',char='t') .ne. 'Thististattest')then
      call unit_check_bad('compact')
      stop 5
   endif
   call unit_check_good('compact')
end subroutine test_compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     noesc(3f) - [M_strings:NONALPHA] convert non-printable characters to a space.
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental function noesc(INSTR)
!!
!!     character(len=*),intent(in) :: INSTR
!!     character(len=len(instr))   :: noesc
!!##DESCRIPTION
!!      Convert non-printable characters to a space.
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_noesc
!!
!!     use M_strings, only : noesc
!!     character(len=128) :: ascii
!!     character(len=128) :: cleared
!!     ! fill variable with base ASCII character set
!!     do i=1,128
!!        ascii(i:i)=char(i-1)
!!     enddo
!!     cleared=noesc(ascii)
!!     write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
!!     call ade(ascii)
!!     write(*,*)'Cleared of non-printable characters'
!!     call ade(cleared)
!!     write(*,*)'Cleared string:'
!!     write(*,*)cleared
!!     contains
!!       subroutine ade(string)
!!       implicit none
!!       ! the string to print
!!       character(len=*),intent(in) :: string
!!       ! number of characters in string to print
!!       integer :: ilen
!!       ! counter used to step thru string
!!       integer :: i
!!          ! get trimmed length of input string
!!          ilen=len_trim(string(:len(string)))
!!
!!          ! replace lower unprintable characters with spaces
!!          write(*,101)(merge(string(i:i),' ',&
!!          & ichar(string(i:i)).ge.32         &
!!          & .and.                            &
!!          & ichar(string(i:i)).le.126)       &
!!          & ,i=1,ilen)
!!
!!          ! print ADE value of character underneath it
!!          write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
!!          write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
!!          write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
!!       ! format for printing string characters
!!       101   format(*(a1:))
!!       ! format for printing ADE values
!!       202   format(*(i1:))
!!       end subroutine ade
!!     end program demo_noesc
!!
!!    Expected output
!!
!!    The string is printed with the ADE value vertically beneath.
!!    The original string has all the ADEs from 000 to 127. After
!!    NOESC(3f) is called on the string all the "non-printable"
!!    characters are replaced with a space (ADE of 032).
!!
!!   characters and their ADE (ASCII Decimal Equivalent)
!!
!!    >                                 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111
!!    >00000000001111111111222222222233333333334444444444555555555566666666667777777777888888888899999999990000000000111111111122222222
!!    >01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
!!
!!   Cleared of non-printable characters
!!
!!    >                                 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    >0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000111111111111111111111111111
!!    >3333333333333333333333333333333333333333444444444455555555556666666666777777777788888888889999999999000000000011111111112222222
!!    >2222222222222222222222222222222223456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
!!
!!   Cleared string:
!!    >                                  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function noesc(INSTR)

character(len=*),parameter::ident_39="@(#)M_strings::noesc(3f): convert non-printable characters to a space"

character(len=*),intent(in) :: INSTR      ! string that might contain nonprintable characters
character(len=len(instr))   :: noesc
integer                     :: ic,i10
!-----------------------------------------------------------------------------------------------------------------------------------
   noesc=''                               ! initialize output string
   do i10=1,len_trim(INSTR(1:len(INSTR)))
      ic=ichar(INSTR(i10:i10))
      if(ic.le.31.or.ic.eq.127)then       ! find characters with ADE of 0-31, 127
         noesc(I10:I10)=' '               ! replace non-printable characters with a space
      else
         noesc(I10:I10)=INSTR(i10:i10)    ! copy other characters as-is from input string to output string
      endif
   enddo
end function noesc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_noesc  ! test noesc
!!use M_strings, only : noesc
character(len=23) :: in,out,clr
integer           :: i10
  ! Use goodbad(1) to indicate the test sequence was begun
   call unit_check_start('noesc',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i10=0,127
      write(in, '(i3.3,1x,4a)')i10,char(i10),char(i10),char(i10),' eol'
      write(clr,'(i3.3,1x,"    eol")')i10
      out=noesc(in)
      if(unit_check_level.gt.0)then
         write(*,'(a)')trim(in)
         write(*,'(a)')trim(out)
      endif
      SELECT CASE (i10)
      CASE (:31,127)
        if(out.ne.clr)then
           write(*,*)'Error: noesc did not replace a string with blanks that it should have'
           call unit_check_bad('noesc')
        endif
      CASE DEFAULT
        if(in.ne.out)then
           write(*,*)'Error: noesc changed a string it should not have'
           call unit_check_bad('noesc')
        endif
      END SELECT
   enddo
   call unit_check_good('noesc')
end subroutine test_noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_value(3f) - [M_strings:NUMERIC] subroutine returns numeric value from string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine string_to_value(chars,valu,ierr)
!!
!!     character(len=*),intent(in)              :: chars   ! input string
!!     integer|real|doubleprecision,intent(out) :: valu
!!     integer,intent(out)                      :: ierr
!!##DESCRIPTION
!!       returns a numeric value from a numeric character string.
!!
!!       works with any g-format input, including integer, real, and
!!       exponential. If the input string begins with "B", "Z", or "O"
!!       and otherwise represents a positive whole number it is assumed to
!!       be a binary, hexadecimal, or octal value. If the string contains
!!       commas they are removed. If the string is of the form NN:MMM... or
!!       NN#MMM then NN is assumed to be the base of the whole number.
!!
!!       if an error occurs in the READ, IOSTAT is returned in IERR and
!!       value is set to zero. if no error occurs, IERR=0.
!!##OPTIONS
!!       CHARS  input string to read numeric value from
!!##RETURNS
!!       VALU   numeric value returned. May be INTEGER, REAL, or DOUBLEPRECISION.
!!       IERR   error flag (0 == no error)
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_string_to_value
!!     use M_strings, only: string_to_value
!!     character(len=80) :: string
!!        string=' -40.5e-2 '
!!        call string_to_value(string,value,ierr)
!!        write(*,*) 'value of string ['//trim(string)//'] is ',value
!!    end program demo_string_to_value
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine a2r(chars,valu,ierr)

character(len=*),parameter::ident_40="@(#)M_strings::a2r(3fp): subroutine returns real value from string"

character(len=*),intent(in) :: chars                      ! input string
real,intent(out)            :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(ierr.eq.0)then
      if(valu8.le.huge(valu))then
         valu=real(valu8)
      else
         call journal('sc','*a2r*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2r
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2i(chars,valu,ierr)

character(len=*),parameter::ident_41="@(#)M_strings::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

character(len=*),parameter::ident_42="@(#)M_strings::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero.  if no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg.ne.'')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_value
!!use M_strings, only: string_to_value, s2v, v2s
CHARACTER(len=80) :: STRING
real              :: RVALUE
doubleprecision   :: DVALUE
doubleprecision   :: SUM, SUM2, DELTA
integer           :: IVALUE
integer           :: GOOD
integer           :: ierr
!===================================================================================================================================
   call unit_check_start('string_to_value',' &
      & -description ''generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
!===================================================================================================================================
   STRING=' -40.5e-2 '
   CALL string_to_value(STRING,RVALUE,IERR)
   CALL string_to_value(STRING,DVALUE,IERR)
   CALL string_to_value(STRING,IVALUE,IERR)
   if(unit_check_level.gt.0)then
      WRITE(*,*) 'string_to_value: real value is ',-40.5e-2
      WRITE(*,*) 'string_to_value: double value is ',-40.5d-2
      WRITE(*,*) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      WRITE(*,*) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
      WRITE(*,*) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   STRING=' -40.5d-2 '
   if(unit_check_level.gt.0)then
      CALL string_to_value(STRING,RVALUE,IERR)
      WRITE(*,*) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      CALL string_to_value(STRING,DVALUE,IERR)
      WRITE(*,*) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
       CALL string_to_value(STRING,IVALUE,IERR)
      WRITE(*,*) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   good=0
   call unit_check('string_to_value',rvalue.eq.-40.5e-2)
      good=good*10+1
   call unit_check('string_to_value',dvalue.eq.-40.5d-2)
      good=good*10+1
   call unit_check('string_to_value',dvalue-spacing(dvalue).le.-40.5d-2.and.dvalue+spacing(dvalue).ge.-40.5d-2)
      good=good*10+1
   call unit_check('string_to_value',rvalue-spacing(rvalue).le.-40.5e-2.and.rvalue+spacing(rvalue).ge.-40.5e-2)
      good=good*10+1
!===================================================================================================================================
   SUM=0.0d0
   string='5.555555555555555555555555555555555'
   CALL string_to_value(STRING,RVALUE,IERR)
   SUM=SUM+RVALUE
   CALL string_to_value(STRING,DVALUE,IERR)
   SUM=SUM+DVALUE
   CALL string_to_value(STRING,IVALUE,IERR)
   SUM=SUM+IVALUE
!===================================================================================================================================
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)
   if(unit_check_level.gt.0)then
      write(*,'(80("="))')
      WRITE(*,*) 'string_to_value: real value is ', 5.555555555555555555555555555555555e0
      WRITE(*,*) 'string_to_value: double value is ', 5.555555555555555555555555555555555d0
      WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',RVALUE
      WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',DVALUE
      WRITE(*,*) 'string_to_value: value of string ['//trim(STRING)//'] is ',IVALUE
      WRITE(*,*) 'string_to_value: SUM=', SUM
      WRITE(*,*) 'string_to_value: SUM2=', SUM2
      WRITE(*,*) 'string_to_value: DELTA=', DELTA
   endif
   if(sum.eq.sum2)then
      good=good*10+1
      if(unit_check_level.gt.0)then
      write(*,*)'string_to_value: good ',good
      endif
   else
      call unit_check_bad('string_to_value')
   endif
   if(sum+delta.ge.sum2.and.sum-delta.le.sum2)then
      good=good*10+1
      if(unit_check_level.gt.0)then
      write(*,*)'string_to_value: good ',good
      endif
   else
      call unit_check_bad('string_to_value')
   endif
!===================================================================================================================================
   call unit_check_good('string_to_value')
!===================================================================================================================================
end subroutine test_string_to_value
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2v(3f) - [M_strings:NUMERIC] function returns doubleprecision numeric value from a string
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function s2v(string[,ierr][,onerr])
!!
!!     character(len=*)             :: string
!!     doubleprecision              :: s2v
!!     integer,intent(out),optional :: ierr
!!     class(*),intent(in),optional :: onerr
!!##DESCRIPTION
!!       This function converts a string to a DOUBLEPRECISION numeric value.
!!
!!       The intrinsics INT(3f), REAL(3f), and DBLE(3f) are also extended to take
!!       CHARACTER variables. The KIND= keyword is not supported on the extensions.
!!##OPTIONS
!!
!!     string   holds string assumed to represent a numeric value
!!     ierr     If an error occurs the program is stopped if the optional
!!              parameter IERR is not present. If IERR returns a non-zero
!!              value an error occurred.
!!     onerr    The value to return on error. A value of NaN is
!!              returned on error by default.
!!##RETURNS
!!     s2v
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_s2v
!!
!!     use M_strings, only: s2v, int, real, dble
!!     implicit none
!!     character(len=8)              :: s=' 10.345 '
!!     integer                       :: i
!!     character(len=14),allocatable :: strings(:)
!!     doubleprecision               :: dv
!!     integer                       :: errnum
!!
!!     ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
!!     strings=[&
!!     &' 10.345       ',&
!!     &'+10           ',&
!!     &'    -3        ',&
!!     &'    -4.94e-2  ',&
!!     &'0.1           ',&
!!     &'12345.678910d0',&
!!     &'              ',& ! Note: will return zero without an error message
!!     &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
!!     &'WHAT?         ']  ! Note: error messages will appear, zero returned
!!
!!     ! a numeric value is returned, so it can be used in numeric expression
!!     write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
!!     write(*,*)
!!     write(*,*)' STRING            VALUE                    ERROR_NUMBER'
!!     do i=1,size(strings)
!!        ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
!!        ! as it does I/O when errors occur, so called on a separate line
!!        dv=s2v(strings(i),errnum)
!!        write(*,*) strings(i)//'=',dv,errnum
!!     enddo
!!     write(*,*)"Extended intrinsics"
!!     write(*,*)'given inputs:',s,strings(:8)
!!     write(*,*)'INT(3f):',int(s),int(strings(:8))
!!     write(*,*)'REAL(3f):',real(s),real(strings(:8))
!!     write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
!!     write(*,*)"That's all folks!"
!!
!!     end program demo_s2v
!!
!!    Expected output
!!
!!     >1/2 value of string is    5.1725000000000003
!!     >
!!     > STRING            VALUE                    ERROR_NUMBER
!!     > 10.345       =   10.345000000000001                0
!!     >+10           =   10.000000000000000                0
!!     >    -3        =  -3.0000000000000000                0
!!     >    -4.94e-2  =  -4.9399999999999999E-002           0
!!     >0.1           =  0.10000000000000001                0
!!     >12345.678910d0=   12345.678910000001                0
!!     >              =   0.0000000000000000                0
!!     >1 2 1 2 1 . 0 =   12121.000000000000                0
!!     >*a2d* - cannot produce number from string [WHAT?]
!!     >*a2d* - [Bad value during floating point read]
!!     >WHAT?         =   0.0000000000000000             5010
!!     >Extended intrinsics
!!     >given inputs: 10.345 10.345 +10 -3 -4.94e-2 0.1 12345.678910d0 1 2 1 2 1 . 0
!!     >INT(3f): 10 10 10 -3 0 0 12345 0 12121
!!     >REAL(3f): 10.3450003 10.3450003 10.0000000 -3.00000000 -4.94000018E-02
!!     >          0.100000001 12345.6787 0.00000000 12121.0000
!!     >DBLE(3f): 10.345000000000001 10.345000000000001 10.000000000000000
!!     >          -3.0000000000000000 -4.9399999999999999E-002 0.10000000000000001
!!     >          12345.678910000001 0.0000000000000000 12121.000000000000
!!     >That's all folks!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!!##PROCEDURE:
!! DESCRIPTION: s2v(3f): function returns doubleprecision number from string;zero if error occurs
!!##VERSION:     2.0, 20160704
!! AUTHOR:      John S. Urban
doubleprecision function s2v(chars,ierr,onerr)
!  1989 John S. Urban

character(len=*),parameter::ident_43="@(#)M_strings::s2v(3f): returns doubleprecision number from string"


character(len=*),intent(in)  :: chars
integer,optional             :: ierr
doubleprecision              :: valu
integer                      :: ierr_local
class(*),intent(in),optional :: onerr

   ierr_local=0
   if(present(onerr))then
      call a2d(chars,valu,ierr_local,onerr)
   else
      call a2d(chars,valu,ierr_local)
   endif
   if(present(ierr))then ! if error is not returned stop program on error
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local.ne.0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!===================================================================================================================================
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!===================================================================================================================================
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
!===================================================================================================================================
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!===================================================================================================================================
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!===================================================================================================================================
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!===================================================================================================================================
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2v()
doubleprecision SUM, SUM2, DELTA
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)

   call unit_check_start('s2v',' &
      & -description ''function returns doubleprecision value from string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   SUM=s2v('5.55555555555555555555555555e0')+REAL(s2v('5.55555555555555555555555555d0'))+INT(s2v('5.55555555555555555555555555'))
   if(unit_check_level.gt.0)then
      WRITE(*,*) 's2v: SUM2=', SUM2
      WRITE(*,*) 's2v: SUM=', SUM
      WRITE(*,*) 's2v: DELTA=', DELTA
   endif
   call unit_check('s2v',sum+delta.ge.sum2.and.sum-delta.le.sum2,msg=msg('SUM=',sum,'SUM2=',sum2,'DELTA=',delta))
   call unit_check_done('s2v')
end subroutine test_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()())()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_strings:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,ilen,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: ilen
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!##DESCRIPTION
!!
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT if longer.
!!       ILEN    position of last non-blank character in returned string; optional.
!!       IERR    If not zero, error occurred; optional.
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      program demo_value_to_string
!!      use M_strings, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: ilen
!!         call value_to_string(3.0/4.0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(3.0/4.0,string,ilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1234,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,ilen)
!!         write(*,*) 'The value is [',string(:ilen),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

character(len=*),parameter::ident_40="@(#)M_strings::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !! cannot currently do I/O from a function being called from I/O
      !!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_value_to_string
!!use M_strings, only: value_to_string
implicit none
CHARACTER(LEN=80) :: STRING
doubleprecision   :: DVALUE
real              :: RVALUE
integer           :: IVALUE
integer           :: ILEN
integer           :: IERR
integer           :: IERRSUM=0
!===================================================================================================================================
   call unit_check_start('value_to_string',' &
      & -description ''generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER value'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   DVALUE=5.5555555555555555555555d0
   call value_to_string(DVALUE,STRING,ILEN,IERR)
   if(unit_check_level.gt.0)then
      write(*,*)'value_to_string: DOUBLE TEST VALUE=',dvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN.le.0)IERRSUM=IERRSUM+1000

   RVALUE=3.3333333333333333333333
   call value_to_string(RVALUE,STRING,ILEN,IERR)
   if(unit_check_level.gt.0)then
      write(*,*)'value_to_string: REAL TEST VALUE=',rvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN.le.0)IERRSUM=IERRSUM+10000

   IVALUE=1234567890
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level.gt.0)then
      write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(string.ne.'1234567890')then
       IERRSUM=IERRSUM+100000
   endif
   if(ILEN.ne.10)then
       IERRSUM=IERRSUM+1000000
   endif

   IVALUE=0
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level.gt.0)then
      write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif

   IVALUE=-12345
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level.gt.0)then
      write(*,*)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   if(string.ne.'-12345')then
       IERRSUM=IERRSUM+1000000
   endif
   if(ILEN.ne.6)then
       IERRSUM=IERRSUM+10000000
   endif
!===================================================================================================================================
   call unit_check('value_to_string',ierrsum.eq.0,msg='value_to_string'//v2s(ierrsum))
   call unit_check_done('value_to_string')
!===================================================================================================================================
end subroutine test_value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      v2s(3f) - [M_strings:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function v2s(value) result(outstr)
!!
!!        integer|real|doubleprecision|logical,intent(in ) :: value
!!        character(len=:),allocatable :: outstr
!!        character(len=*),optional,intent(in) :: fmt
!!
!!##DESCRIPTION
!!
!!    v2s(3f) returns a representation of a numeric value as a
!!    string when given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the strings using internal WRITE()
!!    statements. Trailing zeros are removed from non-zero values, and the
!!    string is left-justified.
!!
!!##OPTIONS
!!    VALUE   input value to be converted to a string
!!    FMT     format can be explicitly given, but is limited to
!!            generating a string of eighty or less characters.
!!
!!##RETURNS
!!    OUTSTR  returned string representing input value,
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_v2s
!!    use M_strings, only: v2s
!!    write(*,*) 'The value of 3.0/4.0 is ['//v2s(3.0/4.0)//']'
!!    write(*,*) 'The value of 1234    is ['//v2s(1234)//']'
!!    write(*,*) 'The value of 0d0     is ['//v2s(0d0)//']'
!!    write(*,*) 'The value of .false. is ['//v2s(.false.)//']'
!!    write(*,*) 'The value of .true. is  ['//v2s(.true.)//']'
!!    end program demo_v2s
!!
!!   Expected output
!!
!!     The value of 3.0/4.0 is [0.75]
!!     The value of 1234    is [1234]
!!     The value of 0d0     is [0]
!!     The value of .false. is [F]
!!     The value of .true. is  [T]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! very odd compiler problems in many (but not all) programs using this routine; GNU Fortran (GCC) 5.4.0; 20161030
function v2s_bug(gval) result(outstr)

character(len=*),parameter::ident_44="@(#)M_strings::v2s_bug(3f): function returns string given numeric value"

class(*),intent(in)          :: gval                         ! input value to convert to a string
character(len=:),allocatable :: outstr                       ! output string to generate
character(len=80)            :: string
   call value_to_string(gval,string)
   outstr=trim(string)
end function v2s_bug
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2s_bug()
doubleprecision SUM, SUM2, DELTA
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)
   call unit_check_start('v2s_bug',' &
      & -description ''generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   SUM=s2v(v2s_bug(5.55555555555555555555555555d0))
   SUM=SUM+REAL(s2v(v2s_bug(5.55555555555555555555555555e0)))
   SUM=SUM+INT(s2v(v2s_bug(5.55555555555555555555555555e0)))
   if(unit_check_level.gt.0)then
      WRITE(*,*) 'v2s_bug: SUM2=', SUM2
      WRITE(*,*) 'v2s_bug: SUM=', SUM
      WRITE(*,*) 'v2s_bug: DELTA=', DELTA
   endif
   call unit_check('v2s_bug',sum+delta.ge.sum2.and.sum-delta.le.sum2)
   call unit_check_done('v2s_bug')
end subroutine test_v2s_bug
!===================================================================================================================================
function d2s(dvalue,fmt) result(outstr)

character(len=*),parameter::ident_45="@(#)M_strings::d2s(3fp): private function returns string given doubleprecision value"

doubleprecision,intent(in)   :: dvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(dvalue,string,fmt=fmt)
   else
      call value_to_string(dvalue,string)
   endif
   outstr=trim(string)
end function d2s
!===================================================================================================================================
function r2s(rvalue,fmt) result(outstr)

character(len=*),parameter::ident_46="@(#)M_strings::r2s(3fp): private function returns string given real value"

real,intent(in)              :: rvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(rvalue,string,fmt=fmt)
   else
      call value_to_string(rvalue,string)
   endif
   outstr=trim(string)
end function r2s
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

character(len=*),parameter::ident_47="@(#)M_strings::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
function l2s(lvalue,fmt) result(outstr)

character(len=*),parameter::ident_48="@(#)M_strings::l2s(3fp): private function returns string given logical value"

logical,intent(in)           :: lvalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)             :: string
   if(present(fmt))then
      call value_to_string(lvalue,string,fmt=fmt)
   else
      call value_to_string(lvalue,string)
   endif
   outstr=trim(string)
end function l2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2s()
use M_math, only : almost
real            :: SUM
doubleprecision :: SUM2
   call unit_check_start('v2s',' &
      & -description ''generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   SUM2=5.555555555555555555555555555555555d0
   SUM=5.555555555555555555555555555555555e0
   call unit_check('v2s',almost(REAL(s2v(v2s(SUM))),SUM,7),msg=msg('real',SUM,REAL(s2v(v2s(SUM)))))
   call unit_check('v2s',almost(s2v(v2s(SUM2)),SUM2,15),msg=msg('doubleprecision',SUM2,s2v(v2s(SUM))))
   call unit_check('v2s',v2s(1234).eq.'1234',msg=msg('integer',1234))
   call unit_check('v2s',v2s(.true.).eq.'T',msg=msg('logical',v2s(.true.)))
   call unit_check('v2s',v2s(.false.).eq.'F',msg=msg('logical',v2s(.false.)))
   call unit_check_done('v2s')
end subroutine test_v2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isnumber(3f) - [M_strings:NUMERIC] determine if a string represents a number
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function isnumber(str,msg)
!!
!!     character(len=*),intent(in)  :: str
!!     character(len=:),intent(out),allocatable,optional  :: msg
!!##DESCRIPTION
!!     ISNUMBER(3f) returns a value greater than zero if the string represents
!!     a number, and a number less than or equal to zero if it is a bad number.
!!     Blank characters are ignored.
!!##OPTIONS
!!     str  the string to evaluate as to whether it represents a numeric value
!!          or not
!!     msg  An optional message describing the string
!!##RETURNS
!!     isnumber  the following values are returned
!!
!!                1 for an integer             [-+]NNNNN
!!                2 for a whole number         [-+]NNNNN.
!!                3 for a real value           [-+]NNNNN.MMMM
!!                4 for a exponential value    [-+]NNNNN.MMMM[-+]LLLL
!!                                             [-+]NNNNN.MMMM[ed][-+]LLLL
!!
!!               values less than 1 represent an error
!!
!!##EXAMPLES
!!
!!   As the example shows, you can use an internal READ(3f) along with the IOSTAT=
!!   parameter to check (and read) a string as well.
!!
!!     program demo_isnumber
!!     use M_strings, only : isnumber
!!     implicit none
!!     character(len=256) :: line
!!     real               :: value
!!     integer            :: ios
!!     integer            :: answer
!!     character(len=256) :: message
!!     character(len=:),allocatable :: description
!!        write(*,*)'Begin entering values, one per line'
!!        do
!!           read(*,'(a)',iostat=ios)line
!!           !
!!           ! try string as number using list-directed input
!!           line=''
!!           read(line,*,iostat=ios,iomsg=message) value
!!           if(ios.eq.0)then
!!              write(*,*)'VALUE=',value
!!           else
!!              write(*,*)'ERROR:',ios,trim(message)
!!           endif
!!           !
!!           ! try string using isnumber(3f)
!!           answer=isnumber(line,msg=description)
!!           if(answer.gt.0)then
!!              write(*,*)' for ',trim(line),' ',answer,':',description
!!           else
!!              write(*,*)' ERROR for ',trim(line),' ',answer,':',description
!!           endif
!!           !
!!        enddo
!!     end program demo_isnumber
!!
!!  Example run
!!
!!    > Begin entering values
!!    > ERROR:          -1 End of file
!!    >  ERROR for            -1 :null string
!!    >10
!!    > VALUE=   10.0000000
!!    >  for 10            1 :integer
!!    >20
!!    > VALUE=   20.0000000
!!    >  for 20            1 :integer
!!    >20.
!!    > VALUE=   20.0000000
!!    >  for 20.            2 :whole number
!!    >30.1
!!    > VALUE=   30.1000004
!!    >  for 30.1            3 :real number
!!    >3e1
!!    > VALUE=   30.0000000
!!    >  for 3e1            4 :value with exponent
!!    >1-2
!!    > VALUE=   9.99999978E-03
!!    >  for 1-2            4 :value with exponent
!!    >100.22d-4
!!    > VALUE=   1.00220004E-02
!!    >  for 100.22d-4            4 :value with exponent
!!    >1--2
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1--2           -5 :bad number
!!    >e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e           -6 :missing leading value before exponent
!!    >e1
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for e1           -6 :missing leading value before exponent
!!    >1e
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e           -3 :missing exponent
!!    >1e+
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+           -4 :missing exponent after sign
!!    >1e+2.0
!!    > ERROR:        5010 Bad real number in item 1 of list input
!!    >  ERROR for 1e+2.0           -5 :bad number
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function isNumber(string,msg,verbose)
implicit none

character(len=*),parameter::ident_49="@(#)M_strings::isnumber(3f): Determines if a string is a number of not."

character(len=*),intent(in)    :: string
character(len=:),intent(out),allocatable,optional :: msg
logical,intent(in),optional                      :: verbose
integer                      :: isnumber

integer             :: i,iend
character(len=1),allocatable :: z(:)
character(len=:),allocatable :: message
logical                      :: founddigit
logical                      :: verbose_local

   i=1
   founddigit=.false.
   isnumber=0
   z=switch(trim(nospace(string)))
   iend=size(z)
   message='not a number'
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   DONE : block
      if(iend.eq.0)then
         isnumber=-1                   ! string is null
         message='null string'
         exit DONE
      endif

      if(index('+-',z(i)).ne.0) i=i+1  ! skip optional leading sign
      if(i.gt.iend)then
         isnumber=-2                   ! string was just a sign
         message='just a sign'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1

      if(i.gt.iend)then
         isnumber=1                    ! [+-]NNNNNN
         message='integer'
         exit DONE
      endif
      if(z(i).eq.'.')then              ! a period would be OK at this point
         i=i+1
      endif

      if(i.gt.iend)then                ! [+-]NNNNNN.
         isnumber=2
         message='whole number'
         exit DONE
      endif

      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=3                    ! [+-]NNNNNN.MMMM
         message='real number'
         exit DONE
      endif

      if(index('eEdD',z(i)).ne.0)then
         i=i+1
         if(i.eq.2)then
            isnumber=-6                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
            message='missing leading value before exponent'
            exit DONE
         endif
      endif
      if(i.gt.iend)then
         isnumber=-3                   ! [+-]NNNNNN[.[MMMM]]e but a value must follow
         message='missing exponent'
         exit DONE
      endif
      if(.not.founddigit)then
         isnumber=-7
         message='missing value before exponent'
         exit DONE
      endif
      if(index('+-',z(i)).ne.0) i=i+1
      if(i.gt.iend)then
         isnumber=-4                   ! [+-]NNNNNN[.[MMMM]]e[+-] but a value must follow
         message='missing exponent after sign'
         exit DONE
      endif
      call next()                      ! position I to next non-digit or end of string+1
      if(i.gt.iend)then
         isnumber=4                    ! [+-]NNNNNN.MMMMe[+-]LL
         message='value with exponent'
         exit DONE
      endif
      isnumber=-5
      message='bad number'
   endblock DONE
   if(verbose_local)then
      write(*,*)trim(string)//' is '//message
   endif
   if(present(msg))then
      msg=message
   endif

contains
   subroutine next() ! move to next non-digit or end of string+1
      integer :: j
      do j=i,iend
         if(.not.isdigit(z(j)))then
            exit
         endif
         founddigit=.true.
         if(verbose_local) write(*,*)'I=',i,' J=',j,' Z(j)=',z(j)
      enddo
      i=j
      if(verbose_local)then
         write(*,*)'I and J=',i
         if(i.le.iend) then
            write(*,*)'Z(I)=',z(i)
         else
            write(*,*)'====>'
         endif
      endif
   end subroutine next
end function isNumber
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isnumber
!!use M_strings, only: isnumber
implicit none
   call unit_check_start('isnumber',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('isnumber',isnumber(' 123 ')                                           .eq. 1,  'integer string')
   call unit_check('isnumber',isnumber(' -123. ')                                         .eq. 2,  'whole number string')
   call unit_check('isnumber',isnumber(' -123.0')                                         .eq. 3,  'real string')
   call unit_check('isnumber',isnumber(' -100.50')                                        .eq. 3,  'real string')
   call unit_check('isnumber',all( [isnumber('4.4e0 '),isnumber('1e1'),isnumber('-3D-4')] .eq. 4), 'exponent string')
   call unit_check('isnumber',isnumber(' Not a number')                                   .lt. 0,  'non-numeric string')
   call unit_check_done('isnumber')
end subroutine test_isnumber
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros(3fp) - [M_strings:NUMERIC] Delete trailing zeros from numeric decimal string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine trimzeros(str)
!!
!!     character(len=*)  :: str
!!##DESCRIPTION
!!    TRIMZEROS(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have trailing
!!          zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros
!!       use M_strings, only : trimzeros
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros('123.450000000000')
!!          write(*,*)trimzeros('12345')
!!          write(*,*)trimzeros('12345.')
!!          write(*,*)trimzeros('12345.00e3')
!!       end program demo_trimzeros
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine trimzeros(string)

character(len=*),parameter::ident_50="@(#)M_strings::trimzeros(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: exp          ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      exp=str(ipos:)                         ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(exp)
   else
      string=str
   endif
end subroutine trimzeros
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trimzeros()
   call unit_check_start('trimzeros',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check_done('trimzeros',msg='UNTESTED')
end subroutine test_trimzeros
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! listout(3f) - [M_strings:NUMERIC] expand a list of numbers where negative numbers denote range ends (1 -10 means 1 thru 10)
!! (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine listout(icurve_lists,icurve_expanded,inums,ierr)
!!
!!    integer,intent(in)    :: icurve_lists(:)
!!    integer,intent(out)   :: icurve_expanded(:)
!!    integer,intent(out)   :: inums
!!    integer,intent(out)   :: ierr
!!##DESCRIPTION
!!
!!##OPTIONS
!!    icurve_lists(:)      input array
!!
!!##RETURNS
!!    icurve_expanded(:)   output array; assumed large enough to hold returned list
!!    inums                number of icurve_expanded numbers on output
!!    ierr                 zero if no error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_listout
!!     use M_strings, only : listout
!!     implicit none
!!     integer,allocatable :: icurve_lists(:)
!!     integer :: icurve_expanded(1000)
!!     ! icurve_lists is input array
!!     integer :: inums
!!     ! icurve_expanded is output array
!!     integer :: i
!!     ! number of icurve_lists values on input, number of icurve_expanded numbers on output
!!     integer :: ierr
!!        icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
!!        inums=size(icurve_lists)
!!        call listout(icurve_lists,icurve_expanded,inums,ierr)
!!        if(ierr.eq.0)then
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        else
!!           write(*,'(a,i0)')'error occurred in *listout* ',ierr
!!           write(*,'(i0)')(icurve_expanded(i),i=1,inums)
!!        endif
!!     end program demo_listout
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine listout(icurve_lists,icurve_expanded,inums_out,ierr)
use M_journal, only : journal
implicit none

character(len=*),parameter::ident_51="&
&@(#)M_strings::listout(3f): copy icurve_lists to icurve_expanded expanding negative numbers to ranges (1 -10 means 1 thru 10)"

!   Created: 19971231
integer,intent(in)    :: icurve_lists(:)             ! input array
integer,intent(out)   :: icurve_expanded(:)          ! output array
integer,intent(out)   :: inums_out                   ! number of icurve_expanded numbers on output
integer,intent(out)   :: ierr                        ! status variable

character(len=80)     :: temp1
integer               :: i80, i90
integer               :: imin, imax
integer               :: idirection, icount
integer               :: iin
integer               :: inums_max

   ierr=0
   icurve_expanded=0                          ! initialize output array
   inums_out=0                                ! initialize number of significant values in output array

   inums_max=size(icurve_expanded)
   if(inums_max.eq.0)then
      ierr=-2
      return
   endif

   iin=size(icurve_lists)
   if(iin.gt.0)then
      icurve_expanded(1)=icurve_lists(1)
   endif

   icount=2
      do i90=2,iin
         if(icurve_lists(i90).lt.0)then
            imax=abs(icurve_lists(i90))
            imin=abs(icurve_lists(i90-1))
            if(imin.gt.imax)then
               idirection=-1
               imin=imin-1
            elseif(imax.gt.imin)then
               idirection=1
               imin=imin+1
            else
               idirection=1
            endif
            do i80=imin,imax,idirection
               if(icount.gt.inums_max) then
                  write(temp1,'(a,i5,a)')'*listout* only ',inums_max,' values allowed'
                  ierr=-1
                  call journal(temp1)
                  inums_out=icount-1
                  exit
               endif
               icurve_expanded(icount)=i80
               icount=icount+1
            enddo
         else
            icurve_expanded(icount)=icurve_lists(i90)
            icount=icount+1
         endif
      enddo
   inums_out=icount-1

end subroutine listout
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_listout()
integer,allocatable :: icurve_lists(:)        ! icurve_lists is input array
integer :: icurve_expanded(1000)  ! icurve_expanded is output array
integer :: inums                  ! number of icurve_lists values on input, number of icurve_expanded numbers on output
integer :: i
integer :: ierr
   call unit_check_start('listout',' &
      & -description ''copy ICURVE() to ICURVE_EXPANDED() expanding negative numbers to ranges (1-10 means 1 thru 10)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
   inums=size(icurve_lists)
   call listout(icurve_lists,icurve_expanded,inums,ierr)
   call unit_check('listout',ierr.eq.0,msg='check error status ierr='//v2s(ierr))
   call unit_check('listout',all(icurve_expanded(:inums).eq.[1,(i,i=20,30),101,100,99,(i,i=100,120),(i,i=222,200,-1)]),msg='expand')
   call unit_check_done('listout')
end subroutine test_listout
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_strings:QUOTES] add quotes to string as if written with list-directed input
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str         input string to add quotes to, using the rules of
!!                list-directed input (single quotes are replaced by two adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the string. If CLIP
!!                is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quote
!!    use M_strings, only : quote
!!    implicit none
!!    character(len=:),allocatable :: str
!!    character(len=1024)          :: msg
!!    integer                      :: ios
!!    character(len=80)            :: inline
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)inline
!!          if(ios.ne.0)then
!!             write(*,*)trim(inline)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!          ! the string processed by quote(3f)
!!          str=quote(inline)
!!          write(*,'(a)')'QUOTED     ['//str//']'
!!
!!          ! write the string list-directed to compare the results
!!          write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!          write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!          write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!       enddo
!!    end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode
!-----------------------------------------------------------------------------------------------------------------------------------
   local_mode=merge_str(mode,'DOUBLE',present(mode))
   if(merge(clip,.false.,present(clip)))then
      quoted_str=adjustl(str)
   else
      quoted_str=str
   endif
   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_quote()
integer                      :: i
integer,parameter            :: line_length=50
character(len=:),allocatable :: test_in(:)
character(len=:),allocatable :: test_out(:)

   test_in=[ character(len=line_length) ::      &
    'this is a test',                           &
    'test a "quote" around a string'     ]

   test_out=[ character(len=line_length) ::     &
    '"this is a test"',                         &
    '"test a ""quote"" around a string"' ]

   call unit_check_start('quote',' &
   & -section 3  &
   & -library libGPF  &
   & -filename `pwd`/M_strings.FF &
   & -documentation y &
   & -ufpp         y &
   & -ccall        n &
   & -archive      GPF.a &
   & ')

   do i=1,size(test_in)
      if(unit_check_level.gt.0)then
         write(*,'(a)')'ORIGINAL ['//test_in(i)//']'
         write(*,'(a)')'QUOTED   ['//quote(test_in(i))//']'
      endif
      call unit_check('quote',quote(test_in(i)).eq.test_out(i),msg=msg(quote(test_in(i)),'==>',trim(test_out(i))))
   enddo
   call unit_check_done('quote')
end subroutine test_quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_strings:QUOTES] remove quotes from string as if read with list-directed input
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes from quoted_str.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unquote
!!       use M_strings, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!    end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen.ge.1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1).eq.single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before.eq.iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current.eq.quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before.eq.quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before.ne.iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unquote()
integer,parameter            :: line_length=1024
character(len=line_length)   :: quoted_str
character(len=:),allocatable :: unquoted_str
character(len=1),parameter   :: esc='\'
character(len=line_length)   :: msg
character(len=line_length)   :: dummy
integer                      :: ios
integer                      :: i
character(len=:),allocatable :: tests(:)

   tests=[ character(len=line_length) :: &
      '"this is a test"',                         &
      '"test a ""quote"" around a string"' ]

   call unit_check_start('unquote',' &
   & -section 3  &
   & -library libGPF  &
   & -filename `pwd`/M_strings.FF &
   & -documentation y &
   & -ufpp         y &
   & -ccall        n &
   & -archive      GPF.a &
   & ')
   do i=1,size(tests)
      quoted_str=tests(i)
      unquoted_str=unquote(trim(quoted_str),esc)                    ! the string processed by unquote(3f)
      read(quoted_str,*,iostat=ios,iomsg=msg)dummy                  ! read the string list-directed to compare the results
      if(unit_check_level.gt.0)then
         write(*,'(a)')'QUOTED        ['//trim(quoted_str)//']'     ! the original string
         write(*,'(a)')'UNQUOTED      ['//unquoted_str//']'
         if(ios.ne.0)then
            write(*,*)trim(msg)
         else
            write(*,'(a)')'LIST DIRECTED ['//trim(dummy)//']'
         endif
      endif
      call unit_check('unquote',unquoted_str.eq.dummy,msg=trim(dummy)//'==>'//unquoted_str)
   enddo
   call unit_check_done('unquote')
end subroutine test_unquote
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    describe(3f) - [M_strings] returns a string describing the name of a single character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function describe(ch) result (string)
!!
!!     character(len=1),intent(in)   :: ch
!!     character(len=:),allocatable  :: string
!!##DESCRIPTION
!!    describe(3f) returns a string describing long name of a single character
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_describe
!!     use M_strings, only : describe
!!     implicit none
!!     integer :: i
!!        do i=1,128  ! fill variable with base ASCII character set
!!           write(*,*)describe(char(i-1))
!!        enddo
!!    end program demo_describe
!!
!!   Expected output
!!
!!     ctrl-@ or ctrl-? (NUL) null
!!     ctrl-A (SOH) start of heading
!!     ctrl-B (STX) start of text
!!     ctrl-C (ETX) end of text
!!     ctrl-D (EOT) end of transmission
!!     ctrl-E (ENQ) enquiry
!!     ctrl-F (ACK) acknowledge
!!     ctrl-G (BEL) bell
!!     ctrl-H (BS) backspace
!!     ctrl-I (HT) horizontal tabulation
!!     ctrl-J (LF) line feed
!!     ctrl-K (VT) vertical tabulation
!!     ctrl-L (FF) form feed
!!     ctrl-M (CR) carriage return
!!     ctrl-N (SO) shift out
!!     ctrl-O (SI) shift in
!!     ctrl-P (DLE) data link escape
!!     ctrl-Q (DC1) device control 1
!!     ctrl-R (DC2) device control 2
!!     ctrl-S (DC3) device control 3
!!     ctrl-T (DC4) device control 4
!!     ctrl-U (NAK) negative acknowledge
!!     ctrl-V (SYN) synchronous idle
!!     ctrl-W (ETB) end of transmission block
!!     ctrl-X (CAN) cancel
!!     ctrl-Y (EM) end of medium
!!     ctrl-Z (SUB) substitute
!!     ctrl-[ (ESC) escape
!!     ctrl-\ or ctrl-@ (FS) file separator
!!     ctrl-] (GS) group separator
!!     ctrl-^ or ctrl-= (RS) record separator
!!     ctrl-_ (US) unit separator
!!     space
!!     ! exclamation point
!!     " quotation marks
!!     # number sign
!!     $ currency symbol
!!     % percent
!!     & ampersand
!!     ' apostrophe
!!     ( left parenthesis
!!     ) right parenthesis
!!     * asterisk
!!     + plus
!!     , comma
!!     - minus
!!     . period
!!     / slash
!!     0 zero
!!     1 one
!!     2 two
!!     3 three
!!     4 four
!!     5 five
!!     6 six
!!     7 seven
!!     8 eight
!!     9 nine
!!     : colon
!!     ; semicolon
!!     < less than
!!     = equals
!!     > greater than
!!     ? question mark
!!     @ at sign
!!     majuscule A
!!     majuscule B
!!     majuscule C
!!     majuscule D
!!     majuscule E
!!     majuscule F
!!     majuscule G
!!     majuscule H
!!     majuscule I
!!     majuscule J
!!     majuscule K
!!     majuscule L
!!     majuscule M
!!     majuscule N
!!     majuscule O
!!     majuscule P
!!     majuscule Q
!!     majuscule R
!!     majuscule S
!!     majuscule T
!!     majuscule U
!!     majuscule V
!!     majuscule W
!!     majuscule X
!!     majuscule Y
!!     majuscule Z
!!     [ left bracket
!!     \ backslash
!!     ] right bracket
!!     ^ caret
!!     _ underscore
!!     ` grave accent
!!     miniscule a
!!     miniscule b
!!     miniscule c
!!     miniscule d
!!     miniscule e
!!     miniscule f
!!     miniscule g
!!     miniscule h
!!     miniscule i
!!     miniscule j
!!     miniscule k
!!     miniscule l
!!     miniscule m
!!     miniscule n
!!     miniscule o
!!     miniscule p
!!     miniscule q
!!     miniscule r
!!     miniscule s
!!     miniscule t
!!     miniscule u
!!     miniscule v
!!     miniscule w
!!     miniscule x
!!     miniscule y
!!     miniscule z
!!     { left brace
!!     | vertical line
!!     } right brace
!!     ~ tilde
!!     ctrl-? (DEL) delete
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function describe(ch) result (string)

character(len=*),parameter::ident_52="@(#)M_strings::describe(3f): return string describing long name of a single character"

character(len=1),intent(in)   :: ch
character(len=:),allocatable  :: string
! LATER: add hex, octal, decimal, key-press description, alternate names
!  ASCII character codes
   select case (ichar(ch))
   case(     0  ); STRING="ctrl-@ or ctrl-? (NUL) null"
   case(     1  ); STRING="ctrl-A (SOH) start of heading"
   case(     2  ); STRING="ctrl-B (STX) start of text"
   case(     3  ); STRING="ctrl-C (ETX) end of text"
   case(     4  ); STRING="ctrl-D (EOT) end of transmission"
   case(     5  ); STRING="ctrl-E (ENQ) enquiry"
   case(     6  ); STRING="ctrl-F (ACK) acknowledge"
   case(     7  ); STRING="ctrl-G (BEL) bell"
   case(     8  ); STRING="ctrl-H (BS) backspace"
   case(     9  ); STRING="ctrl-I (HT) horizontal tabulation"
   case(    10  ); STRING="ctrl-J (LF) line feed"
   case(    11  ); STRING="ctrl-K (VT) vertical tabulation"
   case(    12  ); STRING="ctrl-L (FF) form feed"
   case(    13  ); STRING="ctrl-M (CR) carriage return"
   case(    14  ); STRING="ctrl-N (SO) shift out"
   case(    15  ); STRING="ctrl-O (SI) shift in"
   case(    16  ); STRING="ctrl-P (DLE) data link escape"
   case(    17  ); STRING="ctrl-Q (DC1) device control 1"
   case(    18  ); STRING="ctrl-R (DC2) device control 2"
   case(    19  ); STRING="ctrl-S (DC3) device control 3"
   case(    20  ); STRING="ctrl-T (DC4) device control 4"
   case(    21  ); STRING="ctrl-U (NAK) negative acknowledge"
   case(    22  ); STRING="ctrl-V (SYN) synchronous idle"
   case(    23  ); STRING="ctrl-W (ETB) end of transmission block"
   case(    24  ); STRING="ctrl-X (CAN) cancel"
   case(    25  ); STRING="ctrl-Y (EM) end of medium"
   case(    26  ); STRING="ctrl-Z (SUB) substitute"
   case(    27  ); STRING="ctrl-[ (ESC) escape"
   case(    28  ); STRING="ctrl-\ or ctrl-@ (FS) file separator"
   case(    29  ); STRING="ctrl-] (GS) group separator"
   case(    30  ); STRING="ctrl-^ or ctrl-= (RS) record separator"
   case(    31  ); STRING="ctrl-_ (US) unit separator"
   case(    32  ); STRING="space"
   case(    33  ); STRING="! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)"
   case(    34  ); STRING=""" quotation marks"
   case(    35  ); STRING="# number sign (hash, pound sign, hashtag)"
   case(    36  ); STRING="$ currency symbol"
   case(    37  ); STRING="% percent"
   case(    38  ); STRING="& ampersand"
   case(    39  ); STRING="' apostrophe"
   case(    40  ); STRING="( left parenthesis"
   case(    41  ); STRING=") right parenthesis"
   case(    42  ); STRING="* asterisk"
   case(    43  ); STRING="+ plus"
   case(    44  ); STRING=", comma"
   case(    45  ); STRING="- minus"
   case(    46  ); STRING=". period"
   case(    47  ); STRING="/ slash"
   case(    48  ); STRING="0 zero"
   case(    49  ); STRING="1 one"
   case(    50  ); STRING="2 two"
   case(    51  ); STRING="3 three"
   case(    52  ); STRING="4 four"
   case(    53  ); STRING="5 five"
   case(    54  ); STRING="6 six"
   case(    55  ); STRING="7 seven"
   case(    56  ); STRING="8 eight"
   case(    57  ); STRING="9 nine"
   case(    58  ); STRING=": colon"
   case(    59  ); STRING="; semicolon"
   case(    60  ); STRING="< less than"
   case(    61  ); STRING="= equals"
   case(    62  ); STRING="> greater than"
   case(    63  ); STRING="? question mark"
   case(    64  ); STRING="@ at sign"
   case(    65  ); STRING="A majuscule A"
   case(    66  ); STRING="B majuscule B"
   case(    67  ); STRING="C majuscule C"
   case(    68  ); STRING="D majuscule D"
   case(    69  ); STRING="E majuscule E"
   case(    70  ); STRING="F majuscule F"
   case(    71  ); STRING="G majuscule G"
   case(    72  ); STRING="H majuscule H"
   case(    73  ); STRING="I majuscule I"
   case(    74  ); STRING="J majuscule J"
   case(    75  ); STRING="K majuscule K"
   case(    76  ); STRING="L majuscule L"
   case(    77  ); STRING="M majuscule M"
   case(    78  ); STRING="N majuscule N"
   case(    79  ); STRING="O majuscule O"
   case(    80  ); STRING="P majuscule P"
   case(    81  ); STRING="Q majuscule Q"
   case(    82  ); STRING="R majuscule R"
   case(    83  ); STRING="S majuscule S"
   case(    84  ); STRING="T majuscule T"
   case(    85  ); STRING="U majuscule U"
   case(    86  ); STRING="V majuscule V"
   case(    87  ); STRING="W majuscule W"
   case(    88  ); STRING="X majuscule X"
   case(    89  ); STRING="Y majuscule Y"
   case(    90  ); STRING="Z majuscule Z"
   case(    91  ); STRING="[ left bracket"
   case(    92  ); STRING="\ backslash"
   case(    93  ); STRING="] right bracket"
   case(    94  ); STRING="^ caret"
   case(    95  ); STRING="_ underscore"
   case(    96  ); STRING="` grave accent"
   case(    97  ); STRING="a miniscule a"
   case(    98  ); STRING="b miniscule b"
   case(    99  ); STRING="c miniscule c"
   case(   100  ); STRING="d miniscule d"
   case(   101  ); STRING="e miniscule e"
   case(   102  ); STRING="f miniscule f"
   case(   103  ); STRING="g miniscule g"
   case(   104  ); STRING="h miniscule h"
   case(   105  ); STRING="i miniscule i"
   case(   106  ); STRING="j miniscule j"
   case(   107  ); STRING="k miniscule k"
   case(   108  ); STRING="l miniscule l"
   case(   109  ); STRING="m miniscule m"
   case(   110  ); STRING="n miniscule n"
   case(   111  ); STRING="o miniscule o"
   case(   112  ); STRING="p miniscule p"
   case(   113  ); STRING="q miniscule q"
   case(   114  ); STRING="r miniscule r"
   case(   115  ); STRING="s miniscule s"
   case(   116  ); STRING="t miniscule t"
   case(   117  ); STRING="u miniscule u"
   case(   118  ); STRING="v miniscule v"
   case(   119  ); STRING="w miniscule w"
   case(   120  ); STRING="x miniscule x"
   case(   121  ); STRING="y miniscule y"
   case(   122  ); STRING="z miniscule z"
   case(   123  ); STRING="{ left brace"
   case(   124  ); STRING="| vertical line"
   case(   125  ); STRING="} right brace"
   case(   126  ); STRING="~ tilde"
   case(   127  ); STRING="ctrl-? (DEL) delete"
   case default
         STRING='UNKNOWN'//v2s(ICHAR(ch))
   end select
end function describe
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_describe
!!use M_strings, only: describe
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
! initialize database description of routine
   call unit_check_start('describe',' &
      & -description ''returns a string describing character'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

! call all descriptions to exercise procedure
if(unit_check_level.gt.0)then
   do i=0,number_of_chars-1
      write(*,*)i,char(i),' ',describe(char(i))
   enddo
endif

! unit tests
call unit_check('describe', describe(char( 23) ) .eq.  'ctrl-W (ETB) end of transmission block' , 'describe ctrl-W')
call unit_check('describe', &
   describe(char( 33) ) .eq.  '! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)' , &
   'describe exclamation point')
call unit_check('describe', describe(char( 52) ) .eq.  '4 four'                                 , 'describe four')
call unit_check('describe', describe(char( 63) ) .eq.  '? question mark'                        , 'describe question mark')
call unit_check('describe', describe(char( 64) ) .eq.  '@ at sign'                              , 'describe at sign')
call unit_check('describe', describe(char( 74) ) .eq.  'J majuscule J'                          , 'describe J')
call unit_check('describe', describe(char( 117)) .eq.  'u miniscule u'                          , 'describe u')
call unit_check('describe', describe(char( 126)) .eq.  '~ tilde'                                , 'describe tilde')
call unit_check_done('describe')

end subroutine test_describe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getvals(3f) - [M_strings:NUMERIC] read arbitrary number of REAL values from a character variable up to size of VALUES() array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine getvals(line,values,icount,ierr)
!!
!!     character(len=*),intent(in)  :: line
!!     class(*),intent(out)         :: values(:)
!!     integer,intent(out)          :: icount
!!     integer,intent(out),optional :: ierr
!!##DESCRIPTION
!!
!!   GETVALS(3f) reads a relatively arbitrary number of numeric values from
!!   a character variable into a REAL array using list-directed input.
!!
!!   NOTE: In this version null values are skipped instead of meaning to leave
!!         that value unchanged
!!
!!        1,,,,,,,2 / reads VALUES=[1.0,2.0]
!!
!!   Per list-directed rules when reading values, allowed delimiters are
!!   comma, semi-colon and space.
!!
!!   the slash separator can be used to add inline comments.
!!
!!        10.1, 20.43e-1 ; 11 / THIS IS TREATED AS A COMMENT
!!
!!   Repeat syntax can be used up to the size of the output array. These are
!!   equivalent input lines:
!!
!!        4*10.0
!!        10.0, 10.0, 10.0, 10.0
!!
!!##OPTIONS
!!
!!   LINE      A character variable containing the characters representing
!!             a list of numbers
!!
!!##RETURNS
!!
!!   VALUES()  array holding numbers read from string. May be of type
!!             INTEGER, REAL, DOUBLEPRECISION, or CHARACTER. If CHARACTER the
!!             strings are returned as simple words instead of numeric values.
!!   ICOUNT    number of defined numbers in VALUES(). If ICOUNT reaches
!!             the size of the VALUES() array parsing stops.
!!   IERR      zero if no error occurred in reading numbers. Optional.
!!             If not present and an error occurs the program is terminated.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_getvals
!!       use M_strings, only: getvals
!!       implicit none
!!       integer,parameter  :: longest_line=256
!!       character(len=longest_line) :: line
!!       real               :: values(longest_line/2+1)
!!       integer            :: ios,icount,ierr
!!       INFINITE: do
!!          read(*,'(a)',iostat=ios) line
!!          if(ios.ne.0)exit INFINITE
!!          call getvals(line,values,icount,ierr)
!!          write(*,*)'VALUES=',values(:icount)
!!       enddo INFINITE
!!       end program demo_getvals
!!
!!   Sample input lines
!!
!!        10,20 30.4
!!        1 2 3
!!        1
!!
!!        3 4*2.5 8
!!        32.3333 / comment 1
!!        30e3;300,    30.0, 3
!!        even 1 like this! 10
!!        11,,,,22,,,,33
!!
!!   Expected output:
!!
!!       VALUES=   10.0000000       20.0000000       30.3999996
!!       VALUES=   1.00000000       2.00000000       3.00000000
!!       VALUES=   1.00000000
!!       VALUES=
!!       VALUES=   3.00000000       2.50000000       2.50000000       2.50000000       2.50000000       8.00000000
!!       VALUES=   32.3333015
!!       VALUES=   30000.0000       300.000000       30.0000000       3.00000000
!!       *getvals* WARNING:[even] is not a number
!!       *getvals* WARNING:[like] is not a number
!!       *getvals* WARNING:[this!] is not a number
!!       VALUES=   1.00000000       10.0000000
!!       VALUES=   11.0000000       22.0000000       33.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine getvals(line,values,icount,ierr)
implicit none

character(len=*),parameter::ident_53="@(#)M_strings::getvals(3f): read arbitrary number of values from a character variable"

! JSU 20170831

character(len=*),intent(in)  :: line
class(*),intent(out)         :: values(:)
integer,intent(out)          :: icount
integer,intent(out),optional :: ierr

character(len=:),allocatable :: buffer
character(len=len(line))     :: words(size(values))
integer                      :: ios, i, ierr_local,isize

   isize=0
   select type(values)
   type is (integer);          isize=size(values)
   type is (real);             isize=size(values)
   type is (doubleprecision);  isize=size(values)
   type is (character(len=*)); isize=size(values)
   end select

   ierr_local=0

   words=' '                            ! make sure words() is initialized to null+blanks
   buffer=trim(line)//"/"               ! add a slash to the end so how the read behaves with missing values is clearly defined
   read(buffer,*,iostat=ios) words      ! undelimited strings are read into an array
   icount=0
   do i=1,isize                         ! loop thru array and convert non-blank words to numbers
      if(words(i).eq.' ')cycle

      select type(values)
      type is (integer);          read(words(i),*,iostat=ios)values(icount+1)
      type is (real);             read(words(i),*,iostat=ios)values(icount+1)
      type is (doubleprecision);  read(words(i),*,iostat=ios)values(icount+1)
      type is (character(len=*)); values(icount+1)=words(i)
      end select

      if(ios.eq.0)then
         icount=icount+1
      else
         ierr_local=ios
         write(ERROR_UNIT,*)'*getvals* WARNING:['//trim(words(i))//'] is not a number of specified type'
      endif
   enddo

   if(present(ierr))then
      ierr=ierr_local
   elseif(ierr_local.ne.0)then        ! error occurred and not returning error to main program to print message and stop program
      write(ERROR_UNIT,*)'*getval* error reading line ['//trim(line)//']'
      stop 2
   endif

end subroutine getvals
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getvals()
integer,parameter  :: longest_line=256
real               :: rvalues(longest_line/2+1)
integer            :: ivalues(longest_line/2+1)
doubleprecision    :: dvalues(longest_line/2+1)
integer            :: icount,ierr
   call unit_check_start('getvals',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   call getvals('11,,,22,33,-44, 55 , ,66  ',ivalues,icount,ierr)
   call unit_check('getvals',all(ivalues(:icount).eq.[11,22,33,-44,55,66]),msg='integer test')

   call getvals('1234.56 3.3333, 5.5555',rvalues,icount,ierr)
   call unit_check('getvals',all(rvalues(:icount).eq.[1234.56,3.3333,5.5555]),msg='real test')

   call getvals('1234.56d0 3.3333d0, 5.5555d0',dvalues,icount,ierr)
   if(unit_check_level.gt.0)then
      write(*,*)dvalues(:icount)
      write(*,*)[1234.56d0,3.3333d0,5.5555d0]
   endif
   call unit_check('getvals',all(dvalues(:icount).eq.[1234.56d0,3.3333d0,5.5555d0]),msg='double test')

   call unit_check_done('getvals')
end subroutine test_getvals
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      string_to_values(3f) - [M_strings:NUMERIC] read a string representing numbers into a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine string_to_values(line,iread,values,inums,delims,ierr)
!!
!!        character(len=*) :: line
!!        integer          :: iread
!!        real             :: values(*)
!!        integer          :: inums
!!        character(len=*) :: delims
!!        integer          :: ierr
!!##DESCRIPTION
!!    This routine can take a string representing a series of numbers and
!!    convert it to a numeric array and return how many numbers were found.
!!
!!##OPTIONS
!!
!!       LINE     Input string containing numbers
!!       IREAD    maximum number of values to try to read from input string
!!
!!##RESULTS
!!
!!       VALUES   real array to be filled with numbers
!!       INUMS    number of values successfully read (before error occurs
!!                if one does)
!!       DELIMS   delimiter character(s), usually a space. must not be a
!!                null string. If more than one character, a space must
!!                not be the last character or it will be ignored.
!!       IERR     error flag (0=no error, else column number string starts
!!                at that error occurred on).
!!
!!##EXAMPLE
!!
!!    Sample Program:
!!
!!      program demo_string_to_values
!!       use M_strings, only : string_to_values
!!       character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!       integer,parameter  :: isz=10
!!       real               :: array(isz)
!!
!!       call string_to_values(s,10,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
!!       call reportit()
!!
!!       contains
!!          subroutine reportit()
!!             write(*,*)'string_to_values:'
!!             write(*,*)'input string.............',trim(s)
!!             write(*,*)'number of values found...',inums
!!             write(*,*)'values...................',(array(ii),ii=1,inums)
!!          end subroutine reportit
!!      end program demo_string_to_values
!!
!!    Expected output
!!
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           6
!!     values...................   10.0000000  20000.0000  3.45000005  -4.00299978  1234.00000  5678.00000
!!     string_to_values:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found...           3
!!     values...................   10.0000000  2.29999995  3.14159989
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine string_to_values(line,iread,values,inums,delims,ierr)
use M_journal, only : journal
implicit none
!----------------------------------------------------------------------------------------------------------------------------------
!   1989,1997-12-31,2014 John S. Urban

!   given a line of structure , string , string , string process each
!   string as a numeric value and store into an array.
!   DELIMS contain the legal delimiters. If a space is an allowed delimiter, it must not appear last in DELIMS.
!   There is no direct checking for more values than can fit in VALUES.
!   Quits if encounters any errors in read.
!----------------------------------------------------------------------------------------------------------------------------------

character(len=*),parameter::ident_54="@(#)M_strings::string_to_values(3f): reads an array of numbers from a numeric string"

character(len=*),intent(in)  :: line          ! input string
integer,intent(in)           :: iread         ! maximum number of values to try to read into values
real,intent(inout)           :: values(iread) ! real array to be filled with values
integer,intent(out)          :: inums         ! number of values successfully read from string
character(len=*),intent(in)  :: delims        ! allowed delimiters
integer,intent(out)          :: ierr          ! 0 if no error, else column number undecipherable string starts at
!----------------------------------------------------------------------------------------------------------------------------------
character(len=256)           :: delims_local        ! mutable copy of allowed delimiters
integer                      :: istart,iend,ilen,icol
integer                      :: i10,i20,i40
real                         :: rval
integer                      :: ier
integer                      :: delimiters_length
!----------------------------------------------------------------------------------------------------------------------------------
      delims_local=delims                                 ! need a mutable copy of the delimiter list
      if(delims_local.eq.'')then                          ! if delimiter list is null or all spaces make it a space
         delims_local=' '                                 ! delimiter is a single space
         delimiters_length=1                        ! length of delimiter list
      else
         delimiters_length=len_trim(delims)         ! length of variable WITH TRAILING WHITESPACE TRIMMED
      endif
!----------------------------------------------------------------------------------------------------------------------------------
      ierr=0                                        ! initialize error code returned
      inums=0                                       ! initialize count of values successfully returned
      istart=0
!----------------------------------------------------------------------------------------------------------------------------------
      ilen=0                                        ! ilen will be the position of the right-most non-delimiter in the input line
      do i20=len(line),1,-1                         ! loop from end of string to beginning to find right-most non-delimiter
         if(index(delims_local(:delimiters_length),line(i20:i20)).eq.0)then   ! found a non-delimiter
            ilen=i20
            exit
         endif
      enddo
      if(ilen.eq.0)then                             ! command was totally composed of delimiters
         call journal('*string_to_values* blank line passed as a list of numbers')
         return
      endif
!----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter sub-string
!     ilen is the column position of the last non-delimiter character
!     now, starting at beginning of string find next non-delimiter
      icol=1                                                     ! pointer to beginning of unprocessed part of LINE
      LOOP: dO i10=1,iread,1                                     ! each pass should find a value
         if(icol.gt.ilen) EXIT LOOP                              ! everything is done
         INFINITE: do
            if(index(delims_local(:delimiters_length),line(icol:icol)).eq.0)then           ! found non-delimiter
               istart=icol
               iend=0                                            ! FIND END OF SUBSTRING
               do i40=istart,ilen                                ! look at each character starting at left
                  if(index(delims_local(:delimiters_length),line(i40:i40)).ne.0)then       ! determine if character is a delimiter
                     iend=i40                                    ! found a delimiter. record where it was found
                     EXIT                                        ! found end of substring so leave loop
                  endif
               enddo
              if(iend.eq.0)iend=ilen+1                           ! no delimiters found, so this substring goes to end of line
               iend=iend-1                                       ! do not want to pass delimiter to be converted
               rval=0.0
               call string_to_value(line(istart:iend),rval,ier)  ! call procedure to convert string to a numeric value
               if(ier.eq.0)then                                  ! a substring was successfully converted to a numeric value
                  values(i10)=rval                               ! store numeric value in return array
                  inums=inums+1                                  ! increment number of values converted to a numeric value
               else                                              ! an error occurred converting string to value
                  ierr=istart                                    ! return starting position of substring that could not be converted
                  return
               endif
               icol=iend+2                                       ! set to next character to look at
               CYCLE LOOP                                        ! start looking for next value
            else                                                 ! this is a delimiter so keep looking for start of next string
               icol=icol+1                                       ! increment pointer into LINE
               CYCLE INFINITE
            endif
         enddo INFINITE
      enddo LOOP
!     error >>>>> more than iread numbers were in the line.
end subroutine string_to_values
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_values()
character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
integer,parameter  :: isz=10
real               :: array(isz)
integer            :: ierr
integer            :: inums
   call unit_check_start('string_to_values',' &
      & -description ''subroutine returns values from a string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   call string_to_values(s,10,array,inums,' ;',ierr)
   call unit_check('string_to_values',all(array(:inums).eq.[10.0,20e3,3.45,-400.3e-2,1234.0,5678.0]),msg=msg(s))
   call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
   call unit_check('string_to_values',all(array(:inums).eq.[10.0,2.3,3.1416]),msg=msg(array(1),array(2),array(3)))
   call unit_check('string_to_values',inums.eq.3,msg=msg('number of values is',inums))
   call unit_check_done('string_to_values',msg='')
end subroutine test_string_to_values
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      s2vs(3f) - [M_strings:NUMERIC] given a string representing numbers return a numeric array
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       function s2vs(line[,delim])
!!
!!        character(len=*) :: line
!!        doubleprecision,allocatable :: s2vs(:)
!!##DESCRIPTION
!!
!!    The function S2VS(3f) takes a string representing a series of numbers
!!    and converts it to a numeric doubleprecision array. The string values
!!    may be delimited by spaces, semi-colons, and commas by default.
!!
!!##OPTIONS
!!       LINE   Input string containing numbers
!!       DELIM  optional list of delimiter characters. If a space is
!!              included, it should appear as the left-most character
!!              in the list. The default is " ;," (spaces, semi-colons,
!!              and commas).
!!##RESULTS
!!       S2VS   doubleprecision array
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!      program demo_s2vs
!!      use M_strings, only : s2vs
!!      character(len=80)           :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
!!      doubleprecision,allocatable :: values(:)
!!      integer,allocatable         :: ivalues(:)
!!
!!      values=s2vs(s)
!!      ivalues=int(s2vs(s))
!!      call reportit()
!!
!!      contains
!!        subroutine reportit()
!!          write(*,*)'S2VS:'
!!          write(*,*)'input string.............',trim(s)
!!          write(*,*)'number of values found...',size(values)
!!          write(*,*)'values...................',(values(ii),ii=1,size(values))
!!          write(*,*)'ivalues..................',(ivalues(ii),ii=1,size(values))
!!        end subroutine reportit
!!      end program demo_s2vs
!!
!!    Expected output
!!
!!     S2VS:
!!     input string............. 10 20e3;3.45 -400.3e-2;1234; 5678
!!     number of values found... 6
!!     values................... 10.000000000000000  20000.000000000000 3.4500000000000002
!!     -4.0030000000000001       1234.0000000000000  5678.0000000000000
!!     ivalues.................. 10  20000  3  -4 1234 5678
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function s2vs(string,delim) result(darray)

character(len=*),parameter::ident_55="@(#)M_strings::s2vs(3f): function returns array of values from a string"

character(len=*),intent(in)        :: string                       ! keyword to retrieve value for from dictionary
character(len=*),optional          :: delim                        ! delimiter characters
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)                    ! function type

character(len=:),allocatable       :: carray(:)                    ! convert value to an array using split(3f)
integer                            :: i
integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(string,carray,delimiters=delim_local)         ! split string into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function s2vs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2vs()
character(len=80)           :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
doubleprecision,allocatable :: values(:)
integer,allocatable         :: ivalues(:)
   call unit_check_start('s2vs',' &
      & -description ''function returns a doubleprecision array of numbers from a string'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   values=s2vs(s)
   ivalues=int(s2vs(s))
   call unit_check('s2vs',size(values).eq.6, msg='number of values')
   call unit_check('s2vs',all(ivalues.eq.[10, 20000, 3, -4,1234,5678]))
   call unit_check_done('s2vs')
end subroutine test_s2vs

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isprint(onechar)

character(len=*),parameter::ident_56="@(#)M_strings::isprint(3f): indicates if input character is a printable ASCII character"

character,intent(in) :: onechar
logical              :: isprint
   select case (onechar)
      case (' ':'~')   ; isprint=.TRUE.
      case default     ; isprint=.FALSE.
   end select
end function isprint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isprint
!!use M_strings, only: isprint
implicit none
integer :: i
   call unit_check_start('isprint',' &
      & -description ''elemental function determines if CHR is an ASCII printable character'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=1,255
      SELECT CASE (i)
      CASE (32:126)
         if (isprint(char(i)) .eqv. .false.)then
            write(*,*)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 2
         endif
      CASE DEFAULT
         if (isprint(char(i)) .eqv. .true.)then
            write(*,*)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 3
         endif
      END SELECT
   enddo
call unit_check_good('isprint')
end subroutine test_isprint
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    msg(3f) - [M_strings] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function msg(g1,g2g3,g4,g5,g6,g7,g8,g9,nospace)
!!
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     logical,intent(in),optional   :: nospace
!!     character,len=(:),allocatable :: msg
!!
!!##DESCRIPTION
!!    msg(3f) builds a space-separated string from up to nine scalar values.
!!
!!##OPTIONS
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!    nospace  if nospace=.true., then no spaces are added between values
!!##RETURNS
!!    msg     description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_msg
!!    use M_strings, only : msg
!!    implicit none
!!    character(len=:),allocatable :: pr
!!    character(len=:),allocatable :: frmt
!!    integer                      :: biggest
!!
!!    pr=msg('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    write(*,'(a)')pr
!!    pr=msg('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    write(*,'(a)')pr
!!    pr=msg('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    write(*,'(a)')pr
!!    pr=msg('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    write(*,'(a)')pr
!!
!!    ! create a format on the fly
!!    biggest=huge(0)
!!    frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',nospace=.true.)
!!    write(*,*)'format=',frmt
!!
!!    ! although it will often work, using msg(3f) in an I/O statement is not recommended
!!    write(*,*)msg('program will now stop')
!!
!!    end program demo_msg
!!
!!  Output
!!
!!    HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!    real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!    doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!    complex         : (3.40282347E+38,1.17549435E-38)
!!     format=(*(i9:,1x))
!!     program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,nospace)
implicit none

character(len=*),parameter::ident_57="&
&@(#)M_strings::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
logical,intent(in),optional   :: nospace
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(nospace))then
      if(nospace)then
         increment=1
      else
         increment=2
      endif
   else
      increment=2
   endif

   istart=1
   line=' '
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
integer             :: iblanks
   iblanks=0
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(1l)') generic
      type is (character(len=*));
                                        if(generic.eq.' ')then ! keep totally blank strings
                                           write(line(istart:),'(a)') generic
                                           iblanks=len(generic)
                                        else
                                           write(line(istart:),'(a)') trim(generic)
                                        endif
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment+iblanks
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,nospace)
implicit none

character(len=*),parameter::ident_58="&
&@(#)M_strings::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
logical,intent(in),optional   :: nospace
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(nospace))then
      if(nospace)then
         increment=1
      else
         increment=2
      endif
   else
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (logical);                write(line(istart:),'("[",*(1l,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   line=trim(line)//"]"
   istart=len_trim(line)+increment
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isgraph(onechar)

character(len=*),parameter::ident_59="&
&@(#)M_strings::isgraph(3f) :indicates if character is printable ASCII character excluding space"

character,intent(in) :: onechar
logical              :: isgraph
   select case (iachar(onechar))
   case (33:126)
     isgraph=.TRUE.
   case default
     isgraph=.FALSE.
   end select
end function isgraph
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isgraph
!!use M_strings, only: isgraph
implicit none
integer :: i
   call unit_check_start('isgraph',' &
      & -description ''elemental function true if CHR is an ASCII printable character except considers a space non-printable'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=1,255
      SELECT CASE (i)
      CASE (33:126)
         if (isgraph(char(i)) .eqv. .false.)then
            write(*,*)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 2
         endif
      CASE DEFAULT
         if (isgraph(char(i)) .eqv. .true.)then
            write(*,*)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 3
         endif
      END SELECT
   enddo
   call unit_check_good('isgraph')
end subroutine test_isgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isalpha(ch) result(res)

character(len=*),parameter::ident_60="@(#)M_strings::isalpha(3f): Return .true. if character is a letter and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z','a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function isalpha
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalpha
!!use M_strings, only: isalpha
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_check_start('isalpha',' &
      & -description ''elemental function returns .true. if CHR is a letter and .false. otherwise'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z')
         if (isalpha(ch) .eqv. .false.)then
            write(*,*)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 1
         endif
      CASE DEFAULT
         if (isalpha(ch) .eqv. .true.)then
            write(*,*)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalpha')
end subroutine test_isalpha
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isxdigit(ch) result(res)

character(len=*),parameter::ident_61="@(#)M_strings::isxdigit(3f): returns .true. if c is a hexadecimal digit (0-9,a-f, or A-F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'F','a':'f','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isxdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isxdigit
!!use M_strings, only: isxdigit
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isxdigit',' &
      & -description ''elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'f','A':'F','0':'9')
         if (isxdigit(char(i)) .eqv. .false.)then
            write(*,*)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isxdigit(char(i)) .eqv. .true.)then
            write(*,*)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isxdigit')
end subroutine test_isxdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isdigit(ch) result(res)

character(len=*),parameter::ident_62="@(#)M_strings::isdigit(3f): Returns .true. if ch is a digit (0-9) and .false. otherwise"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isdigit
!!use M_strings, only: isdigit
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isdigit',' &
      & -description ''elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (48:57)
         if (isdigit(char(i)) .eqv. .false.)then
            write(*,*)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isdigit(char(i)) .eqv. .true.)then
            write(*,*)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isdigit')
end subroutine test_isdigit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isblank(ch) result(res)

character(len=*),parameter::ident_63="@(#)M_strings::isblank(3f): returns .true. if character is a blank (space or horizontal tab)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ',char(9))
     res=.true.
   case default
     res=.false.
   end select
end function isblank
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isblank
!!use M_strings, only: isblank
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isblank',' &
      & -description ''elemental function returns .true. if CHR is a blank character (space or horizontal tab.'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      select case (i)
      case (9,32)
         if (isblank(char(i)) .eqv. .false.)then
            write(*,*)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 1
         endif
      case default
         if (isblank(char(i)) .eqv. .true.)then
            write(*,*)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 2
         endif
      end select
   enddo
   call unit_check_good('isblank')
end subroutine test_isblank
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isascii(ch) result(res)

character(len=*),parameter::ident_64="@(#)M_strings::isascii(3f): returns .true. if character is in the range char(0) to char(127)"

character,intent(in) :: ch
logical              :: res
   select case(ichar(ch))
   case(0:127)
     res=.true.
   case default
     res=.false.
   end select
end function isascii
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isascii
!!use M_strings, only: isascii
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isascii',' &
      & -description ''elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0:127)
         if (isascii(char(i)) .eqv. .false.)then
            write(*,*)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 1
         endif
      CASE DEFAULT
         if (isascii(char(i)) .eqv. .true.)then
            write(*,*)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isascii')
end subroutine test_isascii
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function isspace(ch) result(res)

character(len=*),parameter::ident_65="@(#)M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isspace
!!use M_strings, only: isspace
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isspace',' &
      & -description ''elemental function true if CHR is null, space, tab, carriage return, new line, vertical tab, or formfeed'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0,9:13,32)
         if (isspace(char(i)) .eqv. .false.)then
            write(*,*)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 1
         endif
      CASE DEFAULT
         if (isspace(char(i)) .eqv. .true.)then
            write(*,*)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isspace')
end subroutine test_isspace
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function iscntrl(ch) result(res)

character(len=*),parameter::ident_66="@(#)M_strings::iscntrl(3f): true if a delete or ordinary control character(0x7F or 0x00-0x1F)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(char(127),char(0):char(31))
     res=.true.
   case default
     res=.false.
   end select
end function iscntrl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_iscntrl
!!use M_strings, only: iscntrl
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('iscntrl',' &
      & -description ''elemental function returns .true. if CHR is a delete character or ordinary control character'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0:31,127)
         if (iscntrl(char(i)) .eqv. .false.)then
            write(*,*)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 1
         endif
      CASE DEFAULT
         if (iscntrl(char(i)) .eqv. .true.)then
            write(*,*)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('iscntrl')
end subroutine test_iscntrl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function ispunct(ch) result(res)

character(len=*),parameter::ident_67="@(#)M_strings::ispunct(3f): true if a printable punctuation character (isgraph(c)&&"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case (char(33):char(47), char(58):char(64), char(91):char(96), char(123):char(126))
     res=.true.
!  case(' ','0':'9','A':'Z','a':'z',char(128):)
!    res=.true.
!  case(char(0):char(31),char(127))
!    res=.true.
   case default
     res=.false.
   end select
end function ispunct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ispunct
!!use M_strings, only: ispunct
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('ispunct',' &
      & -description ''elemental function returns .true. if CHR is a printable punctuation character'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (33:47, 58:64, 91:96, 123:126)
         if (ispunct(char(i)) .eqv. .false.)then
            write(*,*)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 1
         endif
      CASE DEFAULT
         if (ispunct(char(i)) .eqv. .true.)then
            write(*,*)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('ispunct')
end subroutine test_ispunct
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function isupper(ch) result(res)

character(len=*),parameter::ident_68="@(#)M_strings::isupper(3f): returns true if character is an uppercase letter (A-Z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('A':'Z')
     res=.true.
   case default
     res=.false.
   end select
end function isupper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isupper
!!use M_strings, only: isupper
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isupper',' &
      & -description ''elemental function returns .true. if CHR is an uppercase letter (A-Z)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('A':'Z')
         if (isupper(ch) .eqv. .false.)then
            write(*,*)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 1
         endif
      CASE DEFAULT
         if (isupper(ch) .eqv. .true.)then
            write(*,*)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isupper')
end subroutine test_isupper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
elemental function islower(ch) result(res)

character(len=*),parameter::ident_69="@(#)M_strings::islower(3f): returns true if character is a miniscule letter (a-z)"

character,intent(in) :: ch
logical              :: res
   select case(ch)
   case('a':'z')
     res=.true.
   case default
     res=.false.
   end select
end function islower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_islower
!!use M_strings, only: islower
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('islower',' &
      & -description ''elemental function returns .true. if CHR is a miniscule letter (a-z)'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z')
         if (islower(ch) .eqv. .false.)then
            write(*,*)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 1
         endif
      CASE DEFAULT
         if (islower(ch) .eqv. .true.)then
            write(*,*)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('islower')
end subroutine test_islower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isalnum,isalpha,iscntrl,isdigit,isgraph,islower,
!!    isprint,ispunct,isspace,isupper,isascii,isblank,isxdigit(3f) - [M_strings:COMPARE] test membership in subsets of ASCII set
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Where "FUNCNAME" is one of the function names in the group, the functions are defined by
!!
!!     elemental function FUNCNAME(onechar)
!!     character,intent(in) :: onechar
!!     logical              :: FUNC_NAME
!!##DESCRIPTION
!!
!!       These elemental functions test if a character belongs to various subsets of the ASCII character set.
!!
!!       isalnum    returns .true. if character is a letter (a-z,A-Z) or digit (0-9)
!!       isalpha    returns .true. if character is a letter and .false. otherwise
!!       isascii    returns .true. if character is in the range char(0) to char(127)
!!       isblank    returns .true. if character is a blank (space or horizontal tab).
!!       iscntrl    returns .true. if character is a delete character or ordinary control character (0x7F or 0x00-0x1F).
!!       isdigit    returns .true. if character is a digit (0,1,...,9) and .false. otherwise
!!       isgraph    returns .true. if character is a printable ASCII character excluding space
!!       islower    returns .true. if character is a miniscule letter (a-z)
!!       isprint    returns .true. if character is a printable ASCII character
!!       ispunct    returns .true. if character is a printable punctuation character (isgraph(c) && !isalnum(c)).
!!       isspace    returns .true. if character is a null, space, tab, carriage return, new line, vertical tab, or formfeed
!!       isupper    returns .true. if character is an uppercase letter (A-Z)
!!       isxdigit   returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).
!!
!!##EXAMPLES
!!
!!   Sample Program:
!!
!!    program demo_isdigit
!!
!!     use M_strings, only : isdigit, isspace, switch
!!     implicit none
!!     character(len=10),allocatable :: string(:)
!!     integer                       :: i
!!        string=[&
!!        & '1 2 3 4 5 ' ,&
!!        & 'letters   ' ,&
!!        & '1234567890' ,&
!!        & 'both 8787 ' ]
!!        ! if string is nothing but digits and whitespace return .true.
!!        do i=1,size(string)
!!           write(*,'(a)',advance='no')'For string['//string(i)//']'
!!           write(*,*) &
!!           all(isdigit(switch(string(i))).or.isspace(switch(string(i))))
!!        enddo
!!
!!     end program demo_isdigit
!!
!!   Expected output:
!!
!!    For string[1 2 3 4 5 ] T
!!    For string[letters   ] F
!!    For string[1234567890] T
!!    For string[both 8787 ] F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental function isalnum(ch) result(res)

character(len=*),parameter::ident_70="@(#)M_strings::isalnum(3f): returns true if character is a letter (a-z,A-Z) or digit(0-9)"

character,intent(in)       :: ch
logical                    :: res
   select case(ch)
   case('a':'z','A':'Z','0':'9')
     res=.true.
   case default
     res=.false.
   end select
end function isalnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalnum
!!use M_strings, only: isalnum
implicit none
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isalnum',' &
      & -description ''elemental function returns .true. if CHR is a letter or digit'' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z','0':'9')
         if (isalnum(char(i)) .eqv. .false.)then
            write(*,*)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 1
         endif
      CASE DEFAULT
         if (isalnum(char(i)) .eqv. .true.)then
            write(*,*)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalnum')
end subroutine test_isalnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    base(3f) - [M_strings:BASE] convert whole number string in base [2-36] to string in alternate base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function base(x,b,y,a)
!!
!!    character(len=*),intent(in)  :: x
!!    character(len=*),intent(out) :: y
!!    integer,intent(in)           :: b,a
!!##DESCRIPTION
!!
!!    Convert a numeric string from base B to base A. The function returns
!!    FALSE if B is not in the range [2..36] or if string X contains invalid
!!    characters in base B or if result Y is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    x   input string representing numeric whole value
!!    b   assumed base of input string
!!    y   output string
!!    a   base specified for output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_base
!!    use M_strings, only : base
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(base(x,bd,y,ba))then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!        else
!!          print *,'Error in decoding/encoding number.'
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_base
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
logical function base(x,b,y,a)
implicit none
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

character(len=*),parameter::ident_71="&
&@(#)M_strings::base(3f): convert whole number string in base [2-36] to string in alternate base [2-36]"

base=.true.
if(decodebase(x,b,temp)) then
   if(codebase(temp,a,y)) then
   else
      print *,'Error in coding number.'
      base=.false.
   endif
else
   print *,'Error in decoding number.'
   base=.false.
endif

end function base
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_base()
   character(len=:),allocatable :: in(:)
   character(len=:),allocatable :: expected(:)
   call unit_check_start('base',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   ! convert base2 values to base10 in brief mode
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[character(len=10) :: '2','10','42','170','682','2730']
   call checkit(in,expected,2,10)

   ! convert base10 values to base2
   in=[character(len=10) :: '2','10','42','170','682','2730']
   expected=[ character(len=32) :: '10', '1010', '101010', '10101010', '1010101010', '101010101010']
   call checkit(in,expected,10,2)

   ! convert base10 values to base3
   in=[character(len=7) :: '10','20','30','40','50']
   expected = [character(len=10) :: '101', '202', '1010', '1111', '1212']
   call checkit(in,expected,10,3)

   ! convert values of various explicit bases to base10
   call checkit(['11'],['3'],2,10)
   call checkit(['1212'],['50'],3,10)
   call checkit(['123123'],['1755'],4,10)

! convert values of various explicit bases to base2 in brief mode
   call checkit(['1111'],['1111'],2,2)
   call checkit(['10'],['11'],3,2)
   call checkit(['10'],['100'],4,2)
   call checkit(['10'],['1000'],8,2)
   call checkit(['10'],['10000'],16,2)

   call unit_check_done('base')
contains
subroutine checkit(in,answers,inbase,outbase)
character(len=*),intent(in)  :: in(:)
character(len=*),intent(in)  :: answers(:)
integer,intent(in)           :: inbase
integer,intent(in)           :: outbase
character(len=256)           :: answer
integer                      :: i
   do i=1,size(in)
      if(base(in(i),inbase,answer,outbase))then
           call unit_check('base',answer.eq.answers(i), &
              'converting '//trim(in(i))//' got '//trim(answers(i))//' expected '//trim(answer) )
       else
          call unit_check_bad('base',msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_strings:BASE] convert whole number string in base [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_decodebase
!!    use M_strings, only : codebase, decodebase
!!    implicit none
!!    integer           :: ba,bd
!!    character(len=40) :: x,y
!!    integer           :: r
!!
!!    print *,' BASE CONVERSION'
!!    write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!    write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!    INFINITE: do
!!       print *,''
!!       write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!       if(x.eq.'0') exit INFINITE
!!       if(decodebase(x,bd,r)) then
!!          if(codebase(r,ba,y)) then
!!            write(*,'("In base ",I2,": ",A20)')  ba, y
!!          else
!!            print *,'Error in coding number.'
!!          endif
!!       else
!!          print *,'Error in decoding number.'
!!       endif
!!    enddo INFINITE
!!
!!    end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)
implicit none

character(len=*),parameter::ident_72="@(#)M_strings::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call string_to_value(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_decodebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_check_start('decodebase',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   ! convert base2 values to base10 in brief mode
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected,2)

   ! convert values of various explicit bases to base10
   call checkit(['11'],[3],2)
   call checkit(['1212'],[50],3)
   call checkit(['123123'],[1755],4)
   call checkit(['10'],[16],16)
   call checkit(['10'],[8],8)

   call unit_check_done('decodebase')
contains
subroutine checkit(in,answers,inbase)
character(len=*),intent(in)  :: in(:)
integer,intent(in)           :: answers(:)
integer,intent(in)           :: inbase
integer                      :: answer
integer                      :: i
   do i=1,size(in)
      if(decodebase(in(i),inbase,answer))then
           call unit_check('decodebase',answer.eq.answers(i), &
              'converting '//trim(in(i)) )
       else
          call unit_check_bad('decodebase',msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    codebase(3f) - [M_strings:BASE] convert whole number in base 10 to string in base [2-36]
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function codebase(in_base10,out_base,answer)
!!
!!    integer,intent(in)           :: in_base10
!!    integer,intent(in)           :: out_base
!!    character(len=*),intent(out) :: answer
!!##DESCRIPTION
!!
!!    Convert a number from base 10 to base OUT_BASE. The function returns
!!    .FALSE. if OUT_BASE is not in [2..36] or if number IN_BASE10 is
!!    too big.
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_codebase
!!    use M_strings, only : codebase
!!    implicit none
!!    character(len=20) :: answer
!!    integer           :: i, j
!!    logical           :: ierr
!!    do j=1,100
!!       do i=2,36
!!          ierr=codebase(j,i,answer)
!!          write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
!!       enddo
!!    enddo
!!    end program demo_codebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!     Ref.: "Math matiques en Turbo-Pascal by
!!            M. Ducamp and A. Reverchon (2),
!!            Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function codebase(inval10,outbase,answer)
implicit none

character(len=*),parameter::ident_73="@(#)M_strings::codebase(3f): convert whole number in base 10 to string in base [2-36]"

integer,intent(in)           :: inval10
integer,intent(in)           :: outbase
character(len=*),intent(out) :: answer
integer                      :: n
real                         :: inval10_local
integer                      :: outbase_local
integer                      :: in_sign
  answer=''
  in_sign=sign(1,inval10)*sign(1,outbase)
  inval10_local=abs(inval10)
  outbase_local=abs(outbase)
  if(outbase_local<2.or.outbase_local>36) then
    print *,'*codebase* ERROR: base must be between 2 and 36. base was',outbase_local
    codebase=.false.
  else
     do while(inval10_local>0.0 )
        n=INT(inval10_local-outbase_local*INT(inval10_local/outbase_local))
        if(n<10) then
           answer=ACHAR(IACHAR('0')+n)//answer
        else
           answer=ACHAR(IACHAR('A')+n-10)//answer
        endif
        inval10_local=INT(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign.eq.-1)then
     answer='-'//trim(answer)
  endif
  if(answer.eq.'')then
     answer='0'
  endif
end function codebase
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_codebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_check_start('codebase',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')

   ! convert base10 values to base2 strings
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected,2)

   ! convert values to various explicit bases
   call checkit(['11'],[3],2)
   call checkit(['1212'],[50],3)
   call checkit(['123123'],[1755],4)
   call checkit(['10'],[16],16)
   call checkit(['10'],[8],8)

   call unit_check_done('codebase')
contains
subroutine checkit(answer,values,outbase)
character(len=*),intent(in)  :: answer(:)
integer,intent(in)           :: values(:)
integer,intent(in)           :: outbase
character(len=32)            :: out
integer                      :: i
   do i=1,size(answer)
      if(codebase(values(i),outbase,out) )then
           call unit_check('codebase',out.eq.answer(i), &
              'checking for '//trim(answer(i))//' in base '//v2s(outbase)//' from value '//v2s(values(i)) )
       else
          call unit_check_bad('codebase',msg='Error answer decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_codebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function todecimal(base, instr)

character(len=*),parameter::ident_74="@(#)M_strings::todecimal(3f): given string and base return decimal integer"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
character(*),intent(in)      :: instr
character(len=:),allocatable :: instr_local
integer                      :: todecimal
integer                      :: length, i, n

   instr_local=trim(lower(instr))
   todecimal = 0
   length = len(instr_local)
   do i = 1, length
      n = index(alphanum, instr_local(i:i)) - 1
      n = n * base**(length-i)
      todecimal = todecimal + n
   enddo
end function todecimal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function tobase(base, number)

character(len=*),parameter::ident_75="@(#)M_strings::todecimal(3f): given integer and base return string"

! based on an example at rosetta code.
character(len=36),parameter  :: alphanum = "0123456789abcdefghijklmnopqrstuvwxyz"
integer,intent(in)           :: base
integer,intent(in)           :: number
character(len=:),allocatable :: tobase
character(len=31)            :: holdit
integer                      :: number_local, i, rem
   number_local=number

   holdit = "                               "
   do i = 31, 1, -1
      if(number_local < base) then
         holdit(i:i) = alphanum(number_local+1:number_local+1)
         exit
      endif
      rem = mod(number_local, base)
      holdit(i:i) = alphanum(rem+1:rem+1)
      number_local = number_local / base
   enddo
   tobase = adjustl(holdit)
end function tobase

!SUBROUTINE DectoBase(decimal, string, base)
! CHARACTER string
!    string = '0'
!    temp = decimal
!    length = CEILING( LOG(decimal+1, base) )   !<<<<<<<< INTERESTING
!    DO i = length, 1, -1
!      n = MOD( temp, base )
!      string(i) = "0123456789abcdefghijklmnopqrstuvwxyz"(n+1)
!      temp = INT(temp / base)
!    ENDDO
! END
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_strings:TOKENS] Tokenize a string, consuming it one token per call
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fmt(source_string,length)
!!
!!    character(len=*),intent(in)       :: source_string
!!    integer,intent(in)                :: length
!!    character(allocatable(len=length)    :: fmt(:)
!!##DESCRIPTION
!!    fmt(3f) breaks a long line into a simple paragraph of specified line length.
!!
!!    Given a long string break it on spaces into an array such that no variable is
!!    longer than the specified length. Individual words longer than LENGTH will be
!!    placed in variables by themselves.
!!##OPTIONS
!!     SOURCE_STRING  input string to break into an array of shorter strings on blank delimiters
!!     LENGTH         length of lines to break the string into.
!!##RETURNS
!!     FMT  character array filled with data from source_string broken at spaces into
!!          variables of length LENGTH.
!!##EXAMPLE
!!
!!  sample program
!!
!!    program demo_fmt
!!    use M_strings, only : fmt
!!    character(len=80),allocatable :: paragraph(:)
!!    character(len=*),parameter    :: string= '&
!!     &one two three four five &
!!     &six seven eight &
!!     &nine ten eleven twelve &
!!     &thirteen fourteen fifteen sixteen &
!!     &seventeen'
!!
!!    paragraph=fmt(string,40)
!!    write(*,'(a)')paragraph
!!
!!    write(*,'(a)')fmt(string,0)
!!    write(*,'(3x,a)')fmt(string,77)
!!
!!    end program demo_fmt
!!
!!   Results:
!!
!!    one two three four five six seven eight
!!    nine ten eleven twelve thirteen fourteen
!!    fifteen sixteen seventeen
!!    one
!!    two
!!    three
!!    four
!!    five
!!    six
!!    seven
!!    eight
!!    nine
!!    ten
!!    eleven
!!    twelve
!!    thirteen
!!    fourteen
!!    fifteen
!!    sixteen
!!    seventeen
!!       one two three four five six seven eight nine ten eleven twelve thirteen
!!       fourteen fifteen sixteen seventeen
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fmt(source_string,length)

character(len=*),parameter::ident_76="@(#)M_strings::fmt(3f): wrap a long string into a paragraph"

character(len=*),intent(in)       :: source_string
integer,intent(in)                :: length
integer                           :: itoken
integer                           :: istart
integer                           :: iend
character(len=*),parameter        :: delimiters=' '
character(len=:),allocatable      :: fmt(:)
integer                           :: ilines
integer                           :: ilength
integer                           :: iword, iword_max
integer                           :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse string once to find out how big to make the returned array, then redo everything but store the data
!  could store array of endpoints and leave original whitespace alone or many other options
   do i=1,2
      iword_max=0                                  ! length of longest token
      ilines=1                                     ! number of output line output will go on
      ilength=0                                    ! length of output line so far
      itoken=0                                     ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword.gt.length)then                   ! this token is longer than the desired line length so put it on a line by itself
            if(ilength.ne.0)then
               ilines=ilines+1
            endif
            if(i.eq.2)then                 ! if fmt has been allocated store data, else just gathering data to determine size of fmt
               fmt(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword.le.length)then       ! this word will fit on current line
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)//' '
            endif
            ilength=ilength+iword+1
         else                                      ! adding this word would make line too long so start new line
            ilines=ilines+1
            ilength=0
            if(i.eq.2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)//' '
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then                                 ! determined number of lines needed so allocate output array
         allocate(character(len=max(length,iword_max)) :: fmt(ilines))
         fmt=' '
      endif
   enddo
   fmt=fmt(:ilines)
end function fmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_m_strings
!!use M_strings, only: reverse
!!use M_strings, only: lower
!!use M_strings, only: switch
!!use M_strings, only: isgraph,isprint
implicit none
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=1)            :: chars(36)
call unit_check_start('combined')
!-----------------------------------------------------------------------------------------------------------------------------------
! COMBINED TESTS
chars=switch(uc)     ! convert string to character array
chars=chars(36:1:-1) ! reverse order of characters
call unit_check('combined',lower(reverse(switch(chars))).eq.lc,msg='combined lower(),reverse(),switch()')
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level.gt.0)then
   write(*,*)'isprint'
   write(*,*)'   letter a      ',isprint('a')
   write(*,*)'   horizontal tab',isprint(char(9))
   write(*,*)'   array of letters;.',isprint([';','.',' '])
   write(*,*)'   array of letters',isprint(switch(uc))
   write(*,*)'   array of letters',isprint(uc)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level.gt.0)then
   write(*,*)'isgraph'
   write(*,*)'   letter a      ',isgraph('a')
   write(*,*)'   horizontal tab',isgraph(char(9))
   write(*,*)'   array of letters;.',isgraph([';','.',' '])
   write(*,*)'   array of letters',isgraph(switch(uc))
   write(*,*)'   array of letters',isgraph(uc)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_done('combined')
end subroutine test_m_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int()
   call unit_check_start('int',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('int',int('1234').eq.1234,msg='test string to integer for overloaded INT()')
   call unit_check_done('int',msg=' overload of INT()')
end subroutine test_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real()
   call unit_check_start('real',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('real', real('3.0d0').eq.3.0d0,msg='test string to real for overloaded REAL()')
   call unit_check_done('real',msg='overload of REAL(3f)')
end subroutine test_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble()
   call unit_check_start('dble',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_strings.FF &
      & -documentation y &
      & -ufpp         y &
      & -ccall        n &
      & -archive      GPF.a &
      & ')
   call unit_check('dble', dble('3.0d0').eq.3.0d0,msg='test string to double for overloaded DBLE()')
   call unit_check_done('dble',msg='overload of DBLE(3f)')
end subroutine test_dble
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_m_strings()
   call test_adjustc()
   call test_atleast()
   call test_base()
   call test_c2s()
   call test_change()
   call test_chomp()
   call test_codebase()
   call test_compact()
   call test_crop()
   call test_decodebase()
   call test_delim()
   call test_describe()
   call test_expand()
   call test_getvals()
   call test_indent()
   call test_isalnum()
   call test_isalpha()
   call test_isascii()
   call test_isblank()
   call test_iscntrl()
   call test_isdigit()
   call test_isgraph()
   call test_islower()
   call test_isnumber()
   call test_isprint()
   call test_ispunct()
   call test_isspace()
   call test_isupper()
   call test_isxdigit()
   call test_join()
   call test_len_white()
   call test_lenset()
   call test_listout()
   call test_lower()
   call test_matchw()
   call test_merge_str()
   call test_modif()
   call test_noesc()
   call test_nospace()
   call test_notabs()
   call test_replace()
   call test_reverse()
   call test_s2c()
   call test_s2v()
   call test_s2vs()
   call test_split()
   call test_string_to_value()
   call test_string_to_values()
   call test_strtok()
   call test_substitute()
   call test_switch()
   call test_transliterate()
   call test_rotate13()
   call test_quote()
   call test_unquote()
   call test_upper()
   call test_v2s()
   call test_v2s_bug()
   call test_value_to_string()
   call test_visible()
   call test_m_strings()
   call test_dble()
   call test_int()
   call test_real()
   call test_stretch()
   call test_trimzeros()
end subroutine test_suite_m_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_strings
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!
!           X
!           X
!  XXXXX   XXXX   XXX XX   XXX    XX XX    XXXXXX
! X     X   X       XX  X    X     XX  X  X    X
!  XXX      X       X        X     X   X  X    X
!     XX    X       X        X     X   X  X    X
! X     X   X  X    X        X     X   X   XXXXX
!  XXXXX     XX   XXXXX    XXXXX  XXX XXX      X
!                                              X
!                                          XXXX
!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_strings_oop
!
! Define an OOP (Object-Oriented Programming) interface for the M_strings module.
!
! Leveraging the existing procedural functions in module M_strings to do the calculations allows
! this to be a definition of a derived type ( TYPE(STRING) ) and the
! methods it supports and overloading of operators to support the new data type.
!
use M_strings, only : upper, lower                       ! case
use M_strings, only : lenset, atleast, adjustc, compact, crop     ! whitespace
use M_strings, only : reverse
use M_strings, only : notabs, noesc, expand
use M_strings, only : substitute, transliterate
use M_strings, only : string_to_value, switch, v2s, s2v
use M_strings, only : switch, split, matchw
implicit none
private
integer,parameter,private :: dp=kind(0.0d0)
public p
!-----------------------------------------------------------------------------------------------------------------------------------
   public string
!-----------------------------------------------------------------------------------------------------------------------------------
!DERIVED TYPE STRING
!
type string
   ! COMPONENTS:
   character(len=:),allocatable :: str
contains
   ! METHODS:
   procedure  ::  adjustc        =>  oop_adjustc
   procedure  ::  adjustl        =>  oop_adjustl
   procedure  ::  adjustr        =>  oop_adjustr
   procedure  ::  compact        =>  oop_compact
   procedure  ::  crop           =>  oop_crop
   procedure  ::  dble           =>  oop_dble
   procedure  ::  expand         =>  oop_expand
   procedure  ::  index          =>  oop_index
   procedure  ::  init           =>  init_string
   procedure  ::  int            =>  oop_int
   procedure  ::  len            =>  oop_len
   procedure  ::  len_trim       =>  oop_len_trim
   procedure  ::  lenset         =>  oop_lenset
   procedure  ::  atleast        =>  oop_atleast
   procedure  ::  match          =>  oop_matchw
   procedure  ::  lower          =>  oop_lower
   procedure  ::  noesc          =>  oop_noesc
   procedure  ::  notabs         =>  oop_notabs
   procedure  ::  real           =>  oop_real
   procedure  ::  reverse        =>  oop_reverse
   procedure  ::  substitute     =>  oop_substitute
   procedure  ::  transliterate  =>  oop_transliterate
   procedure  ::  trim           =>  oop_trim
   procedure  ::  upper          =>  oop_upper
   procedure  ::  chars          =>  oop_switch
!!   procedure  ::  split          =>  oop_split
   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(STRING)
   procedure,private :: eq
   generic           :: operator(==) => eq
   procedure,private :: lt
   generic           :: operator(<)  => lt
   procedure,private :: gt
   generic           :: operator(>)  => gt
   procedure,private :: ge
   generic           :: operator(>=) => ge
   procedure,private :: le
   generic           :: operator(<=) => le
   procedure,private :: ne
   generic           :: operator(/=) => ne

   procedure,private :: string_plus_value
   generic           :: operator(+) => string_plus_value   ! string + integer|real|doubleprecision|string|character
   procedure,private :: string_minus_value
   generic           :: operator(-) => string_minus_value  ! string - integer|real|doubleprecision|string|character
   procedure,private :: string_multiply_value
   generic           :: operator(*) => string_multiply_value  ! string * integer|real|doubleprecision
   procedure,private :: string_append_value
   generic           :: operator(//) => string_append_value

!!   procedure,private :: minus_string
!!   generic           :: operator(-)  => minus_string
end type
!===================================================================================================================================
! User-defined constructors are created by defining a generic interface
! with the same name as the derived type they're supposed to construct.
interface string
   module procedure construct_from_fill
end interface string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this function is used internally in the module, but is also declared to be a constructor for creating TYPE(DATE_TYPE) structures
!
function construct_from_fill(chars,len)

character(len=*),parameter::ident_77="@(#)M_strings::construct_from_fill(3f): construct TYPE(STRING)"

character(len=*),intent(in),optional :: chars
integer,intent(in),optional          :: len
type(string)                         :: construct_from_fill
   if(present(chars))then
      construct_from_fill%str=chars
   else
      construct_from_fill%str=''
   endif
   if(present(len))then
      construct_from_fill%str=lenset(construct_from_fill%str,len)
   endif
end function construct_from_fill
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! DEFINE THE METHODS FOR THE TYPE
! These functions are privately used to define the methods that TYPE(STRING) will support
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_len(self) result (length)

character(len=*),parameter::ident_78="@(#)M_strings::oop_len(3f): length of string"

class(string),intent(in)    :: self
integer                     :: length
   length=len(self%str)
end function oop_len
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_len_trim(self) result (length)

character(len=*),parameter::ident_79="@(#)M_strings::oop_len_trim(3f): trimmed length of string"

class(string),intent(in)    :: self
integer                     :: length
   length=len_trim(self%str)
end function oop_len_trim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_switch(self) result (array)

character(len=*),parameter::ident_80="@(#)M_strings::oop_switch(3f): convert string to array of single characters"

class(string),intent(in)    :: self
character(len=1)            :: array(len(self%str))
   array=switch(self%str)
end function oop_switch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_index(self,substring,back) result (location)

character(len=*),parameter::ident_81="@(#)M_strings::oop_index(3f): find starting position of a substring in a string"

class(string),intent(in)    :: self
character(len=*),intent(in) :: substring
integer                     :: location
logical,optional,intent(in) :: back
   if(present(back))then
      location=index(self%str,substring,back)
   else
      location=index(self%str,substring)
   endif
end function oop_index
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_upper(self) result (string_out)

character(len=*),parameter::ident_82="@(#)M_strings::oop_upper(3f): convert string to uppercase"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=upper(self%str)
end function oop_upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_lower(self) result (string_out)

character(len=*),parameter::ident_83="@(#)M_strings::oop_lower(3f): convert string to miniscule"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=lower(self%str)
end function oop_lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_expand(self,escape_char) result (string_out)

character(len=*),parameter::ident_84="@(#)M_strings::oop_expand(3f): expand common escape sequences by calling expand(3f)"

class(string),intent(in)      :: self
character,intent(in),optional :: escape_char
type(string)                  :: string_out
   if(present(escape_char))then
      string_out%str=expand(self%str,escape_char)
   else
      string_out%str=expand(self%str)
   endif
end function oop_expand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_trim(self) result (string_out)

character(len=*),parameter::ident_85="@(#)M_strings::oop_trim(3f): trim trailing spaces"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=trim(self%str)
end function oop_trim
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_crop(self) result (string_out)

character(len=*),parameter::ident_86="@(#)M_strings::oop_crop(3f): crop leading and trailing spaces"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=crop(self%str)
end function oop_crop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_reverse(self) result (string_out)

character(len=*),parameter::ident_87="@(#)M_strings::oop_reverse(3f): reverse string"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=reverse(self%str)
end function oop_reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustl(self) result (string_out)

character(len=*),parameter::ident_88="@(#)M_strings::oop_adjustl(3f): adjust string to left"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=adjustl(self%str)
end function oop_adjustl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustr(self) result (string_out)

character(len=*),parameter::ident_89="@(#)M_strings::oop_adjustr(3f): adjust string to right"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=adjustr(self%str)
end function oop_adjustr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_adjustc(self,length) result (string_out)

character(len=*),parameter::ident_90="@(#)M_strings::oop_adjustc(3f): adjust string to center"

class(string),intent(in)     :: self
type(string)                 :: string_out
integer,intent(in),optional  :: length
   if(present(length))then
      string_out%str=lenset(adjustc(self%str,length),length)
   else
      string_out%str=adjustc(self%str)
   endif
end function oop_adjustc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_int(self) result (value)

character(len=*),parameter::ident_91="@(#)M_strings::oop_int(3f): string to integer"

class(string),intent(in)     :: self
integer                      :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_real(self) result (value)

character(len=*),parameter::ident_92="@(#)M_strings::oop_real(3f): string to real"

class(string),intent(in)     :: self
real                         :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_dble(self) result (value)

character(len=*),parameter::ident_93="@(#)M_strings::oop_dble(3f): string to double"

class(string),intent(in)     :: self
doubleprecision              :: value
integer                      :: ierr
   call string_to_value(self%str,value,ierr)
end function oop_dble
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_compact(self,char) result (string_out)

character(len=*),parameter::ident_94="@(#)M_strings::oop_compact(3f): adjust string to center"

class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),optional    :: char
   if(present(char))then
      string_out%str=compact(self%str,char)
   else
      string_out%str=compact(self%str)
   endif
   string_out%str=trim(string_out%str)
end function oop_compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_substitute(self,old,new) result (string_out)

character(len=*),parameter::ident_95="&
&@(#)M_strings::oop_substitute(3f): change all occurrences of oldstring to newstring non-recursively"

class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),intent(in)  :: old
character(len=*),intent(in)  :: new
   string_out%str=self%str
   call substitute(string_out%str,old,new)
end function oop_substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_transliterate(self,old,new) result (string_out)

character(len=*),parameter::ident_96="&
&@(#)M_strings::oop_transliterate(3f): change all occurrences of oldstring to newstring non-recursively"

class(string),intent(in)     :: self
type(string)                 :: string_out
character(len=*),intent(in)  :: old
character(len=*),intent(in)  :: new
   string_out%str=transliterate(self%str,old,new)
end function oop_transliterate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_atleast(self,length) result (string_out)

character(len=*),parameter::ident_97="@(#)M_strings::oop_atleast(3f): set string to at least specified length"

class(string),intent(in)     :: self
type(string)                 :: string_out
integer,intent(in)           :: length
   string_out%str=atleast(self%str,length)
end function oop_atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_lenset(self,length) result (string_out)

character(len=*),parameter::ident_98="@(#)M_strings::oop_lenset(3f): set string to specific length"

class(string),intent(in)     :: self
type(string)                 :: string_out
integer,intent(in)           :: length
   string_out%str=lenset(self%str,length)
end function oop_lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_matchw(self,pattern) result (answer)

character(len=*),parameter::ident_99="@(#)M_strings::oop_matchw(3f): test if wildcard pattern matches string"

class(string),intent(in)     :: self
character(len=*),intent(in)  :: pattern
logical                      :: answer
   answer=matchw(self%str,pattern)
end function oop_matchw
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_notabs(self) result (string_out)

character(len=*),parameter::ident_100="&
&@(#)M_strings::oop_notabs(3f): expand tab characters assuming tab stops every eight(8) characters"

class(string),intent(in)     :: self
type(string)                 :: string_out
integer                      :: length
   string_out%str=lenset('',8*len(self%str)) ! make long enough assuming all tab characters
   call notabs(self%str,string_out%str,length)
   string_out%str=trim(string_out%str)
end function oop_notabs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function oop_noesc(self) result (string_out)

character(len=*),parameter::ident_101="@(#)M_strings::oop_noesc(3f): replace non-printable characters with spaces"

class(string),intent(in)     :: self
type(string)                 :: string_out
   string_out%str=noesc(self%str)
end function oop_noesc
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function p(self) result (string_out)

character(len=*),parameter::ident_102="@(#)M_strings::oop_p(3f): return CHARACTER string from TYPE(STRING)"

class(string),intent(in)     :: self
character(len=len(self%str)) :: string_out
   string_out=self%str
end function p
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine init_string(self)
!
! allow for TYPE(STRING) object to be initialized.
!

character(len=*),parameter::ident_103="@(#)M_strings::init_dt(3f): initialize TYPE(STRING)"

class(string)                        :: self
   self%str=''
end subroutine init_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! FUNCTIONS FOR DEFINING OVERLOADED OPERATORS
!===================================================================================================================================
function string_plus_value(self,value) result (other)

character(len=*),parameter::ident_104="@(#)M_strings::string_plus_value(3f): add value to TYPE(STRING)"

class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "+" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);          other%str=v2s(value+s2v(self%str))  ! convert string%str to integer, add to value
   type is (real);             other%str=v2s(value+s2v(self%str))  ! convert string%str to real, add to value
   type is (doubleprecision);  other%str=v2s(value+s2v(self%str))  ! convert string%str to doubleprecision, add to value
   type is (character(len=*)); other%str=self%str//' '//value      ! append space and CHARACTER to string %str
   type is (string);           other%str=self%str//' '//value%str  ! append string %str values with space between
   end select
end function string_plus_value
!===================================================================================================================================
function string_minus_value(self,value) result (other)

character(len=*),parameter::ident_105="@(#)M_strings::string_minus_value(3f): subtract value from TYPE(STRING)"

class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "-" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);         other%str=v2s(value-s2v(self%str))
   type is (real);            other%str=v2s(value-s2v(self%str))
   type is (doubleprecision); other%str=v2s(value-s2v(self%str))
   type is (character(len=*))
      other%str=self%str
      call substitute(other%str,value,'')
   type is (string)
      other%str=self%str
      call substitute(other%str,value%str,'')
   end select
end function string_minus_value
!===================================================================================================================================
function string_append_value(self,value) result (other)

character(len=*),parameter::ident_106="@(#)M_strings::string_append_value(3f): append value to TYPE(STRING)"

class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "//" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,CHARACTER,TYPE(STRING) )
   select type(value)
   type is (integer);          other%str=self%str//v2s(value)
   type is (real);             other%str=self%str//v2s(value)
   type is (doubleprecision);  other%str=self%str//v2s(value)
   type is (character(len=*)); other%str=self%str//value
   type is (string);           other%str=self%str//value%str
   end select
end function string_append_value
!===================================================================================================================================
function string_multiply_value(self,value) result (other)

character(len=*),parameter::ident_107="@(#)M_strings::string_multiply_value(3f): multiply TYPE(STRING) value times"

class(string),intent(in)      :: self
type(string)                  :: other
class(*),intent(in)           :: value
!  This function is primarily intended to provide behaviors for the "//" operator for TYPE(STRING) values
!  Notice that the value can be any of several types ( INTEGER,REAL,DOUBLEPRECISION )
   select type(value)
   type is (integer);          other%str=repeat(self%str,value)
   type is (real);             other%str=repeat(self%str,nint(value))
   type is (doubleprecision);  other%str=repeat(self%str,nint(value))
   end select
end function string_multiply_value
!===================================================================================================================================
logical function eq(self,other)

character(len=*),parameter::ident_108="@(#)M_strings::eq(3f): compare derived type string objects (eq,lt,gt,le,ge,ne)"

   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   eq= self%str .eq. other%str
end function eq
logical function lt(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   lt= self%str .lt. other%str
end function lt
logical function gt(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   gt= self%str .gt. other%str
end function gt
logical function le(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   le= self%str .le. other%str
end function le
logical function ge(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   ge= self%str .ge. other%str
end function ge
logical function ne(self,other)
   class(string),intent(in) :: self
   type(string),intent(in)  :: other
   ne= self%str .ne. other%str
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_strings_oop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
