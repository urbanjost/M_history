!>
!!##NAME
!!    redo(3f) - [M_history] Fortran-based Input History Editor
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine redo(inputline,r)
!!
!!      character(len=*) :: inputline
!!      character(len=1),intent(in),optional :: r
!!
!!##DESCRIPTION
!!    the redo(3f) routine lets you recall, list, save, and modify previously
!!    entered program input. Built-in help is included.
!!
!!    The redo(3f) input history editor is a simple-to-use input history
!!    editor interface modeled on the CDC NOS command REDO. It uses a
!!    line editor model that means no special escape characters or control
!!    characters are required. Typically, only a few minutes are required
!!    to master usage.
!!
!!    When using redo(3f) input lines are usually first read into a character
!!    variable and then passed to the routine. The returned string can then
!!    be parsed or read from with an internal READ(3f). So, for example,
!!    if you have an existing READ(3f) such as
!!
!!       READ(*,101) A,I,K
!!
!!    replace it with something similar to
!!
!!      USE M_HISTORY,ONLY : REDO
!!      CHARACTER(LEN=255) :: LINE ! make variable big enough to read a line
!!            :
!!            :
!!      READ(*,'(A)') LINE   ! read line into character variable
!!      CALL REDO(LINE)      ! pass line to REDO(3f). This is a no-op except
!!                           ! for storing the line into the input history
!!                           ! unless the input line is the "r" command
!!      READ(LINE,101)A,I,K  ! read from variable like you did from file
!!##OPTIONS
!!      inputline    line to record into history buffer file or to edit.
!!
!!      r            Optional character to use as command to invoke editing.
!!                   Defaults to 'r'.
!!
!!##USAGE
!!    When prompted for an input line by your program you may at any time
!!    enter "r" on a line by itself, or a line beginning with "r r_command"
!!    and you will enter the command history edit mode. Now you can recall
!!    and edit previous input or compose an input line using the editor
!!    commands.
!!
!!    By default, you will be editing the last line you entered, shifted
!!    one character to the right by an exclamation character.
!!
!!    The character you respond with in column one controls what happens next.
!!
!!    o If you enter "?" while in command edit mode, help is displayed.
!!
!!    o If the last input line is not the desired line to edit,
!!      select the line to edit by entering its line number or by
!!      using the /,l,u, and d commands (see below for details) to find the desired input line.
!!    o Next enter an editing directive (c,m) to edit the selected line. The
!!      "change" command will change all occurrences of an old string to a
!!      new string ...
!!
!!       c/old/new/
!!
!!    o or the "modify" command can be used with the special characters # &amp; and ^ ...
!!        o A # under a character will delete a character.
!!        o An "&" (ampersand) will cause the character above it to be replaced with a space.
!!        o  To insert a string enter ^string#.
!!        o Otherwise, enter a character under one in the currently displayed command and it will replace it.
!!        o hit RETURN to start another edit of the line
!!    o Once the change is executed you will be prompted for another edit
!!      directive
!!    o You will stay in edit mode until you enter a return on a
!!      blank line to feed your line to your program; or enter "." or
!!      "q" (which means cancel changes and return a blank line).
!!
!!    A detailed summary of the main edit-mode commands follows. In the
!!    descriptions, N stands for a number ...
!!
!!  LISTING COMMAND HISTORY
!!     l|p N      list from line N. -N shows N last lines
!!     L|P N      same as 'l' except no line numbers (for pasting)
!!     /string    search for simple string in all history lines
!!
!!  Note that the buffer is set to the last line displayed
!!
!!  POSITIONING TO PREVIOUS COMMANDS
!!     u N        up through buffer
!!     d N        down through buffer
!!     N          load line number
!!
!!  EDITING THE CURRENT BUFFER LINE
!!     c/oldstring/newstring/   change all occurrences of old string
!!                              to new string. Note that s
!!                              (for substitute) is a synonym for c
!!                              (for change).
!!
!!                              For the "c" directive the second character
!!                              becomes the delimiter. Traditionally one
!!                              usually uses a delimiter of / unless the
!!                              string you are editing contains /.
!!
!!     mmod_string    If the first character of your entry is m or blank,
!!              o REPLACE a string by entering a replacement character under it
!!              o LEAVE a character alone by leaving a space under it
!!              o DELETE a character by putting a # character under it
!!              o BLANK OUT a character by putting an & under it
!!              o INSERT A STRING by entering ^STRING#
!!
!!       The "modify" directive takes a little practice but this single
!!       directive accommodates positionally deleting, replacing, and
!!       inserting text. it is hardest using "modify" to put the strings
!!       "&" and "#" into your lines. to put a # or & character into a
!!       string use the 'c' command instead or ^&# or ^##.
!!
!!     ;N N N N ...  Append specified lines to current line
!!
!!  HELP
!!        h|?    display help text
!!
!!  SYSTEM COMMANDS
!!        !cmd   execute system command
!!
!!  DUMPING AND LOADING THE COMMAND HISTORY
!!
!!        w FILENAME   write entire command history to specified file
!!        r FILENAME   replace command history with file contents
!!        a FILENAME   append lines from file onto command history
!!
!!##EXAMPLE PROGRAM
!!   Sample program
!!
!!       program demo_redo
!!       use M_history, only : redo
!!       implicit none
!!       character(len=1024) ::  line
!!       integer             :: ios
!!       integer             :: cstat
!!       character(len=256)  :: sstat
!!       write(*,'(a)')                                             &
!!       & 'REDO(3f) COMMAND INPUT EDITOR',                         &
!!       & 'enter "r" or "r r_command" on the input line to go',    &
!!       & 'into history edit mode. Once in history edit mode you', &
!!       & 'may enter "?" to get some help. Enter "quit" to exit',  &
!!       & 'the program.'
!!       do
!!          write(*,'(a)',advance='no')'>->'    ! write prompt
!!          read(*,'(a)',iostat=ios) line       ! read new input line
!!          ! if "r", edit and return a line from the history editor
!!          call redo(line) ! store into history if not "r".
!!          if(line == 'quit')stop ! exit program if user enters "quit"
!!          ! now call user code to process new line of data
!!          ! As an example, call the system shell
!!          call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat)
!!       enddo
!!       end program demo_redo
!!
!!##SAMPLE USAGE
!!
!!    The example program is basically a loop that reads a command from
!!    standard input and then executes it as a subshell unless the "r"
!!    command is entered.
!!
!!    Now, we will enter an echo(1) command followed by a few other lines
!!    of input. Then we recall the echo(1) command and use a few of the
!!    features of redo(3) to change and then re-execute the command.
!!
!!       >echo This isss a Test
!!       This isss a Test
!!       >date
!!       Sun May 31 23:54:09 EDT 2009
!!       >pwd
!!       /cygdrive/c/urbanjs/MYCYGWIN/DISKA/public_html/public/CLONE/REDO
!!       >r                            ! enter edit mode
!!       00001 echo This isss a Test   ! last commands are displayed
!!       00002 date
!!       00003 pwd
!!       !pwd
!!       >1                            ! go to first line in history
!!       !echo This isss a Test
!!                    ##   t           ! delete and replace characters
!!       !echo This is a test          ! insert a string
!!                       ^new #
!!       !echo This is a new test
!!       c/test/TEST/                  ! change a substring
!!       !echo This is a new TEST
!!                          &          | replace character with spaces
!!       !echo This is a newTEST
!!                                     ! a blank line ends editing
!!       This is a newTEST
!!       >quit
!!
!!##AUTHOR
!!    John S. Urban, 1988,2009,2011,2015 (last change: Nov 2019)
!!##LICENSE
!!    MIT
module M_history
!
!  Acting much like a line-mode editor, the REDO(3f) procedure lets
!  you list, edit, save, and modify your interactively entered program
!  input. Built-in help and no dependence on terminal control sequences
!  makes this a simple-to-master and portable input history editor.
!
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT        ! access computing environment
use, intrinsic :: iso_fortran_env, only : output_unit, stderr=>error_unit
implicit none
private

public  :: redo                    !  copy a line into history file or edit history if command is "r" and return line

private :: open_history_           !  open history file
private :: redol_                  !  edit history
private :: help_                   !  produce help text for redo(3f) usage

!  should use unused file, not just unit 1071 for history
!  add option to read in and replace history file

integer,parameter :: READLEN=1024  ! width of history file


integer,save,private       :: stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

interface string_to_value
   module procedure a2d, a2i
end interface

interface v2s
   module procedure i2s
end interface

interface msg
   module procedure msg_scalar, msg_one
end interface msg

interface journal
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
end interface journal

interface str
   module procedure str_scalar, str_one
end interface str
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine redo(inputline,r,lun)
!      if line starts with r word call redol_()
!      uses unit 1071
!       r
!       r string

! ident_1="@(#) M_history redo(3f) open binary direct access file for keeping history"

character(len=*),intent(inout) :: inputline                ! user string
character(len=1),intent(in),optional :: r                  ! character to use to trigger editing
integer,intent(in),optional          :: lun
character(len=1)                     :: r_local            ! character to use to trigger editing
integer,save                         :: iobuf=1071         ! unit number to use for redo history buffer
integer,save                         :: iredo              ! number of lines read from standard input into redo file
logical,save                         :: lcalled=.false.    ! flag whether first time this routine called or not
character(len=READLEN)               :: onerecord
integer                              :: ioparc
integer                              :: ilast
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(r))then
   r_local=r
else
   r_local='r'
endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  open history file and initialize
   if(.not.lcalled)then                                     ! open the redo buffer file
      lcalled=.true.
      iredo=0   ! number of lines in redo buffer
      call open_history_(iobuf,' ','scratch',ioparc)        ! redo buffer
      if(ioparc /= 0)then
         call journal('sc','error creating history file')
         return
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilast=len_trim(inputline)

   if(ilast == 1.and.inputline(1:1) == r_local)then                             ! redo command
      call redol_(inputline,iobuf,iredo,READLEN,' ',lun)
      ilast=len_trim(inputline)
   elseif(inputline(1:min(2,len(inputline))) == r_local//' ')then               ! redo command with a string following
      call redol_(inputline,iobuf,iredo,READLEN,inputline(3:max(3,ilast)),lun)
      ilast=len_trim(inputline)
   endif

   if(ilast /= 0)then                                                           ! put command into redo buffer
      iredo=iredo+1
      onerecord=inputline                ! make string the correct length; ASSUMING inputline IS NOT LONGER THAN onerecord
      write(iobuf,rec=iredo)onerecord
   endif
end subroutine redo
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine open_history_(iunit,fname,sname,ierr)
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#) M_history open_history_(3fp) open history file for REDO(3f) procedure"

integer,intent(in)          :: iunit   ! Fortran unit to open
character(len=*),intent(in) :: fname   ! filename to open
character(len=*),intent(in) :: sname   ! flag. If "scratch" ignore FNAME and open a scratch file
integer,intent(out)         :: ierr    ! error code returned by opening file
character(len=1024)         :: msg
!-----------------------------------------------------------------------------------------------------------------------------------
  if(sname == 'scratch')then
     open(unit=iunit,status='scratch',form='unformatted',access='direct',recl=READLEN,iostat=ierr,iomsg=msg,action='readwrite')
  else
     open(unit=iunit,file=trim(fname),status=trim(sname),form='unformatted',access='direct', &
     & recl=READLEN,iostat=ierr,iomsg=msg,action='readwrite')
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
  if(ierr /= 0)then
     call journal('sc','*open_history_* open error ',ierr,'=',msg)
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine open_history_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine redol_(redoline,iobuf,iredo,ibuf0,init,lun)
!
!  to do:
!  might want to support a count on change to do the Nth to the Mth occurrence
!  support edit window in change
!  prompt to verify each change made with change()
!  maybe make .NAME stick into variable $NAME in the calculator
!  allow changing the edit characters in a modify

! ident_3="@(#) M_history redoline(3fp) redo a previous input line"

character(len=*),intent(out)   :: redoline    ! edited command line to be returned
integer,intent(in)             :: iobuf       ! history file unit to read old commands from
integer                        :: iredo       !iredo ......  (i) number of lines in history file
character(len=*),intent(in)    :: init        ! initial command string
integer,intent(in)             :: ibuf0       ! the width of the history file in characters; <= len(redoline)
integer,intent(in),optional    :: lun         ! LUN to read history commands from

doubleprecision                :: val8
integer                        :: i10, i15, i20, i30
integer                        :: iounit
integer                        :: idump
integer                        :: idown
integer                        :: lun_local
integer                        :: ipoint
integer                        :: iread
integer                        :: istart
integer                        :: ios
integer                        :: ii
integer                        :: ilong
integer                        :: icall
integer                        :: iup
integer                        :: ix
integer                        :: ibuf
integer                        :: ilast
integer                        :: cstat
character                      :: cmd
character(:),allocatable       :: cmdline
character(len=len(redoline)+1) :: cin, cinbuf ! 1 greater than length of redoline
character(len=1024),save       :: numbers
character(len=1024),save       :: msg
integer,allocatable            :: ivals(:)
integer                        :: iend
integer                        :: i
integer                        :: ierr
integer                        :: ier
logical,save                   :: ddd=.false.
data numbers/'123456789012345678901234567890123456789012345678901234567890&
   &12345678901234567890123456789012345678901234567890123456789012345678901234&
   &56789012345678901234567890123456789012345678901234567890123456789012345678&
   &90123456789012345678901234567890123456789012345678901234567890123456789012&
   &34567890123456789012345678901234567890123456789012345678901234567890123456&
   &78901234567890123456789012345678901234567890123456789012345678901234567890&
   &12345678901234567890123456789012345678901234567890123456789012345678901234&
   &56789012345678901234567890123456789012345678901234567890123456789012345678&
   &90123456789012345678901234567890123456789012345678901234567890123456789012&
   &34567890123456789012345678901234567890123456789012345678901234567890123456&
   &78901234567890123456789012345678901234567890123456789012345678901234567890&
   &'/
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(lun))then
      lun_local=lun
   else
      lun_local=5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ipoint=iredo                                          ! initial line in history file to start with
   icall=0                                               ! flag if have been thru loop or just got here
   cin=init                                              ! initialize the directive
   ibuf=min(ibuf0,len(redoline))
   if(ibuf <= 0)return
!-----------------------------------------------------------------------------------------------------------------------------------
1  continue
   if(ipoint <= 0)then                                   ! if no lines in redo history file
      redoline=' '                                       ! make command to 'redo' a blank line since no commands entered
   else
      read(iobuf,rec=ipoint,err=999)redoline(1:ibuf)     ! get last line in history file as line to redo
      ! WARNING: OSF1 DIGITAL Fortran 77 Driver V5.2-10 DIGITAL Fortran 77 V5.2-171-428BH
      ! after this read the following storage was corrupted; switched declaration of
      ! init and redoline and problem cleared but it is probably corrupting cin and
      ! doesn't show because of logic.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   READLINE: do                                             ! display buffer and decide on command on first call or read command
      ilong=max(1,len_trim(redoline(1:ibuf)))               ! find length of command to redo
      write(*,'(a,a)')'!',redoline(:ilong)                  ! show old command
      if(icall /= 0)then                                    ! if not first call read the directive
         read(lun_local,'(a)',iostat=ios)cinbuf
         if(ios /= 0)then                                   ! if there was an I/O error reread line
            exit READLINE
         endif
         call notabs(cinbuf,cin,ilast)
      elseif(cin == ' ')then                                ! first call and no initial command passed in
         cin='l -5'                                         ! on first call do this default command if init is blank
         ilast=4
      else                                                  ! if initial command was not blank do it instead of default
         ilast=len_trim(cin)
      endif
      icall=icall+1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ilast == 0)then                                                 ! blank command line; return and execute
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      cmd=cin(1:1)
      if(ddd)call journal('d','*redol* cmd=',cmd,'options=',cin)
      select case(cmd)                                                   ! first character defines edit action
!-----------------------------------------------------------------------------------------------------------------------------------
       case(' ')                                                         ! modify the string
         call modif(redoline,cin(2:))
!-----------------------------------------------------------------------------------------------------------------------------------
       case('m')                                                         ! modify the string with line number header
         write(*,'(1x,a)',iostat=ios)numbers(:len_trim(redoline))
         call modif(redoline,cin(2:))
!-----------------------------------------------------------------------------------------------------------------------------------
       case('c','s')                                                     ! change old string to new
         call change(redoline,trim(cin(1:255)),ier)                      ! xedit-like change command
!     C/STRING1/STRING2/    OR CW/STRING1/STRING2/  (CHANGE IN WINDOW)
!     WHERE / MAY BE ANY CHARACTER OTHER THAN W OR BLANK, WHICH IS NOT
!     INCLUDED IN STRING1 OR STRING2
!-----------------------------------------------------------------------------------------------------------------------------------
       case('u','b')                                                     ! up or back through buffer
         if(cin(2:) == ' ')then
            iup=1
         else
            iup=int(s2v(cin(2:),ierr,onerr=0))
         endif
         ipoint=max(ipoint-iup,1)
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('d','f')                                                     ! down or forward through buffer
         if(cin(2:) == ' ')then
            idown=1
         else
            idown=int(s2v(cin(2:),ierr,onerr=0))
         endif
         ipoint=min(ipoint+idown,iredo)
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case(';')                                                         ! append lines
         ivals=int(s2vs(cin(2:)))
         if(allocated(ivals))then
            do i=1,size(ivals)
               ii=ivals(i)
               if(ii >= 1.and.ii <= iredo)then
                  read(iobuf,rec=ii,err=999)cinbuf(1:ibuf)               ! get last line in history file as line to redo
                  iend=len_trim(redoline)
                  redoline=redoline(:iend)//';'//trim(cinbuf)            !! should warn of truncation
               else
                  call journal('sc','*redol_* line not found in history',ii)
               endif
            enddo
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('?','h')                                                     ! display help
         call help_()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('D')                                                         ! toggle debug mode
         if(ddd .eqv. .false.)then
            ddd=.true.
            call journal('>')
         else
            ddd=.false.
            call journal('<')
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('l','p')                                                     ! display history buffer file with line numbers
         if(cin(2:) == ' ')then
            istart=iredo+1-20                                            ! default is to back up 20 lines
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(ddd)call journal('d','*redol* istart=',istart,'ierr=',ierr)
            if(ierr /= 0)istart=iredo
            if(istart < 0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         if(ddd)call journal('d','*redol* istart=',istart,'iredo=',iredo)
         do i10=istart,iredo
            read(iobuf,rec=i10,iostat=ios)redoline(1:ibuf)
            if(ios /= 0)then
               exit READLINE
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(i5.5,1x,a)',iostat=ios)i10,redoline(:ix)
            if(ios /= 0)then
               exit READLINE
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('w')                                                         ! dump to a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin == ' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_w()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('e','E')                                                     ! dump and edit history file and read it back in
          cmdline=cin(2:)                                                ! assume rest of command is a system command
          if(cmdline=='')cmdline='vim'                                   ! if no system command use "vim"
          cin='scratch.tmp'                                              ! assume this is a writable scratch file name
          cmdline=trim(cmdline)//' '//cin                                ! append scratch filename to system command
          call do_w()                                                    ! dump history file
          call execute_command_line(cmdline,cmdstat=cstat,cmdmsg=msg)    ! Execute the command line specified by the string.
          if(cstat == 0)then                                             ! rewrite or append to history file
             if(cmd == 'e')iredo=0
             call do_ar()
          endif
          open(newunit=iounit,file=cin,iostat=ios)                       ! remove scratch file
          if(ios /= 0)then
            call journal('sc','*redol_* error opening scratch file file',cin,ios,'=',msg)
          endif
          close(unit=iounit,status='delete',iostat=ios,iomsg=msg)
          if(ios /= 0)then
            call journal('sc','*redol_* error removing scratch file file',cin,ios,'=',msg)
          endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('a')                                                         ! append to history from a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin == ' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('r')                                                         ! replace history from a file
          iredo=0
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin == ' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('P','L')                                                     ! display history buffer file without line numbers
         if(cin(2:) == ' ')then                                          ! default is to go back up to 20
            istart=iredo+1-20
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(istart < 0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         do i30=istart,iredo                                             ! easier to cut and paste if no numbers
            read(iobuf,rec=i30,iostat=ios)redoline(1:ibuf)
            if(ios /= 0)then
               goto 999
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(a)',err=999)redoline(:ix)
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('/')                                                         ! display matches in buffer
         if(ilast < 2)then
            cycle
         endif
         do i20=1,iredo
            read(iobuf,rec=i20,err=999,iostat=ios)redoline(1:ibuf)
            if(index(redoline(1:ibuf),cin(2:ilast)) /= 0)then
               ix=max(1,len_trim(redoline))
               write(*,'(i5.5,1x,a)',err=999)i20,redoline(:ix)
               ipoint=i20
            endif
         enddo
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('!')                                                              ! external command
         if(ilast < 2)then
            cycle
         endif
         call execute_command_line(trim(cin(2:)),cmdstat=cstat,cmdmsg=msg)    ! Execute the command line specified by the string.
         !call system(trim(cin(2:)))                                          ! Execute the command line specified by the string.
!-----------------------------------------------------------------------------------------------------------------------------------
       case('.','q')                                                          ! blank out command and quit
         exit READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
       case default                                                           ! assume anything else is a number
         val8=s2v(cin,ierr,onerr=0)
         if(ierr == 0)then
            iread=int(val8)
         else
            iread=0
         endif
         if(iread > 0.and.iread <= iredo)then
            read(iobuf,rec=iread,err=999,iostat=ios)redoline(1:ibuf)
            ipoint=iread
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
999 continue
   redoline=' '
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine do_w()
WRITE: block
   open(newunit=idump,file=cin,iostat=ios,status='UNKNOWN',iomsg=msg)
   if(ios /= 0)then
      call journal('sc','*redol_* error opening dump file',ios,'=',msg)
      exit WRITE
   endif
   do i15=1,iredo
      read(iobuf,rec=i15,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios /= 0)then
         call journal('sc','*redol_* error reading history file',ios,'=',msg)
         exit WRITE
      endif
      ix=max(1,len_trim(redoline))
      write(idump,'(a)',iostat=ios,iomsg=msg)redoline(:ix)
      if(ios /= 0)then
         call journal('sc','*redol_* error writing dump file',ios,'=',msg)
         close(idump,iostat=ios)
         exit WRITE
      endif
   enddo
   call journal('sc','wrote file ',cin)
endblock WRITE
close(idump,iostat=ios)
end subroutine do_w
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine do_ar()
REPLACE: block
   open(newunit=idump,file=cin,iostat=ios,status='OLD',iomsg=msg)
   if(ios /= 0)then
      call journal('sc','*redol_* error opening file',ios,'=',msg)
      exit REPLACE
   endif
   do
      read(idump,'(a)',iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios /= 0)then
         if(.not.is_iostat_end(ios))then
            call journal('sc','*redol_* error reading file ',cin,ios,'=',msg)
         endif
         exit REPLACE
      endif
      iredo=iredo+1
      write(iobuf,rec=iredo,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios /= 0)then
         call journal('sc','*redol_* error writing history file',ios,'=',msg)
         exit REPLACE
      endif
   enddo
endblock REPLACE
call journal('sc','read file ',cin)
close(idump,iostat=ios)
end subroutine do_ar
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine redol_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine help_()

! ident_4="@(#) M_history help_(3fp) prints help for REDO(3f)"

character(len=80),allocatable :: usage(:)
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
usage=[ &
&' History Edit commands (where N is a number):                                   ',&
&'+______________________________________________________________________________+',&
&'|List History                           |History File:                         |',&
&'| l|p N    # list from line N.          | w   file # write history to a file   |',&
&'!          # -N shows N last lines      | a   file # append file to history    |',&
&'| L|P N    # same as l sans line numbers| r   file # replace history with file |',&
&'| /string  # search for simple string   |Return to Normal Command Mode:        |',&
&'|Position in History File:              |      # return and execute command    |',&
&'| u|b N    # up/back through buffer     | .|q  # quit and return a blank line  |',&
&'| d|f N    # down/forward through buffer|Help:                                 |',&
&'| N        # load line number           |  h|?   # display this help text      |',&
&'|System:                                |Append lines to current line:         |',&
&'| !system_command # execute command     |  ;N N N N ...                        |',&
&'|______________________________________________________________________________|',&
&'|Edit Buffer:                                                                  |',&
&'| c|s/oldstring/newstring/  # change/substitute                                |',&
&'| mmod_string               # Modify with line number header                   |',&
&'|  mod_string               # Modify (replace, delete, insert)                 |',&
&'|    #         -- deletes                                                      |',&
&'|    &         -- replaces with a blank                                        |',&
&'|    ^STRING#  -- inserts a string                                             |',&
&'|              -- blank leaves as-is                                           |',&
&'|    Any other -- replaces character                                           |',&
&'+______________________________________________________________________________+']
!-----------------------------------------------------------------------------------------------------------------------------------
   !WRITE(*,'(a)'),usage(i),i=1,size(usage))
   do i=1,size(usage)
      call journal('sc',usage(i))
   enddo
end subroutine help_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sep(input_line,delimiters,nulls)

intrinsic index, min, present, len
character(len=*),intent(in)              :: input_line
character(len=*),optional,intent(in)     :: delimiters
character(len=*),optional,intent(in)     :: nulls
character(len=:),allocatable             :: sep(:)
   call split(input_line,sep,delimiters,'right',nulls)
end function sep
subroutine split(input_line,array,delimiters,order,nulls)

intrinsic index, min, present, len
character(len=*),intent(in)              :: input_line
character(len=*),optional,intent(in)     :: delimiters
character(len=*),optional,intent(in)     :: order
character(len=*),optional,intent(in)     :: nulls
character(len=:),allocatable,intent(out) :: array(:)
integer                       :: n
integer,allocatable           :: ibegin(:)
integer,allocatable           :: iterm(:)
character(len=:),allocatable  :: dlim
character(len=:),allocatable  :: ordr
character(len=:),allocatable  :: nlls
integer                       :: ii,iiii
integer                       :: icount
integer                       :: lgth
integer                       :: i10,i20,i30
integer                       :: icol
integer                       :: idlim
integer                       :: ifound
integer                       :: inotnull
integer                       :: ireturn
integer                       :: imax
   if (present(delimiters)) then
      if(delimiters /= '')then
         dlim=delimiters
      else
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)
      endif
   else
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)
   endif
   idlim=len(dlim)
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif
   n=len(input_line)+1
   if(allocated(ibegin))deallocate(ibegin)
   if(allocated(iterm))deallocate(iterm)
   allocate(ibegin(n))
   allocate(iterm(n))
   ibegin(:)=1
   iterm(:)=1
   lgth=len(input_line)
   icount=0
   inotnull=0
   imax=0
   if(lgth > 0)then
      icol=1
      infinite: do i30=1,lgth,1
         ibegin(i30)=icol
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then
            iterm(i30)=lgth
            do i10=1,idlim
               ifound=index(input_line(ibegin(i30):lgth),dlim(i10:i10))
               if(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2
            inotnull=inotnull+1
         else
            iterm(i30)=icol-1
            icol=icol+1
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30
         if(icol > lgth)then
            exit infinite
         endif
      enddo infinite
   endif
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   allocate(character(len=imax) :: array(ireturn))
   select case (trim(adjustl(ordr)))
   case ('reverse','right') ; ii=ireturn ; iiii=-1
   case default             ; ii=1       ; iiii=1
   end select
   do i20=1,icount
      if(iterm(i20) < ibegin(i20))then
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
   end subroutine split

subroutine substitute(targetline,old,new,ierr,start,end)

character(len=*)               :: targetline
character(len=*),intent(in)    :: old
character(len=*),intent(in)    :: new
integer,intent(out),optional   :: ierr
integer,intent(in),optional    :: start
integer,intent(in),optional    :: end
character(len=len(targetline)) :: dum1
integer                        :: ml, mr, ier1
integer                        :: maxlengthout
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: ichr
   if (present(start)) then
      ml=start
   else
      ml=1
   endif
   if (present(end)) then
      mr=end
   else
      mr=len(targetline)
   endif
   ier1=0
   maxlengthout=len(targetline)
   original_input_length=len_trim(targetline)
   dum1(:)=' '
   id=mr-ml
   len_old=len(old)
   len_new=len(new)
   if(id <= 0)then
      il=1
      ir=maxlengthout
      dum1(:)=' '
   else
      il=ml
      ir=min0(mr,maxlengthout)
      dum1=targetline(:il-1)
   endif
   if(len_old == 0)then
      ichr=len_new + original_input_length
      if(ichr > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new > 0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1
      if(present(ierr))ierr=ier1
      return
   endif
   ichr=il
   ic=il
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1
      if(ind == ic-1.or.ind > ir)then
         exit loop
      endif
      ier1=ier1+1
      if(ind > ic)then
         ladd=ind-ic
         if(ichr-1+ladd > maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(ichr:)=targetline(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(ichr-1+len_new > maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new /= 0)then
         dum1(ichr:)=new(:len_new)
         ichr=ichr+len_new
      endif
      ic=ind+len_old
   enddo loop
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)
   case default
      ladd=original_input_length-ic
      if(ichr+ladd > maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic < len(targetline))then
         dum1(ichr:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
end subroutine substitute
subroutine change(target_string,cmd,ierr)

character(len=*),intent(inout)   :: target_string
character(len=*),intent(in)      :: cmd
character(len=1)                 :: delimiters
integer                          :: ierr
integer                          :: itoken
integer,parameter                :: id=2
character(len=:),allocatable     :: old,new
logical                          :: ifok
integer                          :: lmax
integer                          :: start_token,end_token
   lmax=len_trim(cmd)
   if(lmax >= 4)then
      delimiters=cmd(id:id)
      itoken=0

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id) == cmd(id+1:id+1))then
         new=old
         old=''
      else
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)
         if(end_token  ==  (len(cmd)-id+1) )end_token=len_trim(cmd(id:))
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif

      call substitute(target_string,old,new,ierr,1,len_trim(target_string))
   else
      ierr=-1
      call journal('sc','*change* incorrect change directive -too short')
   endif
end subroutine change
function strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)

character(len=*),intent(in)  :: source_string
character(len=*),intent(in)  :: delimiters
integer,intent(inout)        :: itoken
logical                      :: strtok_status
integer,intent(out)          :: token_start
integer,intent(inout)        :: token_end
integer,save                 :: isource_len
   if(itoken <= 0)then
      token_start=1
   else
      token_start=token_end+1
   endif
   isource_len=len(source_string)
   if(token_start > isource_len)then
      token_end=isource_len
      strtok_status=.false.
      return
   endif
   do while (token_start  <=  isource_len)
      if(index(delimiters,source_string(token_start:token_start))  /=  0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
   token_end=token_start
   do while (token_end  <=  isource_len-1)
      if(index(delimiters,source_string(token_end+1:token_end+1))  /=  0) then
         exit
      endif
      token_end = token_end + 1
   enddo
   if (token_start  >  isource_len) then
      strtok_status=.false.
   else
      itoken=itoken+1
      strtok_status=.true.
   endif
end function strtok
subroutine modif(cline,modi)

character(len=*)            :: cline
character(len=*),intent(in) :: modi
character(len=len(cline))   :: cmod
character(len=3),parameter  :: c='#&^'
integer                     :: maxscra
character(len=len(cline))   :: dum2
logical                     :: linsrt
integer :: i, j, ic, ichr, iend, lmax, lmx1
maxscra=len(cline)
   cmod=trim(modi)
   lmax=min0(len(cline),maxscra)
   lmx1=lmax-1
   dum2=' '
   linsrt=.false.
   iend=len_trim(cmod)
   i=0
   ic=0
   ichr=0
11 continue
   i=i+1
   if(ichr > lmx1)goto 999
   if(linsrt) then
      if(i > iend) cmod(i:i)=c(1:1)
      if(cmod(i:i) == c(1:1))then
         linsrt=.false.
         if(ic+1 == i)then
            ichr=ichr+1
            dum2(ichr:ichr)=c(1:1)
         endif
         do j=ic,i
            ichr=ichr+1
            if(ichr > lmax)goto 999
            dum2(ichr:ichr)=cline(j:j)
         enddo
         ic=i
         goto 1
      endif
      ichr=ichr+1
      dum2(ichr:ichr)=cmod(i:i)
   else
      ic=ic+1
      if(cmod(i:i) == c(1:1))goto 1
      if(cmod(i:i) == c(3:3))then
         linsrt=.true.
         goto 1
      endif
      ichr=ichr+1
      if(cmod(i:i) == c(2:2))then
         dum2(ichr:ichr)=' '
         goto 1
      endif
      if(cmod(i:i) == ' ')then
         dum2(ichr:ichr)=cline(ic:ic)
      else
         dum2(ichr:ichr)=cmod(i:i)
      endif
   endif
1  continue
   if(i < lmax)goto 11
999   continue
   cline=dum2
end subroutine modif

elemental pure function upper(str,begin,end) result (string)

character(*), intent(in)      :: str
integer, intent(in), optional :: begin,end
character(len(str))           :: string
integer                       :: i
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)
       select case (str(i:i))
       case ('a':'z')
          string(i:i) = char(iachar(str(i:i))+diff)
       end select
   enddo

end function upper
elemental pure function lower(str,begin,end) result (string)

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)
      case default
      end select
   enddo

end function lower

elemental impure subroutine notabs(instr,outstr,lgth)

character(len=*),intent(in)   :: instr
character(len=*),intent(out)  :: outstr
integer,intent(out)           :: lgth
integer,parameter             :: tabsize=8
integer                       :: ipos
integer                       :: lenin
integer                       :: lenout
integer                       :: istep
character(len=1)              :: c
integer                       :: iade
   ipos=1
   lenin=len_trim(instr( 1:len(instr) ))
   lenout=len(outstr)
   outstr=" "
      scan_line: do istep=1,lenin
         c=instr(istep:istep)
         iade=iachar(c)
         expand_tabs : select case (iade)
         case(9)
            ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
         case(10,13)
            ipos=ipos+1
         case default
            if(ipos > lenout)then
               call journal("*notabs* output string overflow")
               exit
            else
               outstr(ipos:ipos)=c
               ipos=ipos+1
            endif
         end select expand_tabs
      enddo scan_line
      ipos=min(ipos,lenout)
      lgth=len_trim(outstr(:ipos))
end subroutine notabs

subroutine a2i(chars,valu,ierr)

character(len=*),intent(in) :: chars
integer,intent(out)         :: valu
integer,intent(out)         :: ierr
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8 <= huge(valu))then
      if(valu8 <= huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
subroutine a2d(chars,valu,ierr,onerr)

character(len=*),intent(in)  :: chars
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu
integer,intent(out)          :: ierr
class(*),optional,intent(in) :: onerr
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"
character(len=15)            :: frmt
character(len=256)           :: msg
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
   ierr=0
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   call substitute(local_chars,',','')
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')
         frmt='(Z'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')
         frmt='(B'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')
         frmt='(O'//v2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu
      end select
   endif
   if(ierr /= 0)then
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars /= 'eod')then
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
doubleprecision function s2v(chars,ierr,onerr)

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
   if(present(ierr))then
      ierr=ierr_local
      s2v=valu
   elseif(ierr_local /= 0)then
      write(*,*)'*s2v* stopped while reading '//trim(chars)
      stop 1
   else
      s2v=valu
   endif
end function s2v
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
real function real_s2v(chars)
character(len=*),intent(in) :: chars
   real_s2v=real(s2v(chars))
end function real_s2v
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
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
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt /= '') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt == '') then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   else
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
      if(index(chars,'.') /= 0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local /= 0)then
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
function i2s(ivalue,fmt) result(outstr)

integer,intent(in)           :: ivalue
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
subroutine trimzeros_(string)

character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: expo
integer                      :: ipos
integer                      :: i, ii
   str=string
   ipos=scan(str,'eEdD')
   if(ipos>0) then
      expo=str(ipos:)
      str=str(1:ipos-1)
   endif
   if(index(str,'.') == 0)then
      ii=len_trim(str)
      str(ii+1:ii+1)='.'
   endif
   do i=len_trim(str),1,-1
      select case(str(i:i))
      case('0')
         cycle
      case('.')
         if(i <= 1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)
         exit
      end select
   enddo
   if(ipos>0)then
      string=trim(str)//trim(expo)
   else
      string=str
   endif
end subroutine trimzeros_
function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str
character(len=1),optional,intent(in) :: esc
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
   if(present(esc))then
      iesc=iachar(esc)
   else
      iesc=-1
   endif
   inlen=len(quoted_str)
   allocate(character(len=inlen) :: unquoted_str)
   if(inlen >= 1)then
      if(quoted_str(1:1) == single_quote)then
         quote=iachar(single_quote)
      else
         quote=iachar(double_quote)
      endif
   else
      quote=iachar(double_quote)
   endif
   before=-2
   unquoted_str(:)=''
   iput=1
   inside=.false.
   stepthrough: do i=1,inlen
      current=iachar(quoted_str(i:i))
      if(before == iesc)then
           iput=iput-1
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2
      elseif(current == quote)then
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo stepthrough
   unquoted_str=unquoted_str(:iput-1)
end function unquote
function s2vs(string,delim) result(darray)

character(len=*),intent(in)        :: string
character(len=*),optional          :: delim
character(len=:),allocatable       :: delim_local
doubleprecision,allocatable        :: darray(:)

character(len=:),allocatable       :: carray(:)
integer                            :: i
integer                            :: ier
   if(present(delim))then
      delim_local=delim
   else
      delim_local=' ;,'
   endif
   call split(string,carray,delimiters=delim_local)
   allocate(darray(size(carray)))
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)
   enddo
end function s2vs

logical function base(x,b,y,a)
implicit none
character(len=*),intent(in)  :: x
character(len=*),intent(out) :: y
integer,intent(in)           :: b,a
integer                      :: temp

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

logical function decodebase(string,basein,out_baseten)
implicit none

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: xmaxreal=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')
  if(basein == 0.and.ipound > 1)then
     call string_to_value(string_local(:ipound-1),basein_local,ierr)
     string_local=string_local(ipound+1:)
     if(basein_local >= 0)then
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  all: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else all
     out_baseten=0;y=0.0; mult=1.0
     long=len_trim(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch == '-'.and.k == 1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit all
        endif
        if(ch<='9') then
              j=iachar(ch)-iachar('0')
        else
              j=iachar(ch)-iachar('A')+10
        endif
        if(j>=basein_local)then
           exit all
        endif
        y=y+mult*j
        if(mult>xmaxreal/basein_local)then
           exit all
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif all
end function decodebase
logical function codebase(inval10,outbase,answer)
implicit none

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
        n=int(inval10_local-outbase_local*int(inval10_local/outbase_local))
        if(n<10) then
           answer=achar(iachar('0')+n)//answer
        else
           answer=achar(iachar('A')+n-10)//answer
        endif
        inval10_local=int(inval10_local/outbase_local)
     enddo
     codebase=.true.
  endif
  if(in_sign == -1)then
     answer='-'//trim(answer)
  endif
  if(answer == '')then
     answer='0'
  endif
end function codebase
function todecimal(base, instr)

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
function tobase(base, number)

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

function fmt(source_string,length)

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
   do i=1,2
      iword_max=0
      ilines=1
      ilength=0
      itoken=0
      do while ( strtok(source_string,itoken,istart,iend,delimiters) )
         iword=iend-istart+1
         iword_max=max(iword_max,iword)
         if(iword > length)then
            if(ilength /= 0)then
               ilines=ilines+1
            endif
            if(i == 2)then
               fmt(ilines)=source_string(istart:iend)//' '
            endif
            ilength=iword+1
         elseif(ilength+iword <= length)then
            if(i == 2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=ilength+iword+1
         else
            ilines=ilines+1
            ilength=0
            if(i == 2)then
               fmt(ilines)=fmt(ilines)(:ilength)//source_string(istart:iend)
            endif
            ilength=iword+1
         endif
      enddo
      if(i==1)then
         allocate(character(len=max(length,iword_max)) :: fmt(ilines))
         fmt=' '
      endif
   enddo
   fmt=fmt(:ilines)
end function fmt

function msg_scalar(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

class(*),intent(in),optional  :: generic1 ,generic2 ,generic3 ,generic4 ,generic5
class(*),intent(in),optional  :: generic6 ,generic7 ,generic8 ,generic9
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
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
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
end function msg_scalar
function msg_one(generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

class(*),intent(in)           :: generic1(:)
class(*),intent(in),optional  :: generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable   :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
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
subroutine print_generic(generic)
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
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//"]"//sep_local
end subroutine print_generic
end function msg_one

subroutine where_write_message(where,msg)

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times
character(len=3)                   :: adv

character(len=:),allocatable,save  :: prefix_template
character(len=:),allocatable       :: prefix
logical,save                       :: prefix_it=.false.
character(len=4096)                :: mssge
   adv='yes'
   prefix=''
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         endif
      case('S','s')
         write(stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      case('+'); adv='no'
      case('>'); debug=.true.
      case('<'); debug=.false.
      case('%')
         if(msg == '')then
            prefix_it=.false.
         else
            prefix_template=msg
            prefix_it=.true.
         endif
      case('N')
         if(msg /= ' '.and.msg /= '#N#'.and.msg /= '"#N#"')then
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios == 0)then
               stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg == ' ')then
            close(unit=last_int,iostat=ios)
            stdout=6
         endif
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times == 0)then
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times == 0)then
               write(stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios /= 0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
end subroutine where_write_message

subroutine where_write_message_all(where, g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,nospace)
implicit none
character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
logical,intent(in),optional   :: nospace
   call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9,nospace))
end subroutine where_write_message_all

subroutine write_message_only(message)

character(len=*),intent(in)          :: message
   call where_write_message('sc',trim(message))
end subroutine write_message_only
function str_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none
class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:), allocatable :: str_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   str_scalar=trim(line)
contains
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function str_scalar
function str_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none
class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: str_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   str_one=trim(line)
contains

subroutine print_generic(generic)
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
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*));       write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         stop 'unknown type in *print_generic*'
   end select
   line=trim(line)//"]"//sep_local
   istart=len_trim(line)+increment
end subroutine print_generic

end function str_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_history
