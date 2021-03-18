!>
!!##NAME
!!    redo(3f) - [M_history] Fortran-based Input History Editor
!!    (LICENSE:PD)
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
!!          if(line.eq.'quit')stop ! exit program if user enters "quit"
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
!!    Public Domain
!===================================================================================================================================
module M_history
!
!  Acting much like a line-mode editor, the REDO(3f) procedure lets
!  you list, edit, save, and modify your interactively entered program
!  input. Built-in help and no dependence on terminal control sequences
!  makes this a simple-to-master and portable input history editor.
!
implicit none
private

public  :: redo                    !  copy a line into history file or edit history if command is "r" and return line

private :: open_history_           !  open history file
private :: redol_                  !  edit history
private :: help_                   !  produce help text for redo(3f) usage

!  should use unused file, not just unit 1071 for history
!  add option to read in and replace history file

integer,parameter :: READLEN=1024  ! width of history file

interface v2s
   module procedure d2s, r2s, i2s, l2s
end interface

interface string_to_value
   module procedure a2d, a2r, a2i
end interface
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine redo(inputline,r,lun)
!      if line starts with r word call redol_()
!      uses unit 1071
!       r
!       r string

character(len=*),parameter::ident_1="@(#)M_history::redo(3f): open binary direct access file for keeping history"

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
      if(ioparc.ne.0)then
         write(*,*)'sc','error creating history file'
         return
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilast=len_trim(inputline)

   if(ilast.eq.1.and.inputline(1:1).eq.r_local)then                             ! redo command
      call redol_(inputline,iobuf,iredo,READLEN,' ',lun)
      ilast=len_trim(inputline)
   elseif(inputline(1:min(2,len(inputline))).eq.r_local//' ')then               ! redo command with a string following
      call redol_(inputline,iobuf,iredo,READLEN,inputline(3:max(3,ilast)),lun)
      ilast=len_trim(inputline)
   endif

   if(ilast.ne.0)then                                                           ! put command into redo buffer
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

character(len=*),parameter::ident_2="@(#)M_history::open_history_(3fp): open history file for REDO(3f) procedure"

integer,intent(in)          :: iunit   ! Fortran unit to open
character(len=*),intent(in) :: fname   ! filename to open
character(len=*),intent(in) :: sname   ! flag. If "scratch" ignore FNAME and open a scratch file
integer,intent(out)         :: ierr    ! error code returned by opening file
character(len=1024)         :: msg
!-----------------------------------------------------------------------------------------------------------------------------------
  if(sname.eq.'scratch')then
     open(unit=iunit,status='scratch',form='unformatted',access='direct',recl=READLEN,iostat=ierr,iomsg=msg)
  else
     open(unit=iunit,file=trim(fname),status=trim(sname),form='unformatted',access='direct',recl=READLEN,iostat=ierr,iomsg=msg)
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
  if(ierr.ne.0)then
     write(*,*)'*open_history_* open error ',ierr,'=',trim(msg)
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

character(len=*),parameter::ident_3="@(#)M_history::redoline(3fp): redo a previous input line"

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
integer                        :: ilen
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
   if(ibuf.le.0)return
!-----------------------------------------------------------------------------------------------------------------------------------
1  continue
   if(ipoint.le.0)then                                   ! if no lines in redo history file
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
      ilen=max(1,len_trim(redoline(1:ibuf)))                ! find length of command to redo
      write(*,'(a,a)')'!',redoline(:ilen)                   ! show old command
      if(icall.ne.0)then                                    ! if not first call read the directive
         read(lun_local,'(a)',iostat=ios)cinbuf
         if(ios.ne.0)then                                   ! if there was an I/O error reread line
            exit READLINE
         endif
         call notabs(cinbuf,cin,ilast)
      elseif(cin.eq.' ')then                                ! first call and no initial command passed in
         cin='l -5'                                         ! on first call do this default command if init is blank
         ilast=4
      else                                                  ! if initial command was not blank do it instead of default
         ilast=len_trim(cin)
      endif
      icall=icall+1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ilast.eq.0)then                                                 ! blank command line; return and execute
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      cmd=cin(1:1)
      if(ddd)write(*,*)'d','*redol* cmd=',cmd,'options=',trim(cin)
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
         call change(redoline,cin(1:255),ier)                            ! xedit-like change command
!     C/STRING1/STRING2/    OR CW/STRING1/STRING2/  (CHANGE IN WINDOW)
!     WHERE / MAY BE ANY CHARACTER OTHER THAN W OR BLANK, WHICH IS NOT
!     INCLUDED IN STRING1 OR STRING2
!-----------------------------------------------------------------------------------------------------------------------------------
       case('u','b')                                                     ! up or back through buffer
         if(cin(2:).eq.' ')then
            iup=1
         else
            iup=int(s2v(cin(2:),ierr,onerr=0))
         endif
         ipoint=max(ipoint-iup,1)
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('d','f')                                                     ! down or forward through buffer
         if(cin(2:).eq.' ')then
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
               if(ii.ge.1.and.ii.le.iredo)then
                  read(iobuf,rec=ii,err=999)cinbuf(1:ibuf)               ! get last line in history file as line to redo
                  iend=len_trim(redoline)
                  redoline=redoline(:iend)//';'//trim(cinbuf)            !! should warn of truncation
               else
                  write(*,*)'*redol_* line not found in history',ii
               endif
            enddo
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('?','h')                                                     ! display help
         call help_()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('l','p')                                                     ! display history buffer file with line numbers
         if(cin(2:).eq.' ')then
            istart=iredo+1-20                                            ! default is to back up 20 lines
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(ierr.ne.0)istart=iredo
            if(istart.lt.0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         do i10=istart,iredo
            read(iobuf,rec=i10,iostat=ios)redoline(1:ibuf)
            if(ios.ne.0)then
               exit READLINE
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(i5.5,1x,a)',iostat=ios)i10,redoline(:ix)
            if(ios.ne.0)then
               exit READLINE
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('w')                                                         ! dump to a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_w()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('e','E')                                                     ! dump and edit history file and read it back in
          cmdline=cin(2:)                                                ! assume rest of command is a system command
          if(cmdline=='')cmdline='vim'                                   ! if no system command use "vim"
          cin='scratch.tmp'                                              ! assume this is a writeable scratch file name
          cmdline=trim(cmdline)//' '//cin                                ! append scratch filename to system command
          call do_w()                                                    ! dump history file
          call execute_command_line(cmdline,cmdstat=cstat,cmdmsg=msg)    ! Execute the command line specified by the string.
          if(cstat.eq.0)then                                             ! rewrite or append to history file
             if(cmd.eq.'e')iredo=0
             call do_ar()
          endif
          open(newunit=iounit,file=cin,iostat=ios)                       ! remove scratch file
          if(ios.ne.0)then
            write(*,*)'*redol_* error opening scratch file file',trim(cin),ios,'=',trim(msg)
          endif
          close(unit=iounit,status='delete',iostat=ios,iomsg=msg)
          if(ios.ne.0)then
            write(*,*)'*redol_* error removing scratch file ',trim(cin),ios,'=',trim(msg)
          endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('a')                                                         ! append to history from a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('r')                                                         ! replace history from a file
          iredo=0
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('P','L')                                                     ! display history buffer file without line numbers
         if(cin(2:).eq.' ')then                                          ! default is to go back up to 20
            istart=iredo+1-20
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(istart.lt.0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         do i30=istart,iredo                                             ! easier to cut and paste if no numbers
            read(iobuf,rec=i30,iostat=ios)redoline(1:ibuf)
            if(ios.ne.0)then
               goto 999
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(a)',err=999)redoline(:ix)
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('/')                                                         ! display matches in buffer
         if(ilast.lt.2)then
            cycle
         endif
         do i20=1,iredo
            read(iobuf,rec=i20,err=999,iostat=ios)redoline(1:ibuf)
            if(index(redoline(1:ibuf),cin(2:ilast)).ne.0)then
               ix=max(1,len_trim(redoline))
               write(*,'(i5.5,1x,a)',err=999)i20,redoline(:ix)
               ipoint=i20
            endif
         enddo
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('!')                                                              ! external command
         if(ilast.lt.2)then
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
         if(ierr.eq.0)then
            iread=int(val8)
         else
            iread=0
         endif
         if(iread.gt.0.and.iread.le.iredo)then
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
   if(ios.ne.0)then
      write(*,*)'*redol_* error opening dump file',ios,'=',trim(msg)
      exit WRITE
   endif
   do i15=1,iredo
      read(iobuf,rec=i15,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         write(*,*)'*redol_* error reading history file',ios,'=',trim(msg)
         exit WRITE
      endif
      ix=max(1,len_trim(redoline))
      write(idump,'(a)',iostat=ios,iomsg=msg)redoline(:ix)
      if(ios.ne.0)then
         write(*,*)'*redol_* error writing dump file',ios,'=',trim(msg)
         close(idump,iostat=ios)
         exit WRITE
      endif
   enddo
   write(*,*)'wrote file',trim(cin)
endblock WRITE
close(idump,iostat=ios)
end subroutine do_w
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine do_ar()
REPLACE: block
   open(newunit=idump,file=cin,iostat=ios,status='OLD',iomsg=msg)
   if(ios.ne.0)then
      write(*,*)'*redol_* error opening file',ios,'=',trim(msg)
      exit REPLACE
   endif
   do
      read(idump,'(a)',iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         if(.not.is_iostat_end(ios))then
            write(*,*)'*redol_* error reading file ',cin,ios,'=',trim(msg)
         endif
         exit REPLACE
      endif
      iredo=iredo+1
      write(iobuf,rec=iredo,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         write(*,*)'*redol_* error writing history file',ios,'=',trim(msg)
         exit REPLACE
      endif
   enddo
endblock REPLACE
write(*,*)'read file',trim(cin)
close(idump,iostat=ios)
end subroutine do_ar
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine redol_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine help_()

character(len=*),parameter::ident_4="@(#)M_history::help_(3fp): prints help for REDO(3f)"

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
      write(*,*)usage(i)
   enddo
end subroutine help_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! COPIES OF OTHER MODULE ROUTINES
!===================================================================================================================================
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
      write(*,*)'*change* incorrect change directive -too short'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine change
!===================================================================================================================================
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
!===================================================================================================================================
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
               write(*,*)"*notabs* output string overflow"
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
!===================================================================================================================================
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
!----------------------------------------------------------------------------------------------------------------------------------
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
         write(*,*)'*substitute* new line will be too long'
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
      write(*,*)'*substitute* new line will be too long'
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(ichar+ladd.gt.maxlengthout)then
         write(*,*)'*substitute* new line will be too long'
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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
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
         write(*,*)'*value_to_string* UNKNOWN TYPE'
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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
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
         write(*,*)'*a2r*','- value too large',valu8,'>',huge(valu)
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
         write(*,*)'*a2i*','- value too large',valu8,'>',huge(valu)
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
         write(*,*)'*a2d* - cannot produce number from string ['//trim(chars)//']'
         if(msg.ne.'')then
            write(*,*)'*a2d* - ['//trim(msg)//']'
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_history
