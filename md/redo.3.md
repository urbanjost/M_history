<?
<body>
  <a name="top" id="top"></a>

  <div id="Container">
    <div id="Content">
      <div class="c1">
      </div><a name="0"></a>

      <h3><a name="0">NAME</a></h3>

      <blockquote>
        <b>redo(3f)</b> - [M_history] Fortran-based Input History Editor <b>(LICENSE:MIT)</b>
      </blockquote><a name="contents" id="contents"></a>

      <h3>CONTENTS</h3>

      <blockquote>
        <a href="#1">Synopsis</a><br />
        <a href="#2">Description</a><br />
        <a href="#3">Options</a><br />
        <a href="#4">Usage</a><br />
        <a href="#5">Listing Command History</a><br />
        <a href="#6">Positioning To Previous Commands</a><br />
        <a href="#7">Editing The Current Buffer Line</a><br />
        <a href="#8">Help</a><br />
        <a href="#9">System Commands</a><br />
        <a href="#10">Dumping And Loading The Command History</a><br />
        <a href="#11">Example Program</a><br />
        <a href="#12">Sample Usage</a><br />
        <a href="#13">Author</a><br />
        <a href="#14">License</a><br />
      </blockquote><a name="15"></a>

      <h3><a name="15">SYNOPSIS</a></h3>

      <blockquote>
        <pre>
subroutine <b>redo</b>(inputline,r)
<br />     character(len=*) :: inputline
     character(len=1),intent(in),optional :: r
<br />
</pre>
      </blockquote><a name="2"></a>

      <h3><a name="2">DESCRIPTION</a></h3>

      <blockquote>
        the <b>redo</b>(3f) routine lets you recall, list, save, and modify previously entered program input. Built-in help is included.

        <p>The <b>redo</b>(3f) input history editor is a simple-to-use input history editor interface modeled on the CDC NOS command REDO. It uses a line
        editor model that means no special escape characters or control characters are required. Typically, only a few minutes are required to master
        usage.</p>

        <p>When using <b>redo</b>(3f) input lines are usually first read into a character variable and then passed to the routine. The returned string can
        then be parsed or read from with an internal <b>READ</b>(3f). So, for example, if you have an existing <b>READ</b>(3f) such as</p>
        <pre>
      READ(*,101) A,I,K
<br />
</pre>replace it with something similar to
        <pre>
     USE M_HISTORY,ONLY : REDO
     CHARACTER(LEN=255) :: LINE ! make variable big enough to read a line
           :
           :
     READ(*,'(A)') LINE   ! read line into character variable
     CALL REDO(LINE)      ! pass line to REDO(3f). This is a no-op except
                          ! for storing the line into the input history
                          ! unless the input line is the "r" command
     READ(LINE,101)A,I,K  ! read from variable like you did from file
</pre>
      </blockquote><a name="3"></a>

      <h3><a name="3">OPTIONS</a></h3>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" colspan="1">inputline</td>
            <td>line to record into history buffer file or to edit.</td>
          </tr>

          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">r</td>

            <td valign="bottom">Optional character to use as command to invoke editing. Defaults to 'r'.</td>
          </tr>

        </table>
      </blockquote><a name="4"></a>

      <h3><a name="4">USAGE</a></h3>

      <blockquote>
        When prompted for an input line by your program you may at any time enter "r" on a line by itself, or a line beginning with "r r_command" and you
        will enter the command history edit mode. Now you can recall and edit previous input or compose an input line using the editor commands.

        <p>By default, you will be editing the last line you entered, shifted one character to the right by an exclamation character.</p>

        <p>The character you respond with in column one controls what happens next.</p>

        <table cellpadding="3">

          <tr valign="top">
            <td width="3%">o</td>

            <td>If you enter "?" while in command edit mode, help is displayed.</td>
          </tr>

          <tr valign="top">
            <td width="3%">o</td>

            <td>If the last input line is not the desired line to edit, select the line to edit by entering its line number or by using the /,l,u, and d
            commands (see below for details) to find the desired input line.</td>
          </tr>

          <tr valign="top">
            <td width="3%">o</td>

            <td>Next enter an editing directive (c,m) to edit the selected line. The "change" command will change all occurrences of an old string to a new
            string ...</td>
          </tr>

        </table>
        <pre>
      c/old/new/
<br />
</pre>

        <table cellpadding="3">
          <tr valign="top">
            <td width="3%">o</td>

            <td>
              or the "modify" command can be used with the special characters # &amp; and ^ ...

              <table width="100%" cellpadding="3">

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>A # under a character will delete a character.</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>An "&amp;" (ampersand) will cause the character above it to be replaced with a space.</td>
                </tr>

                <tr valign="top">
                  <td class="c2" width="3%" nowrap="nowrap">o</td>

                  <td valign="bottom">To insert a string enter ^string#.</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>Otherwise, enter a character under one in the currently displayed command and it will replace it.</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>hit RETURN to start another edit of the line</td>
                </tr>

              </table>
            </td>
          </tr>

          <tr valign="top">
            <td width="3%">o</td>

            <td>Once the change is executed you will be prompted for another edit directive</td>
          </tr>

          <tr valign="top">
            <td width="3%">o</td>

            <td>You will stay in edit mode until you enter a return on a blank line to feed your line to your program; or enter "." or "q" (which means
            cancel changes and return a blank line).</td>
          </tr>

        </table>A detailed summary of the main edit-mode commands follows. In the descriptions, N stands for a number ...
      </blockquote><a name="5"></a>

      <h4><a name="5">LISTING COMMAND HISTORY</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">l|p N</td>

            <td valign="bottom">list from line N. <b>-N</b> shows N last lines</td>
          </tr>

          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">L|P N</td>

            <td valign="bottom">same as 'l' except no line numbers (for pasting)</td>
          </tr>

          <tr valign="top">
            <td class="c2" colspan="1">/string</td>
            <td>search for simple string in all history lines</td>
          </tr>

        </table>
      </blockquote>

      <p>Note that the buffer is set to the last line displayed <a name="6"></a></p>

      <h4><a name="6">POSITIONING TO PREVIOUS COMMANDS</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">u N</td>

            <td valign="bottom">up through buffer</td>
          </tr>

          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">d N</td>

            <td valign="bottom">down through buffer</td>
          </tr>

          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">N</td>

            <td valign="bottom">load line number</td>
          </tr>

        </table>
      </blockquote><a name="7"></a>

      <h4><a name="7">EDITING THE CURRENT BUFFER LINE</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" colspan="1">c/oldstring/newstring/</td>
            <td>
              change all occurrences of old string to new string. Note that s (for substitute) is a synonym for c (for change).

              <p>For the "c" directive the second character becomes the delimiter. Traditionally one usually uses a delimiter of / unless the string you are
              editing contains /.</p>
            </td>
          </tr>

          <tr valign="top">
            <td class="c2" colspan="1">mmod_string</td>
            <td>
              If the first character of your entry is m or blank,

              <table width="100%" cellpadding="3">

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>REPLACE a string by entering a replacement character under it</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>LEAVE a character alone by leaving a space under it</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>DELETE a character by putting a # character under it</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>BLANK OUT a character by putting an &amp; under it</td>
                </tr>

                <tr valign="top">
                  <td width="3%">o</td>

                  <td>INSERT A STRING by entering ^STRING#</td>
                </tr>

              </table>
            </td>
          </tr>

          <tr>
            <td colspan="2">The "modify" directive takes a little practice but this single directive accommodates positionally deleting, replacing, and
            inserting text. it is hardest using "modify" to put the strings "&amp;" and "#" into your lines. to put a # or &amp; character into a string use
            the 'c' command instead or ^&#0; or ^##.</td>
          </tr>

          <tr valign="top">
            <td class="c2" colspan="1">;N N N N ...</td>
            <td>Append specified lines to current line</td>
          </tr>

        </table>
      </blockquote><a name="8"></a>

      <h4><a name="8">HELP</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">h|?</td>

            <td valign="bottom">display help text</td>
          </tr>

        </table>
      </blockquote><a name="9"></a>

      <h4><a name="9">SYSTEM COMMANDS</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" width="6%" nowrap="nowrap">!cmd</td>

            <td valign="bottom">execute system command</td>
          </tr>

        </table>
      </blockquote><a name="10"></a>

      <h4><a name="10">DUMPING AND LOADING THE COMMAND HISTORY</a></h4>

      <blockquote>
        <table cellpadding="3">
          <tr valign="top">
            <td class="c2" colspan="1">w FILENAME</td>
            <td>write entire command history to specified file</td>
          </tr>

          <tr valign="top">
            <td class="c2" colspan="1">r FILENAME</td>
            <td>replace command history with file contents</td>
          </tr>

          <tr valign="top">
            <td class="c2" colspan="1">a FILENAME</td>
            <td>append lines from file onto command history</td>
          </tr>

        </table>
      </blockquote><a name="11"></a>

      <h3><a name="11">EXAMPLE PROGRAM</a></h3>

      <blockquote>
        Sample program
        <pre>
      program demo_redo
      use M_history, only : redo
      implicit none
      character(len=1024) ::  line
      integer             :: ios
      integer             :: cstat
      character(len=256)  :: sstat
      write(*,'(a)')                                             &amp;
      &amp; 'REDO(3f) COMMAND INPUT EDITOR',                         &amp;
      &amp; 'enter "r" or "r r_command" on the input line to go',    &amp;
      &amp; 'into history edit mode. Once in history edit mode you', &amp;
      &amp; 'may enter "?" to get some help. Enter "quit" to exit',  &amp;
      &amp; 'the program.'
      do
         write(*,'(a)',advance='no')'&gt;-&gt;'    ! write prompt
         read(*,'(a)',iostat=ios) line       ! read new input line
         ! if "r", edit and return a line from the history editor
         call redo(line) ! store into history if not "r".
         if(line.eq.'quit')stop ! exit program if user enters "quit"
         ! now call user code to process new line of data
         ! As an example, call the system shell
         call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat)
      enddo
      end program demo_redo
<br />
</pre>
      </blockquote><a name="12"></a>

      <h3><a name="12">SAMPLE USAGE</a></h3>

      <blockquote>
        <p>The example program is basically a loop that reads a command from standard input and then executes it as a subshell unless the "r" command is
        entered.</p>

        <p>Now, we will enter an <b>echo</b>(1) command followed by a few other lines of input. Then we recall the <b>echo</b>(1) command and use a few of
        the features of <b>redo</b>(3) to change and then re-execute the command.</p>
        <pre>
      &gt;echo This isss a Test
      This isss a Test
      &gt;date
      Sun May 31 23:54:09 EDT 2009
      &gt;pwd
      /cygdrive/c/urbanjs/MYCYGWIN/DISKA/public_html/public/CLONE/REDO
      &gt;r                            ! enter edit mode
      00001 echo This isss a Test   ! last commands are displayed
      00002 date
      00003 pwd
      !pwd
      &gt;1                            ! go to first line in history
      !echo This isss a Test
                   ##   t           ! delete and replace characters
      !echo This is a test          ! insert a string
                      ^new #
      !echo This is a new test
      c/test/TEST/                  ! change a substring
      !echo This is a new TEST
                         &amp;          | replace character with spaces
      !echo This is a newTEST
                                    ! a blank line ends editing
      This is a newTEST
      &gt;quit
<br />
</pre>
      </blockquote><a name="13"></a>

      <h3><a name="13">AUTHOR</a></h3>

      <blockquote>
        John S. Urban, 1988,2009,2011,2015 (last change: Nov 2019)
      </blockquote><a name="14"></a>

      <h3><a name="14">LICENSE</a></h3>

      <blockquote>
        MIT
      </blockquote>
      <hr />

      <br />

      <div class="c1"><img src="images/redo.3.gif" /></div>
    </div>
  </div>
</body>
</html>
