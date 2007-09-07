Table of contents

       1.0 Kit installation instructions
           1.1 Briefly
           1.2 Unpacking
           1.3 Quick install -- personal
              1.3.1 Activating the packages
              1.3.2 What next? Where I put other lisp packages?
           1.4 Quick install -- site wide
           1.5 Install from scratch -- without prior Emacs lisp package directory
              1.5.1 Preconditions for Unix
              1.5.2 Preconditions for Win32
           1.6 Autoload files
           1.7 Minimum installation
           1.8 Optional step - make procedure
           1.9 Makefile.mak (obsolete)
           1.10 Submitting bug reports

       2.0 Project information
           2.1 What does prefix "tiny" mean
           2.2 Thank you section

       3.0 Further reading -- about loading packages
           3.1 Dynamic byte compilation note
           3.2 Use autoloads
           3.3 Customizing packages

       4.0 Appendix A - Win32 and Unix beginners
           4.1 Prompt syntaxes
           4.2 Dos and Unix command equivalences
           4.3 Calling perl programs
           4.4 Changing environment settings

1.0 Kit installation instructions

    1.1 Briefly

        #T2HTML-TITLE Emacs Tiny Tools Project README
        #T2HTML-METAKEYWORDS Emacs, Emacs Lisp, Tiny Tools Documentation
        #T2HTML-OPTION --css-code-bg
        #T2HTML-OPTION --css-code-note=Note:

        #t2html-comment http://www.xemacs.org/Documentation/packageGuide.html

        Copyright (C) 1995-2007 Jari Aalto

        License: This material may be distributed only subject to the
        terms and conditions set forth in the Open Publication
        License; either version 1.0, or (at your option) any later
        version. The copyright holder does not exercise any of the
        "LICENSE OPTIONS" that may be listed in the license. Visit
        <http://opencontent.org/openpub/>.

        Emacs Tiny Tools is a collection of libraries and packages,
        which are designed to be OS and X/Emacs platform independent.
        E.g. configure load-path automatically, URL handler,
        easy-delete, mail complete and many more. This file is
        README.txt included in Emacs Tiny Tools kit. It contains
        instructions how to install a cross platform Emacs/XEmacs/
        Unix/Win32 compatible utilities for wide variety of tasks:
        diff/patch; searching, replacing, caching files, automatic
        load-path setup and more.

    1.2 Unpacking

        Extract the kit and it will create directory according to the
        version number.

            $ mkdir -p $HOME/elisp/packages/
            $ mkdir -p $HOME/elisp/config       # configuration settings
            $ mkdir -p $HOME/tmp                # for temporary files
            $ cd $HOME/elisp/packages/
            $ gzip -dc ~/tmp/download/emacs-tiny-tools*.gz | tar xvf -

        For Win32 platform, a good free archive program is available at
        <http://www.ultimatezip.com/>.

    1.3 Quick install -- personal

       1.3.1 Activating the packages

        If you just want to install the package for your personal use, add
        following lines at the start of your Emacs initialization file.
        These lines below will actually configure your Emacs to be aware of
        *all* lisp paths under `$HOME/elisp' or `$HOME/lisp', so whatever
        magic you used to do to keep your `load-path' up to date is no
        longer needed. The Lisp package path variable `load-path' is
        periodically kept up to date without manual tweaking.

            ;; $HOME/.emacs
            ;; This line loads package "tinypath.el". Notice, no file
            ;; extension ".el" is used, it's implicit.

            (load "~/elisp/packages/tiny-tools/lisp/tiny/tinypath")

            ;; The rest of the code can be anywhere in startup file.

            ...

            ;;   See  M-x tiny-setup-display  and  C-h f tiny-setup

            (require 'tiny-setup)
            (tiny-setup nil)

            ;; Perhaps you would like to load some package immediately.

            (require 'tinymy)

            ;; End of example

       1.3.2 What next? Where do I put other lisp packages?

        When the `tinypath.el' package is active, you can pretty much put
        packages or individual lisp files anywhere, provided that they
        reside under *$HOME/elisp*. you can create or delete directories
        and move files around to organize your lisp files. The only
        restriction is that you do not use any symlinks; not to directories
        or not to files (reason is explained in file tinypath.el).
        Let's suppose the hierarchy of lisp files look something like this:

            $HOME/elisp/
                        |
                        +-packages/
                        |         |
                        |         + emacs-tiny-tools-NNNN.NNNN/
                        |         + jdee-NN.NN/
                        +-other/
                        +-my/

        Now, perhaps you see an *utility.el* in gnu.emacs.sources that you
        would like to use. How do you install it? Just drop it in, say,
        `$HOME/elisp/other/' directory and call `M-x'
        `tinypath-cache-regenerate'. After that Emacs knows the package now
        and in next Emacs sessions. You still have to add relevant lines to
        your Emacs startup which make the package active. Look inside
        *utility.el* and its setup instructions, something like:

            # $HOME/.emacs

            <leave those Tiny tools definitions here>

            (load "utility")    ;; Start using the new file
            (utility-install)   ;; or whatever is needed to activate it

            # End of file

    1.4 Quick install -- site wide

        If you are owner of your own workstation or administrate the
        current host and you have both XEmacs and Emacs installations; plus
        you have lot of extra Emacs packages that are available form the
        Net, you need to tell the _different_ locations where the installed
        lisp directories are. In this case, you have to set the
        `tinypath-:load-path-root'. In previous personal installation
        example, this was automatically determined.

        After the code below, user have access to all packages installed
        to your system, no matter where they reside. The `load-path'
        is configured porperly at Emacs startup and you, the admin, can
        concentrate on getting your hand on the most prospective
        lisp packages. Just drop them anywhere, move them around, the
        changes will be noticed automatically.

            ;;  Your lisp/site-start.el should contain this installation code.
            ;;
            ;;  List of ROOT directories where all Emacs lisp
            ;;  files are located. Update list according to your site.
            ;;  No need to optimize away non-existing directories,
            ;;  because they will be ignored.

            (setq tinypath-:load-path-root
              (list
                (if (boundp 'xemacs-logo)               ;; ROOT DIR ONE
                    "The-XEmacs-root/lisp"
                  "The-Emacs-root/lisp")
                (if (boundp 'xemacs-logo)               ;; ROOT DIR TWO
                    "Your-XEmacs-site-list-directory"
                  "Your-Emacs-site-list-directory")
                "~/elisp"                               ;; ... and so on
                "~/lisp"))

            (load "/path/to/tinypath.el")

            ;; End of example

    1.5 Install from scratch -- without prior Emacs lisp package directory

       1.5.1 Preconditions for Unix

        If you don't have your private Emacs lisp directory yet, it's
        time to create one. Decide where you want to put future downloaded
        packages. Traditionally this has been `$HOME/elisp'. Run command:

            $ mkdir -p $HOME/elisp/config  # for configuration settings
            $ mkdir -p $HOME/tmp           # for temporary files

       1.5.2 Preconditions for Win32

        In Windows operating system, there is no default HOME, so you have
        to choose one directory for your personal use. This might be
        something like:

            C:/emacs/emacs-20.7.1/      # some version of Emacs
            C:/emacs/emacs-21.3/        # newer Emacs
            C:/home/elisp               # your private lisp package dir

            dos> c:
            dos> cd \
            dos> md \home
            dos> md \home\elisp
            dos> md \home\elisp\config

        At minimum, you must create environment variable *HOME* and make
        your Win98/NT/W2K/XP see it at boot time. In Win9x, add this line
        to your `c:\autoexec.bat':

            set HOME=c:\home

        In other Windows versions, use Control Panel => System =>
        Environment. Make sure you also create the directory to disk. Refer
        to NT Emacs FAQ at
        <http://www.gnu.org/software/emacs/windows/ntemacs.html> for more
        complete information about installing Win32 Emacs. After this line,
        you Emacs translates tilde(~) character into reference to the
        $HOME. Your Emacs startup file must reside at directory
        `$HOME/.emacs' which is the traditional way of saying "Emacs
        startup file is under HOME". In win32, this means that the location
        would be:

            c:\home\.emacs

    1.6 Autoload files

        If you choose not to use file *tiny-setup.el* and function
        `tiny-setup', you could add following statements to your Emacs
        startup file in `$HOME/.emacs'. The autoload file provide
        "functions" that are immediately available in Emacs, but they do
        not load any packages. This is better than using `require'
        commands, because it makes your Emacs start faster.

            ;; $HOME/.emacs

            (load "/path/to/tinypath.el")

            ;;  These won't modify anything in your existing Emacs.

            (require 'tiny-autoload-loaddefs-tiny)
            (require 'tiny-autoload-loaddefs-other)

            ;;  Now, configure anything as you would like.
            ;;  Each feature must be taken into use separately.
            ;;  Read the documentation sections at the beginning of each file.

            (autoload 'turn-on-tinyperl-mode "tinyperl" "" t)
            (add-hook 'perl-mode-hook  'turn-on-tinyperl-mode)
            (add-hook 'pcerl-mode-hook 'turn-on-tinyperl-mode)

            ;; End of $HOME/.emacs

    1.7 Minimum installation

        If you are tight of space or otherwise do not need all the tools or
        if you're only interested in specific package, here are the
        instructions how you use minimum installation. The basic idea is to
        include libraries + package(s). Most of the packages require only
        four core libraries, but others may need more e.g. due to XEmacs
        compatibility.

            tinyliba.el     - autoloads
            tinylibb.el     - backward compatibility; Emacs version changes
            tinylibm.el     - macros and other low level forms
            tinylib.el      - main library
            tinylibxe.el    - XEmacs support library

            + NOW select packages that you're interested in.

    1.8 Optional step - make procedure

        FIXME: This will change future release.

          Note: YOU MUST CONFIGURE `load-path.el' in the kit if you
          compile the packages. _Save_ _your_ _modified_ _copy_ for
          next installation and use the `-l' switch for the perl
          *makefile.pl*. It is not strictly needed to compile
          anything, because all lisp files continue to work uncompiled.

        Due to platform independence, Perl has been chosen for all
        scripting tasks. It offers more expressive power than `make(1)'
        which is old Unix-only solution. The older make compatible
        "makefile.mak" method is no longer supported due to its vulnerable
        syntax (old time Makefile users know what a missing TAB causes).
        The old makefile.mak file is preserved for educational purposes
        which you can take a look if you need makefile example for
        programming languages like Java or C++.

        You need Unix Perl 5.004+ in order to use the current build method
        Perl for Windows is included in Cygwin. To build the kit, change
        directory to `bin/' and feed the *makefile.pl* to perl. In Win32
        platform, add make directive `unix2dos' and in Unix add `dos2unix'
        which will fix the line endings. Substitute option `emacs' with
        `xemacs' if you're compiling for XEmacs.

            $ cd bin/
            $ perl makefile.pl --help
            $ perl makefile.pl --binary emacs --verbose 2  dos2unix all
                               |                           +======= ===
              All command line options must                |
              come BEFORE build options                    |
                                                           |
                                      Two build options here

        In case everything does not compile right, due to missing lisp
        packages, modify `load-path.el' to include any additional
        directories. Make a copy of it to yourself for later updates and
        supply your version with added option. The "\" at the end of line
        means that the line continues. In Unix, you literally type that
        character, in Win32, you just type everything as one line.

            $  perl makefile.pl --binary emacs --verbose 2  \
               --load your-modified-copy-here/load-path.el  \
               dos2unix all

        _Gnus_ _note:_ packages *tinygnus.el* and *tinymail.el* require the
        very latest development version of Gnus. Modify your copy of
        `load-path.el' to include path to the latest Gnus version.

    1.9 Makefile.mak (obsolete)

        The old *makefile.mak* from very early Unix-only releases has been
        included, but it is not used and no fixes are
        incorporated even if suggested. The file has been kept in the
        distribution, because it may have some educational value to learn
        makefiles in Unix.

    1.10 Submitting bug reports

        IT IS IMPORTANT THAT YOU USE NON-COMPILED FILES AND SUBMIT THE
        *BACKTRACE* Each lisp package has one or two contact functions.
        When you find an error, immediately contact maintainer with the
        bug report function. The `XXX' is prefix of the package, like
        `tinyurl' for *tinyurl.el*

            M-x load-library RET package.el RET    # load non-compiled
            M-x turn-on-emacs-debug RET            # From tinyliba.el

            ...  repeat what you did until the point of error ...

            M-x XXX-submit-bug-report       << prefer this if it exists >>
            M-x XXX-submit-feedback

            ... and copy paste any relevant information, like the lisp
            ... error buffer *Backtrace* that might have been generated.

2.0 Project information

    2.1 What does prefix "tiny" mean

        The first package made was *tinyappend.el*, a simple utility to
        gather selected areas of text somewhere convenient. Since then the
        prefix "tiny" has been preserved in front of every lisp file. The
        word has no other meaning.

    2.2 Thank you section

        The life with Emacs evolved in the direction called "Tiny Tools";
        modules that got initially written in the past years starting with
        Emacs 18.59 and after that with 19.28. The current code is aimed to
        be XEmacs/Emacs Unix/Win32 platform independent. The cross platform
        could not be possible without following testers that patiently kept
        reporting bugs and making improvement suggestions. Most sincere
        thanks to you all.

        o   Henk SPG <tank AT xs4all.nl> who had courage to take early v1.6x
            TinyPgp in XEmacs. I got very good feedback and bug tracing
            assistance from him.
        o   Samuel Tardieu <sam AT inf.enst.fr> has been a key figure
            in  testing the PGP interfaces with 2.6.x and 5.x in Unix.
        o   Dan Rich <drich AT cisco.com> who assisted me with solving the
            XEmacs 20-21.x incompatibility problems. The correction cycle
            rolled out changes that caused rewriting my other packages too.
            In addition he suggested many other new features I didn't think
            of before. He cleared many problems by talking to XEmacs 20.x
            maintainers and if that's not all, he also made sure the
            packages worked in VM. I'm very happy that Dan came into
            picture.
        o   Sami Khoury <skhoury AT omnisig.com> tried every new tiny tools
            release and reported any byte compiler errors with latest Emacs
            releases. He also suggested many improvements, many new
            features to add and sent patches. Without Sami, I would have
            not been able to know how things work in Emacs 20.x and 21.x.
            Sami was the ears and eyes in the bleeding edge Emacs
            development till the FSF opened their Emacs CVS server.
        o   David x callaway <dxc AT xprt.net> joined the team somewhere 2000-10
            and kept asking, suggesting and reporting improvements in rapid
            fashion. That's what is needed to make all tools better. Thank
            you David, Don't stop bugging the maintainer *smile*.
        o   Luis Miguel Hernanz Iglesias <luish AT germinus.com> got
            interested in 2000-10 and was fascinated with the TinyPath
            (new utility) which made Emacs lisp package path configuration
            a joy. He joined the development and sent lof of patches and
            squeezed many bugs from TinyPath. I'm impressed!

3.0 Further reading -- about loading packages

    3.1 Dynamic byte compilation note

          Please read these instructions with some salt, because the
          they may not be 100% accurate for later Emacs versions.
          Feel free to suggest corrections to this text.

        All the files in Tiny Tools kit turn on the dynamic byte
        compilation flag, which appeared in Emacs 19.29 and XEmacs 19.14.
        That's something you don't have to worry about, but I think you are
        would be interested in hearing, that it has some nice benefits over
        the normal compilation. If you say

            (require 'tinylibm)

        the statement used to mean "load library tinylibm". All functions
        were imported into emacs in a whip. But that's  different now with
        dynamically compiled files. Now it means "load STUBS from library
        tinylibm". The STUBS are pointers in a table to all the rest of the
        functions in the library. So, the functions are not actually loaded
        to your Emacs; neither does your Emacs suffer from sudden memory
        bloat.

        When the actual function is called, Emacs automatically substitutes
        the STUB with the function definition by loading it from the
        library. Just that function, not the others. That's the benefit of
        the dynamic byte compilation in a nutshell.

        By the way, if you happen to need some function from the libraries,
        don't do this:

            (require 'tinylimt)     ;; mail tools

        _Do_ _not_ _do_ _this_ either, if you need something from the main
        library:

            (require 'tinylib)

        The correct way to let your emacs know about all the existing
        library functions, is to load the *m* library. It will define
        all the necessary autoloads to rest of the function and you don't
        have to worry about the location of a particular function

            ;;  Define everything, publish interfaces. Defines
            ;;  backward compatibility functions, autoloads etc.
            (require 'tinylibm)

    3.2 Use autoloads

        When you install packages, try to avoid loading them from your
        $HOME/.emacs like this

            (require 'tinytab)  ;; TAB minor mode

        Because it means that the whole package is loaded (or STUBS) if you
        do have many `require' commands, your Emacs startup time grows
        proportionally. You hate slow Emacs startup; right? Okay, there is
        much better way to load files; called `autoload'. It means that you
        tell Emacs; that "function exists in package xxx". When function or
        feature is called (and the autoload triggers), Emacs loads function
        from the package. Some day you don't use some feature in your emacs,
        sometimes you use, and Emacs grows and loads the packages when you
        happen to need them. No need to use `require' to have it all at once.

        Now, if you read carefully the *installation* section from every
        package and prefer copying the autoload setup instead, you will
        have the most optimized way to install the package to your .emacs.
        If some package doesn't have autoload choice, complain immediately
        to the Author. The package writer should have thought the autoload
        possibility too. There may also be a note that package can't be
        autoloaded, so in that case there is no other possibility that to
        `require' it. For even more slicker and faster Emacs startup
        sequence, see package *TinyLoad* which you can uset to optimise
        lisp file loadings even more.

    3.3 Customizing packages

        New Emacs releases 19.34+ come with the package *defcustom.el*
        bundled. If you have limited lisp skills or if you want an
        easy interface to packages' variables, then you can customize
        the modules. The *group* is completed when you press `tab'
        key:

            M-x customize-group RET group RET

4.0 Appendix A - Win32 and Unix beginners

    4.1 Prompt syntaxes

        If you're just starting to use Windows or have never heard of Unix,
        here is a short course of the terms and lingo you see all the time
        in the documentation:

            $       MEANS: the command prompt, a bash/korn/zsh shell
            %       MEANS: the command prompt, csh/tcsh shell
            >       MEANS: the command prompt, again some other shell (dos)

    4.2 Dos and Unix command equivalences

        The shell (DOS in Win32) where you type the command does not usually
        matter, but the command you see may not be available in Win32. A crash
        course would be:

            Unix    In Windows
            -----   ------------------------------------------------------
            cp      MEANS  "copy" command
            mv      MEANS  move, which is combination of "copy" and "del"
            ls      MEANS  list, which is "dir"
            rm      MEANS  remove, which is "del"
            ln      MEANS  link, Sorry, you're out of luck in Win32.
                           --> do a complete tree or file "copy" to destination.
            chmod   MEANS  change modes, ignore these in Win32
            mkdir   MEANS  make directory, "md"
            --------------------------------------------------------------

        Here is an example of one instruction, similar to what you can expect.
        The explanation has been "opened" for you to the right hand in
        double quotes.

            % cp file1.txt file2.txt    "Copy file1.txt to file2.txt"

    4.3 Calling perl programs

        In Win32, the perl file (.pl) must be called differently than in
        Unix. In Unix you must make the files executable and it can be
        called directly using the name.

            $ export PATH=$PATH:$HOME/bin   "Here are the programs"
            $ mkdir $HOME/bin       "Put all your programs here"
            $ chmod +x bin/*        "Make all files in bin/* executable"
            $ mywebget.pl --help    "Run the perl program from bin/"

        In Windows, there is no such command as `chmod' and Windows does
        not know that perl scripts are runnable programs. You must prepend
        the perl interpreter in front of every call an possibly add `-S' to
        instruct to search the script along your PATH variable. The
        following calls would also work in any environment:

            dos> perl t2html.pl --help     (Notice "perl" at front)
            dos> perl -S t2html.pl --help  (If you put the script along PATH)

    4.4 Changing environment settings

        The environment variables in Unix are prefixed with dollar and
        written in all caps, like talking about $PATH. Read it like this:

            $ echo $PATH

        It's the same variable in Windows, but the syntax to show it
        is different:

            dos> echo %PATH%

        If you're instructed to change any environment variables, you must
        open the `c:\autoexec.bat' in Win9x with some text editor and
        modify the contents. In WinME/NT/2000/XP+ you must edit the `Start'
        => `Control' `Panel' => `System' => `Environment'.

End of file
