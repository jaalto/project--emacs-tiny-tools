#!/usr/bin/perl
#
# makefile.pl -- Makefile for Tiny Tools kit
#
#  File id
#
#	Copyright (C) 2000-2008 Jari Aalto
#
#	This program is free software; you can redistribute it and or
#	modify it under the terms of the GNU General Public License as
#	published by the Free Software Foundation; either version 2 of
#	the License, or (at your option) any later version.
#
#	This program is distributed in the hope that it will be useful, but
#	WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#	General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with program. If not, write to the
#	Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
#	Boston, MA 02110-1301, USA.
#
#	Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#   Description
#
#	Call program with
#
#	    % perl makefile.pl --help
#	    % perl makefile.pl --verbose 2 --test all
#	    % perl makefile.pl all

use 5.004;
use strict;
use English;
use Getopt::Long;
use Cwd;
use File::Basename;
use File::Path;
use Pod::Text;
use Pod::Html;

IMPORT:
{
    use Env;
    use vars qw
    (
	$PATH
	$SHELL
	$HOME
	$TEMP
	$TEMPDIR
    );

    use vars qw ( $VERSION );

    #	This is for use of Makefile.PL and ExtUtils::MakeMaker
    #	So that it puts the tardist number in format YYYY.MMDD
    #	The REAL version number is defined later

    #	The following variable is updated by my Emacs setup whenever
    #	this file is saved

    $VERSION = '2007.0905.2137';
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Set global variables for the program
#
#   INPUT PARAMETERS
#
#	none
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub Initialize ()
{
    use vars qw
    (
	$debug

	$PROGNAME
	$LIB

	$FILE_ID
	$VERSION
	$CONTACT
	$URL
	$NAME
	$AUTHOR
	$WIN32
	$CYGWIN_PERL

	$PATH
	@PATH
    );

    $PROGNAME	= "makefile.pl";
    $LIB	= $PROGNAME;
    my $id	= "$LIB.Initialize";

    $FILE_ID  = q$Id: makefile.pl,v 2.39 2007/05/01 17:20:31 jaalto Exp $; # m:
    # $VERSION	= (split (' ', $FILE_ID))[2];
    $CONTACT  = "";
    $NAME     = "Jari Aalto";
    $AUTHOR   = "Jari Aalto";
    $URL      = "http://tiny-tools.sourceforge.net/";

    $WIN32    = 1   if	$OSNAME =~ /win32/i;

    $OUTPUT_AUTOFLUSH = 1;

    if ( $OSNAME =~ /cygwin/i )
    {
	# We need to know if this perl is Cygwin native perl?

	use vars qw( %Config );
	eval "use Config";

	$EVAL_ERROR  and die "$EVAL_ERROR";

	if (  $main::Config{osname} =~ /cygwin/i )
	{
	    $CYGWIN_PERL = 1;
	}
    }

    if ( not defined $HOME or  not -e $HOME  or	 $HOME eq '' )
    {
	unless ( $WIN32 )
	{
	    die <<EOF;
$id: No environment variable HOME found.
$id: This is [$OSNAME]
$id: Please contant your site admin or if OS is Win32
$id: then send mail to maintainer $CONTACT
EOF
	}

	die <<EOF;
$id: Hm, Can't find environment variable HOME, which
$id: is expected to point to directory where you
$id: keep your personal files (like .emacs). Maybe
$id: you have set it in registry, but nonetheless it
$id: isn't seen here where you started the program.
$id:
$id: Choices
$id:
$id: a) Edit your c:\\autoexec.bat and add
$id:	something like:
$id:
$id:	      REM    Use forward slashes
$id:	      set HOME=c:/home/yourFirstName
$id:
$id:	This will work for Win95/98/NT.
$id:	Don't know WinME and w2k.
$id:	Boot the PC after this change.
$id:
$id: b) Temporarily set the environment, in this
$id:	dos-shell you're presumably running,
$id:	by typing the above command now and run
$id:	$PROGNAME again.
$id:
$id: Note: Do not use a directory with spaces.
$id: Note: Use FORWARD slashes.
EOF

    }

    if ( $HOME =~ /\s/ )
    {
	die "Environment variable HOME [$HOME] must not contain spaces.";
    }

    unless (  -d "$HOME/tmp" )
    {
	die <<EOF;
$id:  You do not have temporary directory $HOME/tmp. Please create.
$id:  It is used to store temporary files.
EOF

    }


    unless (  -d "$HOME/elisp" )
    {
	warn <<EOF;
$id:  You do not have standard Emacs Lisp directory $HOME/elisp
$id:  Kindly create this directory and keep all your Emacs Lisp files under it.
EOF
    }

    unless (  -d "$HOME/elisp/config" )
    {
	warn <<EOF;
$id:  You do not have Emacs Lisp configuration directory $HOME/elisp/config
$id:  Lisp package cache and self-configuration files are kept there.
$id:  Kindly create this directory.
EOF
    }




    #	Add current directory to path so that perl programs can be found
    #	We suppose; tat user is runnig this program with
    #
    #	    % cd bin
    #	    % perl makefile.pl

    my $sep    = $WIN32 ? q(;)	: q(:)	;

    @PATH = split /$sep/, $PATH;

    push @PATH, cwd();

    $ENV{PATH} = join $sep, @PATH;
    $PATH      = $ENV{PATH};

}

# ***************************************************************** &help ****
#
#   DESCRIPTION
#
#	Print help and exit.
#
#   INPUT PARAMETERS
#
#	$msg	[optional] Reason why function was called.-
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

=pod

=head1 NAME

makefile.pl - Emacs Tiny Tools kit makefile written in Perl.

=head1 README

<short overall description here. This section is ripped by CPAN>

=head1 SYNOPSIS

    % cd <kit-directory> bin
    % perl makefile.pl --binary emacs  --verbose 2  dos2unix all
    % perl makefile.pl --binary xemacs --verbose 2  dos2unix all

=head1 OPTIONS

=head2 Targets

=over 4

=item B<all>

Make all targets:  "html" and "lisp"

=item B<clean>

Remove files that can be generated from sources.

=item B<dos2unix>

Convert all files to Unix line format LF \n.

=item B<lisp>

Compile Emacs Lisp files.

=item B<unix2dos>

Convert all files to Dos line format CRLF \r\n.

=back

=over 4

=head2 Options that may affect targets

=item B<--binary EMACS-BINARY>

Compile using "emacs" or "xemacs". The program binary must be
given.

=item B<--load lisp.el>

by default file C<lisp/load-path.el> is included during byte compilation.
You can instruct to load alternative files, e.g. if you have modified
C<load-path.el> to your environment (Which you should). This option can be
given multiple times and it suppresses loading the default C<lisp/load-path.el>

=back

=head2 Administrative TARGET options

=over 4

=item B<autoload>

Generate autoload files.

=back

=head2 Miscellaneous options

=over 4

=item B<--debug -d LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.
PLEASE ATTACH THIS OUTPUT FOR BUG REPORTS TO THE MAINTAINER OF THIS
PROGRAM.

=item B<--help -h>

Print help

=item B<--help-html -h>

Print help in HTML format.

=item B<--test -t>

Run in test mode. Do now actually execute commands.

=item B<--verbose LEVEL	 -v LEVEL>

Turn on verbose. Increase LEVEL for more meaningfull messages.
to really see what's going on, use --debug (attach the output
for bug reports)

=item B<--Version>

Print contact and version information

=back

=head1 DESCRIPTION

This file is included in Tiny Tools distribution and replaces the Unix-only
makefile.mak. This file compiles the Emacs lisp Libraries and generates
html documentation. To run the program, chdir to the same directory where
makefile.pl is and run command

    % perl makefile.pl --binary emacs --verbose 2 all

=head1 TROUBLESHOOTING

During the Emacs Lisp file compilation, file C<lisp/load-path.el>
that is loaded to tell where to look for any additional lisp packages.
If you see following error, update the C<load-path.el> and include the
location of the directory. COPY YOUR MODIFIED C<load-path.el> and use it
for later updates.

  !! File error (("Cannot open load file" "gnus"))

You should call the makefile with:

    % perl makefile.pl --binary emacs \
      --load ~/elisp/my-load-path.el --verbose 2 all

=head1 EXAMPLES

<example calls for the program in different situations>

=head1 ENVIRONMENT

PATH HOME

=head1 FILES

<what files program generates>

=head1 SEE ALSO

make(1)

=head1 BUGS

<known limitations>

=head1 AVAILABILITY

Project homepage is at http://tiny-tools.sourceforge.net/

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 COREQUISITES

<what CPAN modules are needed to run this program>

=head1 OSNAMES

C<any>

=head1 VERSION

$Id: makefile.pl,v 2.39 2007/05/01 17:20:31 jaalto Exp $

=head1 AUTHOR

Copyright (C) 2000-2008 Jari Aalto. All rights reserved.
This program is free software; you can redistribute and/or modify program
under the same terms as Perl itself or in terms of Gnu General Public
licence v2 or later.

=cut

sub Help (;$$)
{
    my $id  = "$LIB.Help";
    my $msg = shift;  # optional arg, why are we here...

    my $type = shift;  # optional arg, type

    if ( $type eq -html )
    {
	pod2html $PROGRAM_NAME;
    }
    else
    {
	pod2text $PROGRAM_NAME;
    }

    defined $msg  and  print $msg;

    exit 1;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert to absolute paths
#
#   INPUT PARAMETERS
#
#	$bin		Which emacs
#	@list		list of paths
#
#   RETURN VALUES
#
#	@list
#
# ****************************************************************************

sub AbsolutePaths ( $ @ )
{
    my	  $id	 = "$LIB.AbsolutePaths";
    my ( $bin, @list ) = @ARG;

    local ( $ARG );

    my $pwd = cwd();
    my @ret;

    for ( @list )
    {
	if ( ! -e )
	{
	    print "$id: No file exists $ARG\n";
	    $debug and die;
	}
	else
	{
	    chdir dirname $ARG	or  die "$id: $ARG $ERRNO";

	    my $file = basename $ARG;
	    my $dir  = cwd();


	    #  Emacs is not an Cygwin application

	    unless ( $bin =~ /xemacs/ )
	    {
		$dir = CygwinToDos( $dir );
	    }

	    $file = "$dir/$file";

	    $debug and print "$id: absolute path $file\n";

	    push @ret, $file;
	}
    }

    chdir $pwd;
    @ret;
}

# ************************************************************** &args *******
#
#   DESCRIPTION
#
#	Read and interpret command line arguments ARGV. Sets global variables
#
#   INPUT PARAMETERS
#
#	none
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub HandleCommandLineArgs ()
{
    my	  $id = "$LIB.HandleCommandLineArgs";

    local $ARG;

    use vars qw
    (
	$debug
	$verb
	$test

	$BINARY
	@LIBRARY
    );

    my
    (
	$version
	, $help
	, $helpHTML
	, $binary
	, $upload
    );

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
	no_ignore_case
	require_order
    ));

    GetOptions	    # Getopt::Long
    (
	  "h|help"		=> \$help
	, "help-html"		=> \$helpHTML
	, "verbose:i"		=> \$verb
	, "Version"		=> \$version
	, "debug:i"		=> \$debug
	, "test"		=> \$test
	, "load=s@"		=> \@LIBRARY
	, "binary=s"		=> \$BINARY
    );

    $version	    and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help	    and Help();
    $helpHTML	    and Help(undef, -html);
    $debug = 1	    if	defined $debug	and $debug == 0;
    $verb = 10	    if	$debug;
    $verb = 10	    if	$test;

    if ( $debug )
    {
	print "$id:\n"
	    , "\tBINARY = [$BINARY]\n"
	    , "\tDEBUG	= [$debug]\n"
	    , "\tVERB	= [$verb]\n"
	    , "\tTEST	= [$test]\n"
	    , "\tLOAD	= [@LIBRARY]\n"
	    , "\tARGS	= @ARGV\n"
	    ;
    }

    if ( defined $verb	and  $verb < 1)
    {
	$verb = 1;
    }

    $verb = 0 unless defined $verb;

    #	We must find absolute paths, because we change directories
    #	and relative --load ../load-path.el will not work

    @LIBRARY = AbsolutePaths $BINARY, @LIBRARY	    if @LIBRARY; #font s/

}

# ****************************************************************************
#
#   DESCRIPTION
#
#	find binary from PATH
#
#   INPUT PARAMETERS
#
#	$	like "emacs"
#	$	regexp to match on path to accept binary
#	$	if non-zero, return all binaries
#
#   RETURN VALUES
#
#	@	List of absolute paths
#
# ****************************************************************************

sub FindBinary ( $; $$ )
{
    my $id = "$LIB.FindBinary";
    my ( $bin, $regexp, $allChoices ) = @ARG;

    unless ( $bin )
    {
	die "$id: BIN argument missing";
    }

    $regexp = "."   if not defined $regexp;

    my $sep = ":";
    $sep    = ";"   if $PATH =~ /;/;	#font s/   Win32

    my @path = split /$sep/, $PATH;	#font s/

    local $ARG;
    my	  @ret;

    $debug  and	 print "$id: Searching $bin with regexp $regexp\n";

    for ( @path )
    {
	next unless /$regexp/o;	    #font s/

	s,[\\/]$,,;		    #		remove training slash
	s,\\,/,g;		    #font s/	Path to unix slashes

	my $suffix;
	my @list = ("");

	$WIN32 and push @list,	".com", ".exe", ".bat", ".cmd";

	for $suffix ( @list )
	{
	    my $file = "$ARG/$bin$suffix";

	    if ( -f $file )
	    {
		$debug	and  print "$id: $file\n";
		push @ret, $file;
		return @ret unless $allChoices;
	    }
	}
    }

    @ret;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert to Unix or dos styled path
#
#   INPUT PARAMETERS
#
#	$path	    Path to convert
#	$unix	    [optional] If non-zero, convert to unix slashes.
#		    If missing or zero, convert to dos paths.
#	$tail	    [optional] if set, make sure there is trailing
#		    slash or backslash
#
#   RETURN VALUES
#
#	$	    New path
#
# ****************************************************************************

sub PathConvert ( $ ; $$ )
{
    my $id	     = "$LIB.PathConvert";
    local ( $ARG   ) = shift;
    my	  ( $unix  ) = shift;
    my	  ( $trail ) = shift;

    #  Option "-dos"  or "-win" is not regarded as Unix, like in call:
    #
    #	  PathConvert $bin, $WIN32 ? -dos : -unix;

    if ( $unix	 and not  $unix =~ /dos|win/i )
    {
	s,\\,/,g;		    #font s/

	if ( $trail )
	{
	    s,/*$,/,;		    #font s/
	}
	else
	{
	    s,/+$,,;
	}
    }
    else
    {
	s,/,\\,g;		    #fonct s/

	if ( $trail )
	{
	    s,\\*$,\\,;
	}
	else
	{
	    s,\\+$,,;
	}
    }

    $ARG;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Remove the Drive letter from the path if we are running under
#	Win32 Cygwin
#
#   INPUT PARAMETERS
#
#	Path
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub PathFix ( $ )
{
    my $id	   = "$LIB.PathFix";
    local ( $ARG ) = @ARG;

    my $orig = $ARG;

    if ( defined $SHELL )	# Cygwin sets the SHELL variable
    {
	s/^[a-z]://i;		#font s/
    }


    $debug  and	 print "$id: $orig => $ARG\n";

    $ARG;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert /cygdrive/d/path	to d:/path
#	or	/cygdrive/network/path	to UNC path notation //network/path
#
#   INPUT PARAMETERS
#
#	Path
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub CygwinToDos ( $ )
{
    my $id	   = "$LIB.CygwinToDos";
    local ( $ARG ) = @ARG;

    my $orig = $ARG;

    if ( m,^/cygdrive/([^/]+)/(.*),  )
    {
	my $network = $1;
	my $path    = $2;

	if ( length $network > 1 )
	{
	    $ARG = "//$network/$path";
	}
	else
	{
	    $ARG = "${network}:/$path";
	}
    }


    $debug  and	 print "$id: $orig => $ARG\n";

    $ARG;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Find out the temporary directory
#
#   INPUT PARAMETERS
#
#	none
#
#   RETURN VALUES
#
#	$	temporary directory
#
# ****************************************************************************

sub TempDir ()
{
    my $id	   = "$LIB.TempDir";

    my $tmp;

    if ( defined $TEMPDIR  and	-d $TEMPDIR)
    {
	$tmp = $TEMPDIR;
    }
    elsif ( defined $TEMP  and	-d $TEMP)
    {
	$tmp = $TEMP;
    }
    elsif ( -d "/tmp" )
    {
	$tmp = "/tmp";
    }
    elsif ( -d "c:/temp" )
    {
	$tmp = "c:/temp"
    }
    elsif ( -d "$HOME/temp" )
    {
	$tmp = "$HOME/temp"
    }
    else
    {
	die "$id:  Can't find temporary directory. Please set TEMPDIR."
    }

    if ( $tmp  and not -d  $tmp )
    {
	die "$id: Temporary directory found is invalid: [$tmp]";
    }

    $tmp;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Prepare thing for a shell command. See if call needs
#	"perl -S" or just "perl" if we can locate BINARY from
#	unpack directory.
#
#	The pwd() is pushed to the list of directoryes, so you can
#	back track with ShellPerlEnd()
#
#	    ShellPerlStart
#	      ShellPerlStart
#		ShellPerlStart
#		..
#		ShellPerlEnd
#	      ShellPerlEnd
#	    ShellPerlEnd
#
#   INPUT PARAMETERS
#
#	$bin	    Perl script name BIN
#	$dir	    Chdir to directory DIR
#
#   RETURN VALUES
#
#	$perl	    The perl call syntax
#	$bin	    The perl script name. Possibly absolute.
#
#	Join the result to shell command with "$perl $bin"
#
# ****************************************************************************

{
    my @staticDirs;

sub ShellPerlStart ($ ; $)
{
    my $id    = "$LIB.ShellPerlStart";
    my ($bin, $dir) = @ARG;

    # .................................................... record dir ...

    my $pwd = cwd();		    # Supposing ROOT dir. See Main()
    push @staticDirs, $pwd;

    if ( $dir )
    {
	chdir $dir or die "$id: chdir $dir failed.";
    }

    # ..................................................... check bin ...

    my $perl  = "perl -S";
    my $prg   = "$pwd/bin/$bin";

    if ( -f  $prg  )
    {
	$debug	and  print "$id: Good, $bin found. Not using -S\n";
	$perl = "perl";
	$bin   = $prg;
    }


    $perl, $bin ;
}

sub ShellPerlEnd()
{
    my $id  = "$LIB.ShellPerlEnd";
    my $dir = pop @staticDirs;

    unless ( $dir )
    {
	die "$id: ERROR No directory to pop back!";
    }

    chdir $dir	or  die "$id: chdir $dir failed";

}}

# ****************************************************************************
#
#   DESCRIPTION
#
#
#
#   INPUT PARAMETERS
#
#
#
#   RETURN VALUES
#
#
#
# ****************************************************************************

sub MessagePerlCall ($$$)
{
    my $id = "$LIB.MessagePerlCall";
    my ($str, $bin, $cd ) = @ARG;

    print <<EOF;
$str: About to run external shell command
      Shell call "perl -S" where -S option will
      try to locate the $bin along the PATH,
      because it was not found in directory
      [$cd]

      If this call does not succeed and you see something like:

	 Can't find $bin on PATH.

      Then check of you can locate somewhere
      else. Maybe you need to update PATH. If
      you don't find $bin anywhere, please
      contact maintainer $CONTACT.
EOF
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Make Lisp
#
#   INPUT PARAMETERS
#
#	@	directories to compile
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub TargetLisp (@)
{
    my $id = "$LIB.Lisp";
    my (@dir) = @ARG;

    my ($shell, $bin) = ShellPerlStart "emacs-compile.pl" ;

    local $ARG;
    my (@load, $dir);
    my $addopt = "";

    $addopt .= " --verbose $verb"  if $verb;
    $addopt .= " --debug $debug"   if $debug > 1;

    my $emacs = "--emacs $BINARY";
    $emacs = "--xemacs $BINARY" if $BINARY =~ /xemacs/i;

    $verb  and	print "$id: using option $emacs $addopt\n";

    for $dir ( @dir )
    {
	chdir $dir	    or die "$id: Cannot chdir to $dir";

	unless ( @load )
	{
	    @load = ( "--load " . PathFix cwd() . "/load-path.el" );

	    if ( @LIBRARY )
	    {
		@load = map { $ARG = CygwinToDos $ARG; "--load $ARG" } @LIBRARY;
	    }
	}

	my $opt = join " ", @load, $addopt;

	my @libs;

	if ( -e "tinyliba.el" )
	{
	    #	We're compiling directory that contains tiny* files.
	    #	Compile these first.

	    @libs = qw( tinyliba tinylibb tinylibm tinylib* );
	}

	my $cmd = join " ", $shell, $bin, $emacs, $opt, @libs, "*";


	$verb		and  print "$id: cd ", cwd(), "\n$cmd\n";
	system $cmd	unless $test;

    }

    ShellPerlEnd();
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert files
#
#   INPUT PARAMETERS
#
#	$option		"dos" "unix", this is made to --dos --unix
#	@durs		directory list
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub Convert ($ @)
{
    my	  $id = "$LIB.Convert";
    my ($option, @dirs) = @ARG;

    my ($shell, $bin) = ShellPerlStart "dos2unix.pl" ;


    my $opt   = "--no-backup --$option";

    $verb   and $opt .= " --verbose ";

    my $cmd = join " ", $shell, $bin, $opt, "*";

    $verb and print "$id: ", cwd(), "\n$cmd\n";

    for my $dir ( @dirs )
    {
	unless ( chdir $dir )
	{
	    print "$id: ERROR chdir [$dir] failed.";
	}
	else
	{
	    $verb  and	print "$id: ", cwd(), "\n$cmd\n";
	    not $test and system $cmd;
	}
    }

    ShellPerlEnd();
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert files
#
#   INPUT PARAMETERS
#
#	@   file list
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub TargetConvertDos (@)
{
    my	  $id = "$LIB.TargetConvertDos";
    my (@dirs) = @ARG;

    Convert "dos", @dirs;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Convert files
#
#   INPUT PARAMETERS
#
#	@   file list
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub TargetConvertUnix (@)
{
    my	  $id = "$LIB.TargetConvertUnix";
    my (@dirs) = @ARG;

    Convert "unix", @dirs;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Clean unnecessary files: .html and .elc
#
#   INPUT PARAMETERS
#
#	@   directory list
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub TargetClean (@)
{
    my	  $id = "$LIB.Clean";
    my (@dirs) = @ARG;

    local *DIR;

    for my $dir ( @dirs )
    {
	$debug	and  print "$id: cdw ", cwd(), " dir $dir\n";

	unless ( chdir $dir )
	{
	    warn "$id: WARNING chdir $dir failed.";
	    next;
	}

	unless ( opendir DIR, "." )
	{
	    warn "$id: WARNING opendir . failed [$dir]";
	    next;
	}

	#  Files that start with ".#" are CVS failures.


	my @list = grep /(\.elc|[~#])$|^\.#/, readdir DIR;
	closedir DIR;

	$debug	and  print "$id: CLEAN-LIST [@list]\n";


	print "$id: Cleaning ... $dir\n";

	$verb  and  print "\n$id: ", cwd(), "\nrm @list\n";

	unless ( $test )
	{
	    for my $file (@list)
	    {
		unless ( unlink $file )
		{
		    warn "$id: WARNING Can't delete $file $ERRNO\n";
		}
	    }
	}

    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Generate autoloads.
#
#   INPUT PARAMETERS
#
#	$emacs	Emacs to use to generate autoloads.
#	$root	Root directory
#
#   RETURN VALUES
#
#	none	each directory will have autoload file tiny-DIR-autoload.el
#
# ****************************************************************************

sub TargetAutoload ($ $)
{
    my $id	     = "$LIB.Autoload";
    my ($bin, $root) = @ARG;


    my $found = $bin;

    unless ( $bin )
    {
	$verb  and  print "$id: No --binary option. Using [emacs].\n";

	($found) =  FindBinary "emacs";

	if ( not $found	 )
	{
	    $verb  and	print "$id: Can't find [emacs], trying [xemacs].";

	    ($found) =	FindBinary "xemacs";

	    unless ( $found )
	    {
		die "$id: Can't find emacs or xemacs in PATH. Use --binary ",
		    "option."
	    }
	}

	$bin = $found;
    }

    if ( $bin )
    {
	unless ( $bin =~ m,[\\/], )	    # Not an absolute path
	{
	    if ( ($found) = FindBinary $bin )
	    {
		$bin = $found;
	    }
	}

	unless ( -f $bin )
	{
	    print "$id: BINARY does not exist: [$bin]\n";
	}

	$bin = PathConvert $bin, ($WIN32 ? -dos : -unix);
    }

    unless ( $bin )
    {
	die "$id: Which Emacs? Use --binary option to give Emacs [$bin] ";
    }
    else
    {
	$verb  and  print "$id: $bin\n";   # Show full path

	#   But do not use it, because cygwin doesn't like g:/ paths.

	# $found =~ s,.*/,,;
    }

    chdir $root	 or    die "$id: Cannot chdir to $root";;

    my $load = "";

    for my $lib
    (
	"$root/lisp/tiny/load-path.el"
	, "$root/lisp/tiny/tiny-setup.el"
	, "$root/lisp/tiny/tinylibb.el"
    )
    {
	not -e $lib  and   die "$id: Can't find library $lib";
	$load .= " -l $lib";
    }

    # generated-autoload-file

    my $cmd = $bin
	      . " --batch"
	      . " $load"
	      . " -q"
	      . " -f tiny-setup-autoload-batch-update"
	      . " " . cwd()
	      ;

    $verb  and	print "$cmd\n";

    my @ret = qx($cmd)	unless $test;

    if ( $verb )
    {
	print @ret, "\n$id: Done.\n";
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#
#
#   INPUT PARAMETERS
#
#
#
#   RETURN VALUES
#
#
#
# ****************************************************************************

sub CheckNeededDirs (@)
{
    my $id   = "$LIB.CheckNeededDirs";
    my @dirs = @ARG;

    my $stat;

    $verb  and	print "$id: Checking configuration...\n";

    for my $dir ( @dirs )
    {
	unless ( -d $dir )
	{
	    $stat .= " Not found $dir";
	}
    }

    my $msg = $stat ? $stat : "Good";

    $verb  and	print "$id: Checking configuration... [$msg]\n";

    if ( $stat	and  $verb > 1 )
    {
	print<<"EOF";
$id:
    The directories are not what is expected.

    a)	Somebody has moved or deleted directories
    b)	Somebody forgot to make all the directories when
	downloading/installing the files separately.
    c)	The kit was originally built badly
    d)	The directory structure has changed and this file was not updated

    NOTE: this may not be fatal error and you may
    still be able to build the kit.
EOF

	print "$id: Please ensure that all these directories exist:\n\t"
	    , join( "\n\t", @dirs), "\n"
	    ;
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#	Main function
#
#   INPUT PARAMETERS
#
#	none
#
#   RETURN VALUES
#
#	none
#
# ****************************************************************************

sub Main ()
{
    Initialize();
    HandleCommandLineArgs();

    my $id  = "$LIB.Main";

    # .............................................. start directory ...
    #	User may have called us like
    #
    #	perl bin/makefile.pl
    #
    #	But we must chdir to makefile.pl, because all the links are relative
    #	to it.

    my $prgDir = cwd() . "/" . basename $PROGRAM_NAME;

    $prgDir =~ s,\\,/,g;	# Make Unix path
    $prgDir = dirname $prgDir;

    $debug	       and print "$id: prgDir $prgDir\n";

    not -e $prgDir     and die "$id: Initialisation failed [$prgDir]";

    chdir $prgDir	or die "$id: Can't chdir to [$prgDir]";

    # ..................................................... dir vars ...

    chdir ".."	    or die "$id: chdir $prgDir/.. failed";

    $debug and print "$id: cwd ", cwd(), "\n";

    my $ROOT  =	 cwd();		    # relative to this location
    my $BIN   = "$ROOT/bin";


    #	We use perl -S which searches perl files along the path.
    #	The `-S' works for both Win32 and Unix.
    #	Add $BIN directory to path.
    #
    #	The OS may be windows, but what if user is running this script
    #	under cygwin bash or sh ? --> check shell variable which must not
    #	be set, otherwise we are in "unix"

    if ( $PATH =~ /;/ )
    {
	my $bin = PathConvert $BIN;
	$PATH =~ s/;$//;	# Make sure there is no trailing ";"
	$PATH .= ";$bin";
    }
    else
    {
	$PATH =~ s/:$//;	# Make sure there is no trailing ":"
	$PATH .= ":$BIN";
    }

    if ( $debug	 )
    {
	my @path = $PATH =~ /;/ ? split /;/, $PATH : split /:/, $PATH ;
	my $i = 1;

	for my $dir ( @path )
	{
	    printf "$id: PATH %2d %s\n", $i, $dir;
	    $i++;
	}
    }

    $ROOT = PathFix $ROOT;
    $BIN  = PathFix $BIN;

    my $DOC   = "$ROOT/doc/txt";
    my $LISP  = "$ROOT/lisp/tiny";
    my $OTHER = "$ROOT/lisp/other";
    my $RC    = "$ROOT/lisp/rc";

    my @dirs = ( $BIN, $DOC, $LISP, $OTHER, $RC );

    if ( $verb )
    {
	print "$id:\n"
	    , "\tROOT  = $ROOT\n"
	    , "\tSHELL = ", $SHELL , "\n"
	    , "\tWIN32 = ", $WIN32 , "\n"
	    ;
    }

    # ................................................... check dirs ...

    CheckNeededDirs @dirs;

    # ...................................................... targets ...

    my @all = qw( autoload lisp );		    # make destinations

    # ....................................... drop duplicate targets ...

    local $ARG;
    my ( %seen, @targets );


    for ( @ARGV )
    {
	push @targets, $ARG	unless $seen{$ARG};
	$seen{ $ARG } = 1;
    }

    # ................................................. make targets ...

    my $lisp = '^(all|lisp|elc)';

    if ( $BINARY eq ''	and  grep /$lisp/,@targets )	  #font s/
    {
	print "$id: Compile to which Emacs? "
	    , "Use option --binary EMACS-BINARY [targets: @targets] "
	    , " If Emacs is in your path, EMACS-BINARY can"
	    , " be word `emacs' or `xemacs'. Or use complete path.\n"
	    ;

	if ( $verb > 1 )
	{
	    print <<"EOF";
$id:
    XEmacs and Emacs are not compatible platforms and
    program needs to know which one is the target of compilation.

    If you intend to use lisp files both in Emacs and XEmacs, do not
    in any circumstance byte compile lisp files.
EOF
	}

	die;
    }

    unless ( $WIN32 )
    {
	my $cmd = "chmod +x $BIN/*.pl";
	$verb and print "$id: $cmd\n";
	print "$id: executable flag set for binaries in bin/ \n";
	system $cmd;
    }

    my $Sf = 'http://tiny-tools.sourceforge.net/';

    $verb  and	print "$id: Making targets [@targets] cwd = ", cwd(), "\n";

    for ( @targets )
    {
	$verb  and print "$id: Making target [$ARG]\n";

	SWITCH:
	{
	    /$lisp/i	    and TargetLisp	  ( $LISP, $OTHER ) , last;
	    /dos2unix/i	    and TargetConvertUnix ( @dirs )	    , last;
	    /unix2dos/i	    and TargetConvertDos  ( @dirs )	    , last;
	    /clean/	    and TargetClean	  ( @dirs)	    , last;

	    /^auto(load)?/  and TargetAutoload($BINARY, $ROOT), last;

	    print "$id: UNKNOWN target $ARG\n";
	}
    }

    unless ( @targets )
    {
	print "$id: Hm. Nothing to do. TARGETS missing.\n"
	    , "$id: The most usual target option is 'all', without quotes."
	    , " See --help\n"
	    ;
    }
    else
    {
	$verb  and  print "$id: $VERSION done.\n";
    }

}

Main();

0;
__END__
