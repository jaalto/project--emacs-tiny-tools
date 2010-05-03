#!/usr/bin/perl
#
#  File id
#
#	 emacs-compile.pl -- Perl, Compile Emacs lisp *.el files
#
#   Copyright
#
#       Copyright (C) 2000-2010 Jari Aalto
#
#   License
#
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#       GNU General Public License for more details.
#
#       You should have received a copy of the GNU General Public License
#       along with this program. If not, see <http://www.gnu.org/licenses/>.

use autouse 'Pod::Text'     => qw( pod2text );
use autouse 'Pod::Html'     => qw( pod2html );

use strict;
use English;
use Env;
use Cwd;
use Getopt::Long;
use File::Basename;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.0503.0704';

# ****************************************************************************
#
#   DESCRIPTION
#
#       Set global variables for the program
#
#   INPUT PARAMETERS
#
#       none
#
#   RETURN VALUES
#
#       none
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
        $CONTACT
        $URL
        $WIN32

        $HOME
        $EMACS
        $XEMACS

    );

    $PROGNAME   = "emacs-compile.pl";
    $LIB        = $PROGNAME;
    my $id      = "$LIB.Initialize";

    $CONTACT  = "";
    $URL      = "";
    $WIN32    = 1   if  $OSNAME =~ /win32/i;

    $OUTPUT_AUTOFLUSH = 1;

    if ( not -e $HOME or  $HOME eq '' )
    {
        die "$id: HOME not set";
    }

}

# ***************************************************************** &help ****
#
#   DESCRIPTION
#
#       Print help and exit.
#
#   INPUT PARAMETERS
#
#       $msg    [optional] Reason why function was called.-
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

=pod

=head1 NAME

emacs-compile.pl - Compile Emacs files

=head1 README

Win32 and Unix compatible Emacs lisp package compilation program

=head1 SYNOPSIS

    emacs-compile.pl --verbose 2  *
    emacs-compile.pl --Version
    emacs-compile.pl --help


=head1 DESCRIPTION

Emacs is "The Editor" available at http://www.emacs.org/ and for
win32 platform at http://www.gnu.org/software/emacs/windows/ntemacs.html
This program compiles Emacs Lisp files (*.el) using wild cards in platform
independ way. Windows doesn't expand wildcards as in Unix, so you cannot just
say:

    emacs -batch -f batch-byte-compile *

But this program accepts list of file specs to match the compiled files.
The files are compiled in the given order.

    emacs-compile.pl --verbose file-spec ..

Usually the byte compiler needs to know what is the C<load-path> in order
to find additional packages that are requested by C<require> C<load> and
C<autoload> commands inthe Emacs Lisp files. You should supply our own
B<load-path.el> which defines adequate path for the compilation process.
This file and any additional packages to load are requested with the B<--load>
command line switch. Here the package C<jka-compr> is loaded first in order
to be able to load lisp files that are in compressed format.

    emacs-compile.pl -l jka-compr -l ~/elisp/load-path.el.gz *

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<--emacs [binary-name]>

Use environment variable $EMACS as compile command unless BINARY-NAME
is supplied. If no environment variable exists, use "emacs".

=item B<--load PACKAGE.el>

Load lisp package.

=item B<--xemacs [binary-name]>

Use environment variable $XEMACS as compile command unless BINARY-NAME
is supplied. If no environment variable exists, use "xemacs".

=back

=head2 Miscellaneous options

=over 4

=item B<--debug -d LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<--help -h>

Print help

=item B<--help-html -h>

Print help in HTML format.

=item B<--test>

Run in test mode. Do not actually execute anything.

=item B<--verbose> B<-v>

Turn on verbose messages.

=item B<--Version>

Print contact and version information

=back

=head1 EXAMPLES

If distribution contain libraries: compile them first, before any
applications. Specify libraries in the compile line and rest of the
files after that:

    perl -S emacs-compile.pl --verbose 2  tinyliba tinylibb  *lib*.el *

=head1 TROUBLESHOOTING

This program selects only files that have suffix C<.el>. You must rename any
Emacs Lisp to have that suffix in order to compile the file with this program.
That's why giving wildcard "*" is safe.

=head1 ENVIRONMENT

    HOME    Location of home directory.
    PATH    Program path list.
    EMACS   Name of the Emacs binary.
    XEMACS  Name of the XEmacs binary.

=head1 FILES

None.

=head1 SEE ALSO

emacs(1)

=head1 BUGS

No known bugs.

=head1 EXIT STATUS

Not defined.

=head1 DEPENDENCIES

Uses standard Perl modules.

=head1 BUGS AND LIMITATIONS

None.

=head1 AUTHOR

Jari Aalto

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2000-2010 Jari Aalto

This program is free software; you can redistribute and/or modify
program under the terms of GNU General Public license either version 2
of the License, or (at your option) any later version.

=cut

sub Help (;$$)
{
    my $id   = "$LIB.Help";
    my $msg  = shift;  # optional arg, why are we here...
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
#       Convert /cygdrive/d/path to d:/path   or  /cygdrive/network/path
#       to  UNC  //network/path
#
#   INPUT PARAMETERS
#
#       Path
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub CygwinToDos ( $ )
{
    my $id         = "$LIB.CygwinToDos";
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

    $debug  and  print "$id: $orig => $ARG\n";

    $ARG;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       find binary from PATH
#
#   INPUT PARAMETERS
#
#       $       like "emacs"
#       $       regexp to match on path to accept binary
#       $       if non-zero, return all binaries
#
#   RETURN VALUES
#
#       ( )     list of paths
#
# ****************************************************************************

sub FindBinary ( $; $$ )
{
    my $id = "$LIB.FindBinary";
    my ( $bin, $regexp, $allChoices ) = @ARG;

    if ( $bin =~ m,[\\/], )
    {
        $debug  and  print "$id: Exit. Absolute path $bin passed.";
        return $bin;
    }

    $regexp = "."   if not defined $regexp;

    my $sep = ":";
    $sep    = ";"   if $WIN32;

    my @path = split /$sep/, $PATH;

    local $ARG;
    my    @ret;

    if ( $debug > 1 )
    {
        print "$id: Searching BIN [$bin] REGEXP [$regexp] "
            , "ALL [$allChoices]\n"
            ;

        my $count = 0;

        for my $dir ( @path )
        {
            printf "\t%3d %s\n", $count++, $dir;
        }
    }

    for ( @path )
    {

        unless ( /$regexp/o )
        {
            $debug > 1  and  print "$id: REJECT [$regexp] $ARG\n";
            next;
        }

        s,[\\/]$,,;                 # remove training slash
        s,\\,/,g;                   # Path to unix slashes

        my $suffix;
        my @list = ("");

        $WIN32  and  push @list,  ".com", ".exe", ".bat", ".cmd";

        for $suffix ( @list )
        {
            my $file = "$ARG/$bin$suffix";


            $debug > 1   and  print "$id: try $file\n";

            if ( -f $file )
            {
                $debug  and  print "$id: FOUND $file\n";
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
#       Convert to absolute paths
#
#   INPUT PARAMETERS
#
#       @list
#
#   RETURN VALUES
#
#       @list
#
# ****************************************************************************

sub AbsolutePaths ( $ @ )
{
    my    $id    = "$LIB.AbsolutePaths";
    my ( $bin, @list ) = @ARG;

    local ( $ARG );

    my $pwd = cwd();
    my @ret;

    for ( @list )
    {
        if ( ! -e )
        {
            print "$id: No such file: $ARG\n";
            $debug and die;
        }
        else
        {
            chdir dirname $ARG  or  die "$id: $ARG $ERRNO";

            my $file = basename $ARG;
            my $dir  = cwd();

            #  Emacs is not an Cygwin application

            $dir = CygwinToDos( $dir );


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
#       Read and interpret command line arguments ARGV. Sets global variables
#
#   INPUT PARAMETERS
#
#       none
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub HandleCommandLineArgs ()
{
    my    $id = "$LIB.HandleCommandLineArgs";

    use vars qw
    (
        $debug
        $verb
        $test
        @LIBRARY
        $RELEASE
        $BIN
        $BINARY
    );

    my ( $version, $xemacs, $emacs, $help , $tiny, $helpHTML);
    local ( $ARG );

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
    ));

    GetOptions      # Getopt::Long
    (
          "h|help"              => \$help
        , "help-html"           => \$helpHTML
        , "verbose:i"           => \$verb
        , "Version"             => \$version
        , "debug:i"             => \$debug
        , "test"                => \$test

        , "xemacs:s"            => \$xemacs
        , "emacs:s"             => \$emacs
        , "release=s"           => \$RELEASE
        , "Tiny"                => \$tiny
        , "load=s@"             => \@LIBRARY
    );

    $version        and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $helpHTML       and Help(undef, -html);
    $verb = 10      if  $debug;
    $verb = 10      if  $test;

    if ( defined $debug )
    {
        $verb  = 1;
        $debug = 1   if  $debug eq '' ;
    }

    if ( defined $verb  and  $verb eq '')
    {
        $verb = 1;
    }

    # .................................................... get binary ...

    $debug  and  print "$id: XEmacs [$xemacs] Emacs [$emacs]\n";


    if ( defined $emacs )
    {

        #   If run inside emacs M-x shell, the window defined variable
        #   XEMACS=t Hm? make sure there is something to read.

        ($BIN) = grep /\w{5}/, $emacs, $EMACS, "emacs" ;
    }

    if ( defined $xemacs )
    {
        ($BIN) = grep /\w{5}/, $xemacs, $EMACS, "xemacs" ;
    }


    #   Set default value if no binary given

    unless ( defined $BIN )
    {
        die "$id: No emacs instance defined for compiling.\n"
            , "$id: Please check PATH and command line arguments."
            ;
    }

    if ( $tiny )
    {
        my $lib  = $HOME . "/elisp/tiny/lisp/load-path.el";

        -f $lib  and  push  @LIBRARY, $lib;
    }

    #   User may have written command line
    #
    #       -e file.el
    #
    #   The "file.el" is not a binary name, fix it.
    #

    ($BINARY) = FindBinary $BIN, $RELEASE;

    unless ( -e $BINARY )
    {
        die "$id: BINARY [$BIN] does not exist [$BINARY]."
            , "Can't compile anything."
            ;
    }

    unless ( $BINARY )
    {
        unshift @ARGV, $BIN;            # put the file back

        $BIN = "emacs"  if $emacs;
        $BIN = "xemacs" if $xemacs;

        ($BINARY) = FindBinary $BIN, $RELEASE;

        unless ( $BINARY )
        {
            print <<"EOF";
$id: Can't find [$BIN].
$id: In order to compile the lisp files, an emacs instance (program)
$id: must be called. You dind't specify a full path to the program,
$id: so a PATH lookup was tried. There was no [$BIN] along PATH.
$id:
$id: Choices:
$id:
$id: a) Give full path name to the emacs instance binary. Use forward
$id:    slashes in command line. Do not use directory containing spaces
$id:    (relocate emacs instance as neede to non-space directory)
$id:
$id:    --binary /absolute/location/emacs.exe
$id:
$id: b) Include the directory to your PATH.
EOF

            if ( $WIN32 )
            {
                print qq($id:    Add "set PATH=%PATH%;\\absolute\\location\n)
                    , qq($id:    to C:\\AUTOEXEC.BAT and reboot.\n)
                    ;
            }
            else
            {
                print qq($id: Add "export PATH=$PATH:/absolute/location"\n)
                    , qq($id: or  "setenv PATH \${PATH}:/absolute/location"\n)
                    , qq($id: to your $HOME/.bashrc or $HOME/.cshrc)
                    , qq( respectively.\n)
                    ;
            }

            die "\n$id: Exit. ERROR no [$BIN] found.";

        }
    }

    #   We must find absolute paths, becase we change directories
    #   and relative --load ../load-path.el won't work

    my $bin = $emacs || $xemacs;

    @LIBRARY = AbsolutePaths $bin, @LIBRARY  if @LIBRARY;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Expand filenames (wildcards). Win32 doesn't expand them in command line
#       as Unix does.
#
#   INPUT PARAMETERS
#
#       $       HTML page
#
#   RETURN VALUES
#
#       @       files
#
# ****************************************************************************

sub ExpandFiles ( @ )
{
    my $id = "$LIB.ExpandFiles";
    my (@list) = @ARG;

    #   Multiple specs can be given
    #
    #       file* file2* f*
    #
    #   But because the file* already inlcudes file2*, we keep %seen hash
    #   to check if file is already included.

    my @files;
    my %seen;
    local $ARG;

    $debug  and  print "$id: LIST -> @list\n";

    for my $elt ( @list )
    {
        #       Win32 can't expand "*". We must do it here.

        next if -d $elt;

        my @glob = glob $elt;

        $debug  and  print "$id: $elt GLOB -> @glob\n";

        for ( @glob )
        {
            chomp;

            next if /\.elc$/;

            unless ( /\.el$/ )
            {
                $verb  and  print "$id: $ARG -- ADDED [.el]\n";
                $ARG .= ".el";
            }

            unless ( exists $seen{$ARG} )
            {
                $debug  and  print "$id: push $ARG\n";

                push @files, $ARG;
                $seen{ $ARG } = 1;
            }
        }
    }

    $debug  and  print "$id: RET [@files]\n";

    @files;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Start of the program
#
#   INPUT PARAMETERS
#
#       Comamnd line arguments
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub Main
{
    Initialize();
    HandleCommandLineArgs();

    my $id  = "$LIB.main";

    $debug and print "$id: program location at $PROGRAM_NAME\n";

    # ................................................... read files ...
    #   Grep only FILES, not directories.

    $debug  and  print "$id: ARGV [@ARGV]\n";

    my @arg = @ARGV;

    #   Ignore CVS to-do ci files #file#.el

    my @files = grep
    {
        -f
        and -r
        and /\.el$/
        and ! /#/

    } ExpandFiles @ARGV;

    unless ( @files )
    {
        die "$id: Can't find any files to compile here "
            , cwd(), " [@arg]"
            ;
    }

    # ................................................. Load library ...

    my @lib;

    @LIBRARY   and @lib = map { "-l $ARG" } @LIBRARY;

    for my $file ( @LIBRARY )
    {
        unless ( -f $file )
        {
            $verb  and  print "$id: WARNING library not found: $file\n";
        }
    }

    my $opt = " -batch @lib -f batch-byte-compile ";

    # ...................................................... compile ...

    $debug  and  print "$id: PWD ", cwd(), "\n";

    #  It's nice to know at start what Emacs is used for compiling.

    $verb  and  printf "$id: [binary] %s\n", $BINARY;

    for my $file ( @files )
    {
        my $cmd = $BIN . $opt . $file;
        $verb           and print "$cmd\n";
        not $test       and system $cmd;
    }

    if ( $verb )
    {
        my $ver = grep  /Emacs\s+\d/i, `$BINARY --batch --version`;

        chomp $ver;

        printf "$id: [binary] %s\n%s\n", $BINARY, $ver;
    }

    unless ( @files )
    {
        print "$id: No files to compile.\n";
    }
}

Main();
__END__
