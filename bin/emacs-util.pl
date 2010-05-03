#!/usr/bin/perl
#
#  File id
#
#	emacs-util.pl -- Find manual page etc. locations
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

BEGIN
{
    use vars qw( $timeBootA );
    $timeBootA = time();
}

use strict;

use Pod::Text;
use Pod::Html;
use English;
use Getopt::Long;
use File::Basename;
use File::Find;
use Cwd;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.0503.0814';

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
        $CYGWIN_PERL

        $HOME
        $EMACS
        $XEMACS

        $GLOBAL_HOMEDIR
        $GLOBAL_HOMEDIR2
        $EXECUTABLE_REGEXP

    );

    $PROGNAME   = basename $PROGRAM_NAME;
    $LIB        = $PROGNAME;
    my $id      = "$LIB.Initialize";
    $CONTACT    = "";
    $URL        = "";

    $WIN32    = 1   if  $OSNAME =~ /win32|cygwin/i;

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

    $OUTPUT_AUTOFLUSH = 1;

    #  Do not include
    #  .pl
    #  .py
    #  .ksh
    #  .csh
    #  .tcsh
    #
    #  Find only standard programs, that end to .exe or .com or .sh

    $EXECUTABLE_REGEXP = '\.(exe|com|sh|bat|cmd)$';

    $GLOBAL_HOMEDIR    = ExpandHOME( "~" );

    #   Remove Drive prefix in Win32, cygwin /home/foo...

    ( $GLOBAL_HOMEDIR2 = $GLOBAL_HOMEDIR ) =~ s/^[a-z]:(.*)/$1/i;

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

emacs-util.pl - Emacs help utility

=head1 SYNOPSIS

    emacs-util.pl --regexp '*[el]' --Dir PATH ..
    emacs-util.pl --Version
    emacs-util.pl --help

=head1 DESCRIPTION

The program will help finding out information from the file system for
use with creation of Emacs variable load-path, man-path, woman-manpath
etc. If these do not mean anything to you, do not worry, package
tinypath.el will know how to use this program. The same could be done
by using pure Emacs lisp, but scanning many drectories recursively
would be very slow. Perl a faster in this repect.

All the messages are prefixed with a word so that the output would be
easily parseable.

=head1 OPTIONS

=head2 Gneneral options

Search files matching REGEXP.

=over 4

=item B<--Bin>

Display bin directories found.

=item B<--Dir>

Display directories found.

=item B<--File>

Display Files found.

=item B<--Info>

Display info directories found.

=item B<--Lang-lisp-dir>

Display Emacs Lisp directories found.

=item B<--Lang-lisp-file>

Display Emacs lisp files found.

=item B<--Lang-c-src-dir>

Display C directories found.

=item B<--Lang-c-src-file>

Display C files found.

=item B<--Lang-cc-src-dir>

Display C++ directories found.

=item B<--Lang-cc-src-file>

Display C++ files found.

=item B<--Lang-java-jar>

Display Java directories found.

=item B<--Lang-java-src-file>

Display Java files found.

=item B<--Man>

Display man directories found.

=item B<--scan-type TYPE>

Type of scanning. This option is relevant only under Win32, where
there can be both Native Win32 version of Emacs and Cygwin version
of Emacs. Under Cygwin, it is possible to make symlinks to file,
but the Native Win32 Emacs (20.3.x) is unable to follow them.

However, this program may be run under Cygwin Perl, which *does*
understand them. The dilemma is that symlinked files should
not be returned if Native Win32 Emacs needs them. Thus the need
for this option.

Valid values: no-symlinks

This option causes ignoring all symlinks.

=back

=head2 Ignore options

=over 4

=item B<--ignore-emacs-regexp REGEXP>

Ignore directories amtching this regexp, which is in Emacs regexp format.
If you do not know Eamcs regexps, you probably aren't interested in this
option.

=item B<--ignore-symlinks>

Ignore symlink directories.

=back

=head2 Miscellaneous options

=over 4

=item B<-d, --debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<-E, --Exit>

Exit immediately after the command line arguments have been
processed. This will print what directories would be traversed.

=item B<-h, --help>

Print help

=item B<--help-html>

Print help in HTML format.

=item B<--test>

Run in test mode. Do not actually execute anything.

=item B<-v, --verbose>

Turn on verbose messages.

=item B<-V, --version>

Print contact and version information

=back

=head1 EXAMPLES

To find out where are all manual page files are installed:

    perl -S emacs-util.pl --verbose --Man c:/cygwin

=head1 ENVIRONMENT

none.

=head1 FILES

None.

=head1 SEE ALSO

emacs(1)
xemacs(1)

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

    local $ARG;

    use vars qw
    (
        $debug
        $verb
        $test

        $REGEXP
        $IGNORE_SYMLINK
        $IGNORE_REGEXP

        $OPT_DIR
        $OPT_FILE
        $OPT_EXIT

        $OPT_INFO
        $OPT_CODE_LISP_DIR
        $OPT_CODE_LISP_FILE
        $OPT_MAN
        $OPT_BIN

        $OPT_CODE_C_SRC_DIR
        $OPT_CODE_C_SRC_FILE
        $OPT_CODE_C_HDR_DIR
        $OPT_CODE_C_HDR_FILE

        $OPT_CODE_CC_SRC_DIR
        $OPT_CODE_CC_SRC_FILE
        $OPT_CODE_CC_HDR_DIR
        $OPT_CODE_CC_HDR_FILE


        $OPT_CODE_JAVA_FILE
        $OPT_CODE_JAVA_JAR_FILE
        $OPT_CODE_JAVA_JAR_DIR

        $OPT_SCAN_TYPE

    );

    $OPT_DIR = $OPT_INFO = $OPT_MAN = $OPT_BIN = 0;

    $OPT_CODE_LISP_DIR      = $OPT_CODE_LISP_FILE       = 0;
    $OPT_CODE_C_SRC_DIR     = $OPT_CODE_C_HDR_FILE      = 0;
    $OPT_CODE_CC_SRC_DIR    = $OPT_CODE_CC_HDR_FILE     = 0;
    $OPT_CODE_JAVA_JAR_DIR  = $OPT_CODE_JAVA_JAR_FILE   = 0;

    $OPT_CODE_JAVA_FILE     = 0;
    $IGNORE_SYMLINK         = 0;
    $verb                   = 0;

    my ( $version, $help, $helpHTML, $binary, $tempdir, $emacsIgnore);

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
        require_order
    ));

    GetOptions      # Getopt::Long
    (
          "Bin"                 => \$OPT_BIN
        , "debug:i"             => \$debug
        , "Dir"                 => \$OPT_DIR
        , "Exit"                => \$OPT_EXIT
        , "File"                => \$OPT_FILE
        , "h|help"              => \$help
        , "help-html"           => \$helpHTML

        , "ignore-emacs-regexp=s" => \$emacsIgnore     #font "
        , "ignore-symlink"        => \$IGNORE_SYMLINK  #font "

        , "Info"                => \$OPT_INFO

        , "Lang-lisp-dir"       => \$OPT_CODE_LISP_DIR
        , "Lang-lisp-file"      => \$OPT_CODE_LISP_FILE

        , "Lang-c-src-dir"      => \$OPT_CODE_C_SRC_DIR
        , "Lang-c-src-file"     => \$OPT_CODE_C_SRC_FILE
        , "Lang-c-hdr-file"     => \$OPT_CODE_C_HDR_FILE

        , "Lang-cc-src-dir"     => \$OPT_CODE_CC_SRC_DIR
        , "Lang-cc-src-file"    => \$OPT_CODE_CC_SRC_FILE

        , "Lang-java-src"       => \$OPT_CODE_JAVA_FILE
        , "Lang-java-jar"       => \$OPT_CODE_JAVA_JAR_FILE

        , "Man"                 => \$OPT_MAN

        , "regexp=s"              => \$REGEXP          #font "
        , "scan-type=s"         => \$OPT_SCAN_TYPE

        , "test"                => \$test
        , "verbose:i"           => \$verb
        , "V|version"           => \$version
    );

    $version        and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $helpHTML       and Help(undef, -html);

    $OPT_EXIT       and $debug = 2;

    defined $debug  and $debug = $debug  || 1;
    $verb = 10      if  $debug;
    $verb = 10      if  $test;

    $debug = 0      unless defined $debug;
    $verb  = 0      if $verb < 1;

    if ( $emacsIgnore )
    {
        $IGNORE_REGEXP = EmacsToPerlRegexp( $emacsIgnore );
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Convert Emacs regexp to Perl regexp. This is an approximation and
#       for limited use.
#
#   INPUT PARAMETERS
#
#       $regexp     Emacs string
#
#   RETURN VALUES
#
#       $regexp     Perl string
#
# ****************************************************************************

sub EmacsToPerlRegexp ($)
{
    my $id = "EmacsToPerlRegexp";

    local ($ARG) = @ARG;

    #  \( RE  \)  => ( RE )

    s,\\\(,(,g;
    s,\\\),),g;

    #  \|  -> |

    s,\\\|,|,g;

    # \  -> \\

    # Perl paths use only forward slashes
    # [\\/]path[\\/]  is generic form for win32 and unix
    # s,\\,,g;

    s,\\,\\\\,g;

    $debug  and  print "$id: DEBUG IGNORE REGEXP $ARG\n";

    $ARG;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Print content of hash.
#
#   INPUT PARAMETERS
#
#       $str        STRING to print at front of lines.
#       $hashRef    Hash reference.
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub PrintHash ( $ $ )
{
    my $id                = "$LIB.PrintHash";
    my ( $str, $hashRef ) = @ARG;

    while ( my($key, $val) = each %$hashRef )
    {
        print "$str$key\n";
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Count minutes and seconds between time A and B
#
#   INPUT PARAMETERS
#
#       $a          start time
#       $b          stop time
#
#   RETURN VALUES
#
#       string      "N min N sec"
#
# ****************************************************************************

sub TimeDiffString ( $ $ )
{
    my $id = "$LIB.TimeDiffString";
    my ( $a, $b ) = @ARG;

    my $diff = $b - $a;
    my $min  = int( $diff / 60 );
    my $sec  = $diff - 60 * $min;

    sprintf "%d min %d sec", $min, $sec;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Expand ~ for HOME environment variable.
#
#   INPUT PARAMETERS
#
#       $str        Relative path, like ~/tmp
#
#   RETURN VALUES
#
#       $str        Absolute path.
#
# ****************************************************************************

sub ExpandHOME ( $ )
{
    my $id    = "$LIB.ExpandHOME";
    my ($str) = @ARG;

    if ( exists $ENV{HOME} )
    {
        local $ARG = $ENV{HOME};

        s,/$,,;         #  Delete trailing slash

        $str =~ s/~/$ARG/;

        if ( $WIN32 )
        {
            $str =~ s/(.*)/\L$1/;
        }

        $str;
    }
    else
    {
        print "$id: DEBUG ERROR can't expand ~ to mean HOME, no such envvar.";
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Build lookup list where last element is significant
#       last => [ absoluteDir, AbsoluteDir .. ]
#
#   INPUT PARAMETERS
#
#       %hash       Original hash
#
#   RETURN VALUES
#
#       %hash
#
# ****************************************************************************

sub HashLookup ( % )
{
    my $id        = "$LIB.HashLookup";
    my ( %input ) = @ARG;

    #   Build lookup list where the last directory is significant
    #   last => [ absoluteDir, AbsoluteDir .. ]

    my %hash;

    for ( keys %input )
    {
        #   Do not count HOME files, only external files.

        if ( not  /$GLOBAL_HOMEDIR|$GLOBAL_HOMEDIR2/ )
        {
            s,/$,,;
            my ($last) = m,.*/([^/]+),;

            if ( exists  $hash{$last} )
            {
                push @{$hash{$last}}, $ARG;
                $debug  and  print "$id: DEBUG $last => $ARG\n";
            }
            else
            {
                $hash{ $last } = [$ARG];
                $debug  and  print "$id: DEBUG $last => $ARG\n";
            }
        }
    }

    %hash;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       See Module File::find
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

#  Yes, it would be easier to use hashes of references
#
#   hash{'jar'} => REEFENCE to list.
#
#  But for now this will have to do.

my %HASH_FILE_LIST;

my %HASH_DIR;
my %HASH_INFO;
my %HASH_MAN;
my %HASH_BIN;

my %HASH_CODE_LISP_DIR;
my %HASH_CODE_LISP_FILE;
my %HASH_CODE_LISP_PRIVATE_FILE;
my %HASH_CODE_LISP_PRIVATE_DIR;

my %HASH_CODE_JAVA_FILE;
my %HASH_CODE_JAVA_JAR_FILE;
my %HASH_CODE_JAVA_JAR_DIR;

my %HASH_CODE_C_HDR_FILE;
my %HASH_CODE_C_HDR_DIR;

my %HASH_CODE_C_SRC_FILE;
my %HASH_CODE_C_SRC_DIR;

my %HASH_CODE_CC_SRC_FILE;
my %HASH_CODE_CC_SRC_DIR;

sub wanted ()
{
    my $id       = "$LIB.wanted";

    local $ARG = $File::Find::name;      # complete pathname to the file
    my    $dir = $File::Find::dir;       # We are chdir()'d here

    if ( m,/CVS/|/RCS/|[#~]/, )
    {
        $debug > 2  and  print "$id: DEBUG excluded $ARG\n";
        $File::Find::prune = 1;
        return;
    }
    elsif ( defined $OPT_SCAN_TYPE
	    and  $OPT_SCAN_TYPE =~ /no-symlinks/
	    and  -l $ARG  )
    {
        $debug > 2  and  print "$id: DEBUG excluded symlink $ARG\n";
	return;
    }
    elsif ( not $WIN32  and  -d $ARG  and  -l $ARG  )
    {
        $debug   and  print "$id Symlink ignored $ARG\n";
    }
    elsif ( m,(^/cygdrive/[a-zA-Z]/$),  and  -d "$1"  )
    {
        #  Cygwin Perl could traverse / root directory hierarchy,
        #  which would effective scan all disk drives. Huh. Prevent
        #  That form happening, because imagine 4 x 80G drives
        #  to be scanned -- it'll take an hour...

        $debug > 2
	    and print "$id: DEBUG External ROOT drive excluded.. [$ARG]\n";

        $File::Find::prune = 1;
        return;
    }
    else
    {
        # ..................................................... test ...

        my ( $X, $D ) = ( 0, 0);

        -d $ARG  and $D = 1;
        -x $ARG  and $X = 1;   # Reuse stat(1) structure in "_"

        if ( $debug > 3 )
        {
            print "$id: DEBUG test -d[$D] -x[$X] file $ARG\n";
        }
        elsif ( $debug > 2 )
        {
            print "$id: DEBUG file $ARG\n";
        }


        if ( $IGNORE_SYMLINK )
        {
            if ( -l $dir  )
            {
                $File::Find::prune = 1;
                $debug > 1  and  print "$id: DEBUG dir ignored symlink $dir\n";
                return;
            }
        }

        if ( defined $IGNORE_REGEXP  and  $IGNORE_REGEXP ne '' )
        {
            if ( $dir =~ /$IGNORE_REGEXP/o )
            {
                $File::Find::prune = 1;

                $debug > 1   and
                    print "$id: DEBUG dir ignored RE $dir"
                         , " [$MATCH] [$IGNORE_REGEXP]\n";

                return;
            }
        }

        $HASH_FILE_LIST{ $ARG } = 1;

        unless ( exists $HASH_DIR{$dir} )
        {
            $HASH_DIR{ $dir } = 1;
            $debug > 1  and  print "$id: DEBUG dir  $dir\n";
        }

        # ................................................ save info ...

        if ( /\.elc?($|\.gz)/ ) # file.el.gz
        {
            $debug > 2 and $OPT_CODE_LISP_FILE
                and  print "$id: DEBUG LISP $ARG $dir\n";

            if ( ! $D )
            {
                $HASH_CODE_LISP_FILE{ $ARG } = 1;
            }

            $HASH_CODE_LISP_DIR { $dir } = 1;

            if (     $dir =~ m,$GLOBAL_HOMEDIR/e?lisp,io
                  or $dir =~ m,$GLOBAL_HOMEDIR2/e?lisp,io
               )
            {
                ! $D  and  $HASH_CODE_LISP_PRIVATE_FILE{ $ARG } = 1;
                $HASH_CODE_LISP_PRIVATE_DIR{ $dir } = 1;
            }

            return;
        }

        #       following regexps are file tests
        #       Return if the element is directory

        $D  and  return;

        if ( /\.info/ )
        {
            $debug > 2  and $OPT_INFO
                and  print "$id: DEBUG INFO $ARG $dir\n";

            $HASH_INFO{ $dir } = 1;
        }
        elsif ( /\.java$/i )
        {
            $debug > 2 and $OPT_CODE_JAVA_FILE
                and  print "$id: DEBUG JAVA_SRC  $ARG $dir\n";

            $HASH_CODE_JAVA_FILE{ $ARG } = 1;
        }
        elsif ( /\.jar$/i )
        {
            $debug > 2 and $OPT_CODE_JAVA_JAR_FILE
                and  print "$id: DEBUG JAVA_JAR  $ARG $dir\n";

            $HASH_CODE_JAVA_JAR_DIR { $dir } = 1;
            $HASH_CODE_JAVA_JAR_FILE{ $ARG } = 1;
        }
        elsif ( /\.c$/i )
        {
            $debug > 2 and $OPT_CODE_C_SRC_FILE
                and  print "$id: DEBUG C  $ARG $dir\n";

            $HASH_CODE_C_SRC_DIR { $dir } = 1;
            $HASH_CODE_C_SRC_FILE{ $ARG } = 1;
        }
        elsif ( /\.cc$/i )
        {
            $debug > 2 and $OPT_CODE_CC_SRC_FILE
                and  print "$id: DEBUG CC  $ARG $dir\n";

            $HASH_CODE_CC_SRC_DIR { $dir } = 1;
            $HASH_CODE_CC_SRC_FILE{ $ARG } = 1;
        }
        elsif ( /\.h$/i )
        {
            $debug > 2 and $OPT_CODE_C_HDR_FILE
                and  print "$id: DEBUG C  $ARG $dir\n";

            $HASH_CODE_C_SRC_DIR { $dir } = 1;
            $HASH_CODE_C_SRC_FILE{ $ARG } = 1;
        }
        elsif ( /\.\d(\.gz)?$/ )    # man.1.gz
        {
            $debug > 2 and $OPT_MAN   and  print "$id: DEBUG MAN  $ARG $dir\n";

            $HASH_MAN{ $dir } = 1;
        }
        elsif ( (! $WIN32 and $X)
                or ( $WIN32 and /$EXECUTABLE_REGEXP/o )
              )
        {
            #   Unix will respect -x test, but Win32 says .dll and
            #   others are also executables? No thank you.

            $debug > 2 and $OPT_BIN and print "$id: DEBUG BIN  $ARG $dir\n";
            $HASH_BIN{ $dir } = 1;
        }
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Make User files to take precedence over the ones that are elswhere.
#       Will affect globals:
#
#           %HASH_CODE_LISP_DIR;
#           %HASH_CODE_LISP_FILE;
#
#   INPUT PARAMETERS
#
#       None.
#
#   RETURN VALUES
#
#       None.
#
# ****************************************************************************

sub LispFilePrecedence ()
{
    my $id = "$LIB.LispFilePrecedence";

    # my %HASH_CODE_LISP_PRIVATE_FILE;
    # my %HASH_CODE_LISP_PRIVATE_DIR;

    local $ARG;

    my %lookupDirHash = HashLookup %HASH_CODE_LISP_DIR;

    #  Loop through all KEYS. Using `each' saves memory
    #  instead of doing "for ( keys %hash ) .."

    while( ($ARG) = each %HASH_CODE_LISP_PRIVATE_DIR )
    {
        s,/$,,;
        my ($last) = m,.*/([^/]+),;

        if ( $last )
        {
            if ( exists $lookupDirHash{$last} )
            {
                my $dirs = $lookupDirHash{$last};

                $debug  and  print "$id: LOOKUP DIR $ARG => [@$dirs]\n";
            }
        }
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Remove duplicate paths
#
#   INPUT PARAMETERS
#
#       @paths
#
#   RETURN VALUES
#
#       @paths
#
# ****************************************************************************

sub UniqueRoots ( @ )
{
    my $id   = "$LIB.UniqueRoots";
    my @list = @ARG;

    $debug  and  print "$id: DEBUG input [@list]\n";

    local $ARG;

    for ( @list )
    {
        # always use forward slashes and no trailing slash

        s,\\,/,g;
        s,/$,,;

	next if /^\s*$/;   	# Skip empty paths

        if ( $WIN32 )
        {
            s/(.+)/\L$1/;           # Case insensitive
        }
    }

    #   remove duplicates.

    my %hash;
    @hash{ @list } = (1) x  @list ;


    #   shortest first:  /usr   /usr/share   /user/share/site-lisp

    @list = sort { length $a  <=> length $b } @list;
    $ARG  = "";

    if ( $debug )
    {
        print "$id: DEBUG search LIST is now...\n";

        for ( @list )
        {
            print "$id: DEBUG LIST ELT = $ARG\n";
        }
    }

    my (@lookup, @ret);

    for my $path ( @list )
    {
        #   If LOOKUP is a submatch of a current path, then it's already
        #   included in recursive lookup.
        #
        #   Like is LOOKUP contains /usr and current PATH were /usr/share,
        #   then PATH would be rejected

        my $found = 0;
        for my $re ( @lookup )
        {
            if (  $path =~ /^\Q$re/ )   #font  - dummy; fix Emacs font locking
            {
                $found++;
                $debug  and  print "$id: DEBUG match $re\n"
            }
        }

        if ( not $found )
        {
            push @lookup, $path;
            push @ret, $path;
        }
        else
        {
            $debug  and  print "$id: DEBUG REJECTED $path\n"
        }
    }

    for ( @ret )
    {
        $ARG = ExpandHOME $ARG;
    }

    if ( $debug  and  @ret != @list )
    {
        print "$id: DEBUG search list modified\n";

        for my $arg ( @ret )
        {
            print "$id: DEBUG RET $arg\n";
        }
    }

    @ret;
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

sub Main ()
{
    my $timeBootB = time;
    my $timeA     = time;

    Initialize();
    my $id  = "$LIB.main";

    #  Options are not handled at this point so we must use grep()
    #  No $debug variable is available yet.

    if ( grep /^--?d/, @ARGV )
    {
        print "$id: DEBUG \@ARGV ", join(' ', @ARGV), "\n";

        for my $arg ( @ARGV )
        {
            print "$id: DEBUG ARG $arg\n";
        }
    }

    HandleCommandLineArgs();

    $debug  and  print "$id: DEBUG location $PROGRAM_NAME\n";

    my @paths = UniqueRoots @ARGV;

    if ( $OPT_EXIT )
    {
        print "$id: DEBUG [--Exit] option was requested\n";
        die;
    }

    unless ( @paths )
    {
        die "$id: VERB Hm, nothing to search?\n";
    }
    else
    {
        find ( \&wanted, @paths );
    }

    # ................................................... precedence ...

    # LispFilePrecedence();
    # die "", %HASH_CODE_LISP_PRIVATE_DIR;

    # ...................................................... results ...

    $OPT_DIR   and PrintHash "DIR "     , \%HASH_DIR;
    $OPT_FILE  and PrintHash "FILE "    , \%HASH_FILE_LIST;

    $OPT_MAN   and PrintHash "MAN "     , \%HASH_MAN;
    $OPT_INFO  and PrintHash "INFO "    , \%HASH_INFO;
    $OPT_BIN   and PrintHash "BIN "     , \%HASH_BIN;

    $OPT_CODE_LISP_DIR  and PrintHash "LISP-DIR "   , \%HASH_CODE_LISP_DIR;
    $OPT_CODE_LISP_FILE and PrintHash "LISP-FILE "  , \%HASH_CODE_LISP_FILE;

    $OPT_CODE_JAVA_FILE
        and PrintHash "JAVA-SRC-DIR "  , \%HASH_CODE_JAVA_FILE;
    $OPT_CODE_JAVA_JAR_FILE
        and PrintHash "JAVA-JAR-FILE " , \%HASH_CODE_JAVA_JAR_FILE;
    $OPT_CODE_JAVA_JAR_DIR
        and PrintHash "JAVA-JAR-DIR " , \%HASH_CODE_JAVA_JAR_DIR;

    $OPT_CODE_C_SRC_DIR  and PrintHash "C-SRC-DIR "  , \%HASH_CODE_C_SRC_DIR;
    $OPT_CODE_C_SRC_FILE and PrintHash "C-SRC-FILE " , \%HASH_CODE_C_SRC_FILE;

    $OPT_CODE_C_HDR_DIR  and PrintHash "C-HDR-DIR "  , \%HASH_CODE_C_HDR_DIR;
    $OPT_CODE_C_HDR_FILE and PrintHash "C-HDR-FILE " , \%HASH_CODE_C_HDR_FILE;

    $OPT_CODE_CC_SRC_DIR
        and PrintHash "CC-SRC-DIR "  , \%HASH_CODE_CC_SRC_DIR;
    $OPT_CODE_CC_SRC_FILE
        and PrintHash "CC-SRC-FILE " , \%HASH_CODE_CC_SRC_FILE;

    my $timeB = time;

    if ( $verb )
    {
        # print "B ", $timeBootB, "  A", $main::timeBootA;
        # my $timeBoot = TimeDiffString $main::timeBootA, $timeBootB;

        my $timeScan = TimeDiffString $timeA, $timeB;

        printf "VERB STATISTICS scan: %s, dirs: %d, files: %d\n"
            , $timeScan
            , scalar keys %HASH_DIR
            , scalar keys %HASH_FILE_LIST
            ;
    }
}

Main();

__END__
