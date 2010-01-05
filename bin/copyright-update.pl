#!/usr/bin/perl
#
# copyright-update.pl -- Update copyright year
#
#   File id
#
#       Copyright (C) 2000-2010 Jari Aalto
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
#
#   Documentation
#
#       See --help for documentation.
#
#   Code Note
#
#       This code has been edited using Emacs editor, where M-x
#       cperl-mode and M-x font-lock-mode was turned on. Due to
#       highlighting problems, a Perl regexp marker may // sometimes
#       confuse things, so an alternative m,, match operator was used.

use strict;

use autouse 'Pod::Text'     => qw( pod2text );
use autouse 'Pod::Html'     => qw( pod2html );

use English;
use Getopt::Long;
use File::Find;

    my $LIB = "copyright-update.pl";

    use vars qw ( $VERSION );

    #   This is for use of Makefile.PL and ExtUtils::MakeMaker
    #   So that it puts the tardist number in format YYYY.MMDD
    #   The REAL version number is defined later
    #
    #   The following variable is updated by Emacs setup whenever
    #   this file is saved. See http//tiny-tools.sourceforge.net/

    my $VERSION = '2009.1102.1502';

# ****************************************************************************
#
#   DESCRIPTION
#
#       Help function and embedded POD documentation
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

=pod

=head1 NAME

copyright-update.pl - Update Copyright year information

=head1 README

This program updates the copyright year information for given files. The
year is current year unless passed with B<--year> YEAR option.

   copyright-update.pl --verbose 1 --test [--year 2002] *

To change all files recursively from current directory, whose author is
"Mr. Foo" use command below. The B<--regexp> option requires that
file contains matching line. The lines affected are those matches
by the --line regural expression.

   copyright-update.pl \
        --recursive \
        --Regexp "Author:.*Mr. Foo" \
        --line '\bFoo\b' \
        --ignore '\.(bak|bup|[~#]])$' \
        --verbose 1 \
        --year 2002 \
        --test \
        .

For the above command, only files that contain lines like these would
be updated:

   Copyright (C)        YYYY-YYYY
   Copyright: (C)       YYYY-YYYY

The line must have word "Copyright", an ascii "(C)" and range of years.
Varying amount of spaces and tabs are permitted, but there must be no
spaces around the dash-character in YEAR-YEAR.

=head1 OPTIONS

=item B<-a, --fsf-address LEVEL>

Correct FSF addresses, like:

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    You should have received a copy of the GNU General Public License
    along with this package; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

Into FSF recommendation, used in GPL v3 preamble:

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.

=item B<-d, --debug LEVEL>

Turn on debug. Level can be in range 0-10.

=item B<-h, --help>

Print text help

=item B<--Help-html>

Print help in HTML format.

=item B<--Help-man>

Print help in manual page C<man(1)> format.

=item B<-i, --ignore REGEXP>

Ignore files mathing regexp. The match is done against whole path.

=item B<-l, --line REGEXP>

Change only lines which match REGEXP. The match is case-insensitive.

=item B<-r, --recursive>

Recursively search all direcotries given at command line.

=item B<-R, --regexp REGEXP>

Change only files whose content matches REGEXP.

=item B<-t, --test>

hangedRun in test mode. Show what would happen. No files are changed.

=item B<-v, --verbose LEVEL>

Print informational messages. Increase numeric LEVEL for more
verbosity.

=item B<-V, --Version>

Print contact and version information

=item B<-y, --year YEAR>

Update files using YEAR. Year value must be four digits.
The default is current calendar year.

=item B<-Y, --no-year>

Disable updating year.

=back

=head1 DESCRIPTION

<Longer program description>

=head1 TROUBLESHOOTING

None.

=head1 EXAMPLES

None.

=head1 ENVIRONMENT

No environment variables are used.

=head1 FILES

None.

=head1 SEE ALSO

<references to other programs>

=head1 BUGS

No known limitations.

=head1 AVAILABILITY

http://tiny-tools.sourceforge.net/

=head1 SCRIPT CATEGORIES

CPAN/Administrative

=head1 COREQUISITES

Uses tandard Perl modules.

=head1 OSNAMES

C<any>

=head1 AUTHOR

Copyright (C) 2000-2009 Jari Aalto. All rights reserved. This program
is free software; you can redistribute and/or modify program under the
terms of Gnu General Public license v2 or later.

=cut

sub Help (;$$)
{
    my $id   = "$LIB.Help";
    my $type = shift;  # optional arg, type
    my $msg  = shift;  # optional arg, why are we here...

    if ( $type eq -html )
    {
        pod2html $PROGRAM_NAME;
    }
    elsif ( $type eq -man )
    {
	eval "use Pod::Man;";
        $EVAL_ERROR  and  die "$id: Cannot generate Man: $EVAL_ERROR";

        my %options;
        $options{center} = 'cvs status - formatter';

        my $parser = Pod::Man->new(%options);
        $parser->parse_from_file ($PROGRAM_NAME);
    }
    else
    {
	if ( $^V =~ /5\.10/ )
	{
	    # Bug in 5.10. Cant use string ("") as a symbol ref
	    # while "strict refs" in use at
	    # /usr/share/perl/5.10/Pod/Text.pm line 249.

	    system("pod2text $PROGRAM_NAME");
	}
	else
	{
	    pod2text $PROGRAM_NAME;
	}
    }

    defined $msg  and  print $msg;

    exit 1;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Return current year YYYY
#
#   INPUT PARAMETERS
#
#       None
#
#   RETURN VALUES
#
#       number      YYYY
#
# ****************************************************************************

sub Year ()
{
    my $id = "$LIB.Year";
    1900 + (localtime time())[5];
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Read command line arguments and their parameters.
#
#   INPUT PARAMETERS
#
#       None
#
#   RETURN VALUES
#
#       Globally set options.
#
# ****************************************************************************

sub HandleCommandLineArgs ()
{
    my $id = "$LIB.HandleCommandLineArgs";

    use vars qw
    (
        $test
        $verb
        $debug

        $YEAR
        $OPT_NO_YEAR
        $OPT_LINE_REGEXP
        $OPT_RECURSIVE
        $OPT_REGEXP
        $OPT_REGEXP_IGNORE
        $OPT_FSF_ADDRESS
    );

    Getopt::Long::config( qw
    (
        require_order
        no_ignore_case
        no_ignore_case_always
    ));

    my ( $help, $helpMan, $helpHtml );          # local variables to function
    $debug = -1;

    GetOptions      # Getopt::Long
    (
          "a|fsf-address" => \$OPT_FSF_ADDRESS
        , "debug:i"    => \$debug
        , "help"       => \$help
        , "Help-html"  => \$helpHtml
        , "Help-man"   => \$helpMan
        , "ignore=s"   => \$OPT_REGEXP_IGNORE
        , "line=s"     => \$OPT_LINE_REGEXP
        , "Y|no-year"  => \$OPT_NO_YEAR
        , "recursive"  => \$OPT_RECURSIVE
        , "R|regexp=s"   => \$OPT_REGEXP
        , "test"       => \$test
        , "verbose:i"  => \$verb
        , "year=i"     => \$YEAR
    );

    $help     and  Help();
    $helpMan  and  Help(-man);
    $helpMan  and  Help(-html);

    $debug = 1          if $debug == 0;
    $debug = 0          if $debug < 0;

    $YEAR = Year()  unless defined $YEAR;

    unless ( $YEAR =~ m,^\d{4}$, )
    {
        die "$id: Option --year must be given with four digits [$YEAR]";
    }

    if ( defined $verb  and  $verb == 0 )
    {
        $verb = 1;
    }

    $verb = 1  if  $test and $verb == 0;
    $verb = 5  if  $debug;
}


# ****************************************************************************
#
#   DESCRIPTION
#
#       Change FSF Address
#
#   INPUT PARAMETERS
#
#       $content	File content.
#
#   RETURN VALUES
#
#       $string		File content or empty if no chnages.
#
# ****************************************************************************

sub FsfAddress ( $ ; $ )
{
    my $id     = "$LIB.FsfAddress";
    my $X      = "$id: " if $debug;
    local $ARG = shift;
    my $file   = shift;

    my $done;

    s
    {^([^\r\n]*)You\s+should\s+have\s+received .*? write \s+to.*?Boston.*?USA[ \t.]*}
    {\1You should have received a copy of the GNU General Public License\n\1along with this program. If not, see <http://www.gnu.org/licenses/>.}smix
    and $done++;

    $ARG if $done;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Handle Single file
#
#   INPUT PARAMETERS
#
#       %hash       -file   => [filename list]
#                   -regexp => Regexp to match file content.
#                              If regexp is not found in file, file is not
#                              handled.
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub HandleFile ( % )
{
    my $id  = "$LIB.HandleFile";
    my $X   = "$id: " if $debug;
    my %arg = @ARG;

    my @files   = @{ $arg{-file} };
    my $regexp  = $arg{-regexp} || '' ;
    my $linere  = $arg{-line}   || '' ;

    unless ( @files )
    {
        warn "$id: -file argument is empty: ",  $arg{-file};
        return;
    }

    $debug  and  print "$id: -file [@files], ",
                       "-regexp [$regexp] ",
                       "-line [$linere]\n"
		       ;

    my $ffile;

    sub Print(@);
    local *Print = sub (@)
    {
	print "${X}$ffile: ", @_, "\n";
    };

    local ( *FILE, $ARG );

    for my $file ( @files )
    {
	$ffile = $file;			# For Print()

        $debug  and  print "$id: $file: open\n";

        # ..................................................... read ...

        unless ( open FILE, "<", $file )
        {
            $verb  and  Print "ERROR: cannot open";
            next;
        }
        else
        {
            binmode FILE;
            $ARG = join '', <FILE>;
            close FILE;

            unless ( /\w/ )
            {
                $verb  and  Print "WARN: empty file";
                return;
            }
        }

        if ( $regexp )
        {
            unless ( /$regexp/o )
            {
                $verb and
                    Print "WARN: failed regexp check: $regexp";
		next;
            }
        }

	my $done;
        my $msg = $test ? "Would change" : "Changed";

	if ( $OPT_FSF_ADDRESS  and  (my $fsf = FsfAddress $ARG, $file) )
	{
	    Print "$msg FSF address to URL";
	    $ARG = $fsf;
	    $done++;
	}

        my $yyyy    = '\d{4}';
        my $copy    = 'Copyright:?[ \t]+\([Cc]\)[ \t]+' . $yyyy;
        my $repeat  = '-';

        #  If we find the regexp, then check if YEAR is different
        #  and finally do substitution.
        #
        #  If everything went ok, replace file.

        unless ( /$copy$repeat($yyyy)/oi )
        {
            $verb > 1 and  Print "No Copyright line" ;
        }

        my $y = $1;
	$y = "" if $OPT_NO_YEAR;

        if ( $y  and  $y eq $YEAR )
        {
	    $verb > 2  and  Print "Copyright is already $YEAR";
	    $y = "";
        }

	unless ( $OPT_NO_YEAR )
	{
	    if ( $linere )
	    {
		if ( $debug )
		{
		    warn "s/(?:$linere).*\\K($copy$repeat)($yyyy)/\${1}$YEAR/gmi\n";
		    warn "s/($copy$repeat)$yyyy(.*$linere)/\${1}$YEAR\${2}/gmi\n";
		}

		s/(?:$linere).*\K($copy$repeat)$yyyy/$1$YEAR/gmi and $done++;
		s/($copy$repeat)$yyyy(.*$linere)/$1$YEAR$2/gmi and $done++;
	    }
	    else
	    {
		s/($copy$repeat)$yyyy/$1$YEAR/gmi and $done++;
	    }
	}

        $verb   and  $y  and  Print "$msg $y => $YEAR";

        $test   and  next;
	! $done and  next;

        unless ( open FILE, ">", $file )
        {
            Print "ERROR: Cannot open for write";
        }
        else
        {
	    $verb > 2 and Print "wrote";
            binmode FILE;
            print FILE $ARG;
            close FILE;
        }
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Recursively find out all files and chnege their content.
#
#   INPUT PARAMETERS
#
#       None. This function is called from File::FInd.pm library
#
#   RETURN VALUES
#
#       None.
#
# ****************************************************************************

sub wanted ()
{
    my $id = "$LIB.wanted";

    my $dir  = $File::Find::dir;
    my $file = $File::Find::name;  # complete path

    if ( $dir =~ m,(CVS|RCS|\.(bzr|svn|git|darcs|arch|mtn|hg))$,i )
    {
        $File::Find::prune = 1;
        $debug  and  print "$id: Ignored directory: $dir\n";
        return;
    }

    #   Emacs backup files this.txt~  and   #this.text#

    my $ignore = '[#~]$'
                 . '|\.[#]'
                 . '|\.(log|tmp|bak|bin|s?o|com|exe)$'
                 . '\.(ppt|xls|jpg|png|gif|tiff|bmp)$'
                 ;

    if ( $OPT_REGEXP_IGNORE  and  $file =~ /$OPT_REGEXP_IGNORE/o )
    {
        $debug  and  print "$id: skipped --ignore match: $file\n";
        return;
    }

    if ( $file =~ m,$ignore,oi )
    {
        $debug  and  print "$id: Ignored temporary file: $file\n";
        return;
    }

    if ( -f )
    {
        if ( $verb > 3 )
        {
            print "$id: $file\n";
        }

        HandleFile -file => [$file],
	  -line   => $OPT_LINE_REGEXP,
	  -regexp => $OPT_REGEXP;
    }
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Expand files in list. Win32 support
#
#   INPUT PARAMETERS
#
#       @       list of file glob patterns.
#
#   RETURN VALUES
#
#       @       list of filenames
#
# ****************************************************************************

sub FileGlobs ( @ )
{
    my $id   = "$LIB.FileGlobs";
    my @list = @ARG;
    not @list  and  die "$id: No files to expand. Argument list is empty.";

    my @files;

    for my $glob ( @list )
    {
        #       Win32 can't expand "*". We must do it here.
        #       Grep only FILES, not directories.

        push @files, grep { -f } glob $glob;
    }

    $debug  and  print "$id: RETURN [@files]\n";
    @files;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Main function
#
#   INPUT PARAMETERS
#
#       None
#
#   RETURN VALUES
#
#       None
#
# ****************************************************************************

sub Main ()
{
    my $id = "$LIB.Main";

    HandleCommandLineArgs();

    unless ( @ARGV )
    {
        die "What files to change? See --help.";
    }

    $debug  and  print "$id: ARGV [@ARGV]\n";

    # .......................................... expand command line ...


    if ( $OPT_RECURSIVE )
    {
        find( {wanted => \&wanted, no_chdir => 1},  @ARGV );
    }
    else
    {
        my @files = FileGlobs @ARGV;

        unless (@files)
        {
            $verb  and  warn "[WARN] No files matching glob(s): @ARGV\n";
            return;
        }

        HandleFile -file => [@files],
	  -line   => $OPT_LINE_REGEXP,
	  -regexp => $OPT_REGEXP;
    }
}

Main();

0;
__END__
