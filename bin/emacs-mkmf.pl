#!/usr/bin/perl
#
#  File id
#
#	 Like mkmf(1) to update Emacs lisp Makefile dependencies
#
#   Copyright
#
#       Copyright (C) 1997-2010 Jari Aalto
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

use autouse 'Pod::Text' => qw( pod2text );
use strict;
use Env;
use English;
use File::Basename;
use Getopt::Long;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.0503.0807';

# {{{ Initial setup

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
        $PROGNAME
        $LIB
        $CONTACT
        $URL
    );

    $LIB        = basename $PROGRAM_NAME;
    $PROGNAME   = $LIB;
    $CONTACT  = "";
    $URL      = "";

    $OUTPUT_AUTOFLUSH = 1;
}

# }}}
# {{{ usage/help

# ***************************************************************** &help ****
#
#   DESCRIPTION
#
#       Print help and exit.
#
#   INPUT PARAMETERS
#
#       $msg    [optional] Reason why function was called.
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

=pod

=head1 NAME

emacs-mkmf.pl - Like mkmf(1), but update Emacs lisp Makefile dependencies

=head1 SYNOPSIS

    emacs-mkmf.pl *.el

=head1 REAME

The startdard unix makmf(1) creates dependecies for variety of
programming languages. However, there is no support For Emacs lisp
files so that dependency lines would be created into Makefile.

This small perl script adds the .el file dependencies to `Makefile'
after the tag '###', which is expected by standard mkmf(1). However,
in contrast to the original mkmf(1), which  deletes all the rest
lines after the tag, this program preserves last 2 line in the file.
That gives you to change to preserve Following line.

    # End of file XXX.mak

Searched lines are like these. PACKAGE is recorded from file.
_WS_ refers to whitespace and it is optional. The PACKAGE is picked
only if it resides in the current directory, otherwise it is assumed
that package is somewhere else and current file does not depend on it.
This is very common, you include 'mail-utils, which is standard Emacs
package, and it won't be included in the depend list unless you
give option --all.

    _WS_(require 'PACKAGE)
    _WS_(eval-... (require 'PACKAGE)

C<NOTE>

Program automatically selects only *.el files, so it is safe to
give following command to update all Emacs lisp file dependencies.

    emacs-mkmf.pl *

=head1 OPTIONS

=over 4

=item B<-a, --all>

Include all dependencies, even files that are not
in current directory.

=item B<-D, --debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.
This option turns on B<--verbose> too.

=item B<-d, --dir DIR>

Instead of reading *.el from current directory, read DIR

=item B<-e, --exclude REGEXP>

Exclude files.

=item B<-f, --file MAKEFILE>

Instead of updating 'Makefile', update FILE. if FILE is
string "stdout", print the dependencies to stdout.

=item B<-h, --help> B<-h>

Print help page.

=item B<-p, --preserve COUNT>

Instead of preserving last 2 lines preserve N lines.

=item B<-t, --tag REGEXP>

Inser dependencies after REGEXP.
Normally the dependencies are updated after standard mkmf(1) `###' line.

=item B<-V, --version>

Print program's version information.

=back

=head1 ENVIRONMENT

No environment settings.

=head1 FILES

None.

=head1 SEE ALSO

xmkmf(1)

=head1 EXIT STATUS

Not defined.

=head1 DEPENDENCIES

Uses standard Perl modules.

=head1 BUGS AND LIMITATIONS

None.

=head1 AUTHOR

Jari Aalto

=head1 LICENSE AND COPYRIGHT

Copyright (C) 1997-2010 Jari Aalto

This program is free software; you can redistribute and/or modify
program under the terms of GNU General Public license either version 2
of the License, or (at your option) any later version.

=cut

sub Help (;$)
{
    my $id  = "$LIB.Help";
    my $msg = shift;  # optional arg, why are we here...

    pod2text $PROGRAM_NAME;

    exit 1;
}

# }}}

# ************************************************************** &args *******
#
#   DESCRIPTION
#
#       Read and interpret command line arguments
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
    my $id = "$LIB.HandleCommandLineArgs";

    my ( $version, $help );

    # .......................................... command line options ...

    use vars qw
    (
        $DEP_ALL
        $DIR
        $EXCLUDE_RE
        $MAKEFILE
        $PRESERVE_COUNT
        $SORT
        $TAG

        $verb
        $debug

    );

    $EXCLUDE_RE     = "_EXCLUDE_NONE_";
    $MAKEFILE       = "Makefile";
    $TAG            = '^###\s*$';
    $PRESERVE_COUNT = 1;
    $SORT           = 1;

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        require_order
        no_ignore_case
        no_ignore_case_always
    ));

    GetOptions      # Getopt::Long
    (
          "all"             => \$DEP_ALL
        , "dir=s"           => \$DIR
        , "D|debug:i"       => \$debug
        , "d"               => \$debug
        , "exclude"         => \$EXCLUDE_RE
        , "file=s"          => \$MAKEFILE
        , "help"            => \$help
        , "preserve=i"      => \$PRESERVE_COUNT
        , "tag"             => \$TAG
        , "verbose"         => \$verb
        , "V|version"       => \$version
    );


    $version    and  die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help       and  Help();
    $debug      and  $verb = 1;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Read Emacs lisp file and gather dependencies.
#
#   INPUT PARAMETERS
#
#       $       file to check
#
#   RETURN VALUES
#
#       @       list of files
#
# ****************************************************************************

sub GetDependencies ($)
{
    my $id = "$LIB.GetDep";
    my ( $file) = @ARG;

    my ( %hash, $i, @ret, $efile);

    local $ARG;
    my $FILE;

    #   Usinf %hash filters duplicates, but the order is random, that's
    #   why we use key $i to restore the order later.

    unless ( open $FILE, "<", $file )
    {
        warn "$id: Can't open [$file]";
    }
    else
    {
        $i = 0;

        for ( <$FILE> )
        {
            next if /^\s*;/;    # Skip comments

            $i++;

            if ( /^\s*\(require\s+'([^\)\s]+)/ )
            {
                $debug  and print "$id: 1 $file: $ARG";
                $hash{$i} = $1; next;
            }

            if  (   /\(eval-/ and
                    /require\s+'([^\)\s]+)/
                )
            {
                $debug  and print "$id: 2 $file: $ARG";
                $hash{$i} = $1; next;
            }
        }

        close $FILE;
    }

    #   Preserve read order

    for ( sort keys %hash  )
    {
        $efile = "$hash{$ARG}.el";
        $debug  and print "$id: file $file\n";

        if ( $DEP_ALL )
        {
            push @ret, $efile;
        }
        else
        {
            push @ret, $efile  if  -f $efile;
        }
    }

    $debug  and print "$id: [$file] @ret\n";
    @ret;
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
    Initialize();
    HandleCommandLineArgs();

    my $id = "$LIB.main" ;

    my ( $stdout , @slurp, %files, $i, $depList, @list, @depLines, $file);
    my ( $target , @file , $tmp , $cont, $last );

    my $MAKE;

    $stdout =  1    if $MAKEFILE =~ /^stdout$/;

    unless ( $stdout )
    {
        open $MAKE,"<", $MAKEFILE   or die "$id: Can't open [$MAKEFILE] $ERRNO";
        @slurp = <$MAKE>;
	close $MAKE;
    }

    my @files = grep ! /$EXCLUDE_RE/o &&  /\.el$/ , @ARGV;

    # ........................................... &remove-duplicates ...

    $i = 0;

    for ( @files )
    {
        $files{ $i++ } = $ARG;      # preserve order too with $i
    }

    @files = ();

    for ( keys %files )
    {
        push @files , $files{$ARG};
    }

    $SORT   and     @files = sort {$a cmp $b} @files;

    for $file ( @files )
    {
        $depList    = "";

        #   Target is the ".elc" not ".el"

        $target     = "\n${file}c: ";
        @list       = GetDependencies $file;

        for ( $i = 0; $i < @list; $i++ )
        {
            $target     .= " \\ \n" if $i == 0;

            $cont       = "";
            $cont       = "\\" if $list[1 + $i];                # next element?
            $tmp        = sprintf "\t%-20s$cont\n", $list[$i] ;
            $depList    .= $tmp;
        }
        push @depLines, $target . $depList . "\n" ;
    }

    # .................................................. &write-file ...

    if ( $stdout )
    {
        print @depLines;
    }
    else
    {
        MAKE_IT:

        for ( @slurp )
        {
            if ( /$TAG/o )
            {
                #   Stuck the lines after this TAG and quit

                $ARG .=  "\n" . join '',  @depLines;
                push @file, $ARG ;

                $debug  and  print "$id: LAST LINE -> $ARG";

                last MAKE_IT;
            }
            push @file, $ARG ;
        }

        #   Preserve last two lines.

        $last = join '', @slurp[ ($#slurp - $PRESERVE_COUNT)  .. $#slurp ];

        $debug  and print "$id: lst line [$last] "
            , "count [$PRESERVE_COUNT] $last\n"
            ;

        push @file, $last;

        open $MAKE, ">", $MAKEFILE         or die "$id: $MAKEFILE $ERRNO";
        print $MAKE @file;
        close $MAKE;
    }
}

Main();

0;
__END__
