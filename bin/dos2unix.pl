#!/usr/bin/perl
#
#  File id
#
#	 Perl -- Dos to unix and unix to dos line ending converter
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

# {{{ Initial setup

use autouse 'Pod::Text'     => qw( pod2text );
use strict;
use English;
use Getopt::Long;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.0503.0758';

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
        $VERSION
        $CONTACT
        $URL
        $GO
    );

    $PROGNAME   = "dos2unix.pl";
    $LIB        = $PROGNAME;
    $CONTACT  = "";
    $URL      = "";

    $OUTPUT_AUTOFLUSH = 1;

    $GO       = "flip";
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

dos2unix.pl - Convert between Dos and Unix line endings

=head1 SYNOPSIS

    dos2unix.pl --unix --no-backup *
    dos2unix.pl --dos  *
    dos2unix.pl --test *

=head1 DESCRIPTION

Thisi is cross-platform implementation of the binary dos2unix(1)
program. It converts between Unix and Dos text files. Without any
options the files are converted to the opposite line endings.

=head1 OPTIONS

=head2 Gneneral options

=over 4

=item B<-d, --dos>

Convert to Dos

=item B<-n, --no-backup>

Delete backup after conversion (.orig)

=item B<-t, --test>

Detect type, do no conversion. This cancels B<--dos> and
B<--unix> and turns on B<--verbose>

=item B<-u, --unix>

Convert to Unix

=back

=head2 Miscellaneous options

=over 4

=item B<-D, --Debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<-h, --help>

Print help

=item B<-v, --verbose>

Print verbose mesages.

Print help

=item B<-V, --Version>

Print contact and version information

=back

=head1 DESCRIPTION

Unix lines end to LF \n whereas DOS terminates lines with CRLF \r\n
This program converts between Unix and Dos text files. Without any
options the files are converted to the opposite line endings.

=head1 TROUBLESHOOTING

Turn on debug with B<--Debug> and run in B<--test> mode.

=head1 EXAMPLES

To check what type of text files you have in directory:

    dos2unix.pl --test *

=head1 ENVIRONMENT

No environment settings.

=head1 FILES

Original files are moved to .orig unless backup is supressed
with B<--no-backup>

=head1 SEE ALSO

dos2unix(1)
flip(1)

=head1 EXIT STATUS

Not defined.

=head1 DEPENDENCIES

Uses standard Perl modules.

=head1 BUGS AND LIMITATIONS

None.

=head1 AVAILABILITY

http://cpan.perl.org/modules/by-authors/id/J/JA/JARIAALTO/

=head1 AUTHOR

Jari Aalto

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2000-2010 Jari Aalto

This program is free software; you can redistribute and/or modify
program under the terms of GNU General Public license either version 2
of the License, or (at your option) any later version.

=cut

sub Help (;$)
{
    my $id  = "$LIB.Help";
    my $msg = shift;  # optional arg, why are we here...

    pod2text $PROGRAM_NAME;

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

    use vars qw
    (
        $NO_BACKUP
        $TEST
        $QUIET
        $debug
        $verb
    );

    $debug           = 0;

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
    ));


    my ( $dos, $unix, $help, $ver );

    GetOptions      # Getopt::Long
    (
          "h|help"              => \$help
        , "verbose"             => \$verb
        , "Version"             => \$ver
        , "Debug"               => \$debug
        , "test"                => \$TEST

        , "dos"                 => \$dos
        , "unix"                => \$unix
        , "no-backup"           => \$NO_BACKUP
    );

    $ver            and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();
    $verb = 1       if  $debug;

    $GO = "unix" if $unix;
    $GO = "dos"  if $dos;
    $GO = ""     if $TEST;

    $verb = 1    if $TEST;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Recognize the line ending type. The $ARG must have been set already.
#
#   INPUT PARAMETERS
#
#       $       [optional] If set, return opposite type.
#
#   RETURN VALUES
#
#       "dos" or "unix"
#
# ****************************************************************************

sub Type ( ;$ )
{
    my $ret;

    if ( /\r\n/ )
    {
        $ret = @ARG ? "unix" : "dos";
    }
    else
    {
        $ret = @ARG ? "dos" : "unix";
    }

    $ret;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Convert th eline to given TYPE
#
#   INPUT PARAMETERS
#
#       $       type. "unix" or "dos"
#
#   RETURN VALUES
#
#       none
#
# ****************************************************************************

sub Convert( $ )
{
    my ( $type ) = @ARG;

    if ( $type eq "unix" )
    {
        s/\r\n/\n/g;
    }
    else
    {
        s/\n/\r\n/g  unless /\r\n\Z/ ;
    }
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
    HandleCommandLineArgs(),

    my $id = "$LIB.main";
    my ( @files, $source, $prev );

    for ( @ARGV )
    {
        #       Win32 can't expand "*". We must do it here.
        #       Grep only FILES, not directories.

        push @files, grep -f, glob $ARG;
    }

    local ( *IN, *OUT );

    for $source ( @files )
    {
        my $out = "$source.out";
        my $err;

        unless ( $TEST )
        {
            open OUT, ">$out"  or $err++;
            binmode OUT;
        }

        open IN,  $source  or $err++;

        if ( $err )
        {
            warn "$id: Can't open $ARG or $out";
        }
        else
        {
            my ( $detected, $otype, $skip );
            my $type = $GO;                     # the wanted conversion

            binmode IN;

            while ( defined($ARG = <IN>) )
            {
                unless ( $detected )
                {
                    $otype = Type();

                    if ( $GO eq "flip" )
                    {
                        $type = Type( "flip" );
                    }
                }

                $otype eq $type     and $skip = "skipped" , last;
                $TEST               and $skip = "test", last;

                if ( $type eq "dos" )
                {
                    not  /\r\n/  and s/\n/\r\n/;
                }
                else
                {
                    s/\r+//g;
                }

                print OUT $ARG;
            }

            close IN;
            close OUT   unless $TEST;

            my $orig = "$source.orig";
            my $file = sprintf "%-25s", $source;

            $debug and
                   print "$otype $type $source -> $orig $out -> $source\n";

            unless ( $skip )
            {

                if ( $NO_BACKUP  or  rename $source, $orig )
                {
                    rename $out, "$source";
                }
                else
                {
                    print "Rename $source --> $orig failed";
                }
            }

            if ( (not $skip  and  $source ne $prev)  or  $verb )
            {
                print "$file ($otype) $type  $skip\n";
            }

            #   Make sure this temporary file is gone.

            unlink $out  if  -e $out;

            $prev = $source;
        }
    }
}

Main();

0;
__END__
