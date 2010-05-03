#!/usr/bin/perl
#
#  File id
#
#	 emacs-1-fix.pl -- Fix Emacs Lisp package first line to standard format
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
#
#   Description
#
#       Fix first line string in Emacs files to following format:
#
#           ;;; file.el --- One line description string
#
#       The motivation of this would be that there are means
#       of indexing all of the packages that are supplied with Emacs.

use strict;
use English;

sub Main ()
{
    my $id = "$PROGRAM_NAME.Main";
    my @files;

    for ( @ARGV )
    {
        #       Win32 can't expand "*". We must do it here.
        #       Grep only FILES, not directories.

        push @files, grep { -f and /\.el$/ } glob $ARG;
    }

    local ( *FILE, $ARG );

    for my $file ( @files )
    {
        # ..................................................... read ...

        unless ( open FILE, $file )
        {
            print "$id: Cannot open $file $ERRNO\n";
            next;
        }

        binmode FILE;
        my @content = <FILE>;
        close FILE;

        # ...................................................... fix ...

        my $count;

        for (@content)
        {
            s/^;+/;;;/;
            s/(\s+)-+(\s+)/ --- /;
            $count++;
            last if $count == 2;
        }

        # .............................................. replace old ...

        print $content[0];

        unless ( open FILE, "> $file" )
        {
            print "$id: Cannot write $file $ERRNO\n";
            next;
        }

        binmode FILE;
        print FILE @content;
	close FILE;
    }
}

Main();

0;
__END__
