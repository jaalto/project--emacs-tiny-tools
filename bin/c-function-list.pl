#!/usr/bin/perl
#
#  File id
#
#	 c-function-list.pl -- Examine manual pages and return function list
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
#    Description
#
#       Original idea by Peter Simons <simons@petium.rhein.de>.
#       Script to make C/C++ function database for Emacs package tinytag.el

for $filename (@ARGV)
{
    open INFILE, "<", $filename  or  die "Can't open file $filename.";

    @lines = <INFILE>;
    chop(@lines);

    $linesNum = @lines;

    $isSynopsis = 0;
    $includes   = "";
    $currline   = "";

    for ( $i = 0; $i < $linesNum; $i++)
    {
	$lines[$i] =~ s/.//g;

	if ( $lines[$i] =~ /^SYNOPSIS$/ )
	{
	    $isSynopsis = 1;
	    next;
	}

	if ( $lines[$i] =~ /^DESCRIPTION$/ )
	{
	    $isSynopsis = 0;
	    $includes = "";
	    $currline = "";
	    last;
	}

	if ( $isSynopsis == 1 )
	{
	    if ($lines[$i] =~ /^ *#include/)
	    {
		$lines[$i] =~ s/^.*<(.*)>$/<$1>/;
		$includes = $includes . $lines[$i];
	    }
	    elsif ( $lines[$i] =~ /^$/ )
	    {
		if ( $currline ne "" )
		{
		    $currline =~ s/ +/ /g;

		    if ( $includes eq "" )
		    {
			printf "<none>$currline\n";
		    }
		    else
		    {
			printf "$includes$currline\n";
		    }
		    $currline = "";
		}
	    }
	    else
	    {
		$currline = $currline . $lines[$i];
	    }
	}
    }
}

__END__
