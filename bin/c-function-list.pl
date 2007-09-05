#!/usr/bin/perl
#
# c-function-list.pls -- Examine manual pages and return function list
#
#   File id
#
#       Copyright (C) 2000-2007 Jari Aalto
#
#       This program is free software; you can redistribute it and/or
#       modify it under the terms of the GNU General Public License as
#       published by the Free Software Foundation; either version 2 of
#       the License, or (at your option) any later version.
#
#       This program is distributed in the hope that it will be useful, but
#       WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#       General Public License for more details.
#
#       You should have received a copy of the GNU General Public
#       License along with program; see the file COPYING. If not,
#       write to the Free Software Foundation, Inc., 51 Franklin
#       Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
#       Visit <http://www.gnu.org/copyleft/gpl.html> for more information
#
#    Description
#
#       Original idea by Peter Simons <simons@petium.rhein.de>.
#       Script to make C/C++ function database for Emacs package tinytag.el

for $filename (@ARGV)
{
    open(INFILE, $filename) || die("Can't open file $filename.");

    @lines = <INFILE>;
    chop(@lines);

    $lines_num = @lines;

    $isSynopsis = 0;
    $includes   = "";
    $curr_line  = "";

    for (  $i = 0; $i < $lines_num; $i++)
    {
	$lines[$i] =~ s/.//g;

	if ($lines[$i] =~ /^SYNOPSIS$/)
	{
	    $isSynopsis = 1;
	    next;
	}

	if ($lines[$i] =~ /^DESCRIPTION$/)
	{
	    $isSynopsis = 0;
	    $includes = "";
	    $curr_line = "";
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
	      if ($curr_line ne "")
	      {
                $curr_line =~ s/ +/ /g;

                if ( $includes eq "" )
		{
		  printf "<none>$curr_line\n";
                }
                else
		{
		  printf "$includes$curr_line\n";
                }
                $curr_line = "";
	      }
	    }
	    else
	    {
	      $curr_line = $curr_line . $lines[$i];
	    }
	  }
      }
  }

__END__
