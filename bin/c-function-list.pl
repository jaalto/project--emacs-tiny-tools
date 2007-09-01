#! perl
#
# Peter Simons <simons@petium.rhein.de>
# Script to make C/C++ function database for Emacs tinytag.el
#
# 2001-12-13  -- #todo fix and rewrite this

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
