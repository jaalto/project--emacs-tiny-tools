//  bmt-time.js -- Javascript to show internet time
//
//      Copyright (C) 1996-2010 Jari Aalto
//
//      This program is free software; you can redistribute it and/or
//      modify it under the terms of the GNU General Public License as
//      published by the Free Software Foundation; either version 2 of
//      the License, or (at your option) any later version.
//
//      This program is distributed in the hope that it will be useful, but
//      WITHOUT ANY WARRANTY; without even the implied warranty of
//      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//      General Public License for more details.
//
//      You should have received a copy of the GNU General Public License
//      along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//	Visit <http://www.gnu.org/copyleft/gpl.html> for more information
//
//      To install this file, put this code to HTML file
//
//          <HTML>
//              <HEAD>
//                  <SCRIPT LANGUAGE="JavaScript" SRC="bmt-time.js"></SCRIPT>
//              </HEAD>
//              <BODY onLoad="BMTtimeDisplay(0);">
//                  ...
//              <BODY>
//          </HTML>

///////////////////////////////////////////////////////////////////////
//
// DESCRIPTION
//
//	    Take UTC time, and do the following: [(Hours + 1 {Make sure
//	    that the # is not more than 23} *3600*1000) +
//	    (Minutes *60*1000) + (Seconds * 1000)] / 86400
//
// INPUT PARAMETERS
//
//	    None
//
// RETURN VALUES
//
//	    float
///////////////////////////////////////////////////////////////////////

function SwatchBeats ()
{
    // calculate Middle European Time, i.e. UTC + 1

    var bmt = new Date();

    bmt.setTime( bmt.getTime()
            + (bmt.getTimezoneOffset() + 60) * 60 * 1000 );

    var beat = ( bmt.getHours() * 3600 + bmt.getMinutes() * 60
             + bmt.getSeconds() ) /  86.4;

    return beat;
}

///////////////////////////////////////////////////////////////////////
//
// DESCRIPTION
//
//	    Convert floating point number to N number of decimels.
//
// INPUT PARAMETERS
//
//	    float
//
// RETURN VALUES
//
//	    string
///////////////////////////////////////////////////////////////////////

function CutDecimals (n, count)
{
    var str = n.toString();
    var pos = str.indexOf(".");

    if ( pos > 0 )
    {
        if ( count < 1 )
        {
            str = str.substr(0, pos -1 )
        }
        else
        {
            str = str.substr(0, pos + count + 1);
        }
    }

    return str;
}

///////////////////////////////////////////////////////////////////////
//
// DESCRIPTION
//
//	    Return Swatch Biel Mean Time (BMT), World tme, Internet time
//	    with two decimal accuracy. "@NNN.NN"
//
// INPUT PARAMETERS
//
//	    none
//
// RETURN VALUES
//
//	    string
///////////////////////////////////////////////////////////////////////

function BMTtime ()
{
    // "Biel Mean Time" - Swatch headquarter in Switzerland

    var beat = SwatchBeats();
    beat = "@" + CutDecimals( beat, 2);

    return beat;
}

///////////////////////////////////////////////////////////////////////
//
// DESCRIPTION
//
//		Display the BMT to the target source
//
// INPUT PARAMETERS
//
//		0	Show the BMT time in the status bar
//		1	Write the BMT to the current position: SCRIPT  /SCRIPT
//
// RETURN VALUES
//
//		string
///////////////////////////////////////////////////////////////////////

function BMTtimeDisplay (out)
{
    var time = BMTtime();

    if ( out < 1 )
    {
        var str = "Swatch Internet time is now "
                + time;

        defaultStatus = str;		    // Print to the status bar
        setTimeout("BMTtimeDisplay(0)", 5000);  // Repeat ourself
    }
    else
    {
        document.write( time );
    }
}

// End of file
