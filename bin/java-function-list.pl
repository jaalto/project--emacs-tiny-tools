#!/usr/bin/perl
#
#  File id
#
#	java-function-list.pl -- Grep all JDK/SDK Java funtions to a list
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

use autouse 'Pod::Text' => qw( pod2text );
use strict;
use English;
use Env;
use Cwd;
use Getopt::Long;
use File::Basename;
use File::Find;

#   The following variable is updated by developer's Emacs whenever
#   this file is saved

our $VERSION = '2010.0503.0737';

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
        $debug

        $PROGNAME
        $LIB

        $CONTACT
        $URL
        $WIN32

        $HOME
        $PATH
        @PATH
    );

    $PROGNAME   = "java-function-list.pl";
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

    #   Add current directory to path so that perl programs can be found
    #   We suppose; tat user is runnig this program with
    #
    #       % cd bin
    #       % perl java-function-list.pl

    my $sep    = $WIN32 ? q(;)  : q(:)  ;

    @PATH = split /$sep/, $PATH;

    push @PATH, cwd();

    $ENV{PATH} = join $sep, @PATH;
    $PATH      = $ENV{PATH};
}

# }}}


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

java-function-list.pl - Grep all JDK/SDK Java funtions

=head1 SYNOPSIS

    java-function-list.pl -recurse /where/is/your/jdk-root
    java-function-list.pl jdk-javadoc-page/BufferedReader.html [file ..]

=head1 DESCRIPTION

This file generates list of all Java methods, their return values,
call parameters and thrown exections from javadoc generated HTML
pages. The output format is as terse as possibly occupying only one
line per function. The output is optimized for Emacs package
tinytag.el, which can display function information while you write Java
code. (See tiny tools project at AVAILABILITY section)

Here is example which extracts functions from BufferedInputStream HTML page:

    cd java/sun/jdk<version>/docs/api/java/io
    java-function-list.pl Buffered*Input*

    java.io.BufferedInputStream  BufferedInputStream(InputStream in)
    java.io.BufferedInputStream  BufferedInputStream(InputStream in, int size)
    java.io.BufferedInputStream  int available()  throws IOException
    java.io.BufferedInputStream protected byte[] buf
    java.io.BufferedInputStream  void close()  throws IOException
    java.io.BufferedInputStream protected int count
    java.io.BufferedInputStream  void mark(int readlimit) throws IOException
    java.io.BufferedInputStream  boolean markSupported() throws IOException
    java.io.BufferedInputStream protected int marklimit
    java.io.BufferedInputStream protected int markpos
    java.io.BufferedInputStream protected int pos
    java.io.BufferedInputStream  int read()  throws IOException
    java.io.BufferedInputStream  int read(byte[] b, int off, int len)  throws IOExce
    ption
    java.io.BufferedInputStream  void reset()  throws IOException
    java.io.BufferedInputStream  long skip(long n)  throws IOException

To generate full function and variable listing out of Java tree, use
B<--recurse> option and instruct to start from Java installation
directory. The result will be a very long listing.

    java-function-list.pl --recurse java/sun/jdk<version>/docs > jdkVERSION.lst

=head1 OPTIONS

=head2 General options

=over 4

=item B<--recurse ROOT>

Start reading HTML pages from ROOT directory. Multiple B<--root>
options can be given to search several root directories. The ROOT is
usually the JDK/SDK installation directory.

=back

=head2 Miscellaneous options

=over 4

=item B<--debug LEVEL>

Turn on debug with positive LEVEL number. Zero means no debug.

=item B<--help>

Print help

=item B<--test>

Run in test mode. Do now actually execute commands.

=item B<--verbose>

Turn on verbose.

=item B<--Version>

Print contact and version information

=back

=head1 TROUBLESHOOTING

Please turn on --debug with level 2 to check what happens inside.

=head1 BUGS

There is no guarrantee that the HTML parsing is able to read all
function definitions. There has been no rigorous effort to check that
the output has the same information as the HTML pages.

Some Java classes inherit function from base classes and that causes
some of the the well known functions not to appear in the output. This
program does not hava any intelligence to cache or refer to base class
definitions. Program does not know the OO relationships and it simply
chews the envountered javadoc pages.

An example:

If you look at the JDK documentation page in your Java installation
sun/jdk1.2.2/docs/api/index.html and under C<System>, you will find
that there is no functions C<print()> or C<println()> mentioned.
That's why they won't appear in the generated listing. The functions
are in fact reported in C<PrintStream> class, but they are used in
programs like:

    System.out.print
    System.out.println

There is nothing that can be done to this OO-inheritance. You must
manually add any missing entries that you need to the generated files.
For the System.out, copy the java.io.PrintStream page as System.out.*
functions and you're set. Here is a hand made copy which you can
copy/paste to your generated function list

    java.lang.System.out  PrintStream(OutputStream out)
    java.lang.System.out  PrintStream(OutputStream out, boolean autoFlush)
    java.lang.System.out  boolean checkError()
    java.lang.System.out  void close()
    java.lang.System.out  void flush()
    java.lang.System.out  void print(boolean b)
    java.lang.System.out  void print(Object obj)
    java.lang.System.out  void print(char c)
    java.lang.System.out  void print(int i)
    java.lang.System.out  void print(long l)
    java.lang.System.out  void print(float f)
    java.lang.System.out  void print(double d)
    java.lang.System.out  void print(char[] s)
    java.lang.System.out  void print(String s)
    java.lang.System.out  void println()
    java.lang.System.out  void println(boolean x)
    java.lang.System.out  void println(char x)
    java.lang.System.out  void println(int x)
    java.lang.System.out  void println(long x)
    java.lang.System.out  void println(float x)
    java.lang.System.out  void println(double x)
    java.lang.System.out  void println(char[] x)
    java.lang.System.out  void println(String x)
    java.lang.System.out  void println(Object x)
    java.lang.System.out  protected void setError()
    java.lang.System.out  void write(int b)
    java.lang.System.out  void write(byte[] buf, int off, int len)

=head1 ENVIRONMENT

No environment variables used.

=head1 FILES

No files generated.

=head1 SEE ALSO

emacs(1)
java(1)
xemacs(1)

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
        $debug
        $verb
        $test
        @RECURSE
    );


    my ( $version, $help, $binary );

    # .................................................... read args ...

    Getopt::Long::config( qw
    (
        no_ignore_case
        no_ignore_case_always
    ));

    GetOptions      # Getopt::Long
    (
          "help"                => \$help
        , "verbose:i"           => \$verb
        , "Version"             => \$version
        , "debug:i"             => \$debug
        , "test"                => \$test
        , "recurse=s@"          => \@RECURSE
    );

    $version        and die "$VERSION $PROGNAME $CONTACT $URL\n";
    $help           and Help();

    $debug = 1      if  defined $debug;
    $verb  = 1      if  defined $verb;
    $verb  = 1      if  $debug;
    $verb  = 1      if  $test;

    $debug  and  warn "$id: rest command line args [@ARGV]\n";
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
#       %       Hash data of Java HTML page. See HtmlParse()
#
#   RETURN VALUES
#
#       @       Formatted lines
#
# ****************************************************************************

sub Format ( % )
{
    my $id     = "$LIB.Format";
    my (%hash) = @ARG;

    my (@list, $class, $name, $type, $param, $ex , $str);

    for my $key ( sort keys %hash )
    {
        ($class, $name, $type, $param, $ex) = @{ $hash{$key} }[0..4] ;

        # Don't include the information about public/private class memeber

        $type =~ s/public|private//ig;

        $str  = $class;

        # The sprintf choice mighht me more pleasant, but the latter is
        # mode compact fro Emaacs package tinytag.el which shows fucntion info
        #
        # $str .= sprintf " %-4s ", $type       ;

        $str .= " "  . $type;

        $str .= " " . $name     if $name ne '';
        $str .= $param          if $param ne '';

        $str .= " " . $ex       if $ex ne '';

        $str .= "\n";

        push @list, $str;
    }

    @list;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Remove html tags
#
#   INPUT PARAMETERS
#
#       %       Hash data of Java HTMl page. See HtmlParse()
#
#   RETURN VALUES
#
#       @       Formatted lines
#
# ****************************************************************************

sub HtmlClean ( $ )
{
    my ($arg) = @ARG;

    $arg =~ s,<A\s+HREF(.*?)>,,g;
    $arg =~ s,</A>,,g;
    $arg =~ s,&[a-z]+;, ,g;
    $arg;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Get function details out of the HTML page. This function expects
#       that the HTML is in javadoc generated format. Don't try anything else
#       but javadoc that comes with JDK 1.2.2
#
#       Removed numeric index from HASH keys if you want to use them.
#       E.g. Constructors cannot be handled otherwise because the metod
#       name is the same.
#
#   INPUT PARAMETERS
#
#       $       HTML page
#
#   RETURN VALUES
#
#       %hash   JAVA.CLASS.OBJECT
#               => [ JAVA.CLASS, OBJECT, type, parameters, exceptions ]
#
# ****************************************************************************

sub HtmlParse ( $ ; $ )
{
    my    $id   = "$LIB.HtmlParse";
    local $ARG  = shift;
    my    $file = shift;

    # ........................................ Read class definition ...

    my $class = '';

    #     +--<B>java.io.BufferedInputStream</B>
    #     </PRE>

    if ( m,START\s+OF\s+CLASS\s+DATA.*?--<B>(.*?)</B>\s*</PRE>,si )
    {
        $class = $1;
    }

    if ( $class eq '' )
    {
        if ( $debug )
        {
            warn "$id: ERROR Can't find standard +-- Java class definition."
                ,  "$file\n"
                ;
        }
        else
        {
            $verb and warn "$id: ERROR Corrupted HTML? "
                        , "Can't find Java +-- class name. $file"
                        ;
        }

        return;
    }


    $verb  and  warn  "$id: Extracting $file";

    # ...................................................... cleanup ...
    #   Kill until this, the method details follow after it.

    s/^.*=== FIELD DETAIL ===//si;

    # ............................................. read definitions ...

    my %hash;

    #   <!-- ============ FIELD DETAIL =========== -->
    #
    #   <A NAME="field_detail"><!-- --></A>
    #   <TABLE BORDER="1" CELLPADDING="3" CELLSPACING="0" WIDTH="100%">
    #   <TR BGCOLOR="#CCCCFF" CLASS="TableHeadingColor">
    #   <TD COLSPAN=1><FONT SIZE="+2">
    #   <B>Field Detail</B></FONT></TD>
    #   </TR>
    #   </TABLE>
    #
    #   <A NAME="buf"><!-- --></A><H3>
    #   buf</H3>
    #   <PRE>
    #   protected byte[] <B>buf</B></PRE>
    #   <DL>
    #   <DD>The internal buffer array where the data is stored. When necessary,
    #    it may be replaced by another array of
    #    a different size.</DL>
    #   <HR>

    my ( $def, $qualified, $type, $name , $param, $throw , $i );

    while ( m,</H\d>\s*<PRE>\s*(.*?)</PRE>,gsi )
    {
        $def = $1;
        $def =~ s,\s\s+, ,g;

        $debug > 1  and print "$id: DEF 1 >> $def\n";


        #   Extract type and name. Here are two examples from
        #   java.io.BufferedInputStream
        #
        #   protected byte[] <B>buf</B>
        #
        #   <PRE>
        #   public int <B>available</B>()
        #   throws <A HREF="../../java/io/IOException.html">IOException</A>
        #   </PRE>

        if ( $def =~ m,^\s*(.*\S)\s*<B>\s*(\S.*)</B>(.*),si )
        {
            ($type, $name) = ( $1, $2 );
            $def           = $3;

            $debug > 2  and print "$id: DEF 2-0 >> TYPE $type\n";
            $debug > 2  and print "$id: DEF 2-0 >> NAME $name\n";

            $type = HtmlClean $type;
            $name = HtmlClean $name;

            $debug > 1  and print "$id: DEF 2 >> TYPE $type\n";
            $debug > 1  and print "$id: DEF 2 >> NAME $name\n";
            $debug > 1  and print "$id: DEF 2 >> $def\n";

            if ( $def =~  m,(.*)(throws.*),i )
            {
                ( $param, $throw ) = ( $1, $2 );

                $throw = HtmlClean $throw;
            }
            else
            {
                $param = $def;
            }

            #   Clean the line again
            #   (<A HREF="../../java/io/InputStream.html">InputStream</A>&nbsp;in)

            $param = HtmlClean $param;
        }

        $qualified = $class . "." . $name;

        unless ( exists $hash{ $qualified }  )
        {
            $hash{ $qualified } = [$class , $name, $type, $param, $throw];
        }
        else
        {
            $hash{ $qualified . ++$i }
               = [$class , $name, $type, $param, $throw];
        }

        $debug and
            warn "$id: $file MATCHED $qualified - $type - $param - $throw \n";
    }

    %hash;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Parse one Java HTML file
#
#   INPUT PARAMETERS
#
#       $       Java html file
#
#   RETURN VALUES
#
#       @       Function listing. See HtmlParse()
#
# ****************************************************************************

sub FileParse ( $ )
{
    my $id      = "$LIB.FileParse";
    my ($file) = @ARG;

    my    $html;
    local *FILE;

    my %hash;

    if ( not -f $file )
    {
        $verb  and  warn "$id: cannot open [$file]";
    }
    else
    {
        unless ( open FILE, "< $file" )
        {
            if ( $debug )
            {
                $verb  and  warn "$id: open error $file $ERRNO";
            }
            else
            {
                $verb  and  warn "$id: open error $file $ERRNO";
            }
        }
        else
        {
            $html = join "", <FILE>;
            close FILE;
            %hash = HtmlParse $html, $file;
        }
    }

    %hash;
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

    my    ( %seen, @files);
    local $ARG;

    for ( @list )
    {
        #       Win32 can't expand "*". We must do it here.

        for my $file ( glob $ARG )
        {
            unless ( exists $seen{$file} )
            {
                push @files, $file;
                $seen{ $file } = 1;
            }
        }
    }

    @files;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       Handle one javadoc page
#
#   INPUT PARAMETERS
#
#       $file       Javadoc html file
#
#   RETURN VALUES
#
#       none        prints the extracted documentation
#
# ****************************************************************************

sub FileOutput( $ )
{
    my $id       = "$LIB.Fileoutput";
    my ( $file ) = @ARG;

    my %hash  = FileParse $file;
    my @lines = Format %hash;

    print @lines;
}

# ****************************************************************************
#
#   DESCRIPTION
#
#       See File::Find
#
#   INPUT PARAMETERS
#
#
#
#   RETURN VALUES
#
#
#
# ****************************************************************************

#   Java JDK/SDK distribute documentation in separate package and
#   they unpack under directory docs/...
#   This flag gets set if the focs/ directory was encountered.

my $FOUND_DOCS;
my %DIRECTORY_SEEN_HASH;

sub wanted
{
    my $id = "$LIB.wanted";

    my $dir = $File::Find::dir;

    unless ( exists $DIRECTORY_SEEN_HASH{ $dir} )
    {
        $verb   and  warn  "$id: Recursing $dir\n";
        $DIRECTORY_SEEN_HASH{ $dir } = 1;
    }

    if ( $dir =~  m,/docs$, )
    {
        $FOUND_DOCS = 1;
    }

    if ( -f and -r and /\.html$/ )
    {
        $debug and print "$id: $File::Find::name\n";
        FileOutput $ARG;
    }
}

# ************************************************************** &main *******
#
#   DESCRIPTION
#
#       The start of the program
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

sub Main ()
{
    Initialize();
    HandleCommandLineArgs();

    my $id  = "$LIB.Main";

    my @files = grep { -f and -r and /\.html$/ } ExpandFiles @ARGV;

    $debug  and  warn "$id: Reading HTML files [@files]";

    for my $file ( @files )
    {
        $verb  and  warn "$id: Processing file [$file]";
        FileOutput $file;
    }

    if ( defined @RECURSE  and  @RECURSE  )
    {
        $verb  and  warn "$id: Recursing .. [@RECURSE]\n";
        find ( \&wanted, @RECURSE );

        unless ( $FOUND_DOCS )
        {
            warn "$id: Could not see Java documentation directory docs/ "
                 , "anywhere. Did you remmeber to install the Java docs from "
                 , " a separate archive? (ignore this message your doc "
                 , " directory name is different.)";

            if ( $debug )
            {
                warn "$id: Directories checked were:\n";

                while ( my($key) = each %DIRECTORY_SEEN_HASH )
                {
                    warn "$key\n";
                }
            }
        }
    }

    unless ( @files or @RECURSE )
    {
        $verb and  die "$id: nothing to do. Did you forget option --recurse?";
    }
}

Main();

0;
__END__
