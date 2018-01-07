#Comma-separated values

database information organized as field separated lists

Standard RFC 4180

In computing, a comma-separated values (CSV) file stores tabular
data (numbers and text) in plain text. Each line of the file is a
data record. Each record consists of one or more fields, separated by
commas. The use of the comma as a field separator is the source of the
name for this file format.

The CSV file format is not standardized. The basic idea of separating
fields with a comma is clear, but that idea gets complicated when the
field data may also contain commas or even embedded line-breaks. CSV
implementations may not handle such field data, or they may use quotation
marks to surround the field. Quotation does not solve everything: some
fields may need embedded quotation marks, so a CSV implementation may
include escape characters or escape sequences.

In addition, the term "CSV" also denotes some closely related
delimiter-separated formats that use different field delimiters. These
include tab-separated values and space-separated values. A delimiter that
is not present in the field data (such as tab) keeps the format parsing
simple. These alternate delimiter-separated files are often even given a
.csv extension despite the use of a non-comma field separator. This loose
terminology can cause problems in data exchange. Many applications that
accept CSV files have options to select the delimiter character and the
quotation character.

Data exchange[edit]

CSV is a common data exchange format that is widely supported by
consumer, business, and scientific applications. Among its most common
uses is moving tabular data between programs that natively operate on
incompatible (often proprietary or undocumented) formats.[1] This works
despite lack of adherence to RFC 4180 (or any other standard), because
so many programs support variations on the CSV format for data import.

For example, a user may need to transfer information from a database
program that stores data in a proprietary format, to a spreadsheet that
uses a completely different format. The database program most likely can
export its data as "CSV"; the exported CSV file can then be imported by
the spreadsheet program.

Specification[edit]

RFC 4180 proposes a specification for the CSV format, and this is the
definition commonly used. However, in popular usage "CSV" is not a
single, well-defined format. As a result, in practice the term "CSV"
might refer to any file that:[2][5]

   1. is plain text using a character set such as ASCII, various Unicode
      character sets (e.g. UTF-8), EBCDIC, or Shift JIS,
   2. consists of records (typically one record per line),
   3. with the records divided into fields separated by delimiters
      (typically a single reserved character such as comma, semicolon,
      or tab; sometimes the delimiter may include optional spaces),
   4. where every record has the same sequence of fields.

Within these general constraints, many variations are in use. Therefore,
without additional information (such as whether RFC 4180 is honored),
a file claimed simply to be in "CSV" format is not fully specified. As a
result, many applications supporting CSV files allow users to preview the
first few lines of the file and then specify the delimiter character(s),
quoting rules, etc. If a particular CSV file's variations fall outside
what a particular receiving program supports, it is often feasible to
examine and edit the file by hand (i.e., with a text editor) or write
a script or program to produce a conforming format.

History[edit]

Comma-separated values is a data format that pre-dates personal computers
by more than a decade: the IBM Fortran (level H extended) compiler
under OS/360 supported them in 1972.[6] List-directed ("free form")
input/output was defined in FORTRAN 77, approved in 1978. List-directed
input used commas or spaces for delimiters, so unquoted character strings
could not contain commas or spaces.[7]

The "comma-separated value" name and "CSV" abbreviation appear to
have come into common use in 1983, in connection with the SuperCalc
spreadsheet.[8]. The manual for the Osborne Executive computer, which
bundled SuperCalc, documents the CSV quoting convention that allows
strings to contain embedded commas, but does not specify a convention
for embedding quotation marks within quoted strings.[9]

Comma-separated value lists are easier to type (for example into punched
cards) than fixed-column-aligned data, and were less prone to producing
incorrect results if a value was punched one column off from its intended
location.

Comma separated files are used for the interchange of database information
between machines of two different architectures. The plain-text character
of CSV files largely avoids incompatibilities such as byte-order and word
size. The files are largely human-readable, so it is easier to deal with
them in the absence of perfect documentation or communication.[10]

The main standardization initiative – transforming "de facto fuzzy
definition" into a more precise and de jure one – was in 2005, with
RFC4180, defining CSV as a MIME Content Type. Later, in 2013, some of
RFC4180's deficiencies were tackled by a W3C recommendation.[11]

In 2014 IETF published RFC7111 describing application of URI fragments
to CSV documents. RFC7111 specifies how row, column, and cell ranges
can be selected from a CSV document using position indexes.

In 2015 W3C, in an attempt to enhance CSV with formal semantics,
publicized the first drafts of recommendations for CSV-metadata standards,
that began as recommendations in December of the same year.[12]

General functionality[edit]

CSV formats are best used to represent sets or sequences of records
in which each record has an identical list of fields. This corresponds
to a single relation in a relational database, or to data (though not
calculations) in a typical spreadsheet.

The format dates back to the early days of business computing and is
widely used to pass data between computers with different internal word
sizes, data formatting needs, and so forth. For this reason, CSV files
are common on all computer platforms.

CSV is a delimited text file that uses a comma to separate values (many
implementations of CSV import/export tools allow other separators to
be used). Simple CSV implementations may prohibit field values that
contain a comma or other special characters such as newlines. More
sophisticated CSV implementations permit them, often by requiring "
(double quote) characters around values that contain reserved characters
(such as commas, double quotes, or less commonly, newlines). Embedded
double quote characters may then be represented by a pair of consecutive
double quotes,[13] or by prefixing an escape character such as a backslash
(for example in Sybase Central).

CSV formats are not limited to a particular character set.[1] They work
just as well with Unicode character sets (such as UTF-8 or UTF-16) as
with ASCII (although particular programs that support CSV may have their
own limitations). CSV files normally will even survive naive translation
from one character set to another (unlike nearly all proprietary data
formats). CSV does not, however, provide any way to indicate what
character set is in use, so that must be communicated separately, or
determined at the receiving end (if possible).

Databases that include multiple relations cannot be exported as a single CSV file[citation needed].

Similarly, CSV cannot naturally represent hierarchical or object-oriented
database or other data. This is because every CSV record is expected
to have the same structure. CSV is therefore rarely appropriate for
documents such as those created with HTML, XML, or other markup or
word-processing technologies.

Statistical databases in various fields often have a generally
relation-like structure, but with some repeatable groups of fields. For
example, health databases such as the Demographic and Health Survey
typically repeat some questions for each child of a given parent (perhaps
up to a fixed maximum number of children). Statistical analysis systems
often include utilities that can "rotate" such data; for example, a
"parent" record that includes information about five children can be
split into five separate records, each containing (a) the information on
one child, and (b) a copy of all the non-child-specific information. CSV
can represent either the "vertical" or "horizontal" form of such data.

In a relational database, similar issues are readily handled by creating
a separate relation for each such group, and connecting "child" records
to the related "parent" records using a foreign key (such as an ID
number or name for the parent). In markup languages such as XML, such
groups are typically enclosed within a parent element and repeated as
necessary (for example, multiple <child> nodes within a single <parent>
node). With CSV there is no widely accepted single-file solution.

Standardization[edit]

The name "CSV" indicates the use of the comma to separate data
fields. Nevertheless, the term "CSV" is widely used to refer a large
family of formats, which differ in many ways. Some implementations
allow or require single or double quotation marks around some or all
fields; and some reserve the very first record as a header containing
a list of field names. The character set being used is undefined: some
applications require a Unicode byte order mark (BOM) to enforce Unicode
interpretation (sometimes even a UTF-8 BOM).[1] Files that use the tab
character instead of comma can be more precisely referred to as "TSV"
for tab-separated values.

Other implementation differences include handling of more commonplace
field separators (such as space or semicolon) and newline characters
inside text fields. One more subtlety is the interpretation of a blank
line: it can equally be the result of writing a record of zero fields,
or a record of one field of zero length; thus decoding it is ambiguous.

Reliance on the standard documented by RFC 4180 can simplify
CSV exchange. However, this standard only specifies handling of
text-based fields. Interpretation of the text of each field is still
application-specific.

RFC 4180 formalized CSV. It defines the MIME type "text/csv", and
CSV files that follow its rules should be very widely portable. Among
its requirements: MS-DOS-style lines that end with (CR/LF) characters
(optional for the last line).  An optional header record (there is no sure
way to detect whether it is present, so care is required when importing).
Each record "should" contain the same number of comma-separated fields.
Any field may be quoted (with double quotes).  Fields containing a
line-break, double-quote or commas should be quoted. (If they are not, the
file will likely be impossible to process correctly).  A (double) quote
character in a field must be represented by two (double) quote characters.

The format can be processed by most programs that claim to read CSV
files. The exceptions are: (a) programs may not support line-breaks
within quoted fields, (b) programs may confuse the optional header with
data or interpret the first data line as an optional header and (c)
double quotes in a field may not be parsed correctly automatically.

In 2011 Open Knowledge and various partners created a data protocols
working group, which later evolved into the Frictionless Data
initiative. One of the main formats they released was Tabular Data
Package. Tabular Data package was heavily based on CSV, using it as the
main data transport format and adding basic type and schema metadata
(CSV lacks any type information to distinguish the string "1" from the
number 1). An initial v1 of Tabular Data Package was released in 2015,
and after extensive real-world testing and tool development, v1 of a
CSV-based Tabular Data Package was officially released in September
2017.[14] The Frictionless Data Initiative has also provided a standard
CSV Dialect Description Format for describing different dialects of CSV,
for example specifying the field separator or quoting rules.

In 2013 the W3C "CSV on the Web" working group began to specify
technologies providing a higher interoperability for web applications
using CSV or similar formats.[15] The working group completed its work in
February 2016, and is officially closed in March 2016 with the release
of a set documents and W3C recommendations[16] for modeling "Tabular
Data",[17] and enhancing CSV with metadata and semantics.

Basic rules[edit]

Many informal documents exist that describe "CSV" formats. IETF RFC
4180 (summarized above) defines the format for the "text/csv" MIME type
registered with the IANA.

Rules typical of these and other "CSV" specifications and implementations
are as follows: CSV is a delimited data format that has fields/columns
separated by the comma character and records/rows terminated by
newlines.  A CSV file does not require a specific character encoding,
byte order, or line terminator format (some software does not support
all line-end variations).  A record ends at a line terminator. However,
line-terminators can be embedded as data within fields, so software
must recognize quoted line-separators (see below) in order to correctly
assemble an entire record from perhaps multiple lines.  All records should
have the same number of fields, in the same order.  Data within fields
is interpreted as a sequence of characters, not as a sequence of bits
or bytes (see RFC 2046, section 4.1). For example, the numeric quantity
65535 may be represented as the 5 ASCII characters "65535" (or perhaps
other forms such as "0xFFFF", "000065535.000E+00", etc.); but not as a
sequence of 2 bytes intended to be treated as a single binary integer
rather than as two characters (e.g. the numbers 11264-11307 have a comma
as their high order byte: ord(',')*256..ord(',')*257-1). If this "plain
text" convention is not followed, then the CSV file no longer contains
sufficient information to interpret it correctly, the CSV file will not
likely survive transmission across differing computer architectures,
and will not conform to the text/csv MIME type.  Adjacent fields must be
separated by a single comma. However, "CSV" formats vary greatly in this
choice of separator character. In particular, in locales where the comma
is used as a decimal separator, semicolon, TAB, or other characters are
used instead.  1997,Ford,E350

Any field may be quoted (that is, enclosed within double-quote
characters). Some fields must be quoted, as specified in following rules.

    "1997","Ford","E350"

Fields with embedded commas or double-quote characters must be quoted.

    1997,Ford,E350,"Super, luxurious truck"

Each of the embedded double-quote characters must be represented by a
pair of double-quote characters.

    1997,Ford,E350,"Super, ""luxurious"" truck"

Fields with embedded line breaks must be quoted (however, many CSV
implementations do not support embedded line breaks).

     1997,Ford,E350,"Go get one now
     they are going fast"

In some CSV implementations[which?], leading and trailing spaces and
tabs are trimmed (ignored). Such trimming is forbidden by RFC 4180, which
states "Spaces are considered part of a field and should not be ignored."
1997, Ford, E350
not same as

     1997,Ford,E350

According to RFC 4180, spaces outside quotes in a field are not allowed;
however, the RFC also says that "Spaces are considered part of a field
and should not be ignored." and "Implementors should 'be conservative
in what you do, be liberal in what you accept from others' (RFC 793 [8])
when processing CSV files."

     1997, "Ford" ,E350

In CSV implementations that do trim leading or trailing spaces, fields
with such spaces as meaningful data must be quoted.

     1997,Ford,E350," Super luxurious truck "

Double quote processing need only apply if the field starts with a double
quote. Note, however, that double quotes are not allowed in unquoted
fields according to RFC 4180.

    Los Angeles,34°03′N,118°15′W
    New York City,40°42′46″N,74°00′21″W
    Paris,48°51′24″N,2°21′03″E

The first record may be a "header", which contains column names in each
of the fields (there is no reliable way to tell whether a file does this
or not; however, it is uncommon to use characters other than letters,
digits, and underscores in such column names).

    Year,Make,Model
    1997,Ford,E350
    2000,Mercury,Cougar

Example[edit]

Year

Make

Model

Description

Price

   1997 Ford E350 ac, abs, moon 3000.00
   1999 Chevy Venture "Extended Edition"  4900.00
   1999 Chevy Venture "Extended Edition, Very Large"  5000.00
   1996 Jeep Grand Cherokee MUST SELL!
    air, moon roof, loaded 4799.00

The above table of data may be represented in CSV format as follows:

   Year,Make,Model,Description,Price
   1997,Ford,E350,"ac, abs, moon",3000.00
   1999,Chevy,"Venture ""Extended Edition""","",4900.00
   1999,Chevy,"Venture ""Extended Edition, Very Large""",,5000.00
   1996,Jeep,Grand Cherokee,"MUST SELL!
   air, moon roof, loaded",4799.00


Example of a USA/UK CSV file (where the decimal separator is a period/full
stop and the value separator is a comma):

   Year,Make,Model,Length
   1997,Ford,E350,2.34
   2000,Mercury,Cougar,2.38


Example of an analogous European CSV/DSV file (where the decimal separator
is a comma and the value separator is a semicolon):

   Year;Make;Model;Length
   1997;Ford;E350;2,34
   2000;Mercury;Cougar;2,38


The latter format is not RFC 4180 compliant.[18] Compliance could be
achieved by the use of a comma instead of a semicolon as a separator and
either the international notation for the representation of the decimal
mark or the practice of quoting all numbers that have a decimal mark.

Application support[edit]

Main article: CSV application support

The CSV file format is supported by almost all spreadsheets and database
management systems. Many programming languages have libraries available
that support CSV files. Many implementations support changing the
field-separator character and some quoting conventions, although it
is safest to use the simplest conventions, to maximize the recipients'
chances of handling the data.

Microsoft Excel will open .csv files, but depending on the system's
regional settings, it may expect a semicolon as a separator instead
of a comma, since in some languages the comma is used as the decimal
separator. Excel supports the use of a "sep= " row at the beginning of the
file to change the expected delimiter (example: sep=; will cause Excel
to use ";" as the delimiter). Excel also applies some additional data
manipulations, such as reformatting what looks like numbers or dates,
eliminating leading + or 0, which breaks phone numbers, or a leading =
makes the cell a formula, where function names must be in the opener's
local language. Also, many regional versions of Excel will not be able
to deal with Unicode in CSV. One simple solution when encountering such
difficulties is to change the filename extension from .csv to .txt; then
opening the file from an already running Excel instance with the "Open"
command, where the user can manually specify the delimiters, encoding,
format of columns, etc.

Apache OpenOffice Calc and LibreOffice Calc handle CSV files and pasted
text with a Text Import dialog asking the user to manually specify the
delimiters, encoding, format of columns, etc.

Numbers (spreadsheet), the Apple equivalent of Microsoft Excel, supports
import and export of CSV files as well. In fact, this feature is one
that can be expected on almost any spreadsheet editing program.

There are many utility programs on Unix-style systems that can deal with
at least some CSV files. Many such utilities have a way to change the
delimiter character, but lack support for any other variations (or for
Unicode). Some of the useful programs are:

   column (-s to change the delimiter character(s))
   cut (-d to change the delimiter character)
   paste (-d to change the delimiter character(s))
   join (-t to change the delimiter character)
   sort (-t to change the delimiter character)
   uniq (-f to skip comparing the first N fields)
   emacs (using csv-nav mode)
   awk (-F to change the delimiter character)

See also[edit]

   * Comparison of data serialization formats
   * Delimiter-separated values
   * Delimiter collision
   * Introduction to CSV files
   * Tabular Data Package format for sharing CSV files with standardized type and dialect metadata
   * CSV Dialect Description Format (Frictionless Data)

References[edit]

1.^ Jump up to: a b c d Shafranovich, Y. (October 2005). Common Format and MIME Type for CSV Files. IETF. p. 1. RFC 4180.
2.^ Jump up to: a b Shafranovich (2005) states, "This RFC documents the format of comma separated values (CSV) files and formally registers the "text/csv" MIME type for CSV in accordance with RFC 2048".
3.Jump up ^ "CSV - Comma Separated Values". Retrieved 2017-12-02.
4.Jump up ^ "CSV Files". Retrieved June 4, 2014.
5.Jump up ^ "Comma Separated Values (CSV) Standard File Format". Edoceo, Inc. Retrieved June 4, 2014.
6.Jump up ^ IBM FORTRAN Program Products for OS and the CMS Component of VM/370 General Information (PDF) (first ed.), July 1972, p. 17, GC28-6884-0, retrieved February 5, 2016, "For users familiar with the predecessor FORTRAN IV G and H processors, these are the major new language capabilities"
7.Jump up ^ "List-Directed I/O", Fortran 77 Language Reference, Oracle
8.Jump up ^ "SuperCalc², spreadsheet package for IBM, CP/M". Retrieved December 11, 2017.
9.Jump up ^ "Comma-Separated-Value Format File Structure". Retrieved December 11, 2017.
10.Jump up ^ "CSV, Comma Separated Values (RFC 4180)". Retrieved June 4, 2014.
11.Jump up ^ See sparql11-results-csv-tsv, the first W3C recommendation scoped in CSV and filling some of RFC4180's deficiencies.
12.Jump up ^ "Model for Tabular Data and Metadata on the Web - W3C Recommendation 17 December 2015". Retrieved March 23, 2016.
13.Jump up ^ *Creativyst (2010), How To: The Comma Separated Value (CSV) File Format, creativyst.com, retrieved May 24, 2010
14.Jump up ^ "Frictionless Data 1.0 released". Open Knowledge International. 2016. Retrieved 2017-09-04.
15.Jump up ^ "CSV on the Web Working Group". W3C CSV WG. 2013. Retrieved 2015-04-22.
16.Jump up ^ CSV on the Web Repository (on GitHub)
17.Jump up ^ Model for Tabular Data and Metadata on the Web (W3C Recommendation)
18.Jump up ^ Shafranovich (2005) states, "Within the header and each record, there may be one or more fields, separated by commas."
19.Jump up ^ "EmacsWiki: Csv Nav".

Further reading[edit]
"IBM DB2 Administration Guide - LOAD, IMPORT, and EXPORT File Formats".
IBM.
Archived from the original on 2012-12-12.
Retrieved 2016-12-12.
(Has file descriptions of delimited ASCII (.DEL)
(including comma- and semicolon-separated) and non-delimited ASCII (.ASC) files for data transfer.)
