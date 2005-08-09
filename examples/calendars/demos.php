<?

$demogroupname = "Calendars";

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
This demo shows how Harmony can be used to synchronize calendar files
between organizers that use the iCalendar format (e.g., iCal on
MacOS/X, Evolution or kOrganizer on Linux systems, Outlook on
Win32).
<p>
A calendar file in iCalendar format is a list of entries
of different sorts, like events, to-dos, and other less frequently
used kinds of entries. The first example consists in a single event entry
occuring on August 10th, 2005, between 10:00 AM and 2:00 PM.
Try to modify the <tt>LOCATION</tt> or <tt>SUMMARY</tt> of the event in the first 
replica, and close your editor to see how the files will be synchronized by Harmony.
You can do that as many times as desired. If you wish to end the demo, just
 call your editor with the files unchanged. When you are done, proceed to the
next step by typing [make demo2].
<p>
NB : When adding text directly to an iCalendar file, pay attention to the 
special characters like comma or semi-colon, who should be escaped. Also, 
an iCalendar file must not be wider than 75 colums, and each line beginning with
exactly one white space is parsed as the following of the previous line.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
BEGIN:VCALENDAR
PRODID:-//K Desktop Environment//NONSGML KOrganizer 3.3//EN
VERSION:2.0

BEGIN:VEVENT
DTSTAMP:20050809T215429Z
ORGANIZER:MAILTO:lescuyer@seas.upenn.edu
CREATED:20050809T213931Z
UID:KOrganizer-168648156.762
SEQUENCE:2
LAST-MODIFIED:20050809T215425Z
SUMMARY:Something very important
LOCATION:In a galaxy far, far away
CLASS:PUBLIC
PRIORITY:3
CATEGORIES:Business
DTSTART:20050810T150000Z
DTEND:20050810T190000Z
TRANSP:OPAQUE
END:VEVENT

END:VCALENDAR
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "ics";
$demo["r2format"] = "ics";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Now, let us take a look at how iCalendar files are encoded into our
native meta format.
<p>
This file consists in exactly the same event as in the demo1 and
you can see how the data is rearranged in the right replica. 
The abstract view is a bush where the entries are sorted with respect
 to their type (here, we only have one entry of sort <tt>Eventc</tt>, 
so the other sorts do not appear). In each sort, the entries are keyed
 under an unique identifier, which is part of the iCalendar specification.
This uuid is the cornerstone of our system, since it is a convenient and
easy way to align the entries between two files, without considering
their relative order. 
<p>
To illustrate the importance of this uuid, try forcing to uuid in both 
files to something different, and close the editor. What will happen is
that Harmony will not find the old key (the only one present in the 
archive) in either file, and will therefore assume that it has been deleted
from both replicas. On the other hand, each file will contain a new entry
that does not exist on the other side, so both entries (though equal besides
their uuids) will be added to the archive as completely separate entries.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
BEGIN:VCALENDAR
PRODID:-//K Desktop Environment//NONSGML KOrganizer 3.3//EN
VERSION:2.0

BEGIN:VEVENT
DTSTAMP:20050809T215429Z
ORGANIZER:MAILTO:lescuyer@seas.upenn.edu
CREATED:20050809T213931Z
UID:KOrganizer-168648156.762
SEQUENCE:2
LAST-MODIFIED:20050809T215425Z
SUMMARY:Something very important
LOCATION:In a galaxy far, far away
CLASS:PUBLIC
PRIORITY:3
CATEGORIES:Business
DTSTART:20050810T150000Z
DTEND:20050810T190000Z
TRANSP:OPAQUE
END:VEVENT

END:VCALENDAR
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "ics";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instructions"] = <<<XXX
Now, if you looked carefully at the first demo, you may have noticed
that even if you were just modifying the location of the event in
the second replica, the changes done in the first replica by the
harmonizer were involving a change to the <tt>LAST-MODIFIED</tt> property.
If you have not, well, you are welcome to try now with this example :-)
<p>
Indeed, what happens is that the synchronizer detects updates to an entry
thanks to a special primitive lens called <tt>fmodify</tt>, and updates the
date as appropriate using calls to the Unix 'date' command. For this reason,
the <tt>LAST-MODIFIED</tt> is not kept in the abstract view, as you can see
in the view below, since it must not be part of the synchronized data.
<p>
Note that if you modify only one replica, the <tt>LAST-MODIFIED</tt>
property is not updated by the synchronizer in the entry that you 
manually modified. In fact, you should have done that yourself when
making changes to your calendar, and that is what any organizer would 
have been doing if you had not edited this file manually.
<p>
When you are done experimenting with lastmods, proceed to the demo3
to see another similar issue that we faced when developing this calendar
synchronizer.
XXX;
# ---------------------------------------------------------
$demo["r1"] = <<<XXX
BEGIN:VCALENDAR
PRODID:-//K Desktop Environment//NONSGML KOrganizer 3.3//EN
VERSION:2.0

BEGIN:VEVENT
DTSTAMP:20050809T215429Z
ORGANIZER:MAILTO:lescuyer@seas.upenn.edu
CREATED:20050809T213931Z
UID:KOrganizer-168648156.762
SEQUENCE:2
LAST-MODIFIED:20050809T215425Z
SUMMARY:Something very important
LOCATION:In a galaxy far, far away
CLASS:PUBLIC
PRIORITY:3
CATEGORIES:Business
DTSTART:20050810T150000Z
DTEND:20050810T190000Z
TRANSP:OPAQUE
END:VEVENT

END:VCALENDAR
XXX;
# ---------------------------------------------------------
$demo["r1format"] = "ics";
$demo["r2format"] = "meta";
savedemo();
# ---------------------------------------------------------

?>