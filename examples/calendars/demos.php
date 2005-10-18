<?

$demogroupname = "Calendars";
$demo["default_h"] = 100;
$demo["r1_h"] = $demo["r2_h"] = 300;
$demo["forcer1"] = true;
$demo["schema"] = "ICalendar.ICalendar_A";
$demo["l1"] = $demo["l2"] = "ICalendar.l_stamps";
$demo["r1format"] = $demo["r2format"] = "ics";

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
This demo shows how Harmony can be used to synchronize calendar files
between organizers that use the iCalendar format (e.g., iCal on
MacOS/X, Evolution or kOrganizer on Linux systems, Outlook on
Win32).
<p>
A calendar file in iCalendar format is a list of entries
of different sorts, like events, to-dos, and other less frequently
used kinds of entries. The first example consists in a single event entry
occuring on August 10th, 2005, between 10:00 AM and 2:00 PM.
<p>
Try modifying the <tt>LOCATION</tt> or <tt>SUMMARY</tt> of the event in the first 
replica and see how this is synchronized by Harmony.
<p>
(NB: When adding text directly to an iCalendar file, you need to be careful with 
special characters like comma and semi-colon, who should be escaped. Also, 
an iCalendar file must not be wider than 75 colums; a line beginning with
exactly one white space is parsed as a continuation of the previous line.)
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
LOCATION:In a galaxy far\, far away
CLASS:PUBLIC
PRIORITY:3
CATEGORIES:Business
DTSTART:20050810T150000Z
DTEND:20050810T190000Z
TRANSP:OPAQUE
END:VEVENT

END:VCALENDAR

XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Now, let us take a look at how iCalendar files are encoded into our
native meta format.
<p>
This file contains exactly the same event as in the previous demo;
you can see how the data is rearranged in the second replica. 
The abstract view is a bush where the entries are sorted with respect
 to their type (here, we only have one entry of sort <tt>Eventc</tt>, 
so the other sorts do not appear). In each sort, the entries are keyed
 under an unique identifier, which is part of the iCalendar specification.
This uuid is key to the synchronization of calendars, since it offers a convenient and
easy way to align the entries between two files, without considering
their relative order. 
<p>
To illustrate the importance of the uuid, try editing the uuid in each
replica to something different.  What will happen is
that Harmony will not find the old key (the only one present in the 
archive) in either file, and will therefore assume that it has been deleted
from both replicas. On the other hand, each file will contain a new entry
that does not exist on the other side, so both entries (though equal besides
their uuids) will be added to the archive as completely separate entries.
XXX;
# ---------------------------------------------------------
$demo["r2format"] = "meta";
$demo["l2"] = "id";
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Now, if you looked carefully at the first demo, you may have noticed
that even if you were just modifying the location of the event in
the second replica, the changes done in the first replica by the
harmonizer were involving a change to the <tt>LAST-MODIFIED</tt> property.
(If you didn't notice, well, you are welcome to try now with this example :-)
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
making changes to your calendar, and that is what any organizer application would  have 
done (it is not very common to edit iCalendar files manually).
<p>
When you are done experimenting with lastmods, proceed to next part of the
to see another similar issue that we faced when developing this calendar
synchronizer.
XXX;
savedemo();
# ---------------------------------------------------------

##############################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX
Each calendar entry has a <tt>DTSTAMP</tt> property, containing the date
where this instance of the entry was created. It is different from the
<tt>DTCREATE</tt> property, which is the date of first creation of the entry,
in the sense that it has to be updated to the current time when the 
corresponing entry is copied from one file to another.
<p>
Now, in Focal, we have a convenient way to know when we are <i>creating</i> a
part of the view on the putback : this is exactly when the concrete view is 
<tt>missing</tt>. Therefore, we managed to handle the <tt>DTSTAMP</tt> as
appropriate by using a special <tt>const</tt> lens, whose default tree can
be described via a shell command. We then filter away the original 
<tt>DTSTAMP</tt> value on the get way, and replace it during putback, or
use calls to 'date' in the case of creation.
<p>
To check this yourself, select the calendar entry below and
copy-paste it after the sole entry of the file in the first replica
(between <tt>END:VEVENT</tt> and <tt>END:VCALENDAR</tt>). Then synchronize
and verify that the <tt>DTSTAMP</tt> on the right has been updated as
appropriate.
<p>
<tt>
BEGIN:VEVENT<br>
DTSTAMP:20050810T215429Z<br>
ORGANIZER:MAILTO:lescuyer@seas.upenn.edu<br>
CREATED:20050810T213931Z<br>
UID:KOrganizer-168648156.763<br>
SEQUENCE:0<br>
LAST-MODIFIED:20050809T215425Z<br>
SUMMARY:A presentation of Harmony<br>
LOCATION:307 Levine<br>
CLASS:PUBLIC<br>
PRIORITY:5<br>
CATEGORIES:Education<br>
DTSTART:20050810T210000Z<br>
DTEND:20050810T230000Z<br>
TRANSP:OPAQUE<br>
END:VEVENT<br>
</tt>
XXX;
# ---------------------------------------------------------
$demo["r2format"] = "ics";
$demo["l2"] = $demo["l1"];
savedemo();
# ---------------------------------------------------------

?>
