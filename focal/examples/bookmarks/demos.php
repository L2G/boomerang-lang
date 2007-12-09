<?
$demogroupname = "Bookmarks";
$demo["default_wide_w"] = 910;
$demo["default_w"] = ($demo["default_wide_w"] - 10) / 2;
$demo["r1_w"] = $demo["r2_w"] = $demo["default_wide_w"];
$demo["r1_h"] = $demo["r2_h"] = 250;
$demo["schema"] = "Bookmarks.Abstract";

##############################################################################################

$moz = <<<MOZ
<!DOCTYPE NETSCAPE-Bookmark-file-1>
<!-- This is an automatically generated file.
     It will be read and overwritten.
     DO NOT EDIT! -->
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=UTF-8">
<TITLE>Bookmarks</TITLE>
<H1 LAST_MODIFIED="1130422106">Bookmarks</H1>

<DL><p>
    <DT><H3 ADD_DATE="1130422037" LAST_MODIFIED="1130422057" PERSONAL_TOOLBAR_FOLDER="true" ID="rdf:#$vu1Zz3">Bookmarks Toolbar Folder</H3>
    <DL><p>
        <DT><A HREF="http://www.google.com" ADD_DATE="1130422047" LAST_MODIFIED="1130422053" ID="rdf:#$wu1Zz3">Google</A>
    </DL><p>
    <DT><H3 ADD_DATE="1130422060" LAST_MODIFIED="1130422181" ID="rdf:#$xu1Zz3">News</H3>
    <DL><p>
        <DT><H3 ADD_DATE="1130422158" LAST_MODIFIED="1130422218" ID="rdf:#$Au1Zz3">Geek</H3>
        <DL><p>
            <DT><A HREF="http://www.macosxhints.com" ADD_DATE="1130422218" LAST_MODIFIED="1130422240" ID="rdf:#$Cu1Zz3">Mac OS X Hints</A>
            <DT><A HREF="http://slashdot.org/" ADD_DATE="1130422172" LAST_VISIT="1130422322" LAST_MODIFIED="1130422179" LAST_CHARSET="ISO-8859-1" ID="rdf:#$Bu1Zz3">Slashdot</A>
        </DL><p>
        <DT><A HREF="http://nytimes.com" ADD_DATE="1130422065" LAST_MODIFIED="1130422138" ID="rdf:#$yu1Zz3">New York Times</A>
        <DT><A HREF="http://chronicle.com/" ADD_DATE="1130422080" LAST_MODIFIED="1130422102" ID="rdf:#$zu1Zz3">Chronicle of Higher Education</A>
    </DL><p>
</DL><p>
MOZ;

#############################################################################################

$saf = <<<SAF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>Children</key>
	<array>
		<dict>
			<key>Children</key>
			<array>
				<dict>
					<key>URIDictionary</key>
					<dict>
						<key></key>
						<string>http://www.google.com</string>
						<key>title</key>
						<string>Google</string>
					</dict>
					<key>URLString</key>
					<string>http://www.google.com</string>
					<key>WebBookmarkType</key>
					<string>WebBookmarkTypeLeaf</string>
					<key>WebBookmarkUUID</key>
					<string>037AF4AC-708A-4B30-8878-43E5691E191A</string>
				</dict>
			</array>
			<key>Title</key>
			<string>BookmarksBar</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeList</string>
			<key>WebBookmarkUUID</key>
			<string>170CDD4F-2754-45EA-A627-A2160D7382AE</string>
		</dict>
		<dict>
			<key>Children</key>
			<array>
				<dict>
					<key>Children</key>
					<array>
						<dict>
							<key>Children</key>
							<array>
								<dict>
									<key>URIDictionary</key>
									<dict>
										<key></key>
										<string>http://www.macosxhints.com</string>
										<key>title</key>
										<string>Mac OS X Hints</string>
									</dict>
									<key>URLString</key>
									<string>http://www.macosxhints.com</string>
									<key>WebBookmarkType</key>
									<string>WebBookmarkTypeLeaf</string>
									<key>WebBookmarkUUID</key>
									<string>77439671-025D-4738-BF99-3C50873EE380</string>
								</dict>
								<dict>
									<key>URIDictionary</key>
									<dict>
										<key></key>
										<string>http://slashdot.org/</string>
										<key>title</key>
										<string>Slashdot</string>
									</dict>
									<key>URLString</key>
									<string>http://slashdot.org/</string>
									<key>WebBookmarkType</key>
									<string>WebBookmarkTypeLeaf</string>
									<key>WebBookmarkUUID</key>
									<string>ED84E843-2E7A-4546-9504-8405C9C81F0E</string>
								</dict>
							</array>
							<key>Title</key>
							<string>Geek</string>
							<key>WebBookmarkType</key>
							<string>WebBookmarkTypeList</string>
							<key>WebBookmarkUUID</key>
							<string>D257AD2E-6E7A-47CA-80E8-7B65866F8EA3</string>
						</dict>
						<dict>
							<key>URIDictionary</key>
							<dict>
								<key></key>
								<string>http://nytimes.com</string>
								<key>title</key>
								<string>New York Times</string>
							</dict>
							<key>URLString</key>
							<string>http://nytimes.com</string>
							<key>WebBookmarkType</key>
							<string>WebBookmarkTypeLeaf</string>
							<key>WebBookmarkUUID</key>
							<string>34807109-9BD7-48CE-B69C-A3E6E3634980</string>
						</dict>
						<dict>
							<key>URIDictionary</key>
							<dict>
								<key></key>
								<string>http://chronicle.com/</string>
								<key>title</key>
								<string>Chronicle of Higher Education</string>
							</dict>
							<key>URLString</key>
							<string>http://chronicle.com/</string>
							<key>WebBookmarkType</key>
							<string>WebBookmarkTypeLeaf</string>
							<key>WebBookmarkUUID</key>
							<string>A90B567C-7B75-42FA-B787-E3C0ADA7C1F9</string>
						</dict>
					</array>
					<key>Title</key>
					<string>News</string>
					<key>WebBookmarkType</key>
					<string>WebBookmarkTypeList</string>
					<key>WebBookmarkUUID</key>
					<string>0F4FFD03-46A1-43D2-AA73-9015CF096E99</string>
				</dict>
			</array>
			<key>Title</key>
			<string>BookmarksMenu</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeList</string>
			<key>WebBookmarkUUID</key>
			<string>E45C6AB2-6705-4246-A6B2-334F3F7B96C1</string>
		</dict>
		<dict>
			<key>Title</key>
			<string>Address Book</string>
			<key>WebBookmarkIdentifier</key>
			<string>Address Book Bookmark Proxy Identifier</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeProxy</string>
			<key>WebBookmarkUUID</key>
			<string>7CA58A0F-307C-4D94-BAD4-E66E080D40B7</string>
		</dict>
		<dict>
			<key>Title</key>
			<string>Bonjour</string>
			<key>WebBookmarkIdentifier</key>
			<string>Rendezvous Bookmark Proxy Identifier</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeProxy</string>
			<key>WebBookmarkUUID</key>
			<string>3ABAF175-6A94-486F-B32F-ACEC0BE3FC41</string>
		</dict>
		<dict>
			<key>Title</key>
			<string>History</string>
			<key>WebBookmarkIdentifier</key>
			<string>History Bookmark Proxy Identifier</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeProxy</string>
			<key>WebBookmarkUUID</key>
			<string>B0BDE4F2-C2C7-4A23-A093-329CD82496E8</string>
		</dict>
		<dict>
			<key>Title</key>
			<string>All RSS Feeds</string>
			<key>WebBookmarkIdentifier</key>
			<string>RSS Bookmark Proxy Identifier</string>
			<key>WebBookmarkType</key>
			<string>WebBookmarkTypeProxy</string>
			<key>WebBookmarkUUID</key>
			<string>CDE7622E-2E2B-43C4-B075-ADFB3C40B50E</string>
		</dict>
	</array>
	<key>WebBookmarkFileVersion</key>
	<integer>1</integer>
	<key>WebBookmarkType</key>
	<string>WebBookmarkTypeList</string>
	<key>WebBookmarkUUID</key>
	<string>00000000-0000-0000-0000-000000000000</string>
</dict>
</plist>
SAF;

################################################################################

# ---------------------------------------------------------
$demo["instr"] = <<<XXX

<div id="section">Bookmarks</div>

<p>
This demo shows how we can use Harmony to synchronize bookmark data
between the Mozilla Firefox and Safari web browsers. Each browser uses
a different format to represent bookmark data---Firefox represents
bookmarks in an HTML format whereas Safari uses a compressed, binary
XML representation. To handle these heterogeneous formats, our tool
uses <i>lenses</i> to transform each bookmark file from its native
representation into a common abstract format suitable for
synchronization.
</p>

<p>
Let's start by looking at the behavior of the Firefox lens.  The
essential structure of bookmark data is just a list of items where an
item can either be a link, with both <tt>name</tt> and <tt>url</tt>
data, or a folder, with a <tt>name</tt> and a <tt>contents</tt>
leading to a nested list of items.  Because most browers support a
distinguished "toolbar" folder, we represent a complete bookmark file
as a tree with exactly two children at the top level, <tt>toolbar</tt>
and <tt>bookmarks</tt>, each leading to a folder of items.

The HTML format used by Firefox includes many additional bits of data
including the date and time that each date was added and last
modified, an ID field containing RDF data, and a cached copy of
the <tt>favicon.ico</tt> icon for each site.  The get function of the
projects away all of this data, which is inessential for the purpose
of synchronizing with Safari bookmark files, and renders the bookmark
file as a tree in the abstract format described above.
</p>

<p>
The schema that describes such abstract trees (written in our schema
description language syntax) is as follows:
<pre>
  schema Link   = { "name"=Value, 
                    "url"=Value }
  schema Folder = { "name"=Value, 
                    "contents"=Contents }     
  and Contents  = List.T (Folder | {"link"=Link})
  schema Abstract = { "bookmark"=Contents, 
                       "toolbar"=Contents }
</pre>
</p>

<p>
The first replica in this example shows a sample Mozilla bookmark file
in its concrete HTML format; the second shows the same set of
bookmarks in "meta" format, after the Firefox lens has been
applied. Shorten the link for Google to <tt>http://google.com</tt> in
the Mozilla replica and add a link whose name is <tt>"Lambda The
Ultimate"</tt> and URL is <tt>http://lambda-the-ultimate.org</tt> to
the "Geek" folder. Press "Synchronize" to see how these updates are
reflected on each side.
</p>

<p>
Then press "Next" to continue with the next example.
</p>
XXX;
# ---------------------------------------------------------
$demo["forcer1"] = true;
$demo["r1"] = $moz; 
$demo["r1format"] = "html";
$demo["r2format"] = "meta";
$demo["l1"] = "Mozilla.l2";
$demo["l2"] = "id";
savedemo();

#################################################################################

$demo["instr"] = <<<XXX

Next let's look at the behavior of the Safari lens. The first replica
now shows the same bookmark data as in the previous demo as
represented in Safari's XML format. Safari bookmarks are quite verbose
and like Firefox, contain many inessential and redundant bits of
data. For example, notice that URLs, such
as <tt><string>http://www.google.com</string></tt>, appear twice in
the XML elements representing them.  Other fields, such
as <tt>WebBookmarkUUID</tt>, <tt>WebBookmarkType</tt>, etc., are
included for use by other MacOS tools (e.g., iSync) but do not
correspond to any data on the Mozilla side. The get component of the
Safari lens extracts just the essential bits---the folder structure
and links---from a bookmark file, merging rendundant data and
projecting away the inessential parts.</p>

<p>
Make the same updates to each replica as in the last example---in the
first replica, change <i>both</i> URL strings in the Google link
to <tt>http://google.com</tt>; in the second, add a link to the "Geek"
folder for Lambda The Ultimate:
<pre>
  {"link"={name={"Lambda The Ultimate"}, 
   "url"={"http://lambda-the-ultimate.org"}}}
</pre>
Press "Synchronize" and verify that the updates are successfully
propagated to each replica.
</p>

<p>Then click "Next" to continue.</p>
XXX;

$demo["forcer1"] = true;
$demo["r1"] = $saf; 
$demo["r1format"] = "xml";
$demo["r2format"] = "meta";
$demo["l1"] = "Safari.l2";
$demo["l2"] = "id";
savedemo();

#################################################################################

$demo["forcer1"] = false;
$demo["r1"] = $moz; 
$demo["r2"] = $saf;
$demo["r1format"] = "html";
$demo["r2format"] = "xml";
$demo["l1"] = "Mozilla.l2";
$demo["l2"] = "Safari.l2";
$demo["la"] = "id";
$demo["ar"] = "{}";
$demo["instr"] = <<<XXX

<p>
Now let's put these two pieces together and see how Harmony
synchronizes a Safari bookmark file with one from Firefox.
Make the following updates:
<ul>
<li>in the Mozilla replica: add a new link for Wadler's blog to the "Geek" folder: 
<pre>
  &lt;DT&gt;&lt;A HREF="http://wadler.blogspot.org/"&gt;Wadler's Blog&lt;/A&gt;
</pre>
<li>in the Safari replica: delete the Google entry from the toolbar;
<li>in the Safari replica: add a new link for BBC News to the "News" folder:
<pre>
&lt;dict&gt;
  &lt;key&gt;URIDictionary&lt;/key&gt;
  &lt;dict&gt;
    &lt;key&gt;&lt;/key&gt;
    &lt;string&gt;http://news.bbc.co.uk&lt;/string&gt;
    &lt;key&gt;title&lt;/key&gt;
    &lt;string&gt;BBC News&lt;/string&gt;
    &lt;/dict&gt;
    &lt;key&gt;URLString&lt;/key&gt;
    &lt;string&gt;http://news.bbc.co.uk&lt;/string&gt;
    &lt;key&gt;WebBookmarkType&lt;/key&gt;
    &lt;string&gt;WebBookmarkTypeLeaf&lt;/string&gt;
&lt;/dict&gt;
</pre>
</ul>
Press "Synchronize" and verify that the changes appear on both
sides.  (Because the concrete formats are so verbose, it may be easiest to
examine the abstract trees; simply click "Show Abstract
Trees" to display them.)</p>

<p>
Then click "Next" to continue.
</p>

XXX;
savedemo();

#################################################################################

$demo["instr"] = <<<XXX
Thus far, all of the examples in this demo have been carefully
constructed so that the updates made to each replica do not
overlap. However, in a real-world scenario, this is not always likely
to be the case. 

Harmony's generic synchronization algorithm is a simple, recursive
tree walk that traverses the structure of the tree, merging updates
along the way. For lists, which are encoded as simple cons cells, this
means that ordered data is synchronized by <i>absolute</i>
position. To see what can go wrong with this simple strategy, try
adding a new link at the head of the list in the "News" folder: 
<ul>
<li>in the Mozilla replica add: 
<pre>
  &lt;DT&gt;&lt;A HREF="http://slate.com/"&gt;Slate&lt;/A&gt;
</pre>
</li>
<li>in the abstract view add: 
<pre>
  {"link"={name="Washington Post", 
   "url"="http://www.washingtonpost.com"}},
</pre>
</li>
</ul>
Then click "Synchronize". What happens?
</p>

<p>
A <i>schema domain conflict</i> results because the synchronization
algorithm, which has aligned the entries by absolute position in the
list, is unable merge the two newly added links into a tree
representing a single valid link. E.g., merging the two trees produces
a result:
<pre>
{"link"={name={"Slate", "Washington Post"}, 
 "url"={"http://slate.com", "http://www.washingtonpost.com"}}}
</pre>
which does not belong to the <tt>Bookmarks.Abstract</tt> schema.
</p>

<p>
Press "Next" to move on to the final example, where we explore two
ways of handing this problem.
</p>

XXX;

$demo["forcer1"] = true;
$demo["r1"] = $moz;
$demo["r1format"] = "html";
$demo["r2format"] = "meta";
$demo["l1"] = "Mozilla.l2";
$demo["l2"] = "id";
$demo["schema"] = "Bookmarks.Abstract";
savedemo();

#################################################################################

$demo["forcer1"] = true;
$demo["r1"] = $moz;
$demo["r1format"] = "html";
$demo["r2format"] = "meta";
$demo["l1"] = "Mozilla.l3";
$demo["l2"] = "id";
$demo["schema"] = "Bookmarks.BushAbstract";
$demo["flags"] = "-unordered ";
$demo["instr"] = <<<XXX
<p>
Rather than representing a bookmark folder as a <i>list</i> of items,
we can choose to represent a folder as a <i>bush</i> where the URL of
each link and the name of each folder serves as a key field for each
item. The schema for this representation looks like this:
<pre>
  schema Item = { ?"folders"={*=Item}, 
                  ?"links"={*=Value }}
  schema BushAbstract = { "bookmarks"=Item, 
                          "toolbar"=Item }
</pre>
In this example, the Firefox bookmarks are passed to a lens that
produces an abstract view in this unordered schema. Try making the
same updates to the "Geeks" folder as in the last example (rename the
Google URL in two different ways). What happens now?
</p>

<p>
Another possibility is to use a fancier list encoding with a key field
raised up above each cons cell. Using this representation, the
synchronization algorithm propagates changes between two lists as long
as both keys have not changed; when both keys have changed, it leaves
the rest of the list unchanged. For examples of how this works, see
our DBPL 2005 paper.
</p>
XXX;
savedemo();


?>
