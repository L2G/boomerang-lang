#!/usr/local/bin/php

<?php include("header.php"); ?>

<a name="top"></a>
<div style="margin-top:10px;">
<table style="width:100%"><tr><td style="width:250px;">
      <img src="images/harmony-header.png" alt="harmony-header"/>
</td><td style="text-align:right; font-size:11pt; vertical-align:bottom;">
      [ <a href="#introduction">introduction</a> | 
      <a href="#demo">demo</a> | 
      <a href="#source">source</a> |
      <a href="#members">members</a> | 
      <a href="#papers">publications</a> | 
      <a href="#related">related</a> | 
      <a href="#support">support</a> ] 
</td></tr></table>
</div>

<a name="introduction"></a>
<div class="content">
<div class="title">Introduction <div class="top">[<a href="#top">Top</a>]</div></div>
<p>The <strong>Harmony</strong> system is a generic framework for
reconciling disconnected updates to heterogeneous, replicated XML
data. It can be used, for instance, to synchronize the bookmark files
of several different web browsers, allowing bookmarks and bookmark
folders to be added, deleted, edited, and reorganized by different
users running different browser applications on disconnected
machines.</p>

<p>A central theme of the Harmony project is bringing ideas from
programming languages to bear on a set of problems more commonly
regarded as belonging to the purview of databases or distributed
systems. In particular, a major component of the proposed work
concerns developing the foundations of <em>bi-directional</em>
programming languages, in which every program denotes a pair of
functions---one for extracting a <em>view</em> of some complex data
structure, and another for ``putting back'' an updated view into the
original structure. Bi-directional programs play a crucial role in the
way the system deals with heterogeneous structures, mapping between
diverse concrete application data formats and common abstract formats
suitable for synchronization. Similarly, the issue of
<em>alignment</em> during reconciliation---that is, of determining
which parts of divergent replicas are intended to represent ``the same
information''---can be addressed by focusing on the type structure of
the data being reconciled.</p>
</div>

<a name="demo"></a>
<div class="content">
<div class="title">Demo<div class="top">[<a href="#top">Top</a>]</div></div>
<p>
An extensive set of live demos and tutorials on the features of
Harmony can be found 
<a href="http://alliance.seas.upenn.edu/~harmony/cgi-bin/demo.cgi">here</a>.
</p>
</div>

<a name="source"></a>
<div class="content">
<div class="title">Source distribution<div class="top">[<a href="#top">Top</a>]</div></div>
<dl>
<dt><h2>Snapshot tarballs</h2></dt>
<dd>
<table class="spaced" style="margin:7px 0px;">    
<?php
$dh = opendir("../download");
$files = array();
while($f = readdir($dh)) {
  if(preg_match('/.tar.gz$/', $f)) { 
    $files[$f] = filemtime("../download/$f");
  }
 }
closedir($dh);
arsort($files);

$odd = true;
foreach($files as $f=>$t) {
  $size = hsize(filesize("../download/$f"));
  $date = date("j M Y g:ia", $t);    
  $trclass = "";
  if($odd) {
     $odd = false;
     $trclass = " class=\"darkyellow\"";
  } else {
    $odd = true;
    $trclass = " class=\"lightyellow\"";
  }
  print<<<EOF
    <tr$trclass><td>
      <a href="download/$f">
      <img style="vertical-align:middle;border:0;" src="images/floppy.png" alt="floppy"/>
      $f</a>
      </td><td>$size</td><td style="text-align:right">$date</td></tr>
EOF;
}
?>
</table></dd>

<dt><h2>Subversion repository access</h2></dt>
<dd>
  For anonymous, read-only access our <a href="http://subversion.tigris.org/">Subversion</a> repository, follow these steps:
  <ul>
  <li>Store the contents of <a href="harmony_key">this file</a> as <tt>~/.ssh/harmony_key</tt>.</li>
  <li>Update the file's permissions: <tt>chmod 600 ~/.ssh/harmony_key</tt></li>
  <li>Add the line <tt>IdentityFile ~/.ssh/harmony_key</tt> to <tt>~/.ssh/config</tt>.</li>
  <li>Check out the repository: <tt>svn checkout svn+ssh://harmony@halfdome.cis.upenn.edu/trunk</tt>.</li>
  </ul>
  Commit access for frequent contributors can be arranged.
</dd>
</dl>
</div>

<table style="width:100%;">
<tr><td style="width:50%;padding-right:3px;">
  <a name="members"></a>
  <div class="content" style="margin:0;">
    <div class="title">Project members</div>
    <table><tr><td>
    <ul>
      <li><a href="http://www.cis.upenn.edu/~bohannon/">Aaron Bohannon</a></li>
      <li><a href="http://www.cis.upenn.edu/~jnfoster/">Nate Foster</a></li>
      <li><a href="http://www.cis.upenn.edu/~sanjeev">Sanjeev Khanna</a></li>
      <li><a href="http://www.cis.upenn.edu/~kkunal">Keshav Kunal</a></li> 
    </ul>
    </td><td>
    <ul>
      <li><a href="http://www.cis.upenn.edu/~bcpierce/">Benjamin C. Pierce</a> (project leader)</li>      <li><a href="http://sardes.inrialpes.fr/~aschmitt/">Alan Schmitt</a></li>
      <li><a href="http://www.seas.upenn.edu/~vaughan2/">Jeff Vaughan</a></li>
    </ul>
    </td></tr></table>
  </div>
</td><td style="width:50%;padding-left:3px;">
  <div class="content" style="margin:0;">
  <div class="title">Past contributors<div class="top">[<a href="#top">Top</a>]</div></div>
    <table><tr><td>
    <ul>
      <li>Malo Denielou</li>
      <li><a href="http://www.cis.upenn.edu/~mbgreen">Michael Greenwald</a>
      <li>Owen Gunden</li>
      <li><a href="http://www.seas.upenn.edu/~lescuyer">St&eacute;phane Lescuyer</a></li>
    </ul>
    </td><td>
    <ul>
      <li><a href="http://www.cis.upenn.edu/~jonm">Jon Moore</a></li>
      <li><a href="http://www.cis.upenn.edu/~zheyang">Zhe Yang</a></li>
    </ul>
    </td></tr></table>
  </div>
</td></tr></table>

<a name="papers"></a>
<div class="content">
<div class="title">Publications<div class="top">[<a href="#top">Top</a>]</div></div>
<h2>Papers</h2> <?php include("harmony-papers.html"); ?>
<h2>Talks</h2> <?php include("harmony-talks.html"); ?>
<h2>Miscellaneous</h2> <?php include("harmony-misc.html"); ?>
</div>

<a name="related"></a>
<div class="content">
<div class="title">Related projects<div class="top">[<a href="#top">Top</a>]</div></div>
<ul>
  <li> Many forms of synchronization can be found in present-day distributed systems: 
    low-level OS support for distributed filesystems with optimistic replication, 
    consistency protocols for replicated databases, middleware support for groupware 
    applications, and user-level utilities for synchronizing PDAs with desktop 
    databases and laptop filesystems with fileservers. Our earlier work in this 
    area, in the <a href="http://www.cis.upenn.edu/~bcpierce/unison/">Unison</a> project, 
    has focused on the latter category of tools, commonly called <i>file synchronizers</i>. 
    The goal of the Harmony project is to to apply our experience with file synchronization 
    to the related but more general domain of synchronizing arbitrary tree-structured 
    data. Visit the <a href="http://www.cis.upenn.edu/~bcpierce/unison/">Unison home page</a> 
    for more information.</li>
  <li>We also maintain a (very incomplete) list of links to <a href="related.html">related 
    projects</a> on synchronization.</li>
</ul>
</div>

<a name="support"></a>
<div class="content">
<div class="title">Support<div class="top">[<a href="#top">Top</a>]</div></div>
<p>The Harmony project is supported by the National Science Foundation under 
  the following grants:</p>
  <ul>
     <li>ITR-0113226: <i>Principles and Practice of Synchronization</i></li>
     <li>CPA-0429836: <i>Harmony: The Art of Reconciliation</i></li>
     <li>IIS-0534592: <i>Linguistic Foundations for XML View Update</i></li>
  </ul>
</div>

<div style="margin-bottom:10px;text-align:right;">
<img src="images/harmony-footer.png" alt="harmony-footer"/>
</div>
<?php include("footer.php"); ?>
