#!/usr/local/bin/php

<?php include("header.php"); ?>

<a name="top"></a>
<div style="margin-top:10px;">
<table style="width:100%"><tr><td style="width:250px;">
      <img src="images/harmony-header.png" alt="harmony-header"/>
</td><td style="text-align:right; font-size:11pt; vertical-align:bottom;">
      [ <a href="#introduction">introduction</a> | 
      <a href="#distrib">distribution</a> | 
      <a href="#members">members</a> | 
      <a href="#papers">publications</a> | 
      <a href="#support">support</a> ] 
</td></tr></table>
</div>

<a name="introduction"></a>
<div class="content">
<div class="title">Introduction <div class="top">[<a href="#top">Top</a>]</div></div>

<p><strong>Harmony</strong> is framework for defining, updating, 
    and synchronizing <i>views</i> over strings, trees, and relations. 
It can be used, for example, to synchronize the bookmark files 
of several different web browsers, to convert address books represented 
in vCard to CSV, and to map between BiBTeX and RIS-formatted bibliographic
databases.</p>

<p>A central theme of the project is bringing ideas from
programming languages to bear on a set of problems more commonly
regarded as belonging to the purview of databases. Views are usually 
discussed in the context of databases, but actually arise in a host 
of applications across many diverse areas of computing.  In many of 
these applications, is necessary to apply an update to the view, and 
have the update be reflected back on the source. Unfortunately, the 
view update problem remains largely unsolved; except in very simple 
cases, views are read-only.  Our goal is to develop <em>programming 
languages</em> in which programmers can specify both how to calculate 
a view, and how to translate updates to a view back to the source, and
to use these <em>bidirectional programs</em>&#x2014;called 
<em>lenses</em>&#x2014;to build converters and synchronizers for a 
variety of real-world data formats.</p>
</div>

<a name="distrib"></a>
<div class="content">
<div class="title">Distribution<div class="top">[<a href="#top">Top</a>]</div></div>
<p>
Harmony is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation.
</p>
<p>
Harmony is composed of two largely distinct systems: 
<ul>
<li><strong>Boomerang:</strong> our current work, which focuses on lenses for string data.</li>
<li><strong>Focal:</strong> older work on lenses and synchronizers for trees and relations.</li>
</li>
</ul></p>

<h2>Binary Distribution</h2>
<p>We offer pre-compiled Boomerang binaries for Linux (x86), OS X (x86 and PPC), and Window (Cygwin):
<table class="spaced" style="margin:7px 0px;">    
<?php
$dh = opendir("../download");
$files = array();
while($f = readdir($dh)) {
  if(preg_match('/binary.tar.gz$/', $f)) { 
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
</p>

<h2>Source Distribution</h2>
<p>
<table class="spaced" style="margin:7px 0px;">    
<?php
$dh = opendir("../download");
$files = array();
while($f = readdir($dh)) {
  if(preg_match('/source.tar.gz$/', $f)) { 
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
</p>

<h2>Subversion Access</h2>
<p>
  For anonymous, read-only access our <a href="http://subversion.tigris.org/">Subversion</a> repository, follow these steps:
  <ul>
  <li>Store the contents of <a href="harmony_key">this file</a> as <tt>~/.ssh/harmony_key</tt>.</li>
  <li>Update the file's permissions: <tt>chmod 600 ~/.ssh/harmony_key</tt></li>
  <li>Add the line <tt>IdentityFile ~/.ssh/harmony_key</tt> to <tt>~/.ssh/config</tt>.</li>
  <li>Check out the repository: <tt>svn checkout svn+ssh://harmony@halfdome.cis.upenn.edu/trunk</tt>.</li>
  </ul>
  Commit access for frequent contributors can be arranged.
</p>

<h2>Online Demos</h2>
An extensive set of live demos and tutorials on the features of Focal can be found 
<a href="http://alliance.seas.upenn.edu/~harmony/cgi-bin/demo.php">here</a>.
</p>
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
      <li><a href="http://www.cis.upenn.edu/~bcpierce/">Benjamin C. Pierce</a> (project leader)</li>      <li><a href="http://sardes.inrialpes.fr/~aschmitt/">Alan Schmitt</a></li>
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
      <li><a href="http://www.cis.upenn.edu/~kkunal">Keshav Kunal</a></li> 
      <li><a href="http://www.lri.fr/~lescuyer/">St&eacute;phane Lescuyer</a></li>
    </ul>
    </td><td>
    <ul>
      <li>Alexandre Pilkiewicz</li>
      <li>Jon Moore</li>
      <li><a href="http://www.seas.upenn.edu/~vaughan2/">Jeff Vaughan</a></li>
      <li><a href="http://research.microsoft.com/users/zhey/">Zhe Yang</a></li>
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
