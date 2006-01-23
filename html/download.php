#!/usr/local/bin/php

<?php 

include("header.php");

function hsize($size) {
   if($size == 0) { return("0 Bytes"); }
   $filesizename = array(" Bytes", " KB", " MB", " GB", " TB", " PB", " EB", " ZB", " YB");
   return round($size/pow(1024, ($i = floor(log($size, 1024)))), 2) . $filesizename[$i];
}

print<<<TOP
<div class="content">
<div class="title">Harmony Source Code</div>
TOP;

$dh = opendir("../download");
while($f = readdir($dh)) {
  if(preg_match('/.tar.gz$/', $f)) { 
  $size = hsize(filesize("../download/$f"));
  print<<<EOF
   <a href="download/$f">$f</a> [$size]<br>
EOF;
  }
}
closedir($dh);

print<<<BOT
</div>
BOT;
include("footer.php"); 
?>