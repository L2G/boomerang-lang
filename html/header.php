<?php
function hsize($size) {
   if($size == 0) { return("0 Bytes"); }
   $filesizename = array(" Bytes", " KB", " MB", " GB", " TB", " PB", " EB", " ZB", " YB");
   return round($size/pow(1024, ($i = floor(log($size, 1024)))), 2) . $filesizename[$i];
}
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
 "http://www.w3.org/TR/1999/REC-html401-19991224/loose.dtd">
<HTML>
<HEAD>
   <TITLE>Harmony Project home page</TITLE>
   <STYLE type="text/css">
body {
  background: #eeeeee;
  width:90%;
  margin:0 auto;
}
body,td {
  font-family: helvetica, arial, sans-serif;
  font-size:10pt;
}
h2 {
  font-size:11pt;
  font-weight:bold;
}
table { 
  margin:0px;
  padding:0px;
  border-spacing:0px;
  border-collapse:collapse;
}

td { 
  vertical-align:top;
  padding:0px;
}

.spaced { 
  border:1px solid #bbbbbb; 
  width:100%;
}

.darkyellow { background-color:#ffffaa; }
.lightyellow { background-color:#ffffdd; }

.spaced td { 
  padding:3px 5px;
}

.darkyellow:hover,.lightyellow:hover { 
  background-color:#ffff55;
}

a { 
  font-weight:bold;
  text-decoration:none;
  color:#016613;
}
a:hover {
  color:#002306;
}

.top { 
  position:absolute;
  right:3px; top:3px;
  text-align:right; 
  font-size:10px;
  vertical-align:middle;
  margin:0; 
  padding:0; 
  border:0; 
}

.title { 
  position:relative;
  background-color:#99ccff;  
  border:1px solid #bbbbbb;
  font-weight:bold;
  font-size:12pt;
  padding:3px;
}
.content {
  background-color:#ffffcc;
  border:1px solid #bbbbbb;
  padding:5px;
  margin:7px 0px;
}
</style>
</head>
<body>
