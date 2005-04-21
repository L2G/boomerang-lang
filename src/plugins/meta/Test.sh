#!/bin/sh
#
# Test.sh
#
# Your plugin must define the functions updown_test() and sync_test() in this
# file.  You may also define the function custom_test().
#
# The variable $HARMONY is available, and contains the full path to the harmony
# executable.
#
# The test is considered to succeed if and only if the function returns true.
#
# Example:
#
function updown_test() {
#  $HARMONY "updown camino-bookmarks.xml(camino) as [bookmarks]"
  true
}

function sync_test() {
#  $HARMONY "sync camino-bookmarks.xml(camino) camino-bookmarks2.xml(camino) as [camino]"
  true
}
#
#function custom_test() {
#   do_some_stuff;
#   more stuff
#}
