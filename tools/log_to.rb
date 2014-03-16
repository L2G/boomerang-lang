#!/usr/bin/env ruby

outfile = ARGV[0]
File.open(outfile, 'w+') do |f|
  $stdin.each do |line|
    f.puts line
    $stdout.print '.'
    $stdout.flush
  end
end
$stdout.print "\n"
