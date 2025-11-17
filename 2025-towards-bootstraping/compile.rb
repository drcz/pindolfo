#!/usr/bin/ruby

#######################
##  _ZAWSZE_ Z BUTA  ##
#######################

if ARGV.length != 2
  puts "usage: ./compile.rb <source pindolfo file> <target scm file>"
else
  source_fname = ARGV[0]
  tmp_fname = ARGV[0]+".tmp"
  target_fname = ARGV[1]

  print "desugaring phase..."
  src_light = `#{__dir__}/desugar < #{source_fname}`
  puts "ok" # TODO checks?

  print "preprocessing phase..."
  preprocess_input = "(unLETized #{src_light})"
  File.open(tmp_fname, "w") { |file| file.write(preprocess_input) }
  puts "ok" # TODO checks...

  print "aaand compilation proper..."
  o = `cat #{tmp_fname} | guile #{__dir__}/preprocess.scm  | guile #{__dir__}/pindolfo2scm-1.scm > #{target_fname}`
  `rm #{tmp_fname}`
  if o.length < 1
    puts "ok!\nAuf wiedersehen!"
  else
    puts "oww snap... error: #{o}"
  end
end
