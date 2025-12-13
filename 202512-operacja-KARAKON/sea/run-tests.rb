#!/usr/bin/ruby

##################################
##  dalej na przypale c'nie...  ##
##################################

bin = ARGV[0]
tmpres = "tmptmp.tmp"

oks = 0 ; noks = 0

`ls ../tests/*.test`.split("\n").each{|tf|
  # ...a nawet większym, bo to wszyćko na konwencji oparte,
  # zmień nazwę pliku dyslektyku i będzie płacz:
  pf = tf.sub("../tests/", "../progs/").sub(".test",".karakon")
  ef = tf.sub(".test", ".expected")
  cmd = "./#{bin} #{pf} < #{tf} > #{tmpres}"
  `#{cmd}`
  df = `diff #{tmpres} #{ef}`
  `rm #{tmpres}`
  if df.length > 1
     puts "wow for program #{pf} and test #{tf} there's unexpected output, look:"
     puts df
     puts "---\nCMD: #{cmd}"
     noks += 1
  else
     puts "...ok"
     oks += 1
  end
}

if noks==0
   puts "all good no changes!"
else
   puts "out of #{oks+noks} tests #{oks} went ok (no changes) and #{noks} are weird."
end
puts "Auf wiedersehen!"

