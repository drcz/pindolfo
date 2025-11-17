#!/usr/bin/ruby

##############################
##  NA PRZYPALE ALBO WCALE  ##
##############################

TESTS = [
  ["classics-0.pindolfo", "classics-0-test.suite"],
  ["classics-1.pindolfo", "classics-1-test.suite"],
  ["t23.pindolfo", "t23-test.suite"],
  ["substring.pindolfo", "substring-test.suite"],
  ["drc.pindolfo", "drc-test.suite"],
  ["lisp0.pindolfo", "lisp0-test.suite"],
  ["PinP.pindolfo", "PinP-test.suite"],
  ["bnd+reqs.pindolfo", "bnd+reqs-test.suite"],
]

DELETE_COMPILATES_AFTERWARDS = true

#####################################################################
def ok_compilte(fn)
  m = "ok"
  m = "file #{fn} does not exist!" if !File.exist?(fn)
  m = "file #{fn} empty!" if File.zero?(fn)
  m = "no code in #{fn}!" if (`cat #{fn} | wc -l`.strip()=="1")
  m
end

def run_test(pndlf_fname, tests_fname)
  print "* testing #{pndlf_fname} via #{tests_fname} "
  err_msgs = []
  compilate_fname = pndlf_fname.gsub(".pindolfo",".scm")
  msg = ok_compilte(compilate_fname)
  if msg != "ok"
    err_msgs << msg 
    puts msg
  else
    `cat #{tests_fname}`.split("\n").each{|t|
      inp, exp = t.split("|").map{|s| s.strip()}
      act = `echo "#{inp}" | guile #{compilate_fname} 2> /dev/null`.strip()
      if act == exp
        print "."
      else
        print "!"
        err_msgs << "#{compilate_fname} error, for #{inp}\n  expected #{exp}\n   but got #{act}"
      end
    }
    print (err_msgs.length==0 ? " ok!" : " ayy!") + "\n"
  end
  err_msgs
end
#####################################################################
puts "welcome to the cool side of the pillow."

#####################################################################
# compile them first pls
TESTS.each{|pndlf_fname, tests_fname|
  compilate_fname = pndlf_fname.gsub(".pindolfo",".scm")
  print "* compiling #{pndlf_fname}... "
  `../compile.rb #{pndlf_fname} #{compilate_fname} 2> /dev/null` # he_he
  if File.exist?(compilate_fname)
    print "ok, now guile compiles it... "
    `echo "aaa" | guile #{compilate_fname} 2>&1 > /dev/null` # XD
    puts "ok!"
  else
    puts "compile error!!!"
  end
}

#####################################################################
# run the thing...
puts "\nok let's go!"
errors = []
TESTS.each{|pndlf_fname, tests_fname|
  errors += run_test(pndlf_fname, tests_fname)
}

#####################################################################
# report
puts ""
if errors.length > 0
  puts "#{errors.length} errors:"
  errors.each{|e| puts "* #{e}"}
else
  puts "ALL GOOD no errors."
end

#####################################################################
# and cleanup?
if DELETE_COMPILATES_AFTERWARDS
  puts "(deleting compilates)"
  TESTS.each{|pndlf_fname, tests_fname|
    compilate_fname = pndlf_fname.gsub(".pindolfo",".scm")
    `rm #{compilate_fname}`
  }
end

#####################################################################
puts "\nAuf wiedersehen!"
