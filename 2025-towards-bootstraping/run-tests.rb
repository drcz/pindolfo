#!/usr/bin/ruby

##############################
##  NA PRZYPALE ALBO WCALE  ##
##############################

TESTS = [
  ["tests/classics-0.pindolfo", "tests/classics-0-test.suite"],
  ["tests/classics-1.pindolfo", "tests/classics-1-test.suite"],
  ["tests/t23.pindolfo", "tests/t23-test.suite"],
  ["tests/substring.pindolfo", "tests/substring-test.suite"],
  ["tests/nat-arithm.pindolfo", "tests/nat-arithm-test.suite"],
  ["tests/ack.pindolfo", "tests/ack-test.suite"],
  ["tests/merge.pindolfo", "tests/merge-test.suite"],
  ["tests/lists.pindolfo", "tests/lists-test.suite"],
  ["tests/tableaux-prefix.pindolfo", "tests/tableaux-prefix-test.suite"],
  ["tests/tableaux-infix.pindolfo", "tests/tableaux-infix-test.suite"],
  ["tests/bnd+reqs.pindolfo", "tests/bnd+reqs-test.suite"],
  ["tests/drc.pindolfo", "tests/drc-test.suite"],
  ["tests/lisp0.pindolfo", "tests/lisp0-test.suite"],
  ["tests/PinP.pindolfo", "tests/PinP-test.suite"],
  ["tests/lisp2drc.pindolfo", "tests/lisp2drc-test.suite"],

  ["tests/pindolfina.pindolfo", "tests/pindolfina-test.suite"],
  ["tests/tmp-rewriter.pindolfo", "tests/tmp-rewriter-test.suite"]
]

DELETE_COMPILATES_AFTERWARDS = true

#####################################################################
def ok_compilate(fn)
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
  msg = ok_compilate(compilate_fname)
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
  `./compile.sh #{pndlf_fname} #{compilate_fname} 2> /dev/null` # he_he
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
