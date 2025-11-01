#!/usr/bin/ruby

##############################
##  NA PRZYPALE ALBO WCALE  ##
##############################

def run_test(pndlf_fname, tests_fname)
  puts "testing #{pndlf_fname} via #{tests_fname}..."
  errors = 0
  compilate_fname = pndlf_fname.gsub(".ppf",".scm")
  #`guile pindolfo2scm.scm < #{pndlf_fname} > #{compilate_fname}`
  `cat #{tests_fname}`.split("\n").each{|t|
    inp, exp = t.split("|").map{|s| s.strip()}
    act = `echo "#{inp}" | guile #{compilate_fname}`.strip()
    if act == exp
      puts "ok!"
    else
      puts "#{compilate_fname} error, for #{inp} expected:\n#{exp}\nbut got:\n#{act}\n"
      errors += 1
    end
  }
  errors 
end

#####################################################################
TESTS = [
  ["classics-0.ppf", "classics-0-test.suite"],
  ["classics-1.ppf", "classics-1-test.suite"],
  ["t23.ppf", "t23-test.suite"],
  ["substring.ppf", "substring-test.suite"],
  ["drc.ppf", "drc-test.suite"]
]

#####################################################################
# compile them first pls
TESTS.each{|pndlf_fname, tests_fname|
  compilate_fname = pndlf_fname.gsub(".ppf",".scm")
  `guile pindolfo2scm-0.scm < #{pndlf_fname} > #{compilate_fname}`
  `echo "aaa" | guile #{compilate_fname} 2>&1 > /dev/null` # XD
}

#####################################################################
# run the thing...
puts "\n\n\nall files compiled, let's go"
errors = 0
TESTS.each{|pndlf_fname, tests_fname|
  errors += run_test(pndlf_fname, tests_fname)
}

#####################################################################
# and cleanup
TESTS.each{|pndlf_fname, tests_fname|
  compilate_fname = pndlf_fname.gsub(".ppf",".scm")
  `rm #{compilate_fname}`
}

#####################################################################
# yes.
if errors > 0
  puts "#{errors} errors ayyy"
else
  puts "ALL GOOD no errors"
end

puts "Auf wiedersehen!"
#####################################################################
