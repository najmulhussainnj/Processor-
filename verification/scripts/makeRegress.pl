#!/usr/bin/perl

#-----------------------------------------------------------
# makeRegress.pl
# Generates test related files
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use testRunConfig; 
use scriptUtils;

checkSetup();
setEnvConfig();

my $simulator = getConfig("CONFIG_SIM");

doPrint("Regression run ------------\n");

# Parse options
GetOptions(
          qw(submit)   => \my $submit_tests,
          qw(generate)   => \my $generate_tests,
          qw(list=s)   => \my $test_list,
          qw(suite=s)  => \my $test_suite,
          qw(filter=s)  => \my $test_filter,
          qw(test_count=s)    => \my $test_count,
          qw(help)     => \my $help,
          qw(clean)    => \my $clean
);

my $submit;
my $report;
my $testSuite;
my $testCount;
my $generate;
my $testList;
my $filter;

# test submission if not given, we simply report
if ($submit_tests) {
  $submit = 1;
}
else {
  $submit = 0;
}
if (!$test_list) {
  $testList = "$shaktiHome/verification/scripts/tests.list";
}
else {
  $testList = "$shaktiHome/verification/scripts/$test_list";
}

if ($generate_tests) {
  $generate = 1;
}
else {
  $generate = 0;
}

if (!$test_filter) {
  $filter = "";
}
else {
  $filter = $test_filter;
}

# Test suite
if ($test_suite) {
  $testSuite = $test_suite;
}
else {
  $testSuite = "all"; 
}

# Test count
if ($test_count) {
  $testCount = $test_count;
}
else {
  my $count = $ENV{'CONFIG_GEN_COUNT'};
  if ($count) {
    $testCount = $count;
  }
  else {
    $testCount = 3;
  }
}

# Prints command line usage of script
if ($help) {
  printHelp();
  exit(0);
}

# Clean script generated outputs
if ($clean) {
  doClean();
  exit(0);
}

my $MaxCount = 0;
my $count = 0;
if ($generate) {
  systemCmd("rm -rf $shaktiHome/verification/tests/random/riscv-torture/generated_tests/*");
  systemCmd("rm -rf $shaktiHome/verification/tests/random/aapg/generated_tests/*");
  if ($testSuite =~ /^all$/ || $testSuite=~ /^aapg$/) {
    systemCmd("perl -pi -e 's/numberOfTests=.*/numberOfTests=$testCount/' $shaktiHome/verification/tests/random/aapg/configs/*.py");
    my @configs = `ls $shaktiHome/verification/tests/random/aapg/configs/*.py`;
    $MaxCount = $MaxCount + scalar(@configs);
    chomp(@configs);
    foreach my $config (@configs) {
      systemCmd("cp $config $shaktiHome/verification/tools/AAPG/config.py");
      chdir("$shaktiHome/verification/tools/AAPG");
      $config = `basename $config .py`; chomp($config);
      systemCmd("nohup ./make.py gen_only $config &");
    }
  }
  chdir("$shaktiHome/verification/scripts");
  if ($testSuite =~ /^all$/ || $testSuite=~ /^riscv-torture$/) {
    my @configs = `ls $shaktiHome/verification/tests/random/riscv-torture/configs/*.config`;
    $MaxCount = $MaxCount + scalar(@configs);
    chomp(@configs);
    foreach my $config (@configs) {
      $config = `basename $config .config`; chomp($config);
      systemCmd("perl makeTorture.pl --test_config=$config --test_count=$testCount");
    }
  }
  $MaxCount = $MaxCount * $testCount;
  if ($testSuite =~ /^all$/) {
    sleep(5);
    while ($count != $MaxCount) {
      $count = `find $shaktiHome/verification/tests/random/ -name "*.S" | wc -l`;
      chomp($count);
    }
  }
  doPrint("test count = $count\n");
  open TESTLIST, "$shaktiHome/verification/scripts/riscv-tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
  my @listFile = <TESTLIST>;
  close TESTLIST;
  my @genTests = `find  $shaktiHome/verification/tests/random/ -name "*.S"`;
  chomp(@genTests);
  foreach my $test (@genTests) {
    my $file = `basename $test .S`; chomp($file);
    my $suite = `dirname $test`; chomp($suite);
    $suite = substr($suite, index($suite, "tests/"));
    push @listFile, "$file\t\t\t\t$suite\t\t\t\tp\n";
  }
  open TESTLIST, ">$shaktiHome/verification/scripts/tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
  print TESTLIST @listFile;
  close TESTLIST;
}

# Process tests.list -----------
open TESTLIST, "$testList" or die "[$scriptLog.pl] ERROR opening file $!\n";
my @listFile = <TESTLIST>;
close TESTLIST;

my @testList = ();

foreach my $line (@listFile) { # remove unwanted white space
  if ($line !~ /^\s*\#/) {
    if ($line !~ /^\s*$/) {
      my @line = split(" ", $line);
      chomp(@line);
      $line = join(" ", @line);
      push @testList, $line;
    }
  }
}

if ($testSuite !~ /^all$/) {
  @testList = grep /$testSuite/, @testList;
}

# run the tests 
if ($submit) {
  foreach my $line (@testList) {
    my @line = split(" ", $line);
    my $test = $line[0];
    my $tSuite = $line[1];
    my $pv = $line[2];
    unless (-e $workdir or mkdir -p $workdir/$tSuite) {
      die "ERROR: Unable to create workdir!\n";
    }
    #system("nohup perl $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv&");
    #system("perl $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv --sim=$simulator");
    system("nohup perl $shaktiHome/verification/scripts/makeTest.pl --test=$test --suite=$tSuite --type=$pv --sim=$simulator &");
    print "$test\t\t\t$tSuite\t\t\t$pv\t\t\tRunning\n";
  }
}
else {
  my @regress_report = ();
  foreach my $line (@testList) {
    my @line = split(" ", $line);
    my $test = $line[0];
    my $tSuite = $line[1];
    my $pv = $line[2];
    my $pass = "$workdir/$tSuite/$pv/$test/PASSED";
    my $fail = "$workdir/$tSuite/$pv/$test/FAILED";
    my $compile_fail = "$workdir/$tSuite/$pv/$test/COMPILE_FAIL";
    my $model_fail = "$workdir/$tSuite/$pv/$test/MODEL_FAIL";
    my $rtl_fail = "$workdir/$tSuite/$pv/$test/RTL_FAIL";
    my $result;

    if (-e $compile_fail) {
      $result = sprintf("%30s %20s %5s    COMPILE_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $model_fail) {
      $result = sprintf("%30s %20s %5s    MODEL_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $rtl_fail) {
      $result = sprintf("%30s %20s %5s    RTL_FAIL\n", $tSuite, $test, $pv);
    }
    elsif (-e $fail) {
      $result = sprintf("%30s %20s %5s    FAILED\n", $tSuite, $test, $pv);
    }
    elsif (-e $pass) {
      $result = sprintf("%30s %20s %5s    PASSED\n", $tSuite, $test, $pv);
    }
    else {
      $result = sprintf("%30s %20s %5s    NOT_RUN\n", $tSuite, $test, $pv);
    }
    push @regress_report, $result;
  }
  if ($filter) {
    print grep /$filter/, @regress_report;
  }
  else {
    print @regress_report;
  }
}
=cut
#-----------------------------------------------------------
# systemCmd
# Runs and displays the command line, exits on error
#-----------------------------------------------------------
#sub systemCmd {
#  my (@cmd) = @_;
#  chomp(@cmd);
#  doPrint("'$cmd[0]'\n");
#  my $ret = system("@cmd 2>> $workdir/$scriptLog.log >> $workdir/$scriptLog.log");
#  #my $ret = system("@cmd |& tee -a $pwd/$script.log");
#  if ($ret) {
#    die("[$scriptLog.pl] ERROR: While running '@cmd'\n\n");  
#  }
#}
#
##-----------------------------------------------------------
## systemCmd
## Runs and displays the command line, exits on error
##-----------------------------------------------------------
#sub systemFileCmd {
#  my (@cmd) = @_;
#  chomp(@cmd);
#  doPrint("'$cmd[0] > $cmd[1]'\n");
#  my @sysOut = `$cmd[0]`;
#  my $ret = $?;
#  if ($ret) {
#    die("[$scriptLog.pl] ERROR: Running '@cmd'\n\n");  
#  }
#  else {
#    open FILE, ">$cmd[1]";
#    print FILE @sysOut;
#    close FILE;
#  }
#  #my $ret = system("@cmd 2>> $workdir/$scriptLog.log >> $workdir/$scriptLog.log");
#  #my $ret = system("@cmd |& tee -a $pwd/$script.log");
#  #if ($ret) {
#  #  die("[$scriptLog.pl] ERROR Running: '@cmd'\n\n");  
#  #}
#}
#
##-----------------------------------------------------------
## doClean
## Deletes generated output
##------------------------------------------------------------
#sub doClean {
#  doPrint("Cleaning...\n");
#  systemCmd("rm -rf $shaktiHome/verification/workdir/*");
#  systemCmd("rm -rf $shaktiHome/verification/tests/random/riscv-torture/generated_tests/*");
#  systemCmd("rm -rf $shaktiHome/verification/tests/random/aapg/generated_tests/*");
#}
#
##-----------------------------------------------------------
## doPrint
## Prints message
##------------------------------------------------------------
#sub doPrint {
#  my @msg = @_;
#  print "[$scriptLog.pl] @msg";
#  print LOG "[$scriptLog.pl] @msg";
#}
#
##-----------------------------------------------------------
## doDebugPrint
## Prints message to help debug
##------------------------------------------------------------
#sub doDebugPrint {
#  my @msg = @_;
#  if (testRunConfig::getConfig("CONFIG_LOG")) {
#    print "[$scriptLog.pl] @msg";
#    print LOG "[$scriptLog.pl] @msg";
#  }
#}

sub getSimulator {
  my $sim = @_[0];
  if ($sim == 0) {
    return "bluespec";
  }
  elsif ($sim == 1) {
    return "ncverilog";
  }
  else {
    return "vcs";
  }
}
#-----------------------------------------------------------
# printHelp
# Displays script usage
#------------------------------------------------------------
sub printHelp {
  my $usage =<<USAGE;

Description: Generates test dump directory
Options:
  --test=TEST_NAME

USAGE

  print $usage;
}
