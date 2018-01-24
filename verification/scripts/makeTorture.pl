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

my $configPath = "$shaktiHome/verification/tests/random/riscv-torture/configs";
my $testPath =  "$shaktiHome/verification/tests/random/riscv-torture/generated_tests";

doPrint("Test generation: riscv-torture\n");

# Parse options
GetOptions(
          qw(test_config=s)   => \my $test_config,
          qw(test_count=s)    => \my $test_count,
          qw(nodebug) => \my $no_debug,
          qw(help)     => \my $help,
          qw(clean)    => \my $clean
);

if (!$no_debug) {
  testRunConfig::setEnvConfig();
  
}
else {
  testRunConfig::setConfigValue("CONFIG_LOG",1);
}

my $testConfig;
my $testCount;

# Test suite
if ($test_config) {
  $testConfig = $test_config;
}
else {
  $testConfig = "all"; 
}

# Test count
if ($test_count) {
  $testCount = $test_count;
}
else {
  my $count = $ENV{'CONFIG_TORTURE_COUNT'};
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

## Process tests.list -----------
open TESTLIST, "$shaktiHome/verification/scripts/tests.list" or die "[$scriptLog.pl] ERROR opening file $!\n";
my @listFile = <TESTLIST>;
close TESTLIST;

my @testList = ();

if ($testConfig =~ /^all$/) {

}
else {
  my $testGenDir = "$testPath/$testConfig";
  my $configFile = "$configPath/$testConfig\.config";
  systemCmd("mkdir -p $testGenDir");
  #systemCmd("cp $configPath/$testConfig\.config $homePath/riscv-torture/config/default.config");
  chdir("$shaktiHome/verification/tools/riscv-torture");
  doDebugPrint("cd $shaktiHome/verification/tools/riscv-torture\n");
  systemCmd("rm -rf output/*");
  systemCmd("ln -s $shaktiHome/verification/tests/random/riscv-torture/generated_tests output/generated_tests");
  for (my $i=0; $i < $testCount; $i++) {
    my @date = `date +%d%m%Y%s`; chomp(@date);
    my $testName = join("", "generated_tests/$testConfig/",$testConfig,"_", $date[0], "_test$i");
    systemCmd("setsid java -Xmx1G -Xss8M -XX:MaxPermSize=128M -jar sbt-launch.jar \'generator/run --config $configFile --output $testName\' &");
  }
}

##-----------------------------------------------------------
## systemCmd
## Runs and displays the command line, exits on error
##-----------------------------------------------------------
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
#
#
##-----------------------------------------------------------
## printHelp
## Displays script usage
##------------------------------------------------------------
#sub printHelp {
#  my $usage =<<USAGE;
#
#Description: Generates test dump directory
#Options:
#  --test=TEST_NAME
#
#USAGE
#
#  print $usage;
#}
#
