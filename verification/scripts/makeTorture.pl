# ------------------------------------------------------------------------------------------------- 
# Copyright (c) 2018, IIT Madras All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
# 
# - Redistributions of source code must retain the below copyright notice, this list of conditions
#   and the following disclaimer.  
# - Redistributions in binary form must reproduce the above copyright notice, this list of 
#   conditions and the following disclaimer in the documentation and/or other materials provided 
#   with the distribution.  
# - Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
#   promote products derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
# WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# -------------------------------------------------------------------------------------------------
# Author: Lavanya J
# Email id: lavanya.jagan@gmail.com
# -------------------------------------------------------------------------------------------------
#
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
my $configPath = "$shaktiHome/verification/tests/random/riscv-torture/configs";
my $testPath =  "$shaktiHome/verification/tests/random/riscv-torture/generated_tests";

doPrint("Test generation: riscv-torture\n");

# Parse options
GetOptions(
          qw(test_config=s)   => \my $test_config,
          qw(test_count=s)    => \my $test_count,
          qw(submit)          => \my $submit,
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
    $testCount = 1;
  }

}

if ($submit) {
  $testCount = 1;
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
    if ($submit) {
      my $name = join("", $testConfig, "_", $date[0], "_test$i");
      systemCmd("java -Xmx1G -Xss8M -XX:MaxPermSize=128M -jar sbt-launch.jar \'generator/run --config $configFile --output $testName\'");
      system("CONFIG_LOG=1 perl -I $shaktiHome/verification/scripts $shaktiHome/verification/scripts/makeTest.pl --test=$name --suite=random/riscv-torture/generated_tests/$testConfig --type=p --sim=$simulator");
    }
    else {
      systemCmd("setsid java -Xmx1G -Xss8M -XX:MaxPermSize=128M -jar sbt-launch.jar \'generator/run --config $configFile --output $testName\' &");
    }
  }
}
