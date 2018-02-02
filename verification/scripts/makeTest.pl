#!/usr/bin/perl

#-----------------------------------------------------------
# makeTest.pl
# Generates test related files
#-----------------------------------------------------------

use strict;
use Getopt::Long;
use POSIX ":sys_wait_h";
use testRunConfig;
use scriptUtils;

checkSetup();

# Parse options
GetOptions(
          qw(test=s)  => \my $test_name,
          qw(suite=s) => \my $test_suite,
          qw(sim=s)   => \my $test_sim,
          qw(type=s)  => \my $test_type,
          qw(nodebug) => \my $no_debug,
          qw(notrace) => \my $no_trace,
          qw(help)    => \my $help,
          qw(clean)   => \my $clean
);

if (!$no_debug) {
  testRunConfig::setEnvConfig();
  
}
else {
  testRunConfig::setConfigValue("CONFIG_LOG",0);
}

#my $pwd = $ENV{'PWD'};
#my $scriptLog = `basename $0 .pl`; chomp($scriptLog);
my $riscvIncludeDir = "$shaktiHome/verification/tests/directed/riscv-tests";
my $testPath = "$shaktiHome/verification/tests";
#my $workdir = "$testPath/workdir";
doDebugPrint("Generating Test Dump Directory ------------\n");

my $testName;
my $testSuite;
my $currentTestPath;
my $test;
my $simulator;
my $testType;
my $trace;

# Test name
if (!$test_name) {
  doPrint("ERROR: Undefined test name\n");
  exit(0);
}
else {
  $testName = $test_name;
}

# Test suite
if (!$test_suite) {
  doPrint("ERROR: Undefined test suite\n");
  exit(0);
}
else {
  $testSuite = "$testPath/$test_suite";
}

# Simulator
if (!$test_sim) {
  $simulator = "bluespec";
}
elsif ($test_sim =~ /^ncverilog$/ || $test_sim =~ /^vcs$/ || $test_sim =~ /^bluespec$/){
  $simulator = $test_sim;
}
else {
  doPrint("ERROR: Invalid simulator, --sim=[bluespec|ncverilog|vcs]\n");
  exit(0);
}

# Type
if (!$test_type) {
  $testType = "p";
}
elsif ($test_type =~ /^p$/ || $test_type =~ /^v$/) {
  $testType = $test_type;
}
else {
  doPrint("ERROR: Invalid test type, --test_type=[p|v]\n");
  exit(0);
}

# trace

if (!$no_trace) {
  $trace = 1;
}
else {
  $trace = 0;
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

systemCmd("find $testSuite -name $testName.S");
my @test = `find $testSuite -name $testName.S`; chomp(@test);
if (@test > 1) {
  doPrint("ERROR: Duplicate test names\n");
  exit(0);
}
else {
  $currentTestPath = `dirname $test[0]`; chomp($currentTestPath);
}
$test = $test[0];
doDebugPrint("Running test: $test_suite/$testName.S\n");

my $testDir = "$workdir/$test_suite/$testType/$testName";
if (-e "$testDir") {
  systemCmd("rm -rf $testDir");
}
systemCmd("mkdir -p $testDir");
chdir($testDir);
# Creating log for each test in the directory itself
closeLog();
openLog("$testDir/$testName.log");
#chdir("$workdir/$test_suite/$testName");
# Compiling the test
if ($testType =~ /^v$/) {
  systemCmd("riscv64-unknown-elf-gcc -march=rv64g -mabi=lp64 -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -DENTROPY=0x9629af2 -std=gnu99 -O2 -I$riscvIncludeDir/env/v -I$riscvIncludeDir/isa/macros/scalar -T$riscvIncludeDir/env/v/link.ld $riscvIncludeDir/env/v/entry.S $riscvIncludeDir/env/v/*.c $test -o $testName.elf");
}
elsif ($testType =~ /^p$/) {
  systemCmd("riscv64-unknown-elf-gcc -march=rv64g -mabi=lp64 -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles -I$riscvIncludeDir/env/p -I$riscvIncludeDir/isa/macros/scalar -T$riscvIncludeDir/env/p/link.ld $test -o $testName.elf");
}

# Generating the disassembly
systemFileCmd("riscv64-unknown-elf-objdump --disassemble-all --disassemble-zeroes --section=.text --section=.text.startup --section=.text.init --section=.data $testName.elf", "$testName.disass");

# Generating hex
systemFileCmd("elf2hex  8 65536 $testName.elf 2147483648","code.mem");
systemFileCmd("cut -c1-8 code.mem", "code.mem.MSB");
systemFileCmd("cut -c9-16 code.mem", "code.mem.LSB");
if ($trace) {
  systemFileCmd("spike -l --isa=RV64IMAFD $testName.elf 2>&1","$testName\_spike.trace");
}
systemCmd("spike -c --isa=RV64IMAFD $testName.elf");
if ($simulator =~ /^bluespec$/) {
  systemCmd("ln -s $shaktiHome/bin/out.so    out.so");
}
systemCmd("ln -s $shaktiHome/bin/out       out");
if (!(-e "$shaktiHome/bin/boot.MSB")) {
  systemFileCmd("cut -c1-8 $shaktiHome/verification/dts/boot.hex","$shaktiHome/bin/boot.MSB");
  systemFileCmd("cut -c9-16 $shaktiHome/verification/dts/boot.hex","$shaktiHome/bin/boot.LSB");
}
systemCmd("ln -s $shaktiHome/bin/boot.MSB    boot.MSB");
systemCmd("ln -s $shaktiHome/bin/boot.LSB    boot.LSB");
#systemCmd("ln -s $shaktiHome/bin/boot.3l   boot.3l");
#systemCmd("ln -s $shaktiHome/bin/boot.2l   boot.2l");
#systemCmd("ln -s $shaktiHome/bin/boot.1l   boot.1l");
#systemCmd("ln -s $shaktiHome/bin/boot.0l   boot.0l");
#systemCmd("ln -s $shaktiHome/bin/boot.3h   boot.3h");
#systemCmd("ln -s $shaktiHome/bin/boot.2h   boot.2h");
#systemCmd("ln -s $shaktiHome/bin/boot.1h   boot.1h");
#systemCmd("ln -s $shaktiHome/bin/boot.0h   boot.0h");
if ($simulator =~ /^ncverilog$/) {
  systemCmd("ln -s $shaktiHome/bin/work work");
  systemCmd("ln -s $shaktiHome/verilog/cds.lib cds.lib");
  systemCmd("ln -s $shaktiHome/verilog/hdl.var hdl.var");
  systemCmd("ln -s $shaktiHome/verilog/include include");
}

if ($simulator =~ /^bluespec$/) {
  systemFileCmd("./out -w","log.txt");
}
else {
  systemFileCmd("./out","log.txt");
}
my @diff = `diff rtl.dump spike.dump`;
print @diff;
my $result;

if (@diff) {
  `touch FAILED`;
   $result = "$testName.S | $test_suite | FAILED";
}
else {
  `touch PASSED`;
  $result = "$testName.S  | $test_suite | PASSED";
}
systemFileCmd("sdiff -W rtl.dump spike.dump", "diff");
doDebugPrint("---------------------------------------------\n");
doPrint("testDir: $testDir\n");
doPrint("$result\n");
doDebugPrint("---------------------------------------------\n");
closeLOG();
appendLOG("$workdir/$scriptLog.log");
