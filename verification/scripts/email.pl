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
use strict;
use Getopt::Long;
use testRunConfig; 
use scriptUtils;
use List::Util;

checkSetup();
setEnvConfig();
  
my $reportFile = "$shaktiHome/verification/workdir/regress_report.log";

chdir($shaktiHome);
`make regress opts="--report"`;

if (-e $reportFile) {
    my @report = ();
    open REPORT, $reportFile or die "[$scriptLog.pl] ERROR opening file $!\n";
    @report = <REPORT>;
    close REPORT;
    open HTML, ">$shaktiHome/verification/workdir/regress_report.html" or die "[$scriptLog.pl] ERROR opening file $!\n";
    print HTML "<!DOCTYPE html>\n";
    print HTML "<html>\n";
    print HTML "<head>\n";
    print HTML "<style>\n";
    print HTML "table {\n";
    print HTML "    font-family: arial, sans-serif;\n";
    print HTML "    border-collapse: collapse;\n";
    print HTML "    width: auto;\n";
    print HTML "}\n";
    print HTML "\n";
    print HTML "td, th {\n";
    print HTML "    border: 1px solid #dddddd;\n";
    print HTML "    text-align: left;\n";
    print HTML "    padding: 8px;\n";
    print HTML "}\n";
    print HTML "\n";
    print HTML "tr:nth-child(even) {\n";
    print HTML "    background-color: #dddddd;\n";
    print HTML "}\n";
    print HTML "</style>\n";
    print HTML "</head>\n";
    print HTML "<body>\n";
    print HTML "\n";

    print HTML "<h2>Regression Summary</h2>\n";
    print HTML "\n";
    print HTML "<table>\n";

    my @summary = grep /^\#\s/, @report;
    foreach my $sum (@summary) {
      print HTML "  <tr>\n";
      chomp($sum);
      my @sum = split(/:/,$sum);
      foreach my $s (@sum) {
        $s =~ s/^\s+|\s+$//g;
        print HTML "    <th>$s</th>\n";
      }
      print HTML "  </tr>\n";
    }
    print HTML "</table>\n";
    print HTML "\n";
    print HTML "\n";
    print HTML "\n";

    print HTML "<h2>Regression Report</h2>\n";
    print HTML "\n";
    print HTML "<table>\n";
    print HTML "  <tr>\n";
    print HTML "    <th>Suite</th>\n";
    print HTML "    <th>Test</th>\n";
    print HTML "    <th>Type</th>\n";
    print HTML "    <th>Result</th>\n";
    print HTML "  </tr>\n";
    
    foreach my $line (@report) {
      if ($line !~ /^#/) {
        chomp($line);
        $line =~ s/^\s+|\s+$//g;
        my @test = split(/ {1,}/, $line);
        chomp(@test);
        print HTML "  <tr>\n";
        print HTML "    <td>$test[0]</td>\n";
        print HTML "    <td>$test[1]</td>\n";
        print HTML "    <td>$test[2]</td>\n";
        print HTML "    <td>$test[3]</td>\n";
        print HTML "  </tr>\n";

      }
    }
    
    print HTML "</table>\n";
    print HTML "\n";
    print HTML "</body>\n";
    print HTML "</html>\n";
    close HTML;
    print "Mailed the regression status to: @ARGV\n";
    `cat $shaktiHome/verification/workdir/regress_report.html | mail -s \"[c-class] Smoke regress report\" -a\'Content-Type: text/html\' -a\'From: Shakti Bot <shaktibot\@gmail.com>\' @ARGV`;
}
else {
  print "ERROR: Nothing to report. please run regression\n";
  exit(1);
}
