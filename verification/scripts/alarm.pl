#!/usr/bin/perl

#-----------------------------------------------------------
#-----------------------------------------------------------

use strict;
use Getopt::Long;

my $pid;

eval {
    local $SIG{ALARM} = sub { die "TIMEOUT" };
    alarm 2;
    $pid = open my $fh, "sleep 30|";
    alarm 0;
}

if ($@ eq "TIMEOUT") {
    print "kill $pid\n";
    kill 1, $pid;
} elsif ($@) {
    die $@;
}
