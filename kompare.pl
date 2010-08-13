#!/usr/bin/perl -w

use strict;
use File::stat qw(stat);

exit system qw(kompare -c), do {
    my %seen;
    grep {!$seen{$_}++}                             # Remove duplicates.
    sort {stat($a)->mtime() <=> stat($b)->mtime()}  # Order by modified time.
    grep -e,                                        # Files only.
    @ARGV
};
