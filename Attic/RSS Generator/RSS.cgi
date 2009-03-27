#!/usr/bin/perl

use strict;
use utf8;
use warnings;

use CGI ':standard';
use English;
use File::Basename;
use File::Spec::Functions;
use XML::RSS;


eval { exit main(@ARGV) };
print "Content-type: text/plain; charset=UTF-8\n\n";
print "Error: $EVAL_ERROR\n";
exit 1;


sub clean_up {
    my ($channel, $rss) = @ARG;
    my %items;
    
    # Remove duplicate items by identifier.
    foreach my $item (@{$rss->{items}}) {
        $items{$item->{dc}->{identifier}} = $item;
    }
    
    # Sort items by date.
    $rss->{items} = [sort {$a->{dc}->{date} cmp $b->{dc}->{date}} values %items];
    
    # Remove old items.
    shift @{$rss->{items}} while @{$rss->{items}} > 7;
    
    $rss->save($channel->{rdf});
}


sub generate_channel {
    my ($channel) = @ARG;
    my $rss = new XML::RSS;
    
    if (-e $channel->{rdf}) {
        $rss->parsefile($channel->{rdf});
    }
    else {
        $rss->channel(
            title => $channel->{title},
            link => $channel->{link},
            description => $channel->{description},
        );
    }
    
    $channel->{generate}->($rss);
    clean_up($channel, $rss);
    
    print "Content-type: text/xml; charset=UTF-8\n\n";
    print $rss->as_string;
}


sub load_channel {
    my ($name) = @ARG;
    my $package = 'Channel';
    my $module = catfile($package, "$name.pm");
    
    if ($name eq '*') {
        return map {load_channel($ARG)} sort glob catfile($package, '*.pm');
    }
    elsif ($name =~ m/\.pm$/) {
        $module = $name;
        $name = fileparse($name, '.pm');
    }
    
    require $module;
    
    my $description = eval "\$${package}::${name}::description";
    my $generate = eval "\\&${package}::${name}::generate";
    my $link = eval "\$${package}::${name}::link";
    my $title = eval "\$${package}::${name}::title";
    
    return {
        description => $description,
        generate => $generate,
        link => $link,
        module => $module,
        name => $name,
        rdf => catfile($package, "$name.rdf"),
        title => $title,
    };
}


sub main {
    binmode STDOUT, ':utf8';
    my $name = url_param('channel');
    
    if (defined $name) {
        generate_channel(load_channel($name));
    }
    else {
        print_channels(load_channel('*'));
    }
    
    return 0;
}


sub print_channels {
    my (@channels) = @ARG;
    
    print "Content-Type: text/html; charset=UTF-8\n\n";
    print <<'...';
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>RSS Generator</title>
    <meta name="Author" content="Márcio Moniz Bandim Faustino"/>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
  </head>
  
  <body>
    <p>Channels:</p>
    <ul>
...
    
    foreach my $channel (@channels) {
        my ($name, $title, $desc) = @$channel{qw(name title description)};
        print <<"...";
      <li><a href="?channel=$name">$title</a>: $desc</li>
...
    }
    
    print <<'...';
    </ul>
  </body>
</html>
...
}
