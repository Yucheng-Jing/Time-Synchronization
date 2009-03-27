package Channel::RTP2;

use strict;
use utf8;
use warnings;

use Encode;
use English;
use LWP;


our $description = 'Daily TV guide for the RTP2 Portuguese channel.';
our $link = 'http://www.rtp.pt/tv/rtp2/includes/guia.php';
our $title = 'RTP2 TV Guide';


sub _date {
    my ($seconds, $minutes, $hours, $day, $month, $year) = gmtime;
    return sprintf '%d-%02d-%02d', ($year + 1900), ($month + 1), $day;
}


sub _download_guide {
    my ($date) = @ARG;
    my $browser = new LWP::UserAgent;
    my $response = $browser->get(_link($date), 'User-Agent' => 'Mozilla');
    
    $response->is_success
      or die 'Download failed: '.$response->status_line."\n";
    
    $response->content_type eq 'text/html'
      or die 'Invalid content type: '.$response->content_type."\n";
    
    return Encode::decode 'ISO-8859-1', $response->content;
}


sub _link {
    my ($date) = @ARG;
    $date = join '-', reverse split m/-/, $date;
    
    # Old: http://rtp2.rtp.pt/epg_output.php?dia=$date
    return "http://www.rtp.pt/tv/rtp2/includes/epg_output.php?dia=$date";
}


sub _parse_guide {
    my ($content) = @ARG;
    my @guide;
    
    my @hours = ($content =~ m/<span class="guiaHora">([^<]+)/gs);
    my @titles = ($content =~ m/<span class="guiaTitulo"><[^>]+>([^<]+)/gs);
    my @links = ($content =~ m/<span class="guiaTitulo"><a href='([^']+)'/gs);
    
    while (@titles > 0) {
        push @guide, {
            hour => shift @hours,
            title => _uncapitalize(shift @titles),
            link => shift @links,
        }
    }
    
    return @guide;
}


sub _to_html {
    my ($program) = @ARG;
    my ($hour, $link, $title) = @$program{qw(hour link title)};
    
    return "<b>$hour</b> - <a href=\"$link\">$title</a><br/>";
}


sub _uncapitalize {
    my ($text) = @ARG;
    
    $text =~ s/(\p{IsWord}+)/length($1) == 1 ? lc($1) : ucfirst(lc($1))/ge;
    return ucfirst $text;
}


sub generate {
    my ($rss) = @ARG;
    my $today = _date();
    my @guide = _parse_guide(_download_guide($today));
    
    $rss->add_item(
        title => 'Daily Guide',
        link => _link($today),
        description => join('', map {_to_html($ARG)} @guide),
        dc => {
            date => $today,
            identifier => $today,
        },
    );
}