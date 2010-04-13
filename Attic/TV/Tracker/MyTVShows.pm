# http://www.mytvshows.org/
# http://blog.mytvshows.org/kind-of-an-api/

package TV::Tracker::MyTVShows;
use base qw(TV::Tracker);

use autodie;
use List::MoreUtils ();
use Pearl;


sub _download {
    my ($self, $url) = @ARG;
    return $self->SUPER::_download("http://www.mytvshows.org/$url");
}


sub get_status {
    my ($self, $show, $season, $episode) = @ARG;
    my $key = $self->{key};
    my $status = $self->_download("api/get_status/$key/$show/$season/$episode");
    
    if ($status =~ m/wrong.+?key/i) {
        die "Invalid API key.\n";
    }
    elsif ($status =~ m/not\s+found/i) {
        return 'unseen';
    }
    else {
        return $status;
    }
}


sub list_episodes {
    my ($self, $show, $season) = @ARG;
    my $episodes = $self->_download("show/$show/$season");
    my @ids = ($episodes =~ m{span\s+class="nr">([^<]+)}gi);
    my @names = ($episodes =~ m{span\s+class="title">([^<]*)}gi);
    
    return List::MoreUtils::mesh(@ids, @names);
}


sub list_seasons {
    my ($self, $show) = @ARG;
    my $seasons = $self->_download("show/$show");
    
    ($seasons) = ($seasons =~ m{"seasons_list">.+?</ul}gis);
    return $seasons =~ m{href="/show/\Q$show\E/([^/"]+)[^>]+>([^<]+)}gi;
}


sub list_shows {
    my ($self) = @ARG;
    my $shows = $self->_download("user/$self->{name}");
    
    die "Invalid user name.\n" if $shows =~ m/page\s+not\s+found/is;
    return $shows =~ m{href="/show/([^/"]+)[^>]+>([^<]+)}gi;
}


sub new {
    my $class = shift @ARG;
    die "No user name and API key specified.\n" if @ARG != 2;
    
    my ($name, $key) = @ARG;
    return instantiate($class, name => $name, key => $key);
}


1;
