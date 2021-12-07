# shamelessly copied from http://code.google.com/p/irssi-libnotify/
# notify via tcp port and other improvements by Filippo Giunchedi <filippo@debian.org>

use strict;
use Irssi;
use HTML::Entities;
use vars qw($VERSION %IRSSI);

$VERSION = "0.01";

%IRSSI = (
    authors     => 'Luke Macken, Paul W. Frields',
    contact     => 'lewk@csh.rit.edu, stickster@gmail.com',
    name        => 'rnotify',
    description => 'Use libnotify to alert user to hilighted messages',
    license     => 'GNU General Public License',
    url         => 'http://lewk.org/log/code/irssi-notify',
);

Irssi::settings_add_str('misc', $IRSSI{'name'} . '_port', '12000');
Irssi::settings_add_bool('misc', $IRSSI{'name'} . '_if_away', 0);

sub is_port_owner {
    my ($port, $uid) = @_;
    my $wanted = sprintf("0100007F:%04X", $port);

    # XXX linux-specific
    open HANDLE, "< /proc/net/tcp" || return 0;
    while(<HANDLE>){
        #   sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
        my @splitted = split /\s+/;
        my $local = $splitted[2];
        my $remote = $splitted[3];
        my $uid = $splitted[8];

        return 1 if $local eq $wanted and $uid == $<;
    }   
    close HANDLE;
    return 0;
}

sub notify {
    my ($server, $summary, $message) = @_;

    $message = HTML::Entities::encode($message);
    $summary = HTML::Entities::encode($summary);

    # echo \ escaping
    $message =~ s/\\/\\\\/g;
    $summary =~ s/\\/\\\\/g;

    my $port = Irssi::settings_get_str($IRSSI{'name'} . '_port');

    return if ! is_port_owner($port, $<);
    
    # check for being away in every server?
    return if $server->{usermode_away} &&
              (Irssi::settings_get_bool($IRSSI{'name'} . '_if_away') == 0);

    # XXX test for other means of doing TCP
    #print("echo '$summary\n$message\n\n' | /bin/nc 127.0.0.1 $port");
    system("echo '$summary\n$message\n\n' | /bin/nc 127.0.0.1 $port &");

    #my $pid = open(FH, "|-");
    #if( $pid ){
    #    print FH "$summary\n$message\n\n";
    #    close(FH) || warn "exited $?";
    #}else{
    #    exec("/bin/nc 127.0.0.1 $port") || warn "can't exec $!";
    #}
}
 
sub print_text_notify {
    my ($dest, $text, $stripped) = @_;
    my $server = $dest->{server};

    return if (!$server || !($dest->{level} & MSGLEVEL_HILIGHT));
    my $sender = $stripped;
    $sender =~ s/^\<.([^\>]+)\>.+/\1/ ;
    $stripped =~ s/^\<.[^\>]+\>.// ;
    my $summary = "Message on $dest->{target}";
    notify($server, $summary, $stripped);
}

sub message_private_notify {
    my ($server, $msg, $nick, $address) = @_;

    return if (!$server);
    notify($server, "Private message from ".$nick, $msg);
}

sub dcc_request_notify {
    my ($dcc, $sendaddr) = @_;
    my $server = $dcc->{server};

    return if (!$dcc);
    notify($server, "DCC ".$dcc->{type}." request", $dcc->{nick});
}

Irssi::signal_add('print text', 'print_text_notify');
Irssi::signal_add('message private', 'message_private_notify');
Irssi::signal_add('dcc request', 'dcc_request_notify');

# vim: et

