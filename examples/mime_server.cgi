#!/usr/bin/perl
#
# SOAP::MIME test service
#
# Author: Byrne Reese <byrne@majordojo.com>
#
###################################################
# Configuration parameters
###################################################
# Default values
my $HOST          = "network.grandcentral.com";
my $PORT          = 80;
my $WSDL          = "reports.cgi.wsdl";
#
###################################################
# DO NOT EDIT ANYTHING BELOW THIS LINE
###################################################
use Pod::Html;
use Pod::Usage;
if ($ENV{'REQUEST_METHOD'} eq "GET" && $ENV{'QUERY_STRING'} eq "WSDL") {
#    print "Content-type: text/xml\n\n";
#    open WSDL,$WSDL;
#    print while (<WSDL>);
#    close WSDL;
    exit;
} elsif ($ENV{'REQUEST_METHOD'} eq "GET") {
    print "Content-type: text/plain\n\n";
    print @INC;
    pod2usage(-output => \*STDOUT, -verbose => 2);
    exit;
}

use SOAP::Transport::HTTP;
use SOAP::MIME;

SOAP::Transport::HTTP::CGI
  ->dispatch_to('SOAP_MIME_Test')
  ->handle();

BEGIN {

  package SOAP_MIME_Test;
  use strict;
  use vars qw(@ISA);
  @ISA = qw(SOAP::Server::Parameters);

  sub echo {
    my $self = shift;
    my $envelope = pop;
    foreach my $part (@{$envelope->parts}) {
      print STDERR "Attachments.cgi: attachment found! (".ref($$part).")\n";
    }
    print STDERR "envelope is of type: " . ref($envelope) . "\n";
    my $STRING = $envelope->dataof("//echo/whatToEcho")
      or die SOAP::Fault->faultcode("Client")
        ->faultstring("You must specify a string to echo")
          ->faultactor('urn:SOAP_MIME_Test#echo');

    my $ent = build MIME::Entity
	'Id'          => "<1234>",
	'Type'        => "text/xml",
	'Path'        => "/home/breese/soaplite/soapmime/module/examples/attachments/some2.xml",
	'Filename'    => "some2.xml",
	'Disposition' => "attachment";
    return SOAP::Data->name("whatToEcho" => $STRING),$ent;
  }

}
__END__

=head1 NAME

SOAP::MIME Test Service

=head1 SYNOPSIS

This service tests SOAP::MIME's ability to parse attachments.
