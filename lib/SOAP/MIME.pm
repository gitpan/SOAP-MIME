# SOAP::MIME Perl Module
# This is a patch of sorts to SOAP::Lite <http://soaplite.com/>
#
# Author: Byrne Reese <byrne@majordojo.com>
#
# TO DO:
# * SOAP::Lite is incredibly inefficient in its use of memory. It passed
#   most objects around by value, which inadvertently create copies of
#   everything. SOAP::Lite needs to pass around references, especially
#   when passing around things as potentially big as attachments.
package SOAP::MIME;

$VERSION=0.55-3;

BEGIN {

  # This is being added by Byrne to support attachments. I need to add
  # an array property to the SOAP::SOM object as a placeholder for 
  # decoded attachments. I add this getter/setting for attachments, and
  # I will then call this in SOAP::Deserializer to populate the SOM object
  # with the decoded attachments (MIME::Entity's)
  # DEPRECATED:
  #   please use SOAP::SOM::parts
  sub SOAP::SOM::attachments {
    my $self = shift;
    @_ ? ($self->{_attachments} = shift, return $self) : return $self->{_attachments};
  }

  # This exposes any MIME::Entities that were parsed out of a MIME formatted
  # response.
  sub SOAP::SOM::parts {
    my $self = shift;
    @_ ? ($self->{_parts} = shift, return $self) : return $self->{_parts};
  }

  sub SOAP::Deserializer::deserialize {
    SOAP::Trace::trace('()');
    my $self = shift->new;

    # initialize 
    $self->hrefs({});
    $self->ids({});

    # TBD: find better way to signal parsing errors
    # This is returning a parsed body, however, if the message was mime
    # formatted, then the self->ids hash should be populated with mime parts

    # I think I am going to chunk this - and have the decode subroutine
    # parse the MIME::Entity objects into SOAP::SOM parts.
    my $parsed = $self->decode($_[0]); # TBD: die on possible errors in Parser?

    $self->decode_object($parsed);
    my $som = SOAP::SOM->new($parsed);

    if ($self->mimeparser->{'_parts'}) {
      print "*********************ATTACHMENTS FOUND**************************\n";
      $som->parts($self->mimeparser->{'_parts'});
    }

    # if there are some IDs (from mime processing), then process others
    # otherwise delay till we first meet IDs
    if (keys %{$self->ids()}) {
      $self->traverse_ids($parsed);
    } else {
      $self->ids($parsed);
    }
    return $som;
  }

  sub SOAP::MIMEParser::DESTROY {
    my $self = shift;
    $self->{_parts} = undef;
    $self->{_attachments} = undef;
  }

  sub SOAP::MIMEParser::decode {
    my $self = shift;

    my $entity = eval { $self->parse_data(shift) }
      or die "Something wrong with MIME message: @{[$@ || $self->last_error]}\n";

    $self->{_parts} = \$entity->parts;

    my @result = ();
    if ($entity->head->mime_type eq 'multipart/form-data') {
      @result = $self->decode_form_data($entity);
    } elsif ($entity->head->mime_type eq 'multipart/related') {
      @result = $self->decode_related($entity);
    } elsif ($entity->head->mime_type eq 'text/xml') {
      @result = ();
    } else {
      die "Can't handle MIME messsage with specified type (@{[$entity->head->mime_type]})\n";
    }
    @result ? @result 
      : $entity->bodyhandle->as_string ? [undef, '', undef, $entity->bodyhandle->as_string]
	: die "No content in MIME message\n";
  }

  # This exposes an accessor for setting and getting the MIME::Entities
  # that will be attached to the SOAP Envelope.
  sub SOAP::Lite::parts {
    my $self = shift;
    @_ ? ($self->{_parts} = \@_, return $self) : return $self->{_parts};
  }

  # This overrides the SOAP::Lite call subroutine used internally by
  # SOAP::Lite to marshall the request and make a call to the SOAP::Transport
  # module.
  sub SOAP::Lite::call {
    my $self = shift;

    return $self->{_call} unless @_;

    my $serializer = $self->serializer;

    die "Transport is not specified (using proxy() method or service description)\n"
      unless defined $self->proxy && UNIVERSAL::isa($self->proxy => 'SOAP::Client');

    my $top;
    my $headers=new HTTP::Headers();
    if ($self->parts) {
      $top = MIME::Entity->build(
				 'Type' => "multipart/related"
				);

      $top->attach(
		   'Type'             => 'text/xml',
		   'Content-Location' => '/main_envelope',
		   'Content-ID'       => '<main_envelope>',
		   'Data'             => $serializer->envelope(method => shift, @_),
		  );

      foreach $a (@{$self->parts}) {
	$top->add_part($a);
      }

      my ($boundary) = $top->head->multipart_boundary;
      $headers->header('Content-Type' => 'multipart/related; type="text/xml"; start="<main_envelope>"; boundary="'.$boundary.'"');
    }

    $serializer->on_nonserialized($self->on_nonserialized);
    my $response = $self->transport->send_receive(
      endpoint => $self->endpoint,
      action   => scalar($self->on_action->($serializer->uriformethod($_[0]))),
                  # leave only parameters so we can later update them if required
      envelope => ($self->parts() ? $top->stringify_body : $serializer->envelope(method => shift, @_)),
      encoding => $serializer->encoding,
      headers  => $headers,
    );

    $self->parts(undef); # I need to reset this.

    return $response if $self->outputxml;

    # deserialize and store result
    my $result = $self->{_call} = eval { $self->deserializer->deserialize($response) } if $response;

    if (!$self->transport->is_success || # transport fault
        $@ ||                            # not deserializible
        # fault message even if transport OK 
        # or no transport error (for example, fo TCP, POP3, IO implementations)
        UNIVERSAL::isa($result => 'SOAP::SOM') && $result->fault) {
      return $self->{_call} = ($self->on_fault->($self, $@ ? $@ . ($response || '') : $result) || $result);
    }

    return unless $response; # nothing to do for one-ways

    # little bit tricky part that binds in/out parameters
    if (UNIVERSAL::isa($result => 'SOAP::SOM') && 
        ($result->paramsout || $result->headers) && 
        $serializer->signature) {
      my $num = 0;
      my %signatures = map {$_ => $num++} @{$serializer->signature};
      for ($result->dataof(SOAP::SOM::paramsout), $result->dataof(SOAP::SOM::headers)) {
        my $signature = join $;, $_->name, $_->type || '';
        if (exists $signatures{$signature}) {
          my $param = $signatures{$signature};
          my($value) = $_->value; # take first value
          UNIVERSAL::isa($_[$param] => 'SOAP::Data') ? $_[$param]->SOAP::Data::value($value) :
          UNIVERSAL::isa($_[$param] => 'ARRAY')      ? (@{$_[$param]} = @$value) :
          UNIVERSAL::isa($_[$param] => 'HASH')       ? (%{$_[$param]} = %$value) :
          UNIVERSAL::isa($_[$param] => 'SCALAR')     ? (${$_[$param]} = $$value) :
                                                       ($_[$param] = $value)
        }
      }
    }
    return $result;
  } # end of call()

} # end of BEGIN block

#############################################################################
# Many hacks and additional functionality tweaked to make this work. All code
# changes made by me to get this to work are identified by a 'MIME:' comment.
#
package SOAP::Transport::HTTP::Client;
use Compress::Zlib;
use SOAP::Transport::HTTP;

sub send_receive {
  my($self, %parameters) = @_;
  my($envelope, $endpoint, $action, $encoding, $headers) =
    @parameters{qw(envelope endpoint action encoding headers)};
  # MIME:                                            ^^^^^^^
  # MIME: I modified this because the transport layer needs access to the
  #       HTTP headers to properly set the content-type
  $endpoint ||= $self->endpoint;

  my $method='POST';
  $COMPRESS='gzip';
  my $resp;

  $self->options->{is_compress}
    ||= exists $self->options->{compress_threshold}
      && eval { require Compress::Zlib };

 COMPRESS: {

    my $compressed
      = !exists $SOAP::Transport::HTTP::Client::nocompress{$endpoint} &&
	$self->options->{is_compress} &&
	  ($self->options->{compress_threshold} || 0) < length $envelope;
    $envelope = Compress::Zlib::memGzip($envelope) if $compressed;

    while (1) {

      # check cache for redirect
      $endpoint = $SOAP::Transport::HTTP::Client::redirect{$endpoint}
	if exists $SOAP::Transport::HTTP::Client::redirect{$endpoint};
      # check cache for M-POST
      $method = 'M-POST'
	if exists $SOAP::Transport::HTTP::Client::mpost{$endpoint};

      my $req =
	HTTP::Request->new($method => $endpoint,
			   (defined $headers ? $headers : $HTTP::Headers->new),
      # MIME:              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      # MIME: This is done so that the HTTP Headers instance is properly
      #       created.
			   $envelope);
      $req->protocol('HTTP/1.1');

      $req->proxy_authorization_basic($ENV{'HTTP_proxy_user'},
				      $ENV{'HTTP_proxy_pass'})
	if ($ENV{'HTTP_proxy_user'} && $ENV{'HTTP_proxy_pass'});
      # by Murray Nesbitt

      if ($method eq 'M-POST') {
	my $prefix = sprintf '%04d', int(rand(1000));
	$req->header(Man => qq!"$SOAP::Constants::NS_ENV"; ns=$prefix!);
	$req->header("$prefix-SOAPAction" => $action) if defined $action;
      } else {
	$req->header(SOAPAction => $action) if defined $action;
      }

      # allow compress if present and let server know we could handle it
      $req->header(Accept => ['text/xml', 'multipart/*']);

      $req->header('Accept-Encoding' => 
		   [$SOAP::Transport::HTTP::Client::COMPRESS])
	if $self->options->{is_compress};
      $req->content_encoding($SOAP::Transport::HTTP::Client::COMPRESS)
	if $compressed;

      if(!$req->content_type){
	$req->content_type(join '; ',
			   'text/xml',
			   !$SOAP::Constants::DO_NOT_USE_CHARSET && $encoding ?
			   'charset=' . lc($encoding) : ());
      }elsif (!$SOAP::Constants::DO_NOT_USE_CHARSET && $encoding ){
	my $tmpType=$req->headers->header('Content-type');
	# MIME:     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	# MIME: This was changed from $req->content_type which was a bug,
	#       because it does not properly maintain the entire content-type
	#       header.
	$req->content_type($tmpType.'; charset=' . lc($encoding));
      }

      $req->content_length(length($envelope));
      SOAP::Trace::transport($req);
      SOAP::Trace::debug($req->as_string);

      $self->SUPER::env_proxy if $ENV{'HTTP_proxy'};

      $resp = $self->SUPER::request($req);

      SOAP::Trace::transport($resp);
      SOAP::Trace::debug($resp->as_string);

      # 100 OK, continue to read?
      if (($resp->code == 510 || $resp->code == 501) && $method ne 'M-POST') {
	$SOAP::Transport::HTTP::Client::mpost{$endpoint} = 1;
      } elsif ($resp->code == 415 && $compressed) { 
	# 415 Unsupported Media Type
	$SOAP::Transport::HTTP::Client::nocompress{$endpoint} = 1;
	$envelope = Compress::Zlib::memGunzip($envelope);
	redo COMPRESS; # try again without compression
      } else {
	last;
      }
    }
  }

  $SOAP::Transport::HTTP::Client::redirect{$endpoint} = $resp->request->url
    if $resp->previous && $resp->previous->is_redirect;

  $self->code($resp->code);
  $self->message($resp->message);
  $self->is_success($resp->is_success);
  $self->status($resp->status_line);

  my $content =
    ($resp->content_encoding || '') 
      =~ /\b$SOAP::Transport::HTTP::Client::COMPRESS\b/o &&
	$self->options->{is_compress} ? 
	  Compress::Zlib::memGunzip($resp->content)
	      : ($resp->content_encoding || '') =~ /\S/
		? die "Can't understand returned Content-Encoding (@{[$resp->content_encoding]})\n"
		  : $resp->content;
  $resp->content_type =~ m!^multipart/!
    ? join("\n", $resp->headers_as_string, $content) : $content;
}
##############################################################################
##############################################################################

1;

__END__

=head1 NAME

SOAP::MIME - Patch to SOAP::Lite to add attachment support. This module allows
Perl clients to both compose messages with attachments, and to parse messages
with attachments.

Currently the module does not support server side parsing of attachments... at
least it has never been tested.

=head1 SYNOPSIS

SOAP::Lite (http://www.soaplite.com/) is a SOAP Toolkit that
allows users to create SOAP clients and services. As of
July 15, 2002, MIME support in SOAP::Lite was minimal. It could
parse MIME formatted messages, but the data contained in those
attachments was "lost."

This Perl module, patches SOAP::Lite so that users can not only send MIME
formatted messages, but also gain access to those MIME attachments that are
returned in a response.

=head1 TODO/ChangeLog

6/12/2002 - Need to add ability to compose and send attachments.
            FIXED on 7/15/2002
7/15/2002 - Ability to process attachments on the server side has not yet
            been tested.
7/26/2002 - Reworked the parsing of the response to return an array
            of MIME::Entity objects which enables to user to more fully
            utilize the functionality contained within that module

=head1 REFERENCE

=over 8

=item B<SOAP::SOM::attachments()>

DEPRECATED - please use SOAP::SOM::parts instead which stores an
array of MIME::Entity objects

Used to retrieve attachments returned in a response. The
subroutine attachments() returns a hash containing the attachments
parsed out of a message. The keys to the returned hash is the
content-id of the associated attachment. The value of the hash is
an array containing the content-type, the ???, and the content of
the attachment itself:

@(<content-type>,%HASH,<content>)

=item B<SOAP::SOM::parts()>

Used to retrieve MIME parts returned in a response. The
subroutine parts() returns a I<reference> to an array of MIME::Entity
objects parsed out of a message.

=item B<SOAP::Lite::parts(ARRAY)>

Used to specify an array of MIME::Entities. These entities will be
attached to the SOAP message.

=back

=head1 EXAMPLES

=head2 Retrieving an Attachment

  use SOAP::Lite;
  use SOAP::MIME;

  my $soap = SOAP::Lite
    ->readable(1)
      ->uri($NS)
	->proxy($HOST);
  my $som = $soap->foo();

  foreach my $part (${$som->parts}) {
    print $part->stringify;
  }

=head2 Sending an Attachment

  use SOAP::Lite;
  use SOAP::MIME;
  use MIME::Entity;

  my $ent = build MIME::Entity
    Type        => "image/gif",
    Encoding    => "base64",
    Path        => "somefile.gif",
    Filename    => "saveme.gif",
    Disposition => "attachment";

  my $som = SOAP::Lite
    ->readable(1)
    ->uri($SOME_NAMESPACE)
    ->parts([ $ent ])
    ->proxy($SOME_HOST)
    ->some_method(SOAP::Data->name("foo" => "bar"));

=head1 SEE ALSO

L<SOAP::Lite>, L<MIME::Entity>
