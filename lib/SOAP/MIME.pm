#
# Author: Byrne Reese <breese@grandcentral.com>
#
package SOAP::MIME;

$VERSION=0.55;

BEGIN {

  # This is being added by Byrne to support attachments. I need to add
  # an array property to the SOAP::SOM object as a placeholder for 
  # decoded attachments. I add this getter/setting for attachments, and
  # I will then call this in SOAP::Deserializer to populate the SOM object
  # with the decoded attachments (MIME::Entity's)
  sub SOAP::SOM::attachments {
    my $self = shift;
    @_ ? ($self->{_attachments} = shift, return $self) : return $self->{_attachments};
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
    my $parsed = $self->decode($_[0]); # TBD: die on possible errors in Parser?

    $self->decode_object($parsed);
    my $som = SOAP::SOM->new($parsed);

    # if there are some IDs (from mime processing), then process others
    # otherwise delay till we first meet IDs
    if (keys %{$self->ids()}) {
      $som->attachments($self->ids());
      $self->traverse_ids($parsed);
    } else {
      $self->ids($parsed);
    }
    return $som;
  }

}

1;

__END__

=head1 NAME

SOAP::MIME - Patch to SOAP::Lite to add attachment support. 
Currently all that is supported the retrieval of attachments
from a response.

=head1 SYNOPSIS

SOAP::Lite (http://www.soaplite.com/) is a SOAP Toolkit that
allows users to create SOAP clients and services. As of 
June 12, 2002, MIME support in SOAP::Lite was minimal. It could
parse MIME formatted messages, but the data contained in those
attachments was "lost." This Perl module, patches SOAP::Lite so
that those MIME attachments are stored and placed into
a SOAP::SOM object.

=head1 TODO

6/12/2002 - Need to add ability to compose and send attachments.

=head1 Reference

=item attachments()

Used to retrieve attachments returned in a response. The
subroutine attachments() returns a hash containing the attachments
parsed out of a message. The keys to the returned hash is the
content-id of the associated attachment. The value of the hash is
an array containing the content-type, the ???, and the content of 
the attachment itself:

@(<content-type>,%HASH,<content>)

=head1 Example

  use SOAP::Lite;
  use SOAP::MIME;

  my $soap = SOAP::Lite
    ->readable(1)
      ->uri($NS)
	->proxy($HOST);
  my $som = $soap->foo();

  foreach my $cid (keys %{$som->attachments}) {
    print "Attachment content-id: " . $cid . "\n";
    foreach my $foo (@{$som->attachments->{$cid}}) {
      print "  Array elem: $foo\n";
    }
  }
