#=======================================================================
#    ____  ____  _____              _    ____ ___   ____
#   |  _ \|  _ \|  ___|  _   _     / \  |  _ \_ _| |___ \
#   | |_) | | | | |_    (_) (_)   / _ \ | |_) | |    __) |
#   |  __/| |_| |  _|    _   _   / ___ \|  __/| |   / __/
#   |_|   |____/|_|     (_) (_) /_/   \_\_|  |___| |_____|
#
#   A Perl Module Chain to faciliate the Creation and Modification
#   of High-Quality "Portable Document Format (PDF)" Files.
#
#   Copyright 1999-2004 Alfred Reibenschuh <areibens@cpan.org>.
#
#=======================================================================
#
#   This library is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public
#   License as published by the Free Software Foundation; either
#   version 2 of the License, or (at your option) any later version.
#
#   This library is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   Lesser General Public License for more details.
#
#   You should have received a copy of the GNU Lesser General Public
#   License along with this library; if not, write to the
#   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
#   Boston, MA 02111-1307, USA.
#
#   $Id: gFont.pm,v 1.1.1.1 2005/02/17 14:51:57 fredo Exp $
#
#=======================================================================
package PDF::Maki::gFont;

BEGIN {

    use utf8;
    use Encode qw(:all);

    use vars qw( @ISA $VERSION );
    use PDF::API2::Resource::Font;
    use PDF::API2::Util;
    use PDF::API2::Basic::PDF::Utils;
    use Math::Trig;
    use Unicode::UCD 'charinfo';
    use Compress::Zlib;

    @ISA=qw(PDF::API2::Resource::Font);

    ( $VERSION ) = '$Revision: 1.1.1.1 $' =~ /Revision: (\S+)\s/; # $Date: 2005/02/17 14:51:57 $

}
no warnings qw[ deprecated recursion uninitialized ];

=head1 NAME

PDF::Maki::gFont - Module for using graphic (ie. Type3) Fonts.

B<I<*** EXPERIMENTAL VERSION ***>>

=head1 SYNOPSIS

    #
    use PDF::API2;
    #
    $pdf = PDF::API2->new;
    $sft = $pdf->gfont($data,%options);
    #

=head1 METHODS

=over 4

=cut

=item $font = PDF::Maki::gFont->new $pdf, $data, %options

Returns a gfont object.

=cut

sub new 
{
    my ($class,$pdf,$data,@opts) = @_;
    my ($self);
    my %opts=@opts;

    $class = ref $class if ref $class;
    $self = $class->SUPER::new($pdf,pdfkey());
    $pdf->new_obj($self) unless($self->is_obj($pdf));

    $self->{-pdf}=$pdf;
    $self->{' data'}=$data;

    $self->{Subtype} = PDFName('Type3');
    $self->{FirstChar} = PDFNum(0);
    $self->{LastChar} = PDFNum(255);
    $self->{FontMatrix} = PDFArray(map { PDFNum($_) } ( 0.001, 0, 0, 0.001, 0, 0 ) );

    my $procs=PDFDict();
    $self->{CharProcs} = $procs;

    $self->{Resources}=PDFDict();
    $self->{Resources}->{ProcSet}=PDFArray(map { PDFName($_) } qw[ PDF Text ImageB ImageC ImageI ]);

    $self->data->{fontbbox}||=[0,0,0,0];
    $self->data->{char}||=[ '.notdef' ];
    $self->data->{uni}||=[ 0 ];
    $self->data->{wx}||={ '.notdef'=>0 };
    
    $self->data->{ascender}||=800;
    $self->data->{capheight}||=1000;
    $self->data->{descender}||=-200;
    $self->data->{iscore}||=0;
    $self->data->{isfixedpitch}||=0;
    $self->data->{italicangle}||=0;
    $self->data->{missingwidth}||=500;
    $self->data->{underlineposition}||=-200;
    $self->data->{underlinethickness}||=50;
    $self->data->{xheight}||=500;
        
    $self->data->{e2n}=$self->data->{char};
    $self->data->{e2u}=$self->data->{uni};

    $self->data->{u2c}={};
    $self->data->{u2e}={};
    $self->data->{u2n}={};
    $self->data->{n2c}={};
    $self->data->{n2e}={};
    $self->data->{n2u}={};

    return($self);
}


=item $font = PDF::Maki::gFont->new_api $api, $data, %options

Returns a gfont object. This method is different from 'new' that
it needs an PDF::API2-object rather than a PDF::API2::PDF::File-object.

=cut

sub new_api 
{
  my ($class,$api,@opts)=@_;

  my $obj=$class->new($api->{pdf},@opts);

  $api->{pdf}->new_obj($obj) unless($obj->is_obj($api->{pdf}));

  $api->{pdf}->out_obj($api->{pages});
  return($obj);
}

sub register_glyph
{
    my ($self,$name,$enc,$uni,$wx,$xl,$yl,$xr,$yr,$code) = @_;

    $self->data->{wx}->{$name}=$wx;
    $self->data->{char}->[$enc]=$name;
    $self->data->{uni}->[$enc]=$uni;
    
    if($self->data->{fontbbox}->[0] > $xl)
    {
        $self->data->{fontbbox}->[0]=$xl;
    }
    if($self->data->{fontbbox}->[1] > $yl)
    {
        $self->data->{fontbbox}->[1]=$yl;
    }
    if($self->data->{fontbbox}->[2] < $xr)
    {
        $self->data->{fontbbox}->[2]=$xr;
    }
    if($self->data->{fontbbox}->[3] < $yr)
    {
        $self->data->{fontbbox}->[3]=$yr;
    }

    my $char=PDFDict();
    $self->{CharProcs}->{$name}=$char;
    $char->{Filter}=PDFArray(PDFName('FlateDecode'));
    
    $char->{' stream'}=compress("$wx 0 $xl $yl $xr $yr d1\n$code\n");
#    $char->{' stream'}="$wx 0 $xl $yl $xr $yr d1\n$code\n";
    $char->{' nofilt'}=1;
#    $char->{Length}=PDFNum(length($char->{' stream'}));

    $self->{-pdf}->new_obj($char);

    foreach my $n (reverse 0..255) 
    {
        $self->data->{n2c}->{$self->data->{char}->[$n] || '.notdef'}=$n unless(defined $self->data->{n2c}->{$self->data->{char}->[$n] || '.notdef'});
        $self->data->{n2e}->{$self->data->{e2n}->[$n] || '.notdef'}=$n unless(defined $self->data->{n2e}->{$self->data->{e2n}->[$n] || '.notdef'});

        $self->data->{n2u}->{$self->data->{e2n}->[$n] || '.notdef'}=$self->data->{e2u}->[$n] unless(defined $self->data->{n2u}->{$self->data->{e2n}->[$n] || '.notdef'});
        $self->data->{n2u}->{$self->data->{char}->[$n] || '.notdef'}=$self->data->{uni}->[$n] unless(defined $self->data->{n2u}->{$self->data->{char}->[$n] || '.notdef'});

        $self->data->{u2c}->{$self->data->{uni}->[$n]}=$n unless(defined $self->data->{u2c}->{$self->data->{uni}->[$n]});
        $self->data->{u2e}->{$self->data->{e2u}->[$n]}=$n unless(defined $self->data->{u2e}->{$self->data->{e2u}->[$n]});

        $self->data->{u2n}->{$self->data->{e2u}->[$n]}=($self->data->{e2n}->[$n] || '.notdef') unless(defined $self->data->{u2n}->{$self->data->{e2u}->[$n]});
        $self->data->{u2n}->{$self->data->{uni}->[$n]}=($self->data->{char}->[$n] || '.notdef') unless(defined $self->data->{u2n}->{$self->data->{uni}->[$n]});
    }
}

sub outobjdeep 
{
    my $self = shift @_;
    
    my $dict=PDFDict();
    $self->{Encoding}=$dict;
    $dict->{BaseEncoding}=PDFName('WinAnsiEncoding');
    $dict->{Differences}=PDFArray(PDFNum(0),map { PDFName($_||'.notdef') } @{$self->data->{char}});

    $self->{Widths}=PDFArray(map { PDFNum($self->data->{wx}->{$_||'.notdef'} || $self->missingwidth) } @{$self->data->{char}});

    $self->{FontBBox} = PDFArray(map { PDFNum($_) } ( $self->fontbbox ) );

    $self->tounicodemap;
    
    $self->SUPER::outobjdeep(@_);
}

1;

__END__

=back

=head1 AUTHOR

alfred reibenschuh

=head1 HISTORY

    $Log: gFont.pm,v $
    Revision 1.1.1.1  2005/02/17 14:51:57  fredo
    no message


=cut

