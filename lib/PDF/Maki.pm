#=======================================================================
#    ____  ____  _____           __  __       _    _
#   |  _ \|  _ \|  ___|  _   _  |  \/  | __ _| | _(_)
#   | |_) | | | | |_    (_) (_) | |\/| |/ _` | |/ / |
#   |  __/| |_| |  _|    _   _  | |  | | (_| |   <| |
#   |_|   |____/|_|     (_) (_) |_|  |_|\__,_|_|\_\_|
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
#   $Id: Maki.pm,v 1.1.1.1 2005/02/17 14:51:57 fredo Exp $
#
#=======================================================================

package PDF::Maki;

BEGIN {

    use vars qw[ @ISA $VERSION ];

    ($VERSION) = ('$Revision: 1.1.1.1 $' =~ /Revision: (\S+)\s/)[0];  # $Date: 2005/02/17 14:51:57 $

    require 5.008;      # we need this for unicode support
    use PDF::API2 1.68; # this is pdfapi2 with unifont 
    use Compress::Zlib;
    use Math::Trig;
    use POSIX qw[ ceil floor ];
    use utf8;
    use Encode qw[ :all ];

    @ISA = qw[ PDF::API2 ];

}

no warnings qw[ recursion uninitialized ];

=head1 NAME

PDF::Maki - Useful wrappers for PDF::API2.

=head1 SYNOPSIS

    use PDF::API2;
    use PDF::Maki;
    #

=head1 DESCRIPTION

PDF::Maki installs several alternative/additional methods into
the PDF::API2 namespaces so advanced features become available
outside the normal fixing/patch cycle of PDF::API2.

Stable methods will be migrated to PDF::API2 on fixed release states.

=cut

package PDF::API2::Util;

sub xmlMarkupDecl
{
    my $xml=<<EOT;
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<!DOCTYPE p [
EOT
    foreach my $n (sort {lc($a) cmp lc($b)} keys %n2u)
    {
        next if($n eq 'apos');
        next if($n eq 'amp');
        next if($n eq 'quot');
        next if($n eq 'gt');
        next if($n eq 'lt');
        next if($n eq '.notdef');
        next if($n2u{$n}<32);
        $xml.=sprintf('<!ENTITY %s "&#x%04X;">',$n,$n2u{$n})."\n";
    }
    $xml.="\n]>\n";
    return($xml);
}

=head1 PDF::API2 Methods

=over 4

=item $pdf->register_face $fontobj, $facename, $weightname, $stylename

B<I<*** DEVELOPER VERSION ***>>

register font for face/weight/style.

=item $fontobj = $pdf->lookup_face $facename, $weightname, $stylename

B<I<*** DEVELOPER VERSION ***>>

looking up the spec. face/weight/style or falling back to the default face.

=item $fontobj = $pdf->lookup_font $facename, $weightname, $stylename

B<I<*** DEVELOPER VERSION ***>>

looking up the spec. face/weight/style or falling back to alternative styles/weights.

=back

=cut

package PDF::API2;

#----------------------------------
# register font for face/weight/style
sub register_face
{
    my ($self,$font,$face,$weight,$style)=@_;
    $face=lc($face);
    $face=~s|[^a-z0-9]+||go;
    $weight=lc($weight);
    $weight=~s|[^a-z0-9]+||go;
    $style=lc($style);
    $style=~s|[^a-z0-9]+||go;
    if($face eq 'default')
    {
        $self->{fonts}->{$face}=$font;
    }
    else
    {
        $self->{fonts}->{"$face:$weight:$style"}=$font;
    }
}

#----------------------------------
# looking up the spec. face/weight/style or
# falling back to the default face
sub lookup_face
{
    my ($self,$face,$weight,$style)=@_;
    $face=lc($face);
    $face=~s|[^a-z0-9]+||go;
    $weight=lc($weight);
    $weight=~s|[^a-z0-9]+||go;
    $style=lc($style);
    $style=~s|[^a-z0-9]+||go;
    return $self->{fonts}->{"$face:$weight:$style"} 
        if(defined $self->{fonts}->{"$face:$weight:$style"});
    return $self->{fonts}->{$face} 
        if(defined $self->{fonts}->{$face});
    return(undef);
}


## FALLBACK CALCULUS...
sub _calc_style_fallback
{
    my @style_prelookup=@_;
    my %style_fallback=(
        'oblique'    => [qw{ italic normal regular }],
        'italic'     => [qw{ oblique normal regular }],
        'normal'     => [qw{ regular }],
        'regular'    => [qw{ normal }],
    );

    my @style_lookup=();
    my %reg=();
    my $w;
    while($w = lc shift @style_prelookup)
    {
        unless(defined $reg{$w})
        {
            push @style_lookup, $w;
            push @style_prelookup, @{$style_fallback{$w}} if(defined $style_fallback{$w});
            $reg{$w}=1;
        }
    }
    
    return(@style_lookup);
}
sub _calc_weight_fallback
{
    my @weight_prelookup=@_;
    my %weight_fallback=(
        'thin'       => [qw{ ultralight extralight light normal regular }],
        'ultralight' => [qw{ extralight light normal regular }],
        'extralight' => [qw{ light normal regular }],
        'light'      => [qw{ normal regular }],
        'normal'     => [qw{ regular }],
        'medium'     => [qw{ demibold semibold bold }],
        'demibold'   => [qw{ semibold bold }],
        'semibold'   => [qw{ bold }],
        'heavy'      => [qw{ extrabold ultrabold bold }],
        'extrabold'  => [qw{ ultrabold bold }],
        'ultrabold'  => [qw{ bold }],
    );

    my @weight_lookup=();
    my %reg=();
    my $w;
    while($w = lc shift @weight_prelookup)
    {
        unless(defined $reg{$w})
        {
            push @weight_lookup, $w;
            push @weight_prelookup, @{$weight_fallback{$w}} if(defined $weight_fallback{$w});
            $reg{$w}=1;
        }
    }
    
    return(@weight_lookup);
}
#----------------------------------
# looking up the spec. face/weight/style or
# falling back to alternative styles/weights
sub lookup_font
{
    my ($self,$face,$weight,$style)=@_;
    my $font=undef;
    foreach my $weightlookup (_calc_weight_fallback(lc $weight))
    {
        foreach my $stylelookup (_calc_style_fallback(lc $style))
        {
            $font=$self->lookup_face($face,$weightlookup,$stylelookup);
            last if(defined $font);
        }
        last if(defined $font);
    }
    $font=$self->lookup_face('default') unless(defined $font);
    return($font);
}

package PDF::API2::Content;

sub textstate2 
{
    my $self=shift @_;
    my %state;
    if(scalar @_) 
    {
        %state=@_;
        foreach my $k (qw[ charspace hspace wordspace lead rise render ]) 
        {
            next unless($state{$k});
            if($self->{" $k"} ne $state{$k})
            {
                eval ' $self->'.$k.'($state{$k}); ';
            }
        }
        if($state{font} && $state{fontsize}) 
        {
            if($self->{" font"} ne $state{font} || $self->{" fontsize"} ne $state{fontsize})
            {
                $self->font($state{font},$state{fontsize});
            }
        }
        if($state{fillcolor}) 
        {
            $self->fillcolor(@{$state{fillcolor}});
        }
        if($state{strokecolor}) 
        {
            $self->strokecolor(@{$state{strokecolor}});
        }
        %state=();
    } 
    else 
    {
        foreach my $k (qw[ font fontsize charspace hspace wordspace lead rise render ]) 
        {
            $state{$k}=$self->{" $k"};
        }
        $state{fillcolor}=[@{$self->{" fillcolor"}}];
        $state{strokecolor}=[@{$self->{" strokecolor"}}];
    }
    return(%state);
}

=head1 PDF::API2::Page Methods

=over 4

=item $width = $page->textlabel $x, $y, $font, $size, $text, %options

B<I<*** DEVELOPER VERSION ***>>

Applys text with options and returns the width of the given text.

B<Example:> 

    $page->textlabel(300,700,$myfont,20,'Page Header',
        -rotate => -30,
        -color => '#FF0000',
        -hspace => 120,
        -center => 1,
    );

I<This simply a proxy-method to the equivalent PDF::API2::Content method.>


=item $page->textmarkup $xml, %options

B<I<*** EXPERIMENTAL VERSION ***>>

B<Example:> 

    $utr=$pdf->corefont('Times-Roman', -encode=>'latin1');
    $page->textmarkup($xml,
        -x=>100,
        -y=>700,
        -w=>400,
        -fontreg=>[
            [ $utr,'times','regular','regular' ],
            [ $utb,'times','bold','regular' ],
            [ $uti,'times','regular','italic' ],
            [ $utz,'times','bold','regular' ],
        ]
    );

    
=back

=cut

package PDF::API2::Page;

use XML::Parser;
use Math::Trig;
use POSIX qw[ ceil floor ];
use utf8;
use Encode qw[ :all ];
use Data::Dumper;
use PDF::API2::UniWrap;

sub textlabel {
    my ($self,@opts) = @_;
    my $gfx=$self->gfx;
    return $gfx->textlabel(@opts);
}

#   -fontnormal,     -fn      => <f>
#   -fontbold,       -fb      => <f>
#   -fontitalic,     -fi      => <f>
#   -fontbolditalic, -fbi     => <f>
#
#   -mononormal,     -mn      => <f>
#   -monobold,       -mb      => <f>
#   -monoitalic,     -mi      => <f>
#   -monobolditalic, -mbi     => <f>
#
#   -fontsize,       -fs      => <n>  (default: 10)
#
#   -superscript,    -super   => <n>  (default: +fontsize*0.5)
#   -subscript,      -sub     => <n>  (default: -fontsize*0.35)
#   -supersubsize,   -sss     => <n>  (default: fontsize*0.5)
#
#   -lead,           -l       => <n>  (default: fontsize*1.4)
#
#-----
#
# <p>
#       x=          <n>
#       y=          <n>
#       width=      <n>
#       height=     <n>
#       bgcolor=    #00FF00 
#                   | <name>
#
# <i> <b> <u> <s> <c> <e> <sub> <sup> <br/>
#
# <span> 
#       face=       serif
#                   | sans
#                   | mono
#                   | <s>
#                       | Courier 
#                       | Georgia 
#                       | Helvetica 
#                       | Times 
#                       | Trebuchet 
#                       | Verdana 
#
#       size=       xx-small            (6pt)
#                   | x-small           (8pt)
#                   | small             (10pt)
#                   | medium            (12pt)
#                   | normal            (12pt)
#                   | large             (14pt)
#                   | x-large           (18pt)
#                   | xx-large          (24pt)
#                   | <n>
#
#       style=      normal
#                   | regular
#                   | italic
#                   | oblique
#
#       weight=     thin                (100)
#                   | ultralight        (200)
#                   | extralight        (200)
#                   | light             (300)
#                   | normal            (400)
#                   | regular           (400)
#                   | medium            (500)
#                   | demibold          (600)
#                   | semibold          (600)
#                   | bold              (700)
#                   | ultrabold         (800)
#                   | extrabold         (800)
#                   | heavy             (900)
#                   | <n>
#  
#
#       stretch=    ultracondensed      (0.500)
#                   | extracondensed    (0.625) 
#                   | condensed         (0.750)
#                   | semicondensed     (0.875)
#                   | normal            (1.000)
#                   | medium            (1.000)
#                   | semiexpanded      (1.125)
#                   | expanded          (1.250)
#                   | extraexpanded     (1.500)
#                   | ultraexpanded     (2.000)
#                   | <n>
#
#       color=      #00FF00 
#                   | <name>
#
#       underline=  single 
#                   | double 
#                   | low 
#                   | none
#
#       rise=       superscript
#                   | subscript
#                   | none
#                   | <n>
#
#
#   <a> 
#       id="somename" name="somename"   %% create a "#somename" in the name-tree 
#
#       href="url"                      %% if url == m|^#| dest to name-tree 
#
sub _tm_calc_style_fallback
{
    my @style_prelookup=@_;
    my %style_fallback=(
        'oblique'    => [qw{ italic normal regular }],
        'italic'     => [qw{ oblique normal regular }],
        'normal'     => [qw{ regular }],
        'regular'    => [qw{ normal }],
    );

    my @style_lookup=();
    my %reg=();
    my $w;
    while($w = lc shift @style_prelookup)
    {
        unless(defined $reg{$w})
        {
            push @style_lookup, $w;
            push @style_prelookup, @{$style_fallback{$w}} if(defined $style_fallback{$w});
            $reg{$w}=1;
        }
    }
    
    return(@style_lookup);
}
sub _tm_calc_weight_fallback
{
    my @weight_prelookup=@_;
    my %weight_fallback=(
        'thin'       => [qw{ ultralight extralight light normal regular }],
        'ultralight' => [qw{ extralight light normal regular }],
        'extralight' => [qw{ light normal regular }],
        'light'      => [qw{ normal regular }],
        'normal'     => [qw{ regular }],
        'medium'     => [qw{ demibold semibold bold }],
        'demibold'   => [qw{ semibold bold }],
        'semibold'   => [qw{ bold }],
        'heavy'      => [qw{ extrabold ultrabold bold }],
        'extrabold'  => [qw{ ultrabold bold }],
        'ultrabold'  => [qw{ bold }],
    );

    my @weight_lookup=();
    my %reg=();
    my $w;
    while($w = lc shift @weight_prelookup)
    {
        unless(defined $reg{$w})
        {
            push @weight_lookup, $w;
            push @weight_prelookup, @{$weight_fallback{$w}} if(defined $weight_fallback{$w});
            $reg{$w}=1;
        }
    }
    
    return(@weight_lookup);
}
sub _tm_calc_stretch
{
    my $s = shift @_;
    my %stretch=qw[
        ultracondensed  0.500
        extracondensed  0.625
        condensed       0.750
        semicondensed   0.875
        normal          1
        medium          1
        semiexpanded    1.125
        expanded        1.250
        extraexpanded   1.500
        ultraexpanded   2
    ];
    return($s) if($s=~m|^[\d\.]+$|);
    return($stretch{$s}) if(defined $stretch{$s});
    return(1);
}
sub _tm_calc_size
{
    my $s = shift @_;
    my %size=qw[
        xx-small     6
        x-small      8
        small       10
        medium      12
        normal      12
        large       14
        x-large     18
        xx-large    24
    ];
    return($s) if($s=~m|^[\d\.]+$|);
    return($size{$s}) if(defined $size{$s});
    return(10);
}
sub _tm_calc_rise
{
    my ($s,$sz)=@_;
    my %rise=qw[
        superscript 0.5
        subscript   -0.35
        none        0
    ];
    return($s) if($s=~m|^[\-\d\.]+$|);
    return($rise{$s}*$sz) if(defined $rise{$s});
    return(0);
}

#----------------------------------
# register font for face/weight/style
sub _tm_register_face
{
    my ($self,$font,$face,$weight,$style)=@_;
    $face=lc($face);
    $face=~s|[^a-z0-9]+||go;
    $weight=lc($weight);
    $weight=~s|[^a-z0-9]+||go;
    $style=lc($style);
    $style=~s|[^a-z0-9]+||go;
    if($face eq 'default')
    {
        $self->{-fonts}->{$face}=$font;
    }
    else
    {
        $self->{-fonts}->{"$face:$weight:$style"}=$font;
    }
    ## $self->{' api'}->register_face($font,$face,$weight,$style);
}

#----------------------------------
# looking up the spec. face/weight/style or
# falling back to face-only
sub _tm_lookup_face
{
    my ($self,$face,$weight,$style)=@_;
    $face=lc($face);
    $face=~s|[^a-z0-9]+||go;
    $weight=lc($weight);
    $weight=~s|[^a-z0-9]+||go;
    $style=lc($style);
    $style=~s|[^a-z0-9]+||go;
    return $self->{-fonts}->{"$face:$weight:$style"} 
        if(defined $self->{-fonts}->{"$face:$weight:$style"});
    return $self->{-fonts}->{$face} 
        if(defined $self->{-fonts}->{$face});
    return $self->{' api'}->lookup_face($face,$weight,$style);
}

#----------------------------------
# lookup the spec. font or falling back to the default
sub _tm_lookup_font
{
    my ($self,$face,$weight,$style)=@_;
    my $font=undef;
    foreach my $weightlookup (_tm_calc_weight_fallback(lc $weight))
    {
        foreach my $stylelookup (_tm_calc_style_fallback(lc $style))
        {
            $font=$self->_tm_lookup_face($face,$weightlookup,$stylelookup);
            last if(defined $font);
        }
        last if(defined $font);
    }
    $font=$self->_tm_lookup_face('default') unless(defined $font);
    return($font);
}
sub _tm_push_span
{
    my ($xp,$span)=@_;
    push @{$xp->{-pdf}->{content}},$span;
}
sub _tm_push_content
{
    my ($xp,$str)=@_;
    $xp->{-pdf}->{content}->[-1]->[-1].=$str;
}
sub _tm_push
{
    my ($xp,%state)=@_;
    push @{$xp->{-pdf}->{stack}},{%state};
}
sub _tm_pop
{
    my ($xp)=@_;
    return(%{pop @{$xp->{-pdf}->{stack}}});
}
sub _tm_peek
{
    my ($xp)=@_;
    return(%{$xp->{-pdf}->{stack}->[-1]});
}
sub _tm_init
{
    my ($xp)=@_;
    #print STDERR "INIT.\n";
}
sub _tm_final
{
    my ($xp)=@_;
    #print STDERR "FINAL.\n";
}
sub _tm_startelement
{
    my ($xp,$element,%attr)=@_;
    #print STDERR "START: $element\n";
    my %state=_tm_peek($xp);
    if($element eq 'span' || $element eq 'p')
    {
        %state=(%state,%attr);
    }
    elsif($element eq 'b')
    {
        $state{weight}='bold';
    }
    elsif($element eq 'i')
    {
        $state{style}='italic';
    }
    elsif($element eq 'sub')
    {
        $state{rise}=(-$attr{value})||'subscript';
        $state{risesize}=$attr{size}||'normal';
    }
    elsif($element eq 'sup')
    {
        $state{rise}=$attr{value}||'superscript';
        $state{risesize}=$attr{size}||'normal';
    }
    elsif($element eq 'u')
    {
        $state{underline}=$attr{type}||'single';
    }
    elsif($element eq 'c')
    {
        $state{stretch}=$attr{value}||'condensed';
    }
    elsif($element eq 'e')
    {
        $state{stretch}=$attr{value}||'expanded';
    }
    elsif($element eq 'a')
    {
        if(defined $attr{id})
        {
            $state{id}=$attr{id};    
        }
        if(defined $attr{name})
        {
            $state{id}=$attr{name};    
        }
        if(defined $attr{href})
        {
            $state{href}=$attr{href};    
        }
    }
    _tm_push($xp,%state);
    _tm_push_span($xp,[{%state},'']);
}
sub _tm_endelement
{
    my ($xp,$element)=@_;
    #print STDERR "END: $element\n";
    _tm_pop($xp);
    my %state=_tm_peek($xp);
    _tm_push_span($xp,[{%state},'']);
}
sub _tm_char
{
    my ($xp,$str)=@_;
    _tm_push_content($xp,$str);
    #print STDERR 'DEBUG: '.Dumper($xp);
    #print STDERR "CHAR: '$str'\n";
}
sub _tm_debug
{
    #print STDERR 'DEBUG: '.Dumper(\@_);
    return('unknown');
}
sub _tm_null
{
    #print STDERR "NULL.\n";
    return('unknown');
}

sub textmarkup 
{
    my ($self,$xml,%opts)=@_;
    
    if(defined $opts{-fontreg})
    {
        foreach my $reg (@{$opts{-fontreg}})
        {
            $self->_tm_register_face(@{$reg});
        }
    }
    
    my $xp=new XML::Parser::Expat(ProtocolEncoding=>'UTF-8',NoExpand=>0,NoLWP=>1);
    $xp->setHandlers(
        #'Init'         => \&_tm_init,
        #'Final'        => \&_tm_final,
        'Start'        => \&_tm_startelement,
        'End'          => \&_tm_endelement,
        'Char'         => \&_tm_char,
        'Proc'         => \&_tm_null,
        'Comment'      => \&_tm_null,
        'CdataStart'   => \&_tm_null,
        'CdataEnd'     => \&_tm_null,
        'Default'      => \&_tm_null,
        'Unparsed'     => \&_tm_null,
        'Notation'     => \&_tm_null,
        'ExternEnt'    => \&_tm_null,
        'ExternEntFin' => \&_tm_null,
        'Entity'       => \&_tm_null,
        'Doctype'      => \&_tm_null,
        'DoctypeFin'   => \&_tm_null,
        'XMLDecl'      => \&_tm_null,
    );
    $xml=PDF::API2::Util->xmlMarkupDecl."\n".$xml;
    $xp->{-pdf}={
        'fonts'=>{},
        'stack'=>[
            { 
                'face'=>'times', 
                'size'=>'normal', 
                'style'=>'normal', 
                'weight'=>'normal',
                'stretch'=>'normal',
                'color'=>'black',
                'underline'=>'none',
                'rise'=>'none',
            }
        ], 
        'content'=>[], 
    };
    $xp->parse($xml);
    my @content=();
    while(scalar @{$xp->{-pdf}->{content}} > 0)
    {
        my $chunk=shift @{$xp->{-pdf}->{content}};
        next if($chunk->[1] eq '');

        $chunk->[0]->{font}=$self->_tm_lookup_font($chunk->[0]->{face},$chunk->[0]->{weight},$chunk->[0]->{style});
        $chunk->[0]->{fontsize}=_tm_calc_size($chunk->[0]->{size});
        $chunk->[0]->{hspace}=_tm_calc_stretch($chunk->[0]->{stretch})*100;
        $chunk->[0]->{rise}=_tm_calc_rise($chunk->[0]->{rise},$chunk->[0]->{fontsize});
        $chunk->[0]->{fillcolor}=[$chunk->[0]->{color}];
        $chunk->[0]->{fontsize}=_tm_calc_size($chunk->[0]->{risesize}) if(defined $chunk->[0]->{risesize});
        delete $chunk->[0]->{face};
        delete $chunk->[0]->{weight};
        delete $chunk->[0]->{style};
        delete $chunk->[0]->{size};
        delete $chunk->[0]->{stretch};
        delete $chunk->[0]->{color};

        my $wrap=PDF::API2::UniWrap->new(line_length=>1,emergency_break=>100);
        foreach my $t ( $wrap->break_lines($chunk->[1]) )
        {
            utf8::upgrade($t);
            push @content,[$chunk->[0],$t];
        }
    }
    my $w=0;
    my $l=0;
    my $txt=$self->text;
    $txt->transform(-translate=>[$opts{-x},$opts{-y}]);
    my $laststyle=undef;
    foreach my $chunk ( @content )
    {
        my %txtopt=();
        if($opts{-w} < ($w+$txt->advancewidth($chunk->[1],%{$chunk->[0]})))
        {
            $txt->lead($l);
            $txt->nl;
            $w=0;
            $l=$chunk->[0]->{fontsize}*1.2;
        }
        elsif($chunk->[0]->{fontsize}*1.2 > $l)
        {
            $l=$chunk->[0]->{fontsize}*1.2;            
        }
        if($laststyle ne $chunk->[0])
        {
            $txt->font($chunk->[0]->{font},$chunk->[0]->{fontsize});
            $txt->hspace($chunk->[0]->{hspace});
            $txt->rise($chunk->[0]->{rise});
            $txt->fillcolor(@{$chunk->[0]->{fillcolor}});
        }
        $txtopt{-underline}=[$chunk->[0]->{fontsize}*0.20,$chunk->[0]->{fontsize}*0.1] if($chunk->[0]->{underline} ne 'none');
        my $xy1=[$txt->textpos2];
        $w+=$txt->text($chunk->[1],%txtopt);
        $laststyle=$chunk->[0];
        my $xy2=[$txt->textpos2];
        if(defined $chunk->[0]->{id})
        {
            $self->{' api'}->named_destination('Dests','#'.$chunk->[0]->{id})->link($self);

            delete $chunk->[0]->{id};
        }        
        if(defined $chunk->[0]->{href})
        {
            my $at=$self->annotation;
            $at->rect(
                $txt->_textpos(@{$xy1}),
                $txt->_textpos(@{$xy2},0,$chunk->[0]->{fontsize})
            );
            if($chunk->[0]->{href}=~m|^#|)
            {
                $at->link($chunk->[0]->{href});
            }
            else
            {
                $at->url($chunk->[0]->{href});
            }
        }        
    }

    $xp=undef;
    #print STDERR 'XML: '.Dumper(\$content[0]);
    $txt->textend;
}

1;

__END__

=head1 AUTHOR

alfred reibenschuh

=head1 HISTORY

    $Log: Maki.pm,v $
    Revision 1.1.1.1  2005/02/17 14:51:57  fredo
    no message

    

=cut
