
{
 *  Copyright (C) 2021 by 120m4n
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 *  ------------------------------------------------------------------------------
 *
 *  This code is a direct derivation from:
 *      GeoHash Routines for dot Net 2011 (c) Sharon Lourduraj.
 *  The source of which can be found at:
 *      https://github.com/sharonjl/geohash-net/blob/master/geohash-net/Geohash.cs
 *
 }
unit myGeohash;

interface

uses SysUtils;

type
  TCardinalDirectionType = (cTop = 0,
    cRight = 1, cBottom = 2, cLeft = 3);

type
  TInterval  =  array[0..1] of Double;

const // 1d arrays
  Base32 : string = '0123456789bcdefghjkmnpqrstuvwxyz';
  Bits : array[0..4] of Integer = (16,8,4,2,1);

  Neighbors: array[0..1] of array[0..3] of String = (

   ( 'p0r21436x8zb9dcf5h7kjnmqesgutwvy',
    'bc01fg45238967deuvhjyznpkmstqrwx',
    '14365h7k9dcfesgujnmqp0r2twvyx8zb',
    '238967debc01fg45kmstqrwxuvhjyznp' ),

    ('bc01fg45238967deuvhjyznpkmstqrwx',
    'p0r21436x8zb9dcf5h7kjnmqesgutwvy',
    '238967debc01fg45kmstqrwxuvhjyznp',
    '14365h7k9dcfesgujnmqp0r2twvyx8zb' )
  );

  Borders: array[0..1] of array[0..3] of String = ( (
    'prxz', 'bcfguvyz', '028b', '0145hjnp' ), (
    'bcfguvyz', 'prxz', '0145hjnp', '028b' ));


  function Encode( latitude : double;  longitude : double;  precision : integer = 12): string;
  function CalculateAdjacent(hash : string; direction : TCardinalDirectionType):string;
  procedure RefineInterval (var interval : TInterval ; cd : integer; mask : integer);
  function Decode(geohash : string):TInterval;

implementation

procedure RefineInterval (var interval : TInterval ; cd : integer; mask : integer);
begin
  if ((cd and mask) <> 0) then
  begin
    interval[0] := (interval[0] + interval[1])/2;
  end
  else
  begin
    interval[1] := (interval[0] + interval[1])/2;
  end;

end;

function CalculateAdjacent(hash : string; direction : TCardinalDirectionType):string;
var
  hash_length    : integer;
  lastChr      : char;
  dir, is_odd : integer;
  nHash : string;
begin
    hash := AnsiLowerCase(hash);

    lastChr := hash[hash.Length - 1];

    is_odd := hash.Length div 2;
    dir := integer(direction);
    nHash := hash.Substring(0, hash.Length - 1);

    if (Borders[is_odd][dir].IndexOf(lastChr) <>-1) then
    begin
       nHash := CalculateAdjacent(nHash, direction);

    end;

    Result :=  nHash + Base32[Neighbors[is_odd][dir].IndexOf(lastChr)];
end;

function Decode(geohash : string):TInterval;
var
  even: boolean;
  C: Char;
  cd, j, mask : integer;
  lat, lon : TInterval;
begin
  even := True;
  lat[0] := -90.0;
  lat[1] := 90.0;

  lon[0] := -180.0;
  lon[1] := 180.0;


  for C in geohash do
  begin
     cd := Base32.IndexOf(C);
     for j := 0 to 4 do
     begin
       mask := Bits[j];

       if even then
       begin
         RefineInterval(lon, cd, mask);
       end
       else
       begin
        RefineInterval(lat, cd, mask);
       end;

       even := not even;
     end;
  end;

  Result[0] := (lat[0] + lat[1])/2;
  Result[1] := (lon[0] + lon[1])/2;
end;

function Encode( latitude : double;  longitude : double;  precision : integer = 12): string;
var
  even : boolean;
  bit : integer;
  ch : integer;
  geohash : string;
  lat, lon : TInterval;
  mid : double;
begin
  even := true;
  bit := 0;
  ch := 0;
  geohash := '';

  lat[0] := -90.0;
  lat[1] := 90.0;

  lon[0] := -180.0;
  lon[1] := 180.0;

   if (precision < 1) or (precision > 20) then precision := 12;

   while (geohash.Length < precision) do
   begin

      mid := 0;

      if even then
      begin
        mid := (lon[0] + lon[1])/2;
        if (longitude > mid) then
        begin
          ch := ch or Bits[bit];
          lon[0] := mid;
        end
        else
          lon[1] := mid;

      end
      else
      begin
        mid := (lat[0] + lat[1])/2;
        if (latitude > mid) then
        begin
          ch := ch or Bits[bit];
          lat[0] := mid;
        end
        else
          lat[1] := mid;
      end;

      even := not even;

      if (bit < 4) then
      begin
       bit := bit + 1;
      end
      else
      begin
        geohash :=  geohash + Base32[ch+1];
        bit := 0;
        ch := 0;
      end;
   end;

   Result := geohash;

end;


end.
