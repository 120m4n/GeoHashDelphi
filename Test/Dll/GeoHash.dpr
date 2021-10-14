library GeoHash;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  System.SysUtils,
  Windows,
  System.Classes,
  myGeohash in '..\..\myGeohash.pas';

//var
 // _LocalNeighborsString : String;

{$R *.res}

function CalculateGeohashString(CoorText : PChar; LengthCoor : Integer):PChar; stdcall;
var
  _temp : TStringList;
  hash : string;
  formatSettings:  TFormatSettings;
  _tempstr : String;
    lat,lng: double;
  hash_len : Integer;

begin
  FormatSettings := TFormatSettings.Create(GetThreadLocale);
  formatSettings.DecimalSeparator := '.';
  formatSettings.ThousandSeparator := ',';
  formatSettings.CurrencyDecimals := 12;

  try
    Result := '';
    SetString(_tempstr,CoorText,LengthCoor);
    try
      _temp := TStringList.Create;
      ExtractStrings(['|'],[],CoorText,_temp);

      if (_temp.Count >=3) then
      begin
        lat := StrToFloat(_temp[0], formatSettings);
        lng := StrToFloat(_temp[1], formatSettings);
        hash_len :=  Round( StrToFloat(_temp[2], formatSettings));

        hash := Encode(lat, lng, hash_len);
        Result := PChar(hash);
      end;
    finally
      _temp.free;
    end;

//

  except
    on E : Exception do
      Result := PChar(E.Message);

  end;
end;

function CalculateGeohash(latitude,longitude:Double):PChar; stdcall;
var
  hash : string;
begin
  try
    Result := '';
    hash := Encode(latitude, longitude, 12);
    Result := PChar(hash);
  except
    on E : Exception do
      Result := PChar(E.Message);

  end;
end;

function CalculateGeohash_6(latitude,longitude:Double):PChar; stdcall;
var
  hash : string;
begin
  try
    Result := '';
    hash := Encode(latitude, longitude, 6);
    Result := PChar(hash);
  except
    on E : Exception do
      Result := PChar(E.Message);

  end;
end;

function CalculateGeohash_8(latitude, longitude:Double):PChar; stdcall;
var
  hash : string;
begin
  try
    Result := '';
    hash := Encode(latitude, longitude, 8);
    Result := PChar(hash);
  except
    on E : Exception do
      Result := PChar(E.Message);

  end;
end;



Function CalculateNeighborsFromHash(hash : PChar; LengthHash : Integer):PChar; stdcall;
var
  _LocalNeighborsString : String;
  _temp: TStringList;
  _tmpHash,h_top,h_bottom  : String;
begin
  try
    Result := '';
    SetString(_tmpHash,hash,LengthHash);
    _temp := TStringList.Create;
    h_top  := CalculateAdjacent(_tmpHash, ctop );
    h_bottom := CalculateAdjacent(_tmpHash, cBottom);

    _temp.Values['hash']   := _tmpHash;
    _temp.Values['top'] := h_top;
    _temp.Values['right']  := CalculateAdjacent(_tmpHash, cright);
    _temp.Values['bottom'] := h_bottom;
    _temp.Values['left']   := CalculateAdjacent(_tmpHash, cleft);
    _temp.Values['top-right']     := CalculateAdjacent(h_top, cright);
    _temp.Values['botton-right']  := CalculateAdjacent(h_bottom, cright);
    _temp.Values['botton-left']   := CalculateAdjacent(h_bottom, cleft);
    _temp.Values['top-left']      := CalculateAdjacent(h_top, cleft);


    _LocalNeighborsString := _temp.Text;
    Result := PChar(_LocalNeighborsString);


  finally
    _temp.Free;
  end;

end;

function CalculateNeighbors(latitude, longitude:Double; hash_len: Integer = 12):PChar; stdcall;
var
  hash : string;
  h_top,h_bottom  : String;
  _LocalNeighborsString : String;
  _temp: TStringList;
begin
  try
    Result := '';
    hash := Encode(latitude, longitude, hash_len);
    try
      Result := '';
      _temp := TStringList.Create;
      h_top  := CalculateAdjacent(hash, ctop );
      h_bottom := CalculateAdjacent(hash, cBottom);

      _temp.Values['hash']   := hash;
      _temp.Values['top'] := h_top;
      _temp.Values['right']  := CalculateAdjacent(hash, cright);
      _temp.Values['bottom'] := h_bottom;
      _temp.Values['left']   := CalculateAdjacent(hash, cleft);
      _temp.Values['top-right']     := CalculateAdjacent(h_top, cright);
      _temp.Values['botton-right']  := CalculateAdjacent(h_bottom, cright);
      _temp.Values['botton-left']   := CalculateAdjacent(h_bottom, cleft);
      _temp.Values['top-left']      := CalculateAdjacent(h_top, cleft);


      //_LocalNeighborsString := stringreplace(_temp.commatext,',','@',[rfReplaceAll, rfIgnoreCase]);
      _LocalNeighborsString := _temp.Text;
      Result := PChar(_LocalNeighborsString);


    finally
      _temp.Free;
    end;

  except
    on E : Exception do
      Result := PChar(E.Message);

  end;
end;

exports
    CalculateGeohash,
    CalculateGeohash_6,
    CalculateGeohash_8,
    CalculateNeighbors,
    CalculateGeohashString,
    CalculateNeighborsFromHash;



begin
end.
