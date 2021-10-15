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
  myGeohash in 'myGeohash.pas';

const
  LibraryName = 'GeoHash.dll';
  endl: string = #13#10;


{$R *.res}

function CalculateGeohashString(CoorText : PChar; LengthCoor : Integer):PChar; stdcall;
var
  _temp : TArray<String>;
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

      _temp := _tempstr.Split(['|']);
      if (Length(_temp) >=3) then
      begin
        lat := StrToFloat(_temp[0], formatSettings);
        lng := StrToFloat(_temp[1], formatSettings);
        hash_len :=  Round( StrToFloat(_temp[2], formatSettings));

        hash := Encode(lat, lng, hash_len);
        Result := PChar(hash);
      end;

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
  _temp: String;
  _tmpHash,h_top,h_bottom  : String;
begin
  try
    Result := '';
    SetString(_tmpHash,hash,LengthHash);
    h_top  := CalculateAdjacent(_tmpHash, ctop );
    h_bottom := CalculateAdjacent(_tmpHash, cBottom);

    _temp := 'hash:'+hash + endl;
    _temp := _temp + 'top:' + h_top + endl;
    _temp := _temp + 'right:'  + CalculateAdjacent(hash, cright) + endl;
    _temp := _temp + 'bottom:' + h_bottom + endl;
    _temp := _temp + 'left:'   + CalculateAdjacent(hash, cleft) + endl;
    _temp := _temp + 'top-right:'     + CalculateAdjacent(h_top, cright) + endl;
    _temp := _temp + 'botton-right:'  + CalculateAdjacent(h_bottom, cright) + endl;
    _temp := _temp + 'botton-left:'   + CalculateAdjacent(h_bottom, cleft) + endl;
    _temp := _temp + 'top-left:'      + CalculateAdjacent(h_top, cleft);

    Result := PChar(_temp);

  except
    on E : Exception do
      Result := PChar(E.Message);
  end;

end;

function CalculateNeighbors(latitude, longitude:Double; hash_len: Integer = 12):PChar; stdcall;
var
  hash : string;
  h_top,h_bottom  : String;
  _LocalNeighborsString : String;
  _temp: String;
begin
  try
    Result := '';
    hash := Encode(latitude, longitude, hash_len);

      Result := '';

      h_top  := CalculateAdjacent(hash, ctop );
      h_bottom := CalculateAdjacent(hash, cBottom);

      _temp := 'hash:'+hash + endl;
      _temp := _temp + 'top:' + h_top + endl;
      _temp := _temp + 'right:'  + CalculateAdjacent(hash, cright) + endl;
      _temp := _temp + 'bottom:' + h_bottom + endl;
      _temp := _temp + 'left:'   + CalculateAdjacent(hash, cleft) + endl;
      _temp := _temp + 'top-right:'     + CalculateAdjacent(h_top, cright) + endl;
      _temp := _temp + 'botton-right:'  + CalculateAdjacent(h_bottom, cright) + endl;
      _temp := _temp + 'botton-left:'   + CalculateAdjacent(h_bottom, cleft) + endl;
      _temp := _temp + 'top-left:'      + CalculateAdjacent(h_top, cleft);

      Result := PChar(_temp);


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
