unit uMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edtLat: TEdit;
    edtLng: TEdit;
    edtPrecision: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//Dynamic Dll library load
  TCalculateGeohash = function(latitude,longitude:Double):PWideChar;StdCall;
  TCalculateNeighbors = function(latitude,longitude:Double; hash_len: Integer):PWideChar; stdcall;

//Static Dll library load
 function CalculateGeohash(latitude,longitude:Double):PWideChar; stdcall; external 'GeoHash.dll'; //standar 12 hash_length
 function CalculateGeohash_6(latitude,longitude:Double):PWideChar; stdcall; external 'GeoHash.dll';
 function CalculateGeohash_8(latitude,longitude:Double):PWideChar; stdcall; external 'GeoHash.dll';
 function CalculateGeohashString(CoorText : PWideChar; LengthCoor : Integer):PWideChar; StdCall; external 'GeoHash.dll'; //format "lat|lon|len" <> ej. '10.8|-73.13|6'
 function CalculateNeighbors(latitude, longitude:Double; hash_len: Integer = 12):PWideChar; stdcall; external 'GeoHash.dll';
 function CalculateNeighborsFromHash(hash : PWideChar; LengthHash : Integer):PWideChar; stdcall; external 'GeoHash.dll';

var
  Form1: TForm1;

implementation

{$R *.DFM}

function LocalCalculateNeighbors(latitude, longitude:Double; hash_len: Integer):String;
var
   GetQuery : TCalculateNeighbors;
   _rtapchar : PWideChar;
   Handle   : THandle;
  // temp: string;
begin
 Result := '';
 Handle := LoadLibrary('GeoHash.dll');

  if Handle <> 0 then
  begin
    @GetQuery := GetProcAddress(Handle, 'CalculateNeighbors');
    if @GetQuery <> nil then
    Begin
      _rtapchar := GetQuery(latitude,longitude, hash_len);
      Result := _rtapchar;

    End;
  End;
end;

function GetGeoHash(latitude,longitude:Double):String;
var
   GetQuery : TCalculateGeohash;
   _rtapchar : PWideChar;
   Handle   : THandle;
  // temp: string;
begin
Result := '';
Handle := LoadLibrary('GeoHash.dll');

  if Handle <> 0 then
  begin
    @GetQuery := GetProcAddress(Handle, 'CalculateNeighbors');
    if @GetQuery <> nil then
    Begin
      _rtapchar := GetQuery(latitude,longitude);
      Result := _rtapchar;

    End;
  End;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var
   //GetGeoHash : TCalculateGeohash;
   //Handle   : THandle;
    _rtapchar : PWideChar;
    Valor : PWideChar;
   _temp_coor : string;


begin
  DecimalSeparator := '.';
  ThousandSeparator := ',';
   memo1.lines.clear;
     _temp_coor := format('%s|%s|%s',[edtlat.text, edtlng.text, edtPrecision.text]);
   Valor := PWideChar(WideString(_temp_coor));


   _rtapchar := CalculateGeohash(StrToFloat(edtlat.text), StrToFloat(edtlng.text) );
   memo1.lines.add('length 12: ' + _rtapchar);

   _rtapchar := CalculateGeohash_6(StrToFloat(edtlat.text), StrToFloat(edtlng.text) );
   memo1.lines.add('length 6: ' + _rtapchar);

   _rtapchar := CalculateGeohash_8(StrToFloat(edtlat.text), StrToFloat(edtlng.text) );
   memo1.lines.add('length 8: ' + _rtapchar);

   _rtapchar := CalculateGeohashString(Valor, Length(_temp_coor) );
   memo1.lines.add('hash from: '+ QuotedStr(Valor) + ' <--> ' + _rtapchar);

   memo1.lines.add('');
   memo1.lines.add('Neighbors Hash');
   memo1.lines.add('');

   _rtapchar := CalculateNeighbors(StrToFloat(edtlat.text), StrToFloat(edtlng.text), 6 );
   memo1.lines.add(_rtapchar);

  // _rtapchar := CalculateNeighborsFromHash(PWideChar(WideString('d37bxg')), 6);
  // memo1.lines.add(_rtapchar);


end;



procedure TForm1.Button2Click(Sender: TObject);
var
    rta : String;
    //Valor : PWideChar;
   //_temp_coor : string;
begin

  DecimalSeparator := '.';
  ThousandSeparator := ',';
  //rrencyDecimals := 12;
  //rta := GetGeoHash(StrToFloat(edtLat.text), StrToFloat(edtLng.text));
  rta := LocalCalculateNeighbors(StrToFloat(edtLat.text), StrToFloat(edtLng.text), Round(StrToFloat(edtPrecision.text)));
  memo1.lines.add(rta);
end;

end.
