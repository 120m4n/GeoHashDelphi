unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Actions,
  Vcl.ActnList;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edtLat: TEdit;
    edtLng: TEdit;
    edtPrecision: TEdit;
    btnDecode: TButton;
    btnEncode: TButton;
    Label3: TLabel;
    Center: TEdit;
    NorthWest: TEdit;
    North: TEdit;
    NorthEast: TEdit;
    West: TEdit;
    East: TEdit;
    SouthWest: TEdit;
    South: TEdit;
    SouthEast: TEdit;
    Label4: TLabel;
    lblDecode: TLabel;
    ActionList1: TActionList;
    Action1: TAction;
    procedure btnEncodeClick(Sender: TObject);
    procedure btnDecodeClick(Sender: TObject);
    procedure EastMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Action1Execute(Sender: TObject);
    procedure lblDecodeDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses myGeohash;

procedure TForm2.Action1Execute(Sender: TObject);
  function toString(value : TInterval):string;
  begin
     result :=  'lat:' + value[0].ToString + '; lon: ' + value[1].ToString;
  end;

  var
    tmp : TInterval;
begin
  tmp := Decode((Sender as TEdit).Text);

   lblDecode.Caption := toString(tmp);
end;

procedure TForm2.btnDecodeClick(Sender: TObject);

  function toString(value : TInterval):string;
  begin
     result :=  'lat:' + value[0].ToString + '; lon: ' + value[1].ToString;
  end;

  var
  tmp : TInterval;
begin
  tmp := Decode(center.Text);

   lblDecode.Caption := toString(tmp);
end;

procedure TForm2.btnEncodeClick(Sender: TObject);
var
  lat,lng: double;
  hash_len : Integer;
  hash : string;
  h_top: string;
  h_right: string;
  h_bottom: string;
  h_left: string;
  formatSettings:  TFormatSettings;

begin
  {$IFDEF VER220}
    FormatSettings := TFormatSettings.Create(GetThreadLocale);
  {$ELSE}
      GetLocaleFormatSettings(GetThreadLocale, FormatSettings);
  {$ENDIF}
  formatSettings.DecimalSeparator := '.';
  formatSettings.ThousandSeparator := ',';
  formatSettings.CurrencyDecimals := 12;

  lat := StrToFloat(edtLat.Text, formatSettings);
  lng := StrToFloat(edtLng.Text, formatSettings);
  hash_len :=  Round( StrToFloat(edtPrecision.Text, formatSettings));

  hash := Encode(lat, lng, hash_len);
  Center.Text := hash;

  h_top   := CalculateAdjacent(hash, ctop );
  h_right := CalculateAdjacent(hash, cright);
  h_bottom:= CalculateAdjacent(hash, cBottom);
  h_left  := CalculateAdjacent(hash, cleft);


  North.Text := h_top;
  South.Text := h_bottom;
  East.Text := h_right ;
  West.Text := h_left;
  NorthEast.Text := CalculateAdjacent(h_top, cright);
  NorthWest.Text := CalculateAdjacent(h_top, cleft);
  SouthEast.Text := CalculateAdjacent(h_bottom, cright);
  SouthWest.Text := CalculateAdjacent(h_bottom, cleft);

  btnDecode.Enabled := True;

end;

procedure TForm2.EastMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  function toString(value : TInterval):string;
  begin
     result :=  'lat:' + value[0].ToString + ' ; lon:' + value[1].ToString;
  end;
var
  tmp: TInterval;
begin
//  showmessage((Sender as TEdit).Text);
  tmp := Decode((Sender as TEdit).Text);
  lblDecode.Caption := toString(tmp);
end;

procedure TForm2.lblDecodeDblClick(Sender: TObject);
var
  tmp, lat, lon: string;
  LatLon, tmpLatLon : TStringList;
begin
   tmp := (Sender as TLabel).Caption;
   LatLon := TStringList.Create;
   tmpLatLon := TStringList.Create;
    try
      ExtractStrings([';'], [], PChar(tmp), tmpLatLon);
      lat := tmpLatLon[0];
      lon := tmpLatLon[1];
      ExtractStrings([':'], [], PChar(lat), LatLon);
      lat := LatLon[1];
      LatLon.Clear;
      ExtractStrings([':'], [], PChar(lon), LatLon);
      lon := LatLon[1];

      edtLat.Text := lat.Replace(',','.');
      edtLng.Text := lon.Replace(',','.');

    finally
      LatLon.Free;
      tmpLatLon.Free;
    end;
end;

end.
