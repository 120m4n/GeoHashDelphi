object Form2: TForm2
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'GeoHash'
  ClientHeight = 211
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 19
    Top = 37
    Width = 33
    Height = 13
    Caption = 'Latitud'
  end
  object Label2: TLabel
    Left = 19
    Top = 64
    Width = 41
    Height = 13
    Caption = 'Longitud'
  end
  object Label3: TLabel
    Left = 19
    Top = 91
    Width = 42
    Height = 13
    Caption = 'Precision'
  end
  object Label4: TLabel
    Left = 184
    Top = 169
    Width = 43
    Height = 13
    Caption = 'Decode: '
  end
  object lblDecode: TLabel
    Left = 233
    Top = 169
    Width = 279
    Height = 13
    Alignment = taCenter
    AutoSize = False
    WordWrap = True
    OnDblClick = lblDecodeDblClick
  end
  object edtLat: TEdit
    Left = 66
    Top = 34
    Width = 135
    Height = 21
    Alignment = taCenter
    TabOrder = 0
    Text = '7.1384'
  end
  object edtLng: TEdit
    Left = 66
    Top = 61
    Width = 135
    Height = 21
    Alignment = taCenter
    TabOrder = 1
    Text = '-73.13'
  end
  object edtPrecision: TEdit
    Left = 175
    Top = 88
    Width = 26
    Height = 21
    Alignment = taCenter
    NumbersOnly = True
    TabOrder = 2
    Text = '6'
  end
  object btnDecode: TButton
    Left = 126
    Top = 138
    Width = 75
    Height = 25
    Caption = 'Decode'
    Enabled = False
    TabOrder = 3
    OnClick = btnDecodeClick
  end
  object btnEncode: TButton
    Left = 19
    Top = 138
    Width = 75
    Height = 25
    Caption = 'Encode'
    TabOrder = 4
    OnClick = btnEncodeClick
  end
  object Center: TEdit
    Left = 328
    Top = 78
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 5
    Text = 'hash'
    OnMouseDown = EastMouseDown
  end
  object NorthWest: TEdit
    Left = 232
    Top = 34
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 6
    Text = 'NorthWest'
    OnMouseDown = EastMouseDown
  end
  object North: TEdit
    Left = 327
    Top = 34
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 7
    Text = 'North'
    OnMouseDown = EastMouseDown
  end
  object NorthEast: TEdit
    Left = 422
    Top = 34
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 8
    Text = 'NorthEast'
    OnMouseDown = EastMouseDown
  end
  object West: TEdit
    Left = 233
    Top = 78
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 9
    Text = 'West'
    OnMouseDown = EastMouseDown
  end
  object East: TEdit
    Left = 423
    Top = 78
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 10
    Text = 'East'
    OnMouseDown = EastMouseDown
  end
  object SouthWest: TEdit
    Left = 233
    Top = 121
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 11
    Text = 'SouthWest'
    OnMouseDown = EastMouseDown
  end
  object South: TEdit
    Left = 328
    Top = 121
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 12
    Text = 'South'
    OnMouseDown = EastMouseDown
  end
  object SouthEast: TEdit
    Left = 423
    Top = 121
    Width = 89
    Height = 21
    Alignment = taCenter
    TabOrder = 13
    Text = 'SouthEast'
    OnMouseDown = EastMouseDown
  end
  object ActionList1: TActionList
    Left = 96
    Top = 184
    object Action1: TAction
      Caption = 'Action1'
      OnExecute = Action1Execute
    end
  end
end
