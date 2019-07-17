unit uMiniMap;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CylinderMap, DataStructs, Math, Resources, uHexMap;

const
  mmGeo = 0;
  mmPolitical = 1;
  mmVisible = 2;

  clDarkLand =  $00006060;
  clDarkWater = $00800000;

type
  TMiniMapForm = class(TForm)
    MiniMap: TCylinderMap;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MiniMapDrawHex(Sender: TObject; HX, HY: Integer;
      ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
    procedure MiniMapSelectHex(Sender: TObject; HX, HY: Integer);
    procedure FormActivate(Sender: TObject);
    procedure MiniMapAfterPaint(Sender: TObject);
  private
    HexMap: TCylinderMap;
  public
    constructor Create(AOwner: TComponent; AHexMap: TCylinderMap); reintroduce;
  end;

var
  MiniMapForm: TMiniMapForm;

implementation

uses Main;

{$R *.lfm}

constructor TMiniMapForm.Create(AOwner: TComponent; AHexMap: TCylinderMap);
begin
  HexMap := AHexMap;
  inherited Create(AOwner);
end;

procedure TMiniMapForm.FormCreate(Sender: TObject);
begin
  Config.WriteBool('MiniMap', 'Visible', TRUE);
  Width := Config.ReadInteger('MiniMap', 'Width', 100);
  Height := Config.ReadInteger('MiniMap', 'Height', 100);
  Left := Config.ReadInteger('MiniMap', 'Left', 100);
  Top := Config.ReadInteger('MiniMap', 'Top', 100);
end;

procedure TMiniMapForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
  Config.WriteInteger('MiniMap', 'Width', Width);
  Config.WriteInteger('MiniMap', 'Height', Height);
  Config.WriteInteger('MiniMap', 'Left', Left);
  Config.WriteInteger('MiniMap', 'Top', Top);
  Config.WriteBool('MiniMap', 'Visible', FALSE);
  MiniMapForm := nil;
end;

procedure TMiniMapForm.FormActivate(Sender: TObject);
begin
  if MiniMap.ColCount <> HexMap.ColCount then MiniMap.ColCount := HexMap.ColCount;
  if MiniMap.RowCount <> HexMap.RowCount then MiniMap.RowCount := HexMap.RowCount;
  if MiniMap.FirstOdd <> HexMap.FirstOdd then MiniMap.FirstOdd := HexMap.FirstOdd;
  MiniMap.Center(HexMap.Selected.X, HexMap.Selected.Y);
end;

procedure TMiniMapForm.MiniMapSelectHex(Sender: TObject; HX, HY: Integer);
begin
  HexMap.Center(HX, HY);
end;

procedure TMiniMapForm.MiniMapDrawHex(Sender: TObject; HX, HY: Integer;
  ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
var mapX, mapY: integer;
    R: TRegion;
begin
  if (Game = nil) or (VTurn = nil) then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);
  with Map do
    R := Region(mapX, mapY);
    if R <> nil then begin
      case Config.ReadInteger('MiniMap', 'Mode', mmGeo) of
        mmPolitical:
          if R.Guard <> nil then
            ACanvas.Brush.Color := FactionColor(R.Guard)
          else if Test(R.Terrain.Flags, TER_WATER) then
            ACanvas.Brush.Color := clDarkWater
          else ACanvas.Brush.Color := clDarkLand;
        mmVisible:
          if R.Visited = Turn.Num then
            ACanvas.Brush.Color := clYellow
          else if Test(R.Terrain.Flags, TER_WATER) then
            ACanvas.Brush.Color := clDarkWater
          else ACanvas.Brush.Color := clDarkLand;
        else
          ACanvas.Brush.Color := R.Terrain.Color;
      end;
      ACanvas.Pen.Color := ACanvas.Brush.Color;
      with MiniMap do
      ACanvas.Polygon([Point(CX-hs25, CY-hs43), Point(CX+hs25, CY-hs43),
        Point(CX+hs50, CY), Point(CX+hs25, CY+hs43), Point(CX-hs25, CY+hs43),
        Point(CX-hs50, CY)]);
    end;
end;

procedure TMiniMapForm.MiniMapAfterPaint(Sender: TObject);
var i, x, y, wid, hei: integer;
begin
  if Game = nil then Exit;
  with MiniMap do begin
    Canvas.Pen.Color := SelectColor;
    wid := Round((HexMap.MapRect.Right - HexMap.MapRect.Left) * (hs / HexMap.hs));
    hei := Round((HexMap.MapRect.Bottom - HexMap.MapRect.Top) * (hs / HexMap.hs));

    for i := -1 to 1 do begin
      CalcHexCoords(HexMap.VisHexes.Left + ColCount*i, HexMap.VisHexes.Top, x, y);
      if (x+wid >= DrawOffset.X) and (x < DrawOffset.X+GridWidth) then begin
        Canvas.MoveTo(Max(x, DrawOffset.X), y);
        Canvas.LineTo(Min(x+wid, DrawOffset.X+GridWidth), y);
        if (x+wid < DrawOffset.X+GridWidth) and (wid < GridWidth) then
          Canvas.LineTo(x+wid, y+hei);
        Canvas.MoveTo(Min(x+wid, DrawOffset.X+GridWidth-1), y+hei);
        Canvas.LineTo(Max(x, DrawOffset.X-1), y+hei);
        if (x > DrawOffset.X)  and (wid < GridWidth) then
          Canvas.LineTo(x, y-1);
      end;
    end;
  end;
end;

end.
