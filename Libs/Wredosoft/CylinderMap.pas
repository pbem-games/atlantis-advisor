
 //  Hexagonal Cylindrical DrawGrid component
 //  (C) Wredosoft Software LTD

unit CylinderMap;

{$R cursors.res}

interface

uses Windows, Messages, Classes, Graphics, Forms, Controls, ExtCtrls, Math;

const
  KeyStep = 10;
  crHand = 1;

  dirN = 1;
  dirNE = 2;
  dirSE = 3;
  dirS = 4;
  dirSW = 5;
  dirNW = 6;

type
  TCylinderMapOptions = set of (hmGrid, hmShowSelection, hmDblClickCenter,
    hmRightBtnCenter, hmRightBtnDrag);
  TCylinderMapState = set of (msDblClick, msRedraw, msNoPaint);
  TCylinderMapDrawState = set of (dsSelected);
  THexParts = array[1..6] of boolean;

  TIterationHandler = procedure(HX, HY: integer) of object;

  TMapPaintEvent = procedure (Sender: TObject; ACanvas: TCanvas) of object;
  TDrawHexEvent = procedure (Sender: TObject; HX, HY: integer; ACanvas: TCanvas;
    CX, CY: integer; AState: TCylinderMapDrawState) of object;
  TDrawExtraEvent = procedure (Sender: TObject; HX, HY: integer; ACanvas: TCanvas;
    CX, CY: integer; AState: TCylinderMapDrawState) of object;
  TMoveMapEvent = procedure (Sender: TObject; X, Y: integer) of object;
  TSelectHexEvent = procedure (Sender: TObject; HX, HY: integer) of object;

  TObjectPoint = class
    X, Y: Longint;
    constructor Create(AX, AY: Longint);
  end;

  TCylinderMap = class(TCustomControl)
  private
    FOnBeforePaint: TMapPaintEvent;
    FOnAfterPaint: TNotifyEvent;
    FOnDrawHex: TDrawHexEvent;
    FOnDrawExtra: TDrawExtraEvent;
    FOnMoveMap: TMoveMapEvent;
    FOnSelectHex: TSelectHexEvent;

    FCellWidth: integer;
    FCellHeight: integer;
    FExtraSpan: TPoint; // Neibor hexes affected by cell extras
    FGridColor: TColor;
    FMapRect: TRect; // Visible rectangle on the global map image
    FMargin: integer;
    FOptions: TCylinderMapOptions;
    FRowCount, FColCount: integer;
    FSelected: TPoint;
    FSelectColor: TColor;
    FVisCells: TRect; // Cells currently visible (partially, maybe)
    FVisHexes: TRect; // Hexes currently visible
    FFirstOdd: boolean;
    LastX, LastY: integer;
    FMapState: TCylinderMapState;
    OffScreen: TBitmap;
    OldHexes: TRect;
    StoredCursor: TCursor;
    function Cyl(n: integer): integer;
    function GetCursor: TCursor;
    procedure IterateHexes(HexRect: TRect; Handler: TIterationHandler);
    procedure IHDrawNewHex(HX, HY: integer);
    procedure IHDrawNewCell(HX, HY: integer);
    procedure RecalcExtraSpan;
    procedure RecalcMapRect;
    procedure SetCellWidth(Value: integer);
    procedure SetCellHeight(Value: integer);
    procedure SetColCount(Value: integer);
    procedure SetCursor(Value: TCursor);
    procedure SetGridColor(Value: TColor);
    procedure SetHexSize(Value: integer);
    procedure SetMapRect(Value: TRect);
    procedure SetMargin(Value: integer);
    procedure SetOptions(Value: TCylinderMapOptions);
    procedure SetRowCount(Value: integer);
    procedure SetSelected(Value: TPoint);
    procedure SetSelectColor(Value: TColor);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetFirstOdd(const Value: boolean);
  protected
    procedure BeforePaint; dynamic;
    procedure AfterPaint; dynamic;
    function DoKeyDown(var Message: TWMKey): Boolean;
    procedure DrawHex(HX, HY: integer); dynamic;
    procedure DrawExtra(HX, HY: integer); dynamic;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function MouseToInnerHex(X, Y: integer): TPoint;
    procedure Paint; override;
    procedure SelectHex(HX, HY: integer); dynamic;
  public
    hs, hs25, hs50, hs75, hs43: integer;
    DrawOffset: TPoint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    procedure CalcOffCoords(HX,HY: integer; var CX,CY: integer);
    procedure CalcHexCoords(HX,HY: integer; var CX,CY: integer);
    procedure CalcHexPos(HX,HY: integer; var CX,CY: integer);
    procedure CalcRelHex(X, Y: integer; var HX, HY: real);
    function CellRect(HX,HY: integer): TRect;
    function CellVisible(HX, HY: integer): boolean;
    procedure Center(HX,HY: integer);
    property Cursor read GetCursor write SetCursor;
    function GridWidth: integer;
    function GridHeight: integer;
    procedure Hex(Canvas: TCanvas; CX,CY: integer);
    procedure MoveMap(X,Y: integer); dynamic;
    function MouseToHex(X, Y: integer): TPoint;
    property MapRect: TRect read FMapRect write SetMapRect;
    procedure PartialHex(Canvas: TCanvas; CX,CY: integer; Parts: THexParts);
    procedure Redraw;
    procedure RedrawCell(HX, HY: integer);
    property Selected: TPoint read FSelected write SetSelected;
    property MapState: TCylinderMapState read FMapState write FMapState;
    property VisCells: TRect read FVisCells;
    property VisHexes: TRect read FVisHexes;
  published
    property Align;
    property Color;
    property Constraints;
    property ColCount: integer read FColCount write SetColCount default 12;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FirstOdd: boolean read FFirstOdd write SetFirstOdd;
    property GridColor: TColor read FGridColor write SetGridColor default clBtnShadow;
    property Height;
    property HexSize: integer read hs write SetHexSize;
    property CellWidth: integer read FCellWidth write SetCellWidth;
    property CellHeight: integer read FCellHeight write SetCellHeight;
    property Margin: integer read FMargin write SetMargin;
    property Options: TCylinderMapOptions read FOptions write SetOptions
      default [hmGrid, hmShowSelection, hmDblClickCenter, hmRightBtnDrag];
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount: integer read FRowCount write SetRowCount default 12;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clRed;
    property ShowHint;
    property TabStop;
    property Visible;
    property Width;

    property OnBeforePaint: TMapPaintEvent read FOnBeforePaint write FOnBeforePaint;
    property OnAfterPaint: TNotifyEvent read FOnAfterPaint write FOnAfterPaint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawHex: TDrawHexEvent read FOnDrawHex write FOnDrawHex;
    property OnDrawExtra: TDrawExtraEvent read FOnDrawExtra write FOnDrawExtra;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectHex: TSelectHexEvent read FOnSelectHex write FOnSelectHex;
    property OnStartDrag;
    property OnMoveMap: TMoveMapEvent read FOnMoveMap write FOnMoveMap;
  end;

 procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TCylinderMap]);
end;

  { TCylinderMap }

constructor TCylinderMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed];
  MapState := MapState + [msRedraw];
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  OffScreen := TBitmap.Create;
  FRowCount := 12;
  FColCount := 12;
  FGridColor := clBtnShadow;
  FOptions := [hmGrid, hmShowSelection, hmDblClickCenter, hmRightBtnDrag];
  FSelectColor := clRed;
  FMargin := 15;
  StoredCursor := Cursor;
  HexSize := 48;
  FCellWidth := hs;
  FCellHeight := 2*hs43;
  SetBounds(0, 0, 300, 150);
end;

destructor TCylinderMap.Destroy;
begin
  OffScreen.Free;
  inherited Destroy;
end;

procedure TCylinderMap.BeforePaint;
begin
  if Assigned(FOnBeforePaint) then FOnBeforePaint(Self, OffScreen.Canvas);
end;

procedure TCylinderMap.AfterPaint;
begin
  if Assigned(FOnAfterPaint) then FOnAfterPaint(Self);
end;

{ Calculate coords of hex center on the offscreen bitmap }
procedure TCylinderMap.CalcOffCoords(HX,HY: integer; var CX,CY: integer);
begin
  CalcHexCoords(HX,HY,CX,CY);
  CX := CX + hs;
  CY := CY + hs43*2;
end;

{ Calculate coords of hex center on the control's canvas }
procedure TCylinderMap.CalcHexCoords(HX,HY: integer; var CX,CY: integer);
begin
  CalcHexPos(HX,HY,CX,CY);
  CX := CX - FMapRect.Left + DrawOffset.X;
  CY := CY - FMapRect.Top + DrawOffset.Y;
end;

{ Calculate position of hex center on the map }
procedure TCylinderMap.CalcHexPos(HX,HY: integer; var CX,CY: integer);
begin
  CX := HX * hs75 + hs50;
  CY := HY * hs43 + hs43 + FMargin;
end;

{ Calculate hex-relative map position }
procedure TCylinderMap.CalcRelHex(X, Y: integer; var HX, HY: real);
begin
  HX := (X - hs50) / hs75;
  HY := (Y - hs43 - FMargin) / hs43;
end;

{ Calculate cell rect on offscreen bmp from hex coords }
function TCylinderMap.CellRect(HX,HY: integer): TRect;
var cx,cy: integer;
begin
  CalcOffCoords(HX, HY, cx, cy);
  Result := Rect(cx - CellWidth div 2, cy - CellHeight div 2,
    cx + CellWidth div 2, cy + CellHeight div 2);
end;

function TCylinderMap.CellVisible(HX, HY: integer): boolean;
begin
  if (HX >= VisCells.Left) and (HX <= VisCells.Right) and
    (HY >= VisCells.Top) and (HY <= VisCells.Bottom) then
    Result := TRUE
  else Result := FALSE;
end;

procedure TCylinderMap.Center(HX,HY: integer);
var cx,cy: integer;
begin
  CalcHexPos(HX, HY, cx, cy);
  MoveMap(cx - Min((FMapRect.Right - FMapRect.Left), GridWidth) div 2,
    cy - Min((FMapRect.Bottom - FMapRect.Top), GridHeight) div 2);
end;

// Cylinder X coordinate correction
function TCylinderMap.Cyl(n: integer): integer;
begin
  if (n >= 0) and (n < ColCount) then Result := n
  else begin
    if n > 0 then Result := n mod ColCount
    else Result := n + ((n div ColCount)+1) * ColCount;
  end;
end;

procedure TCylinderMap.DblClick;
begin
  MapState := MapState + [msDblClick];
  inherited DblClick;
  if hmDblClickCenter in FOptions then Center(FSelected.X, FSelected.Y);
end;

function TCylinderMap.DoKeyDown(var Message: TWMKey): Boolean;
begin
  inherited DoKeyDown(Message);
  Result := FALSE;
end;

procedure TCylinderMap.DrawHex(HX, HY: integer);
var cx,cy: integer;
    State: TCylinderMapDrawState;
begin
  CalcOffCoords(HX,HY,cx,cy);
  if (Cyl(HX) = FSelected.X) and (HY = FSelected.Y) then State := [dsSelected]
  else State := [];
  if not (hmGrid in Options) then begin
    OffScreen.Canvas.Pen.Color := Color;
    Hex(OffScreen.Canvas, cx,cy);
  end;
  if Assigned(FOnDrawHex) then FOnDrawHex(Self, Cyl(HX), HY, OffScreen.Canvas,
    cx, cy, State);
  if hmGrid in Options then begin
    OffScreen.Canvas.Pen.Color := GridColor;
    Hex(OffScreen.Canvas, cx,cy);
  end;
end;

procedure TCylinderMap.DrawExtra(HX, HY: integer);
var cx,cy: integer;
    State: TCylinderMapDrawState;
begin
  CalcOffCoords(HX,HY,cx,cy);
  if (Cyl(HX) = FSelected.X) and (HY = FSelected.Y) then State := [dsSelected]
  else State := [];
  if (hmShowSelection in Options) and (dsSelected in State) then begin
    OffScreen.Canvas.Pen.Color := SelectColor;
    Hex(OffScreen.Canvas, cx,cy);
  end;
  if Assigned(FOnDrawExtra) then
    FOnDrawExtra(Self, Cyl(HX), HY, OffScreen.Canvas, cx, cy, State);
end;

function TCylinderMap.GetCursor: TCursor;
begin
  Result := inherited Cursor;
end;

function TCylinderMap.GridWidth: integer;
begin
  Result := ColCount*hs75;
end;

function TCylinderMap.GridHeight: integer;
begin
  Result := (RowCount+1)*hs43 + FMargin*2;
end;

procedure TCylinderMap.Hex(Canvas: TCanvas; CX,CY: Integer);
begin
  with Canvas do begin
    Polyline([Point(CX - hs50, CY),
      Point(CX - hs25, CY + hs43),
      Point(CX + hs25, CY + hs43),
      Point(CX + hs50, CY),
      Point(CX + hs25, CY - hs43),
      Point(CX - hs25, CY - hs43),
      Point(CX - hs50, CY)]);
  end;
end;

procedure TCylinderMap.IterateHexes(HexRect: TRect; Handler: TIterationHandler);
var x,y: integer;
begin
  // Iterate through all hexes
  y := HexRect.Top;
  while y <= HexRect.Bottom do begin
    x := HexRect.Left;
    if Odd(y) xor Odd(HexRect.Left) xor FFirstOdd then Inc(x);
    while x <= HexRect.Right do begin
      Handler(x, y);
      x := x + 2;
    end;
    y := y + 1;
  end;
end;

procedure TCylinderMap.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
  VK_UP:
    MoveMap(FMapRect.Left, FMapRect.Top - KeyStep);
  VK_DOWN:
    MoveMap(FMapRect.Left, FMapRect.Top + KeyStep);
  VK_LEFT:
    MoveMap(FMapRect.Left - KeyStep, FMapRect.Top);
  VK_RIGHT:
    MoveMap(FMapRect.Left + KeyStep, FMapRect.Top);
  VK_HOME:
    Center(FSelected.X, FSelected.Y);
  end;
end;

procedure TCylinderMap.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var MousePoint: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  SetFocus;
  if ssLeft in Shift then begin
    if not (msDblClick in MapState) then begin
      MousePoint := MouseToHex(X, Y);
      if (MousePoint.Y >= 0) and (MousePoint.Y < RowCount) then
        SelectHex(MousePoint.X, MousePoint.Y);
    end
    else MapState := MapState - [msDblClick];
  end;
  if ssRight in Shift then begin
    if hmRightBtnDrag in Options then begin
      LastX := X; LastY := Y;
      inherited Cursor := crHand;
    end;
    if hmRightBtnCenter in Options then begin
      MousePoint := MouseToHex(X, Y);
      Center(MousePoint.X, MousePoint.Y);
    end;
  end;
end;

procedure TCylinderMap.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if (ssRight in Shift) and (hmRightBtnDrag in Options) then begin
    MoveMap(FMapRect.Left + LastX-X, FMapRect.Top + LastY-Y);
    LastX := X; LastY := Y;
  end
  else inherited Cursor := StoredCursor;
end;

procedure TCylinderMap.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  inherited Cursor := StoredCursor;
end;

function TCylinderMap.MouseToInnerHex(X, Y: integer): TPoint;
var PosX, PosY, CellX, CellY: integer;
    Tangent, Disp: real;
begin
  X := X + FMapRect.Left - DrawOffset.X;
  Y := Y + FMapRect.Top - FMargin - DrawOffset.Y;
  CellX := X div hs75;  PosX := X mod hs75;
  CellY := Y div hs43;  PosY := Y mod hs43;

  if Odd(CellX + CellY) xor FirstOdd then begin
    Tangent := hs25 / hs43;
    Disp := 0;
  end
  else begin
    Tangent := - hs25 / hs43;
    Disp := hs25;
  end;
  if PosX < PosY * Tangent + Disp then Result.X := CellX - 1
  else Result.X := CellX;

  if Odd(Result.X) xor FirstOdd then begin
    if Odd(CellY) then Result.Y := CellY else Result.Y := CellY - 1;
  end
  else begin
    if Odd(CellY) then Result.Y := CellY - 1 else Result.Y := CellY;
  end;
end;

function TCylinderMap.MouseToHex(X, Y: integer): TPoint;
begin
  Result := MouseToInnerHex(X, Y);
  Result.X := Cyl(Result.X);
end;

procedure TCylinderMap.MoveMap(X,Y: integer);
begin
  MapRect := Rect(X, Y, X + FMapRect.Right - FMapRect.Left,
    Y + FMapRect.Bottom - FMapRect.Top);
  if Assigned(FOnMoveMap) then FOnMoveMap(Self,X,Y);
end;

procedure TCylinderMap.Paint;
begin
  if not (msNoPaint in MapState) then begin
    BeforePaint;
    if msRedraw in MapState then Redraw
    else Canvas.CopyRect(Rect(DrawOffset.X, DrawOffset.Y, Width - DrawOffset.X,
      Height - DrawOffset.Y), OffScreen.Canvas, Rect(hs + DrawOffset.X,
      hs43*2 + DrawOffset.Y, hs+Width - DrawOffset.X, hs43*2+Height -
      DrawOffset.Y));
    AfterPaint;
  end;
end;

procedure TCylinderMap.PartialHex(Canvas: TCanvas; CX,CY: Integer; Parts: THexParts);
begin
  with Canvas do begin
    if Parts[dirS] then
      begin MoveTo(CX - hs25, CY + hs43); LineTo(CX + hs25, CY + hs43); end;
    if Parts[dirSW] then
      begin MoveTo(CX - hs50, CY); LineTo(CX - hs25, CY + hs43); end;
    if Parts[dirSE] then
      begin MoveTo(CX + hs25, CY + hs43); LineTo(CX + hs50, CY); end;
    if Parts[dirN] then
      begin MoveTo(CX + hs25, CY - hs43); LineTo(CX - hs25, CY - hs43); end;
    if Parts[dirNW] then
      begin MoveTo(CX - hs25, CY - hs43); LineTo(CX - hs50, CY); end;
    if Parts[dirNE] then
      begin MoveTo(CX + hs50, CY); LineTo(CX + hs25, CY - hs43); end;
  end;
end;

procedure TCylinderMap.RecalcExtraSpan;
begin
  FExtraSpan.X := ((FCellWidth div 2) + hs50) div hs75;
  FExtraSpan.Y := (FCellHeight div 2) div hs43 + 1;
end;

procedure TCylinderMap.RecalcMapRect;
var hx,hy: real;
begin
  CalcRelHex(FMapRect.Left - hs50, FMapRect.Top - hs43, hx, hy);
  FVisHexes.TopLeft := Point(Ceil(hx), Ceil(Max(hy,0)));
  CalcRelHex(FMapRect.Right + hs50, FMapRect.Bottom + hs43, hx, hy);
  FVisHexes.BottomRight := Point(Floor(Min(hx, FVisHexes.Left+ColCount)),
    Floor(Min(hy,FRowCount-1)));

  FVisCells := Rect(FVisHexes.Left - FExtraSpan.X, FVisHexes.Top - FExtraSpan.Y,
    FVisHexes.Right + FExtraSpan.X, FVisHexes.Bottom + FExtraSpan.Y);

  if (FMapRect.Right - FMapRect.Left) <= GridWidth then DrawOffset.X := 0
  else DrawOffset.X := ((FMapRect.Right - FMapRect.Left) - GridWidth) div 2;
  if (FMapRect.Bottom - FMapRect.Top) <= GridHeight then DrawOffset.Y := 0
  else DrawOffset.Y := ((FMapRect.Bottom - FMapRect.Top) - GridHeight) div 2;

  OffScreen.Width := (FMapRect.Right - FMapRect.Left) + hs*2 + 1;
  OffScreen.Height := (FMapRect.Bottom - FMapRect.Top) + hs43*4 + 1;
end;

procedure TCylinderMap.Redraw;
begin
  MapState := MapState - [msRedraw];
 { Draw hexes on off-screen bitmap }
  OffScreen.Canvas.Brush.Color := Color;
  OffScreen.Canvas.FillRect(OffScreen.Canvas.ClipRect);
  IterateHexes(FVisHexes, DrawHex);
 { Draw extras }
  IterateHexes(FVisCells, DrawExtra);
  Paint;
end;

procedure TCylinderMap.RedrawCell(HX, HY: integer);
var Extras: TRect;
begin
  if CellVisible(HX,HY) then begin
   { Draw hex background }
    DrawHex(HX,HY);
   { Restore neibors' extras removed by drawn hex }
    Extras := Rect(HX - FExtraSpan.X, Max(HY-FExtraSpan.Y, 0),
      HX + FExtraSpan.X, Min(HY+FExtraSpan.Y, RowCount-1));
    IterateHexes(Extras, DrawExtra);
  end;
  Paint;
end;

procedure TCylinderMap.SelectHex(HX, HY: integer);
var OldSelect: TPoint;
begin
  OldSelect := FSelected;
  FSelected := Point(HX, HY);
  RedrawCell(OldSelect.X, OldSelect.Y);
  RedrawCell(OldSelect.X + ColCount, OldSelect.Y);
  RedrawCell(FSelected.X, FSelected.Y);
  RedrawCell(FSelected.X + ColCount, FSelected.Y);
  if Assigned(FOnSelectHex) then FOnSelectHex(Self, HX, HY);
end;

procedure TCylinderMap.SetCellWidth(Value: integer);
begin
  if Value >= hs then FCellWidth := Value;
  RecalcExtraSpan;
end;

procedure TCylinderMap.SetCellHeight(Value: integer);
begin
  if Value >= hs43*2 then FCellHeight := Value;
  RecalcExtraSpan;
end;

procedure TCylinderMap.SetColCount(Value: integer);
begin
  if Value > 0 then begin
    if Odd(Value) then Inc(Value);
    FColCount := Value;
    RecalcMapRect;
    MapState := MapState + [msRedraw];
    Invalidate;
  end;
end;

procedure TCylinderMap.SetCursor(Value: TCursor);
begin
  inherited Cursor := Value;
  StoredCursor := Value;
end;

procedure TCylinderMap.SetGridColor(Value: TColor);
begin
  FGridColor := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.SetHexSize(Value: integer);
begin
  hs := Value;
  hs25 := Value div 4;
  hs50 := Value div 2;
  hs75 := hs50 + hs25;
  hs43 := Round(Value * 0.433);
  RecalcExtraSpan;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.IHDrawNewHex(HX, HY: integer);
begin
  if not ((HX >= OldHexes.Left) and (HX <= OldHexes.Right)
    and (HY >= OldHexes.Top) and (HY <= OldHexes.Bottom)) then
      DrawHex(HX, HY);
end;

procedure TCylinderMap.IHDrawNewCell(HX, HY: integer);
begin
  if not ((HX >= OldHexes.Left + FExtraSpan.X) and (HX <= OldHexes.Right - FExtraSpan.X)
    and (HY >= OldHexes.Top + FExtraSpan.Y) and (HY <= OldHexes.Bottom - FExtraSpan.Y)) then
      DrawExtra(HX, HY);
end;

procedure TCylinderMap.SetMapRect(Value: TRect);
var TempScreen: TBitmap;
    X, Y: integer;
begin
  // Vertical correction
  Y := Min(Value.Top, GridHeight - (Value.Bottom - Value.Top));
  Y := Max(Y,0);
  Value.Bottom := Value.Bottom + Y - Value.Top;
  Value.Top := Y;
  // Move image on the offscreen bmp to specified position
  TempScreen := TBitmap.Create;
  with TempScreen do begin
    Width := OffScreen.Width;
    Height := OffScreen.Height;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Draw(FMapRect.Left-Value.Left, FMapRect.Top-Value.Top, OffScreen);
    OffScreen.Assign(TempScreen);
    TempScreen.Free;
  end;
  // Cylinder correction
  X := Value.Left;
  if (Value.Left >= GridWidth) then begin
    X := Value.Left - GridWidth;
    FVisHexes.Left := FVisHexes.Left - ColCount;
    FVisHexes.Right := FVisHexes.Right - ColCount;
  end;
  if (Value.Left < 0) then begin
    X := Value.Left + GridWidth;
    FVisHexes.Left := FVisHexes.Left + ColCount;
    FVisHexes.Right := FVisHexes.Right + ColCount;
  end;
  Value.Right := Value.Right + X - Value.Left;
  Value.Left := X;
  // Redraw hexes positioned on the newly opened margins
  OldHexes := FVisHexes;
  FMapRect := Value;
  RecalcMapRect;
  IterateHexes(FVisHexes, IHDrawNewHex);
  // Redraw extras
  IterateHexes(FVisCells, IHDrawNewCell);
  Paint;
end;

procedure TCylinderMap.SetMargin(Value: integer);
begin
  if Value > 0 then FMargin := Value;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.SetOptions(Value: TCylinderMapOptions);
begin
  if hmRightBtnCenter in Value then
    Value := Value - [hmRightBtnDrag, hmDblClickCenter];
  FOptions := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.SetRowCount(Value: integer);
begin
  if Value > 0 then FRowCount := Value;
  RecalcMapRect;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.SetSelected(Value: TPoint);
begin
  SelectHex(Value.X, Value.Y);
end;

procedure TCylinderMap.SetSelectColor(Value: TColor);
begin
  FSelectColor := Value;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

procedure TCylinderMap.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TCylinderMap.WMSize(var Message: TWMSize);
begin
  MapState := MapState + [msNoPaint];
  inherited;
  FMapRect.Right := FMapRect.Left + Width;
  FMapRect.Bottom := FMapRect.Top + Height;
  MoveMap(FMapRect.Left, FMapRect.Top);
  MapState := MapState + [msRedraw] - [msNoPaint];
end;

procedure TCylinderMap.SetFirstOdd(const Value: boolean);
begin
  FFirstOdd := Value;
  if Odd(Selected.X) xor Odd(Selected.Y) xor FFirstOdd then begin
    if Selected.Y < RowCount then Inc(FSelected.Y)
    else if Selected.X < ColCount then Inc(FSelected.X);
  end;
  MapState := MapState + [msRedraw];
  Invalidate;
end;

{ TObjectPoint }

constructor TObjectPoint.Create(AX, AY: Longint);
begin
  inherited Create;
  X := AX;
  Y := AY;
end;

end.
