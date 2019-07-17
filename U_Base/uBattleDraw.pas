unit uBattleDraw;

interface

uses
  SysUtils, Graphics, Types, DataStructs, Resources, MyStrings, Math,
  uSoldiers, Painter, ImgList;

const
  unit_dy = 32;     // distance between unit anchors in line
  unit_margin = 20; // minimal space between top of screen and first anchor
  subphases = 3;

var
  Battle: TBattle;
  SelUnit: TBattleUnit;
  SelRound: integer;
  HalfPhase: integer;
  DrawNames: boolean;
  TerIndex: integer;

  procedure SetupDrawArea(Painter: TPainter);
  function UnitInPos(X, Y: integer): TBattleUnit;
  procedure DrawBattleActions;
  procedure RedrawBattleRect;
  procedure ShiftImage(NewShift: integer);
  function Phase: integer;

implementation

const
  overlap_y = 10; // extra space occupied by monster triades
  overlap_x = 32;
  offscreen_margin = 32 + overlap_y;
  LineOffset: array[False..True] of integer = (85, 25); // front, behind
  clDamaged = $006060FF;

var
  APainter: TPainter;
  Offscreen: TBitmap;
  TopShift: integer;

procedure SetupDrawArea(Painter: TPainter);
begin
  APainter := Painter;
  Offscreen.Width := Painter.Width;
  Offscreen.Height := Painter.Height + offscreen_margin * 2;
  TopShift := 0;
end;

function Phase: integer;
begin
  if HalfPhase = -1 then Result := -1
  else Result := HalfPhase div subphases;
end;

procedure DrawBattleTerrain(Rect: TRect; TerIndex: integer);
const dy = 20; dx = 35; // hex picture dimensions
var x, y: integer;
    Bmp: TBitmap;
    R1: TRect;
    odder: boolean;
begin
  R1 := Rect;
  odder := Odd((Rect.Top + TopShift) div dy) xor Odd(Rect.Left div dx);
  Dec(Rect.Top, (Rect.Top + TopShift) mod dy);
  Dec(Rect.Left, Rect.Left mod dx);
  Bmp := TBitmap.Create;
  Bmp.Width := Rect.Right - Rect.Left;
  Bmp.Height := Rect.Bottom - Rect.Top;
  ResForm.Terrains.DrawingStyle := ImgList.dsNormal;
  y := -1;
  while (y * dy) < Bmp.Height + dy do begin
    if Odd(y) xor odder then x := -1
    else x := 0;
    while (x * dx) < Bmp.Width do begin
      ResForm.Terrains.Draw(Bmp.Canvas, x * dx - 1, y * dy - 1, TerIndex);
      Inc(x, 2);
    end;
    Inc(y);
  end;
  Offscreen.Canvas.CopyRect(R1, Bmp.Canvas, Bounds(R1.Left - Rect.Left,
    R1.Top - Rect.Top, R1.Right - R1.Left, R1.Bottom - R1.Top));
  Bmp.Free;
end;

function GetUnitPos(BUnit: TBattleUnit): TPoint;
var ARect: TRect;
    top_y, num_front, line_len, line_idx: integer;
    side: boolean;
begin
  side := BUnit.Side;
  if side = sideAttack then
    ARect := Rect(0, 0, APainter.Width div 2, APainter.Height)
  else ARect := Rect(APainter.Width div 2, 0, APainter.Width, APainter.Height);

  line_idx := Battle.Units[side].IndexOf(BUnit);
  num_front := 0;
  while (num_front < Battle.Units[side].Count)
    and not Battle.Units[side][num_front].Behind do Inc(num_front);

  if not BUnit.Behind then
    line_len := num_front
  else begin
    line_len := Battle.Units[Side].Count - num_front;
    Dec(line_idx, num_front);
  end;

  top_y := Max(unit_margin, (APainter.Height - line_len * unit_dy) div 2);

  if side = sideAttack then
    Result.X := ARect.Left + LineOffset[BUnit.Behind]
  else Result.X := ARect.Right - LineOffset[BUnit.Behind] - 32;
  Result.Y := top_y + line_idx * unit_dy;

  Inc(Result.Y, offscreen_margin - TopShift);
end;

function UnitInPos(X, Y: integer): TBattleUnit;
var i: integer;
    side: boolean;
    P: TPoint;
begin
  Result := nil;
  if Battle = nil then Exit;
  // Unit info
  side := sideAttack;
  while Result = nil do begin
    i := Battle.Units[side].Count-1;
    while (i >= 0) and (Result = nil) do begin
      P := GetUnitPos(Battle.Units[side][i]);
      if PtInRect(Bounds(P.X, P.Y - 32, 32, 32), Point(X, Y)) then
        Result := Battle.Units[side][i];
      Dec(i);
    end;
    if side = sideDefence then Break
    else side := not side;
  end;
end;

procedure DrawUnit(P: TPoint; U: TBattleUnit);
var j, x, y: integer;
    Trace: TTrace;
    Act: TBattleAction;
begin
  // Selection
  if U = SelUnit then begin
    Offscreen.Canvas.Brush.Color := clRed;
    Offscreen.Canvas.FrameRect(Bounds(P.X, P.Y, 32, 32));
  end;

  // Image
  Trace := TTrace.Create(U.Viewer.Picture);
  j := 0;
  repeat
    case j of
      1: begin
          if U.Side = sideAttack then x := P.X - 20 else x := P.X + 20;
          if Pos(' & ', Trace.Text) > 0 then y := P.Y - 10
          else y := P.Y;
         end;
      2: begin
          if U.Side = sideAttack then x := P.X - 20 else x := P.X + 20;
          y := P.Y + 10;
         end;
      else begin x := P.X; y := P.Y; end;
    end;

    // Move acting unit
    if (SelRound >= 0) and (Phase >= 0) then begin
      Act := Battle.Rounds[SelRound].Actions[Phase];
      if ((Act.ActionType <> raSideLoses) and (Act.BUnit = U))
        or ((Act.ActionType = raSideLoses)
        and (Act.BUnit.Side = U.Side) and (U.Damaged)) then begin
        if (U.Side = sideAttack) xor (Act.ActionType in [raTakeHits, raSideLoses]) then
          Inc(X, 4)
        else Dec(X, 4);
      end;
    end;

    DrawBattleImage(Offscreen.Canvas, x, y, Trace.Before(' & '),
      FactionColor(U.Faction), (P.X > APainter.Width div 2));
    Inc(j);
  until Trace.Ends or (j = 3);
  Trace.Free;

  // Amount of men
  if not U.Damaged then Offscreen.Canvas.Font.Color := clYellow
  else Offscreen.Canvas.Font.Color := clDamaged;
  if (j <= 1) or (U.Side = sideAttack) then
    DrawTranspText(Offscreen.Canvas, P.x + 27, P.y + 14,
      IntToStr(U.Items.Amount(IT_MAN + IT_MONSTER)), 20, True)
  else
    DrawTranspText(Offscreen.Canvas, P.x + 47, P.y + 14,
      IntToStr(U.Items.Amount(IT_MAN + IT_MONSTER)), 20, True);
  // Loses
  if U.Loses > 0 then DrawTranspText(Offscreen.Canvas, P.x + 27, P.y + 3,
    IntToStr(U.Loses) + '%', 20, True);

  // Name
  if DrawNames then begin
    x := Max(0, (60 - Offscreen.Canvas.TextWidth(U.Name)) div 2);
    DrawTranspText(Offscreen.Canvas, P.x - 15 + x, P.y + 23, U.Name, 60, True);
  end;
end;

procedure DrawAction(P: TPoint; Action: TBattleAction; Step: integer);
var code, fname: string;
    text_x, text_y, act_x: integer;
begin
  code := '';
  Offscreen.Canvas.Font.Color := clDamaged;
  text_y := P.Y + 6;
  if Action.BUnit.Side = sideAttack then begin
    text_x := P.X + 32;
    act_x := P.X;
  end
  else begin
    text_x := P.X - 18;
    act_x := P.X - 64;
  end;
  case Action.ActionType of
    raHeals: begin
      code := 'healing';
      Offscreen.Canvas.Font.Color := clLime;
    end;
    raCast: begin
      if Action.Special <> nil then code := Action.Special.Name
      else code := 'default';
      if Action.BUnit.Side = sideAttack then text_x := P.X + 78
      else text_x := P.X - 64;
    end;
    raTakeHits: begin
      code := 'hit';
    end;
    raSideLoses: begin
      code := 'hit';
      if Action.BUnit.Side = sideAttack then
        text_x := APainter.Width div 2 - 12
      else text_x := APainter.Width div 2 + 12;
      text_y := APainter.Height div 2 - 10 + offscreen_margin;
    end;
  end;

  // Image
  if code <> '' then begin
    code := code + ' ' + IntToStr(Step + 1);
    fname := BattleIni.ReadString('Spells', code, '');
    if fname = '' then fname := BattleIni.ReadString('Spells',
      'default' + ' ' + IntToStr(Step + 1), '');
    if fname <> '' then DrawFileFlipped(Offscreen.Canvas, act_x, P.Y,
      BaseDir + BattleFolder + fname, (Action.BUnit.Side = sideDefence));
  end;

  // Power
  if Step >= (3 - subphases) then
    DrawTranspText(Offscreen.Canvas, text_x, text_y, IntToStr(Action.Power),
      18, True);
end;

procedure DrawBattleUnits(Rect: TRect);
var i: integer;
    side: boolean;
    P: TPoint;
begin
  Offscreen.Canvas.Font.Name := 'Small Fonts';
  Offscreen.Canvas.Font.Size := 6;
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      P := GetUnitPos(Battle.Units[side][i]);
      if (P.Y > Rect.Top) and (P.Y + 32 < Rect.Bottom) then
        DrawUnit(P, Battle.Units[side][i]);
    end;
end;

procedure DrawUnitsAround(ARect: TRect);
begin
  ARect := Rect(ARect.Left - 32, ARect.Top - (32 + overlap_y),
    ARect.Right + 32, ARect.Bottom + (32 + overlap_y));
  if ARect.Left > APainter.Width div 2 then Dec(ARect.Left, overlap_x);
  if ARect.Right < APainter.Width div 2 then Inc(ARect.Right, overlap_x);
  DrawBattleUnits(ARect);
end;

// Copy offscreen bitmap to painter
procedure UpdateRect(ARect: TRect);
var DR: TRect;
begin
  DR := ARect;
  OffsetRect(DR, 0, -offscreen_margin);
  APainter.Bitmap.Canvas.CopyRect(DR, Offscreen.Canvas, ARect);
  APainter.Invalidate;
end;

procedure DrawBattleActions;
var i: integer;
    P: TPoint;
    ARect: TRect;
    Action: TBattleAction;
    U: TBattleUnit;

  function ActionRect(Action: TBattleAction): TRect;
  begin
    if Action.BUnit.Side = sideAttack then
      Result := Bounds(P.X - overlap_x, P.Y - overlap_y, 96 + overlap_x,
      32 + overlap_y * 2)
    else Result := Bounds(P.X - 64, P.Y - overlap_y, 96 + overlap_x,
      32 + overlap_y * 2);
  end;

begin
  if Battle = nil then Exit;

  // Draw background for actions
  for i := Max(0, Phase-1) to Phase do begin
    Action := Battle.Rounds[SelRound].Actions[i];
    if Action.ActionType = raSideLoses then
      RedrawBattleRect
    else begin
      P := GetUnitPos(Action.BUnit);
      if not ((P.Y > 0) and (P.Y + 32 < Offscreen.Height)) then
        Continue;
      ARect := ActionRect(Action);
      DrawBattleTerrain(ARect, TerIndex);
      DrawUnitsAround(ARect);
      UpdateRect(ARect);
    end;
  end;

  // Draw actions
  Offscreen.Canvas.Font.Name := 'MS Sans Serif';
  Offscreen.Canvas.Font.Size := 8;
  Action := Battle.Rounds[SelRound].Actions[HalfPhase div subphases];

  if (Action.ActionType <> raSideLoses) then begin
    P := GetUnitPos(Action.BUnit);
    if not ((P.Y > 0) and (P.Y + 32 < Offscreen.Height)) then begin
      if HalfPhase mod SubPhases <> 0 then Exit;
      ShiftImage(Max(0, TopShift + P.Y - (APainter.Height - 32) div 2));
      P := GetUnitPos(Action.BUnit);
    end;
    ARect := ActionRect(Action);
    DrawAction(P, Action, Abs(HalfPhase mod SubPhases));
    UpdateRect(ARect);
  end
  else begin
    for i := 0 to Battle.Units[Action.BUnit.Side].Count-1 do begin
      U := Battle.Units[Action.BUnit.Side][i];
      if not U.Damaged then Continue;
      P := GetUnitPos(U);
      if not ((P.Y > 0) and (P.Y + 32 < Offscreen.Height)) then Continue;
      DrawAction(P, Action, Abs(HalfPhase mod SubPhases));
    end;
    UpdateRect(Bounds(0, 0, Offscreen.Width, Offscreen.Height));
  end;
end;

procedure RedrawBattleRect;
var ARect: TRect;
begin
  if Battle = nil then Exit;
  ARect := Bounds(0, 0, Offscreen.Width, Offscreen.Height);
  DrawBattleTerrain(ARect, TerIndex);
  DrawBattleUnits(ARect);
  UpdateRect(ARect);
end;

procedure ShiftImage(NewShift: integer);
var dy: integer;
    FullR, DR, NewR: TRect;
begin
  dy := NewShift - TopShift;
  TopShift := NewShift;
  FullR := Bounds(0, 0, Offscreen.Width, Offscreen.Height);
  DR := FullR;
  // Copy shifted area
  OffsetRect(DR, 0, -dy);
  Offscreen.Canvas.CopyRect(DR, Offscreen.Canvas, FullR);
  // Draw new area
  if dy > 0 then NewR := Rect(0, DR.Bottom, Offscreen.Width, Offscreen.Height)
  else NewR := Rect(0, 0, Offscreen.Width, DR.Top);
  DrawBattleTerrain(NewR, TerIndex);
  DrawUnitsAround(NewR);
  UpdateRect(FullR);
end;


initialization
  Offscreen := TBitmap.Create;

finalization
  Offscreen.Free;

end.
