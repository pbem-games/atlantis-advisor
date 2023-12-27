unit uTurnEvents;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataStructs, Resources, uInterface, MyStrings,
  uGameSubs, StdCtrls, Math, uHexMap, uBattle;

type
  TTurnEventsForm = class(TForm)
    Grid: TPowerGrid;
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLines, OpenedKeys: TStrings;
  public
    procedure Setup(ALines: TStrings);
  end;

var
  TurnEventsForm: TTurnEventsForm;

implementation

uses Main;

{$R *.dfm}

procedure TTurnEventsForm.Setup(ALines: TStrings);
var i, j, k: integer;
    s, s1, s2, token, ss, ss1, ss2, ttoken: string;
    Lines: TStrings;
    open: boolean;

  function UnitNum(s: string): integer;
  var st, en: integer;
  begin
    Result := 0;
    st := Pos('(', s);
    en := Pos(')', s);
    if (st > 0) and (en > 0) then
    try
      Result := StrToInt(Copy(s, st + 1, en - st - 1));
    except
    end;
  end;

  function DecodeLine(line: string; var token, s, s1, s2: string): integer;
  begin
    Result := 0;
    token := '';
    s := Line;
    if (Length(s) >= 3) and (s[1] = '!') and (s[3] in ['0'..'9']) then begin
      Result := StrToInt(s[3]);
      token := s[2];
      s := Copy(s, 5, Length(s));
    end;
    if Pos(':', s) > 0 then begin
      s1 := Copy(s, 1, Pos(':', s)-1);
      s2 := Copy(s, Pos(':', s)+2, Length(s));
    end
    else begin
      s1 := '';
      s2 := '';
    end;
  end;

  procedure AddRow(token, s: string; Data: pointer);
  var row: integer;
  begin
    row := Grid.RowCount;
    Grid.Cells[0, row] := token;
    Grid.Cells[1, row] := s;
    Grid.Rows[row].Data := Data;
    if token = 'H' then Grid.Rows[row].FontStyle := [fsBold];
  end;

begin
  Grid.RowCount := 0;
  FLines := ALines;
  Lines := TStringList.Create;
  Lines.Assign(ALines);

  // Sort events by priority
  for i := 0 to Lines.Count-1 do
    for j := i+1 to Lines.Count-1 do
      if (Pos('!', Lines[i]) = 1) and ((Pos('6', Lines[i]) = 3) or (Pos('7', Lines[i]) = 3))
        and (Pos('!', Lines[j]) = 1) and (Length(Lines[j]) > 3) and (Lines[j][3] = Lines[j][3])
        and (UnitNum(Lines[i]) > UnitNum(Lines[j])) then
        Lines.Exchange(i, j);

  // Add events to grid
  for j := 0 to 9 do begin
    i := -1;
    while i < Lines.Count-1 do begin
      Inc(i);
      if DecodeLine(Lines[i], token, s, s1, s2) <> j then Continue;

      // Grouping
      if (token = 'G') and (Pos(':', s) > 0) then begin
        open := OpenedKeys.IndexOf(s2) >= 0;
        if open then AddRow('-', s2, nil)
        else AddRow('+', s2, nil);
        k := i-1;
        while (k < Lines.Count - 1) do begin
          Inc(k);
          if DecodeLine(Lines[k], ttoken, ss, ss1, ss2) <> j then Continue;
          if (ttoken <> 'G') or (ss2 <> s2) then Continue;
          if open then AddRow('G', ss1, Lines.Objects[k]);
          Lines.Delete(k);
          Dec(k);
        end;
        Continue;
      end;

      AddRow(token, s, Lines.Objects[i]);
    end;
  end;
  Grid.Fixup;
  Lines.Free;
end;

procedure TTurnEventsForm.GridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var arow: integer;
begin
  with Grid do begin
    arow := MouseCell.Y;
    if (arow >= 0) and (arow < RowCount) and (Rows[arow].Data <> nil)
      and ((TObject(Rows[arow].Data).ClassType = TBattle)
      or (TObject(Rows[arow].Data).ClassType = TUnit)
      or (TObject(Rows[arow].Data).ClassType = TARegion)) then
      Cursor := crHandPoint
    else Cursor := crDefault;
  end;
end;

procedure TTurnEventsForm.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var AUnit: TUnit;
    Obj: TObject;
    i: integer;
    MousePos: TSmallPoint;
    C: TCoords;
    s0, s1: string;
begin
  C := Coords(0, 0, -1);
  AUnit := nil;
  Obj := TObject(Grid.ImgRows[Grid.MouseCell.Y].Data);
  if Obj <> nil then begin
    if Obj.ClassType = TBattle then begin
      C := TBattle(Obj).Region.Coords;
      // Open battle viewer
      if (ssShift in Shift) or (Button = mbRight) then begin
        MousePos := PointToSmallPoint(Point(X, Y));
        SendMessage(Grid.Handle, WM_LBUTTONUP, 0, Integer(MousePos));
        with TBattleForm.Create(Self, TBattle(Obj)) do begin
          ShowModal;
          Free;
        end;
        Grid.SetFocus;
        SendMessage(Grid.Handle, WM_LBUTTONUP, 0, Integer(MousePos));
        Exit;
      end;
    end
    else if Obj.ClassType = TUnit then begin
      AUnit := TUnit(Obj);
      if Grid.ImgCells[0, Grid.MouseCell.Y] = 'M' then begin
        C := AUnit.FinalCoords;
        if C.z = -1 then C := AUnit.Region.Coords;
      end
      else C := AUnit.Region.Coords;
    end
    else if Obj.ClassType = TARegion then C := TARegion(Obj).Coords;
  end
  else if Grid.MouseCell.X = 0 then begin
    s0 := Grid.ImgCells[0, Grid.MouseCell.Y];
    s1 := Grid.ImgCells[1, Grid.MouseCell.Y];
    if s0 = '+' then begin
      OpenedKeys.Add(s1);
      Setup(FLines);
    end
    else if (s0 = '-') and (OpenedKeys.IndexOf(s1) >= 0) then begin
      OpenedKeys.Delete(OpenedKeys.IndexOf(s1));
      Setup(FLines);
    end;
  end;

  if C.Z >= 0 then begin
    // Center on region
    GameConfig.WriteInteger('Map', 'Level', C.z);
    GameConfig.WriteInteger('Map', 'SelX_' + Map.Levels[C.z].Name, C.x);
    GameConfig.WriteInteger('Map', 'SelY_' + Map.Levels[C.z].Name, C.y);
    MainForm.HexMapSetup;
    // Select unit
    if AUnit <> nil then
      with MainForm.UnitGrid do begin
        i := RowCount-1;
        while (i >= 1) and (TUnit(ImgRows[i].Data).Num <> AUnit.Num) do Dec(i);
        if i >= 1 then Row := i;
      end;
  end;
  if ssDouble in Shift then Close;
end;

procedure TTurnEventsForm.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
var x, y: integer;
begin
  x := TxtRect.Left + 2;
  y := TxtRect.Top + 4;
  with TPowerGrid(Sender) do begin
    if ACol = 0 then begin
      if Rows[ARow].Data <> nil then begin
        // Region
        if TObject(Rows[ARow].Data).ClassType = TARegion then
          ResForm.IconList.Draw(Canvas, x, y, bmpError);
        // Battle
        if TObject(Rows[ARow].Data).ClassType = TBattle then
          ResForm.IconList.Draw(Canvas, x, y, bmpAdvance);
        // Unit
        if TObject(Rows[ARow].Data).ClassType = TUnit then
          if (ImgCells[ACol, ARow] = 'E') or (ImgCells[ACol, ARow] = 'M') then
            ResForm.IconList.Draw(Canvas, x, y, bmpError)
          else if (ImgCells[ACol, ARow] = 'G') then
            ResForm.IconList.Draw(Canvas, x + 5, y, bmpError)
          else if ImgCells[ACol, ARow] = 'S' then
            ResForm.IconList.Draw(Canvas, x, y, bmpScript)
          else if ImgCells[ACol, ARow] = 'W' then
            ResForm.IconList.Draw(Canvas, x, y, bmpMen)
          else ResForm.IconList.Draw(Canvas, x, y, bmpInfo);
        // SkillData
        if TObject(Rows[ARow].Data).ClassType = TSkillData then
          ResForm.IconList.Draw(Canvas, x, y, SkillIcon(Rows[ARow].Data));
        // ItemData
        if TObject(Rows[ARow].Data).ClassType = TItemData then
          DrawItemIcon(Canvas, x, y, Rows[ARow].Data);
        // StructData
        if TObject(Rows[ARow].Data).ClassType = TStructData then
          DrawStructIcon(Canvas, x, y, TStructData(Rows[ARow].Data), False);
      end
      // Default info icon, if not header
      else if ImgCells[ACol, ARow] = 'U' then
        ResForm.IconList.Draw(Canvas, x, y, bmpUnknownItem)
      else if ImgCells[ACol, ARow] = '+' then
        ResForm.IconList.Draw(Canvas, x, y, bmpListPlus)
      else if ImgCells[ACol, ARow] = '-' then
        ResForm.IconList.Draw(Canvas, x, y, bmpListMinus)
      else if ImgCells[ACol, ARow] <> 'H' then
        ResForm.IconList.Draw(Canvas, x, y, bmpInfo);
      TxtRect.Left := TxtRect.Right;
    end;
  end;
end;

procedure TTurnEventsForm.FormActivate(Sender: TObject);
begin
  Grid.SetFocus;
end;

procedure TTurnEventsForm.FormCreate(Sender: TObject);
begin
  OpenedKeys := TStringList.Create;
end;

procedure TTurnEventsForm.FormDestroy(Sender: TObject);
begin
  OpenedKeys.Free;
end;

end.
