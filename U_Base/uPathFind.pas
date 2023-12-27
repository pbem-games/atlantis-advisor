unit uPathFind;

{$MODE Delphi}

interface

uses
  SysUtils, LCLIntf, LCLType, LMessages, Classes, DataStructs, uGameSubs, Math, uKeys;

  function PathFind(U: TUnit; C: TCoords; Sail: boolean): string;

implementation

const
  MaxDiff = 1.5;

function VectorLen(C1, C2: TCoords): real;
begin
  Result := Sqrt(Sqr(C1.x - C2.x) + Sqr(C1.y - C2.y));
end;

function VectorDir(C1, C2: TCoords): integer;
var wid, dx, dy: integer;
    ns: boolean;
begin
  dx := C2.x - C1.x;
  wid := Map.Levels[C1.z].Bounds.Right - Map.Levels[C1.z].Bounds.Left;
  if (dx > wid / 2) then dx := dx + wid;
  if (dx < -wid / 2) then dx := dx - wid;
  dy := C2.y - C1.y;
  ns := Abs(dx) < Abs(dy / 2);
  if dy < 0 then begin
    if ns then Result := dirN
    else if dx > 0 then Result := dirNE
    else Result := dirNW;
  end
  else begin
    if ns then Result := dirS
    else if dx > 0 then Result := dirSE
    else Result := dirSW;
  end;
end;

function SideDir(dir: integer; shift: integer): integer;
begin
  Result := dir + shift;
  if Result < 1 then Inc(Result, 6);
  if Result > 6 then Dec(Result, 6);
end;

function IsWay(C: TCoords; dir: integer; swim: boolean; var A: TCoordArray): boolean;
var i: integer;
begin
  Result := MovableDir(C, dir, False, mtWalk, swim, nil);
  if not Result then Exit;
  i := High(A);
  while (i >= 0) and not EqualCoords(A[i], CoordsInDir(C, dir)) do Dec(i);
  Result := (i < 0);
end;

function PathFind(U: TUnit; C: TCoords; Sail: boolean): string;
var C1: TCoords;
    w1, w2: boolean;
    i, dir, apos: integer;
    swim: boolean;
    s: string;
    A: TCoordArray;
begin
  Result := '';
  SetLength(A, 0);
  apos := -1;
  s := '';
  C1 := U.Region.Coords;
  swim := CanSwim(U);
  while not EqualCoords(C1, C) do begin
    // Get straight direction to target
    dir := VectorDir(C1, C);

    // Check if unit can move there, find other way
    i := 0;
    if not IsWay(C1, dir, swim, A) then begin
      while Abs(i) < 3 do begin
        Inc(i);
        w1 := IsWay(C1, SideDir(dir, i), swim, A);
        w2 := IsWay(C1, SideDir(dir, -i), swim, A);
        if w1 and not w2 then begin
          dir := SideDir(dir, i);
          Break;
        end
        else if w2 and not w1 then begin
          dir := SideDir(dir, -i);
          Break;
        end
        else if w1 and w2 then begin
          if VectorLen(CoordsInDir(C1, SideDir(dir, i)), C) >
            VectorLen(CoordsInDir(C1, SideDir(dir, i)), C) then
            dir := SideDir(dir, i)
          else dir := SideDir(dir, -i);
        end;
      end;
      if Abs(i) >= 3 then begin
        if EqualCoords(C1, U.Region.Coords) then Exit
        else begin
          dir := 0;
          if apos = -1 then apos := High(A)-1
          else if EqualCoords(A[High(A)], A[apos]) then Dec(apos);
          if apos = -1 then Exit;
          C1 := A[apos];
          s := Copy(s, 1, Length(s)-2);
        end;
      end;
    end;
    if dir = 0 then Continue;

    // Move
    AddCoords(A, C1);
    C1 := CoordsInDir(C1, dir);
    s := s + ' ' + GetDir(dir);
  end;
  Result := ' ' + Trim(s);
end;

end.
