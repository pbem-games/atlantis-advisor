unit uRoute;

{$MODE Delphi}

interface

uses
  SysUtils, Windows, Classes, Math, DataStructs, MyStrings, uKeys,
  uGameSubs;

type
  TRoute = class
    Moves: TCoordArray;
    Stops: TIntArray;
    Position: integer;
    Order: string;
    OneWay: boolean;
    Remaining: string;
    function Empty: boolean;
    function Stop(Idx: integer): boolean;
    procedure Clear;
  end;

  procedure RunAllRoutes(ATurn: TTurn);
  procedure RunRoute(U: TUnit);
  procedure ReadRoute(U: TUnit; Route: TRoute);
  procedure SetRoute(U: TUnit; Route: TRoute);

implementation

uses
  uVisualOrders;

{ TRoute }

procedure TRoute.Clear;
begin
  SetLength(Moves, 0);
  SetLength(Stops, 0);
  Position := 0;
  Order := '';
  OneWay := False;
  Remaining := '';
end;

function TRoute.Empty: boolean;
begin
  Result := (Length(Moves) <= 1);
end;

function TRoute.Stop(Idx: integer): boolean;
var i: integer;
begin
  if (Idx = 0) or (Idx >= High(Moves)) then Result := True
  else begin
    i := Length(Stops)-1;
    while (i >= 0) and (Stops[i] <> Idx) do Dec(i);
    Result := (i >= 0);
  end;
end;


procedure ReadRoute(U: TUnit; Route: TRoute);
var i, struct, order_idx: integer;
    Trace: TTrace;
    C: TCoords;
    order, s: string;
    R: TRegion;
    end_route: boolean;

  procedure HandleError(Msg: string);
  begin
    Route.Clear;
    U.Orders[order_idx] := order + '; ' + Msg;
  end;

begin
  i := U.Orders.Count-1;
  while (i >= 0) and (U.Order(i) <> '@;route') do Dec(i);
  if i < 0 then Exit;

  // Route order found
  order_idx := i;
  order := '@;' + Uncomment(Copy(Trim(U.Orders[i]), 3, Length(U.Orders[i])));
  Trace := TTrace.Create(order);
  Trace.Before('@;route');
  Trace.SkipSpaces;

  try
    // @;route [oneway] [move] (0,0[,underworld]) N NW N 1 in N, S 2 in SE S[: 3]
    if Pos(':', Trace.Text) > 0 then
      Route.Position := StrToInt(Trace.Backwards(':'));

    // Modifiers
    if Pos('oneway', Trace.Text) = 1 then begin
      Route.OneWay := True;
      Trace.Word;
      Trace.SkipSpaces;
    end;
    Route.Order := 'move';
    if Pos('(', Trace.Text) <> 1 then
      Route.Order := LowerCase(Trace.Word);

    // Initial coords
    Trace.Before('(');
    C.x := StrToInt(Trace.Before(','));
    C.y := Trace.Num;
    if Pos(',', Trace.Text) = 1 then begin
      Inc(Trace.StPos);
      C.z := Map.Levels.NumOf(Trace.Before(')'));
    end
    else begin
      C.z := Map.Levels.NumOf(Keys[s_Surface]);
      Trace.Before(')');
    end;
    if C.z < 0 then raise Exception.Create('Incorrect syntax');
    AddCoords(Route.Moves, C);

    // Way
    struct := 0;
    while not Trace.Ends do begin
      // Stop
      if Pos(',', Trace.Text) = 1 then begin
        AddInt(Route.Stops, High(Route.Moves));
        Inc(Trace.StPos);
      end;

      Trace.SkipSpaces;
      s := UpperCase(Trace.Word);

      // IN
      if s = 'IN' then begin
        if struct = 0 then raise Exception.Create('Wrong MOVE IN');
        if Route.Order = 'sail' then
          raise Exception.Create('Can''t MOVE IN when sailing');
        end_route := True;
        R := Map.Region(C.x, C.y, C.z);
        if (R <> nil) and R.FullData then begin
          i := R.Structs.Count-1;
          while (i >= 0) and (R.Structs[i].Num <> struct) do Dec(i);
          if (i >= 0) and Test(R.Structs[i].Data.Flags, ST_SHAFT)
            and R.Structs[i].HasExit then begin
            end_route := False;
            C := R.Structs[i].Passage;
            AddCoords(Route.Moves, C);
          end;
        end;
        if end_route then begin
          Route.Remaining := IntToStr(struct) + ' IN';
          Break;
        end;
        Continue;
      end;

      // Object number
      if IsNumber(s) then begin
        if Route.Order = 'sail' then
          raise Exception.Create('Can''t enter object when sailing');
        struct := StrToInt(s);
        Continue;
      end;

      // Direction
      i := 6;
      while (i >= 1) and (GetKey(s_N, i - 1) <> s) do Dec(i);
      if i = 0 then Exception.Create('Incorrect syntax');
      C := CoordsInDir(C, i);
      AddCoords(Route.Moves, C);
    end;

    Route.Remaining := Route.Remaining + Trace.Text;

  except
    on E: EConvertError do HandleError('Incorrect syntax');
    on E: ENaN do HandleError('Incorrect syntax');
    on E: Exception do HandleError(E.Message);
  end;

  Trace.Free;
end;

procedure ClearRoutes(U: TUnit);
var i: integer;
begin
  i := 0;
  while i < U.Orders.Count do
    if U.Order(i) = '@;route' then U.Orders.Delete(i)
    else Inc(i);
end;

procedure SetRoute(U: TUnit; Route: TRoute);
var i, j: integer;
    s: string;
    C: TCoords;
    Shaft: TStruct;
    Trace: TTrace;
begin
  ClearRoutes(U);

  // Add route
  if Route.Empty then Exit;
  s := '@;route ';
  if Route.OneWay then s := s + 'oneway ';
  if Route.Order <> 'move' then s := s + Route.Order + ' ';

  C := Route.Moves[0];
  s := s + '(' + IntToStr(C.x) + ',' + IntToStr(C.y);
  if Map.Levels[C.z].Name <> Keys[s_Surface] then
    s := s + ',' + Map.Levels[C.z].Name;
  s := s + ')';

  for i := 1 to Length(Route.Moves)-1 do begin
    s := s + ' ';
    if Route.Moves[i].z = C.z then begin
      j := DirectionFromTo(C, Route.Moves[i]);
      if j < 1 then Break;
      s := s + GetKey(s_N, j - 1);
    end
    else begin
      Shaft := FindShaft(C, Route.Moves[i]);
      if Shaft = nil then Break;
      s := s + IntToStr(Shaft.Num) + ' IN';
    end;
    C := Route.Moves[i];
    if (i < High(Route.Moves)) and Route.Stop(i) then s := s + ',';
  end;
  Trace := TTrace.Create(Route.Remaining);
  s := Trim(s + ' ' + Trace.Before(','));
  Trace.Free;

  if Route.Position > 0 then s := s + ': ' + IntToStr(Route.Position);
  if Route.Position = High(Route.Moves) then s := s + '; Completed';

  U.Orders.Add(s);
end;

procedure RunRoute(U: TUnit);
var i, mt, mp, dir: integer;
    swim, complete: boolean;
    C: TCoords;
    Shaft: TStruct;
    Route: TRoute;
begin
  Route := TRoute.Create;
  ReadRoute(U, Route);
  if Route.Empty then Exit;

  // Determine unit's position on route, from last stop
  i := Route.Position;
  while (i < Length(Route.Moves)) and not EqualCoords(Route.Moves[i],
    U.Region.Coords) do Inc(i);
  if i = Length(Route.Moves) then begin
    // If not found, search from beginning
    i := 0;
    while (i < Route.Position) and not EqualCoords(Route.Moves[i],
      U.Region.Coords) do Inc(i);
    if i = Route.Position then Exit; // unit not on route
  end;
  Route.Position := i;
  ClearUnitMoves(U);

  if Route.Position >= High(Route.Moves) then begin
    // Final point reached
    if Route.OneWay then begin
      Route.Free;
      ClearRoutes(U);
      Exit;
    end;
    if not EqualCoords(Route.Moves[High(Route.Moves)], Route.Moves[0]) then begin
      SetRoute(U, Route);
      Route.Free;
      Exit;
    end;
    Route.Position := 0;
  end;

  // Add moves
  mt := MovementType(U);
  if (mt <> mtNone) or (Route.Order = 'sail') then begin
    swim := (Route.Order = 'sail') or CanSwim(U);
    if Route.Order = 'sail' then mp := 4
    else mp := MovesLeft(U);

    complete := True;
    C := Route.Moves[Route.Position];
    for i := Route.Position + 1 to Length(Route.Moves) - 1 do begin
      Dec(mp, EnterCost(mt, Route.Moves[i], C, swim));
      if mp < 0 then Break;
      if (C.z = Route.Moves[i].z) then begin
        // Move in direction
        dir := DirectionFromTo(C, Route.Moves[i]);
        if not MovableDir(C, dir, (Route.Order = 'sail'), mt, swim, U.Struct) then begin
          complete := False;
          Break;
        end;
        AddOrderTo(U, Route.Order + ' ' + GetKey(s_N, dir - 1), False);
      end
      else begin
        // Move through shaft
        Shaft := FindShaft(C, Route.Moves[i]);
        if Shaft = nil then begin
          complete := False;
          Break;
        end;
        AddOrderTo(U, Route.Order + ' ' + IntToStr(Shaft.Num) + ' IN', False);
      end;
      C := Route.Moves[i];
      if Route.Stop(i) then Break;
    end;
    // If there is movepoints left, add remaining orders
    if complete and (mp > 0) then begin
      AddOrderTo(U, Route.Order + ' ' + Route.Remaining, False);
    end;
  end;

  // Write modified route back
  SetRoute(U, Route);
  Route.Free;
end;

procedure RunAllRoutes(ATurn: TTurn);
var i, j: integer;
    R: TRegion;
begin
  for i := 0 to ATurn.Regions.Count-1 do begin
    R := ATurn.Regions[i];
    if R.PlayerTroop = nil then Continue;
    for j := 0 to R.PlayerTroop.Units.Count-1 do
      RunRoute(R.PlayerTroop.Units[j]);
  end;
end;

end.
