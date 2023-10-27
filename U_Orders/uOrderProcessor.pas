unit uOrderProcessor;

interface

uses
  SysUtils, DataStructs, MyStrings, uGameSubs, uKeys, Math, Windows, Classes,
  uAvatars, uScript, uOrders, Resources;

var
  ThreadTerminate: TNotifyEvent;
  ParseErrors: TStrings;

  // Process orders for region; if region = nil, process all orders
  // Before resuming and processing, execute Script for current unit
  procedure DoProcessOrders(ARegion: TRegion; AScript: integer; Args: string;
    AStartup: boolean);
  procedure StopProcessOrders;


implementation

type
  TProcessor = procedure (AUnit: TUnit; s: string; var Line: integer);

  TProcessThread = class(TThread)
  private
    procedure DoManualScript;
    procedure DoOrder(AUnit: TUnit; Order: string; var Line: integer;
      Processor: TProcessor);
    procedure DoOrders(R: TRegion; Order: string; Processor: TProcessor);
    procedure DoScripts(R: TRegion; Order: string);
    procedure DoGlobalOrders(R: TRegion);
    procedure ProcessRegion(R: TRegion; RecreateRegion: boolean);
    procedure ProcessAllRegions;
  protected
    Region: TRegion;
    Script: integer;
    ScriptArgs: string;
    Startup: boolean;
    UnitNum: integer;
    procedure Execute; override;
  public
    function IsTerminated: boolean;
  end;

  TTerminator = class
    procedure FreeThread(Sender: TObject);
  end;

var
  ProcessThread: TProcessThread;
  Terminator: TTerminator;

procedure TProcessThread.DoOrder(AUnit: TUnit; Order: string; var Line:
  integer; Processor: TProcessor);
var args, msg: string;
begin
  Order := Trim(Order);
  if Pos('@;;', Order) = 1 then args := Copy(Order, 4, Length(Order))
  else if Pos(' ', Order) = 0 then args := ''
  else args := Copy(Order, Pos(' ', Order) + 1, Length(Order));
  try
    Processor(AUnit, args, Line);
  except
    on E: EParseError do begin
      msg := ';. ' + E.Message;
      if (Line >= 0) and (AUnit.Orders.Count > Line) and (Order = AUnit.Orders[Line]) then
        // Add comment to order
        AUnit.Orders[Line] := Uncomment(AUnit.Orders[Line]) + msg
      else
        // Add comment as first line
        if AUnit.Orders.IndexOf(msg) < 0 then begin
          AUnit.Orders.Insert(0, msg);
          Inc(Line);
        end;
      ParseErrors.AddObject('!E4 ' + AUnit.Name + ' (' +
        AUnit.NumStr + '): ' + E.Message, AUnit);
    end;
  end;
end;

// Process one type of orders for all units in region
procedure TProcessThread.DoOrders(R: TRegion; Order: string; Processor: TProcessor);
var i, k: integer;
    AUnit: TUnit;
begin
  i := 0;
  while (i < R.PlayerTroop.Units.Count) and not Terminated do begin
    AUnit := R.PlayerTroop.Units[i];
    // Run scripts
    RunScripts(AUnit, Order, ParseErrors);

    // Find orders
    k := 0;
    // TAX order may be fired from unit flag
    if AUnit.Flags[flgTax] and (Order = 'tax') then begin
      k := -1;
      DoOrder(AUnit, 'tax', k, Processor);
    end
    else begin
      while k < AUnit.Orders.Count do begin
        // Skip TURN construction
        while AUnit.Order(k) = 'turn' do begin
          repeat
            Inc(k);
          until (AUnit.Order(k) = 'endturn') or (k >= AUnit.Orders.Count);

          Inc(k);
        end;

        // Check if we found given order
        if AUnit.Order(k) = Order then
          if Order = '@;warning' then
            ParseErrors.AddObject('!W2 ' + AUnit.Name + ' (' + AUnit.NumStr +
              '): ' + Copy(AUnit.Orders[k], Pos(Order, AUnit.Orders[k]) +
              Length(Order) + 1, Length(AUnit.Orders[k])), AUnit)
          else
            DoOrder(AUnit, AUnit.Orders[k], k, Processor);

        Inc(k);
      end;
    end;

    Inc(i);
  end;
end;

procedure TProcessThread.DoScripts(R: TRegion; Order: string);
var i: integer;
begin
  for i := 0 to R.PlayerTroop.Units.Count-1 do
    RunScripts(R.PlayerTroop.Units[i], Order, ParseErrors);
end;


// Execute global orders for all regions except current before region processing
procedure TProcessThread.DoGlobalOrders(R: TRegion);
var i: integer;
    Fac: TFaction;
begin
  // Reset faction attitudes
  for i := 0 to VTurn.Factions.Count-1 do begin
    Fac := FindFaction(VTurn.Factions[i].Num);
    if Fac <> nil then VTurn.Factions[i].Attitude := Fac.Attitude;
  end;
  // Reset unclaimed silver
  VTurn.Unclaimed := Turn.Unclaimed;
  // Execute global orders for all regions
  for i := 0 to VTurn.Regions.Count-1 do
    if (VTurn.Regions[i] <> R) and (VTurn.Regions[i].PlayerTroop <> nil) then begin
      DoOrders(VTurn.Regions[i], 'declare', DoDeclare);
      DoOrders(VTurn.Regions[i], 'claim', DoClaimRepeat);
    end;
end;

// Process orders for region
procedure TProcessThread.ProcessRegion(R: TRegion; RecreateRegion: boolean);
var i, k: integer;
    U: TUnit;
    tempItems: TItemList;

  procedure AddToMaint(C: TCoords);
  var j: integer;
  begin
    j := High(MaintRegions);
    while (j >= 0) and not EqualCoords(MaintRegions[j], C) do
      Dec(j);
    if j < 0 then AddCoords(MaintRegions, C);
  end;

begin
  // Ensure that the region has player units
  if R.PlayerTroop = nil then Exit;

  if RecreateRegion then begin
    SetLength(MaintRegions, 0);
    // Clear parse errors in region units
    if R.PlayerTroop <> nil then
      for i := 0 to R.PlayerTroop.Units.Count-1 do begin
        U := R.PlayerTroop.Units[i];
        ClearErrorComments(U.Orders);
        while ParseErrors.IndexOfObject(U) >= 0 do
          ParseErrors.Delete(ParseErrors.IndexOfObject(U));
      end;
    while ParseErrors.IndexOfObject(R) >= 0 do
      ParseErrors.Delete(ParseErrors.IndexOfObject(R));
    while ParseErrors.IndexOfObject(nil) >= 0 do
      ParseErrors.Delete(ParseErrors.IndexOfObject(nil));
    // Recreate region (for single-region processing)
    DoGlobalOrders(R);
    Game.RecreateVRegion(R.Coords);
  end;

  AddToMaint(R.Coords);
  TaxUnits := TUnitList.Create;
  PillageUnits := TUnitList.Create;
  WorkUnits := TUnitList.Create;
  EntertainUnits := TUnitList.Create;

  // Instants
  if Startup then DoScripts(R, 'startup');
  DoScripts(R, '');
  DoOrders(R, 'form',       DoForm);
  DoOrders(R, 'autotax',    DoFlag);
  DoOrders(R, 'avoid',      DoFlag);
  DoOrders(R, 'behind',     DoFlag);
  DoOrders(R, 'claim',      DoClaim);
  DoOrders(R, 'combat',     DoCombat);
  DoOrders(R, 'consume',    DoExtFlag);
  DoOrders(R, 'declare',    DoDeclare);
  DoOrders(R, 'describe',   DoDescribe);
  DoOrders(R, '@;;',        DoLocal);
  DoOrders(R, 'faction',    DoFaction);
  DoOrders(R, 'guard',      DoFlag);
  DoOrders(R, 'hold',       DoFlag);
  DoOrders(R, 'name',       DoName);
  DoOrders(R, 'noaid',      DoFlag);
  DoOrders(R, 'nocross',    DoFlag);
  DoOrders(R, 'share',      DoFlag);
  DoOrders(R, 'reveal',     DoExtFlag);
  DoOrders(R, 'spoils',     DoExtFlag);
  DoOrders(R, 'leave',      DoLeave);
  DoOrders(R, 'enter',      DoEnter);
  DoOrders(R, 'promote',    DoPromote);
  DoOrders(R, 'evict',      DoEvict);
  DoOrders(R, 'attack',     DoAttack);
  DoOrders(R, 'steal',      DoSteal);
  DoOrders(R, 'destroy',    DoDestroy);
  DoOrders(R, 'give',       DoGive);
  DoOrders(R, 'pillage',    DoPillage);
  ResolveTaxes(R, PillageUnits, 2, 4);
  DoOrders(R, 'tax',        DoTax);
  ResolveTaxes(R, TaxUnits, 1, 1);
  DoOrders(R, 'cast',       DoCast);
  DoOrders(R, 'sell',       DoSell);
  DoOrders(R, 'buy',        DoBuy);
  DoOrders(R, 'forget',     DoForget);
  DoScripts(R, 'monthlong');

  // Move orders
  DoOrders(R, 'sail',       DoMove);
  DoOrders(R, 'move',       DoMove);
  DoOrders(R, 'advance',    DoMove);

  // Month-long orders
  DoOrders(R, 'build',      DoBuild);
  DoOrders(R, 'entertain',  DoEntertain);
  ResolveEntertainment(R);
  DoOrders(R, 'produce',    DoProduce);
  DoOrders(R, 'study',      DoStudy);
  DoOrders(R, 'teach',      DoTeach);
  DoOrders(R, 'work',       DoWork);
  ResolveWork(R);
  DoOrders(R, 'distribute', DoDistribute);
  DoOrders(R, 'transport',  DoTransport);
  
  // Add final regions of moving units to list
  for i := 0 to R.PlayerTroop.Units.Count - 1 do
  begin
    U := R.PlayerTroop.Units[i];
    if not EqualCoords(U.FinalCoords, R.Coords) then
      AddToMaint(U.FinalCoords);
  end;

  if RecreateRegion then
    for i := 0 to High(MaintRegions) do
      ResolveMaintenance(MaintRegions[i], ParseErrors);

  DoScripts(R, 'final');
  DoOrders(R, '@;warning', nil);

  TaxUnits.Free;
  PillageUnits.Free;
  WorkUnits.Free;
  EntertainUnits.Free;

  if RecreateRegion then CheckFPoints(ParseErrors);

  // Final unit processing
  for i := 0 to R.PlayerTroop.Units.Count - 1 do
  begin
    U := R.PlayerTroop.Units[i];
    // Check if we have stuff without men
    if (U.Items.Count > 0) and (U.Items.Amount(IT_MAN) = 0) then
    begin
      if U.Orders.IndexOf(';. No men in unit') < 0 then
        U.Orders.Insert(0, ';. No men in unit');
      ParseErrors.AddObject(Format('!E4 %s (%s): No men in unit', [U.Name, U.NumStr]), U);
    end
    // Add warnings for units without month order
    else if (U.Items.Count > 0) and (U.MonthOrder = '') and (Pos('@;!NOP', UpperCase(U.Orders.Text)) = 0) then
      ParseErrors.AddObject('! 5 ' + U.Name + ' (' + U.NumStr + '): No monthlong order', U);

    // Set avatars
    SetExportAvatars(U);

    // Clear empty orders
    k := 0;
    while k < U.Orders.Count do
      if Trim(U.Orders[k]) = '' then U.Orders.Delete(k)
      else Inc(k);

    // Calculate final items
    U.FinalItems.ClearItems();

    tempItems := U.Inventory.BalanceOn(tsDistribute);
    U.FinalItems.AssignItems(tempItems);
    tempItems.ClearAndFree();
  end;
end;

procedure TProcessThread.DoManualScript;
var AUnit: TUnit;
begin
  if Script >= 0 then begin
    AUnit := VFaction.Units.Find(UnitNum);
    if AUnit <> nil then RunScript(AUnit, Script, ScriptArgs, ParseErrors);
  end;
end;

procedure TProcessThread.ProcessAllRegions;
var i: integer;
begin
  Game.CreateVirtualTurn;
  for i := 0 to VFaction.Units.Count-1 do
    ClearErrorComments(VFaction.Units[i].Orders);
  ParseErrors.Clear;
  DoManualScript;
  if Terminated then Exit;
  SetLength(MaintRegions, 0);
  for i := 0 to VTurn.Regions.Count-1 do
    ProcessRegion(VTurn.Regions[i], False);
  for i := 0 to High(MaintRegions) do
    ResolveMaintenance(MaintRegions[i], ParseErrors);
  CheckFPoints(ParseErrors);
end;

procedure TProcessThread.Execute;
begin
  TerminatedQuery := IsTerminated;
  if Region <> nil then ProcessRegion(Region, True)
  else ProcessAllRegions;
end;

// We need it to set ProcessThread to nil; thread will actually free by itself
procedure TTerminator.FreeThread(Sender: TObject);
begin
  ProcessThread := nil;
  if Assigned(ThreadTerminate) then ThreadTerminate(Sender);
end;

// Start order-processing thread
procedure DoProcessOrders(ARegion: TRegion; AScript: integer; Args: string;
  AStartup: boolean);
begin
  if ProcessThread = nil then begin
    ProcessThread := TProcessThread.Create(True);
    with ProcessThread do begin
      if CurrUnit <> nil then begin
        Script := AScript;
        ScriptArgs := Args;
        UnitNum := CurrUnit.Num;
      end
      else Script := -1;
      Region := ARegion;
      Startup := AStartup;
      FreeOnTerminate := True;
      OnTerminate := Terminator.FreeThread;
      Resume;
    end;
  end;
end;

procedure StopProcessOrders;
begin
  if ProcessThread <> nil then ProcessThread.Terminate;
end;

function TProcessThread.IsTerminated: boolean;
begin
  Result := Terminated;
end;


initialization
  ParseErrors := TStringList.Create;
  Terminator := TTerminator.Create;

finalization
  ParseErrors.Free;
  Terminator.Free;

end.

