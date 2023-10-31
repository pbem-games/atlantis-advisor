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
  TProcessor = procedure (ARegion: TRegion; AUnit: TUnit; s: string; var Line: integer; Order: string);

  TProcessThread = class(TThread)
  private
    procedure DoManualScript;
    procedure DoOrder(ARegion: TRegion; AUnit: TUnit; Order: string; var Line: integer;
      Processor: TProcessor; CurrentOrder: string);
    procedure DoOrders(R: TRegion; Orders: array of string; Processor: TProcessor);
    procedure DoScripts(R: TRegion; Order: string);
    procedure DoGlobalOrders(R: TRegion);
    procedure ProcessRegion(Regs: TRegionList; RecreateRegion: boolean);
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

procedure TProcessThread.DoOrder(ARegion: TRegion; AUnit: TUnit; Order: string; var Line:
  integer; Processor: TProcessor; CurrentOrder: string);
var args, msg: string;
begin
  Order := Trim(Order);
  if Pos('@;;', Order) = 1 then args := Copy(Order, 4, Length(Order))
  else if Pos(' ', Order) = 0 then args := ''
  else args := Copy(Order, Pos(' ', Order) + 1, Length(Order));
  try
    Processor(ARegion, AUnit, args, Line, CurrentOrder);
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
procedure TProcessThread.DoOrders(R: TRegion; Orders: array of string; Processor: TProcessor);
var i: integer;
    u: TUnit;
    inRegion: array of TUnit;
    inStructs: array of TUnit;

  function isOrder(const order: string; out foundOrder: string): boolean;
  var p: integer;
  begin
    Result := False;
    for p := 0 to High(Orders) do
      if order = orders[p] then
      begin
        foundOrder := orders[p];
        Result := True;
        Break;
      end;
  end;

  procedure execOrders(AUnit: TUnit);
    var k: integer;
        currentOrder: string;
  begin
    currentOrder := Orders[0];

    // Run scripts
    RunScripts(AUnit, currentOrder, ParseErrors);

    // Find orders
    k := 0;
    // TAX order may be fired from unit flag
    if AUnit.Flags[flgTax] and (currentOrder = 'tax') then begin
      k := -1;
      DoOrder(R, AUnit, 'tax', k, Processor, 'tax');
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
        if isOrder(AUnit.Order(k), currentOrder) then
        begin
          if currentOrder = '@;warning' then
            ParseErrors.AddObject('!W2 ' + AUnit.Name + ' (' + AUnit.NumStr +
              '): ' + Copy(AUnit.Orders[k], Pos(currentOrder, AUnit.Orders[k]) +
              Length(currentOrder) + 1, Length(AUnit.Orders[k])), AUnit)
          else
            DoOrder(R, AUnit, AUnit.Orders[k], k, Processor, currentOrder);
        end;
        Inc(k);
      end;
    end;
  end;
begin
  SetLength(inRegion, 0);
  SetLength(inStructs, 0);

  for i := 0 to R.PlayerTroop.Units.Count-1 do begin
    u := R.PlayerTroop.Units[i];

    if u.Struct <> nil then
    begin
      SetLength(inStructs, Length(inStructs) + 1);
      inStructs[High(inStructs)] := u;
    end
    else
    begin
      SetLength(inRegion, Length(inRegion) + 1);
      inRegion[High(inRegion)] := u;
    end;
  end;

  i := 0;
  while (i < Length(inRegion)) and not Terminated do begin
    execOrders(inRegion[i]);
    Inc(i);
  end;

  i := 0;
  while (i < Length(inStructs)) and not Terminated do begin
    execOrders(inStructs[i]);
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
      DoOrders(VTurn.Regions[i], ['declare'], DoDeclare);
      DoOrders(VTurn.Regions[i], ['claim'], DoClaimRepeat);
    end;
end;

// Process orders for region
// NOTE: that orders must be processed by phases in EACH region
procedure TProcessThread.ProcessRegion(Regs: TRegionList; RecreateRegion: boolean);
var i, k, n: integer;
    U: TUnit;
    tempItems: TItemList;
    R, freg: TRegion;
    finalRegions: array of TRegion;
    troop: TTroop;

  procedure addToFinal(r: TRegion);
  var m: integer;
  begin
    for m := 0 to High(finalRegions) do
      if finalRegions[m] = r then Exit;

    SetLength(finalRegions, Length(finalRegions) + 1);
    finalRegions[High(finalRegions)] := r;
  end;

begin
  SetLength(finalRegions, 0);

  // for n := 0 to Regs.Count - 1 do
  // begin
  //   R := Regs[n];

  //   for k := 0 to R.ArrivingTroop.Count - 1 do
  //     R.ArrivingTroop[k].Free;

  //   for k := 0 to R.FinalTroops.Count - 1 do
  //     R.FinalTroops[k].Free;

  //   R.ArrivingTroop.Clear;
  //   R.FinalTroop.Clear;
  // end;

  if RecreateRegion then
  begin
    for n := 0 to Regs.Count - 1 do
    begin
      R := Regs[n];

      // Clear parse errors in region units
      if R.PlayerTroop <> nil then
        for i := 0 to R.PlayerTroop.Units.Count-1 do
        begin
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
  end;

  for n := 0 to Regs.Count - 1 do
  begin
    R := Regs[n];

    for k := 0 to R.FinalTroops.Count - 1 do
      R.FinalTroops[k].Free;
    R.FinalTroops.Clear;

    for k := 0 to R.ArrivingTroops.Count - 1 do
      R.ArrivingTroops[k].Free;
    R.ArrivingTroops.Clear;

    Assert(R.FinalTroops.Count = 0, 'Final troop list is not empty');
    Assert(R.ArrivingTroops.Count = 0, 'Arriving troop list is not empty');

    // Ensure that the region has player units
    if R.PlayerTroop = nil then Continue;

    TaxUnits := TUnitList.Create;

    // Instants
    if Startup then DoScripts(R, 'startup');

    DoScripts(R, '');

    DoOrders(R, ['form'],       DoForm);
    DoOrders(R, ['@;;'],        DoLocal);
    DoOrders(R, ['name'],       DoName);
    DoOrders(R, ['autotax', 'avoid', 'behind', 'guard', 'hold', 'noaid', 'nocross', 'share'], DoFlag);
    DoOrders(R, ['consume', 'reveal', 'spoils'], DoExtFlag);
    DoOrders(R, ['claim'],      DoClaim);
    DoOrders(R, ['combat'],     DoCombat);
    DoOrders(R, ['declare'],    DoDeclare);
    DoOrders(R, ['describe'],   DoDescribe);
    DoOrders(R, ['faction'],    DoFaction);
    DoOrders(R, ['leave'],      DoLeave);
    DoOrders(R, ['enter'],      DoEnter);
    DoOrders(R, ['promote'],    DoPromote);
    DoOrders(R, ['evict'],      DoEvict);
    DoOrders(R, ['attack'],     DoAttack);
    DoOrders(R, ['steal'],      DoSteal);
    DoOrders(R, ['destroy'],    DoDestroy);
    DoOrders(R, ['give'],       DoGive);

    PillageUnits := TUnitList.Create;
    DoOrders(R, ['pillage'],    DoPillage);
    ResolveTaxes(R, PillageUnits, 2, 4);

    DoOrders(R, ['tax'],        DoTax);
    ResolveTaxes(R, TaxUnits, 1, 1);

    TaxUnits.Free;
    PillageUnits.Free;

    DoOrders(R, ['cast'],       DoCast);
    DoOrders(R, ['sell'],       DoSell);
    DoOrders(R, ['buy'],        DoBuy);
    DoOrders(R, ['forget'],     DoForget);

    DoScripts(R, 'monthlong');

    // Move orders
    DoOrders(R, ['sail', 'move', 'advance'], DoMove);
  end;

  // Add units to the final region
  for n := 0 to Regs.Count - 1 do
  begin
    R := Regs[n];

    // Ensure that the region has player units
    troop := R.PlayerTroop;
    if troop = nil then Continue;

    for i := 0 to troop.Units.Count - 1 do
    begin
      U := troop.Units[i];
      freg := R;

      if not EqualCoords(U.FinalCoords, freg.Coords) then
        freg := VTurn.Regions.Find(U.FinalCoords);

      if freg <> nil then
      begin
        // if region not found then unit will end up in the unexplored region
        // we will calculate their maintenance separately
        freg.FinalTroops.Seek(U.Faction.Num).Units.Add(U);
        addToFinal(freg);
      end;
    end;
  end;

  for n := 0 to High(finalRegions) do
  begin
    R := finalRegions[n];

    // Month-long orders
    DoOrders(R, ['build'],      DoBuild);
    
    EntertainUnits := TUnitList.Create;
    DoOrders(R, ['entertain'],  DoEntertain);
    ResolveEntertainment(R);
    EntertainUnits.Free;
    
    DoOrders(R, ['produce'],    DoProduce);
    DoOrders(R, ['study'],      DoStudy);
    DoOrders(R, ['teach'],      DoTeach);
    
    WorkUnits := TUnitList.Create;
    DoOrders(R, ['work'],       DoWork);
    ResolveWork(R);
    WorkUnits.Free;
  end;

  for n := 0 to High(finalRegions) do
  begin
    R := finalRegions[n];

    // Transport orders
    DoOrders(R, ['transport', 'distribute'],  DoTransport);
  end;

  if RecreateRegion then
  begin
    for n := 0 to High(finalRegions) do
    begin
      R := finalRegions[n];
      ResolveMaintenance(R.Coords, R.FinalPlayerTroop.Units, ParseErrors);
    end;

    for n := 0 to High(Unexplored) do
      ResolveMaintenance(Unexplored[n].Coords, Unexplored[n].Units, ParseErrors);
  end;

  for n := 0 to High(finalRegions) do
  begin
    R := finalRegions[n];

    DoScripts(R, 'final');
    DoOrders(R, ['@;warning'], nil);
  end;

  if RecreateRegion then CheckFPoints(ParseErrors);

  for n := 0 to High(finalRegions) do
  begin
    R := finalRegions[n];
    
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
      tempItems := U.Inventory.BalanceOn(tsTransport);
      U.FinalItems.AssignItems(tempItems);
      tempItems.ClearAndFree();
    end;
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
  reg: TRegion;
  troop: TTroop;
begin
  InitUnexplored;
  try
    Game.CreateVirtualTurn;
    
    for i := 0 to VFaction.Units.Count-1 do
      ClearErrorComments(VFaction.Units[i].Orders);

    ParseErrors.Clear;
    DoManualScript;
    
    if Terminated then Exit;

    // NOTE: ORDERS can influence other regions, so we must process them all.
    //       To always process all regions will slow down the client.
    //       To speed up the client, we need to keep track of dependencies.
    //       And always process all regions that depend on the current one.
    //
    //       MOVE orders place units in the new region and all further stages will be influenced with arrived unit.
    //       TRANSPORT/DISTRIBUTE move items into different regions before maintenance stage.
    //       Regions must have a list of dependent regions that must be processed after the current one.
    //       We must collect all dependent regions before processing and process them after the current one.
    //       If during processing we find a new dependent region, we must add it to the list and reprocess all regions again.

    // Need to clean up dependencies
    for i := 0 to VTurn.Regions.Count - 1 do
    begin
      reg := VTurn.Regions[i];

      reg.DependsOn.Clear;
      reg.DependedBy.Clear;
    end;

    ProcessRegion(VTurn.Regions, False);

    // After all regions were processed, the dirty flag must be cleared
    VTurn.Dirty := False;

    for i := 0 to VTurn.Regions.Count - 1 do
    begin
      troop := VTurn.Regions[i].FinalPlayerTroop;
      if troop = nil then Continue;

      ResolveMaintenance(VTurn.Regions[i].Coords, troop.Units, ParseErrors);
    end;

    for i := 0 to High(Unexplored) do
      ResolveMaintenance(Unexplored[i].Coords, Unexplored[i].Units, ParseErrors);
    
    CheckFPoints(ParseErrors);
  finally
    ClearUnexplored;
  end;
end;

procedure TProcessThread.Execute;
type
  TRegionArray = array of TRegion;

  procedure push(item: TRegion; var list: TRegionArray);
  begin
    SetLength(list, Length(list) + 1);
    list[High(list)] := item;
  end;

  function pop(var list: TRegionArray): TRegion;
  begin
    Result := list[High(list)];
    SetLength(list, Length(list) - 1);
  end;

  function empty(var list: TRegionArray): boolean;
  begin
    Result := Length(list) = 0;
  end;

  function contains(item: TRegion; var list: TRegionArray): boolean;
  var i: integer;
  begin
    Result := False;
    for i := 0 to High(list) do
      if list[i] = item then
      begin
        Result := True;
        Break;
      end;
  end;

var
  regs: TRegionList;

  function getRegionsToProcess(r: TRegion): TRegionList;
  var
    i: integer;
    p, s: TRegionArray;

  begin
    Result := TRegionList.Create;
    SetLength(p, 0);
    SetLength(s, 0);

    // First we need to travel upwords to the roots
    push(r, s);
    while not empty(s) do
    begin
      // take the item from the stack
      r := pop(s);

      if contains(r, p) then
        Continue;

      // if we have not seen it before, add it to the list of processed regions
      push(r, p);

      // add its dependencies to the stack
      for i := 0 to r.DependsOn.Count - 1 do
        push(r.DependsOn[i], s);
    end;

    SetLength(s, Length(p));
    for i := 0 to High(p) do
      s[i] := p[i];
    SetLength(p, 0);
    
    // Now we need to travel downwords to the leaves
    while not empty(s) do
    begin
      // take the item from the stack
      r := pop(s);

      if contains(r, p) then
        Continue;

      // if we have not seen it before, add it to the list of processed regions
      push(r, p);

      // add its dependents to the stack
      for i := 0 to r.DependedBy.Count - 1 do
        push(r.DependedBy[i], s);
    end;

    // eventually we will have all regions in the list of processed regions
    // this will include loops too

    // Go through all regions and add them to the result if they are in the list
    // We do that to preserve the order of regions
    for i := 0 to VTurn.Regions.Count - 1 do
      if contains(VTurn.Regions[i], p) then
        Result.Add(VTurn.Regions[i]);
  end;

begin
  InitUnexplored;
  try
    TerminatedQuery := IsTerminated;

    // Only one region (with its dependencies) can be processed if all dependencies are known
    if (Region <> nil) and not VTurn.Dirty then
    begin
      regs := getRegionsToProcess(Region);
      try
        ProcessRegion(regs, True);
      finally
        regs.Free;
      end;

      if VTurn.Dirty then
        ProcessAllRegions;
    end
    else
      ProcessAllRegions;
  finally
    ClearUnexplored;
  end;
end;

// We need it to set ProcessThread to nil; thread will actually free by itself
procedure TTerminator.FreeThread(Sender: TObject);
begin
  ProcessThread := nil;
  if Assigned(ThreadTerminate) then ThreadTerminate(Sender);
end;

// Start order-processing thread
procedure DoProcessOrders(ARegion: TRegion; AScript: integer; Args: string; AStartup: boolean);
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

