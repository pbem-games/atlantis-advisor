unit uManager;

{$MODE Delphi}

{$WARN UNIT_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Grids, PowerGrid, ImgList, FileCtrl, DataStructs, RepRead,
  AtlaDate, ExtCtrls, ComCtrls, uHistory, Clipbrd, IniFiles, MyStrings,
  Resources, uOptions, uKeys, uGameSubs, uAvatars, uNewGame, uUnitRecs,
  IntEdit, uMgrOptions, uAnalyzers;

const
  FilenameCol = 3;

type
  TManagerForm = class(TForm)
    RepOpenDialog: TOpenDialog;
    MainPanel: TPanel;
    Panel1: TPanel;
    Label1: TLabel;
    GameCombo: TComboBox;
    NewGameBtn: TBitBtn;
    KillBtn: TBitBtn;
    RenameBtn: TBitBtn;
    RepGrid: TPowerGrid;
    AddBtn: TBitBtn;
    PasteBtn: TBitBtn;
    DelBtn: TBitBtn;
    ReadBtn: TBitBtn;
    ProgressBar: TProgressBar;
    ReadCloseBtn: TBitBtn;
    DetailBtn: TSpeedButton;
    CloseBtn: TBitBtn;
    Panel2: TPanel;
    LogMemo: TMemo;
    btnOptions: TBitBtn;
    procedure DetailBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ReadCloseBtnClick(Sender: TObject);
    procedure NewGameBtnClick(Sender: TObject);
    procedure KillBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GameComboChange(Sender: TObject);
    procedure RenameBtnClick(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure ReadBtnClick(Sender: TObject);
    procedure RepGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure PasteBtnClick(Sender: TObject);
    procedure OptionBtnClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
  private
    procedure WMDrawClipboard(var Msg: TMessage); message WM_DRAWCLIPBOARD;
    procedure WMChangeCBChain(var Msg: TMessage); message WM_CHANGECBCHAIN;
  public
    ActiveGame: string;
    procedure SetLogPanel(Open: boolean);
    procedure CheckRepList;
    procedure GlobalEnable;
    procedure FillRepGrid;
    procedure AddReport(filename: string; Move: Boolean);
    function MoveReport(Lines: TStrings; OldName: string): string;
    procedure ReadList;
  end;

var
  ManagerForm: TManagerForm;
  NextInChain : THandle;
  Reports: TStrings;
  OrdersLoaded: boolean;

  procedure OpenGame(GameName: string);
  procedure CloseGame;
  procedure RunLocalTurn(Num: integer; GetOutput: boolean);
  function GetLocalFolder(TurnNum: integer): string;
  function MakeFilename(TurnNum: integer): string;

implementation


{$R *.lfm}

procedure RunEngine(params: string; dir: string);
var exe: string;
    si: TStartupInfo;
    pinf: TProcessInformation;
begin
  exe := Config.ReadString('Engine', 'EngineFile', '');
  if FileExists(exe) and (DirectoryExists(dir)) then begin
    FillChar( Si, SizeOf( Si ) , 0 );
    Si.cb := SizeOf(Si);
    Si.dwFlags := startf_UseShowWindow;
    Si.wShowWindow := SW_SHOW;

    CreateProcess(nil, PChar(exe + ' ' + params), nil, nil, False, CREATE_DEFAULT_ERROR_MODE,
      nil, PChar(dir), si, pinf);
    WaitForSingleObject(pinf.hProcess, infinite);
  end
  else raise EInOutError.Create('Either path to exe file or directory is incorrect');
end;

procedure RunLocalTurn(Num: integer; GetOutput: boolean);
var dir, turn_dir: string;
begin
  dir := BaseDir + Game.Name + '\local\';
  turn_dir := dir + 'turn.' + IntToStr(Num) + '\';
  ForceDirectories(turn_dir);

  if GetOutput then begin
    CopyFile(PChar(dir + 'game.out'), PChar(turn_dir + 'game.in'), False);
    DeleteFile(dir + 'game.out');
    CopyFile(PChar(dir + 'players.out'), PChar(turn_dir + 'players.in'), False);
    DeleteFile(dir + 'players.out');
  end;

  DeleteFile(turn_dir + 'game.out');
  DeleteFile(turn_dir + 'players.out');
  DeleteFile(turn_dir + 'report.3');

  RunEngine('run', turn_dir);

  Config.WriteInteger('Engine', 'LastTurn', Num);

  CopyFile(PChar(turn_dir + 'game.out'), PChar(dir + 'game.out'), False);
  DeleteFile(turn_dir + 'game.out');
  CopyFile(PChar(turn_dir + 'players.out'), PChar(dir + 'players.out'), False);
  DeleteFile(turn_dir + 'players.out');

  GameConfig.WriteInteger('Game', 'LastTurnRan', Num);
end;

function GetLocalFolder(TurnNum: integer): string;
begin
  Result := BaseDir + Game.Name + '\local\turn.' + IntToStr(TurnNum) + '\';
end;

procedure CreateLocalWorld;
var dir: string;
    Lines: TStrings;
begin
  dir := BaseDir + Game.Name + '\local\';

  // Create world
  DeleteFile(dir + 'game.out');
  DeleteFile(dir + 'players.out');

  RunEngine('new', dir);

  if not FileExists(dir + 'players.out') then
    raise EInOutError.Create('File players.out not found');

  // Add new faction
  Lines := TStringList.Create;
  Lines.LoadFromFile(dir + 'players.out');
  Lines.Add('Faction: new');
  Lines.Add('LastOrders: 10000');
  Lines.SaveToFile(dir + 'players.out');
  Lines.Free;

  RunLocalTurn(1, True);
end;

function MakeFilename(TurnNum: integer): string;
begin
  Result := TurnToShortDate(TurnNum);
  Result[4] := '_';
  Result := IntToStr(TurnNum) + '_' + Result;
  if TurnNum < 10 then Result := '00' + Result
  else if TurnNum < 100 then Result := '0' + Result;
end;

procedure OpenGame(GameName: string);
var Trace: TTrace;
begin
  if GameName <> '' then begin
   // Create game and config
    Game := TGame.Create(GameName);
    GameConfig := TMemIniFile.Create(ExtractFilePath(Application.ExeName) +
      GameName + '\game.ini');

   // Decode reports
    Trace := TTrace.Create(GameConfig.ReadString('Game', 'Reports', ''));
    Reports.Clear;
    while not Trace.Ends and (Pos('.', Trace.Text) > 1) do
      Reports.Add(Trace.QBlock);
    Trace.Free;
  end;
end;

procedure CloseGame;
var i: integer;
    A: array of string;
    Lines: TStrings;
begin
  if GameConfig <> nil then begin
    // Save reports
    SetLength(A, Reports.Count);
    for i := 0 to Reports.Count-1 do A[i] := '"' + Reports[i] + '"';
    GameConfig.WriteString('Game', 'Reports', MakeList(A));
    GameConfig.UpdateFile;

   // Save history, if game was fully loaded
    if Game.Turns.Count > 1 then begin
      Lines := TStringList.Create;
      WriteGameHistory(Lines);
      SaveFilters(Lines);
      UnitRecs.Write(Lines);
      Lines.SaveToFile(BaseDir + Game.Name + '\game.dat');
      Lines.Free;
    end;

    for i := 0 to AvFilters.Count-1 do AvFilters[i].Free;
    AvFilters.Clear;

  // Wipe all
    FreeAndNil(GameConfig);
    FreeAndNil(Game);
    Turn := nil;
    Faction := nil;
  end;
end;

  { Form handlers }

procedure TManagerForm.FormCreate(Sender: TObject);
var Trace: TTrace;
    Base, s: string;
begin
 // Setup clipboard listener
  NextInChain := SetClipboardViewer(Handle);
  PasteBtn.Enabled := Clipboard.HasFormat(CF_TEXT);

 // Configure window
  RepGrid.Cols[2].Format := cfNumber;
  SetLogPanel(Config.ReadBool('Game Manager', 'Details', TRUE));

 // Fill games
  Base := ExtractFilePath(Application.ExeName);
  Trace := TTrace.Create(Config.ReadString('Game Manager', 'Games', ''));
  while not Trace.Ends and (Pos('.', Trace.Text) > 1) do begin
    s := Trace.QBlock;
    if (s <> '') and DirectoryExists(Base + s) then GameCombo.Items.Add(s);
  end;
  Trace.Free;
  if Game <> nil then ActiveGame := Game.Name;
  GameCombo.ItemIndex := Config.ReadInteger('Game Manager', 'GameIndex', 0);
  GameComboChange(Self);
end;

procedure TManagerForm.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
    A: array of string;
begin
 // Deactivate clipboard listener
  ChangeClipboardChain(Handle, NextInChain);

 // Close game if no reports read
  if (Game <> nil) and (Game.Turns.Count <= 1) then CloseGame;

 // Save games
  Config.WriteInteger('Game Manager', 'GameIndex', GameCombo.ItemIndex);
  SetLength(A, GameCombo.Items.Count);
  for i := 0 to GameCombo.Items.Count-1 do A[i] := '"' + GameCombo.Items[i] + '"';
  Config.WriteString('Game Manager', 'Games', MakeList(A));

  // Re-run last loaded turn for local game
  if (Game <> nil) and GameConfig.ReadBool('Game', 'Local', False)
    and (GameConfig.ReadInteger('Game', 'LastTurnRan', 0) > Turn.Num) then
    RunLocalTurn(Turn.Num, False);
end;

function TManagerForm.MoveReport(Lines: TStrings; OldName: string): string;
var newname, cycle, FName: string;
    i, FNum, TurnNum: integer;
begin
  if ReadRepHeader(Lines, FName, FNum, TurnNum) = rrsOk then
    newname := MakeFilename(TurnNum)
  else newname := 'unknown';
  newname := BaseDir + ActiveGame + '\reports\' + newname;

  if newname + '.rep' <> OldName then begin
    // Try filename0.rep, filename1.rep, and so on
    cycle := '';
    i := 0;
    while (i <= 10) and FileExists(newname + cycle + '.rep') do begin
      cycle := IntToStr(i);
      Inc(i);
    end;
    if i > 10 then begin
      MessageDlg('Cannot write file "'+ newname + '.rep"', mtError, [mbOk], 0);
      Result := '';
    end
    else begin
      Result := newname + cycle + '.rep';
      Lines.SaveToFile(Result);
    end;
  end;
end;

procedure TManagerForm.AddReport(filename: string; Move: boolean);
var Lines: TStrings;
    s: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(filename);
  except
  end;
  if Move then begin
    s := MoveReport(Lines, filename);
    if s <> '' then begin
      DeleteFile(filename);
      filename := s;
    end;
  end;
  Reports.Add(Localized(BaseDir + ActiveGame, filename));
  Lines.Free;
end;

procedure TManagerForm.AddBtnClick(Sender: TObject);
var i: integer;
begin
  RepOpenDialog.InitialDir := Config.ReadString('Game Manager', 'LastDir', '');
  if RepOpenDialog.Execute then begin
    Config.WriteString('Game Manager', 'LastDir',
      ExtractFilePath(RepOpenDialog.FileName));
    with RepOpenDialog do
      for i := 0 to Files.Count-1 do
        AddReport(Files[i], Config.ReadBool('Game Manager', 'MoveReps', TRUE));
    FillRepGrid;
  end;
  CheckRepList;
end;

// Look for duplicated or wrong reports
procedure TManagerForm.CheckRepList;
var i, k: integer;
begin
  i := 0;
  while (i < RepGrid.RowCount) and (RepGrid.Cells[0, i] <> 'failed') do Inc(i);
  if i < RepGrid.RowCount then begin
    if Config.ReadBool('Game Manager', 'PromptRemWrong', TRUE) and
      (MessageDlg('Some files not recognised '+
      'as reports or failed to load. Do you want to remove them from list?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
      for i := 0 to RepGrid.RowCount-1 do
        if RepGrid.Cells[0, i] = 'failed' then begin
          with Game do begin
            k := 0;
            while k < Reports.Count do
              if Reports[k] = RepGrid.Cells[FilenameCol, i] then Reports.Delete(k)
              else Inc(k);
          end;
        end;
      FillRepGrid;
    end;
  end;
end;

procedure TManagerForm.ReadCloseBtnClick(Sender: TObject);
begin
  ReadList;
  Close;
end;

procedure TManagerForm.DelBtnClick(Sender: TObject);
var i: integer;
begin
  i := 0;
  while (i < Reports.Count) and (Reports[i] <>
    RepGrid.ImgCells[FilenameCol, RepGrid.Row]) do Inc(i);
  if i < Reports.Count then Reports.Delete(i);
  FillRepGrid;
end;

procedure TManagerForm.DetailBtnClick(Sender: TObject);
begin
  SetLogPanel(not Config.ReadBool('Game Manager', 'Details', TRUE));
end;

procedure TManagerForm.FillRepGrid;
var i, FNum, TurnNum: integer;
    FName: string;
    Lines: TStrings;
begin
  // Reset window
  RepGrid.RowCount := 0;
  ProgressBar.Position := 0;

  // Fill reports grid
  if ActiveGame <> '' then begin
    for i := 0 to Reports.Count-1 do begin
      RepGrid.Cells[FilenameCol, i] := Reports[i];
      Lines := TStringList.Create;
      try
        Lines.LoadFromFile(Unlocalize(BaseDir + ActiveGame,  Reports[i]));
      except
      end;
      if ReadRepHeader(Lines, FName, FNum, TurnNum) = rrsErrors then
        RepGrid.Cells[0, i] := 'failed'
      else begin
        RepGrid.Cells[1, i] := TurnToShortDate(TurnNum);
        RepGrid.Cells[2, i] := IntToStr(TurnNum);
      end;
      Lines.Free;
    end;
  end;
  RepGrid.Fixup;
end;


procedure TManagerForm.GameComboChange(Sender: TObject);
begin
  if GameCombo.Text <> ActiveGame then begin
    CloseGame;
    ActiveGame := GameCombo.Text;
    OpenGame(ActiveGame);
  end;
  GlobalEnable;
  FillRepGrid;
end;

procedure TManagerForm.GlobalEnable;
var Value: boolean;
begin
  Value := (ActiveGame <> '');

  RenameBtn.Enabled := Value;
  KillBtn.Enabled := Value;

  AddBtn.Enabled := Value;
  PasteBtn.Enabled := Value;
  DelBtn.Enabled := Value;
  ReadBtn.Enabled := Value;
  ReadCloseBtn.Enabled := Value;
end;

procedure TManagerForm.KillBtnClick(Sender: TObject);
var i: integer;
begin
  if MessageDlg('You are about to kill "'+GameCombo.Text+'". Note ' +
    'that all files will remain in game folder. Are you sure to remove '+
    'the game from game list?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    i := GameCombo.ItemIndex;
    GameCombo.ItemIndex := -1;
    GameCombo.Items.Delete(i);
    GameCombo.Text := '';
    GameComboChange(Self);
    GlobalEnable;
  end;
end;

procedure TManagerForm.NewGameBtnClick(Sender: TObject);
var i: integer;
    s: string;
begin
  if not ProgOpened and (GameCombo.Items.Count >= 1) then begin
    MessageDlg('Cannot create more than one game in unregistered version.',
      mtWarning, [mbOk], 0);
    Exit;
  end;
  with TNewGameForm.Create(Self) do begin
    if (ShowModal = mrOk) and (cmRuleset.ItemIndex >= 0) then begin
      if DirectoryExists(BaseDir + GameNameEdit.Text) then begin
        // Check if game already in list
         i := 0;
         while (i < GameCombo.Items.Count) and (GameCombo.Items[i] <>
           GameNameEdit.Text) do Inc(i);
         if GameCombo.Items[i] = GameNameEdit.Text then MessageDlg('This game ' +
           'already exists.', mtError, [mbOK], 0)
         else begin
          // Prompt to re-attach the game
           if MessageDlg(s + 'Game folder "' + BaseDir + GameNameEdit.Text +
             '" already exists. Do you want to open it?', mtWarning, [mbYes, mbCancel], 0)
             = mrYes then begin
             GameCombo.Items.Add(GameNameEdit.Text);
             GameCombo.ItemIndex := GameCombo.Items.Count-1;
             GameComboChange(Self);
           end;
         end;
      end
      else begin
        // Create new game
        CreateDir(BaseDir + GameNameEdit.Text);
        CreateDir(BaseDir + GameNameEdit.Text + '\orders');
        CreateDir(BaseDir + GameNameEdit.Text + '\reports');
        if cbLocal.Checked then
          CreateDir(BaseDir + GameNameEdit.Text + '\local');
        // Copy ruleset to history
        CopyFile(PChar(BaseDir + RuleFolder +
          cmRuleset.Items[cmRuleset.ItemIndex] + '.dat'),
          PChar(BaseDir + GameNameEdit.Text + '\game.dat'), False);
        // Add to game list
        GameCombo.Items.Add(GameNameEdit.Text);
        GameCombo.ItemIndex := GameCombo.Items.Count-1;
        GameComboChange(Self);
        // Create world and add first report for local game
        if cbLocal.Checked then begin
          GameConfig.WriteBool('Game', 'Local', True);
          CreateLocalWorld;
          Reports.Add(GetLocalFolder(1) + 'report.3');
          FillRepGrid;
        end;
      end;
    end;
    Free;
  end;
end;

procedure TManagerForm.OptionBtnClick(Sender: TObject);
begin
  OptionForm := TOptionForm.Create(Self);
  OptionForm.ShowModal;
end;

procedure TManagerForm.PasteBtnClick(Sender: TObject);
var Lines: TStrings;
    filename: string;
begin
  Lines := TStringList.Create;
  Lines.Text := Clipboard.AsText;
  filename := MoveReport(Lines, '');
  Lines.Free;
  if filename <> '' then begin
    Reports.Add(Localized(BaseDir + ActiveGame, filename));
    FillRepGrid;
  end;
end;

procedure TManagerForm.ReadList;
var i, j: integer;
    rrs: TRepReadStatus;
    Lines: TStrings;
    filename: string;
    sr: TSearchRec;
begin
  Screen.Cursor := crHourGlass;
  CloseGame;
  OpenGame(ActiveGame);
  LogMemo.Lines.Clear;

 // Read game history
  LogMemo.Lines.Add('Reading game history');
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(BaseDir + ActiveGame + '\game.dat');
  except
  end;
  try
    ReadGameHistory(Lines);
    SimRegion.Terrain := Game.TerrainData.Find('plain');
    ReadFilters(Lines);
    UnitRecs.Read(Lines);
  except
    on E: Exception do begin
      MessageDlg('Error in history file: "' + E.Message + '". You should ' +
        'manually correct "game.dat" or create new game for this reports.',
        mtError, [mbOk], 0);
      Exit;
    end;
  end;
  Lines.Free;
  ProgressBar.Position := Round(1 / (RepGrid.RowCount+1) * 100);

  if History.Factions[1].Num > 0 then begin
    LogMemo.Lines.Add('Active faction: ' + History.Factions[1].Name +
      ' (' + IntToStr(History.Factions[1].Num) + ')');
  end;
  LogMemo.Lines.Add('');

  // Read reports
  for i := RepGrid.RowCount-1 downto 0 do begin
    filename := Unlocalize(BaseDir + ActiveGame, RepGrid.ImgCells[FilenameCol, i]);
    LogMemo.Lines.Add('Reading ' + filename);

    Lines := TStringList.Create;
    Lines.LoadFromFile(filename);
    rrs := ReadRep(Lines, LogMemo.Lines);
    Lines.Free;

    // Analyze loaded turn
    RunTurnAnalyzers;

    ProgressBar.Position := Round((RepGrid.RowCount - i + 1) / (RepGrid.RowCount+1) * 100);
    if rrs <> rrsFailed then begin
      if Turn.Num <= ToInt(RepGrid.ImgCells[2, 0]) -
        Config.ReadInteger('Game Manager', 'KeepReps', 10) then begin
        MergeWithHistory(Turn);
        rrs := -1;
        j := 0;
        while (j < Reports.Count) and (Reports[j] <>
          RepGrid.ImgCells[FilenameCol, i]) do Inc(j);
        if j < Reports.Count then Reports.Delete(j);
      end;
    end;
    case rrs of
      -1:
        RepGrid.ImgCells[0, i] := 'archive';
      rrsOk:
        RepGrid.ImgCells[0, i] := 'ok';
      rrsFailed:
        RepGrid.ImgCells[0, i] := 'failed';
      else
        RepGrid.ImgCells[0, i] := 'errors';
    end;
    RepGrid.Repaint;
  end;
  RepGrid.Fixup;

  RereadIncomplete;

  // Load last turn order
  OrdersLoaded := False;
  if Config.ReadBool('Game Manager', 'LoadLastOrder', True)
    and (Game.Turns.Count > 1) then begin
    if FindFirst(BaseDir + ActiveGame + '\orders\*.ord', 0, sr) = 0 then begin
      repeat
        RepLines := TStringList.Create;
        RepPos := 0;
        RepLines.LoadFromFile(BaseDir + ActiveGame + '\orders\' + sr.Name);
        if OrderTurn = Turn.Num then begin
          ReadOrders;
          LogMemo.Lines.Add('Found orders for turn ' + IntToStr(Turn.Num));
          OrdersLoaded := True;
        end;
        RepLines.Free;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

  LogMemo.Lines.Add('Done.');
  Screen.Cursor := crDefault;
end;

procedure TManagerForm.RenameBtnClick(Sender: TObject);
var s, text: string;
    i: integer;
begin
  text := InputBox('Rename game', 'Enter new name for ' + ActiveGame, ActiveGame);
  if text <> ActiveGame then begin
    if RenameFile(BaseDir + ActiveGame, BaseDir + Text) then begin
      ActiveGame := Text;
      GameConfig.Rename(BaseDir + ActiveGame + '\game.ini', FALSE);
      i := GameCombo.ItemIndex;
      GameCombo.Items[i] := Text;
      GameCombo.ItemIndex := i;
      if Game <> nil then Game.Name := Text;
    end
    else begin
      s := 'Cannot rename game folder "' + BaseDir + ActiveGame + '" to "' +
        Text + '". ';
      if DirectoryExists(BaseDir + ActiveGame) then
        s := s + 'Target folder already exists.';
      MessageDlg(s, mtError, [mbOK], 0);
    end;
  end;
end;


procedure TManagerForm.ReadBtnClick(Sender: TObject);
begin
  ReadList;
end;

procedure TManagerForm.RepGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with RepGrid do
    if ACol = 0 then begin
      if ImgCells[ACol, ARow] = 'ok' then
        ResForm.IconList.Draw(RepGrid.Canvas, TxtRect.Left + 1, TxtRect.Top, bmpOK)
      else if ImgCells[ACol, ARow] = 'failed' then
        ResForm.IconList.Draw(RepGrid.Canvas, TxtRect.Left + 1, TxtRect.Top, bmpNo)
      else if ImgCells[ACol, ARow] = 'archive' then
        ResForm.IconList.Draw(RepGrid.Canvas, TxtRect.Left + 1, TxtRect.Top, bmpSpell)
      else if ImgCells[ACol, ARow] = 'errors' then
        ResForm.IconList.Draw(RepGrid.Canvas, TxtRect.Left + 1, TxtRect.Top, bmpError);
      TxtRect.Left := TxtRect.Right;
    end;
end;

procedure TManagerForm.SetLogPanel(Open: boolean);
begin
 // Update config
  Config.WriteBool('Game Manager', 'Details', Open);
  if not Open then Width := MainPanel.Width + 2
  else Width := MainPanel.Width + LogMemo.Width + 6;
 // Draw button
  with DetailBtn.Glyph.Canvas do begin
    Brush.Color := clBtnFace;
    FillRect(ClipRect);
    Pen.Color := clBlack;
    Brush.Color := clAqua;
    if not Open then Polygon([Point(2,0), Point(7,5), Point(7,6), Point(2,11)])
    else Polygon([Point(7,0), Point(2,5), Point(2,6), Point(7,11)]);
  end;
end;


  { Clipboard }

procedure TManagerForm.WMDrawClipboard(var Msg: TMessage);
begin
  if (Clipboard.HasFormat(cf_text) and (ActiveGame <> '')) then
    PasteBtn.Enabled := TRUE //(Pos(Keys[s_Header], Clipboard.AsText) > 0)
  else PasteBtn.Enabled := FALSE;
  if NextInChain <> 0 then
    SendMessage(NextInChain, WM_DrawClipboard, 0, 0);
end;

procedure TManagerForm.WMChangeCBChain(var Msg: TMessage);
var Remove, Next: THandle;
begin
  Remove := Msg.WParam;
  Next := Msg.LParam;
  with Msg do
    if NextInChain = Remove then NextInChain := Next
    else if NextInChain <> 0 then
    SendMessage(NextInChain, WM_ChangeCBChain, Remove, Next)
end;

procedure TManagerForm.btnOptionsClick(Sender: TObject);
begin
  with TManagerOptionsForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;


initialization
  Reports := TStringList.Create;

finalization
  Reports.Free;

end.
