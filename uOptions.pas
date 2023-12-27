unit uOptions;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Resources, DataStructs, IniFiles,
  uKeys, MyStrings, Grids, ColorBtn, CheckLst, uGameSubs,
  Spin, Buttons, uExport, Math, uShortcuts, Menus;

type
  TOptionForm = class(TForm)
    CancelBtn: TButton;
    ColorDialog: TColorDialog;
    OKBtn: TButton;
    OptionPages: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet4: TTabSheet;
    pcEnvironment: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox4: TGroupBox;
    Label10: TLabel;
    Label1: TLabel;
    LongITCheck: TCheckBox;
    clCustomStr: TCheckListBox;
    cbNameAsLocal: TCheckBox;
    cmSkillColumn: TComboBox;
    cbArmyColors: TCheckBox;
    GroupBox5: TGroupBox;
    SaveConfCheck: TCheckBox;
    cbUnsavedOrders: TCheckBox;
    GroupBox6: TGroupBox;
    cbCompactOrders: TCheckBox;
    cbOverwriteMonthlong: TCheckBox;
    cbTurnEvents: TCheckBox;
    AttBox: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ColorBtn1: TColorBtn;
    ColorBtn2: TColorBtn;
    ColorBtn3: TColorBtn;
    ColorBtn4: TColorBtn;
    ColorBtn5: TColorBtn;
    lbBookmarks: TListBox;
    btnRemove: TButton;
    TabSheet5: TTabSheet;
    gShortcuts: TStringGrid;
    btnClear: TButton;
    lMessage: TLabel;
    Label2: TLabel;
    cbBattleImages: TCheckBox;
    TabSheet6: TTabSheet;
    mFormTemplate: TMemo;
    cmFormTemplate: TComboBox;
    btnRemoveTemplate: TButton;
    cbMonthNeeds: TCheckBox;
    eNeedsPriority: TSpinEdit;
    pcGame: TPageControl;
    TabSheet7: TTabSheet;
    Label14: TLabel;
    GameNameEdit: TEdit;
    PasswEdit: TEdit;
    PasswLabel: TLabel;
    GroupBox3: TGroupBox;
    Label11: TLabel;
    Label15: TLabel;
    Label12: TLabel;
    ServEmailEdit: TEdit;
    ServSubjEdit: TEdit;
    FromEdit: TEdit;
    cbNoMailDialog: TCheckBox;
    TabSheet8: TTabSheet;
    GroupBox2: TGroupBox;
    Label9: TLabel;
    Label8: TLabel;
    btnDetect: TButton;
    lbDimensions: TListBox;
    rbAutodetect: TRadioButton;
    rbManual: TRadioButton;
    eMapX: TSpinEdit;
    eMapY: TSpinEdit;
    GroupBox7: TGroupBox;
    tcWeather: TTabControl;
    imgBadSeason: TImage;
    lBadSeason: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    cbWeather: TCheckBox;
    cbWinterRegions: TCheckBox;
    cbFlyingCross: TCheckBox;
    cbMonthTax: TCheckBox;
    Label16: TLabel;
    Label17: TLabel;
    eLeaderMainentance: TSpinEdit;
    ePeasantMainentance: TSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    eEntertainIncome: TSpinEdit;
    eTaxIncome: TSpinEdit;
    eStudents: TSpinEdit;
    eHeals: TSpinEdit;
    Label22: TLabel;
    Label23: TLabel;
    cmArmyRout: TComboBox;
    Label24: TLabel;
    cbRedirNew: TCheckBox;
    cbCompactRepeating: TCheckBox;
    Label18: TLabel;
    eCustomFog: TSpinEdit;
    Label25: TLabel;
    cmMod: TComboBox;
    TabSheet9: TTabSheet;
    gPoints: TStringGrid;
    eMaxFP: TSpinEdit;
    lFpAmount: TLabel;
    cbTurnInTitle: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure btnDetectClick(Sender: TObject);
    procedure lbDimensionsClick(Sender: TObject);
    procedure rbDimClick(Sender: TObject);
    procedure eMapDimChange(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure tcWeatherChange(Sender: TObject);
    procedure WeatherClick(Sender: TObject);
    procedure gShortcutsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnClearClick(Sender: TObject);
    procedure cmFormTemplateChange(Sender: TObject);
    procedure mFormTemplateExit(Sender: TObject);
    procedure btnRemoveTemplateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FPChange(Sender: TObject);
    procedure eMaxFPChange(Sender: TObject);
    procedure eMaxFPKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    Filling: boolean;
  public
    BtnColors: array[0..5] of TColor;
    function ColorButton(Index: integer): TColorBtn;
    procedure PickupConfig;
    procedure SetupConfig;
    procedure FillShortcuts;
    procedure FillProgresses;
  end;

var
  OptionForm: TOptionForm;

implementation

uses Types, Main;

{$R *.lfm}

procedure TOptionForm.FormCreate(Sender: TObject);
begin
  OptionPages.ActivePageIndex := 0;
  pcEnvironment.ActivePageIndex := 0;
  pcGame.ActivePageIndex := 0;
  PickupConfig;
  FillShortcuts;
  tcWeatherChange(Self); // Fill weather months
end;

procedure TOptionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ApplyShortcuts(MainForm.MainMenu.Items);
end;

procedure TOptionForm.OKBtnClick(Sender: TObject);
var i: integer;
begin
  SetupConfig;
  // Warnings
  if cbWeather.Checked then begin
    i := 0;
    while (i < Map.Levels.Count) and (Map.Levels[i].Name <> Keys[s_Surface]) do
      Inc(i);
    if (i < Map.Levels.Count) and (Map.Levels[i].MaxPoint.X = 9999) then begin
      MessageDlg('Cannot guess weather on auto-detected surface. Please ' +
        'either uncheck "Guess Weather" or set map size for surface.',
        mtWarning, [mbOk], 0);
      Exit;
    end;
  end;
  ModalResult := mrOk;
end;

function TOptionForm.ColorButton(Index: integer): TColorBtn;
var i: integer;
begin
  Result := nil;
  for i := 0 to AttBox.ControlCount-1 do
    if AttBox.Controls[i].Tag = Index then Result := TColorBtn(AttBox.Controls[i]);
end;


procedure TOptionForm.PickupConfig;
var i, mask: integer;
begin
  with Config do begin
   // Attitudes
    for i := 1 to 5 do
      ColorButton(i).Color := TColorExtra(ColorExtras[i]).Color;

   // MainWin
    SaveConfCheck.Checked := ReadBool('MainWin', 'ConfirmSave', FALSE);
    cbUnsavedOrders.Checked := ReadBool('MainWin', 'UnsavedOrders', FALSE);
    cbTurnInTitle.Checked := ReadBool('MainWin', 'TurnInTitle', False);
    cbCompactOrders.Checked := ReadBool('MainWin', 'CompactOrders', False);
    cbCompactRepeating.Checked := ReadBool('MainWin', 'CompactRepeating', False);
    cbOverwriteMonthlong.Checked := ReadBool('MainWin', 'OverwriteMonthlong', False);
    cbMonthNeeds.Checked := ReadBool('MainWin', 'MonthNeeds', False);
    eNeedsPriority.Value := ReadInteger('MainWin', 'NeedsPriority', 0);
    cbTurnEvents.Checked := Config.ReadBool('MainWin', 'TurnEventsStart', True);
    cbArmyColors.Checked := Config.ReadBool('MainWin', 'ArmyColors', True);
    cbBattleImages.Checked := Config.ReadBool('MainWin', 'BattleImages', True);
    cbRedirNew.Checked := Config.ReadBool('MainWin', 'RedirNew', False);
    eCustomFog.Value := Config.ReadInteger('MainWin', 'CustomFogMonths', 12);

    // Unit grid
    mask := 1;
    for i := 0 to 7 do begin
      clCustomStr.Checked[i] := (Config.ReadInteger('MainWin', 'CustomString',
        U_NAME + U_FACTION) and mask <> 0);
      mask := mask * 2;
    end;
    LongITCheck.Checked := Config.ReadBool('MainWin', 'LongITs', True);
    cbNameAsLocal.Checked := Config.ReadBool('MainWin', 'NameAsLocal', True);
    cmSkillColumn.ItemIndex := Config.ReadInteger('MainWin', 'SkillColumn', 0);

    // Bookmarks
    if GameConfig <> nil then
      GameConfig.ReadSection('Bookmarks', lbBookmarks.Items);

    // FORM Templates
    Config.ReadSection('FormTemplates', cmFormTemplate.Items);
    if cmFormTemplate.Items.Count > 0 then begin
      cmFormTemplate.ItemIndex := 0;
      cmFormTemplateChange(cmFormTemplate);
    end;
  end;

  // Current Game
  with GameConfig do begin
    // Main
    GameNameEdit.Text := Game.Name;
    PasswEdit.Text := Game.Password;
    ServEmailEdit.Text := ReadString('Game', 'Server', '');
    FromEdit.Text := ReadString('Game', 'From', '');
    ServSubjEdit.Text := ReadString('Game', 'Subject', '');
    cbNoMailDialog.Checked := not ReadBool('Game', 'MailDialog', True);
    // Settings
    eLeaderMainentance.Value := ReadInteger('Settings', 'LeaderMaintenance', 20);
    ePeasantMainentance.Value := ReadInteger('Settings', 'PeasantMaintenance', 10);
    eEntertainIncome.Value := ReadInteger('Settings', 'EntertainIncome', 20);
    eTaxIncome.Value := ReadInteger('Settings', 'TaxIncome', 50);
    eStudents.Value := ReadInteger('Settings', 'StudentsPerTeacher', 10);
    eHeals.Value := ReadInteger('Settings', 'HealsPerMan', 5);
    cbFlyingCross.Checked := ReadBool('Settings', 'FlyingCross', True);
    cbMonthTax.Checked := ReadBool('Settings', 'MonthTax', False);
    cmArmyRout.ItemIndex := ReadInteger('Settings', 'ArmyRout', 0);
    cmMod.ItemIndex := ReadInteger('Settings', 'Mod', modStandard);
  end;

  FillProgresses;

  // Map
  lbDimensions.Clear;
  for i := 0 to Map.Levels.Count-1 do begin
    if Map.Levels[i].MaxPoint.x = 9999 then
      lbDimensions.Items.Add(Map.Levels[i].Name + ', auto')
    else
      lbDimensions.Items.Add(Map.Levels[i].Name + ', ' +
        IntToStr(Map.Levels[i].MaxPoint.X) + ' x ' +
        IntToStr(Map.Levels[i].MaxPoint.Y));
  end;
  lbDimensions.ItemIndex := 0;
  lbDimensionsClick(lbDimensions);

  // Weather
  cbWeather.Checked := GameConfig.ReadBool('Map', 'GuessWeather', False);
  cbWinterRegions.Checked := GameConfig.ReadBool('Map', 'WinterRegions', False);
end;

procedure TOptionForm.FillProgresses;
var i, j: integer;
  modid:  integer;
  pr:     integer;
begin
  modid := cmMod.ItemIndex;

  eMaxFP.Value := Length(Progress[0]) - 1;

  gPoints.RowCount := ProgressCount(modid) + gPoints.FixedRows;
  gPoints.ColCount := Length(Progress[0]) + 1;

  for i := 1 to gPoints.ColCount-1 do begin
    gPoints.ColWidths[i] := 30;
    gPoints.Cells[i, 0] := IntToStr(i-1);
  end;

  for i := 1 to gPoints.RowCount-1 do begin
    pr := IndexToProgress(modid, i - 1);

    gPoints.Cells[0, i] := ProgressNames[pr];
    for j := 0 to High(Progress[pr]) do
      gPoints.Cells[j + 1, i] := IntToStr(Progress[pr, j]);
  end;
end;

procedure TOptionForm.SetupConfig;
var mask, flags, i, j: integer;
  modid:  integer;
  pr:     integer;
begin
  modid := cmMod.ItemIndex;

  with Config do begin
    // Progresses
    for i := 1 to gPoints.RowCount-1 do
    begin
      pr := IndexToProgress(modid, i - 1);
      for j := 1 to gPoints.ColCount-1 do
        Progress[pr, j-1] := StrToInt(gPoints.Cells[j, i]);
    end;

   // Attitudes
    for i := 1 to 5 do
      TColorExtra(ColorExtras[i]).Color := ColorButton(i).Color;

   // MainWin
    WriteBool('MainWin', 'ConfirmSave', SaveConfCheck.Checked);
    WriteBool('MainWin', 'UnsavedOrders', cbUnsavedOrders.Checked);
    WriteBool('MainWin', 'TurnInTitle', cbTurnInTitle.Checked);
    WriteBool('MainWin', 'CompactOrders', cbCompactOrders.Checked);
    WriteBool('MainWin', 'CompactRepeating', cbCompactRepeating.Checked);
    WriteBool('MainWin', 'OverwriteMonthlong', cbOverwriteMonthlong.Checked);
    WriteBool('MainWin', 'MonthNeeds', cbMonthNeeds.Checked);
    WriteInteger('MainWin', 'NeedsPriority', eNeedsPriority.Value);
    WriteBool('MainWin', 'TurnEventsStart', cbTurnEvents.Checked);
    WriteBool('MainWin', 'ArmyColors', cbArmyColors.Checked);
    WriteBool('MainWin', 'BattleImages', cbBattleImages.Checked);
    WriteBool('MainWin', 'RedirNew', cbRedirNew.Checked);
    WriteInteger('MainWin', 'CustomFogMonths', eCustomFog.Value);

    // Unit grid
    mask := 1;
    flags := 0;
    for i := 0 to 7 do begin
      if clCustomStr.Checked[i] then flags := flags or mask;
      mask := mask * 2;
    end;
    Config.WriteInteger('MainWin', 'CustomString', flags);
    Config.WriteBool('MainWin', 'LongITs', LongITCheck.Checked);
    Config.WriteBool('MainWin', 'NameAsLocal', cbNameAsLocal.Checked);
    Config.WriteInteger('MainWin', 'SkillColumn', cmSkillColumn.ItemIndex);
  end;

  // Current Game
  Game.Name := GameNameEdit.Text;
  if Game.Password <> PasswEdit.Text then begin
    Game.Password := PasswEdit.Text;
    FactionLeader.Orders.Add('password "' + Game.Password + '"');
  end;

  with GameConfig do begin
    WriteString('Game', 'Server', ServEmailEdit.Text);
    WriteString('Game', 'From', FromEdit.Text);
    WriteString('Game', 'Subject', ServSubjEdit.Text);
    WriteBool('Game', 'MailDialog', not cbNoMailDialog.Checked);
    // Settings
    WriteInteger('Settings', 'LeaderMaintenance', eLeaderMainentance.Value);
    WriteInteger('Settings', 'PeasantMaintenance', ePeasantMainentance.Value);
    WriteInteger('Settings', 'EntertainIncome', eEntertainIncome.Value);
    WriteInteger('Settings', 'TaxIncome', eTaxIncome.Value);
    WriteInteger('Settings', 'StudentsPerTeacher', eStudents.Value);
    WriteInteger('Settings', 'HealsPerMan', eHeals.Value);
    WriteBool('Settings', 'FlyingCross', cbFlyingCross.Checked);
    WriteBool('Settings', 'MonthTax', cbMonthTax.Checked);
    WriteInteger('Settings', 'ArmyRout', cmArmyRout.ItemIndex);
    WriteInteger('Settings', 'Mod', modid);

    // Map
    for i := 0 to Map.Levels.Count-1 do begin
      WriteInteger('Map', 'MaxX_' + Map.Levels[i].Name, Map.Levels[i].MaxPoint.x);
      WriteInteger('Map', 'MaxY_' + Map.Levels[i].Name, Map.Levels[i].MaxPoint.y);
    end;

    // Weather
    WriteBool('Map', 'GuessWeather', cbWeather.Checked);
    WriteBool('Map', 'WinterRegions', cbWinterRegions.Checked);
  end;
end;


  { Form maintenance }

procedure TOptionForm.btnDetectClick(Sender: TObject);
var Bounds: TRect;
    i: integer;
begin
  i := lbDimensions.ItemIndex;
  Bounds := Map.Levels[i].Bounds;
  eMapX.Value := Bounds.Right + 1;
  eMapY.Value := Bounds.Bottom + 1;
  Map.Levels[i].MaxPoint := Point(Bounds.Right + 1, Bounds.Bottom + 1);
  lbDimensions.Items[i] := Map.Levels[i].Name + ', ' +
    IntToStr(Bounds.Right + 1) + ' x ' + IntToStr(Bounds.Bottom + 1);
end;

procedure TOptionForm.lbDimensionsClick(Sender: TObject);
var i: integer;
begin
  Filling := True;
  i := lbDimensions.ItemIndex;
  if Map.Levels[i].MaxPoint.X = 9999 then begin
    rbAutodetect.Checked := True;
    eMapX.Value := 0;
    eMapY.Value := 0;
  end
  else begin
    rbManual.Checked := True;
    eMapX.Value := Map.Levels[i].MaxPoint.x;
    eMapY.Value := Map.Levels[i].MaxPoint.y;
  end;
  eMapX.Enabled := (Map.Levels[i].MaxPoint.X < 9999);
  eMapY.Enabled := (Map.Levels[i].MaxPoint.X < 9999);
  btnDetect.Enabled := (Map.Levels[i].MaxPoint.X < 9999);
  Filling := False;
end;

procedure TOptionForm.rbDimClick(Sender: TObject);
var i: integer;
begin
  if Filling then Exit;
  i := lbDimensions.ItemIndex;
  if rbAutodetect.Checked then begin
    Map.Levels[i].MaxPoint := Point(9999, 9999);
    lbDimensions.Items[i] := Map.Levels[i].Name + ', auto';
  end
  else btnDetectClick(Sender);
  lbDimensionsClick(Sender);
end;

procedure TOptionForm.eMapDimChange(Sender: TObject);
var i: integer;
begin
  if Filling then Exit;
  i := lbDimensions.ItemIndex;
  Map.Levels[i].MaxPoint := Point(eMapX.Value, eMapY.Value);
  lbDimensions.Items[i] := Map.Levels[i].Name + ', ' +
    IntToStr(eMapX.Value) + ' x ' + IntToStr(eMapY.Value);
end;

procedure TOptionForm.btnRemoveClick(Sender: TObject);
begin
  if lbBookmarks.ItemIndex >= 0 then begin
    GameConfig.DeleteKey('Bookmarks', lbBookmarks.Items[lbBookmarks.ItemIndex]);
    lbBookmarks.DeleteSelected;
  end;
end;

procedure TOptionForm.tcWeatherChange(Sender: TObject);
var i, j: integer;
begin
  Filling := True;
  // Season name
  with imgBadSeason.Canvas do begin
    Brush.Color := clBlue;
    FillRect(ClipRect);
  end;
  if tcWeather.TabIndex = areaTropical then begin
    ResForm.Extras.Draw(imgBadSeason.Canvas, 0, 0, bmp_extMonsoon);
    lBadSeason.Caption := 'monsoon season';
  end
  else begin
    ResForm.Extras.Draw(imgBadSeason.Canvas, 0, 0, bmp_extWinter);
    lBadSeason.Caption := 'winter';
  end;
  // Months
  for i := 1 to 12 do begin
    j := 0;
    while (j < tcWeather.ControlCount) and ((tcWeather.Controls[j].ClassType <>
      TCheckBox) or (TCheckBox(tcWeather.Controls[j]).Tag <> i-1)) do Inc(j);
    TCheckBox(tcWeather.Controls[j]).Checked :=
      (WeatherMonths[tcWeather.TabIndex, i].MoveCost > 1);
  end;
  Filling := False;
end;

procedure TOptionForm.WeatherClick(Sender: TObject);
var i, j, bad: integer;
begin
  if Filling then Exit;
  if tcWeather.TabIndex = areaTropical then bad := weatherMonsoon
  else bad := weatherWinter;
  for i := 1 to 12 do begin
    j := 0;
    while (j < tcWeather.ControlCount) and ((tcWeather.Controls[j].ClassType <>
      TCheckBox) or (TCheckBox(tcWeather.Controls[j]).Tag <> i-1)) do Inc(j);
    if TCheckBox(tcWeather.Controls[j]).Checked then
      WeatherMonths[tcWeather.TabIndex, i] := Game.WeatherData[bad]
    else WeatherMonths[tcWeather.TabIndex, i] := Game.WeatherData[weatherClear];
  end;
end;

{ Shortcuts }

procedure TOptionForm.FillShortcuts;
var i: integer;
    Lines: TStrings;
    Sc: TShortCut;
    Key: Word;
    Shift: TShiftState;

  procedure AddItem(Item: TMenuItem; s: string);
  var row, i: integer;
  begin
    if Item.Caption = '-' then Exit;

    if s <> '' then s := s + ' | ';
    s := s + StringReplace(Item.Caption, '&', '', []);
    if Item.Count = 0 then begin
      row := gShortcuts.RowCount;
      gShortcuts.Cells[0, row] := s;
      gShortcuts.Cells[1, row] := ShortcutToStr(FindShortcut(s, False));
    end
    else
      for i := 0 to Item.Count-1 do AddItem(Item.Items[i], s);
  end;

begin
  // FIXME: broken
  //Lines := TStringList.Create;
  //Config.ReadSection('Shortcuts', Lines);
  //
  //gShortcuts.RowCount := 0;
  //// Unit orders
  //for i := 0 to High(ShortcutOrders) do begin
  //  gShortcuts.Cells[0, i] := ShortcutOrders[i];
  //  Sc := FindShortcut(ShortcutOrders[i], False);
  //  if Sc > 0 then begin
  //    ShortCutToKey(Sc, Key, Shift);
  //    if (i < RepeatingShortcuts) and (ssShift in Shift) then begin
  //      Sc := ShortCut(Key, Shift - [ssShift]);
  //      gShortcuts.Cells[0, i] := '@' + ShortcutOrders[i];
  //    end
  //    else gShortcuts.Cells[0, i] := ShortcutOrders[i];
  //    gShortcuts.Cells[1, i] := ShortcutToStr(Sc);
  //  end;
  //  if i < RepeatingShortcuts then
  //    gShortcuts.Rows[i].FontStyle := [fsBold];
  //end;
  //// Menu
  //for i := 0 to MainForm.MainMenu.Items.Count-1 do
  //  AddItem(MainForm.MainMenu.Items[i], '');
  //gShortcuts.Fixup;
  //Lines.Free;
end;

procedure TOptionForm.gShortcutsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var s: string;
begin
  // FIXME: broken
  //s := gShortcuts.ImgCells[0, gShortcuts.Row];
  //if Copy(s, 1, 1) = '@' then s := Copy(s, 2, Length(s));
  //RecordShortCut(Key, Shift, s);
  //FillShortcuts;
  //Key := 0;
end;

procedure TOptionForm.btnClearClick(Sender: TObject);
var s: string;
begin
  // FIXME: broken
  //s := gShortcuts.ImgCells[0, gShortcuts.Row];
  //if Copy(s, 1, 1) = '@' then s := Copy(s, 2, Length(s));
  //ClearShortcut(s);
  //FillShortcuts;
end;

{ FORM Templates }

procedure TOptionForm.cmFormTemplateChange(Sender: TObject);
var s: string;
begin
  if cmFormTemplate.ItemIndex = -1 then Exit;
  s := Config.ReadString('FormTemplates',
    cmFormTemplate.Items[cmFormTemplate.ItemIndex], '');
  s := StringReplace(s, '<br>', #13#10, [rfReplaceAll]);
  mFormTemplate.Text := s;
  mFormTemplate.Modified := False;
end;

procedure TOptionForm.mFormTemplateExit(Sender: TObject);
var s: string;
begin
  if cmFormTemplate.ItemIndex = -1 then Exit;
  if not mFormTemplate.Modified then Exit;
  s := mFormTemplate.Text;
  s := StringReplace(s, #13#10, '<br>', [rfReplaceAll]);
  Config.WriteString('FormTemplates',
    cmFormTemplate.Items[cmFormTemplate.ItemIndex], s);
end;

procedure TOptionForm.btnRemoveTemplateClick(Sender: TObject);
begin
  // FIXME: broken
  //if cmFormTemplate.ItemIndex = -1 then Exit;
  //Config.DeleteKey('FormTemplates', cmFormTemplate.Items[cmFormTemplate.ItemIndex]);
  //cmFormTemplate.DeleteSelected;
  //cmFormTemplate.Text := '';
  //if cmFormTemplate.Items.Count > 0 then begin
  //  cmFormTemplate.ItemIndex := 0;
  //  cmFormTemplateChange(cmFormTemplate);
  //end
  //else mFormTemplate.Text := '';
  //cmFormTemplate.Repaint;
end;

procedure TOptionForm.FPChange(Sender: TObject);
begin
  FillProgresses;
end;

procedure TOptionForm.eMaxFPChange(Sender: TObject);
var i, value: integer;
    T: TTurn;
begin
  if eMaxFP.Text = '' then Exit;
  value := eMaxFP.Value + 1;
  if value < Length(Progress[0]) then
    if MessageDlg('This amount of FP is less than detected. Amounts of ' +
      'distributed FP from report may be lowered to fit this limit. ' +
      'Are you sure?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then begin
      eMaxFp.Value := Length(Progress[0]) - 1;
      Exit;
    end;
  for i := 0 to prCount-1 do
    if Length(Progress[i]) <> value then
      SetLength(Progress[i], value);
  for i := 0 to Game.Turns.Count-1 do begin
    T := Game.Turns[i];
    T.War := Min(T.War, value);
    T.Trade := Min(T.War, value);
    T.Magic := Min(T.War, value);
  end;
  FillProgresses;
end;

procedure TOptionForm.eMaxFPKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then eMaxFPChange(eMaxFP);
end;

end.
