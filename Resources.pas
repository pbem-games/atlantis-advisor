unit Resources;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, IniFiles, ImgList, Math,
  // custom
  MyStrings, DataStructs;

const
  IconsFolder = 'Images\Icons\';
  TerrainFolder = 'Images\Terrain\';
  ExtrasFolder = 'Images\Extras\';
  BattleFolder = 'Images\Battle\';
  RuleFolder = 'Rulesets\';

  TerrainBmpCount = 18;
  clNether = $00000008;
  clDefaultTerrainColor = clMaroon;
  clDefaultAttitudeColor = clWhite;

  // ExtFlags
  bmp_efConsume = 7;
  bmp_efReveal = 10;
  bmp_efSpoils = 13;

 // Terrains
  bmp_terrRock = 7;
  bmp_terrRoad = 1;

 // Extras
  bmp_extFlag = 0;
  bmp_extBuilding = 2;
  bmp_extFort = 4;
  bmp_extShip = 6;
  bmp_extBalloon = 8;
  bmp_extLair = 10;
  bmp_extShaft = 12;
  bmp_extShield = 14;

  extFlag = 0;
  extBuilding = 1;
  extFort = 2;
  extShip = 3;
  extBalloon = 4;
  extLair = 5;
  extShaft = 6;
  extShield = 7;
  extCount = 8;

  bmp_extVillage = bmp_extFlag + 16;
  bmp_extTown = bmp_extVillage + 1;
  bmp_extCity = bmp_extVillage + 2;

  bmp_extUnknown = bmp_extVillage + 3;

  bmp_extWeather = bmp_extUnknown;
  bmp_extClear = bmp_extWeather + 1;
  bmp_extWinter = bmp_extWeather + 2;
  bmp_extMonsoon = bmp_extWeather + 3;

  bmp_extMonster = bmp_extWeather + 4;
  bmp_extGate = bmp_extMonster + 1;
  bmp_extBattle = bmp_extMonster + 2;
  bmp_extWarning = bmp_extMonster + 3;
  bmp_extNotes = 37;
  bmp_extCross = bmp_extMonster + 4;
  bmp_extMage = bmp_extMonster + 5;
  bmp_extWpnClasses = bmp_extMonster + 6;
  bmp_extGrayFlag = 38;
  bmp_extTrade = 39;
  bmp_extTax = 40;
  bmp_extAttackTypes = 41;
  bmp_extAttack = 47;
  bmp_extStudyTarget = 48;

  // IconList
  bmpUnknownItem = 0;
  bmpMen = bmpUnknownItem + 1;
  bmpMonsters = bmpUnknownItem + 2;
  bmpMagic = bmpUnknownItem + 3;
  bmpWeapon = bmpUnknownItem + 4;
  bmpArmor = bmpUnknownItem + 5;
  bmpFood = bmpUnknownItem + 6;
  bmpMounts = bmpUnknownItem + 7;
  bmpResources = bmpUnknownItem + 8;
  bmpAdvanced = bmpUnknownItem + 9;
  bmpTools = bmpUnknownItem + 10;
  bmpWagons = bmpUnknownItem + 11;
  bmpSilver = bmpUnknownItem + 12;
  bmpTradeGoods = bmpUnknownItem + 13;
  bmpShip = bmpUnknownItem + 20;

  bmpOk = bmpUnknownItem + 14;
  bmpNo = bmpOk + 1;
  bmpInfo = bmpOk + 2;
  bmpError = bmpOk + 3;

  bmpStructs = bmpUnknownItem + 18;

  bmpSkill = 25;
  bmpSpell = 45;
  bmpCombatSpell = bmpSkill + 1;
  bmpFoundation = bmpSkill + 2;

  bmpPack = bmpSkill + 3;
  bmpFaction = bmpPack + 1;
  bmpAdvance = bmpPack + 2;
  bmpBook = bmpPack + 3;
  bmpGlasses = bmpPack + 4;
  bmpTax = bmpPack + 5;
  bmpCheckOff = bmpPack + 6;
  bmpCheckOn = bmpCheckOff + 1;
  bmpScript = bmpPack + 8;
  bmpSpecial = bmpPack + 9;
  bmpEffect = bmpPack + 10;
  bmpFormTemplate = bmpPack + 11;
  bmpForce = bmpPack + 12; // must be even number
  bmpNewUnit = bmpPack + 14;
  bmpDelUnit = bmpPack + 15;
  bmpRoute = bmpPack + 16;
  bmpListPlus = 46;
  bmpListMinus = 47;

  // Arrows
  bmp_arrTranspArrows = 7;
  bmp_arrGreenArrows = 0;
  bmp_arrWhiteArrows = 14;
  bmp_arrCyanArrows = 21;

  // BattleImages
  FirstCustomBattleImage = 2;

type
  TColorExtra = class
    Color: TColor;
    Images: array[0..extCount-1] of TBitmap;
    constructor Create(AColor: TColor);
    destructor Destroy; override;
  end;

  TResForm = class(TForm)
    Terrains: TImageList;
    Extras: TImageList;
    BtnImages: TImageList;
    Arrows: TImageList;
    IconList: TImageList;
    FlagImages: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    BattleImages: TImageList;
    Label7: TLabel;
    SmallBtnList: TImageList;
    Label8: TLabel;
  public
    function GetColorExtra(Index: integer; Color: TColor): TBitmap;
    procedure LoadTerrainImages;
    procedure LoadPeasantExtras;
  end;

var
  ResForm: TResForm;
  Config, GameConfig: TMemIniFile;
  BaseDir: string;
  ColorExtras: TList; // Colored extras for each color
  Icons, PeasantExtras: TStrings; // Names of custom icons
  ProgOpened: boolean;
  ScanKey: DWord;

  function ItemIconIndex(AItemData: TItemData): integer;
  procedure DrawCExtra(Index: integer; AFaction: TFaction; ACanvas: TCanvas; x, y: integer);
  procedure LoadIcons;
  procedure DrawItemIcon(Canvas: TCanvas; X, Y: integer; AItemData: TItemData);
  procedure MakeItemBmp(Bitmap: TBitmap; AItemData: TItemData);
  function FindIcon(Short: string): boolean;
  function SeekColorExtra(AColor: TColor): integer;
  procedure ClearColorExtras;
  function FactionCIndex(AFaction: TFaction): integer;
  function FactionColor(AFaction: TFaction): TColor;
  procedure LoadConfigColors;
  procedure SaveConfigColors;
  function GetMaskedBmp(List: TImageList; Index: integer; Color: TColor): TBitmap;
  procedure DrawTranspText(ACanvas: TCanvas; x, y: integer; s: string;
    MaxWidth: integer; Shadow: boolean);
  procedure DrawBattleImage(Cnv: TCanvas; X, Y: integer; Filename: string;
    Color: TColor; Flip: boolean);
  procedure DrawFileFlipped(Cnv: TCanvas; X, Y: integer; Filename: string;
    Flip: boolean);
  procedure PrepareBmp(Bmp: TBitmap; Wid, Hei: integer; Fill: TColor);
  function ShiftPressed: boolean;
  function AltPressed: boolean;
  function CtrlPressed: boolean;
  function RealKey(Key: Word): Word;
  function GameMod: integer;
  procedure DrawExternalImage(Filename: string; ACanvas: TCanvas; X, Y: integer; clTransp: TColor);

implementation

{$R *.DFM}

function GameMod: integer;
begin
  Result := GameConfig.ReadInteger('Settings', 'Mod', modStandard);
end;

function ShiftPressed: boolean;
begin
  Result := (GetKeyState(VK_SHIFT) and $80 <> 0);
end;

function AltPressed: boolean;
begin
  Result := (GetKeyState(VK_MENU) and $80 <> 0);
end;

function CtrlPressed: boolean;
begin
  Result := (GetKeyState(VK_CONTROL) and $80 <> 0);
end;

function RealKey(Key: Word): Word;
begin
  Result := Key;
  if not Test(ScanKey, $100) then
    case Key of
      VK_DELETE: Result := VK_DECIMAL;
      VK_INSERT: Result := VK_NUMPAD0;
      VK_END:   Result := VK_NUMPAD1;
      VK_DOWN:  Result := VK_NUMPAD2;
      VK_NEXT:  Result := VK_NUMPAD3;
      VK_LEFT:  Result := VK_NUMPAD4;
      VK_CLEAR: Result := VK_NUMPAD5;
      VK_RIGHT: Result := VK_NUMPAD6;
      VK_HOME:  Result := VK_NUMPAD7;
      VK_UP:    Result := VK_NUMPAD8;
      VK_PRIOR: Result := VK_NUMPAD9;
    end;
end;


{ TResForm }

procedure DrawTranspText(ACanvas: TCanvas; x, y: integer; s: string;
  MaxWidth: integer; Shadow: boolean);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.Canvas.Font.Assign(ACanvas.Font);
  // Add ...
  if Bmp.Canvas.TextExtent(s).cx > MaxWidth then begin
    while (s <> '') and (Bmp.Canvas.TextExtent(s + '...').cx > MaxWidth) do
      s := Copy(s, 1, Length(s) - 1);
    s := s + '...';
  end;
  // Draw
  Bmp.Width := Min(Bmp.Canvas.TextExtent(s).cx, MaxWidth) + 3;
  Bmp.Height := Bmp.Canvas.TextExtent(s).cy + 3;
  if Shadow then begin
    Bmp.Canvas.Font.Color := clBlack;
    DrawTranspText(Bmp.Canvas, 1, 0, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 1, 2, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 0, 1, s, MaxWidth, False);
    DrawTranspText(Bmp.Canvas, 2, 1, s, MaxWidth, False);
    Bmp.Canvas.Font.Color := ACanvas.Font.Color;
    DrawTranspText(Bmp.Canvas, 1, 1, s, MaxWidth, False);
  end
  else Bmp.Canvas.TextOut(1, 1, s);
  Bmp.Transparent := TRUE;
  ACanvas.Draw(x, y, Bmp);
  Bmp.Free;
end;


function GetMaskedBmp(List: TImageList; Index: integer; Color: TColor): TBitmap;
var Mask, Image: TBitmap;
    clTransp: TColor;
begin
  if Color = clBlack then Color := clBlack + 8;
  if Color <> clFuchsia then clTransp := clFuchsia
  else clTransp := clOlive;
  // Draw color mask over extra image
  Image := TBitmap.Create;
  List.BkColor := clTransp;
  List.GetBitmap(Index*2, Image);
  Mask := TBitmap.Create;
  List.BkColor := Color;
  List.GetBitmap(Index*2 + 1, Mask);
  Mask.Transparent := TRUE;
  Image.Canvas.Draw(0, 0, Mask);
  Image.Transparent := TRUE;
  // Drop image onto canvas
  Mask.Free;
  List.BkColor := clNone;
  Result := Image;
end;

function TResForm.GetColorExtra(Index: integer; Color: TColor): TBitmap;
begin
  Result := GetMaskedBmp(Extras, Index, Color);
end;

procedure TResForm.LoadPeasantExtras;
var sr: TSearchRec;

  procedure AddExtra(Short: string);
  var Bmp: TBitmap;
  begin
    Bmp := TBitmap.Create;
    Bmp.LoadFromFile(BaseDir + ExtrasFolder + Short + '.bmp');
    Extras.AddMasked(Bmp, Bmp.TransparentColor);
    Bmp.Free;
    PeasantExtras.Add(Short + '=' + IntToStr(Extras.Count-1));
  end;

begin
  if FindFirst(BaseDir + ExtrasFolder + '*.bmp', faAnyFile, sr) = 0 then begin
    AddExtra(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
    while FindNext(sr) = 0 do
      AddExtra(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
  end;
  FindClose(sr);
end;

procedure TResForm.LoadTerrainImages;
var Ini: TMemIniFile;
    i, j: integer;
    s: string;
    Weather: TWeatherData;
    DefColor: TColor;

  procedure AddTerrain(Filename: string);
  var Bmp: TBitmap;
  begin
    Bmp := TBitmap.Create;
    Bmp.LoadFromFile(Filename);
    Terrains.AddMasked(Bmp, Bmp.TransparentColor);
    Bmp.Free;
  end;

begin
  Ini := TMemIniFile.Create(BaseDir + TerrainFolder + 'terrain.ini');
  for i := 0 to Game.TerrainData.Count-1 do begin
    // Main terrain
    s := Ini.ReadString('Images', Game.TerrainData[i].Name, '');
    if s <> '' then begin
      AddTerrain(BaseDir + TerrainFolder + s);
      for j := 0 to weatherCount-1 do
        Game.TerrainData[i].BmpIndexes[j] := Terrains.Count-1;
    end;
    // Weather terrains
    for j := 0 to weatherCount-1 do begin
      Weather := Game.WeatherData[j];
      if Weather <> nil then begin
        s := Ini.ReadString('Images', Game.TerrainData[i].Name + ' ' +
          Weather.Text, '');
        if s <> '' then begin
          AddTerrain(BaseDir + TerrainFolder + s);
          Game.TerrainData[i].BmpIndexes[j] := Terrains.Count-1;
        end;
      end;
    end;
    // Terrain color for minimap
    DefColor := Config.ReadInteger('TerrainColors', 'unknown', clWhite);
    Game.TerrainData[i].Color := Config.ReadInteger('TerrainColors',
      Game.TerrainData[i].Name, DefColor);
  end;
  Ini.Free;
end;

 { Icons }

// Find custom icons and battle images
procedure LoadIcons;
var sr: TSearchRec;
begin
  // Icons
  if FindFirst(BaseDir + IconsFolder + '*.bmp', faAnyFile, sr) = 0 then begin
    Icons.Add(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
    while FindNext(sr) = 0 do
      Icons.Add(UpperCase(Copy(sr.Name, 1, Pos('.', sr.Name)-1)));
  end;
  FindClose(sr);
end;

procedure DrawFlipped(Cnv: TCanvas; X, Y: integer; Bmp: TBitmap; Flip: boolean);
var Bmp1: TBitmap;
    i: integer;
begin
  if Flip then begin
    Bmp1 := TBitmap.Create;
    Bmp1.Width := Bmp.Width;
    Bmp1.Height := Bmp.Height;
    for i := 0 to Bmp.Width-1 do
      Bmp1.Canvas.CopyRect(Rect(i, 0, i+1, Bmp1.Height), Bmp.Canvas,
        Rect(Bmp.Width-i - 1, 0, Bmp.Width-i, Bmp.Height));
    Bmp1.Transparent := True;
    Cnv.Draw(x, y, Bmp1);
    Bmp1.Free;
  end
  else begin
    Bmp.Transparent := TRUE;
    Cnv.Draw(X, Y, Bmp);
  end;
end;

// Draw onto canvas custom icon from file
procedure DrawExternalImage(Filename: string; ACanvas: TCanvas;
  X, Y: integer; clTransp: TColor);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(BaseDir + Filename);
  except
    on EFOpenError do FreeAndNil(Bmp);
  end;
  if Bmp <> nil then begin
    Bmp.Transparent := True;
    if clTransp <> -1 then Bmp.TransparentColor := clTransp;
    ACanvas.Draw(X, Y, Bmp);
    Bmp.Free;
  end;
end;

// Check if there is custom icon for this item
function FindIcon(Short: string): boolean;
var i: integer;
begin
  i := 0;
  while (i < Icons.Count) and (LowerCase(Icons[i]) <> LowerCase(Short)) do
    Inc(i);
  Result := (i < Icons.Count);
end;

function ItemIconIndex(AItemData: TItemData): integer;
begin
  if (AItemData.Flags and IT_MAN <> 0) then Result := bmpMen
  else if (AItemData.Flags and IT_MONSTER <> 0) then Result := bmpMonsters
  else if (AItemData.Flags and IT_WEAPON <> 0) then Result := bmpWeapon
  else if (AItemData.Flags and IT_MAGIC <> 0) then Result := bmpMagic
  else if (AItemData.Flags and IT_ARMOR <> 0) then Result := bmpArmor
  else if (AItemData.Flags and IT_MOUNT <> 0) then Result := bmpMounts
  else if (AItemData.Flags and IT_FOOD <> 0) then Result := bmpFood
  else if (AItemData.Flags and IT_TOOL <> 0) then Result := bmpTools
  else if (AItemData.Flags and IT_SILVER <> 0) then Result := bmpSilver
  else if (AItemData.Flags and IT_WAGON <> 0) then Result := bmpWagons
  else if (AItemData.Flags and IT_TRADE <> 0) then Result := bmpTradeGoods
  else if (AItemData.Flags and IT_ADVANCED <> 0) then Result := bmpAdvanced
  else if (AItemData.Flags and IT_RESOURCE <> 0) then Result := bmpResources
  else if (AItemData.Flags and IT_SHIP <> 0) then Result := bmpShip
  else Result := 0;
end;

procedure DrawItemIcon(Canvas: TCanvas; X, Y: integer; AItemData: TItemData);
begin
  if FindIcon(AItemData.Short) then
    DrawExternalImage(IconsFolder + AItemData.Short + '.bmp', Canvas, X, Y, -1)
  else
    ResForm.IconList.Draw(Canvas, X, Y, ItemIconIndex(AItemData));
end;

procedure MakeItemBmp(Bitmap: TBitmap; AItemData: TItemData);
begin
  Bitmap.Width := 16;
  Bitmap.Height := 16;
  Bitmap.Canvas.Brush.Color := clNether;
  Bitmap.Canvas.FillRect(Bitmap.Canvas.ClipRect);
  DrawItemIcon(Bitmap.Canvas, 0, 0, AItemData);
end;

procedure DrawCExtra(Index: integer; AFaction: TFaction; ACanvas: TCanvas; x, y: integer);
begin
  ACanvas.Draw(x, y, TColorExtra(ColorExtras[FactionCIndex(AFaction)]).Images[Index]);
end;

{ TColorExtra }

constructor TColorExtra.Create(AColor: TColor);
var i: integer;
begin
  Color := AColor;
  for i := 0 to extCount-1 do Images[i] := ResForm.GetColorExtra(i, Color);
end;

destructor TColorExtra.Destroy;
var i: integer;
begin
  for i := 0 to extCount-1 do Images[i].Free;
end;

function FactionCIndex(AFaction: TFaction): integer;
begin
  if AFaction = nil then Result := attNeutral
  else if AFaction.Data.ColorIndex <> 0 then Result := AFaction.Data.ColorIndex
  else if AFaction.Attitude <> 0 then Result := AFaction.Attitude
  else if VFaction.Attitude <> 0 then Result := VFaction.Attitude
  else Result := attNeutral;
end;

function FactionColor(AFaction: TFaction): TColor;
begin
  Result := TColorExtra(ColorExtras[FactionCIndex(AFaction)]).Color;
end;

function SeekColorExtra(AColor: TColor): integer;
var i: integer;
begin
  i := 1;
  while (i < ColorExtras.Count) and (TColorExtra(ColorExtras[i]).Color <> AColor) do
    Inc(i);
  if i < ColorExtras.Count then Result := i
  else begin
    ColorExtras.Add(TColorExtra.Create(AColor));
    Result := ColorExtras.Count-1;
  end;
end;

procedure ClearColorExtras;
var i: integer;
begin
  for i := 0 to ColorExtras.Count-1 do TColorExtra(ColorExtras[i]).Free;
  ColorExtras.Free;
end;

 { Config supplement }

procedure LoadConfigColors;
var Trace: TTrace;
    i: integer;
begin
  // Get attitude colors from config. Should be before faction colors.
  ColorExtras.Add(TColorExtra.Create(0));
  Trace := TTrace.Create(Config.ReadString('Attitudes', 'Colors', ''));
  i := 1;
  while i <= 5 do begin
    if not Trace.Ends then ColorExtras.Add(TColorExtra.Create(Trace.Num))
    else ColorExtras.Add(TColorExtra.Create(clDefaultAttitudeColor));
    Trace.Before(', ');
    Inc(i);
  end;
  Trace.Free;
end;

procedure SaveConfigColors;
var A: array of string;
    i: integer;
begin
  // Write attitude colors
  SetLength(A, 5);
  for i := 1 to 5 do A[i-1] := IntToStr(TColorExtra(ColorExtras[i]).Color);
  Config.WriteString('Attitudes', 'Colors', MakeList(A));
end;

{ Battle Images}

procedure DrawBattleImage(Cnv: TCanvas; X, Y: integer; Filename: string;
  Color: TColor; Flip: boolean);
var Bmp, MaskBmp: TBitmap;
    s, img, mask: string;
    Trace: TTrace;

  function CreateUnitBmp(Fill: TColor): TBitmap;
  begin
    Result := TBitmap.Create;
    Result.Width := 32;
    Result.Height := 32;
    Result.Canvas.Brush.Color := Fill;
    Result.Canvas.FillRect(Result.Canvas.ClipRect);
  end;

begin
  if Filename = '' then begin
    // Default image
    Bmp := GetMaskedBmp(ResForm.BattleImages, 0, Color);
    DrawFlipped(Cnv, X, Y, Bmp, Flip);
    Bmp.Free;
  end
  else begin
    // Load custom graphics
    Bmp := CreateUnitBmp(clNether);
    Trace := TTrace.Create(Filename);
    while not Trace.Ends do begin
      s := Trace.Before(' ');
      if s <> '' then begin
        if Pos('+', s) > 0 then begin
          mask := Trim(Copy(s, Pos('+', s)+1, Length(s)));
          img := Trim(Copy(s, 1, Pos('+', s)-1));
          DrawExternalImage(BattleFolder + img, Bmp.Canvas, 0, 0, -1);
          MaskBmp := CreateUnitBmp(Color);
          DrawExternalImage(BattleFolder + mask, MaskBmp.Canvas, 0, 0, clWhite);
          MaskBmp.Transparent := True;
          Bmp.Canvas.Draw(0, 0, MaskBmp);
          MaskBmp.Free;
        end
        else DrawExternalImage(BattleFolder + s, Bmp.Canvas, 0, 0, -1);
      end;
    end;
    Trace.Free;
    DrawFlipped(Cnv, X, Y, Bmp, Flip);
    Bmp.Free;
  end;
end;

procedure DrawFileFlipped(Cnv: TCanvas; X, Y: integer; Filename: string;
  Flip: boolean);
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(Filename);
    DrawFlipped(Cnv, X, Y, Bmp, Flip);
  except
  end;
  Bmp.Free;
end;

procedure PrepareBmp(Bmp: TBitmap; Wid, Hei: integer; Fill: TColor);
begin
  Bmp.Width := Wid;
  Bmp.Height := Hei;
  Bmp.Canvas.Brush.Color := Fill;
  Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
end;


initialization
  BaseDir := ExtractFilePath(Application.ExeName);
  Config := TMemIniFile.Create(BaseDir + 'config.ini');
  Icons := TStringList.Create;
  PeasantExtras := TStringList.Create;
  ColorExtras := TList.Create;

finalization
  Config.UpdateFile;
  Config.Free;
  Icons.Free;
  PeasantExtras.Free;
  ColorExtras.Free;

end.
