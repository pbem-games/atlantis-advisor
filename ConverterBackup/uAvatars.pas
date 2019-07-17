unit uAvatars;

interface

uses
  Graphics, Forms, Classes, SysUtils, MyStrings, uGameSubs, DataStructs,
  Resources, uExport;


const
  PackInfoFile = 'pack.nfo';
  AvatarsFolder = 'Images\Avatars\';

  avNone = 0;
  avExists = 1;
  avWrong = 2;

  BinAvatarCount = 3;
  BinAvatars: array[0..BinAvatarCount-1] of string = ('elhana', 'flint', 'raistlin');

type
  TAvPack = class
    Images: TStrings;
    Name, Author, Short: string;
    constructor Create;
    destructor Destroy; override;
  end;

  TAvFilter = class
    NameMask, FactionMask, ParamMask: string;
    Image, Pack: string;
  end;


  TAvPackList = class(TList)
  protected
    function Get(Index: Integer): TAvPack;
    procedure Put(Index: Integer; Item: TAvPack);
  public
    property Items[Index: Integer]: TAvPack read Get write Put; default;
  end;

  TAvFilterList = class(TList)
  protected
    function Get(Index: Integer): TAvFilter;
    procedure Put(Index: Integer; Item: TAvFilter);
  public
    property Items[Index: Integer]: TAvFilter read Get write Put; default;
  end;


var
  AvPacks: TAvPackList;
  AvFilters: TAvFilterList;

  procedure AvatarToPath(Avatar: string; var Pack, Image: string);
  function ApplyFilter(Faction, Name, Params: string; var Pack, Image: string): boolean;
  function DrawAvatar(Pack, Image: string; ACanvas: TCanvas; X, Y: integer): boolean;
  function ExtractAvatar(Descr: string; var Pack, Image: string): boolean;
  function FindAvatar(AUnit: TUnit; var Pack, Image: string): boolean;
  procedure SetExportAvatars(AUnit: TUnit);
  procedure ReadFilters(Lines: TStrings);
  procedure SaveFilters(Lines: TStrings);
  function DrawCombinedAvatar(AUnit: TUnit; AFaction: TFaction; BattleImg: string;
    Bmp: TBitmap; Flip: boolean): boolean;

implementation

{$R resources.res}

var
  BaseDir: string;

function CompareMask(Mask, s: string): boolean;
begin
  Result := (Pos(Mask, s) > 0) or (Mask = '');
end;

// Find filter for unit strings
function ApplyFilter(Faction, Name, Params: string; var Pack, Image: string): boolean;
var i: integer;
begin
  Result := FALSE;
  i := 0;
  while (i < AvFilters.Count) and not (CompareMask(AvFilters[i].NameMask,
    Name) and CompareMask(AvFilters[i].FactionMask, Faction) and
    (CompareMask(AvFilters[i].ParamMask, Params))) do Inc(i);
  if (i < AvFilters.Count) then begin
    Pack := AvFilters[i].Pack;
    Image := AvFilters[i].Image;
    Result := TRUE;
  end;
end;

// Decode 'bin/elhana' to 'bin','elhana'
procedure AvatarToPath(Avatar: string; var Pack, Image: string);
var slash: integer;
begin
  slash := Pos('/', Avatar);
  Pack := Copy(Avatar, 1, slash - 1);
  Image := Copy(Avatar, slash + 1, Length(Avatar));
end;

// Draw avatar image onto canvas
function DrawAvatar(Pack, Image: string; ACanvas: TCanvas; X, Y: integer): boolean;
var Bitmap: TBitmap;
begin
  Bitmap := nil;
  if (Pack <> '') and (Image <> '') then begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.LoadFromFile(BaseDir + AvatarsFolder + Pack + '\' + Image + '.bmp');
    except
      on EFOpenError do FreeAndNil(Bitmap);
    end;
  end;
  with ACanvas do begin
    Brush.Color := clBtnFace;
    FillRect(Rect(X, Y, X + 50, Y + 50));
    if Bitmap <> nil then begin
      Draw(X, Y, Bitmap);
      Bitmap.Free;
      Result := TRUE;
    end
    else begin
      Bitmap := TBitmap.Create; // get pack icon from resource file,
                                // to avoid messing with Resources module
      Bitmap.LoadFromResourceName(HInstance, 'PackIcon');
      Bitmap.Transparent := TRUE;
      Draw(X+1, Y+8, Bitmap);
      if (Pack <> '') then TextOut(X+17, Y+10, Pack)
      else TextOut(X+17, Y+10, '?');
      TextOut(X+3, Y+24, 'no image');
      Bitmap.Free;
      Result := FALSE;
    end;
  end;
end;

// Cut avatar tag [#a bin/elhana] from unit's description
function ExtractAvatar(Descr: string; var Pack, Image: string): boolean;
var Trace: TTrace;
begin
  Image := ''; Pack := '';
  Trace := TTrace.Create(Descr);
  Descr := Trace.Before('[#a ');
  Pack := Trace.Before('/');
  Image := Trace.Before(']');
  Descr := Descr + Trace.Text;
  Trace.Free;
  Result := (Image <> '') and (Pack <> '');
end;

function FindFilteredAvatar(AUnit: TUnit; var Pack, Image: string): boolean;
begin
  Result := ApplyFilter(UnitString(AUnit, U_FACTION), UnitString(AUnit, U_NAME),
    UnitString(AUnit, U_FLAGS + U_ITEMS + U_SKILLS + U_COMBAT + U_CANSTUDY),
    Pack, Image);
end;

// Determine if unit in question has filtered or imported avatar
//  (filters has higher priority)
function FindAvatar(AUnit: TUnit; var Pack, Image: string): boolean;
begin
  if FindFilteredAvatar(AUnit, Pack, Image) then Result := TRUE
  else if ExtractAvatar(AUnit.Description, Pack, Image) then Result := TRUE
  else Result := FALSE;
end;

// Find avatar images in Avatars subdir. Should be called once.
procedure LoadAvPacks;
var sr: TSearchRec;

  procedure MakeAvPack(AName: string);
  var sr: TSearchRec;
      NewPack: TAvPack;
      Lines: TStrings;
      Trace: TTrace;
  begin
    NewPack := TAvPack.Create;
    AvPacks.Add(NewPack);

   // Load pack info
    with NewPack do begin
      Name := AName;
      Short := AName;
      if FileExists(BaseDir + AvatarsFolder + AName + '\' + PackInfoFile) then begin
        Lines := TStringList.Create;
        Lines.LoadFromFile(BaseDir + AvatarsFolder + AName + '\' + PackInfoFile);
        if Lines.Count > 0 then begin
          Trace := TTrace.Create(Lines[0]);
          Lines.Free;
          Name := Trace.Before(' [');
          if not (Trace.Ends) then Short := Trace.Before('] by ');
          if not (Trace.Ends) then Author := Trace.Before('');
          Trace.Free;
        end;
      end;

     // Load images
      if FindFirst(BaseDir + AvatarsFolder + AName + '\*.bmp', faAnyFile, sr) = 0 then begin
        Images.Add(Copy(sr.Name, 1, Pos('.', sr.Name)-1));
        while FindNext(sr) = 0 do Images.Add(Copy(sr.Name, 1, Pos('.', sr.Name)-1));
      end;
      FindClose(sr);
    end;
  end;

begin
  if FindFirst(BaseDir + AvatarsFolder + '*.*', faDirectory, sr) = 0 then begin
    if (sr.Attr = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
      MakeAvPack(sr.Name);
    while FindNext(sr) = 0 do
      if (sr.Attr = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
        MakeAvPack(sr.Name);
  end;
  FindClose(sr);
end;

procedure FilterParser(Trace: TTrace);
var AvFilter: TAvFilter;
begin
  AvFilter := TAvFilter.Create;
  with AvFilter do begin
    Trace.Before('"');
    Pack := Trace.Before('/');
    Image := Trace.Before('", ');
    NameMask := Trace.QBlock;
    FactionMask := Trace.QBlock;
    ParamMask := Trace.QBlock;
  end;
  AvFilters.Add(AvFilter);
end;

// Read filters from game.dat
procedure ReadFilters(Lines: TStrings);
var line: integer;
begin
  line := 0;
  ReadDataList('[Avatars]', Lines, line, FilterParser);
end;

// Write filters to game.dat
procedure SaveFilters(Lines: TStrings);
var i: integer;
    s: string;
begin
  Lines.Add('[Avatars]');
  for i := 0 to AvFilters.Count-1 do begin
    with AvFilters[i] do
      FmtStr(s, '"%s/%s", "%s", "%s", "%s"', [Pack, Image, NameMask, FactionMask,
        ParamMask]);
    Lines.Add(s);
  end;
  Lines.Add('');
end;

procedure SetExportAvatars(AUnit: TUnit);
var pack, img, new_desc: string;
    i: integer;
    desc_orders: boolean;
begin
  if AUnit.Faction.Num <> VFaction.Num then Exit;
  // Remove describe orders
  desc_orders := False;
  i := 0;
  while i < AUnit.Orders.Count do begin
    if Pos('describe unit', AUnit.Orders[i]) in [1..2] then begin
      AUnit.Orders.Delete(i);
      desc_orders := True;
    end
    else Inc(i);
  end;
  // Include tags if needed
  if FindFilteredAvatar(AUnit, pack, img)
    and Config.ReadBool('ExportPacks', pack, False) then begin
      new_desc := IncludeTags(StripTags(AUnit.Description), '[#a ' + pack +
        '/' + img + ']');
      if (AUnit.Description <> new_desc) then begin
         AUnit.Description := new_desc;
        desc_orders := True;
      end;
  end
  else if AUnit.Description <> StripTags(AUnit.Description) then begin
    AUnit.Description := StripTags(AUnit.Description);
    desc_orders := True;
  end;
  // Make correct describe order
  if desc_orders then
    AUnit.Orders.Add('describe unit "' + AUnit.Description + '"');
end;

function DrawCombinedAvatar(AUnit: TUnit; AFaction: TFaction; BattleImg: string;
  Bmp: TBitmap; Flip: boolean): boolean;
var pack, img: string;
begin
  Result := False;
  PrepareBmp(Bmp, 50, 50, clBtnFace);
  if (AUnit <> nil) and FindAvatar(AUnit, pack, img) then begin
    DrawAvatar(pack, img, Bmp.Canvas, 0, 0);
    Result := True;
  end
  else begin
    if BattleImg = '-' then Exit;
    DrawBattleImage(Bmp.Canvas, 9, 9, BattleImg, FactionColor(AFaction), Flip);
    Result := True;
  end;
end;


{ TAvPack }

constructor TAvPack.Create;
begin
  Images := TStringList.Create;
end;

destructor TAvPack.Destroy;
begin
  Images.Free;
end;


  { Lists }

function TAvPackList.Get(Index: integer): TAvPack;
begin
  Result := TAvPack(inherited Get(Index));
end;

procedure TAvPackList.Put(Index: integer; Item: TAvPack);
begin
  inherited Put(Index, Item);
end;

function TAvFilterList.Get(Index: integer): TAvFilter;
begin
  Result := TAvFilter(inherited Get(Index));
end;

procedure TAvFilterList.Put(Index: integer; Item: TAvFilter);
begin
  inherited Put(Index, Item);
end;


initialization
  BaseDir := ExtractFilePath(Application.ExeName);
  AvPacks := TAvPackList.Create;
  AvFilters := TAvFilterList.Create;
  LoadAvPacks;

finalization
  AvPacks.Free;
  AvFilters.Free;

end.

