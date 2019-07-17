unit uListFilter;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Resources, CheckLst, DataStructs;

type
  TListFilterForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ItemBox: TCheckListBox;
    CheckBox1: TCheckBox;
    procedure ItemBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ListFilterForm: TListFilterForm;

implementation

{$R *.lfm}

procedure TListFilterForm.ItemBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var idx: integer;
begin
  ItemBox.Canvas.FillRect(Rect);
  idx := Index;
  if idx = bmpTools + 1 then idx := bmpTradeGoods;
  ResForm.IconList.Draw(ItemBox.Canvas, Rect.Left, Rect.Top, idx);
  ItemBox.Canvas.TextOut(Rect.Left + 18, Rect.Top + 1, ItemBox.Items[Index]);
end;

procedure TListFilterForm.FormCreate(Sender: TObject);
var Filter: DWord;
begin
  Filter := Config.ReadInteger('Map', 'ListFilter', IT_ALL);
  with ItemBox do begin
    Checked[bmpUnknownItem] := (Filter and IT_UNKNOWN <> 0);
    Checked[bmpMen] := (Filter and IT_MAN <> 0);
    Checked[bmpMonsters] := (Filter and IT_MONSTER <> 0);
    Checked[bmpMagic] := (Filter and IT_MAGIC <> 0);
    Checked[bmpWeapon] := (Filter and IT_WEAPON <> 0);
    Checked[bmpArmor] := (Filter and IT_ARMOR <> 0);
    Checked[bmpFood] := (Filter and IT_FOOD <> 0);
    Checked[bmpMounts] := (Filter and IT_MOUNT <> 0);
    Checked[bmpResources] := (Filter and IT_RESOURCE <> 0);
    Checked[bmpAdvanced] := (Filter and IT_ADVANCED <> 0);
    Checked[bmpTools] := (Filter and IT_TOOL <> 0);
    Checked[bmpTools+1] := (Filter and IT_TRADE <> 0);
  end;
end;

procedure TListFilterForm.FormDestroy(Sender: TObject);
var Filter: DWord;
begin
  if ModalResult = mrOk then
    with ItemBox do begin
      Filter := 0;
      if Checked[bmpUnknownItem] then Filter := Filter or IT_UNKNOWN;
      if Checked[bmpMen] then Filter := Filter or IT_MAN;
      if Checked[bmpMonsters] then Filter := Filter or IT_MONSTER;
      if Checked[bmpMagic] then Filter := Filter or IT_MAGIC;
      if Checked[bmpWeapon] then Filter := Filter or IT_WEAPON;
      if Checked[bmpArmor] then Filter := Filter or IT_ARMOR;
      if Checked[bmpFood] then Filter := Filter or IT_FOOD;
      if Checked[bmpMounts] then Filter := Filter or IT_MOUNT;
      if Checked[bmpResources] then Filter := Filter or IT_RESOURCE;
      if Checked[bmpAdvanced] then Filter := Filter or IT_ADVANCED;
      if Checked[bmpTools] then Filter := Filter or IT_TOOL;
      if Checked[bmpTools+1] then Filter := Filter or IT_TRADE;
      Config.WriteInteger('Map', 'ListFilter', Filter);
    end;
end;

procedure TListFilterForm.CheckBox1Click(Sender: TObject);
var i: integer;
begin
  for i := 0 to ItemBox.Items.Count-1 do ItemBox.Checked[i] := CheckBox1.Checked;
end;

end.
