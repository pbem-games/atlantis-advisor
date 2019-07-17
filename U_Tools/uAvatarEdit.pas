unit uAvatarEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, DataStructs, Grids, PowerGrid, StdCtrls, Buttons, ComCtrls, MyStrings,
  Resources, ExtCtrls, uAvatars, ToolWin, uGameSubs, CheckLst;

const
  FilterGridColCount = 3;
  FilterGridCols: array[0..FilterGridColCount-1] of string = ('Faction',
    'Unit name', 'Unit parameters');

type
  TAvatarForm = class(TForm)
    CloseBtn: TButton;
    FilterGrid: TStringGrid;
    ShortLabel: TLabel;
    AuthorLabel: TLabel;
    PackGroupBox: TGroupBox;
    ImgGrid: TDrawGrid;
    Label1: TLabel;
    Image: TImage;
    Bevel1: TBevel;
    PackLabel: TLabel;
    Bevel2: TBevel;
    Image1: TImage;
    FilterPackLabel: TLabel;
    Image2: TImage;
    Label3: TLabel;
    lbPacks: TCheckListBox;
    CreateBtn: TSpeedButton;
    DeleteBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    UpBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FilterGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure CreateBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FilterGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; State: TGridDrawState);
    procedure FilterGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure DownBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure ImgGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      TxtRect: TRect; State: TGridDrawState);
    procedure PackDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PackDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ImgGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure lbPacksDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbPacksClick(Sender: TObject);
  private
    CurrPack: TAvPack;
    procedure FillFilterGrid;
    procedure FillImgs;
  public
    procedure MakeUnitAvatar(AUnit: TUnit);
  end;

var
  AvatarForm: TAvatarForm;

implementation

{$R *.DFM}

procedure TAvatarForm.FormCreate(Sender: TObject);
var i: integer;
begin
  if ProgOpened then FilterGrid.Height := 143
  else FilterGrid.Height := 121;

  // Fill pack box
  lbPacks.Sorted := False;
  for i := 0 to AvPacks.Count-1 do
    if ProgOpened or (AvPacks[i].Short = 'bin') then begin
      lbPacks.AddItem(AvPacks[i].Name, AvPacks[i]);
      lbPacks.Checked[lbPacks.Items.Count-1] :=
        Config.ReadBool('ExportPacks', AvPacks[i].Short, False);
    end;
  lbPacks.Sorted := True;
  lbPacks.ItemIndex := 0;
  lbPacksClick(lbPacks);

  // Setup avatars grid
  for i := 0 to FilterGridColCount-1 do
    FilterGrid.Cells[i, 0] := FilterGridCols[i];
  FillFilterGrid;
end;

procedure TAvatarForm.FormClose(Sender: TObject; var Action: TCloseAction);
var i: integer;
begin
  // Remove empty avatars
  i := 0;
  while i < AvFilters.Count do
    if (AvFilters[i].NameMask = '') and (AvFilters[i].FactionMask = '') and
      (AvFilters[i].ParamMask = '') then AvFilters.Delete(i)
    else Inc(i);
  // Store exported packs
  for i := 0 to lbPacks.Items.Count-1 do
    if lbPacks.Checked[i] and ProgOpened then
      Config.WriteBool('ExportPacks', TAvPack(lbPacks.Items.Objects[i]).Short, True)
    else Config.DeleteKey('ExportPacks', TAvPack(lbPacks.Items.Objects[i]).Short);
end;


procedure TAvatarForm.MakeUnitAvatar(AUnit: TUnit);
var af: TAvFilter;
begin
  if AUnit = nil then Exit;
  af := TAvFilter.Create;
  af.NameMask := AUnit.Name + ' (' + IntToStr(AUnit.Num) + ')';
  if AUnit.Faction.Num > 0 then
    af.FactionMask := AUnit.Faction.Name + ' (' + IntToStr(AUnit.Faction.Num) + ')';
  AvFilters.Insert(0, af);
  FillFilterGrid;
end;

procedure TAvatarForm.CreateBtnClick(Sender: TObject);
begin
  AvFilters.Add(TAvFilter.Create);
  FillFilterGrid;
end;

procedure TAvatarForm.DeleteBtnClick(Sender: TObject);
begin
  AvFilters.Delete(FilterGrid.Row - 1);
  FillFilterGrid;
end;

procedure TAvatarForm.UpBtnClick(Sender: TObject);
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  with FilterGrid do begin
    if Row > 1 then begin
      Lines.Assign(Rows[Row-1]);
      Rows[Row-1].Assign(Rows[Row]);
      Rows[Row].Assign(Lines);
      AvFilters.Exchange(Row-2, Row-1);
      Row := Row - 1;
    end;
  end;
  Lines.Free;
end;

procedure TAvatarForm.DownBtnClick(Sender: TObject);
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  with FilterGrid do begin
    if Row < RowCount-1 then begin
      Lines.Assign(Rows[Row+1]);
      Rows[Row+1].Assign(Rows[Row]);
      Rows[Row].Assign(Lines);
      AvFilters.Exchange(Row-1, Row);
      Row := Row + 1;
    end;
  end;
  Lines.Free;
end;

procedure TAvatarForm.FillFilterGrid;
var i: integer;
    b: boolean;
begin
  with FilterGrid do begin
    if AvFilters.Count = 0 then AvFilters.Add(TAvFilter.Create);
    RowCount := AvFilters.Count + 1;
    for i := 0 to AvFilters.Count-1 do
      with AvFilters[i] do begin
        Cells[0, i+1] := FactionMask;
        Cells[1, i+1] := NameMask;
        Cells[2, i+1] := ParamMask;
      end;
    FilterGridSelectCell(Self, Col, Row, b);
  end;
end;

procedure TAvatarForm.FilterGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; ARect: TRect; State: TGridDrawState);
begin
  with FilterGrid do begin
    if ARow > 0 then begin
      if ARow = Row then begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else begin
        Canvas.Brush.Color := clWindow;
        Canvas.Font.Color := clWindowText;
      end;
    end;
    Canvas.FillRect(ARect);
    Canvas.TextRect(ARect, ARect.Left+2, ARect.Top+2, Cells[ACol, ARow]);
  end;
end;

procedure TAvatarForm.FilterGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var i: integer;
begin
  DrawAvatar(AvFilters[ARow-1].Pack, AvFilters[ARow-1].Image,
    Image.Canvas, 0, 0);
  i := 0;
  while (i < AvPacks.Count) and (AvFilters[ARow-1].Pack <> AvPacks[i].Short) do
    Inc(i);
  if i < AvPacks.Count then FilterPackLabel.Caption := AvPacks[i].Name
  else FilterPackLabel.Caption := AvFilters[ARow-1].Pack;
  FilterGrid.Invalidate;
end;

procedure TAvatarForm.FilterGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  case ACol of
    0: AvFilters[ARow-1].FactionMask := Value;
    1: AvFilters[ARow-1].NameMask := Value;
    2: AvFilters[ARow-1].ParamMask := Value;
  end;
end;

procedure TAvatarForm.FillImgs;
begin
  ImgGrid.Selection := TGridRect(Rect(0, 0, 0, 0));
  ImgGrid.RowCount := (CurrPack.Images.Count div ImgGrid.ColCount) + 1;
  ImgGrid.Repaint;
  ShortLabel.Caption := CurrPack.Short;
  if CurrPack.Author <> '' then AuthorLabel.Caption := ' by ' + CurrPack.Author
  else AuthorLabel.Caption := '';
  AuthorLabel.Left := ShortLabel.Left + ShortLabel.Width;
end;

procedure TAvatarForm.ImgGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; TxtRect: TRect; State: TGridDrawState);
var Index: integer;
begin
  if CurrPack = nil then Exit;
  with Sender as TDrawGrid do begin
    if (ARow = Row) and (ACol = Col) then Canvas.Pen.Color := clHighlight
    else Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 3;
    Canvas.Brush.Color := clBtnFace;
    Canvas.Polygon([Point(TxtRect.Left + 3, TxtRect.Top + 3),
      Point(TxtRect.Right - 4, TxtRect.Top + 3),
      Point(TxtRect.Right - 4, TxtRect.Bottom - 4),
      Point(TxtRect.Left + 3, TxtRect.Bottom - 4)]);
    Index := ARow * ColCount + ACol;
    if (Index < CurrPack.Images.Count) then
      DrawAvatar(CurrPack.Short, CurrPack.Images[Index], Canvas,
        TxtRect.Left + 10, TxtRect.Top + 10);
  end;
end;

procedure TAvatarForm.ImgGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var Index: integer;
    b: boolean;
begin
  Index := ARow * ImgGrid.ColCount + ACol;
  if Index >= CurrPack.Images.Count then CanSelect := FALSE
  else if FilterGrid.Row-1 < AvFilters.Count then begin
    with AvFilters[FilterGrid.Row-1] do begin
      Pack := CurrPack.Short;
      Image := CurrPack.Images[Index];
    end;
    FilterGridSelectCell(Self, FilterGrid.Col, FilterGrid.Row, b);
  end;
end;

procedure TAvatarForm.PackDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListView) and (Source <> Sender);
end;

procedure TAvatarForm.PackDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  with TListView(Sender).Items.Add do begin
    Caption := TListView(Source).Selected.Caption;
    Data := TListView(Source).Selected.Data;
    ImageIndex := bmpPack + TListView(Sender).Tag;
    Selected := TRUE;
  end;
  with TListView(Source) do begin
    Selected.Delete;
    Arrange(arDefault);
  end;
end;

procedure TAvatarForm.lbPacksDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.IconList.Draw(Canvas, Rect.Left+1, Rect.Top, bmpPack);
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TAvatarForm.lbPacksClick(Sender: TObject);
var author: string;
begin
  if lbPacks.ItemIndex >= 0 then begin
    CurrPack := TAvPack(lbPacks.Items.Objects[lbPacks.ItemIndex]);
    if CurrPack.Author <> '' then author := ' by ' + CurrPack.Author
    else author := '';
    PackLabel.Caption := CurrPack.Short + author;
    FillImgs;
  end;
end;

end.
