unit uUnitNeeds;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, StdCtrls, ComCtrls, ToolWin, IntEdit,
  Grids, PowerGrid, MyStrings, uGameSubs, uInterface, uNeeds;

type
  TUnitNeedsForm = class(TForm)
    lMen: TLabel;
    btnOk: TButton;
    Button2: TButton;
    gAvai: TPowerGrid;
    cbEquipment: TCheckBox;
    cbNoGive: TCheckBox;
    Grid: TPowerGrid;
    eEquipPriority: TIntEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gAvaiDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure gAvaiMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gAvaiDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure gAvaiDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cbEquipmentClick(Sender: TObject);
    procedure cbNoGiveClick(Sender: TObject);
    procedure eEquipPriorityChange(Sender: TObject);
  private
    Filling: boolean;
    Needs: TNeedsList;
    procedure FillGrid;
    procedure FillAvailable;
  public
  end;

var
  UnitNeedsForm: TUnitNeedsForm;

implementation

{$R *.dfm}

const Masks: array[0..7] of string = ('items', 'weapons', 'usable weapons',
  'armor', 'mounts', 'trade', 'spoils', 'materials');

{ TUnitNeedsForm }

procedure TUnitNeedsForm.FormCreate(Sender: TObject);
var men: integer;
begin
  gAvai.Cols[0].Format := cfNumber;
  Needs := TNeedsList.Create;

  Grid.Cells[0, 0] := 'Amount';
  Grid.Cells[1, 0] := 'Unnd';
  Grid.Cells[2, 0] := 'Item';
  Grid.Cells[3, 0] := 'Priority';

  if CurrUnit <> nil then begin
    Caption := Caption + ' - ' + CurrUnit.Name;

    men := CurrUnit.Items.Amount(IT_MAN);
    lMen.Caption := IntToStr(men);
    if IsLeader(CurrUnit.Items) then
      lMen.Caption := lMen.Caption + Numeric(' leader', men)
    else if men > 1 then lMen.Caption := lMen.Caption + ' men'
    else lMen.Caption := lMen.Caption + ' man';

    ReadNeeds(CurrUnit, Needs);
    FillGrid;
    FillAvailable;
  end;
end;

procedure TUnitNeedsForm.FormDestroy(Sender: TObject);
begin
  Needs.ClearAndFree;
end;

procedure TUnitNeedsForm.FillGrid;
var i, row: integer;
begin
  Filling := True;

  cbEquipment.Checked := False;
  cbNoGive.Checked := False;
  Grid.RowCount := 1;
  for i := 0 to Needs.Count-1 do begin
    if Needs[i].Equipment then begin
      cbEquipment.Checked := True;
      eEquipPriority.Value := Needs[i].Priority;
    end
    else if Needs[i].NoGive then cbNoGive.Checked := True
    else begin
      row := Grid.RowCount;
      // Amount
      Grid.Cells[0, row] := IntToStr(Needs[i].Amount);
      // Unneeded
      if Needs[i].Unneeded then Grid.Cells[1, row] := 'x'
      else Grid.Cells[1, row] := '';
      // Item
      if Needs[i].Data = nil then begin
        Grid.Cells[2, row] := Needs[i].MaskText;
        if Needs[i].Usable then
          Grid.Cells[2, row] := 'usable ' + Grid.Cells[2, row];
      end
      else Grid.Cells[2, row] := Needs[i].Data.Name(Needs[i].Amount <> 1);
      // Priority
      Grid.Cells[3, row] := IntToStr(Needs[i].Priority);

      Grid.Rows[row].Data := Needs[i];
    end;
  end;
  Grid.Fixup;
  Filling := False;
end;

procedure TUnitNeedsForm.FillAvailable;
var i, row: integer;
    IData: TItemData;
begin
  gAvai.RowCount := 0;

  for i := 0 to High(Masks) do begin
    row := gAvai.RowCount;
    gAvai.Cells[0, row] := Masks[i];
    gAvai.SortKeys[0, row] := IntToStr(i - Length(Masks));
    gAvai.Rows[row].FontStyle := [fsBold];
  end;

  for i := 0 to Game.ItemData.Count-1 do begin
    IData := Game.ItemData[i];
    if not Test(IData.Flags, IT_MAN + IT_MONSTER) then begin
      row := gAvai.RowCount;
      gAvai.Cells[0, row] := Game.ItemData[i].Name;
      gAvai.SortKeys[0, row] := IntToStr(i);
      gAvai.Rows[row].Data := Game.ItemData[i];
    end;
  end;
  gAvai.Fixup;
end;

procedure TUnitNeedsForm.btnOkClick(Sender: TObject);
begin
  SetNeeds(CurrUnit, Needs);
end;

procedure TUnitNeedsForm.gAvaiDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with gAvai do begin
    if TItemData(ImgRows[ARow].Data) <> nil then
      DrawItemIcon(Canvas, TxtRect.Left + 2, TxtRect.Top,
        TItemData(ImgRows[ARow].Data));
    Inc(TxtRect.Left, 20);
  end;
end;

procedure TUnitNeedsForm.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Rec: TNeedItem;
    P: TGridCoord;
    ARect: TRect;
    Cur: TPoint;
begin
  P := Grid.MouseCell;
  if (P.X < 0) or (P.Y < 0) then Exit;
  Rec := Grid.ImgRows[P.Y].Data;
  if Rec = nil then Exit;

  ARect := Grid.CellRect(P.X, P.Y);
  Cur := Grid.ScreenToClient(Mouse.CursorPos);
  Grid.Editing := False;

  case P.X of
    0: begin
      if (Cur.X >= ARect.Right - 21) and (Cur.X <= ARect.Right - 11) then begin
        if Rec.Amount >= 0 then begin
          Rec.Amount := -1;
          Rec.EnoughAmt := False;
        end
        else Rec.Amount := 0;
        FillGrid;
      end
      else if (Cur.X >= ARect.Right - 10) and (Cur.X <= ARect.Right) then begin
        Rec.EnoughAmt := not Rec.EnoughAmt;
        Rec.Amount := 0;
        FillGrid;
      end
      else begin
        if Rec.Amount < 0 then Rec.Amount := 0;
        Rec.EnoughAmt := False;
        FillGrid;
        Grid.Editing := True;
      end;
    end;
    1: begin
      Rec.Unneeded := not Rec.Unneeded;
      FillGrid;
    end;
    3: Grid.Editing := True;
  end;
  Grid.BeginDrag(False);
end;

procedure TUnitNeedsForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var Rec: TNeedItem;

  procedure DrawButton(x: integer; Text: string; Down: boolean);
  var left, right: integer;
      clBrush, clFont: TColor;
  begin
    left := TxtRect.Right - 21 + x;
    right := left + 10;
    with Grid.Canvas do begin
      clBrush := Brush.Color;
      clFont := Font.Color;
      MoveTo(right, TxtRect.Top);
      if Down then Pen.Color := clBtnHighlight
      else Pen.Color := clBtnShadow;
      LineTo(right, TxtRect.Bottom-1);
      LineTo(left, TxtRect.Bottom-1);
      if Down then Pen.Color := clBtnShadow
      else Pen.Color := clBtnHighlight;
      LineTo(left, TxtRect.Top);
      LineTo(right, TxtRect.Top);
      Font.Color := clBlack;
      if not Down then Brush.Color := clBtnFace
      else Brush.Color := clBtnHighlight;
      TextRect(Rect(left+1, TxtRect.Top+1, right, TxtRect.Bottom-1),
        left+2, TxtRect.Top, Text);
      Brush.Color := clBrush;
      Font.Color := clFont;
    end;
  end;

begin
  with Grid do begin
    Rec := TNeedItem(ImgRows[ARow].Data);
    if Rec = nil then Exit;

    if ACol = 0 then begin
      DrawButton(0, 'a', (Rec.Amount = -1));
      DrawButton(11, 'e', Rec.EnoughAmt);
      if (Rec.Amount = -1) or Rec.EnoughAmt then TxtRect.Right := TxtRect.Left
      else Dec(TxtRect.Right, 21);
    end;
    if ACol = 1 then begin
      if ImgCells[ACol, ARow] <> '' then
        ResForm.IconList.Draw(Canvas, TxtRect.Left + 7, TxtRect.Top, bmpCheckOn)
      else ResForm.IconList.Draw(Canvas, TxtRect.Left + 7, TxtRect.Top, bmpCheckOff);
      TxtRect.Left := TxtRect.Right;
    end;
    if ACol = 2 then begin
      if Rec.Data <> nil then begin
        DrawItemIcon(Canvas, TxtRect.Left, TxtRect.Top,
          TNeedItem(ImgRows[ARow].Data).Data);
        Inc(TxtRect.Left, 18);
      end
      else begin
        Canvas.Font.Style := [fsBold];
        Canvas.TextRect(TxtRect, TxtRect.Left + 2, TxtRect.Top + 2,
          ImgCells[ACol, ARow]);
        Canvas.Font.Style := [];
        TxtRect.Left := TxtRect.Right;
      end;
    end;
  end;
end;

procedure TUnitNeedsForm.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var Rec: TNeedItem;
begin
  Rec := TNeedItem(Grid.ImgRows[ARow].Data);
  if Rec = nil then Exit;

  case ACol of
    0: Rec.Amount := ToInt(Value);
    3: Rec.Priority := ToInt(Value);
  end;
end;

procedure TUnitNeedsForm.gAvaiMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  gAvai.BeginDrag(False);
end;

procedure TUnitNeedsForm.GridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = gAvai);
end;

procedure TUnitNeedsForm.gAvaiDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Grid);
end;

procedure TUnitNeedsForm.GridDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var Rec: TNeedItem;
    i: integer;
begin
  if (Source <> gAvai) or (gAvai.RowCount = 0) then Exit;

  Rec := TNeedItem.Create;
  Rec.AUnit := CurrUnit;
  Rec.Data := gAvai.Rows[gAvai.Row].Data;
  if Rec.Data = nil then begin
    i := High(Masks);
    while (i >= 0) and (Masks[i] <> gAvai.Cells[0, gAvai.Row]) do Dec(i);
    if i >= 0 then Rec.MaskText := gAvai.Cells[0, gAvai.Row];
  end;
  Needs.Add(Rec);
  FillGrid;
end;

procedure TUnitNeedsForm.gAvaiDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var Rec: TNeedItem;
begin
  if (Source <> Grid) or (Grid.RowCount = 1) then Exit;

  Rec := TNeedItem(Grid.Rows[Grid.Row].Data);
  Needs.Delete(Needs.IndexOf(Rec));
  Rec.Free;
  FillGrid;
end;

procedure TUnitNeedsForm.cbEquipmentClick(Sender: TObject);
var i: integer;
    Rec: TNeedItem;
begin
  if Filling then Exit;
  if cbEquipment.Checked then begin
    Rec := TNeedItem.Create;
    Rec.AUnit := CurrUnit;
    Rec.Equipment := True;
    Rec.Priority := eEquipPriority.Value;
    Needs.Add(Rec);
  end
  else begin
    i := 0;
    while (i < Needs.Count) do
      if Needs[i].Equipment then begin
        Needs[i].Free;
        Needs.Delete(i);
      end
      else Inc(i);
  end;
end;

procedure TUnitNeedsForm.cbNoGiveClick(Sender: TObject);
var i: integer;
    Rec: TNeedItem;
begin
  if Filling then Exit;
  if cbNoGive.Checked then begin
    Rec := TNeedItem.Create;
    Rec.AUnit := CurrUnit;
    Rec.NoGive := True;
    Needs.Add(Rec);
  end
  else begin
    i := 0;
    while (i < Needs.Count) do
      if Needs[i].NoGive then begin
        Needs[i].Free;
        Needs.Delete(i);
      end
      else Inc(i);
  end;
end;

procedure TUnitNeedsForm.eEquipPriorityChange(Sender: TObject);
var i: integer;
begin
  if Filling then Exit;
  for i := 0 to Needs.Count-1 do
    if Needs[i].Equipment then Needs[i].Priority := eEquipPriority.Value;
end;

end.
