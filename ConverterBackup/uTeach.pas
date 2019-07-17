unit uTeach;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, PowerGrid, StdCtrls, Buttons, DataStructs, Resources,
  ComCtrls, ToolWin, uGameSubs, MyStrings;

type
  TTeachForm = class(TForm)
    TeachGrid: TPowerGrid;
    Button2: TButton;
    btnOk: TButton;
    ToolBar1: TToolBar;
    btnMyFaction: TToolButton;
    btnStudents: TToolButton;
    lStudents: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TeachGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure TeachGridClick(Sender: TObject);
    procedure btnMyFactionClick(Sender: TObject);
    procedure btnStudentsClick(Sender: TObject);
  private
    StudentNums: string;
    Students: integer;
    procedure FillGrid(Study: boolean);
  public
    function GetOrders: string;
  end;

var
  TeachForm: TTeachForm;

implementation

{$R *.dfm}

procedure TTeachForm.FormCreate(Sender: TObject);
var i, j: integer;
    U: TUnit;
begin
  TeachGrid.LoadColumns(Config);
  TeachGrid.Cols[0].CanHide := False;
  TeachGrid.Cols[0].CanMove := False;
  // Find teach order
  StudentNums := ' ';
  for i := 0 to CurrUnit.Orders.Count-1 do
    if CurrUnit.Order(i) = 'teach' then
      StudentNums := Uncomment(Copy(CurrUnit.Orders[i], Pos('teach',
        CurrUnit.Orders[i]) + Length('teach'), Length(CurrUnit.Orders[i]))) + ' ';

  // Count students
  Students := 0;
  for i := 0 to CurrRegion.Troops.Count-1 do
    for j := 0 to CurrRegion.Troops[i].Units.Count-1 do begin
      U := CurrRegion.Troops[i].Units[j];
      if Pos(' ' + U.NumStr + ' ', StudentNums) > 0 then
        Inc(Students, U.Items.Amount(IT_MAN));
    end;
  lStudents.Caption := 'Students: ' + IntToStr(students) + ' of ' +
    IntToStr(CurrUnit.Items.Amount(IT_MAN) * 10);

  TeachGrid.Filters.New(2, relEQ, IntToStr(VFaction.Num));
  btnMyFaction.Down := True;
  btnStudents.Down := True;
  FillGrid(True);
end;

procedure TTeachForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TeachGrid.SaveColumns(Config);
end;

procedure TTeachForm.FillGrid(Study: boolean);
var i, j, k, row: integer;
    U: TUnit;
    s1: string;
begin
  row := 1;
  TeachGrid.RowCount := 1;
  for i := 0 to CurrRegion.Troops.Count-1 do begin
    for j := 0 to CurrRegion.Troops[i].Units.Count-1 do begin
      U := CurrRegion.Troops[i].Units[j];
      if (U <> CurrUnit) and (not Study or (Pos('study', U.MonthOrder)
        in [1, 2]) or (Pos(' ' + U.NumStr + ' ', StudentNums) > 0)) then begin
        TeachGrid.Cells[1, row] := U.Faction.Name;
        TeachGrid.Cells[2, row] := IntToStr(U.Faction.Num);
        TeachGrid.Cells[3, row] := U.Name;
        TeachGrid.Cells[4, row] := U.NumStr;
        s1 := '';
        for k := 0 to U.Items.Count-1 do
          if Test(U.Items[k].Data.Flags, IT_MAN) then begin
            if s1 <> '' then s1 := s1 + ',';
            s1 := s1 + ' ' + U.Items[k].Name;
          end;
        TeachGrid.Cells[5, row] := IntToStr(U.Items.Amount(IT_MAN)) + s1;
        TeachGrid.Cells[6, row] := U.MonthOrder;
        TeachGrid.Rows[row].Color := FactionColor(U.Faction);
        TeachGrid.Rows[row].Data := U;
        Inc(row);
      end;
    end;
  end;
  TeachGrid.Fixup;
end;

procedure TTeachForm.TeachGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
var U: TUnit;
begin
  with Sender as TPowerGrid do begin
    U := ImgRows[ARow].Data;
    if U = nil then Exit;
    if ACol = 0 then begin
      if Pos(' ' + U.NumStr + ' ', StudentNums) = 0 then
        ResForm.IconList.Draw(Canvas, TxtRect.Left, TxtRect.Top, bmpCheckOff)
      else ResForm.IconList.Draw(Canvas, TxtRect.Left, TxtRect.Top, bmpCheckOn);
      TxtRect.Right := TxtRect.Left;
    end
    else if Cols[6].ImgCol = ACol then begin
      if (Pos(' ' + U.NumStr + ' ', StudentNums) = 0)
        and (ClearOrder(U.MonthOrder) = 'study')
        and (U.MonthInfo.Details = '') then
        Canvas.Font.Style := [fsBold];
    end;
  end;
end;

procedure TTeachForm.TeachGridClick(Sender: TObject);
var U: TUnit;
    i: integer;
begin
  with TeachGrid do
    if MouseCell.X = 0 then begin
      U := ImgRows[MouseCell.Y].Data;
      i := Pos(' ' + U.NumStr + ' ', StudentNums);
      if i = 0 then begin
        StudentNums := StudentNums + U.NumStr + ' ';
        Inc(Students, U.Items.Amount(IT_MAN))
      end
      else begin
        StudentNums := Copy(StudentNums, 1, i-1) +
          Copy(StudentNums, i + Length(' ' + U.NumStr), Length(StudentNums));
        Dec(Students, U.Items.Amount(IT_MAN));
      end;

      lStudents.Caption := 'Students: ' + IntToStr(students) + ' of ' +
        IntToStr(CurrUnit.Items.Amount(IT_MAN) * 10);
      Invalidate;  
    end;
end;

function TTeachForm.GetOrders: string;
begin
  if Trim(StudentNums) <> '' then Result := 'teach ' + Trim(StudentNums);
end;

procedure TTeachForm.btnMyFactionClick(Sender: TObject);
var i: integer;
begin
  if btnMyFaction.Down then
    TeachGrid.Filters.New(2, relEQ, IntToStr(Faction.Num))
  else begin
    i := 0;
    while (i < TeachGrid.Filters.Count) and (TeachGrid.Filters[i].Col <> 2) do
      Inc(i);
    if i < TeachGrid.Filters.Count then TeachGrid.Filters.Delete(i);
  end;
  TeachGrid.Fixup;
end;

procedure TTeachForm.btnStudentsClick(Sender: TObject);
begin
  FillGrid(btnStudents.Down);
end;

end.
