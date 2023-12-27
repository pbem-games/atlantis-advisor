unit PowerGrid;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Menus, IniFiles, Math, StdCtrls, Masks;

const
  relEQ = 0;
  relNE = 1;
  relGT = 2;
  relLT = 3;

  DragThreshold = 2;

type
  TRelation = integer;
  TSortOrder = (soAscending, soDescending);
  TColFormat = (cfString, cfNumber);
  TPowerGridOptions = set of (pgoLines, pgoRangeSelect, pgoColSizing, pgoColMoving,
    pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoNoSelect, pgoStretchLastCol,
    pgoMultilineCells, pgoMultiselect);

  TPowerGrid = class;

  TFilter = class
    Tag: integer;
    Description: string;
    Enabled: boolean;
    Col: integer;
    Rel: TRelation;
    Value: string;
    constructor Create(ACol: integer; ARel: TRelation; AValue: string);
    procedure Assign(Source: TFilter);
  end;

  TFilterList = class(TList)
  protected
    function Get(Index: Integer): TFilter;
    procedure Put(Index: Integer; Item: TFilter);
  public
    property Items[Index: Integer]: TFilter read Get write Put; default;
    function New(ACol: integer; ARel: TRelation; AValue: string): TFilter;
  end;

  TSelection = record
    StartPos, EndPos: Integer;
  end;

  TPwInplaceEdit = class(TCustomMaskEdit)
  private
    FGrid: TPowerGrid;
    FClickTime: Longint;
    procedure InternalMove(const Loc: TRect; Redraw: Boolean);
    procedure SetGrid(Value: TPowerGrid);
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message); message WM_PASTE;
    procedure WMCut(var Message); message WM_CUT;
    procedure WMClear(var Message); message WM_CLEAR;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure BoundsChanged; virtual;
    procedure UpdateContents; virtual;
    procedure WndProc(var Message: TMessage); override;
    property  Grid: TPowerGrid read FGrid;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Deselect;
    procedure Hide;
    procedure Invalidate; reintroduce;
    procedure Move(const Loc: TRect);
    function PosEqual(const Rect: TRect): Boolean;
    procedure SetFocus; reintroduce;
    procedure UpdateLoc(const Loc: TRect);
    function Visible: Boolean;
  end;

  TPowerGridCol = class
  private
    FGrid: TPowerGrid;
    FEnabled: boolean;
    FWidth: integer;
    Field: integer;
    procedure Assign(Source: TPowerGridCol);
  protected
    function GetImgCol: integer;
    procedure SetEnabled(Value: boolean);
    procedure SetWidth(Value: integer);
    procedure SetImgCol(Value: integer);
  public
    Format: TColFormat;
    CanHide: boolean;
    CanMove: boolean;
    AutoEdit: boolean;
    constructor Create(AGrid: TPowerGrid);
    property Enabled: boolean read FEnabled write SetEnabled;
    property Width: integer read FWidth write SetWidth;
    property ImgCol: integer read GetImgCol write SetImgCol;
  end;

  TPowerGridRow = class
  private
    function GetField(Index: integer): string;
    procedure SetField(Index: integer; Value: string);
    procedure Assign(Source: TPowerGridRow);
    function GetSortKey(Index: integer): string;
    procedure SetSortKey(Index: integer; const Value: string);
  public
    Fields, SortKeys: TStrings; // For internal use only
    Color: TColor;
    FontStyle: TFontStyles;
    Data: Pointer;
    ImageIndex: integer;
    Selected: boolean;
    constructor Create;
    destructor Destroy; override;
    property Field[Index: integer]: string read GetField write SetField;
    property SortKey[Index: integer]: string read GetSortKey write SetSortKey;
  end;


{ TPowerGrid
    Data stored in internal rows (Cells) and transfers onto the
  screen rows (ImgCells) by Fixup. It appears there sorted and
  filtered. ImgCells is read-only.
    There must be only one FixedRow. If there is, value of row 0
  (fixed) are keeped in FHeaders.
    New Cells may be added anywhere after end of rows, it will create
  one new row with data in given column.
}

  TPowerDrawCellEvent = procedure (Sender: TObject; ACol, ARow: Longint;
    var TxtRect: TRect; State: TGridDrawState) of object;
  TPowerDrawIconEvent = procedure (Sender: TObject; ACanvas: TCanvas;
    X, Y: integer; Data: pointer) of object;

  TPowerGrid = class(TCustomGrid)
  private
    FOnDrawCell: TPowerDrawCellEvent;
    FOnSelectCell: TSelectCellEvent;
    FOnMouseDownFirst: TMouseEvent;
    FOnGetEditText: TGetEditEvent;
    FOnSetEditText: TSetEditEvent;
    FOnDrawIcon: TPowerDrawIconEvent;

    FCols: TList;
    FDefaultRowColor: TColor;
    FDefaultColFormat: TColFormat;

    FEditing: boolean;
    FEditorMode: boolean;
    FInplaceCol, FInplaceRow: integer;
    FInplaceEdit: TPwInplaceEdit;
    FCanEditModify: boolean;

    FLinesColor: TColor;
    FRows: TList;
    FHeaders: TStrings;
    FImgRows: TList;
    FImgCols: TList;
    FOptions: TPowerGridOptions;
    FPopupMenu: TPopupMenu;
    FSorted: boolean;
    FSortBy: longint;
    FSortOrder: TSortOrder;
    FStickySelect: boolean;
    ColDragRequest: boolean;
    HeaderPopup: TPopupMenu;
    HeadRow: TPowerGridRow;
    ColDragging, LastDragHalf: boolean;
    DragCol, LastDragCol, DragCycles: integer;
    MouseMoved: boolean;
    UpdatingColWidths: boolean;
    FImageCol: integer;
    FImages: TImageList;
    SelectionBase: integer;
    MouseIsDown: boolean;
    IsDragging, IsDragged: boolean;
    function CompareRows(Item1, Item2: integer): Integer;
    procedure HeaderPopupHandler(Sender: TObject);
    function GetGridColCount: integer;
    procedure SetGridColCount(Value: integer);
    property GridColCount: integer read GetGridColCount write SetGridColCount;
    procedure UpdateColWidths;
    function GetWrapLine(s: string; Index, Wid: integer; var Count: integer): string;
    procedure ResizeRows;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetImageCol(const Value: integer);
    procedure SetImages(const Value: TImageList);
    function GetSortKeys(ACol, ARow: integer): string;
    procedure SetSortKeys(ACol, ARow: integer; const Value: string);
    procedure ShowEditor(ACol, ARow: longint);
    procedure UpdateEdit(ACol, ARow: longint);
    procedure HideEdit;
    procedure UpdateText;
    procedure SetEditing(const Value: boolean);
    procedure UpdateSelection(NewRow: integer);
  protected
    function GetCell(ACol,ARow: integer): string;
    function GetCol(Index: integer): TPowerGridCol;
    function GetColCount: integer;
    function GetFixedRows: integer;
    function GetRow(Index: integer): TPowerGridRow;
    function GetRowCount: integer;
    function GetImgCell(ACol,ARow: integer): string;
    function GetImgCol(Index: integer): TPowerGridCol;
    function GetImgColCount: integer;
    function GetImgRow(Index: integer): TPowerGridRow;
    function GetImgRowCount: integer;
    procedure SetCell(ACol,ARow: integer; Value: string);
    procedure SetCol(Index: integer; Value: TPowerGridCol);
    procedure SetColCount(Value: integer);
    procedure SetFixedRows(Value: integer);
    procedure SetLinesColor(Value: TColor);
    procedure SetImgCell(ACol,ARow: integer; Value: string);
    procedure SetOptions(Value: TPowerGridOptions);
    procedure SetRow(Index: integer; Value: TPowerGridRow);
    procedure SetRowCount(Value: integer);
    procedure SetSorted(Value: boolean);
    procedure SetSortBy(Value: longint);
    procedure SetSortOrder(Value: TSortOrder);
    procedure SetStickySelect(Value: boolean);

    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;

    procedure ColWidthsChanged; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
      State: TGridDrawState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure Paint; override;
    procedure DoExit; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    NoRepaint: boolean;
    Filters: TFilterList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Sort;
    procedure Fixup;
    procedure HideEditor;
    function MouseCell: TGridCoord;
    procedure MouseHalfCell(var ACol, ARow: integer; var Half: boolean);
    procedure SaveColumns(Cfg: TCustomIniFile);
    procedure LoadColumns(Cfg: TCustomIniFile);
    function CellRect(ACol, ARow: longint): TRect;
    function SelectedRows: integer;

    property Canvas;
    property Cells[ACol,ARow: integer]: string read GetCell write SetCell;
    property Cols[Index: integer]: TPowerGridCol read GetCol write SetCol;
    property Col;
    property ImgCells[ACol,ARow: integer]: string read GetImgCell write SetImgCell;
    property ImgCols[Index: integer]: TPowerGridCol read GetImgCol;
    property ImgColCount: integer read GetImgColCount;
    property ImgRows[Index: integer]: TPowerGridRow read GetImgRow;
    property ImgRowCount: integer read GetImgRowCount;
    property Rows[Index: integer]: TPowerGridRow read GetRow write SetRow;
    property Row;
    property RowHeights;
    property SortKeys[ACol,ARow: integer]: string read GetSortKeys write SetSortKeys;
    property HitTest;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property ColCount: integer read GetColCount write SetColCount;
    property DefaultColFormat: TColFormat read FDefaultColFormat write FDefaultColFormat default cfString;
    property DefaultRowColor: TColor read FDefaultRowColor write FDefaultRowColor;
    property DefaultColWidth;
    property DefaultRowHeight default 17;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Editing: boolean read FEditing write SetEditing;
    property FixedRows: integer read GetFixedRows write SetFixedRows;
    property Font;
    property Images: TImageList read FImages write SetImages;
    property ImageCol: integer read FImageCol write SetImageCol;
    property LinesColor: TColor read FLinesColor write SetLinesColor default clActiveBorder;
    property Options: TPowerGridOptions read FOptions write SetOptions
      default [pgoLines, pgoColMoving, pgoColSizing, pgoRowSelect, pgoColHiding,
        pgoSortOnClick];
    property ParentColor;
    property ParentFont;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property RowCount: integer read GetRowCount write SetRowCount;
    property ScrollBars;
    property Sorted: boolean read FSorted write SetSorted;
    property SortBy: longint read FSortBy write SetSortBy;
    property SortOrder: TSortOrder read FSortOrder write SetSortOrder default soAscending;
    property ShowHint;
    property StickySelect: boolean read FStickySelect write SetStickySelect;
    property TopRow;
    property VisibleColCount;
    property VisibleRowCount;

    property OnClick;
    property OnDblClick;
    property OnDrawCell: TPowerDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawIcon: TPowerDrawIconEvent read FOnDrawIcon write FOnDrawIcon;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditText: TGetEditEvent read FOnGetEditText write FOnGetEditText;
    property OnSetEditText: TSetEditEvent read FOnSetEditText write FOnSetEditText;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDownFirst: TMouseEvent read FOnMouseDownFirst write FOnMouseDownFirst;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectCell: TSelectCellEvent read FOnSelectCell write FOnSelectCell;
  end;


 procedure Register;
 function CompareFmt(s1,s2: string; Format: TColFormat): integer;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TPowerGrid]);
end;


type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxCustomExtents] of Integer;



function ShiftPressed: boolean;
begin
  Result := (GetKeyState(VK_SHIFT) and $80 <> 0);
end;

function CtrlPressed: boolean;
begin
  Result := (GetKeyState(VK_CONTROL) and $80 <> 0);
end;

function SafeStrToInt(s: string): integer;
begin
  if s = '' then Result := 0
  else
    try
      Result := StrToInt(s);
    except
      on EConvertError do Result := -1;
    end;
end;

// Compare two string values by given format
function CompareFmt(s1,s2: string; Format: TColFormat): integer;
var i1,i2: integer;
begin
  case Format of
      cfNumber: begin
                  i1 := SafeStrToInt(s1);
                  i2 := SafeStrToInt(s2);
                  if i1 = i2 then Result := 0
                  else if i1 < i2 then Result := -1 else Result := 1;
                end;
    else        Result := AnsiCompareStr(s1,s2);
  end;
end;


{ TFilter }

function TFilterList.Get(Index: integer): TFilter;
begin
  Result := TFilter(inherited Get(Index));
end;

procedure TFilterList.Put(Index: integer; Item: TFilter);
begin
  inherited Put(Index, Item);
end;

function TFilterList.New(ACol: integer; ARel: TRelation; AValue: string): TFilter;
begin
  Result := TFilter.Create(ACol, ARel, AValue);
  Add(Result);
end;

constructor TFilter.Create(ACol: integer; ARel: TRelation; AValue: string);
begin
  Col := ACol;
  Rel := ARel;
  Value := AValue;
  Enabled := TRUE;
end;

procedure TFilter.Assign(Source: TFilter);
begin
  Col := Source.Col;
  Rel := Source.Rel;
  Value := Source.Value;
  Tag := Source.Tag;
  Description := Source.Description;
end;

constructor TPwInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  DoubleBuffered := False;
end;

procedure TPwInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TPwInplaceEdit.SetGrid(Value: TPowerGrid);
begin
  FGrid := Value;
end;

procedure TPwInplaceEdit.CMShowingChanged(var Message: TMessage);
begin
  { Ignore showing using the Visible property }
end;

procedure TPwInplaceEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
end;

procedure TPwInplaceEdit.WMPaste(var Message);
begin
  if not EditCanModify then Exit;
  inherited
end;

procedure TPwInplaceEdit.WMClear(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TPwInplaceEdit.WMCut(var Message);
begin
  if not EditCanModify then Exit;
  inherited;
end;

procedure TPwInplaceEdit.DblClick;
begin
  Grid.DblClick;
end;

function TPwInplaceEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TPwInplaceEdit.EditCanModify: Boolean;
begin
  Result := Grid.CanEditModify;
end;

procedure TPwInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    Grid.KeyDown(Key, Shift);
    Key := 0;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    GridKeyDown := Grid.OnKeyDown;
    if Assigned(GridKeyDown) then GridKeyDown(Grid, Key, Shift);
  end;

  function ForwardMovement: Boolean;
  begin
    Result := True;
  end;

  function Ctrl: Boolean;
  begin
    Result := ssCtrl in Shift;
  end;

  function Selection: TSelection;
  begin
    SendMessage(Handle, EM_GETSEL, Longint(@Result.StartPos), Longint(@Result.EndPos));
  end;

  function CaretPos: Integer;
  var
    P: TPoint;
  begin
    Windows.GetCaretPos(P);
    Result := SendMessage(Handle, EM_CHARFROMPOS, 0, MakeLong(P.X, P.Y));
  end;

  function RightSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = GetTextLen) and
        ((StartPos = 0) or (EndPos = StartPos)) and (EndPos = GetTextLen);
   end;

  function LeftSide: Boolean;
  begin
    with Selection do
      Result := (CaretPos = 0) and (StartPos = 0) and
        ((EndPos = 0) or (EndPos = GetTextLen));
  end;

begin
  case Key of
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_ESCAPE, VK_RETURN: SendToParent;
    VK_INSERT:
      if Shift = [] then SendToParent
      else if (Shift = [ssShift]) and not Grid.CanEditModify then Key := 0;
    VK_LEFT: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_RIGHT: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_HOME: if ForwardMovement and (Ctrl or LeftSide) then SendToParent;
    VK_END: if ForwardMovement and (Ctrl or RightSide) then SendToParent;
    VK_F2:
      begin
        ParentEvent;
        if Key = VK_F2 then
        begin
          Deselect;
          Exit;
        end;
      end;
    VK_TAB: if not (ssAlt in Shift) then SendToParent;
    VK_DELETE:
      if Ctrl then
        SendToParent
      else
        if not Grid.CanEditModify then Key := 0;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TPwInplaceEdit.KeyPress(var Key: Char);
var
  Selection: TSelection;
begin
  Grid.KeyPress(Key);
  if (Key in [#32..#255]) and not Grid.CanEditAcceptKey(Key) then
  begin
    Key := #0;
    MessageBeep(0);
  end;
  case Key of
    #9, #27: Key := #0;
    #13:
      begin
        SendMessage(Handle, EM_GETSEL, Longint(@Selection.StartPos), Longint(@Selection.EndPos));
        if (Selection.StartPos = 0) and (Selection.EndPos = GetTextLen) then
          Deselect else
          SelectAll;
        Key := #0;
      end;
    ^H, ^V, ^X, #32..#255:
      if not Grid.CanEditModify then Key := #0;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TPwInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Grid.KeyUp(Key, Shift);
end;

procedure TPwInplaceEdit.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_SETFOCUS:
      begin
        if (GetParentForm(Self) = nil) or GetParentForm(Self).SetFocusedControl(Grid) then Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      begin
        if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
          Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TPwInplaceEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Longint($FFFFFFFF));
end;

procedure TPwInplaceEdit.Invalidate;
var
  Cur: TRect;
begin
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, @Cur);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TPwInplaceEdit.Hide;
begin
  if HandleAllocated and IsWindowVisible(Handle) then
  begin
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOZORDER or
      SWP_NOREDRAW);
    if Focused then Windows.SetFocus(Grid.Handle);
  end;
end;

function TPwInplaceEdit.PosEqual(const Rect: TRect): Boolean;
var
  Cur: TRect;
begin
  GetWindowRect(Handle, Cur);
  MapWindowPoints(HWND_DESKTOP, Grid.Handle, Cur, 2);
  Result := EqualRect(Rect, Cur);
end;

procedure TPwInplaceEdit.InternalMove(const Loc: TRect; Redraw: Boolean);
begin
  if IsRectEmpty(Loc) then Hide
  else
  begin
    CreateHandle;
    Redraw := Redraw or not IsWindowVisible(Handle);
    Invalidate;
    with Loc do
      SetWindowPos(Handle, HWND_TOP, Left, Top, Right - Left, Bottom - Top,
        SWP_SHOWWINDOW or SWP_NOREDRAW);
    BoundsChanged;
    if Redraw then Invalidate;
    if Grid.Focused then
      Windows.SetFocus(Handle);
  end;
end;

procedure TPwInplaceEdit.BoundsChanged;
var
  R: TRect;
begin
  R := Rect(2, 2, Width - 2, Height);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TPwInplaceEdit.UpdateLoc(const Loc: TRect);
begin
  InternalMove(Loc, False);
end;

function TPwInplaceEdit.Visible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TPwInplaceEdit.Move(const Loc: TRect);
begin
  InternalMove(Loc, True);
end;

procedure TPwInplaceEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

procedure TPwInplaceEdit.UpdateContents;
begin
  Text := '';
  EditMask := Grid.GetEditMask(Grid.FInplaceCol, Grid.FInplaceRow);
  Text := Grid.GetEditText(Grid.FInplaceCol, Grid.FInplaceRow);
  MaxLength := Grid.GetEditLimit;
end;


  { TPowerGridCol }

constructor TPowerGridCol.Create(AGrid: TPowerGrid);
begin
  FGrid := AGrid;
  Format := FGrid.DefaultColFormat;
  FWidth := FGrid.DefaultColWidth;
  CanHide := True;
  CanMove := True;
end;

procedure TPowerGridCol.Assign(Source: TPowerGridCol);
begin
  Format := Source.Format;
  FEnabled := Source.Enabled;
  FWidth := Source.Width;
end;

function TPowerGridCol.GetImgCol: integer;
begin
  Result := 0;
  while (Result < FGrid.FImgCols.Count) and (FGrid.ImgCols[Result] <> Self) do
    Result := Result + 1;
  if Result > FGrid.FImgCols.Count then Result := -1;
end;

procedure TPowerGridCol.SetEnabled(Value: boolean);
var i: integer;
begin
  if not CanHide then Exit;
  if FEnabled and not Value and (FGrid.FImgCols.Count > 1) then begin  // Disable column
    with FGrid do begin
      i := ImgCol;
      FImgCols.Delete(i);
      GridColCount := FImgCols.Count;
      if FSortBy = i then SortBy := 0;
      if FSortBy > i then SortBy := FSortBy - 1;
    end;
    FEnabled := FALSE;
  end;
  if not FEnabled and Value then begin // Enable column
    if Field > FGrid.ImgColCount then i := FGrid.ImgColCount else i := Field;
    FGrid.FImgCols.Insert(i, Self);
    with FGrid do begin
      GridColCount := FImgCols.Count;
      if FSortBy >= i then SortBy := FSortBy + 1;
    end;
    FEnabled := TRUE;
  end;
end;

procedure TPowerGridCol.SetWidth(Value: integer);
begin
  FGrid.ColWidths[ImgCol] := Value;
  FWidth := Value;
end;

procedure TPowerGridCol.SetImgCol(Value: integer);
var t: integer;
begin
  if not CanMove then Exit;
  if FEnabled then begin
    t := ImgCol;
    if (Value < t) and (Value >= 0) then begin
      FGrid.FImgCols.Delete(t);
      FGrid.FImgCols.Insert(Value, Self);
    end;
    if (Value > t) and (Value <= FGrid.FImgCols.Count) then begin
      FGrid.FImgCols.Insert(Value, Self);
      FGrid.FImgCols.Delete(t);
    end;
    with FGrid do begin
      if FSortBy = t then SortBy := Value
      else if (FSortBy < t) and (FSortBy > Value) then SortBy := FSortBy + 1
           else if (FSortBy > t) and (FSortBy < Value) then SortBy := FSortBy - 1;
      UpdateColWidths;
    end;
  end;
end;

  { TPowerGridRow }

constructor TPowerGridRow.Create;
begin
  Fields := TStringList.Create;
  SortKeys := TStringList.Create;
end;

destructor TPowerGridRow.Destroy;
begin
  Fields.Free;
  SortKeys.Free;
end;

procedure TPowerGridRow.Assign(Source: TPowerGridRow);
begin
  Color := Source.Color;
  Fields.Assign(Source.Fields);
  SortKeys.Assign(Source.SortKeys);
end;

function TPowerGridRow.GetField(Index: integer): string;
begin
  if Index < Fields.Count then Result := Fields[Index]
  else Result := '';
end;

procedure TPowerGridRow.SetField(Index: integer; Value: string);
var i: integer;
begin
  for i := Fields.Count to Index do Fields.Add('');
  Fields[Index] := Value;
  SortKey[Index] := Value;
end;

function TPowerGridRow.GetSortKey(Index: integer): string;
begin
  if Index < SortKeys.Count then Result := SortKeys[Index]
  else Result := '';
end;

procedure TPowerGridRow.SetSortKey(Index: integer; const Value: string);
var i: integer;
begin
  for i := SortKeys.Count to Index do SortKeys.Add('');
  SortKeys[Index] := Value;
end;

  { TPowerGrid }

// Create and destroy

constructor TPowerGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCols := TList.Create;
  Filters := TFilterList.Create;
  FHeaders := TStringList.Create;
  FImgRows := TList.Create;
  FImgCols := TList.Create;
  FRows := TList.Create;

  HeadRow := TPowerGridRow.Create;
  HeadRow.Color := clBtnFace;
  HeadRow.Fields := FHeaders;
  HeaderPopup := TPopupMenu.Create(Self);

  inherited Options := [];
  Options :=  [pgoLines, pgoColMoving, pgoColSizing, pgoRowSelect, pgoColHiding,
    pgoSortOnClick];
  ColCount := 3;
  DefaultColFormat := cfString;
  DefaultDrawing := FALSE;
  DefaultRowHeight := 17;
  FixedCols := 0;
  FSortOrder := soAscending;
  FSorted := TRUE;
  LinesColor := clActiveBorder;
  RowCount := 2;

  Fixup;
end;

destructor TPowerGrid.Destroy;
var i: integer;
begin
  for i:=FCols.Count-1 downto 0 do TPowerGridCol(FCols[i]).Free;
  FCols.Free;
  FImgCols.Free;
  for i:=Filters.Count-1 downto 0 do Filters[i].Free;
  Filters.Free;
  for i:=FRows.Count-1 downto 0 do TPowerGridRow(FRows[i]).Free;
  FRows.Free;
  FHeaders.Free;
  FImgRows.Free;
  HeaderPopup.Free;
  inherited Destroy;
end;

{ Update column widths in internal array }
procedure TPowerGrid.ColWidthsChanged;
var i: integer;
begin
  inherited ColWidthsChanged;
  if not UpdatingColWidths then begin
    for i := 0 to ImgColCount-1 do ImgCols[i].FWidth := ColWidths[i];
  end;
  ResizeRows;
end;

procedure TPowerGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  State: TGridDrawState);
var i, cnt, wid, margin: integer;
    s: string;
    TxtRect: TRect;
begin
  if NoRepaint then Exit;

  if (pgoMultiselect in Options) and (ARow < ImgRowCount) then begin
    if ImgRows[ARow].Selected then State := State + [gdSelected]
    else State := State - [gdSelected];
  end;

  with Canvas do begin
    if gdFixed in State then begin  // Draw fixed cell

     { Draw cell lines and background }
      Brush.Color := clBtnFace;
      Pen.Color := cl3DDkShadow;
      MoveTo(ARect.Right-1,ARect.Top);
      LineTo(ARect.Right-1,ARect.Bottom-1);
      LineTo(ARect.Left-1,ARect.Bottom-1);
      FillRect(Rect(ARect.Left,ARect.Top,ARect.Right-1,ARect.Bottom-1));
      Pen.Color := clBtnShadow;
      MoveTo(ARect.Right-2,ARect.Top);
      LineTo(ARect.Right-2,ARect.Bottom-2);
      LineTo(ARect.Left,ARect.Bottom-2);
      Pen.Color := clBtnHighlight;
      MoveTo(ARect.Right-2,ARect.Top);
      LineTo(ARect.Left,ARect.Top);
      LineTo(ARect.Left,ARect.Bottom-1);
      TxtRect := Rect(ARect.Left+1,ARect.Top+1,ARect.Right-2,ARect.Bottom-2);

     { Draw sort symbol }
      if FSorted and (ACol = SortBy) and (ARow = 0) and
       (TxtRect.Right-TxtRect.Left >= 10) then begin
        if FSortOrder = soAscending then begin
          Pen.Color := clGray;
          MoveTo(TxtRect.Right-10, TxtRect.Bottom-4);
          LineTo(TxtRect.Right-7, TxtRect.Top+3);
          LineTo(TxtRect.Right-6, TxtRect.Top+3);
          Pen.Color := clWhite;
          LineTo(TxtRect.Right-3, TxtRect.Bottom-4);
          LineTo(TxtRect.Right-10, TxtRect.Bottom-4);
        end
        else begin
          Pen.Color := clGray;
          MoveTo(TxtRect.Right-3, TxtRect.Top+4);
          LineTo(TxtRect.Right-10, TxtRect.Top+4);
          LineTo(TxtRect.Right-7, TxtRect.Bottom-3);
          LineTo(TxtRect.Right-6, TxtRect.Bottom-3);
          Pen.Color := clWhite;
          LineTo(TxtRect.Right-3, TxtRect.Top+4);
        end;
        TxtRect.Right := TxtRect.Right - 11;
      end;

     { Call event handler }
      if Assigned(FOnDrawCell) then FOnDrawCell(Self, ACol, ARow, TxtRect, State);

     { Draw header text }
      if TxtRect.Right > TxtRect.Left then begin
        Brush.Color := clBtnFace;
        Font.Color := clBtnText;
        Font.Style := [];
        s := ImgCells[ACol,ARow];
        wid := TxtRect.Right-TxtRect.Left-3;
        if TextExtent(s).cx > wid then
          if TextExtent('...').cx > wid then s := ''
          else begin
            while TextExtent(s+'...').cx > wid do
              s := Copy(s,1,Length(s)-1);
            s := s + '...';
          end;
        TextRect(TxtRect, TxtRect.Left+2, TxtRect.Top+1, s);
      end;

     { Draw anchor if column moving }
      if ColDragging then begin
        Brush.Color := clBlack;
        if ((ACol = LastDragCol) and not LastDragHalf) or
           ((ACol = LastDragCol+1) and LastDragHalf) then
          FillRect(Rect(ARect.Left,ARect.Top,ARect.Left+3,ARect.Bottom));
        if (ACol = LastDragCol) and LastDragHalf and (ACol = ImgColCount-1) then
          FillRect(Rect(ARect.Right-4,ARect.Top,ARect.Right-1,ARect.Bottom));
      end;
    end
    else begin  // Draw non-fixed cell

     { Draw cell background }
      if (gdSelected in State) and (Focused or StickySelect or IsDragging)
        and not (pgoNoSelect in Options) and (ARow < ImgRowCount) then
        Brush.Color := clHighlight
      else Brush.Color := Color;
      Pen.Color := LinesColor;
      FillRect(ARect);

      if ARow < ImgRowCount then begin
       { Draw lines }
        if pgoLines in Options then begin
          MoveTo(ARect.Right-1,ARect.Top);
          LineTo(ARect.Right-1,ARect.Bottom-1);
          LineTo(ARect.Left-1,ARect.Bottom-1);
        end;
        TxtRect := Rect(ARect.Left,ARect.Top,ARect.Right-2,ARect.Bottom-1);

        if (gdSelected in State) and (Focused or StickySelect or IsDragging)
          and not (pgoNoSelect in Options) then begin
          Brush.Color := clHighlight;
          Font.Color := clHighlightText;
          Font.Style := [];
        end
        else begin
          Brush.Color := Color;
          Font.Color := ImgRows[ARow].Color;
          Font.Style := ImgRows[ARow].FontStyle;
        end;

       { Call event handler }
        if Assigned(FOnDrawCell) then FOnDrawCell(Self, ACol, ARow, TxtRect, State);

        if (TxtRect.Right > TxtRect.Left) then begin
          // Cell image
          if (ACol = ImageCol) and (ARow >= FixedRows) then begin
            if Assigned(OnDrawIcon) then begin
              OnDrawIcon(Self, Canvas, TxtRect.Left, TxtRect.Top,
                ImgRows[ARow].Data);
              Inc(TxtRect.Left, 18);
            end
            else if (Images <> nil) and (ImgRows[ARow].ImageIndex >= 0)
              and (ImgRows[ARow].ImageIndex < Images.Count) then begin
              Images.Draw(Canvas, TxtRect.Left, TxtRect.Top, ImgRows[ARow].ImageIndex);
              Inc(TxtRect.Left, 18);
            end;
          end;

          // Cell text
          margin := (DefaultRowHeight - Canvas.TextHeight('A')) div 2;
          if pgoMultilineCells in Options then begin
            i := 0;
            repeat
              TextRect(Rect(TxtRect.Left, TxtRect.Top +
                i * Canvas.TextHeight('A') + margin, TxtRect.Right,
                TxtRect.Bottom),
                TxtRect.Left + margin,
                TxtRect.Top + i * Canvas.TextHeight('A') + margin,
                GetWrapLine(ImgCells[ACol,ARow], i, TxtRect.Right -
                  TxtRect.Left - 4, cnt));
              Inc(i);
            until i >= cnt;
          end
          else TextRect(TxtRect, TxtRect.Left + 2, TxtRect.Top+margin, ImgCells[ACol,ARow]);
        end;
      end;
    end;
  end;
end;


{ Transfer internal cells onto screen through filters and perform sort }
procedure TPowerGrid.Fixup;
var i, j, res: integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Min, Max: Integer;
  begin
    Result := False;
    if (ScrollBars = ssBoth) or
      ((Code = SB_HORZ) and (ScrollBars = ssHorizontal)) or
      ((Code = SB_VERT) and (ScrollBars = ssVertical)) then
    begin
      GetScrollRange(Handle, Code, Min, Max);
      Result := Min <> Max;
    end;
  end;

begin
  FImgRows.Clear;
  for i:=0 to FRows.Count-1 do FImgRows.Add(FRows[i]);
  // Apply filters
  for i:=0 to Filters.Count-1 do
    with TFilter(Filters[i]) do
      if Enabled then begin
        j := 0;
        while j < FImgRows.Count do begin
          res := CompareFmt(TPowerGridRow(FImgRows[j]).Field[Col],
            Value, Cols[Col].Format);
          if ((Rel = relEQ) and (res <> 0)) or
             ((Rel = relNE) and (res = 0)) or
             ((Rel = relGT) and (res <> 1)) or
             ((Rel = relLT) and (res <> -1)) then FImgRows.Delete(j)
          else j := j + 1;
        end;
      end;

  // Cheat to keep fixed row when 1 or 0 lines given
  if FImgRows.Count = 0 then inherited RowCount := FixedRows+1
  else inherited RowCount := FImgRows.Count + FixedRows;

  if Sorted then Sort
  else ResizeRows;
  SelectCell(0, Row);
  Invalidate;
end;

function TPowerGrid.GetCell(ACol,ARow: integer): string;
begin
  if ARow < RowCount then Result := Rows[ARow].Field[ACol]
  else Result := '';
end;

function TPowerGrid.GetSortKeys(ACol, ARow: integer): string;
begin
  if ARow < RowCount then Result := Rows[ARow].SortKey[ACol]
  else Result := '';
end;

function TPowerGrid.GetCol(Index: integer): TPowerGridCol;
begin
  Result := TPowerGridCol(FCols[Index]);
end;

function TPowerGrid.GetColCount: integer;
begin
  Result := FCols.Count;
end;

function TPowerGrid.GetFixedRows: integer;
begin
  Result := inherited FixedRows;
end;

function TPowerGrid.GetGridColCount: integer;
begin
  Result := inherited ColCount;
end;

function TPowerGrid.GetImgCell(ACol,ARow: integer): string;
var field: integer;
begin
  field := ImgCols[ACol].Field;
  if (field >= 0) and (ARow < ImgRowCount) then
    Result := ImgRows[ARow].Field[field]
  else Result := '';
end;

function TPowerGrid.GetImgCol(Index: integer): TPowerGridCol;
begin
  Result := TPowerGridCol(FImgCols[Index]);
end;

function TPowerGrid.GetImgColCount: integer;
begin
  Result := FImgCols.Count;
end;

function TPowerGrid.GetImgRow(Index: integer): TPowerGridRow;
begin
  if FixedRows > 0 then Index := Index - FixedRows;
  if Index < 0 then Result := HeadRow
  else Result := TPowerGridRow(FImgRows[Index]);
end;

function TPowerGrid.GetImgRowCount: integer;
begin
  Result := FImgRows.Count + FixedRows;
end;

function TPowerGrid.GetRow(Index: integer): TPowerGridRow;
begin
  if FixedRows > 0 then Index := Index - FixedRows;
  if Index < 0 then Result := HeadRow
  else Result := TPowerGridRow(FRows[Index]);
end;

function TPowerGrid.GetRowCount: integer;
begin
  Result := FRows.Count + FixedRows;
end;

procedure TPowerGrid.HeaderPopupHandler(Sender: TObject);
begin
  with TMenuItem(Sender) do Cols[Tag].Enabled := not Checked;
end;

procedure TPowerGrid.UpdateSelection(NewRow: integer);
var i: integer;
    sel: boolean;
begin
  if IsDragging then Exit;
  if (RowCount <= FixedRows) or (NewRow < 0) or (NewRow >= RowCount) then Exit;
  if MouseIsDown and (CtrlPressed or ImgRows[NewRow].Selected) then Exit;

  if ShiftPressed then begin
    if SelectionBase < 0 then SelectionBase := Row;
  end
  else SelectionBase := -1;

  for i := 0 to Min(ImgRowCount-1, inherited RowCount) do begin
    sel := (i = NewRow);
    if pgoMultiselect in Options then begin
      if ShiftPressed then
        sel := (i >= Min(SelectionBase, NewRow)) and (i <= Max(SelectionBase, NewRow))
      else if CtrlPressed then begin
        if i = NewRow then sel := not ImgRows[i].Selected
        else sel := ImgRows[i].Selected;
      end;
    end;
    if sel <> ImgRows[i].Selected then
      InvalidateRow(i);
    ImgRows[i].Selected := sel;
  end;
end;

procedure TPowerGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseIsDown := True;
  if Assigned(FOnMouseDownFirst) then FOnMouseDownFirst(Self, Button, Shift, X, Y);
  HideEditor;
  inherited MouseDown(Button,Shift,X,Y);

  if (MouseCell.X >= FixedCols) and (MouseCell.Y >= FixedRows)
    and (FEditing or ImgCols[MouseCell.X].AutoEdit) then begin
    ShowEditor(MouseCell.X, MouseCell.Y);
  end;

  if (MouseCell.Y = 0) and (FixedRows > 0) and (pgoColMoving in Options) then begin
    ColDragRequest := TRUE;
    DragCycles := 0;
  end;
  MouseMoved := FALSE;
end;

function TPowerGrid.MouseCell: TGridCoord;
begin
  Result := MouseCoord(HitTest.X, HitTest.Y);
end;

procedure TPowerGrid.MouseHalfCell(var ACol,ARow: integer; var Half: boolean);
var Rect: TRect;
begin
  ACol := MouseCell.X; ARow := MouseCell.Y;
  Rect := CellRect(ACol,ARow);
  if HitTest.X < Rect.Left + (Rect.Right - Rect.Left) div 2 then Half := FALSE
  else Half := TRUE;
end;

procedure TPowerGrid.MouseMove(Shift: TShiftState; X,Y: Integer);
var NX, NY: integer;
    Half: boolean;
begin
  inherited MouseMove(Shift,X,Y);
  if SSLeft in Shift then begin
    MouseMoved := TRUE;
    if ColDragRequest and (MouseCell.X >= 0) and ImgCols[MouseCell.X].CanMove
      and (FGridState <> gsColSizing) and (ImgColCount > 1) then begin
      if DragCycles < DragThreshold then DragCycles := DragCycles + 1
      else begin
        ColDragRequest := FALSE;
        ColDragging := TRUE;
        Screen.Cursor := crDrag;
        DragCol := ImgCols[MouseCell.X].Field;
        ImgCols[MouseCell.X].Enabled := FALSE;
        MouseHalfCell(LastDragCol, NY, LastDragHalf);
      end;
    end;
    if ColDragging then begin
      MouseHalfCell(NX, NY, Half);
      if ((NX <> LastDragCol) or (Half <> LastDragHalf)) and (NX > -1) then begin
        LastDragCol := NX; LastDragHalf := Half;
        InvalidateRow(0);
      end;
    end;
  end;
end;

procedure TPowerGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var i: integer;
begin
  MouseIsDown := False;
  if (pgoMultiselect in Options) and not (csLButtonDown in ControlState) then begin
    IsDragging := True;
    IsDragged := False;
  end;
  if (pgoMultiselect in Options) and (Row = MouseCell.Y)
    and (Col <> MouseCell.X) then SelectCell(Col, Row);
  inherited MouseUp(Button,Shift,X,Y);
  ColDragRequest := FALSE;
 { End of column dragging }
  if ColDragging then begin
    ColDragging := FALSE;
    Screen.Cursor := crDefault;
    if LastDragHalf then LastDragCol := LastDragCol + 1;
    Cols[DragCol].Enabled := TRUE;
    if Cols[DragCol].Field < LastDragCol then LastDragCol := LastDragCol + 1;
    Cols[DragCol].ImgCol := LastDragCol;
  end
  else begin
 { Right mouse button clicked }
    if (Button = mbRight) then begin
      if (FixedRows > 0) and (MouseCell.Y = 0) and (pgoColHiding in Options) then
        begin  // Call header popup
        for i:=0 to HeaderPopup.Items.Count-1 do
          with HeaderPopup.Items[i] do begin
            Caption := Cells[i,0];
            Checked := Cols[i].Enabled;
            Tag := i;
          end;
        HeaderPopup.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end
      else if FPopupMenu <> nil then
        FPopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    end
 { Left button - change sort order }
    else begin
      if not MouseMoved and (FixedRows > 0) and (MouseCell.Y = 0)
        and (pgoSortOnClick in Options) then begin
        FSorted := TRUE;
        if FSortBy = MouseCell.X then begin
          if FSortOrder = soAscending then FSortOrder := soDescending
          else FSortOrder := soAscending;
        end
        else FSortBy := MouseCell.X;
        Sort;
        SelectCell(0, Row);
      end;
    end;
  end;
end;

procedure TPowerGrid.Paint;
begin
  if not NoRepaint then inherited Paint;
end;

function TPowerGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  UpdateSelection(ARow);
  Result := True;
  if Assigned(FOnSelectCell) and (ARow < ImgRowCount) then
    FOnSelectCell(Self, ACol, ARow, Result);
end;

procedure TPowerGrid.SetCell(ACol,ARow: integer; Value: string);
begin
  if ARow >= RowCount then begin  // If row don't exist
    ARow := RowCount;
    RowCount := RowCount + 1;     // Add one new row
  end;
  Rows[ARow].Field[ACol] := Value;
end;

procedure TPowerGrid.SetSortKeys(ACol, ARow: integer; const Value: string);
begin
  if ARow >= RowCount then begin  // If row don't exist
    ARow := RowCount;
    RowCount := RowCount + 1;     // Add one new row
  end;
  Rows[ARow].SortKey[ACol] := Value;
end;

procedure TPowerGrid.SetCol(Index: integer; Value: TPowerGridCol);
begin
  Cols[Index].Assign(Value);
end;

procedure TPowerGrid.SetColCount(Value: integer);
var i: integer;
    NewCol: TPowerGridCol;
    PopupItem: TMenuItem;
begin
  if Value < 1 then Value := 1;  // Can't be less than 1 column
 // Make new column records
  if FCols.Count < Value then
    for i:=FCols.Count to Value-1 do begin
      NewCol := TPowerGridCol.Create(Self);
      NewCol.Field := i;
      FCols.Add(NewCol);
      NewCol.Enabled := TRUE;
      PopupItem := TMenuItem.Create(Self);
      PopupItem.OnClick := HeaderPopupHandler;
      HeaderPopup.Items.Add(PopupItem);
    end;
 // or remove unused ones
  if FCols.Count > Value then
    for i:=FCols.Count-1 downto Value do begin
      TPowerGridCol(FCols[i]).Enabled := FALSE;
      TPowerGridCol(FCols[i]).Free;
      FCols.Delete(i);
      HeaderPopup.Items.Delete(Value);
    end;
  GridColCount := FImgCols.Count;
end;

procedure TPowerGrid.SetFixedRows(Value: integer);
begin
  if Value > 0 then inherited FixedRows := 1
  else inherited FixedRows := 0;
end;

procedure TPowerGrid.SetGridColCount(Value: integer);
begin
  inherited ColCount := Value;
  UpdateColWidths;
  Invalidate;
end;

procedure TPowerGrid.SetImgCell(ACol,ARow: integer; Value: string);
var field: integer;
begin
  field := ImgCols[ACol].Field;
  if (field >= 0) and (ARow < ImgRowCount) then
    ImgRows[ARow].Field[field] := Value;
end;

procedure TPowerGrid.SetLinesColor(Value: TColor);
begin
  FLinesColor := Value;
  Invalidate;
end;

procedure TPowerGrid.SetOptions(Value: TPowerGridOptions);
begin
  FOptions := Value;
  if pgoRangeSelect in Options then
    inherited Options := inherited Options + [goRangeSelect]
  else inherited Options := inherited Options - [goRangeSelect];
  if pgoColSizing in Options then
    inherited Options := inherited Options + [goColSizing]
  else inherited Options := inherited Options - [goColSizing];
  if pgoRowSelect in Options then
    inherited Options := inherited Options + [goRowSelect]
  else inherited Options := inherited Options - [goRowSelect];
end;

procedure TPowerGrid.SetRow(Index: integer; Value: TPowerGridRow);
begin
  if Index >= RowCount then begin  // If row don't exist
    Index := RowCount;
    RowCount := RowCount + 1;     // Add one new row
  end;
  Rows[Index].Assign(Value);
end;

procedure TPowerGrid.SetRowCount(Value: integer);
var i: integer;
    NewRow: TPowerGridRow;
begin
  Value := Value - FixedRows;
  FImgRows.Clear;
 // Make new row records
  if FRows.Count < Value then
    for i:=FRows.Count to Value do begin
      NewRow := TPowerGridRow.Create;
      NewRow.Color := DefaultRowColor;
      FRows.Add(NewRow);
    end;
 // or remove unused ones
  if FRows.Count > Value then
    for i := FRows.Count-1 downto Max(0, Value) do begin
      TPowerGridRow(FRows[i]).Free;
      FRows.Delete(i);
    end;
end;

procedure TPowerGrid.SetSorted(Value: boolean);
begin
  FSorted := Value;
  if Value = TRUE then Sort;
end;

procedure TPowerGrid.SetSortBy(Value: integer);
begin
  if Value < ImgColCount then FSortBy := Value else FSortBy := ImgColCount - 1;
  if Sorted then Sort;
end;

procedure TPowerGrid.SetSortOrder(Value: TSortOrder);
begin
  FSortOrder := Value;
  if Sorted then Sort;
end;

procedure TPowerGrid.SetStickySelect(Value: boolean);
begin
  FStickySelect := Value;
  Invalidate;
end;

 { Compare cells in column SortBy. If equal, compare cells from other cols
   in order from 0 to ColCount-1. }
function TPowerGrid.CompareRows(Item1, Item2: integer): Integer;
var col: integer;
    s1, s2: string;
begin
  s1 := TPowerGridRow(FImgRows[Item1]).SortKey[ImgCols[SortBy].Field];
  s2 := TPowerGridRow(FImgRows[Item2]).SortKey[ImgCols[SortBy].Field];
  Result := CompareFmt(s1, s2, ImgCols[SortBy].Format);
  if Result = 0 then begin
    col := 0;
    repeat
      if col = SortBy then begin
        Inc(col);
        if col >= ImgColCount then break;
      end;
      s1 := TPowerGridRow(FImgRows[Item1]).SortKey[ImgCols[col].Field];
      s2 := TPowerGridRow(FImgRows[Item2]).SortKey[ImgCols[col].Field];
      Result := CompareFmt(s1, s2, ImgCols[col].Format);
      Inc(col);
    until (col >= ImgColCount) or (Result <> 0);
  end;
end;

procedure TPowerGrid.Sort;
var cmp_order: integer;

  procedure QuickSort(iLo, iHi: Integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while CompareRows(Lo, Mid) = -cmp_order do Inc(Lo);
      while CompareRows(Hi, Mid) = cmp_order do Dec(Hi);
      if Lo <= Hi then
      begin
        FImgRows.Exchange(Lo,Hi);
        if Mid = Lo then Mid := Hi
        else if Mid = Hi then Mid := Lo;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if Hi > iLo then QuickSort(iLo, Hi);
    if Lo < iHi then QuickSort(Lo, iHi);
  end;

begin
  if SortOrder = soAscending then cmp_order := 1 else cmp_order := -1;
  if FImgRows.Count > 1 then QuickSort(0, FImgRows.Count-1);
  ResizeRows;
  Invalidate;
end;

procedure TPowerGrid.UpdateColWidths;
var i: integer;
begin
  UpdatingColWidths := TRUE;
  for i:=0 to FImgCols.Count-1 do ColWidths[i] := ImgCols[i].Width;
  UpdatingColWidths := FALSE;
end;

procedure TPowerGrid.LoadColumns(Cfg: TCustomIniFile);
var A: array of string;
    amt, i, j: integer;

  function ReadList(s: string; var A: array of string): integer;
  var i, st: integer;
  begin
    i := Low(A);
    while (Trim(s) <> '') and (i <= High(A)) do begin
      st := Pos(', ', s);
      if st = 0 then st := Length(s)+1;
      A[i] := Copy(s, 1, st-1);
      s := Copy(s, st+2, Length(s) - st);
      Inc(i);
    end;
    Result := i;
  end;

begin
  SetLength(A, ColCount);
  if FixedRows > 0 then begin
    ReadList(Cfg.ReadString(Name, 'Headers', ''), A);
    for i := 0 to High(A) do Cells[i, 0] := A[i];
  end;
  amt := ReadList(Cfg.ReadString(Name, 'ColWidths', ''), A);
  for i := 0 to amt-1 do Cols[i].Width := StrToInt(A[i]);
  amt := ReadList(Cfg.ReadString(Name, 'ColFormat', ''), A);
  for i := 0 to amt-1 do Cols[i].Format := TColFormat(StrToInt(A[i]));
  amt := ReadList(Cfg.ReadString(Name, 'ColEnabled', ''), A);
  for i := 0 to amt-1 do Cols[i].Enabled := (A[i] <> '0');
  amt := ReadList(Cfg.ReadString(Name, 'ColImgCol', ''), A);
  for i := 0 to amt-1 do begin
    j := 0;
    while (j < amt) and (StrToInt(A[j]) <> i) do Inc(j);
    if j < amt then Cols[j].ImgCol := i;
  end;
  FSorted := Cfg.ReadBool(Name, 'Sorted', True);
  FSortOrder := TSortOrder(Cfg.ReadInteger(Name, 'SortOrder', 0));
  FSortBy := Cfg.ReadInteger(Name, 'SortBy', 0);
  Fixup;
end;

procedure TPowerGrid.SaveColumns(Cfg: TCustomIniFile);
var A: array of string;
    i: integer;

  function MakeList(var A: array of string): string;
  var i: integer;
  begin
    Result := '';
    for i := Low(A) to High(A) do begin
      Result := Result + A[i];
      if i < High(A) then Result := Result + ', ';
    end;
  end;

  function BoolToStr(b: boolean): string;
  begin
    if b then Result := '1' else Result := '0';
  end;

begin
  Cfg.WriteBool(Name, 'Sorted', FSorted);
  Cfg.WriteInteger(Name, 'SortOrder', Integer(FSortOrder));
  Cfg.WriteInteger(Name, 'SortBy', FSortBy);
  SetLength(A, ColCount);
{  // Headers
  if FixedRows > 0 then begin
    for i := 0 to ColCount-1 do A[i] := Cells[i, 0];
    Cfg.WriteString(Name, 'Headers', MakeList(A));
  end;}
  // Columns
  for i := 0 to ColCount-1 do A[i] := IntToStr(Cols[i].Width);
  Cfg.WriteString(Name, 'ColWidths', MakeList(A));
{  for i := 0 to ColCount-1 do A[i] := IntToStr(Integer(Cols[i].Format));
  Cfg.WriteString(Name, 'ColFormat', MakeList(A));}
  for i := 0 to ColCount-1 do A[i] := IntToStr(Cols[i].ImgCol);
  Cfg.WriteString(Name, 'ColImgCol', MakeList(A));
  for i := 0 to ColCount-1 do A[i] := BoolToStr(Cols[i].Enabled);
  Cfg.WriteString(Name, 'ColEnabled', MakeList(A));
end;

function TPowerGrid.GetWrapLine(s: string; Index, Wid: integer;
  var Count: integer): string;
var line, st, en, space: integer;
begin
  if Canvas.TextWidth(s) <= Wid then begin
    Result := s;
    Count := 1;
  end
  else begin
    Result := '';
    st := 1;
    en := 1;
    space := 0;
    line := 0;
    while (en < Length(s)) do begin
      // Scan string while it fits in cell
      while (Canvas.TextWidth(Copy(s, st, en-st+1)) < Wid)
        and (en < Length(s)) do begin
        if s[en] in [' ', '-'] then space := en;
        Inc(en);
      end;
      // Try to break on last space
      if (en < Length(s)) and (space > st) then en := space;
      // If found specified chunk, return it
      if line = Index then Result := Copy(s, st, en-st+1);
      st := en + 1;
      Inc(line);
    end;
    // Return count of chunks found
    Count := line;
  end;
end;

procedure TPowerGrid.ResizeRows;
var i, j, max_count, line_count: integer;
begin
  if not (pgoMultilineCells in Options) then Exit;
  // Change row heights for multiline cells
  for i := 0 to FImgRows.Count-1 do begin
    max_count := 1;
    for j := 0 to TPowerGridRow(FImgRows[i]).Fields.Count-1 do begin
      GetWrapLine(TPowerGridRow(FImgRows[i]).Fields[j], 0, ColWidths[j]-6,
        line_count);
      max_count := Max(max_count, line_count);
    end;
    RowHeights[i + FixedRows] := Canvas.TextHeight('A') * max_count +
      (DefaultRowHeight - Canvas.TextHeight('A'));
  end;
end;

function TPowerGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var i, hei: integer;
begin
  Result := pgoNoSelect in Options;
  if Result then begin
    // Calc row to show whole last page on screen
    hei := FixedRows * DefaultRowHeight;
    i := TopRow;
    while (i < RowCount) and (hei <= Height) do begin
      hei := hei + RowHeights[i];
      Inc(i);
    end;
    if hei > Height then TopRow := TopRow + 1;
  end
  else Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TPowerGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := pgoNoSelect in Options;
  if Result then begin
    if TopRow > FixedRows then TopRow := TopRow - 1;
  end
  else Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

procedure TPowerGrid.WMSize(var Msg: TWMSize);
var i, wid: integer;
begin
  // Stretch last column
  if (ImgColCount > 0) and (pgoStretchLastCol in Options) then begin
    wid := 0;
    for i := 0 to ImgColCount-2 do wid := wid + ColWidths[i];
    ColWidths[ImgColCount-1] := ClientWidth - wid;
  end;
  inherited;
end;

procedure TPowerGrid.SetImageCol(const Value: integer);
begin
  FImageCol := Value;
  Invalidate;
end;

procedure TPowerGrid.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

function TPowerGrid.CellRect(ACol, ARow: Integer): TRect;
begin
  Result := inherited CellRect(ACol, ARow);
end;

function TPowerGrid.GetEditText(ACol, ARow: Integer): string;
begin
  if Assigned(OnGetEditText) then OnGetEditText(Self, ACol, ARow, Result)
  else Result := ImgCells[ACol, ARow];
end;

procedure TPowerGrid.SetEditText(ACol, ARow: Integer; const Value: string);
begin
  ImgCells[ACol, ARow] := Value;
  if Assigned(OnSetEditText) then OnSetEditText(Self, ACol, ARow, Value);
end;

procedure TPowerGrid.UpdateEdit(ACol, ARow: longint);
var CanEditShow: boolean;
    R: TRect;

  procedure UpdateEditor;
  begin
    FInplaceCol := ACol;
    FInplaceRow := ARow;
    FInplaceEdit.UpdateContents;
    if FInplaceEdit.MaxLength = -1 then FCanEditModify := False
    else FCanEditModify := True;
    FInplaceEdit.SelectAll;
  end;

begin
  CanEditShow := {FEditing and }not (csDesigning in ComponentState)
    and HandleAllocated;

  if CanEditShow then
  begin
    if FInplaceEdit = nil then
    begin
      FInplaceEdit := TPwInplaceEdit.Create(Self);
      FInplaceEdit.SetGrid(Self);
      FInplaceEdit.Parent := Self;
      UpdateEditor;
    end
    else
    begin
      if (ACol <> FInplaceCol) or (ARow <> FInplaceRow) then
      begin
        HideEdit;
        UpdateEditor;
      end;
    end;
    if CanEditShow then begin
      R := CellRect(ACol, ARow);
      Dec(R.Right);
      Dec(R.Bottom);
      FInplaceEdit.Move(R);
    end;
  end;
end;

procedure TPowerGrid.ShowEditor(ACol, ARow: longint);
begin
  FEditorMode := True;
  UpdateEdit(ACol, ARow);
end;

procedure TPowerGrid.HideEdit;
begin
  if FInplaceEdit <> nil then
    try
      UpdateText;
    finally
      FInplaceCol := -1;
      FInplaceRow := -1;
      FInplaceEdit.Hide;
    end;
end;


procedure TPowerGrid.HideEditor;
begin
  if FEditorMode then begin
    FEditorMode := False;
    HideEdit;
  end;
end;

procedure TPowerGrid.UpdateText;
begin
  if (FInplaceCol <> -1) and (FInplaceRow <> -1) then
    SetEditText(FInplaceCol, FInplaceRow, FInplaceEdit.Text);
end;

procedure TPowerGrid.SetEditing(const Value: boolean);
begin
  FEditing := Value;
  if not Value then HideEditor;
end;

procedure TPowerGrid.DoExit;
begin
  HideEditor;
  inherited;
end;

procedure TPowerGrid.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
  IsDragging := False;
  if not IsDragged then UpdateSelection(Row);
end;

procedure TPowerGrid.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  IsDragged := True;
end;

function TPowerGrid.SelectedRows: integer;
var i: integer;
begin
  Result := 0;
  for i := FixedRows to RowCount-1 do
    if Rows[i].Selected then Inc(Result);
end;

procedure TPowerGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then HideEditor;
end;

end.

