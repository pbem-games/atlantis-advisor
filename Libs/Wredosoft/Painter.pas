unit Painter;

interface

uses
  SysUtils, Windows, Classes, Controls, Graphics, Messages, ExtCtrls, Forms,
  Math;

const
  crHand = 1;

type
  TMoveCanvas = procedure(Sender: TObject; dx, dy: integer) of object;

  TPainter = class(TCustomControl)
  private
    FRightBtnScroll: boolean;
    FMouseOver, FMouseOut: TNotifyEvent;
    FMoveCanvas: TMoveCanvas;
    FCanvasWidth, FCanvasHeight: integer;
    MouseIsOver: boolean;
    StoredCursor: TCursor;
    OldX, OldY: integer;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure Timeout(Sender: TObject);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    Bitmap: TBitmap;
    CanvasRect: TRect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MoveTo(X, Y: integer);
  published
    property Align;
    property Color;
    property Constraints;
    property CanvasWidth: integer read FCanvasWidth write FCanvasWidth;
    property CanvasHeight: integer read FCanvasHeight write FCanvasHeight;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightBtnScroll: boolean read FRightBtnScroll write FRightBtnScroll;
    property ShowHint;
    property TabStop;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseOver: TNotifyEvent read FMouseOver write FMouseOver;
    property OnMouseOut: TNotifyEvent read FMouseOut write FMouseOut;
    property OnMoveCanvas: TMoveCanvas read FMoveCanvas write FMoveCanvas;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TPainter]);
end;

{ TPainter }

constructor TPainter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csFramed];
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  StoredCursor := Cursor;

  Bitmap := TBitmap.Create;
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  CanvasRect := Rect(0, 0, Width, Height);
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TPainter.Destroy;
begin
  Bitmap.Free;
  inherited;
end;

procedure TPainter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  SetFocus;
  if (Button = mbRight) and RightBtnScroll then
    Cursor := crHand;
end;

procedure TPainter.MouseMove(Shift: TShiftState; X, Y: Integer);
var dx, dy: integer;
begin
  // MouseOver
  if not MouseIsOver then begin
    MouseIsOver := True;
    if Assigned(OnMouseOver) then OnMouseOver(Self);
    with TTimer.Create(Self) do begin
      OnTimer := Timeout;
      Interval := 50;
    end;
    OldX := X;
    OldY := Y;
  end;
  // Scroll
  if FRightBtnScroll and (ssRight in Shift) then begin
    if not ((X = OldX) and (Y = OldY)) then begin
      dx := Min(OldX - X, CanvasWidth - CanvasRect.Right);
      dx := Max(dx, -CanvasRect.Left);
      dy := Min(OldY - Y, CanvasHeight - CanvasRect.Bottom);
      dy := Max(dy, -CanvasRect.Top);
      if (dx <> 0) or (dy <> 0) then begin
        OffsetRect(CanvasRect, dx, dy);
        if Assigned(OnMoveCanvas) then OnMoveCanvas(Self, dx, dy);
      end;
    end;
  end
  else Cursor := StoredCursor;
  OldX := X;
  OldY := Y;
  inherited MouseMove(Shift, X, Y);
end;

procedure TPainter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  Cursor := StoredCursor;
end;

procedure TPainter.MoveTo(X, Y: integer);
var dx, dy: integer;
begin
  if not ((X = CanvasRect.Left) and (Y = CanvasRect.Top)) then begin
    dx := Min(X - CanvasRect.Left, CanvasWidth - CanvasRect.Right);
    dx := Max(dx, -CanvasRect.Left);
    dy := Min(Y - CanvasRect.Top, CanvasHeight - CanvasRect.Bottom);
    dy := Max(dy, -CanvasRect.Top);
    if (dx <> 0) or (dy <> 0) then begin
      OffsetRect(CanvasRect, dx, dy);
      if Assigned(OnMoveCanvas) then OnMoveCanvas(Self, dx, dy);
    end;
  end;
end;

procedure TPainter.Paint;
begin
  Canvas.CopyRect(Canvas.ClipRect, Bitmap.Canvas, Canvas.ClipRect);
end;

procedure TPainter.Timeout(Sender: TObject);
var P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  if not PtInRect(ClientRect, P) then begin
    TTimer(Sender).Free;
    if Assigned(OnMouseOut) then OnMouseOut(Self);
    MouseIsOver := False;
  end;
end;

procedure TPainter.WMSize(var Message: TWMSize);
begin
  Bitmap.Width := Width;
  Bitmap.Height := Height;
  CanvasRect := ClientRect;
end;

procedure TPainter.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

end.
