unit ImageBtn;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Dialogs, Messages,
  ExtCtrls;

type
  TImageBtn = class(TCustomControl)
  private
    FMouseOver, FMouseOut: TNotifyEvent;
    FResName, FResOver, FResDown: string;
    Down: boolean;
    MouseIn: boolean;
    Timer: TTimer;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure TimerAlarm(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Constraints;
    property Caption;
    property Enabled;
    property Font;
    property Height;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ResName: string read FResName write FResName;
    property ResOver: string read FResOver write FResOver;
    property ResDown: string read FResDown write FResDown;
    property ShowHint;
    property TabStop;
    property Visible;
    property Width;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
  end;

 procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TImageBtn]);
end;

  { TImageBtn }

constructor TImageBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 34;
  Height := 17;
end;

destructor TImageBtn.Destroy;
begin
  inherited Destroy;
end;

procedure TImageBtn.Paint;
var Bmp, TxBmp: TBitmap;
    s: string;
    x, y: integer;
begin
  inherited;
  Bmp := TBitmap.Create;
  s := FResName;
  if Down and (FindResource(HInstance, PChar(FResDown), RT_BITMAP) > 0) then
    s := FResDown
  else if MouseIn and (FindResource(HInstance, PChar(FResOver),
    RT_BITMAP) > 0) then s := FResOver;
  if FindResource(HInstance, PChar(s), RT_BITMAP) = 0 then begin
    Bmp.Width := Width;
    Bmp.Height := Height;
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Canvas.FillRect(Bmp.Canvas.ClipRect);
    Bmp.Canvas.Brush.Color := clYellow;
    Bmp.Canvas.FrameRect(Bmp.Canvas.ClipRect);
  end
  else begin
    Bmp.LoadFromResourceName(HInstance, s);
    Width := Bmp.Width;
    Height := Bmp.Height;
  end;
  // Caption
  if Caption <> '' then begin
    TxBmp := TBitmap.Create;
    TxBmp.Canvas.Font.Assign(Font);
    with TxBmp.Canvas do begin
      TxBmp.Width := TextWidth(Caption) + 1;
      TxBmp.Height := TextHeight(Caption) + 1;
      Brush.Color := clFuchsia;
      FillRect(ClipRect);
      TextOut(1, 1, Caption);
      TxBmp.Transparent := True;
      TxBmp.TransparentColor := clFuchsia;
    end;
    x := (Width - TxBmp.Width) div 2;
    y := (Height - TxBmp.Height) div 2 - 1;
    if Down then begin
      x := x + 1;
      y := y + 1;
    end;
    Bmp.Canvas.Draw(x, y, TxBmp);
    TxBmp.Free;
  end;
  Canvas.CopyRect(Canvas.ClipRect, Bmp.Canvas, Canvas.ClipRect);
  Bmp.Free;
end;

procedure TImageBtn.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Enabled then begin
    Down := True;
    Invalidate;
  end;
  inherited;
end;

procedure TImageBtn.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Down := False;
  Invalidate;
  inherited;
end;

procedure TImageBtn.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Timer = nil then begin
    Timer := TTimer.Create(Self);
    Timer.OnTimer := TimerAlarm;
    Timer.Interval := 10;
    if Enabled and not MouseIn then begin
      BringToFront;
      MouseIn := True;
      Invalidate;
      if Assigned(FMouseOver) then OnMouseOver(Self);
    end;
  end;
end;

procedure TImageBtn.TimerAlarm(Sender: TObject);
var P: TPoint;
begin
  P := ScreenToClient(Mouse.CursorPos);
  if not PtInRect(ClientRect, P) then begin
    FreeAndNil(Timer);
    MouseIn := False;
    Invalidate;
    if Assigned(FMouseOut) then OnMouseOut(Self);
  end;
end;

end.
