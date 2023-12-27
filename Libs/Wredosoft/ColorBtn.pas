unit ColorBtn;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Controls, Dialogs;

type
  TColorBtn = class(TCustomControl)
  private
    FColorDialog: TColorDialog;
    FColor: TColor;
    FTransparency: boolean;
    procedure SetColorDialog(const Value: TColorDialog);
    procedure SetColor(const Value: TColor);
    procedure SetTransparency(const Value: boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Color: TColor read FColor write SetColor;
    property ColorDialog: TColorDialog read FColorDialog write SetColorDialog;
    property Constraints;
    property Enabled;
    property Font;
    property Height;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Transparency: boolean read FTransparency write SetTransparency;
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
  end;

 procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TColorBtn]);
end;

  { TColorBtn }

constructor TColorBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Width := 34;
  Height := 17;
end;

destructor TColorBtn.Destroy;
begin
  inherited Destroy;
end;

procedure TColorBtn.Paint;
var i: integer;
    Bmp: TBitmap;
begin
  inherited;
  Bmp := TBitmap.Create;
  Bmp.Width := Width;
  Bmp.Height := Height;
  with Bmp.Canvas do begin
    // Color fill
    if FTransparency and (Color = -1) then begin
      Brush.Color := clBtnFace;
      FillRect(Canvas.ClipRect);
      TextOut(((Width - 5) div 2) - (TextWidth('no') div 2),
        Height div 2 - TextHeight('no') div 2 - 1, 'no');
    end
    else begin
      Brush.Color := Color;
      FillRect(Canvas.ClipRect);
    end;
    // Main frame
    Pen.Color := clBtnHighlight;
    MoveTo(Width-2, 1);
    LineTo(1, 1);
    LineTo(1, Height-2);
    Pen.Color := clBtnShadow;
    LineTo(Width-2, Height-2);
    LineTo(Width-2, 1);
    // Main border
    Brush.Color := clBlack;
    FrameRect(ClipRect);
    if FTransparency then begin
      // Transp frame
      Pen.Color := clBtnFace;
      for i := 0 to 8 do begin
        MoveTo(Width-3 - i, Height-3);
        LineTo(Width-2, Height-4 - i);
      end;
      Pen.Color := clBtnHighlight;
      MoveTo(Width-12, Height-3);
      LineTo(Width-2, Height-13);
      Pen.Color := clBtnShadow;
      MoveTo(Width-13, Height-3);
      LineTo(Width-2, Height-14);
    end;
  end;
  Canvas.CopyRect(Canvas.ClipRect, Bmp.Canvas, Canvas.ClipRect);
  Bmp.Free;
end;

procedure TColorBtn.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if Transparency and ((X - (Width-13)) - ((Height-3) - Y) > 0) then
    Color := -1
  else if Assigned(FColorDialog) then begin
    FColorDialog.Color := Color;
    if FColorDialog.Execute then Color := FColorDialog.Color;
  end;
  if Assigned(OnClick) then OnClick(Self);
end;

procedure TColorBtn.SetColor(const Value: TColor);
begin
  FColor := Value;
  Repaint;
end;

procedure TColorBtn.SetColorDialog(const Value: TColorDialog);
begin
  FColorDialog := Value;
end;

procedure TColorBtn.SetTransparency(const Value: boolean);
begin
  FTransparency := Value;
  Repaint;
end;

end.
