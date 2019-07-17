{*******************************************************************************
    TMemoEx v2.3
    A replacement of a standard VCL TMemo component.

    Copyright (c) 2001-2, TMemoEx.com team
    http://www.tmemoex.com, support@tmemoex.com
*******************************************************************************}

unit MemoEx;

{$WARN SYMBOL_DEPRECATED OFF}

{$IFDEF VER120}
{$DEFINE DELPHI4PLUS}
{$ENDIF}
{$IFDEF VER130}
{$DEFINE DELPHI4PLUS}
{$ENDIF}
{$IFDEF VER140}
{$DEFINE DELPHI4PLUS}
{$ENDIF}

{$IFNDEF DELPHI4PLUS}
error: delphi4+ needed to compile this unit
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, ClipBrd, MemoExScrollBar, MemoExXPCompat{$IFDEF VER140}, RTLConsts{$ENDIF};

const
  WM_EDITCOMMAND = WM_USER + $101;

  ME_EX_STYLE_DEFAULT = 0;          //  Extended text style

  //  Case conversion operations codes
  ME_CASE_CONVERT_UPPER   = 0;      //  Convert to upper case
  ME_CASE_CONVERT_LOWER   = 1;      //  Convert to lower case
  ME_CASE_CONVERT_INVERT  = 2;      //  Invert case

type
  TCellRect = record
    Width: integer;
    Height: integer;
  end;

  TLineAttr = record
    FC, BC: TColor;
    Style: TFontStyles;
    ex_style: byte;
  end;

  TCustomMemoEx = class;

  {
    Line of attributes.
    Unlimited array helps to not to think about very long lines.
  }
  TLineAttrs = array of TLineAttr;

  {
    Line of selected symbols.
  }
  TSelAttrs  = array of boolean;

  {
    User-defined attributes provider.
  }
  TOnGetLineAttr = procedure(Sender: TObject; const Line: string; Index: integer;
    const SelAttrs: TSelAttrs; var Attrs: TLineAttrs) of object;

  {
    Event which fires when gutter is being painted.
  }
  TOnPaintGutter = procedure (Sender: TObject; Canvas: TCanvas) of object;

  {
    Event which fires on status (insert/overwrite mode, record mode,
    undo buffer, selection) change.
  }
  TOnChangeStatus = TNotifyEvent;

  {
    Event which fires on clipboard change.
    CanPaste is true when there is a text in clipboard.
  }
  TOnChangeClipboardState = procedure (Sender: TObject; const CanPaste: boolean) of object;

  {
    Event which fires when user click on the word.
    The word is the text with one style.
    WordText contains text of the word.
    WordStyle contains style of the word.
  }
  TOnWordClick = procedure (Sender: TObject; const WordText: string; WordStyle: word) of object;

  {
    Event which fires when user move mouse over the text.
    WordStyle contains style of the word under mouse cursor.
    _Cursor contains one of the available cursor images index.
  }
  TOnMouseOver = procedure (Sender: TObject; WordStyle: word; var _Cursor: TCursor) of object;

  {
    Event which fires when the line is being break.
    Original contains not breaked yet line.
    _New contains part of the line which will be wrapped.
  }
  TOnBreakLine = procedure (Sender: TObject; const Original: string; var _New: string) of object;

  {
    Event which fires when two lines are being pasted together.
    Original contains line to which _New will be pasted.
    _New contains line being pated to Original.
  }
  TOnConcatLine = procedure (Sender: TObject; const Original: string; var _New: string) of object;

  {
    Event which fires when the text is being inserted with InsertTextAtCurrentPos,
    block insert or pasted from clipboard.
  }
  TOnTextInsert = procedure (Sender: TObject; var Text: string) of object;

  {
    User-defined case conversion routine.
  }
  TOnCaseConversion = function (Sender: TObject; Conversion: byte; const Text: string): string of object;

  {
    Block insert.
    Text contains a text of the block being inserted.
  }
  TOnInsertBlock = function (Sender: TObject; var Text: string): boolean of object;

  {
    Block save.
    Text contains a text of the block for saving in file.
  }
  TOnSaveBlock = procedure (Sender: TObject; const Text: string) of object;

  {
    MacroID is the identifier of the macro shortcut.
    The result of the event is the text being inserted at current position.
  }
  TOnInsertMacro = function (Sender: TObject; MacroID: integer): string of object;

  {
    User-defined block operation.
    MacroID contains identifier of the block operation.
    Text contains the text of the block to operate.
    The result of the event is the modified Text.
  }
  TOnBlockOperation = function (Sender: TObject; MacroID: integer; const Text: string): string of object;

  {
    User-defined operation with auto-completion text.
    Text contains original text for defined auto-completion.
    The result of the event is the modified Text.
  }
  TOnPreprocessCompletion = function (Sender: TObject; const ID, Text: string): string of object;

  {
    Words for auto-change feature.
  }
  PAutoChangeWord = ^TAutoChangeWord;
  TAutoChangeWord = record
    OldWord, NewWord: string;
  end;

  {
    Paragraph.
    All lines of the file separated by <CR> or <LF> symbols are paragraphs.
    Paragraph contains wrapped lines.
  }
  TParagraph = record
    FChanged: boolean;              //  paragraph was changed
    FPreCount,
    FCount: integer;                //  number of wrapped lines
    FStrings: array of string;      //  wrapped lines
    FAttrs: TLineAttrs;             //  paragraph attributes
  end;

  {
    List of the paragraphs.
  }
  PParagraphList = ^TParagraphList;
  TParagraphList = array [0..MaxListSize div 2] of TParagraph;

  {
    Editor lines storage.
    Like TStrings but with word-wrap features.
  }
  TMemoExStrings = class(TStrings)
  private
    FMemoEx: TCustomMemoEx;
    FList: PParagraphList;
    FParaLinesCount, FCount: integer;
    FCapacity: integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;

    {
      Event which fires after loading the file by LoadFromFile.
    }
    FOnAfterLoad: TNotifyEvent;

    {
      Event which fires before saving lines to file by SaveToFile.
    }
    FOnBeforeSave: TNotifyEvent;

    procedure FinalizeParagraph(Index: integer);
    procedure FinalizeParagraphs;
    procedure Recount(Index: integer);
    function _GetString(ParaIndex: integer): string;
    procedure _PutString(ParaIndex: integer; const S: string);
    procedure ReformatParagraph(ParaIndex: integer);
    procedure Reformat;
    procedure CheckLength(const st: string);
    procedure SetLockText(const Text: string);
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: integer): string; override;
    function GetCapacity: integer; override;
    function GetCount: integer; override;
    function GetParaLineCount: integer;
    function GetParaString(Index: integer): string;
    function GetParagraph(Index: integer): TParagraph;
    procedure Put(Index: integer; const S: string); override;
    procedure PutParaString(Index: integer; const S: string);
    procedure SetCapacity(NewCapacity: integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetTextStr(const Value: string); override;
    procedure SetInternal(Index: integer; const Value: string);
    procedure SetInternalParaStr(Index: integer; const Value: string);

    {
      AddParaStr adds wrapped line to paragraph.
    }
    function AddParaStr(ParaIndex: integer; const S: string): integer;
    function InsertAtPos(Index: integer; const S: string): integer;

    procedure Index2ParaIndex(Index: integer; var Para, ParaIndex: integer);
    function GetParagraphByIndex(Index: integer; var ParaIndex, IndexOffs: integer): string;
    procedure Caret2Paragraph(X, Y: integer; var ParaIndex, IndexOffs: integer);
    procedure Paragraph2Caret(ParaIndex, IndexOffs: integer; var X, Y: integer);
    function GetParaOffs(ParaIndex: integer): integer;
    procedure ReLine;

    procedure SetParaChanged(ParaIndex: integer);

    property Internal[Index: integer]: string write SetInternal;
    property InternalParaStrings[Index: integer]: string write SetInternalParaStr;
    property Paragraphs[Index: integer]: TParagraph read GetParagraph;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    {
      Add, Delete and Insert used for work with paragraphs.
      It was done for compatibility with TStrings.
      When adding the line (paragraph) it will be automatically wrapped.
    }
    function Add(const S: string): integer; override;
    procedure Delete(Index: integer); override;
    procedure Insert(Index: integer; const S: string); override;

    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;

    {
      ParaLineCount contains number of all wrapped lines in list.
      Count contains number of paragraphs in list.
    }
    property ParaLineCount: integer read GetParaLineCount;
    {
      ParaStrings return wrapped line.
      Strings return paragraph line.
    }
    property ParaStrings[Index: integer]: string read GetParaString write PutParaString;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  {
    Bookmark.
  }
  TBookmark = record
    X, Y: integer;                  //  coordinates
    Valid: boolean;                 //  is bookmark valid
  end;

  TBookmarkNum = 0..9;
  TBookmarks = array[TBookmarkNum] of TBookmark;

  TEditorClient = class
  private
    FMemoEx: TCustomMemoEx;
    Top: integer;
    function Left: integer;
    function Height: integer;
    function Width: integer;
    function ClientWidth: integer;
    function ClientHeight: integer;
    function ClientRect: TRect;
    function BoundsRect: TRect;
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  TGutter = class(TObject)
  private
    FMemoEx: TCustomMemoEx;
    FFont: TFont;
    FDrawBitmap: TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Paint;
    procedure Invalidate;
  end;

  TEditCommand = word;

  TMacro = string;

  TEditKey = class
  public
    Key1, Key2: Word;
    Shift1, Shift2: TShiftState;
    Command: TEditCommand;
    constructor Create(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    constructor Create2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
  end;

  TKeyboard = class
  private
    List: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState);
    procedure Add2(const ACommand: TEditCommand; const AKey1: word;
      const AShift1: TShiftState; const AKey2: word;
      const AShift2: TShiftState);
    procedure Clear;
    function Command(const AKey: word; const AShift: TShiftState): TEditCommand;
    function Command2(const AKey1: word; const AShift1: TShiftState;
      const AKey2: word; const AShift2: TShiftState): TEditCommand;
    procedure SetDefLayout;
  end;

  EMemoExError = class(Exception);

  TUndoBuffer = class;

  TUndo = class
  private
    FMemoEx: TCustomMemoEx;
    function UndoBuffer: TUndoBuffer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx);
    procedure Undo; dynamic; abstract;
    procedure Redo; dynamic; abstract;
  end;

  TUndoBuffer = class(TList)
  private
    FMemoEx: TCustomMemoEx;
    FPtr: integer;
    FCancelUndo, InUndo: boolean;
    function LastUndo: TUndo;
    function IsNewGroup(const AUndo: TUndo): boolean;
  public
    constructor Create;
    procedure Add(var AUndo: TUndo);
    procedure Undo;
    procedure Redo;
    procedure Clear; override;
    procedure Delete;
  end;

  TCompletion = class;
  TOnCompletion = procedure(Sender: TObject; var Cancel: boolean) of object;

  {$IFDEF VER120}
  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: HWND;
    case Integer of
      0: (
        XPos: Smallint;
        YPos: Smallint);
      1: (
        Pos: TSmallPoint;
        Result: Longint);
  end;
  {$ENDIF}

  TTabStop = (tsTabStop, tsAutoIndent);

  EInvalidRightMarginValue = class(Exception);

  { TCustomMemoEx }

  TCustomMemoEx = class(TCustomControl)
  private
    { internal objects }
    FLines: TMemoExStrings;
    scbHorz: TMemoExScrollBar;
    scbVert: TMemoExScrollBar;
    EditorClient: TEditorClient;
    FGutter: TGutter;
    FKeyboard: TKeyboard;
    FBookmarks: TBookmarks;
    FUpdateLock: integer;
    FUndoBuffer: TUndoBuffer;
    FGroupUndo: boolean;
    FCompletion: TCompletion;

    FCols, FRows: integer;
    FLeftCol, FTopRow: integer;
    FLastVisibleCol, FLastVisibleRow: integer;
    FCaretX, FCaretY: integer;
    FVisibleColCount: integer;
    FVisibleRowCount: integer;

    FAllRepaint: boolean;
    FCellRect: TCellRect;
    IgnoreKeyPress: boolean;
    WaitSecondKey: Boolean;
    Key1: Word;
    Shift1: TShiftState;

    { internal - selection attributes }
    FSelected: boolean;
    FSelBlock: boolean;             //  reserved
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
    FUpdateSelBegX, FUpdateSelEndX, FUpdateSelBegY, FUpdateSelEndY: integer;
    FSelStartX, FSelStartY: integer;
    FclSelectBC, FclSelectFC: TColor;

    { mouse support }
    timerScroll: TTimer;
    MouseMoveX, MouseMoveY, MouseMoveXX, MouseMoveYY: integer;

    { internal }
    FTabPos: array of boolean;
    FTabStops: string {$IFDEF VER140} deprecated{$ENDIF};
    FCharWidth: array of integer;
    FTabSize, FIndentSize, FAutoIndentSize: integer;
    FSmartIndent, FSmartTab: boolean;

    { internal - primary for TIReader support }
    FEditBuffer: string;
    FPEditBuffer: PChar;
    FEditBufferSize: integer;

    FCompound: integer;
    { FMacro - buffer of TEditCommand, each command represents by two chars }
    FMacro: TMacro;
    FDefMacro: TMacro;

    { visual attributes - properties }
    FBorderStyle: TBorderStyle;
    FGutterColor: TColor;
    FGutterWidth: integer;
    FRightMarginVisible: boolean;
    FRightMargin, FRealRightMargin: integer;
    FRightMarginColor: TColor;
    FScrollBars: TScrollStyle;
    FDoubleClickLine: boolean;
    FBackspaceUnindents: Boolean;
    FAutoIndent: Boolean;
    FKeepTrailingBlanks: Boolean;
    FCursorBeyondEOF: Boolean;
    FCursorBeyondEOL: Boolean;
    FInclusive: Boolean;            //  include last symbol into selection or not

    FSimpleBeginLine: boolean;

    { non-visual attributes - properties }
    FInsertMode: boolean;
    FReadOnly: boolean;
    FModified: boolean;
    FRecording: boolean;

    { Events }
    FOnGetLineAttr: TOnGetLineAttr;
    FOnChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnChangeStatus: TOnChangeStatus;
    FOnChangeClipboardState: TOnChangeClipboardState;
    FOnScroll: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaintGutter: TOnPaintGutter;
    FOnWordClick: TOnWordClick;
    FOnMouseOver: TOnMouseOver;
    FOnBreakLine: TOnBreakLine;
    FOnConcatLine: TOnConcatLine;
    FOnTextInsert: TOnTextInsert;
    FOnCaseConversion: TOnCaseConversion;
    FOnInsertBlock: TOnInsertBlock;
    FOnSaveBlock: TOnSaveBlock;
    FOnInsertMacro: TOnInsertMacro;
    FOnBlockOperation: TOnBlockOperation;

    FOnCompletionIdentifier: TOnCompletion;
    FOnCompletionTemplate: TOnCompletion;
    FOnCompletionDrawItem: TDrawItemEvent;
    FOnCompletionMeasureItem: TMeasureItemEvent;
    FOnPreprocessCompletion: TOnPreprocessCompletion;

    FDrawBitmap: TBitmap;

    FFont: TFont;

    FWantTabs: boolean;

    FWordWrap: boolean;

    FStripInvisible: boolean;

    FParaX, FParaY: integer;

    NextClipViewer: THandle;
    scbVertWidth, scbHorzHeight: integer;
    Max_X: integer;

    mouse_down, mouse_dragged, double_clicked, gutter_clicked: boolean;
    FWordUnderCursor: string;
    FWordStyleUnderCursor: byte;

    FUseMaxCharWidth: boolean;

    FCurrentTheme: HTHEME;

    procedure SetUseMaxCharWidth(const Value: boolean);
    procedure SetFont(Value: TFont);

    procedure SetMax_X(const Value: integer);
    procedure UpdateEditorSize(const FullUpdate: boolean = true; const RepaintGutter: boolean = true);
    procedure RedrawFrom(YFrom: integer);
    function RepaintParagraph(LineIndex: integer): integer;

    procedure DoCompletionIdentifier(var Cancel: boolean);
    procedure DoCompletionTemplate(var Cancel: boolean);
    function DoPreprocessCompletion(const ID, OldText: string): string;

    procedure ScrollTimer(Sender: TObject);

    procedure ReLine;
    function GetDefTabStop(const X: integer; const Next: Boolean): integer;
    function GetTabStop(const X, Y: integer; const What: TTabStop;
      const Next: Boolean): integer;
    function GetBackStop(const X, Y: integer): integer;

    procedure TextAllChangedInternal(const Unselect: Boolean);

    { property }
    procedure SetGutterWidth(AWidth: integer);
    procedure SetGutterColor(AColor: TColor);
    function GetLines: TStrings;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetLines(ALines: TStrings);
    function GetRealOffs(DefOffs, Index: integer): integer;
    function GetSelStart: integer;
    procedure SetSelStart(const ASelStart: integer);
    procedure SetSelLength(const ASelLength: integer);
    function GetSelLength: integer;
    procedure SetMode(index: integer; Value: boolean);
    procedure SetCaretPosition(const index, Pos: integer);
    procedure SetCols(ACols: integer);
    procedure SetRows(ARows: integer);
    procedure SetScrollBars(Value: TScrollStyle);
    procedure SetRightMarginVisible(Value: boolean);
    procedure SetRightMargin(Value: integer);
    procedure SetRightMarginColor(Value: TColor);

    function ExtractStringWithStyle(XX, YY: integer; const From: string; Style: word; const LineAttrs: TLineAttrs): string;
    procedure GetWordUnderCursor(X, Y: integer);

    function GetAfterLoad: TNotifyEvent;
    procedure SetAfterLoad(Value: TNotifyEvent);
    function GetBeforeSave: TNotifyEvent;
    procedure SetBeforeSave(Value: TNotifyEvent);

    procedure SetWordWrap(Value: boolean);
    procedure SetStripInvisible(Value: boolean);
    procedure SetSelectedText(Value: boolean);

    procedure FontChanged(Sender: TObject);

    function GetPlainText: string;
    procedure SetPlainText(const AValue: string);

    function GetCaretPos: TPoint;

    function GetCanUndo: boolean;


    function GetThemeState: integer;

    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    SelAttrs_Size: integer;
    SelAttrs: TSelAttrs;

    property FSelectedText: boolean read FSelected write SetSelectedText;

    procedure Resize; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var
      ScrollPos: integer);
    procedure Scroll(const Vert: boolean; const ScrollPos: integer);
    procedure PaintLine(const Line: integer; ColBeg, ColEnd: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure KeyPress(var Key: Char); override;
    procedure InsertChar(const Key: Char);

    procedure SetSel(const ASelX, ASelY: integer);
    function GetAttrDelta(StartFrom, EndTo: integer; const LineAttrs: TLineAttrs): integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
    procedure PaintSelection;
    procedure SetUnSelected;
    procedure Mouse2Cell(const X, Y: integer; var CX, CY: integer);
    procedure Mouse2Caret(const X, Y: integer; var CX, CY: integer);
    procedure CaretCoord(const X, Y: integer; var CX, CY: integer);
    function PosFromMouse(const X, Y: integer): integer;
    procedure SetLockText(const Text: string);
    function ExpandTabs(const S: string): string;

    procedure CantUndo {$IFDEF VER140}; deprecated{$ENDIF};

    procedure SetCaretInternal(X, Y: integer);
    procedure ValidateEditBuffer;

    procedure ChangeBookmark(const Bookmark: TBookmarkNum; const Valid:
      boolean);
    procedure InsertText(const Text: string);
    procedure BeginRecord;
    procedure EndRecord(var AMacro: TMacro);
    procedure PlayMacro(const AMacro: TMacro);
    function YinBounds(AY: integer): boolean;
    function DoChangeCase(const st: string; Conversion: byte): string;

    { triggers for descendants }
    procedure Changed; dynamic;
    procedure TextAllChanged; dynamic;
    procedure StatusChanged; dynamic;
    procedure SelectionChanged; dynamic;
    procedure ClipboardChanged; dynamic;
    procedure GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg, ColEnd: integer; const ALine: string); virtual;
    procedure GetAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure ChangeAttr(Line, ColBeg, ColEnd: integer); virtual;
    procedure GutterPaint(Canvas: TCanvas); dynamic;
    procedure CompletionIdentifier(var Cancel: boolean); dynamic;
    procedure CompletionTemplate(var Cancel: boolean); dynamic;
    procedure BookmarkCnanged(Bookmark: integer); dynamic;
    function GetBookmark(AIndex: integer): TBookmark;
    procedure SetBookmark(AIndex: integer; ABookmark: TBookmark);

    property Gutter: TGutter read FGutter;
    property Cols: integer read FCols write SetCols;
    property Rows: integer read FRows write SetRows;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Invalidate2;
    procedure InvalidateGutter;
    procedure InvalidateLine(Index: integer);

    function LineHeight: integer;

    procedure WndProc(var Message: TMessage); override;

    procedure SetLeftTop(ALeftCol, ATopRow: integer);

    function CalcCellRect(const X, Y: integer): TRect;
    procedure SetCaret(const X, Y: integer);
    procedure CaretFromPos(const Pos: integer; var X, Y: integer);
    function PosFromCaret(const X, Y: integer): integer;
    procedure PaintCaret(const bShow: boolean);
    function GetTextLen: integer;
    function GetSelText: string;
    procedure SetSelText(const AValue: string);
    function GetWordOnCaret: string;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MakeRowVisible(ARow: integer);

    procedure Command(ACommand: TEditCommand); virtual;
    procedure PostCommand(ACommand: TEditCommand);
    procedure InsertTextAtCurrentPos(const AText: string);
    procedure ReplaceWord(const NewString: string);
    procedure InvalidateBookmarks;
    procedure BeginCompound;
    procedure EndCompound;

    function GetText(Position: longint; Buffer: PChar; Count: longint): longint;

    procedure ClipBoardCopy {$IFDEF VER140}; deprecated{$ENDIF};
    procedure ClipBoardPaste {$IFDEF VER140}; deprecated{$ENDIF};
    procedure ClipBoardCut {$IFDEF VER140}; deprecated{$ENDIF};
    procedure DeleteSelected {$IFDEF VER140}; deprecated{$ENDIF};
    function IsUndoEmpty: boolean {$IFDEF VER140}; deprecated{$ENDIF};

    procedure Clear;
    procedure SetCaretPos(const AValue: TPoint);
    procedure ClearSelection;
    procedure ClearUndo;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure SelectAll;
    procedure Undo;

    procedure MouseWheelScroll(Delta: integer);

    property LeftCol: integer read FLeftCol;
    property TopRow: integer read FTopRow;
    property VisibleColCount: integer read FVisibleColCount;
    property VisibleRowCount: integer read FVisibleRowCount;
    property LastVisibleCol: integer read FLastVisibleCol;
    property LastVisibleRow: integer read FLastVisibleRow;
    property CaretX: integer index 0 read FCaretX write SetCaretPosition;
    property CaretY: integer index 1 read FCaretY write SetCaretPosition;
    property CaretPos: TPoint read GetCaretPos;
    property Modified: boolean read FModified write FModified;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    property SelectedText: boolean read FSelected;
    property PlainText: string read GetPlainText write SetPlainText;
    property Text: string read GetPlainText write SetPlainText;
    property Bookmarks[Index: integer]: TBookmark read GetBookmark write SetBookmark;
    property Keyboard: TKeyboard read FKeyboard;
    property CellRect: TCellRect read FCellRect;
    property UndoBuffer: TUndoBuffer read FUndoBuffer;
    property Recording: boolean read FRecording;
    property WordUnderCursor: string read FWordUnderCursor;
    property WordStyleUnderCursor: byte read FWordStyleUnderCursor;
    property CanUndo: boolean read GetCanUndo;
  public { published in descendants }
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Lines: TStrings read GetLines write SetLines;
    property LinesEx: TMemoExStrings read FLines write FLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Cursor default crIBeam;
    property Color default clWindow;

    property Font: TFont read FFont write SetFont;

    property GutterWidth: integer read FGutterWidth write SetGutterWidth;
    property GutterColor: TColor read FGutterColor write SetGutterColor default clBtnFace;

    property RightMarginVisible: boolean read FRightMarginVisible write SetRightMarginVisible default true;
    property RightMargin: integer read FRightMargin write SetRightMargin default 80;
    property RightMarginColor: TColor read FRightMarginColor write SetRightMarginColor default clBtnFace;
    property InsertMode: boolean index 0 read FInsertMode write SetMode default true;
    property ReadOnly: boolean index 1 read FReadOnly write SetMode default false;
    property DoubleClickLine: boolean read FDoubleClickLine write FDoubleClickLine default false;
    property Completion: TCompletion read FCompletion write FCompletion;
    property TabStops: string read FTabStops write FTabStops;
    property TabSize: integer read FTabSize write FTabSize;
    property IndentSize: integer read FIndentSize write FIndentSize;
    property AutoIndentSize: integer read FAutoIndentSize write FAutoIndentSize;
    property SmartTab: boolean read FSmartTab write FSmartTab default true;
    property SmartAutoIndent: boolean read FSmartIndent write FSmartIndent default true;
    property BackspaceUnindents: Boolean read FBackspaceUnindents write FBackspaceUnindents default true;
    property AutoIndent: Boolean read FAutoIndent write FAutoIndent default true;
    property KeepTrailingBlanks: Boolean read FKeepTrailingBlanks write FKeepTrailingBlanks default false;
    property CursorBeyondEOF: Boolean read FCursorBeyondEOF write FCursorBeyondEOF default false;
    property CursorBeyondEOL: Boolean read FCursorBeyondEOL write FCursorBeyondEOL default true;
    property SelForeColor: TColor read FclSelectFC write FclSelectFC;
    property SelBackColor: TColor read FclSelectBC write FclSelectBC;

    property StripInvisible: boolean read FStripInvisible write SetStripInvisible default false;
    property WantTabs: boolean read FWantTabs write FWantTabs default true;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;

    property SimpleBeginLine: boolean read FSimpleBeginLine write FSimpleBeginLine default true;
    property UseMaxCharWidth: boolean read FUseMaxCharWidth write SetUseMaxCharWidth default true;

    property OnAfterLoad: TNotifyEvent read GetAfterLoad write SetAfterLoad;
    property OnBeforeSave: TNotifyEvent read GetBeforeSave write SetBeforeSave;

    property OnGetLineAttr: TOnGetLineAttr read FOnGetLineAttr write FOnGetLineAttr;
    property OnChangeStatus: TOnChangeStatus read FOnChangeStatus write FOnChangeStatus;
    property OnChangeClipboardState: TOnChangeClipboardState read FOnChangeClipboardState write FOnChangeClipboardState;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaintGutter: TOnPaintGutter read FOnPaintGutter write FOnPaintGutter;
    property OnMouseOver: TOnMouseOver read FOnMouseOver write FOnMouseOver;
    property OnWordClick: TOnWordClick read FOnWordClick write FOnWordClick;
    property OnBreakLine: TOnBreakLine read FOnBreakLine write FOnBreakLine;
    property OnConcatLine: TOnConcatLine read FOnConcatLine write FOnConcatLine;
    property OnTextInsert: TOnTextInsert read FOnTextInsert write FOnTextInsert;
    property OnCaseConversion: TOnCaseConversion read FOnCaseConversion write FOnCaseConversion;
    property OnInsertBlock: TOnInsertBlock read FOnInsertBlock write FOnInsertBlock;
    property OnSaveBlock: TOnSaveBlock read FOnSaveBlock write FOnSaveBlock;
    property OnInsertMacro: TOnInsertMacro read FOnInsertMacro write FOnInsertMacro;
    property OnBlockOperation: TOnBlockOperation read FOnBlockOperation write FOnBlockOperation;
    property OnCompletionIdentifier: TOnCompletion read FOnCompletionIdentifier write FOnCompletionIdentifier;
    property OnCompletionTemplate: TOnCompletion read FOnCompletionTemplate write FOnCompletionTemplate;
    property OnCompletionDrawItem: TDrawItemEvent read FOnCompletionDrawItem write FOnCompletionDrawItem;
    property OnCompletionMeasureItem: TMeasureItemEvent read FOnCompletionMeasureItem write FOnCompletionMeasureItem;
    property OnPreprocessCompletion: TOnPreprocessCompletion read FOnPreprocessCompletion write FOnPreprocessCompletion;
    property DockManager;
  end;

  TMemoEx = class(TCustomMemoEx)
  published
    property TabOrder;
    property BorderStyle;
    property Lines;
    property ScrollBars;
    property GutterWidth;
    property GutterColor;
    property RightMarginVisible;
    property RightMargin;
    property RightMarginColor;
    property InsertMode;
    property ReadOnly;
    property DoubleClickLine;
    property Completion;
    property TabStops;
    property TabSize;
    property IndentSize;
    property AutoIndentSize;
    property SmartTab;
    property SmartAutoIndent;
    property BackspaceUnindents;
    property AutoIndent;
    property KeepTrailingBlanks;
    property CursorBeyondEOF;
    property CursorBeyondEOL;
    property SelForeColor;
    property SelBackColor;

    property StripInvisible;

    property SimpleBeginLine;
    property UseMaxCharWidth;

    property OnAfterLoad;
    property OnBeforeSave;

    property OnEnter;
    property OnExit;
    property OnGetLineAttr;
    property OnChangeStatus;
    property OnChangeClipboardState;
    property OnScroll;
    property OnResize;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnChange;
    property OnSelectionChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaintGutter;
    property OnMouseOver;
    property OnWordClick;
    property OnBreakLine;
    property OnConcatLine;
    property OnTextInsert;
    property OnCaseConversion;
    property OnInsertBlock;
    property OnSaveBlock;
    property OnInsertMacro;
    property OnBlockOperation;
    property OnCompletionIdentifier;
    property OnCompletionTemplate;
    property OnCompletionDrawItem;
    property OnCompletionMeasureItem;
    property OnPreprocessCompletion;

    { TCustomControl }
    property Align;
    property Enabled;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property Visible;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property UseDockManager default true;
    property DockSite;
    property DragKind;
    property ParentBiDiMode;

    property WantTabs default true;
    property WordWrap default true;

    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnUnDock;
  end;

  TCompletionList = (cmIdentifiers, cmTemplates);

  TCompletion = class(TPersistent)
  private
    FMemoEx: TCustomMemoEx;
    FPopupList: TListBox;
    FAutoChange: TStrings;
    FAutoChangeList: TList;
    FIdentifiers: TStrings;
    FTemplates: TStrings;
    FItems: TStringList;
    FItemIndex: integer;
    FMode: TCompletionList;
    FDefMode: TCompletionList;
    FItemHeight: integer;
    FTimer: TTimer;
    FEnabled: boolean;
    FVisible: boolean;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FListBoxStyle: TListBoxStyle;
    FCaretChar: char;
    FCRLF: string;
    FSeparator: string;
    function DoKeyDown(Key: Word; Shift: TShiftState): boolean;
    procedure DoKeyPress(Key: Char);
    procedure OnTimer(Sender: TObject);
    procedure FindSelItem(var Eq: boolean);
    procedure ReplaceWord(const ANewString: string);

    function Cmp1(const S1, S2: string): integer;
    function Cmp2(const S1, S2: string): boolean;

    procedure AutoChangeChanged(Sender: TObject);
    procedure ClearAutoChangeList;
    procedure UpdateAutoChange;
    procedure SetStrings(index: integer; AValue: TStrings);
    function GetItemIndex: integer;
    procedure SetItemIndex(AValue: integer);
    function GetInterval: cardinal;
    procedure SetInterval(AValue: cardinal);
    procedure MakeItems;
    function GetItems: TStrings;
  public
    constructor Create2(AMemoEx: TCustomMemoEx);
    destructor Destroy; override;
    procedure DropDown(const AMode: TCompletionList; const ShowAlways: boolean);
    procedure DoCompletion(const AMode: TCompletionList);
    procedure CloseUp(const Apply: boolean);
    procedure SelectItem;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Visible: boolean read FVisible write FVisible;
    property Mode: TCompletionList read FMode write FMode;
    property Items: TStringList read FItems;
  published
    property DropDownCount: integer read FDropDownCount write FDropDownCount
      default 6;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth
      default 300;
    property Enabled: boolean read FEnabled write FEnabled default true;
    property Separator: string read FSeparator write FSeparator;
    property Identifiers: TStrings index 0 read FIdentifiers write SetStrings;
    property Templates: TStrings index 1 read FTemplates write SetStrings;
    property AutoChange: TStrings index 2 read FAutoChange write SetStrings;
    property ItemHeight: integer read FItemHeight write FItemHeight;
    property Interval: cardinal read GetInterval write SetInterval;
    property ListBoxStyle: TListBoxStyle read FListBoxStyle write FListBoxStyle;
    property CaretChar: char read FCaretChar write FCaretChar;
    property CRLF: string read FCRLF write FCRLF;
  end;

const

 { Editor commands }

  ecCharFirst = $00;
  ecCharLast = $FF;
  ecCommandFirst = $100;
  ecUser = $8000; { use this for descendants }

  ecLeft = ecCommandFirst + 1;
  ecUp = ecLeft + 1;
  ecRight = ecLeft + 2;
  ecDown = ecLeft + 3;
  ecSelLeft = ecCommandFirst + 9;
  ecSelUp = ecSelLeft + 1;
  ecSelRight = ecSelLeft + 2;
  ecSelDown = ecSelLeft + 3;
  ecPrevWord = ecSelDown + 1;
  ecNextWord = ecPrevWord + 1;
  ecSelPrevWord = ecPrevWord + 2;
  ecSelNextWord = ecPrevWord + 3;
  ecSelWord = ecPrevWord + 4;

  ecWindowTop = ecSelWord + 1;
  ecWindowBottom = ecWindowTop + 1;
  ecPrevPage = ecWindowTop + 2;
  ecNextPage = ecWindowTop + 3;
  ecSelPrevPage = ecWindowTop + 4;
  ecSelNextPage = ecWindowTop + 5;

  ecBeginLine = ecSelNextPage + 1;
  ecEndLine = ecBeginLine + 1;
  ecBeginDoc = ecBeginLine + 2;
  ecEndDoc = ecBeginLine + 3;
  ecSelBeginLine = ecBeginLine + 4;
  ecSelEndLine = ecBeginLine + 5;
  ecSelBeginDoc = ecBeginLine + 6;
  ecSelEndDoc = ecBeginLine + 7;
  ecSelAll = ecBeginLine + 8;

  ecScrollLineUp = ecSelAll + 1;
  ecScrollLineDown = ecScrollLineUp + 1;

  ecInsertPara = ecCommandFirst + 101;
  ecBackspace = ecInsertPara + 1;
  ecDelete = ecInsertPara + 2;
  ecChangeInsertMode = ecInsertPara + 3;
  ecTab = ecInsertPara + 4;
  ecBackTab = ecInsertPara + 5;
  ecIndent = ecInsertPara + 6;
  ecUnindent = ecInsertPara + 7;

  ecDeleteSelected = ecInsertPara + 10;
  ecClipboardCopy = ecInsertPara + 11;
  ecClipboardCut = ecClipboardCopy + 1;
  ecClipBoardPaste = ecClipboardCopy + 2;

  ecDeleteLine = ecClipBoardPaste + 1;
  ecDeleteWord = ecDeleteLine + 1;

  ecToUpperCase = ecDeleteLine + 2;
  ecToLowerCase = ecToUpperCase + 1;
  ecChangeCase = ecToUpperCase + 2;

  ecUndo = ecChangeCase + 1;
  ecRedo = ecUndo + 1;
  ecBeginCompound = ecUndo + 2; { not implemented }
  ecEndCompound = ecUndo + 3; { not implemented }

  ecBeginUpdate = ecUndo + 4;
  ecEndUpdate = ecUndo + 5;

  ecSetBookmark0 = ecEndUpdate + 1;
  ecSetBookmark1 = ecSetBookmark0 + 1;
  ecSetBookmark2 = ecSetBookmark0 + 2;
  ecSetBookmark3 = ecSetBookmark0 + 3;
  ecSetBookmark4 = ecSetBookmark0 + 4;
  ecSetBookmark5 = ecSetBookmark0 + 5;
  ecSetBookmark6 = ecSetBookmark0 + 6;
  ecSetBookmark7 = ecSetBookmark0 + 7;
  ecSetBookmark8 = ecSetBookmark0 + 8;
  ecSetBookmark9 = ecSetBookmark0 + 9;

  ecGotoBookmark0 = ecSetBookmark9 + 1;
  ecGotoBookmark1 = ecGotoBookmark0 + 1;
  ecGotoBookmark2 = ecGotoBookmark0 + 2;
  ecGotoBookmark3 = ecGotoBookmark0 + 3;
  ecGotoBookmark4 = ecGotoBookmark0 + 4;
  ecGotoBookmark5 = ecGotoBookmark0 + 5;
  ecGotoBookmark6 = ecGotoBookmark0 + 6;
  ecGotoBookmark7 = ecGotoBookmark0 + 7;
  ecGotoBookmark8 = ecGotoBookmark0 + 8;
  ecGotoBookmark9 = ecGotoBookmark0 + 9;

  ecCompletionIdentifiers = ecGotoBookmark9 + 1;
  ecCompletionTemplates = ecCompletionIdentifiers + 1;

  ecRecordMacro = ecCompletionTemplates + 1;
  ecPlayMacro = ecRecordMacro + 1;
  ecBeginRecord = ecRecordMacro + 2;
  ecEndRecord = ecRecordMacro + 3;

  ecSaveBlock = ecEndRecord + 1;
  ecInsertBlock = ecSaveBlock + 1;

  ecInsertMacro0 = ecInsertBlock + 1;
  ecInsertMacro1 = ecInsertMacro0 + 1;
  ecInsertMacro2 = ecInsertMacro0 + 2;
  ecInsertMacro3 = ecInsertMacro0 + 3;
  ecInsertMacro4 = ecInsertMacro0 + 4;
  ecInsertMacro5 = ecInsertMacro0 + 5;
  ecInsertMacro6 = ecInsertMacro0 + 6;
  ecInsertMacro7 = ecInsertMacro0 + 7;
  ecInsertMacro8 = ecInsertMacro0 + 8;
  ecInsertMacro9 = ecInsertMacro0 + 9;
  ecInsertMacroA = ecInsertMacro0 + 10;
  ecInsertMacroB = ecInsertMacro0 + 11;
  ecInsertMacroC = ecInsertMacro0 + 12;
  ecInsertMacroD = ecInsertMacro0 + 13;
  ecInsertMacroE = ecInsertMacro0 + 14;
  ecInsertMacroF = ecInsertMacro0 + 15;
  ecInsertMacroG = ecInsertMacro0 + 16;
  ecInsertMacroH = ecInsertMacro0 + 17;
  ecInsertMacroI = ecInsertMacro0 + 18;
  ecInsertMacroJ = ecInsertMacro0 + 19;
  ecInsertMacroK = ecInsertMacro0 + 20;
  ecInsertMacroL = ecInsertMacro0 + 21;
  ecInsertMacroM = ecInsertMacro0 + 22;
  ecInsertMacroN = ecInsertMacro0 + 23;
  ecInsertMacroO = ecInsertMacro0 + 24;
  ecInsertMacroP = ecInsertMacro0 + 25;
  ecInsertMacroQ = ecInsertMacro0 + 26;
  ecInsertMacroR = ecInsertMacro0 + 27;
  ecInsertMacroS = ecInsertMacro0 + 28;
  ecInsertMacroT = ecInsertMacro0 + 29;
  ecInsertMacroU = ecInsertMacro0 + 30;
  ecInsertMacroV = ecInsertMacro0 + 31;
  ecInsertMacroW = ecInsertMacro0 + 32;
  ecInsertMacroX = ecInsertMacro0 + 33;
  ecInsertMacroY = ecInsertMacro0 + 34;
  ecInsertMacroZ = ecInsertMacro0 + 35;

  ecBlockOpA = ecInsertMacroZ + 1;
  ecBlockOpB = ecBlockOpA + 1;
  ecBlockOpC = ecBlockOpA + 2;
  ecBlockOpD = ecBlockOpA + 3;
  ecBlockOpE = ecBlockOpA + 4;
  ecBlockOpF = ecBlockOpA + 5;
  ecBlockOpG = ecBlockOpA + 6;
  ecBlockOpH = ecBlockOpA + 7;
  ecBlockOpI = ecBlockOpA + 8;
  ecBlockOpJ = ecBlockOpA + 9;
  ecBlockOpK = ecBlockOpA + 10;
  ecBlockOpL = ecBlockOpA + 11;
  ecBlockOpM = ecBlockOpA + 12;
  ecBlockOpN = ecBlockOpA + 13;
  ecBlockOpO = ecBlockOpA + 14;
  ecBlockOpP = ecBlockOpA + 15;
  ecBlockOpQ = ecBlockOpA + 16;
  ecBlockOpR = ecBlockOpA + 17;
  ecBlockOpS = ecBlockOpA + 18;
  ecBlockOpT = ecBlockOpA + 19;
  ecBlockOpU = ecBlockOpA + 20;
  ecBlockOpV = ecBlockOpA + 21;
  ecBlockOpW = ecBlockOpA + 22;
  ecBlockOpX = ecBlockOpA + 23;
  ecBlockOpY = ecBlockOpA + 24;
  ecBlockOpZ = ecBlockOpA + 25;

  ecBackword = ecBlockOpZ + 1;
  ecScrollPageUp = ecBackword + 1;
  ecScrollPageDown = ecScrollPageUp + 1;

  twoKeyCommand = High(word);

const
  __Brackets = ['(',')','[',']','{','}'];
  __StdWordDelims = [#0..' ',',','.',';','\',':','''','`']{ + __Brackets};

  procedure Register;

implementation

uses
  Consts, MemoExClipUtils, Math;

const
  RAEditorCompletionChars = #8+'_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnmÉÖÓÊÅÍÃØÙÇÕÚÔÛÂÀÏÐÎËÄÆÝß×ÑÌÈÒÜÁÞ¨éöóêåíãøùçõúôûâàïðîëäæýÿ÷ñìèòüáþ¸';
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z', 'À'..'ß', 'à'..'ÿ'];
  _StIdSymbols = ['>','<','''', '"', '`','!','@','#','$','%','^','&','*','/','?'] + __Brackets + StIdSymbols + [#127..#255];
  _AutoChangePunctuation = [' ','`','~','!','@','#','$','%','^','&','*','(',')','_','-','+','=',';',':','''','"','[',']','{','}',',','.','/','?','<','>'];
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\',':','+','%','*','(',')',';','=','{','}','[',']', '{', '}'];

function Spaces(const N: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to N do Result := Result + ' ';
end;

function HasChar(const Ch: Char; const S: string) : boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function GetWordOnPos(const S: string; const P: integer) : string;
var
  i, Beg: integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    if S[i] in Separators then break;
  Beg := i + 1;
  for i := P to Length(S) do
    if S[i] in Separators then break;
  if i > Beg then Result := Copy(S, Beg, i-Beg)
  else Result := S[P];
end;

function GetWordOnPosEx(const S: string; const P: integer; var iBeg, iEnd: integer): string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  iBeg := P;
  if S[P] in Separators then
    if (P < 1) or ((P > 1) and (S[P-1] in Separators)) then inc(iBeg)
    else
      if ((P > 1) and not (S[P-1] in Separators)) then dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then break
    else dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then break
    else inc(iEnd);
  if iEnd > iBeg then Result := Copy(S, iBeg, iEnd - iBeg)
  else Result := S[P];
end;

function SubStr(const S: string; const Index: integer; const Separator: string): string;
var
  i: integer;
  pB, pE: PChar;
begin
  Result := '';
  if (index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator)) then exit;
  pB := PChar(S);
  for i := 1 to index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then SetString(Result, pB, pE-pB);
end;

function ReplaceWordByPhrase(S: string; const Word, Phrase: string): string;
var
  LW: integer;
  P: PChar;
  Sm: integer;
begin
  LW := Length(Word);
  P := StrPos(PChar(S), PChar(Word));
  while P <> nil do
  begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + Phrase + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(Phrase), PChar(Word));
  end;
  Result := S;
end;

function KeyPressed(VK : integer) : boolean;
begin
  Result := GetKeyState(VK) and $8000 = $8000;
end;

function _CutString(Len: integer; var S: string): string;
var
  T: string;
  j: integer;
begin
  Result := '';
  if Len >= length(S) then exit;
  T := System.Copy(S, 1, Len);
  j := length(T);
  while j > 1 do
    if T[j] = #32 then break
    else dec(j);
  if j = 1 then j := Len;
  Result := System.Copy(S, j + 1, length(S));
  S := System.Copy(S, 1, j);
end;

function _Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] = ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function _TrimLeft(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;

function _TrimRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

type
  TCaretUndo = class(TUndo)
  private
    FCaretX, FCaretY: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TInsertUndo = class(TCaretUndo)
  private
    FText: string;
    FOffset: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AText: string);
    procedure Undo; override;
  end;

  TReLineUndo = class(TInsertUndo);

  TInsertTabUndo = class(TInsertUndo);

  TOverwriteUndo = class(TCaretUndo)
  private
    FOldText, FNewText: string;
    FOffset: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AOldText, ANewText: string);
    procedure Undo; override;
  end;

  TDeleteUndo = class(TInsertUndo)
  public
    procedure Undo; override;
  end;

  TDeleteTrailUndo = class(TDeleteUndo);

  TBackspaceUndo = class(TDeleteUndo)
  public
    procedure Undo; override;
  end;

  TReplaceUndo = class(TCaretUndo)
  private
    FBeg, FEnd: integer;
    FText, FNewText: string;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const ABeg, AEnd: integer; const AText, ANewText: string);
    procedure Undo; override;
  end;

  TDeleteSelectedUndo = class(TDeleteUndo)
  private
    FSelBlock: boolean; { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY, FSelOffs: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const AText: string; const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX,
      ASelEndY, ASelOffs: integer);
    procedure Undo; override;
  end;

  TSelectUndo = class(TCaretUndo)
  private
    FSelBlock: boolean; { vertial block }
    FSelBegX, FSelBegY, FSelEndX, FSelEndY: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY: integer;
      const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX, ASelEndY: integer);
    procedure Undo; override;
  end;

  TUnselectUndo = class(TSelectUndo);

  TBeginCompoundUndo = class(TUndo)
  public
    procedure Undo; override;
  end;

  TEndCompoundUndo = class(TBeginCompoundUndo);

  TIndentUndo = class(TUndo)
  private
    FIndentY1, FIndentY2, FIndentSize: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const AIndentY1, AIndentY2, AIndentSize: integer);
    procedure Undo; override;
  end;

  TUnindentUndo = class(TUndo)
  private
    FIndentY, FIndentSize: integer;
  public
    constructor Create(const AMemoEx: TCustomMemoEx; const AIndentY, AIndentSize: integer);
    procedure Undo; override;
  end;

procedure Err;
begin
  MessageBeep(0);
end;

function FindNotBlankCharPos(const S: string): integer;
var
  i: integer;
begin
  Result := 1;
  for i := 1 to Length(S) do
    if S[i] <> ' ' then Exit;
end;

function ANSIChangeCase(const S: string): string;
var
  i: integer;
  Up: ANSIChar;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    Up := ANSIUpperCase(Result[i])[1];
    if Result[i] = Up then Result[i] := ANSILowerCase(Result[i])[1]
    else Result[i] := Up;
  end;
end;


{ TMemoExStrings }

procedure TMemoExStrings.LoadFromFile(const FileName: string);
begin
  BeginUpdate;
  try
    inherited LoadFromFile(FileName);
    if Assigned(FOnAfterLoad) then FOnAfterLoad(FMemoEx);
  finally
    EndUpdate;
  end;
end;

procedure TMemoExStrings.SaveToFile(const FileName: string);
begin
  if Assigned(FOnBeforeSave) then FOnBeforeSave(FMemoEx);
  inherited SaveToFile(FileName);
end;

{
  Release memory used for paragraphs storage.
}
procedure TMemoExStrings.FinalizeParagraphs;
begin
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  FParaLinesCount := 0;
end;

{
  Precounting for line quick search. 
}
procedure TMemoExStrings.Recount(Index: integer);
var
  i: integer;
begin
  for i := Index to FCount - 1 do
    if i = 0 then FList^[i].FPreCount := 0
    else FList^[i].FPreCount := FList^[i - 1].FPreCount + FList^[i - 1].FCount;
end;

{
  Release memory used for paragraph storage.
}
procedure TMemoExStrings.FinalizeParagraph(Index: integer);
begin
  Finalize(FList^[Index]);
end;

{
  Convert line index (Index) to paragraph coordinates (Paragraph:ParagraphWrappedLineIndex).
}
procedure TMemoExStrings.Index2ParaIndex(Index: integer; var Para, ParaIndex: integer);
var
  L, H, I: integer;
begin
  if (not FMemoEx.FWordWrap) or (FParaLinesCount = FCount) then
  begin
    Para := Index;
    if Para > FCount - 1 then Para := FCount - 1;
    ParaIndex := 0;
  end
  else
    begin
      {
        Paragraph quick search.
      }
      Para := -1;
      ParaIndex := -1;
      L := 0;
      H := FCount - 1;
      while L <= H do
      begin
        I := (L + H) shr 1;
        if Index > FList^[I].FPreCount + FList^[I].FCount - 1 then L := I + 1
        else
          begin
            H := I - 1;
            if (Index <= FList^[I].FPreCount + FList^[I].FCount - 1) and
               (Index >= FList^[I].FPreCount) then
            begin
              Para := I;
              ParaIndex := Index - FList^[I].FPreCount;
              break;
            end;
          end;
      end;
    end;
end;

{
  Returns paragraph by linear index.
}
function TMemoExStrings.GetParagraphByIndex(Index: integer; var ParaIndex, IndexOffs: integer): string;
var
  _P, _PI, i: integer;
begin
  IndexOffs := 0;
  ParaIndex := 0;
  Result := '';
  Index2ParaIndex(Index, _P, _PI);
  if (_P = -1) or (_PI = -1) then Error(SListIndexError, Index);
  ParaIndex := _P;
  for i := 0 to FList^[_P].FCount - 1 do
  begin
    Result := Result + FList^[_P].FStrings[i];
    if i < _PI then inc(IndexOffs, length(FList^[_P].FStrings[i]));
  end;
end;

{
  Convert caret coordinates (X:Y) to paragraph coordinates (ParagraphIndex:IndexOffset).
}
procedure TMemoExStrings.Caret2Paragraph(X, Y: integer; var ParaIndex, IndexOffs: integer);
var
  _P, _PI, i: integer;
begin
  ParaIndex := 0;
  IndexOffs := 0;
  Index2ParaIndex(Y, _P, _PI);
  if (_P = -1) or (_PI = -1) then Error(SListIndexError, Y);
  ParaIndex := _P;
  IndexOffs := X;
  for i := 0 to _PI - 1 do
    inc(IndexOffs, length(FList^[_P].FStrings[i]));
end;

{
  Convert paragraph coordinates (ParagraphIndex:IndexOffset) to caret coordinates (X:Y).
}
procedure TMemoExStrings.Paragraph2Caret(ParaIndex, IndexOffs: integer; var X, Y: integer);
var
  i, j, k: integer;
  b: boolean;
begin
  X := 0;
  Y := ParaIndex;
  b := false;
  k := 0;
  for i := 0 to FCount - 1 do
  begin
    if i >= Y then
    begin
      for j := 0 to FList^[i].FCount - 1 do
      begin
        inc(X, length(FList^[i].FStrings[j]));
        if X >= IndexOffs then
        begin
          b := true;
          Y := k + j;
          X := IndexOffs - (X - length(FList^[i].FStrings[j]));
          break;
        end;
      end;
      if b then break;
    end;
    inc(k, FList^[i].FCount);
  end;
  if not b then
  begin
    if X > 0 then
    begin
      Y := k;
      X := length(FList^[Y].FStrings[FList^[Y].FCount - 1]);
      exit;
    end;
    Y := FCount - 1;
    if Y >= 0 then X := length(FList^[Y].FStrings[FList^[Y].FCount - 1])
    else
      begin
        X := 0;
        Y := 0;
      end;
  end;
end;

{
  Returns paragraph offset from text start.
}
function TMemoExStrings.GetParaOffs(ParaIndex: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to ParaIndex - 1 do
    inc(Result, length(Strings[i]) + 2);
end;

{
  Paragraph formatting.
}
procedure TMemoExStrings.ReformatParagraph(ParaIndex: integer);
var
  s, t: string;
  i: integer;
begin
  with FList^[ParaIndex] do
  begin
    dec(FParaLinesCount, FCount);
    FChanged := true;
    Finalize(FAttrs);
    FPreCount := 0;
    if FCount > 1 then
    begin
      s := Get(ParaIndex);
      FCount := 1;
      FStrings := System.Copy(FStrings, 0, 1);
      FStrings[0] := s;
    end;
    if FMemoEx.FWordWrap then
    begin
      i := 0;
      while i <= FCount - 1 do
      begin
        if length(FStrings[i]) > FMemoEx.FRealRightMargin then
        begin
          // ñòðîêà äëèííåå, ÷åì íàäî
          s := FStrings[i];
          t := _CutString(FMemoEx.FRealRightMargin, s);
          FStrings[i] := s;
          inc(FCount);
          SetLength(FStrings, FCount);
          FStrings[FCount - 1] := t;
        end;
        inc(i);
      end;
    end;
    inc(FParaLinesCount, FCount);
  end;
end;

{
  Reformat all text.
}
procedure TMemoExStrings.Reformat;
var
  i: integer;
begin
  for i := 0 to FCount - 1 do ReformatParagraph(i);
  Recount(0);
  Changed;
end;

procedure TMemoExStrings.CheckLength(const st: string);
begin
  if length(st) + 1 > FMemoEx.Max_X then FMemoEx.SetMax_X(length(st) + 1);
end;

function TMemoExStrings.GetParaLineCount: integer;
begin
  Result := FParaLinesCount;
end;

function TMemoExStrings.GetCount: integer;
begin
  Result := FCount;
end;

constructor TMemoExStrings.Create;
begin
  inherited Create;
  FParaLinesCount := 0;
  FOnAfterLoad := nil;
  FOnBeforeSave := nil;
end;

destructor TMemoExStrings.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  FinalizeParagraphs;
  SetCapacity(0);
end;

function TMemoExStrings.InsertAtPos(Index: integer; const S: string): integer;
var
  t: string;
  i, j: integer;
begin
  i := 1;
  t := '';
  Result := Index;
  if S = '' then InsertItem(Result, '')
  else
    begin
      j := Result;
      while i <= length(S) do
      begin
        if (i = length(S)) or (S[i] in [#10, #13]) then
        begin
          if not (S[i] in [#10, #13]) then t := t + S[i];
          InsertItem(j, FMemoEx.ExpandTabs(t));
          inc(i);
          inc(j);
          while (i <= length(S)) and (S[i] in [#10, #13]) do inc(i);
          t := '';
          continue;
        end
        else t := t + S[i];
        inc(i);
      end;
    end;
end;

{
  Add paragraph.
}
function TMemoExStrings.Add(const S: string): integer;
begin
  Result := InsertAtPos(FCount, S);
end;

{
  Delete paragraph.
}
procedure TMemoExStrings.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Changing;
  dec(FParaLinesCount, TParagraph(FList^[Index]).FCount);
  Dec(FCount);
  FinalizeParagraph(Index);
  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TParagraph));
    Recount(Index);
  end;
  Changed;
end;

{
  Insert paragraph.
}
procedure TMemoExStrings.Insert(Index: integer; const S: string);
begin
  if (Index < 0) or (Index > FCount) then Error(SListIndexError, Index);
  InsertAtPos(Index, S);
end;

procedure TMemoExStrings.InsertItem(Index: integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TParagraph));
  with FList^[Index] do
  begin
    pointer(FStrings) := nil;
    pointer(FAttrs) := nil;
    FCount := 0;
    FPreCount := 0;
    FChanged := false;
  end;
  Inc(FCount);
  AddParaStr(Index, S);
  Changed;
end;

{
  Add line to paragraph.
}
{$WARNINGS OFF}
function TMemoExStrings.AddParaStr(ParaIndex: integer; const S: string): integer;
begin
  if (ParaIndex < 0) or (ParaIndex >= FCount) then Error(SListIndexError, ParaIndex);
  with FList^[ParaIndex] do
  begin
    inc(FCount);
    inc(FParaLinesCount);
    SetLength(FStrings, FCount);
    FStrings[FCount - 1] := FMemoEx.ExpandTabs(S);
    CheckLength(FStrings[FCount - 1]);
  end;
  ReformatParagraph(ParaIndex);
  Recount(ParaIndex);
  Changed;
end;
{$WARNINGS ON}

procedure TMemoExStrings.Changed;
begin
  if FMemoEx.FUpdateLock = 0 then FMemoEx.TextAllChanged;
end;

procedure TMemoExStrings.Changing;
begin
//  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TMemoExStrings.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    FinalizeParagraphs;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TMemoExStrings.BeginUpdate;
begin
  inc(FMemoEx.FUpdateLock);
end;

procedure TMemoExStrings.EndUpdate;
begin
  dec(FMemoEx.FUpdateLock);
  Changed;
end;

function TMemoExStrings._GetString(ParaIndex: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FList^[ParaIndex].FCount - 1 do
    Result := Result + FList^[ParaIndex].FStrings[i];
end;

function TMemoExStrings.Get(Index: integer): string;
begin
  if (Index < 0) or (Index >= FCount) then Result := ''
  else Result := _GetString(Index);
end;

function TMemoExStrings.GetParagraph(Index: integer): TParagraph;
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  Result := FList^[Index];
end;

function TMemoExStrings.GetParaString(Index: integer): string;
var
  _P, _PI: integer;
begin
  if (not FMemoEx.FWordWrap) or (FParaLinesCount = FCount) then Result := Get(Index)
  else
    begin
      Index2ParaIndex(Index, _P, _PI);
      if (_P = -1) or (_PI = -1) then Result := ''      //  Error(SListIndexError, Index);
      else Result := FList^[_P].FStrings[_PI];
    end;
end;

function TMemoExStrings.GetCapacity: integer;
begin
  Result := FCapacity;
end;

procedure TMemoExStrings.Grow;
var
  Delta: integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else
    if FCapacity > 8 then Delta := 16
    else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TMemoExStrings._PutString(ParaIndex: integer; const S: string);
var
  old_count, old_precount: integer;
begin
  old_count := FList^[ParaIndex].FCount;
  old_precount := FList^[ParaIndex].FPreCount;
  dec(FParaLinesCount, TParagraph(FList^[ParaIndex]).FCount);
  FinalizeParagraph(ParaIndex);
  with FList^[ParaIndex] do
  begin
    FCount := 1;
    SetLength(FStrings, FCount);
    FStrings[0] := S;
    inc(FParaLinesCount);
  end;
  ReformatParagraph(ParaIndex);
  if old_count <> FList^[ParaIndex].FCount then Recount(ParaIndex)
  else FList^[ParaIndex].FPreCount := old_precount;
end;

procedure TMemoExStrings.Put(Index: integer; const S: string);
begin
  if (Index < 0) or (Index >= FCount) then Error(SListIndexError, Index);
  CheckLength(S);
  Changing;
  _PutString(Index, S);
  Changed;
end;

procedure TMemoExStrings.PutParaString(Index: integer; const S: string);
var
  _P, _PI: integer;
  old_count, old_precount: integer;
begin
  if not FMemoEx.FWordWrap then Put(Index, S)
  else
    begin
      Index2ParaIndex(Index, _P, _PI);
      if (_P = -1) or (_PI = -1) then Error(SListIndexError, Index);
      Changing;

      old_count := FList^[_P].FCount;
      old_precount := FList^[_P].FPreCount;
      CheckLength(S);
      FList^[_P].FStrings[_PI] := S;
      ReformatParagraph(_P);
      if old_count <> FList^[_P].FCount then Recount(_P)
        else FList^[_P].FPreCount := old_precount;

      Changed;
    end;
end;

procedure TMemoExStrings.SetCapacity(NewCapacity: integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TParagraph));
  FCapacity := NewCapacity;
end;

procedure TMemoExStrings.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing
  else Changed;
end;

procedure TMemoExStrings.SetTextStr(const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  inherited SetTextStr(FMemoEx.ExpandTabs(Value));
  dec(FMemoEx.FUpdateLock);
  if FMemoEx.FUpdateLock = 0 then
  begin
    FMemoEx.ClearUndo;
    FMemoEx.TextAllChanged;
    FMemoEx.FCaretX := -1;
    FMemoEx.FCaretY := -1;
    FMemoEx.SetCaretInternal(0, 0);
  end;
end;

procedure TMemoExStrings.SetParaChanged(ParaIndex: integer);
begin
  FList^[ParaIndex].FChanged := false;
end;

procedure TMemoExStrings.ReLine;
var
  L: integer;
begin
  inc(FMemoEx.FUpdateLock);
  try
    if FParaLinesCount = 0 then L := FMemoEx.FCaretX
    else L := Length(ParaStrings[FParaLinesCount - 1]);
    while FMemoEx.FCaretY > FParaLinesCount - 1 do
    begin
//      if FParaLinesCount > 0 then TReLineUndo.Create(FMemoEx, L, FMemoEx.FCaretY, #13#10);
      if FParaLinesCount > 0 then TReLineUndo.Create(FMemoEx, L, FParaLinesCount - 1, #13#10);
      L := 0;
      Add('');
    end;
    if FMemoEx.FCaretX > Length(ParaStrings[FMemoEx.FCaretY]) then
    begin
      L := FMemoEx.FCaretX - Length(ParaStrings[FMemoEx.FCaretY]);
      TReLineUndo.Create(FMemoEx, Length(ParaStrings[FMemoEx.FCaretY]),
        FMemoEx.FCaretY, Spaces(L));
      PutParaString(FMemoEx.FCaretY, ParaStrings[FMemoEx.FCaretY] + Spaces(L));
    end;
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

procedure TMemoExStrings.SetLockText(const Text: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    inherited SetTextStr(FMemoEx.ExpandTabs(Text));
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

procedure TMemoExStrings.SetInternalParaStr(Index: integer; const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    PutParaString(Index, Value);
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

procedure TMemoExStrings.SetInternal(Index: integer; const Value: string);
begin
  inc(FMemoEx.FUpdateLock);
  try
    Put(Index, Value);
  finally
    dec(FMemoEx.FUpdateLock);
  end;
end;

{ TEditorClient }

function TEditorClient.GetCanvas: TCanvas;
begin
  Result := FMemoEx.Canvas;
end;

function TEditorClient.Left: integer;
begin
  Result := FMemoEx.GutterWidth + 2;
end;

function TEditorClient.Height: integer;
begin
  Result := FMemoEx.ClientHeight;
end;

function TEditorClient.Width: integer;
begin
  Result := Max(FMemoEx.ClientWidth - Left, 0);
end;

function TEditorClient.ClientWidth: integer;
begin
  Result := Width;
end;

function TEditorClient.ClientHeight: integer;
begin
  Result := Height;
end;

function TEditorClient.ClientRect: TRect;
begin
  Result := Bounds(Left, Top, Width, Height);
end;

function TEditorClient.BoundsRect: TRect;
begin
  Result := Bounds(0, 0, Width, Height);
end;


{ TGutter }

constructor TGutter.Create;
begin
  FFont := TFont.Create;
  with FFont do
  begin
    Name := 'MS Sans Serif';
    Size := 8;
    Color := clWindowText;
  end;
  FDrawBitmap := TBitmap.Create;
end;

destructor TGutter.Destroy;
begin
  FFont.Free;
  FDrawBitmap.Free;
  inherited;
end;

procedure TGutter.Invalidate;
begin
  Paint;
end;

procedure TGutter.Paint;
begin
  if FMemoEx.FGutterWidth > 0 then
  begin
    with FDrawBitmap, Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FMemoEx.FGutterColor;
      FillRect(Bounds(0, 0, Width, Height));
      Pen.Width := 1;
      Pen.Color := FMemoEx.Color;
      MoveTo(Width - 2, 0);
      LineTo(Width - 2, Height);
      Pen.Width := 2;
      MoveTo(Width + 1, 0);
      LineTo(Width + 1, Height);
      Pen.Width := 1;
      Pen.Color := clGray;
      MoveTo(Width - 1, 0);
      LineTo(Width - 1, Height);
    end;
    FMemoEx.GutterPaint(FDrawBitmap.Canvas);
  end
  else
    with FDrawBitmap, Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FMemoEx.Color;
      FillRect(Bounds(0, 0, Width, Height));
    end;
  FMemoEx.Canvas.Draw(0, 0, FDrawBitmap);
end;


{*********************** TCustomMemoEx ***********************}

constructor TCustomMemoEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents {, csOpaque}, csDoubleClicks, csReplicatable];
  FInsertMode := true;
  FReadOnly := false;
  FWantTabs := true;
  FLines := TMemoExStrings.Create;
  FLines.FMemoEx := Self;
  FKeyboard := TKeyboard.Create;
  FRows := 1;
  FCols := 1;

  FUndoBuffer := TUndoBuffer.Create;
  FUndoBuffer.FMemoEx := Self;
  FGroupUndo := true;

  FDrawBitmap := TBitmap.Create;

  FFont := TFont.Create;
  with FFont do
  begin
    Name := 'Courier New';
    Size := 10;
    Pitch := fpFixed;
    OnChange := FontChanged;
  end;

  FTabPos := nil;
  FCharWidth := nil;
  SetMax_X(5);

  FRightMarginVisible := true;
  FRightMargin := 80;
  FRealRightMargin := FRightMargin;
  FBorderStyle := bsSingle;
  Ctl3d := true;
  Height := 150;
  Width := 250;
  ParentColor := false;
  Cursor := crIBeam;
  TabStop := true;
  FTabSize := 4;
  FIndentSize := 4;
  FAutoIndentSize := 0;
  FSmartTab := true;
  FSmartIndent := true;
  FBackspaceUnindents := true;
  FAutoIndent := true;
  FKeepTrailingBlanks := false;
  FCursorBeyondEOF := false;
  FCursorBeyondEOL := true;

  FWordWrap := true;

  FScrollBars := ssBoth;
  scbHorz := TMemoExScrollBar.Create;
  scbVert := TMemoExScrollBar.Create;
  scbVert.Kind := sbVertical;
  scbHorz.OnScroll := ScrollBarScroll;
  scbVert.OnScroll := ScrollBarScroll;

  Color := clWindow;
  FGutterColor := clBtnFace;
  FclSelectBC := clHighLight;
  FclSelectFC := clHighLightText;
  FRightMarginColor := clSilver;

  EditorClient := TEditorClient.Create;
  EditorClient.FMemoEx := Self;
  FGutter := TGutter.Create;
  FGutter.FMemoEx := Self;

  FLeftCol := 0;
  FTopRow := 0;
  FSelected := false;
  FCaretX := 0;
  FCaretY := 0;

  timerScroll := TTimer.Create(Self);
  timerScroll.Enabled := false;
  timerScroll.Interval := 100;
  timerScroll.OnTimer := ScrollTimer;

  SelAttrs_Size := 0;

  mouse_down := false;
  double_clicked := false;
  mouse_dragged := false;
  gutter_clicked := false;

  FSimpleBeginLine := true;
  FUseMaxCharWidth := true;

  FKeyboard.SetDefLayout;
  FCompletion := TCompletion.Create2(Self);
end;

destructor TCustomMemoEx.Destroy;
begin
  FLines.Free;
  scbHorz.Free;
  scbVert.Free;
  EditorClient.Free;
  FKeyboard.Free;
  FUndoBuffer.Free;
  FCompletion.Free;
  FGutter.Free;
  FDrawBitmap.Free;
  FFont.Free;
  SelAttrs := nil;
  FTabPos := nil;
  FCharWidth := nil;
  if FCurrentTheme > 0 then FreeThemeHandle(FCurrentTheme);
  inherited Destroy;
end;

procedure TCustomMemoEx.Invalidate;
begin
  if (csLoading in ComponentState) then exit;
  if FUpdateLock = 0 then inherited;
end;

procedure TCustomMemoEx.Loaded;
begin
  inherited Loaded;
  scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
  scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
  NextClipViewer := SetClipboardViewer(Handle);
  UpdateEditorSize;
  Changed;
  SelectionChanged;
  ClipboardChanged;
  FModified := false;
  FCompletion.UpdateAutoChange;
end;

procedure TCustomMemoEx.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array [TBorderStyle] of cardinal = (0, WS_BORDER);
  ScrollStyles: array [TScrollStyle] of cardinal = (0, WS_HSCROLL, WS_VSCROLL,
                                                      WS_HSCROLL or WS_VSCROLL);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle] or ScrollStyles[FScrollBars];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    WindowClass.Style := WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TCustomMemoEx.Resize;
begin
  if not (csLoading in ComponentState) then
  begin
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TCustomMemoEx.CreateWnd;
begin
  inherited CreateWnd;
  if FScrollBars in [ssHorizontal, ssBoth] then scbHorz.Handle := Handle;
  if FScrollBars in [ssVertical, ssBoth] then scbVert.Handle := Handle;
  FAllRepaint := true;
  if (ThemesAvailable) and (IsThemeActive) then FCurrentTheme := CreateThemeHandle(Handle);
end;

procedure TCustomMemoEx.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomMemoEx.PaintSelection;
var
  iR: integer;
begin
  for iR := FUpdateSelBegY to FUpdateSelEndY do PaintLine(iR, -1, -1);
end;

procedure TCustomMemoEx.SetUnSelected;
begin
  if FSelected then
  begin
    FSelectedText := false;
    TUnselectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
      FSelEndX, FSelEndY);
    PaintSelection;
  end;
end;

function IsRectEmpty(R: TRect): boolean;
begin
  Result := (R.Top = R.Bottom) and (R.Left = R.Right);
end;

function TCustomMemoEx.CalcCellRect(const X, Y: integer): TRect;
begin
  Result := Bounds(
    EditorClient.Left + X * FCellRect.Width + 1,
    EditorClient.Top + Y * FCellRect.Height,
    FCellRect.Width,
    FCellRect.Height)
end;

procedure TCustomMemoEx.Paint;
var
  iR: integer;
  ECR: TRect;
  BX, EX, BY, EY: integer;
begin
  if FUpdateLock > 0 then exit;

  PaintCaret(false);

  ECR := EditorClient.Canvas.ClipRect;
  OffsetRect(ECR, -FGutterWidth, 0);
  if FAllRepaint then ECR := EditorClient.BoundsRect;
  BX := ECR.Left div FCellRect.Width - 1;
  EX := ECR.Right div FCellRect.Width + 1;
  BY := ECR.Top div FCellRect.Height;
  EY := ECR.Bottom div FCellRect.Height + 1;
  for iR := BY to EY do
    PaintLine(FTopRow + iR, FLeftCol + BX, FLeftCol + EX);

  PaintCaret(true);
  FGutter.Paint;
  FAllRepaint := false;
end;

procedure TCustomMemoEx.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TCustomMemoEx.EndUpdate;
begin
  if FUpdateLock = 0 then Exit; { Error ? }
  dec(FUpdateLock);
  if FUpdateLock = 0 then
  begin
    FAllRepaint := true;
    UpdateEditorSize(false);
    StatusChanged;
    Invalidate;
  end;
end;

{
  Adjust FTabPos and FCharWidth depending on maximum line length.
}
procedure TCustomMemoEx.SetMax_X(const Value: integer);
var i: integer;
begin
  Max_X := Value;
  SetLength(FTabPos, Max_X);
  SetLength(FCharWidth, Max_X);
  for i := 0 to Max_X - 1 do FCharWidth[i] := FCellRect.Width;
end;

procedure TCustomMemoEx.SetUseMaxCharWidth(const Value: boolean);
begin
  if FUseMaxCharWidth <> Value then
  begin
    FUseMaxCharWidth := Value;
    UpdateEditorSize;
    Invalidate;
  end;
end;

{
  Refresh all parameters.
  FullUpdate is true when recounting of cell and draw bitmap size needed.
}
procedure TCustomMemoEx.UpdateEditorSize(const FullUpdate: boolean = true; const RepaintGutter: boolean = true);
const
  BiggestSymbol = 'W';
var
  i: integer;
begin
  if (csLoading in ComponentState) then exit;
  if FullUpdate then
  begin
    EditorClient.Canvas.Font := Font;
    if FUseMaxCharWidth then EditorClient.Canvas.Font.Style := [fsBold, fsItalic]
    else EditorClient.Canvas.Font.Style := [];
    FCellRect.Width := Max(1, EditorClient.Canvas.TextWidth(BiggestSymbol));
    FCellRect.Height := Max(1, EditorClient.Canvas.TextHeight(BiggestSymbol));
    for i := 0 to Max_X - 1 do FCharWidth[i] := FCellRect.Width;
    EditorClient.Canvas.Font := Font;
    FDrawBitmap.Canvas.Font.Assign(EditorClient.Canvas.Font);
    FDrawBitmap.Canvas.Brush.Assign(EditorClient.Canvas.Brush);
    FDrawBitmap.Width := Width;
    FDrawBitmap.Height := FCellRect.Height;
    FGutter.FDrawBitmap.Width := FGutterWidth + 2;
    FGutter.FDrawBitmap.Height := Height;
  end;
  FVisibleColCount := Trunc(EditorClient.ClientWidth / FCellRect.Width);
  FVisibleRowCount := Trunc(EditorClient.ClientHeight / FCellRect.Height);
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FCols := -1;
  FRows := -1;
  Rows := FLines.ParaLineCount;
  if FWordWrap then Cols := FRealRightMargin
  else Cols := Max_X;
  if RepaintGutter then FGutter.Invalidate;
end;

procedure TCustomMemoEx.PaintLine(const Line: integer; ColBeg, ColEnd: integer);
var
  Ch: string;
  R: TRect;
  F, k, x, i, j, iC, jC, SL, MX, PX, PY: integer;
  T, S: string;
  LA, LB: TLineAttr;
  FL: PParagraphList;
begin
  if (Line < FTopRow) or (Line > FTopRow + FVisibleRowCount) or (FUpdateLock > 0) then exit;
  if ColBeg < FLeftCol then ColBeg := FLeftCol;
  if (ColEnd < 0) or (ColEnd > FLeftCol + FVisibleColCount + 1) then ColEnd := FLeftCol + FVisibleColCount + 1;
  ColEnd := Min(ColEnd, Max_X - 1);
  j := 0;
  i := ColBeg;

  FDrawBitmap.Canvas.Brush.Color := Color;
  FDrawBitmap.Canvas.FillRect(Bounds(EditorClient.Left, 0, EditorClient.Width, FCellRect.Height));

  if (Line > -1) and (Line < FLines.ParaLineCount) then
    with FDrawBitmap do
    begin
      T := FLines.GetParagraphByIndex(Line, PY, PX);
      S := FLines.ParaStrings[Line];
      if not FWordWrap then
      begin
        iC := ColBeg;
        jC := ColEnd;
      end
      else
        begin
          iC := 0;
          jC := length(T);
        end;
      GetLineAttr(PY, Line, PX, length(S), iC, jC, T);

      SL := Length(S);
      if SL > ColEnd then MX := ColEnd
      else MX := SL;

      if (FStripInvisible) and (FReadOnly) and (ColBeg > 0) then
      begin
        FL := FLines.FList;
        x := PX + ColBeg;
        if x >= SelAttrs_Size then x := SelAttrs_Size - 1;
        for k := PX to x do
          if FL^[PY].FAttrs[k].FC = FL^[PY].FAttrs[k].BC then inc(j);
      end;


      while i < MX do
        with Canvas do
        begin
          iC := i + 1;
          jC := iC + 1;
          if iC <= SL then Ch := S[iC]
          else Ch := ' ';
          if (iC + PX > SelAttrs_Size) or (jC + PX > SelAttrs_Size) then break;
          LA := FLines.FList^[PY].FAttrs[iC + PX - 1];
          if SelAttrs[iC + PX - 1] then
            with LA do
            begin
              FC := FclSelectFC;
              BC := FclSelectBC;
            end;
          while (jC <= MX) and (jC + PX <= SelAttrs_Size) do
          begin
            LB := FLines.FList^[PY].FAttrs[jC + PX - 1];
            if SelAttrs[jC + PX - 1] then
              with LB do
              begin
                FC := FclSelectFC;
                BC := FclSelectBC;
              end;
            if CompareMem(@LA, @LB, sizeof(TLineAttr)) then
            begin
              if jC <= SL then Ch := Ch + S[jC]
              else Ch := Ch + ' ';
              inc(jC);
            end
            else break;
          end;
          if (not ((LA.BC = LA.FC) and (FStripInvisible))) or (not ReadOnly) then
          begin
            Brush.Color := LA.BC;
              Font.Color := LA.FC;
            Font.Style := LA.Style;
            R := CalcCellRect(i - FLeftCol - j, Line - FTopRow);
            if LA.BC <> Color then
              FillRect(Bounds(R.Left, 0, FCellRect.Width * Length(Ch), FCellRect.Height));
              Windows.ExtTextOut(Handle, R.Left, 0, 0, nil, PChar(Ch), length(Ch), @FCharWidth[0]);
          end
          else inc(j, length(Ch));
          i := jC - 1;
        end;
    end;

  R := Bounds(EditorClient.Left, (Line - FTopRow) * FCellRect.Height, (FVisibleColCount + 2) * FCellRect.Width, FCellRect.Height);

  if FRightMarginVisible and (FRealRightMargin > FLeftCol) and (FRealRightMargin < FLastVisibleCol + 3) then
    with FDrawBitmap.Canvas do
    begin
      Pen.Color := FRightMarginColor;
      F := CalcCellRect(FRealRightMargin - FLeftCol, 0).Left;
      MoveTo(F, 0);
      LineTo(F, FCellRect.Height);
    end;

  BitBlt(EditorClient.Canvas.Handle, R.Left, R.Top, R.Right - R.Left, FCellRect.Height,
          FDrawBitmap.Canvas.Handle, R.Left, 0,
          SRCCOPY);
end;

{
  Get line attributes.
  Line contains paragraph index.
  LineIdx contains line index in ParaStrings.
  LineOffs contains line offset from the paragraph beginning.
  LineLen length of line.
}
procedure TCustomMemoEx.GetLineAttr(Line, LineIdx, LineOffs, LineLen, ColBeg, ColEnd: integer; const ALine: string);

  procedure ChangeSelectedAttr;

    procedure DoChange(const iBeg, iEnd: integer);
    var
      i: integer;
    begin
      if (iBeg + LineOffs < SelAttrs_Size) and (iEnd + LineOffs < SelAttrs_Size) then
        for i := iBeg + LineOffs to iEnd + LineOffs do SelAttrs[i] := true;
    end;

  begin
    if SelAttrs_Size > 0 then ZeroMemory(@SelAttrs[0], SelAttrs_Size);
    if not FSelected then exit;
    if (LineIdx = FSelBegY) and (LineIdx = FSelEndY) then DoChange(FSelBegX, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive)))
    else
      begin
        if LineIdx = FSelBegY then DoChange(FSelBegX, LineLen - 1);
        if (LineIdx > FSelBegY) and (LineIdx < FSelEndY) then DoChange(0, LineLen - 1);
        if LineIdx = FSelEndY then DoChange(0, Min(LineLen - 1, FSelEndX - 1 + integer(FInclusive)));
      end
  end;

var
  i: integer;
begin
  if SelAttrs_Size <> length(ALine) + 1 then
  begin
    SelAttrs_Size := length(ALine) + 1;
    SetLength(SelAttrs, SelAttrs_Size);
  end;
  ChangeSelectedAttr;

  if FLines.FList^[Line].FChanged then
  begin
    with FLines.FList^[Line] do
    begin
      SetLength(FAttrs, SelAttrs_Size);
      FAttrs[0].FC := Font.Color;
      FAttrs[0].Style := Font.Style;
      FAttrs[0].BC := Color;
      FAttrs[0].ex_style := ME_EX_STYLE_DEFAULT;
      for i := 1 to SelAttrs_Size - 1 do
              Move(FAttrs[0], FAttrs[i], sizeof(TLineAttr));
    end;
    GetAttr(Line, ColBeg, ColEnd);
    if ALine <> '' then
      if Assigned(FOnGetLineAttr) then FOnGetLineAttr(Self, ALine, Line, SelAttrs, FLines.FList^[Line].FAttrs);
    ChangeAttr(Line, ColBeg, ColEnd);
    FLines.FList^[Line].FChanged := false;
  end;
end;

procedure TCustomMemoEx.GetAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TCustomMemoEx.ChangeAttr(Line, ColBeg, ColEnd: integer);
begin
end;

procedure TCustomMemoEx.ScrollBarScroll(Sender: TObject; ScrollCode:
  TScrollCode; var ScrollPos: integer);
begin
  case ScrollCode of
    scLineUp..scPageDown, scTrack:
      begin
        if Sender = scbVert then Scroll(true, ScrollPos)
        else
          if Sender = scbHorz then Scroll(false, ScrollPos);
      end;
  end;
end;

procedure TCustomMemoEx.Scroll(const Vert: boolean; const ScrollPos: integer);
var
  R, RClip, RUpdate: TRect;
  OldFTopRow: integer;
begin
  if FUpdateLock = 0 then
  begin
    PaintCaret(false);
    if Vert then
    begin {Vertical Scroll}
      OldFTopRow := FTopRow;
      FTopRow := ScrollPos;
      if Abs((OldFTopRow - ScrollPos) * FCellRect.Height) < EditorClient.Height then
      begin
        R := EditorClient.ClientRect;
        R.Bottom := R.Top + CellRect.Height * (FVisibleRowCount + 1);
        RClip := R;
        ScrollDC(
          EditorClient.Canvas.Handle,                   // handle of device context
          0,                                            // horizontal scroll units
          (OldFTopRow - ScrollPos) * FCellRect.Height,  // vertical scroll units
          R,                                            // address of structure for scrolling rectangle
          RClip,                                        // address of structure for clipping rectangle
          0,                                            // handle of scrolling region
          @RUpdate                                      // address of structure for update rectangle
          );
        InvalidateRect(Handle, @RUpdate, false);
      end
      else Invalidate;
      Update;
    end
    else {Horizontal Scroll}
      begin
        FLeftCol := ScrollPos;
        Invalidate;
      end;
  end
  else { FUpdateLock > 0 }
    begin
      if Vert then FTopRow := ScrollPos
      else FLeftCol := ScrollPos;
    end;
  FLastVisibleRow := FTopRow + FVisibleRowCount - 1;
  FLastVisibleCol := FLeftCol + FVisibleColCount - 1;
  if FUpdateLock = 0 then
  begin
    FGutter.Invalidate;
    PaintCaret(true);
  end;
  if Assigned(FOnScroll) then FOnScroll(Self);
end;

procedure TCustomMemoEx.PaintCaret(const bShow: boolean);
var
  R: TRect;
begin
  if not bShow then HideCaret(Handle)
  else
    if Focused then
    begin
      R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow);
      Windows.SetCaretPos(R.Left - 1, R.Top + 1);
      ShowCaret(Handle)
    end;
end;

procedure TCustomMemoEx.SetCaretInternal(X, Y: integer);
var
  R: TRect;
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  if not FCursorBeyondEOF then
    Y := Min(Y, FLines.ParaLineCount - 1);
  Y := Max(Y, 0);
  X := Min(X, Max_X);
  X := Max(X, 0);
  if Y < FTopRow then SetLeftTop(FLeftCol, Y)
  else
    if Y > Max(FLastVisibleRow, 0) then SetLeftTop(FLeftCol, Y - FVisibleRowCount + 1);
  if X < 0 then X := 0;
  if X < FLeftCol then SetLeftTop(X, FTopRow)
  else
    if X > FLastVisibleCol then SetLeftTop(X - FVisibleColCount {+ 1}, FTopRow);

  R := CalcCellRect(X - FLeftCol, Y - FTopRow);
  Windows.SetCaretPos(R.Left - 1, R.Top + 1);
  if (FCaretX <> X) or (FCaretY <> Y) then
  begin
    FCaretX := X;
    FCaretY := Y;
    StatusChanged;
  end;
  FCaretX := X;
  FCaretY := Y;
end;

procedure TCustomMemoEx.SetCaret(const X, Y: integer);
begin
  if (X = FCaretX) and (Y = FCaretY) then exit;
  TCaretUndo.Create(Self, FCaretX, FCaretY);
  SetCaretInternal(X, Y);
  if FUpdateLock = 0 then StatusChanged;
end;

procedure TCustomMemoEx.SetCaretPosition(const index, Pos: integer);
begin
  if index = 0 then SetCaret(Pos, FCaretY)
  else SetCaret(FCaretX, Pos)
end;

procedure TCustomMemoEx.KeyDown(var Key: Word; Shift: TShiftState);
var
  Form: TCustomForm;
  Com: word;
begin
  if FCompletion.FVisible then
    if FCompletion.DoKeyDown(Key, Shift) then exit
    else
  else FCompletion.FTimer.Enabled := false;

  if (Key = VK_TAB) and ((Shift = []) or (Shift = [ssShift])) then
    if ((FReadOnly) or (not FWantTabs)) then
    begin
      Form := GetParentForm(Self);
      if Assigned(Form) then
      begin
        Key := 0;
        if Shift = [] then Form.Perform(WM_NEXTDLGCTL, 0, 0)
        else Form.Perform(WM_NEXTDLGCTL, 1, 0);
      end;
      exit;
    end;

  if WaitSecondKey then
  begin
    Com := FKeyboard.Command2(Key1, Shift1, Key, Shift);
    WaitSecondKey := false;
    IgnoreKeyPress := true;
  end
  else
    begin
      inherited KeyDown(Key, Shift);
      Key1 := Key;
      Shift1 := Shift;
      Com := FKeyboard.Command(Key, Shift);
      if Com = twoKeyCommand then
      begin
        IgnoreKeyPress := true;
        WaitSecondKey := true;
      end
      else IgnoreKeyPress := Com > 0;
    end;
  if (Com > 0) and (Com <> twoKeyCommand) then
  begin
    Key := 0;
    Shift := [];
    Command(Com);
  end;
  if (Com = ecBackSpace) then FCompletion.DoKeyPress(#8);
end;

{
  We have to be sure the current line exists and position in it exists too.
}
procedure TCustomMemoEx.ReLine;
begin
  FLines.ReLine;
  UpdateEditorSize;
  scbVert.Position := FTopRow;
end; { ReLine }

procedure TCustomMemoEx.KeyPress(var Key: Char);
begin
  if IgnoreKeyPress then
  begin
    IgnoreKeyPress := false;
    exit
  end;
  if FReadOnly then exit;
  PaintCaret(false);
  inherited KeyPress(Key);

  Command(ord(Key));

  PaintCaret(true);
end;

function AutoChangeCompare(Item1, Item2: pointer): integer;
var
  i, j: integer;
begin
  i := length(PAutoChangeWord(Item1)^.OldWord);
  j := length(PAutoChangeWord(Item2)^.OldWord);
  if i = j then Result := 0
  else
    if i > j then Result := 1
    else Result := -1;
end;

procedure TCustomMemoEx.InsertChar(const Key: Char);

  function GetAutoChangeWord(const CurrentWord: string; var NewWord: string): boolean;
  var
    i, j, k: integer;
    s, t: string;
  begin
    Result := false;
    t := DoChangeCase(CurrentWord, ME_CASE_CONVERT_LOWER);
    j := length(t);
    for i := 0 to FCompletion.FAutoChangeList.Count - 1 do
    begin
      s := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.OldWord;
      k := length(s);
      if j < k then break
      else
        if j = k then
          if t = s then
          begin
            Result := true;
            NewWord := PAutoChangeWord(FCompletion.FAutoChangeList[i])^.NewWord;
            break;
          end;
    end;
  end;

var
  S: string;
  T, old_str, new_str: string;
  k1, k2, str_pos: integer;
  AutoChanged, AddKeyToNewStr: boolean;
  oldChar: string;
  i, _X, _Y, Y: integer;
  b: boolean;                       //  flag showing we should draw a lot
begin
  ReLine;
  case Key of
    #32..#255:
      begin
        if not HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
        begin
          ClearSelection;
          FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
          new_str := '';
          old_str := '';
          str_pos := 0;
          if (Key in _AutoChangePunctuation) and (FCompletion.FAutoChangeList.Count > 0) then
          begin
            S := FLines[FParaY];
            AutoChanged := false;
            AddKeyToNewStr := false;
            str_pos := FParaX - 1;
            //  k1 -- length of the smallest substring for auto-change
            //  k2 -- length of the longest substring for auto-change
            k1 := length(PAutoChangeWord(FCompletion.FAutoChangeList[0])^.OldWord);
            k2 := length(PAutoChangeWord(FCompletion.FAutoChangeList[FCompletion.FAutoChangeList.Count - 1])^.OldWord);
            //  cycling while we are not at the paragraph beginning and
            //  haven't got substring longer than k2
            while (str_pos > -1) and (FParaX - str_pos <= k2) do
            begin
              if FParaX - str_pos >= k1 then
              begin
                old_str := System.Copy(S, str_pos + 1, FParaX - str_pos);
                AutoChanged := GetAutoChangeWord(old_str, new_str); //  should we change substring w/o punctuation sign?
                if not AutoChanged then
                  AutoChanged := GetAutoChangeWord(old_str + Key, new_str) //  again: should we change substring w/o punctuation sign?
                else AddKeyToNewStr := true;
                if AutoChanged then
                  //  substring can be changed if it is at the beginning of the line
                  //  or punctuation sign before it.
                  if ((str_pos > 0) and (S[str_pos] in _AutoChangePunctuation)) or (str_pos = 0) then break
                  else AutoChanged := false;
              end;
              dec(str_pos);
            end;
            if AutoChanged then
              if AddKeyToNewStr then
                //  if we found a replacement, should the punctuation sign
                //  be replace too?
                if GetAutoChangeWord(Key, T) then new_str := new_str + T
                else new_str := new_str + Key
              else
            else
              begin
                //  should we change entered sign?
                AutoChanged := GetAutoChangeWord(Key, new_str);
                if AutoChanged then
                begin
                  str_pos := FParaX;  //  yes.
                  old_str := '';
                end;
              end;
          end
          else AutoChanged := false;
          if AutoChanged then
          begin
            {
              str_pos -- replacement substring start coordinate
              S       -- paragraph
              old_str -- substring to replace
              new_str -- replacement substring
            }

            //  undo for all replacement
            BeginCompound;
            TCaretUndo.Create(Self, FCaretX, FCaretY);
            FLines.Paragraph2Caret(FParaY, str_pos, _X, _Y);
            if (length(old_str) + integer(not FInsertMode) > 0) and (length(S) > 0) then
            begin
              if FInsertMode then T := old_str
              else T := old_str + S[FParaX + 1];
              TDeleteUndo.Create(Self, _X, _Y, T);
            end;
            if length(new_str) > 0 then TInsertUndo.Create(Self, _X, _Y, new_str);
            EndCompound;

            //  replacement
            k1 := FLines.Paragraphs[FParaY].FCount;
            System.Delete(S, str_pos + 1, length(old_str) + integer(not FInsertMode));
            System.Insert(new_str, S, str_pos + 1);
            FLines.Internal[FParaY] := S;
            B := k1 <> FLines.Paragraphs[FParaY].FCount;
            FParaX := str_pos + length(new_str);
          end
          else
            begin
              //  simple symbol inserting
              S := FLines.ParaStrings[FCaretY];
              if FInsertMode then
              begin
                TInsertUndo.Create(Self, FCaretX, FCaretY, Key);
                Insert(Key, S, FCaretX + 1);
              end
              else
                begin
                  if FCaretX + 1 <= Length(S) then
                  begin
                    oldChar := S[FCaretX + 1];
                    S[FCaretX + 1] := Key;
                  end
                  else
                    begin
                      oldChar := '';
                      S := S + Key;
                    end;
                  TOverwriteUndo.Create(Self, FCaretX, FCaretY, oldChar, Key);
                end;

              Y := FCaretY;
              i := FLines.Paragraphs[FParaY].FCount;
              FLines.InternalParaStrings[Y] := S;
              inc(FParaX);

              B := i <> FLines.Paragraphs[FParaY].FCount;
            end;

          //  repanting of the paragraph.
          i := RepaintParagraph(FCaretY);

          //  full paragraph repainting needed only if number
          //  of wrapped lines in paragraph was changed.
          if B then
          begin
            UpdateEditorSize(false);
            RedrawFrom(i + 1);
          end;

          FLines.Paragraph2Caret(FParaY, FParaX, _X, _Y);
          SetCaretInternal(_X, _Y);

          Changed;
        end;
        if HasChar(Key, RAEditorCompletionChars) then
          FCompletion.DoKeyPress(Key);
      end;
  end;
end;

procedure TCustomMemoEx.RedrawFrom(YFrom: integer);
var
  i: integer;
begin
  for i := YFrom - 1 to FLastVisibleRow + 1 do PaintLine(i, -1, -1);
end;

function TCustomMemoEx.RepaintParagraph(LineIndex: integer): integer;
var
  P, PI, i, j, k: integer;
begin
  FLines.Index2ParaIndex(LineIndex, P, PI);
  j := LineIndex - PI;
  k := j + FLines.Paragraphs[P].FCount - 1;
  if j < FTopRow - 1 then j := FTopRow - 1;
  j := Max(0, j);
  if k > FLastVisibleRow + 1 then k := FLastVisibleRow + 1;
  Result := k;
  for i := j to k do PaintLine(i, -1, -1);
end;

function TCustomMemoEx.IsUndoEmpty: boolean;
begin
  Result := FUndoBuffer.FPtr < 0;
end;

function TCustomMemoEx.YinBounds(AY: integer): boolean;
begin
  Result := (AY > -1) and (AY < FLines.ParaLineCount);
end;

function TCustomMemoEx.DoChangeCase(const st: string;
  Conversion: byte): string;
begin
  if Assigned(FOnCaseConversion) then Result := FOnCaseConversion(Self, Conversion, st)
  else
    case Conversion of
      ME_CASE_CONVERT_UPPER:
        Result := ANSIUpperCase(st);
      ME_CASE_CONVERT_LOWER:
        Result := ANSILowerCase(st);
      else
        Result := ANSIChangeCase(st);
    end;
end;

type
  EComplete = class(EAbort);

procedure TCustomMemoEx.Command(ACommand: TEditCommand);
var
  X, Y: integer;
  CaretUndo: boolean;

type
  TPr = procedure of object;

  procedure DoAndCorrectXY(Pr: TPr);
  begin
    Pr;
    X := FCaretX;
    Y := FCaretY;
    CaretUndo := false;
  end;

  function Com(const Args: array of TEditCommand): boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to High(Args) do
      if Args[i] = ACommand then exit;
    Result := false;
  end;

  procedure SetSel1(X, Y: integer);
  begin
    SetSel(X, Y);
    CaretUndo := false;
  end;

  procedure SetSelText1(S: string);
  begin
    SelText := S;
    CaretUndo := false;
  end;

  procedure Complete;
  begin
    raise EComplete.Create('');
  end;

  function BeginLineCoordinate: integer;
  var
    z: string;
    i: integer;
  begin
    if (not FSimpleBeginLine) and (FLines.ParaLineCount > 0) then
    begin
      z := FLines.ParaStrings[Y];
      i := length(z) - length(_TrimLeft(z));
      if X = i then Result := 0
      else Result := i;
    end
    else Result := 0;
  end;

var
  F, _Y, indentX, indentY1, indentY2: integer;
  S, S2, T: string;
  B, isIndent: boolean;
  iBeg, iEnd, i: integer;
begin
  X := FCaretX;
  Y := FCaretY;
  CaretUndo := true;
  PaintCaret(false);

  { macro recording }
  if FRecording and not Com([ecRecordMacro, ecBeginCompound]) and (FCompound = 0) then
    FMacro := FMacro + Char(Lo(ACommand)) + Char(Hi(ACommand));

  try
    try
      case ACommand of

        { caret movements }

        ecLeft, ecRight, ecSelLeft, ecSelRight:
          begin
            if Com([ecSelLeft, ecSelRight]) and not FSelected then SetSel1(X, Y);
            B := Com([ecLeft, ecSelLeft]);
            if B then dec(X)
            else inc(X);
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
            begin
              _Y := 0;
              if (B) and (X < 0) then _Y := -1
              else
                if (not B) and (X > length(FLines.ParaStrings[Y])) then _Y := 1;
              if (_Y <> 0) and (YinBounds(Y + _Y)) then
              begin
                Y := Y + _Y;
                if B then X := length(FLines.ParaStrings[Y])
                else X := 0;
              end
              else
                if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            end
            else
              if not CursorBeyondEOL then X := 0;
            if Com([ecSelLeft, ecSelRight]) then SetSel1(X, Y)
            else SetUnSelected;
          end;
        ecUp, ecDown, ecSelUp, ecSelDown:
          if (Com([ecUp, ecSelUp]) and (Y > 0)) or (Com([ecDown, ecSelDown]) and (Y < FRows - 1)) or ((ACommand = ecDown) and (FCursorBeyondEOF)) then
          begin
            if Com([ecSelUp, ecSelDown]) and not FSelected then SetSel1(X, Y);
            if Com([ecUp, ecSelUp]) then dec(Y)
            else inc(Y);
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            if Com([ecSelUp, ecSelDown]) then SetSel1(X, Y)
            else SetUnSelected;
          end;
        ecPrevWord, ecSelPrevWord:
          if FLines.ParaLineCount > 0 then
          begin
            S := FLines.ParaStrings[Y];
            if X > length(S) then X := length(S);
            if X = 0 then
              if Y > 0 then
              begin
                dec(Y);
                X := length(FLines.ParaStrings[Y]);
              end
              else
            else
              begin
                if (ACommand = ecSelPrevWord) and not FSelected then SetSel1(FCaretX, FCaretY);
                B := false;
                for F := X - 1 downto 0 do
                  if B then
                    if (S[F + 1] in Separators) then
                    begin
                      X := F + 1;
                      break;
                    end
                    else
                  else
                  if not (S[F + 1] in Separators) then B := true;
                if X = FCaretX then X := 0;
                if ACommand = ecSelPrevWord then SetSel1(X, Y)
                else SetUnselected;
                if (not B) and (X = 0) and (Y > 0) then
                begin
                  {
                    Jump to the next line automatically.
                  }
                  FCaretX := X;
                  Command(ACommand);
                  Complete;
                end;
              end;
          end;
        ecNextWord, ecSelNextWord:
          if FLines.ParaLineCount > 0 then
          begin
            if X >= length(FLines.ParaStrings[Y]) then
            begin
              if Y < FLines.ParaLineCount - 1 then
              begin
                inc(Y);
                X := 0;
                if length(FLines.ParaStrings[Y]) > 0 then
                  if FLines.ParaStrings[Y][X + 1] = #32 then
                  begin
                    FCaretX := X;
                    FCaretY := Y;
                    Command(ACommand);
                    Complete;
                  end;
              end;
            end
            else
              begin
                if (ACommand = ecSelNextWord) and not FSelected then SetSel1(FCaretX, FCaretY);
                S := FLines.ParaStrings[Y];
                B := false;
                for F := X to Length(S) - 1 do
                  if B then
                    if not (S[F + 1] in Separators) then
                    begin
                      X := F;
                      break;
                    end
                    else
                  else
                    if (S[F + 1] in Separators) then B := true;
                if X = FCaretX then
                begin
                  B := X <> length(S);
                  X := length(S);
                end;
                if ACommand = ecSelNextWord then SetSel1(X, Y)
                else SetUnselected;
                if (not B) and (X = length(S)) and (Y < FLines.ParaLineCount - 1) then
                begin
                  {
                    Jump to the next line automatically.
                  }
                  FCaretX := X;
                  Command(ACommand);
                  Complete;
                end;
              end;
          end;
        ecScrollLineUp, ecScrollLineDown,
          ecScrollPageUp, ecScrollPageDown:
          begin
            if not ((ACommand = ecScrollLineDown) and (Y >= FLines.ParaLineCount - 1) and (Y = FTopRow)) then
            begin
              case ACommand of
                ecScrollLineUp:
                  F := -1;
                ecScrollLineDown:
                  F := 1;
                ecScrollPageUp:
                  F := -scbVert.LargeChange;
                else
                  F := scbVert.LargeChange;
              end;
              scbVert.Position := scbVert.Position + F;
              Scroll(true, scbVert.Position);
            end;
            if Y < FTopRow then Y := FTopRow
            else
              if Y > FLastVisibleRow then Y := FLastVisibleRow;
          end;
        ecBeginLine, ecSelBeginLine, ecBeginDoc, ecSelBeginDoc,
          ecEndLine, ecSelEndLine, ecEndDoc, ecSelEndDoc:
          begin
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) and not FSelected then
              SetSel1(FCaretX, Y);
            if Com([ecBeginLine, ecSelBeginLine]) then X := BeginLineCoordinate
            else
              if Com([ecBeginDoc, ecSelBeginDoc]) then
              begin
                X := 0;
                Y := 0;
                SetLeftTop(0, 0);
              end
              else
                if Com([ecEndLine, ecSelEndLine]) then
                  if FLines.ParaLineCount > 0 then X := Length(FLines.ParaStrings[Y])
                  else X := 0
                else
                  if Com([ecEndDoc, ecSelEndDoc]) then
                    if FLines.ParaLineCount > 0 then
                    begin
                      Y := FLines.ParaLineCount - 1;
                      X := Length(FLines.ParaStrings[Y]);
                      SetLeftTop(X - FVisibleColCount, Y - FVisibleRowCount + 1{ div 2});
                    end;
            if Com([ecSelBeginLine, ecSelBeginDoc, ecSelEndLine, ecSelEndDoc]) then SetSel1(X, Y)
            else SetUnSelected;
          end;
        ecNextPage, ecPrevPage, ecSelNextPage, ecSelPrevPage:
          begin
            if Com([ecPrevPage, ecSelPrevPage]) then _Y := -1
            else _Y := 1;
            if Com([ecSelNextPage, ecSelPrevPage]) then
            begin
              BeginUpdate;
              SetSel1(X, Y);
            end;
            Y := Y + _Y * FVisibleRowCount;
            if (not CursorBeyondEOF) or (ACommand = ecSelNextPage) then
              if Y > FLines.ParaLineCount - 1 then Y := FLines.ParaLineCount - 1;
            if Y < 0 then Y := 0;

            if Y <= (_Y * scbVert.LargeChange) then
            begin
              scbVert.Position := scbVert.Position + _Y * scbVert.LargeChange;
              Scroll(true, scbVert.Position);
            end;

            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            if Com([ecSelNextPage, ecSelPrevPage]) then
            begin
              SetSel1(X, Y);
              EndUpdate;
            end
            else SetUnSelected;
          end;
        ecSelWord:
          if not FSelected and (GetWordOnPosEx(FLines.ParaStrings[Y] + ' ', X + 1, iBeg, iEnd) <> '') then
          begin
            SetSel1(iBeg - 1, Y);
            SetSel1(iEnd - 1, Y);
            X := iEnd - 1;
          end;
        ecWindowTop:
          begin
            Y := FTopRow;
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          end;
        ecWindowBottom:
          begin
            Y := FTopRow + FVisibleRowCount - 1;
            if (not FCursorBeyondEOL) and (YinBounds(Y)) then
              if X > length(FLines.ParaStrings[Y]) then X := length(FLines.ParaStrings[Y]);
            SetUnSelected;
          end;
        { editing }
        ecCharFirst..ecCharLast:
          if not FReadOnly then
          begin
            InsertChar(Char(ACommand - ecCharFirst));
            Complete;
          end;
        ecInsertPara:
          if not FReadOnly then
          begin
            ClearSelection;
            ReLine;
            X := FCaretX;
            Y := FCaretY;
            FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
            S := FLines[FParaY];
            S2 := Copy(S, FParaX + 1, length(S));
            T := S2;
            if Assigned(FOnBreakLine) then FOnBreakLine(Self, S, S2);
            if S2 = T then
            begin
              {
                Just paragraph insertion.
              }
              F := 0;
              if FAutoIndent then
              begin
                if FSmartIndent then X := GetTabStop(0, Y, tsAutoIndent, true)
                else X := FAutoIndentSize;
                if _Trim(S2) > '' then
                begin
                  i := length(S2) - length(_TrimLeft(S2));
                  if X - i > 0 then
                  begin
                    S2 := Spaces(X - i) + S2;
                    F := X - i;
                  end;
                end;
              end
              else X := 0;
              BeginCompound;
              TInsertUndo.Create(Self, FCaretX, FCaretY, #13#10 + Spaces(F));
              CaretUndo := false;
              if not FKeepTrailingBlanks then
                FLines.Internal[FParaY] := TrimRight(Copy(S, 1, FParaX))
              else FLines.Internal[FParaY] := Copy(S, 1, FParaX);
              FLines.Insert(FParaY + 1, S2);
              FLines.Paragraph2Caret(FParaY + 1, 0, F, Y);
              EndCompound;
            end
            else
              begin
                {
                  User has changed wrapped line.
                }
                T := Copy(S, 1, FParaX) + #13#10 + S2 + #13#10;
                F := FLines.GetParaOffs(FParaY);
                S2 := FLines.Text;
                System.Delete(S2, F + 1, length(S) + 2);    //  delete old text.
                System.Insert(T, S2, F + 1);                //  and insert a new one.

                FLines.Paragraph2Caret(FParaY, 0, F, _Y);

                CaretUndo := false;
                BeginCompound;
                TCaretUndo.Create(Self, FCaretX, FCaretY);
                TDeleteUndo.Create(Self, 0, _Y, S + #13#10);
                TInsertUndo.Create(Self, 0, _Y, T);
                EndCompound;

                FLines.SetLockText(S2);
                FLines.Paragraph2Caret(FParaY + 1, 0, F, Y);
                X := 0;
              end;
            UpdateEditorSize(false);
            F := RepaintParagraph(Max(0, Y - 1));
            RedrawFrom(F + 1);
            Changed;
          end;
        ecBackword:
          if not FReadOnly then
          begin
            if length(FLines.ParaStrings[Y]) > 0 then
            begin
              Command(ecBeginCompound);
              Command(ecBeginUpdate);
              Command(ecSelPrevWord);
              Command(ecDeleteSelected);
              Command(ecEndUpdate);
              Command(ecEndCompound);
            end
            else Command(ecBackspace);
            Complete;
          end;
        ecBackspace:
          if not FReadOnly then
          begin
            if FSelected then
            begin
              DoAndCorrectXY(ClearSelection);
              Changed;
            end
            else
              begin
                ReLine;
                X := FCaretX;
                Y := FCaretY;
                FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
                if X > 0 then
                begin
                  {
                    Delete in the middle of the line.
                  }
                  if FBackspaceUnindents then X := GetBackStop(FCaretX, FCaretY)
                  else X := FCaretX - 1;
                  S := Copy(FLines.ParaStrings[FCaretY], X + 1, FCaretX - X);

                  dec(FParaX, length(S));

                  F := FLines.Paragraphs[FParaY].FCount;

                  FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1, X) +
                               Copy(FLines.ParaStrings[Y], FCaretX + 1, Length(FLines.ParaStrings[Y]));
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                  TBackspaceUndo.Create(Self, X, Y, S);
                  CaretUndo := false;

                  B := F <> FLines.Paragraphs[FParaY].FCount;
                  F := RepaintParagraph(Y);
                  if B then
                  begin
                    UpdateEditorSize(false);
                    RedrawFrom(F + 1);
                  end;
                  Changed;
                end
                else
                  if Y > 0 then
                  begin
                    {
                      Pasting of the lines.
                    }
                    if FParaX > 0 then
                    begin
                      {
                        Pasting of the wrapped lines in paragraph.
                      }
                      T := FLines[FParaY];
                      S := Copy(T, FParaX, 1);

                      System.Delete(T, FParaX, 1);
                      FLines.Internal[FParaY] := T;
                      dec(FParaX);
                      FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                      TBackspaceUndo.Create(Self, X, Y, S);
                      CaretUndo := false;
                    end
                    else
                      if FParaY > 0 then
                      begin
                        {
                          Pasting of the paragraphs.
                        }
                        inc(FUpdateLock);

                        S := FLines[FParaY - 1];
                        S2 := FLines[FParaY];
                        if Assigned(FOnConcatLine) then FOnConcatLine(Self, S, S2);

                        CaretUndo := false;
                        FLines.Paragraph2Caret(FParaY - 1, 0, F, _Y);

                        BeginCompound;
                        TCaretUndo.Create(Self, X, Y);
                        TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY - 1] + #13#10 + FLines[FParaY] + #13#10);
                        TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                        EndCompound;

                        FLines.Internal[FParaY - 1] := S + S2;
                        FLines.Delete(FParaY);

                        dec(FUpdateLock);
                        FLines.Paragraph2Caret(FParaY - 1, length(S), X, Y);
                      end
                      else Complete;
                    UpdateEditorSize(false);
                    F := RepaintParagraph(Y);
                    RedrawFrom(F + 1);
                    Changed;
                  end;
              end;
          end;
        ecDelete:
          if not FReadOnly then
          begin
            if FLines.ParaLineCount = 0 then FLines.Add('');
            if FSelected then
            begin
              DoAndCorrectXY(ClearSelection);
              Changed;
            end
            else
              begin
                ReLine;
                X := FCaretX;
                Y := FCaretY;
                FLines.Caret2Paragraph(X, Y, FParaY, FParaX);
                if X < Length(FLines.ParaStrings[Y]) then
                begin
                  {
                    Deleting in the middle of the line.
                  }
                  TDeleteUndo.Create(Self, FCaretX, FCaretY, FLines.ParaStrings[Y] [X + 1]);
                  CaretUndo := false;

                  F := FLines.Paragraphs[FParaY].FCount;

                  FLines.InternalParaStrings[Y] := Copy(FLines.ParaStrings[Y], 1, X) +
                    Copy(FLines.ParaStrings[Y], X + 2, Length(FLines.ParaStrings[Y]));
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                  B := F <> FLines.Paragraphs[FParaY].FCount;
                  F := RepaintParagraph(Y);
                  if B then
                  begin
                    UpdateEditorSize(false);
                    RedrawFrom(F + 1);
                  end;
                  Changed;
                end
                else
                  if (Y >= 0) and (Y <= FLines.ParaLineCount - 2) then
                  begin
                    {
                      Pasting of the lines.
                    }
                    S := FLines[FParaY];
                    if FParaX < length(S) then
                    begin
                      {
                        Pasting of the wrapped lines.
                      }
                      TDeleteUndo.Create(Self, FCaretX, FCaretY, System.Copy(S, FParaX + 1, 1));
                      CaretUndo := false;

                      System.Delete(S, FParaX + 1, 1);
                      FLines.Internal[FParaY] := S;              
                      FLines.Paragraph2Caret(FParaY, FParaX, X, Y);
                    end
                    else
                      begin
                        {
                          Pasting of the paragraphs.
                        }
                        inc(FUpdateLock);

                        S := FLines[FParaY];
                        S2 := FLines[FParaY + 1];
                        if Assigned(FOnConcatLine) then FOnConcatLine(Self, S, S2);

                        CaretUndo := false;
                        FLines.Paragraph2Caret(FParaY, 0, F, _Y);

                        BeginCompound;
                        TCaretUndo.Create(Self, X, Y);
                        TDeleteUndo.Create(Self, 0, _Y, FLines[FParaY] + #13#10 + FLines[FParaY + 1] + #13#10);
                        TInsertUndo.Create(Self, 0, _Y, S + S2 + #13#10);
                        EndCompound;

                        FLines.Internal[FParaY] := S + S2;
                        FLines.Delete(FParaY + 1);

                        dec(FUpdateLock);
                        FLines.Paragraph2Caret(FParaY, length(S), X, Y);
                      end;
                    UpdateEditorSize(false);
                    F := RepaintParagraph(FCaretY);
                    RedrawFrom(F + 1);
                    Changed;
                  end;
              end;
          end;
        ecTab, ecBackTab:
          if not FReadOnly then
            if FSelected then
              if ACommand = ecTab then PostCommand(ecIndent)
              else PostCommand(ecUnindent)
            else
              begin
                ReLine;
                X := FCaretX;
                Y := FCaretY;
                X := GetTabStop(FCaretX, FCaretY, tsTabStop, ACommand = ecTab);
                if (ACommand = ecTab) and FInsertMode then
                begin
                  S := FLines.ParaStrings[FCaretY];
                  FLines.Caret2Paragraph(FCaretX, FCaretY, FParaY, FParaX);
                  S2 := Spaces(X - FCaretX);

                  TInsertTabUndo.Create(Self, FCaretX, FCaretY, S2);
                  CaretUndo := false;

                  Insert(S2, S, FCaretX + 1);

                  F := FLines.Paragraphs[FParaY].FCount;

                  FLines.InternalParaStrings[FCaretY] := S;
                  inc(FParaX, X - FCaretX);
                  FLines.Paragraph2Caret(FParaY, FParaX, X, Y);

                  B := F <> FLines.Paragraphs[FParaY].FCount;
                  F := RepaintParagraph(FCaretY);
                  if B then
                  begin
                    UpdateEditorSize(false);
                    RedrawFrom(F + 1);
                  end;
                  Changed;
                end;
              end;
        ecIndent, ecUnindent:
          if (not FReadOnly) and (FSelected) then
          begin
            isIndent := ACommand = ecIndent;
            FLines.Caret2Paragraph(FSelBegX, FSelBegY, indentY1, indentX);
            FLines.Caret2Paragraph(FSelEndX, FSelEndY, indentY2, indentX);
            if indentX = 0 then
            begin
              dec(indentY2);
              indentX := length(FLines[indentY2]);
              FLines.Paragraph2Caret(indentY2, indentX, FSelEndX, FSelEndY);
            end;

            BeginCompound;
            TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY, FSelEndX, FSelEndY);
            if isIndent then
            begin
              TIndentUndo.Create(Self, indentY1, indentY2, FIndentSize);
              EndCompound;
            end;
            CaretUndo := false;

            F := FIndentSize;
            if not isIndent then F := (-1) * F;
            S := Spaces(FIndentSize);
            B := false;
            FLines.BeginUpdate;
            if isIndent then
            begin
              B := true;
              for iBeg := indentY1 to indentY2 do
                FLines[iBeg] := S + FLines[iBeg];
            end
            else
              for iBeg := indentY1 to indentY2 do
                if (S > '') and (S[1] = ' ') then
                begin
                  B := true;
                  T := FLines[iBeg];
                  i := length(T) - length(_TrimLeft(T));
                  if i > FIndentSize then i := FIndentSize;
                  System.Delete(T, 1, i);
                  FLines[iBeg] := T;
                  TUnindentUndo.Create(Self, iBeg, i);
                end;
            FLines.EndUpdate;
            if not isIndent then
              if B then EndCompound
              else
                begin
                  UndoBuffer.Delete;
                  UndoBuffer.Delete;
                end;
            if B then
            begin
              inc(FSelBegX, F);
              if FSelBegX < 0 then FSelBegX := 0;
              inc(FSelEndX, F);
              if FSelEndX < 0 then FSelEndX := length(FLines[indentY2]);
              if (Y >= indentY1) and (Y <= indentY2) then inc(X, F);
              Changed;
            end;
            FSelected := true;
          end;
        ecChangeInsertMode:
          begin
            FInsertMode := not FInsertMode;
            StatusChanged;
          end;
        ecClipBoardCut:
          if not FReadOnly then DoAndCorrectXY(CutToClipboard);
        ecClipBoardCopy:
          CopyToClipboard;
        ecClipBoardPaste:
          if not FReadOnly then DoAndCorrectXY(PasteFromClipboard);
        ecDeleteSelected:
          if not FReadOnly and FSelected then DoAndCorrectXY(ClearSelection);

        ecDeleteWord:
          if not FReadOnly then
            if length(FLines.ParaStrings[Y]) = 0 then Command(ecDelete)
            else
              begin
                Command(ecBeginCompound);
                Command(ecBeginUpdate);
                Command(ecSelNextWord);
                Command(ecDeleteSelected);
                Command(ecEndUpdate);
                Command(ecEndCompound);
                Complete;
              end;
        ecDeleteLine:
          if (not FReadOnly) and (Y >= 0) and (Y <= FLines.ParaLineCount - 1) then
          begin
            FLines.Index2ParaIndex(Y, F, _Y);
            B := (not FWordWrap) or (_Y = FLines.Paragraphs[F].FCount - 1);
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginLine);
            Command(ecSelEndLine);
            Command(ecDeleteSelected);
            // âîçìîæíî íóæíî ñêëåèòü ñòðîêè
            if B then Command(ecDelete);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            Complete;
          end;
        ecSelAll:
          begin
            Command(ecBeginCompound);
            Command(ecBeginUpdate);
            Command(ecBeginDoc);
            Command(ecSelEndDoc);
            Command(ecEndUpdate);
            Command(ecEndCompound);
            SelectionChanged;
            Complete;
          end;
        ecToUpperCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_UPPER);
        ecToLowerCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_LOWER);
        ecChangeCase:
          if (not FReadOnly) and (FSelected) then
            SelText := DoChangeCase(SelText, ME_CASE_CONVERT_INVERT);
        ecUndo:
          if not FReadOnly then
          begin
            FUndoBuffer.Undo;
            PaintCaret(true);
            Complete;
          end;
        ecRedo:
          if not FReadOnly then
          begin
            FUndoBuffer.Redo;
            PaintCaret(true);
            Complete;
          end;
        ecBeginCompound:
          BeginCompound;
        ecEndCompound:
          EndCompound;

        ecSetBookmark0..ecSetBookmark9:
          ChangeBookMark(ACommand - ecSetBookmark0, true);
        ecGotoBookmark0..ecGotoBookmark9:
          begin
            ChangeBookmark(ACommand - ecGotoBookmark0, false);
            X := FCaretX;
            Y := FCaretY;
          end;
        ecInsertMacro0..ecInsertMacroZ:
          if (Assigned(FOnInsertMacro)) and (not FReadOnly) then
          begin
            S := FOnInsertMacro(Self, ACommand - ecInsertMacro0);
            if S = '' then exit;
            InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          end;
        ecBlockOpA..ecBlockOpZ:
          if (not FReadOnly) and (Assigned(FOnBlockOperation)) and (FSelected) then
            SelText := FOnBlockOperation(Self, ACommand - ecBlockOpA, SelText);
        ecCompletionIdentifiers:
          if (not FReadOnly) and (FCompletion.Enabled) then
          begin
            FCompletion.DoCompletion(cmIdentifiers);
            PaintCaret(true);
            Complete;
          end;
        ecCompletionTemplates:
          if (not FReadOnly) and (FCompletion.Enabled) then
          begin
            FCompletion.DoCompletion(cmTemplates);
            PaintCaret(true);
            Complete;
          end;
        ecBeginUpdate:
          BeginUpdate;
        ecEndUpdate:
          EndUpdate;

        ecRecordMacro:
          if FRecording then EndRecord(FDefMacro)
          else BeginRecord;
        ecPlayMacro:
          begin
            PlayMacro(FDefMacro);
            Complete;
          end;
        ecSaveBlock:
          if (FSelected) and (Assigned(FOnSaveBlock)) then FOnSaveBlock(Self, SelText);
        ecInsertBlock:
          if (not FReadOnly) and (Assigned(FOnInsertBlock)) then
          begin
            if FOnInsertBlock(Self, S) then InsertTextAtCurrentPos(S);
            PaintCaret(true);
            Complete;
          end;
      end;

      if CaretUndo then SetCaret(X, Y)
      else SetCaretInternal(X, Y);
    except
      on E: EComplete do { OK };
    end;
  finally
    PaintCaret(true);
  end;
end;

procedure TCustomMemoEx.PostCommand(ACommand: TEditCommand);
begin
  PostMessage(Handle, WM_EDITCOMMAND, ACommand, 0);
end;

procedure TCustomMemoEx.ClipboardChanged;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) then exit;
  if Assigned(FOnChangeClipboardState) then
    FOnChangeClipboardState(Self, IsClipboardFormatAvailable(CF_TEXT) or IsClipboardFormatAvailable(CF_OEMTEXT));
end;

function TCustomMemoEx.GetThemeState: integer;
begin
  if not Enabled then Result := ETS_DISABLED
  else
    if FReadOnly then Result := ETS_READONLY
    else
      if Focused then Result := ETS_FOCUSED
      else Result := ETS_NORMAL;
end;

procedure TCustomMemoEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomMemoEx.WndProc(var Message: TMessage);
var
  Form: TCustomForm;
  pt, temp: TPoint;
  RC: TRect;
  DC: HDC;
begin
  case Message.Msg of
    CM_COLORCHANGED:
      begin
        Message.Result := 0;
        Invalidate2;
        exit;
      end;
    WM_MOUSEWHEEL:
      begin
        MouseWheelHandler(Message);
        Message.Result := 0;
        exit;
      end;
    WM_SYSCHAR:
      if Message.wParam = VK_BACK then
      begin
        Message.Result := 0;
        exit;
      end;
    WM_SETFOCUS:
      begin
        Form := GetParentForm(Self);
        if (Form <> nil) and (not Form.SetFocusedControl(Self)) then exit;
        CreateCaret(Handle, 0, 2, CellRect.Height - 2);
        PaintCaret(true);
        DoEnter;
      end;
    WM_KILLFOCUS:
      begin
        if csFocusing in ControlState then exit;
        if FCompletion.FVisible then FCompletion.CloseUp(false);
        DestroyCaret;
        DoExit;
      end;
    WM_GETDLGCODE:
      begin
        inherited WndProc(Message);
        TWMGetDlgCode(Message).Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
        if FWantTabs then TWMGetDlgCode(Message).Result := TWMGetDlgCode(Message).Result or DLGC_WANTTAB;
        exit;
      end;
    WM_HSCROLL:
      begin
        scbHorz.DoScroll(TWMHScroll(Message));
        exit;
      end;
    WM_VSCROLL:
      begin
        scbVert.DoScroll(TWMVScroll(Message));
        exit;
      end;
    WM_SETTINGCHANGE:
      begin
        scbVertWidth := GetSystemMetrics(SM_CXVSCROLL);
        scbHorzHeight := GetSystemMetrics(SM_CYHSCROLL);
      end;
    WM_EDITCOMMAND:
      begin
        Command(Message.WParam);
        Message.Result := ord(true);
        exit;
      end;
    WM_CHANGECBCHAIN:
      begin
        Message.Result := 0;
        if TWMChangeCBChain(Message).Remove = NextClipViewer then NextClipViewer := TWMChangeCBChain(Message).Next
          else SendMessage(NextClipViewer, WM_CHANGECBCHAIN, TWMChangeCBChain(Message).Remove, TWMChangeCBChain(Message).Next);
        exit;
      end;
    WM_DRAWCLIPBOARD:
      begin
        ClipboardChanged;
        SendMessage(NextClipViewer, WM_DRAWCLIPBOARD, 0, 0);
        exit;
      end;
    WM_DESTROY:
      ChangeClipboardChain(Handle, NextClipViewer);
    WM_CONTEXTMENU:
      begin
        pt := SmallPointToPoint(TWMContextMenu(Message).Pos);
        if pt.X < 0 then temp := pt
        else temp := ScreenToClient(pt);
        if PtInRect(ClientRect, temp) then
          GetWordUnderCursor(temp.X, temp.Y);
      end;
    WM_COPY:
      begin
        PostCommand(ecClipboardCopy);
        Message.Result := ord(true);
        exit;
      end;
    WM_CUT:
      begin
        if not FReadOnly then PostCommand(ecClipboardCut);
        Message.Result := ord(true);
        exit;
      end;
    WM_PASTE:
      begin
        if not FReadOnly then PostCommand(ecClipBoardPaste);
        Message.Result := ord(true);
        exit;
      end;
    WM_THEMECHANGED:
      begin
        if FCurrentTheme > 0 then FreeThemeHandle(FCurrentTheme);
        if ThemesAvailable then
          FCurrentTheme := CreateThemeHandle(Handle);
        Invalidate;
        Message.Result := 0;
        exit;
      end;
    WM_NCPAINT:
      if (FCurrentTheme > 0) and (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_CLIENTEDGE <> 0) then
      begin
        inherited WndProc(Message);
        DC := GetWindowDC(Handle);
        try
          RC := Rect(0, 0, Width, Height);
          ExcludeClipRect(DC, RC.Left + 2, RC.Top + 2, RC.Right - 2, RC.Bottom - 2);
          if IsThemeBackgroundPartiallyTransparent(FCurrentTheme, DC, EP_CARET) then
            DrawThemeParentBackground(Handle, DC, RC);
          DrawThemeBackground(FCurrentTheme, DC, EP_CARET, GetThemeState, RC, nil);
        finally
          ReleaseDC(Handle, DC);
        end;
        Message.Result := 0;
        exit;
      end;
    WM_ERASEBKGND:
      begin
        Message.Result := 0;
        exit;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomMemoEx.InvalidateBookmarks;
var
  i: TBookmarkNum;
begin
  for i := Low(TBookmarkNum) to High(TBookmarkNum) do FBookmarks[i].Valid := false;
  Invalidate;
end;

procedure TCustomMemoEx.ChangeBookmark(const BookMark: TBookMarkNum; const
  Valid: boolean);

  procedure SetXY(X, Y: integer);
  var
    X1, Y1: integer;
  begin
    X1 := FLeftCol;
    Y1 := FTopRow;
    if (Y < FTopRow) or (Y > FLastVisibleRow) then
      Y1 := Y - (FVisibleRowCount div 2);
    if (X < FLeftCol) or (X > FVisibleColCount) then
      X1 := X - (FVisibleColCount div 2);
    SetLeftTop(X1, Y1);
    SetCaret(X, Y);
  end;

begin
  if Valid then
    if FBookmarks[Bookmark].Valid and (FBookmarks[Bookmark].Y = FCaretY) then FBookmarks[Bookmark].Valid := false
    else
      begin
        FBookmarks[Bookmark].X := FCaretX;
        FBookmarks[Bookmark].Y := FCaretY;
        FBookmarks[Bookmark].Valid := true;
      end
  else
    if FBookmarks[Bookmark].Valid then SetXY(FBookmarks[Bookmark].X, FBookmarks[Bookmark].Y);
  BookmarkCnanged(BookMark);
end;

procedure TCustomMemoEx.BookmarkCnanged(BookMark: integer);
begin
  FGutter.Invalidate;
end;

function TCustomMemoEx.GetBookmark(AIndex: integer): TBookmark;
begin
  if (AIndex < Low(TBookmarkNum)) or (AIndex > High(TBookmarkNum)) then raise EListError.CreateFmt(SListIndexError, [AIndex]);
  Result := FBookmarks[AIndex];
end;

procedure TCustomMemoEx.SetBookmark(AIndex: integer; ABookmark: TBookmark);
begin
  if (AIndex < Low(TBookmarkNum)) or (AIndex > High(TBookmarkNum)) then raise EListError.CreateFmt(SListIndexError, [AIndex]);
  FBookmarks[AIndex] := ABookmark;
end;

procedure TCustomMemoEx.SelectionChanged;
begin
  if not (csLoading in ComponentState) then
    if Assigned(FOnSelectionChange) then FOnSelectionChange(Self);
end;

procedure TCustomMemoEx.SetSel(const ASelX, ASelY: integer);

  procedure UpdateSelected;
  var
    iR: integer;
  begin
    if FUpdateLock = 0 then
    begin
      if (FUpdateSelBegY <> FSelBegY) or (FUpdateSelBegX <> FSelBegX) then
        for iR := Min(FUpdateSelBegY, FSelBegY) to Max(FUpdateSelBegY, FSelBegY) do
          PaintLine(iR, -1, -1);
      if (FUpdateSelEndY <> FSelEndY) or (FUpdateSelEndX <> FSelEndX) then
        for iR := Min(FUpdateSelEndY, FSelEndY) to Max(FUpdateSelEndY, FSelEndY) do
          PaintLine(iR, -1, -1);
      SelectionChanged;
    end;
  end;

var
  SelX, SelY: integer;
begin
  if ASelX < 0 then SelX := 0
  else SelX := ASelX;
  if ASelY < 0 then SelY := 0
  else SelY := ASelY;
  if not FSelected then
  begin
    FSelStartX := SelX;
    FSelStartY := SelY;
    FSelEndX := SelX;
    FSelEndY := SelY;
    FSelBegX := SelX;
    FSelBegY := SelY;
    FSelected := true;
  end
  else
    begin
      TSelectUndo.Create(Self, FCaretX, FCaretY, FSelBlock, FSelBegX, FSelBegY,
        FSelEndX, FSelEndY);
      FUpdateSelBegX := FSelBegX;
      FUpdateSelBegY := FSelBegY;
      FUpdateSelEndX := FSelEndX;
      FUpdateSelEndY := FSelEndY;
      if SelY <= FSelStartY then FSelBegY := SelY;
      if SelY >= FSelStartY then FSelEndY := SelY;
      if (SelY < FSelStartY) or ((SelY = FSelStartY) and (SelX <= FSelStartX)) then
      begin
        FSelBegX := SelX;
        FSelEndX := FSelStartX;
        FSelEndY := FSelStartY;
      end
      else
        if (SelY > FSelStartY) or ((SelY = FSelStartY) and (SelX >= FSelStartX)) then
        begin
          FSelBegX := FSelStartX;
          FSelBegY := FSelStartY;
          FSelEndX := SelX;
        end;
      if FSelBegY < 0 then FSelBegY := 0;
      FSelected := true;
      if FCompound = 0 then UpdateSelected;
    end;
  if FUpdateSelBegY > FSelBegY then FUpdateSelBegY := FSelBegY;
  if FUpdateSelEndY < FSelEndY then FUpdateSelEndY := FSelEndY;
end;

procedure TCustomMemoEx.Mouse2Cell(const X, Y: integer; var CX, CY: integer);
begin
  CX := Round((X - EditorClient.Left) / FCellRect.Width);
  CY := (Y - EditorClient.Top) div FCellRect.Height;
end;

procedure TCustomMemoEx.Mouse2Caret(const X, Y: integer; var CX, CY: integer);
begin
  Mouse2Cell(X, Y, CX, CY);
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := CX + FLeftCol;
  CY := CY + FTopRow;
  if CX > FLastVisibleCol then CX := FLastVisibleCol;
  if CY > FLines.ParaLineCount - 1 then CY := FLines.ParaLineCount - 1;
end;

procedure TCustomMemoEx.CaretCoord(const X, Y: integer; var CX, CY: integer);
begin
  CX := X - FLeftCol;
  CY := Y - FTopRow;
  if CX < 0 then CX := 0;
  if CY < 0 then CY := 0;
  CX := FCellRect.Width * CX;
  CY := FCellRect.Height * CY;
end;

function TCustomMemoEx.ExtractStringWithStyle(XX, YY: integer; const From: string; Style: word; const LineAttrs: TLineAttrs): string;
var
  i: integer;
begin
  if Style <> ME_EX_STYLE_DEFAULT then
  begin
    Result := '';
    if XX <= length(From) then
      for i := XX downto 0 do
        if LineAttrs[i].ex_style = Style then Result := From[i + 1] + Result
        else break;
    for i := XX + 1 to length(From) - 1 do
      if LineAttrs[i].ex_style = Style then Result := Result + From[i + 1]
      else break;
  end;
end;

{
  We shound count real attribute's offset if invisble stripped.
}
function TCustomMemoEx.GetAttrDelta(StartFrom, EndTo: integer; const LineAttrs: TLineAttrs): integer;
var
  i, j: integer;
begin
  Result := 0;
  if (ReadOnly) and (FStripInvisible) then
  begin
    j := EndTo;
    i := StartFrom;
    while (i <= j) and (i < SelAttrs_Size) do
    begin
      if LineAttrs[i].FC = LineAttrs[i].BC then
      begin
        inc(Result);
        inc(j);
      end;
      inc(i);
    end;
  end;
end;

function TCustomMemoEx.DoMouseWheel(Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint): boolean;
begin
  MouseWheelScroll(WheelDelta);
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TCustomMemoEx.MouseWheelScroll(Delta: integer);
var
  i: integer;
begin
  i := Mouse.WheelScrollLines;
  if Delta > 0 then i := -i;
  scbVert.Position := scbVert.Position + i;
  Scroll(true, scbVert.Position);
end;

procedure TCustomMemoEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
var
  XX, YY: integer;
begin
  if double_clicked then
  begin
    double_clicked := false;
    exit;
  end;
  if FCompletion.FVisible then FCompletion.CloseUp(false);
  mouse_down := true;
  SetCapture(Handle);
  mouse_dragged := false;
  gutter_clicked := (X <= FGutterWidth);
  Mouse2Caret(X, Y, XX, YY);
  PaintCaret(false);
  if (Button = mbLeft) and (not (ssShift in Shift)) then SetUnSelected;
  SetFocus;
  if YinBounds(YY) then
  begin
    if not FCursorBeyondEOL then
      if XX > length(FLines.ParaStrings[YY]) then XX := length(FLines.ParaStrings[YY]);
    if (ssShift in Shift) and (SelLength = 0) then SetSel(FCaretX, FCaretY);
    SetCaret(XX, YY);
    if ssShift in Shift then SetSel(XX, YY);
  end;
  PaintCaret(true);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomMemoEx.DblClick;
var
  i, PY, PX, iBeg, iEnd: integer;
begin
  double_clicked := true;
  if Assigned(FOnDblClick) then FOnDblClick(Self);
  if FDoubleClickLine then
  begin
    PaintCaret(false);
    SetSel(0, FCaretY);
    if FCaretY = FLines.ParaLineCount - 1 then
    begin
      SetSel(Length(FLines.ParaStrings[FCaretY]), FCaretY);
      SetCaret(Length(FLines.ParaStrings[FCaretY]), FCaretY);
    end
    else
      begin
        SetSel(0, FCaretY + 1);
        SetCaret(0, FCaretY + 1);
      end;
    PaintCaret(true);
  end
  else
    if YinBounds(FCaretY) then
    begin
      FLines.GetParagraphByIndex(FCaretY, PY, PX);
      i := GetAttrDelta(PX, FCaretX + PX, FLines.Paragraphs[PY].FAttrs);
      if GetWordOnPosEx(FLines.ParaStrings[FCaretY] + ' ', FCaretX + 1 + i, iBeg, iEnd) <> '' then
      begin
        PaintCaret(false);
        SetSel(iBeg - 1, FCaretY);
        SetSel(iEnd - 1, FCaretY);
        SetCaret(iEnd - 1 - i, FCaretY);
        PaintCaret(true);
      end;
    end;
end;

procedure TCustomMemoEx.GetWordUnderCursor(X, Y: integer);
var
  XX, YY, PX, PY, i: integer;
  s: string;
begin
  Mouse2Caret(X, Y, XX, YY);
  if YinBounds(YY) then
  begin
    s := FLines.GetParagraphByIndex(YY, PY, PX);
    GetLineAttr(PY, YY, PX, length(s), 0, length(s), s);
    i := XX + PX + GetAttrDelta(PX, XX + PX, FLines.Paragraphs[PY].FAttrs);
    if (i > 0) and (i < SelAttrs_Size) then
    begin
      FWordStyleUnderCursor := FLines.Paragraphs[PY].FAttrs[i - 1].ex_style;
      FWordUnderCursor := ExtractStringWithStyle(i, YY, s,
                                  FWordStyleUnderCursor,
                                  FLines.Paragraphs[PY].FAttrs);
    end
    else
      begin
        FWordUnderCursor := '';
        FWordStyleUnderCursor := ME_EX_STYLE_DEFAULT;
      end;
  end;
end;

procedure TCustomMemoEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  timerScroll.Enabled := false;
  mouse_down := false;
  ReleaseCapture;
  if (Button = mbLeft) and (not mouse_dragged) then
      if Assigned(FOnWordClick) then
      begin
        GetWordUnderCursor(X, Y);
        if FWordUnderCursor <> '' then
          FOnWordClick(Self, FWordUnderCursor, FWordStyleUnderCursor);
      end;
  inherited MouseUp(Button, Shift, X, Y)
end;

procedure TCustomMemoEx.MouseMove(Shift: TShiftState; X, Y: integer);
var
  C: TCursor;
  S: string;
  i, PY, PX: integer;
begin
  MouseMoveX := X;
  MouseMoveY := Y;
  Mouse2Caret(X, Y, MouseMoveXX, MouseMoveYY);
  if X < FGutterWidth then Cursor := crArrow
  else Cursor := crIBeam;
  if (Shift = [ssLeft]) and (mouse_down) then
  begin
    mouse_dragged := true;
    Cursor := crIBeam;
    PaintCaret(false);
    if MouseMoveYY <= FLastVisibleRow then
    begin
      if not FCursorBeyondEOL then
        if YinBounds(MouseMoveYY) then
          if MouseMoveXX > length(FLines.ParaStrings[MouseMoveYY]) then MouseMoveXX := length(FLines.ParaStrings[MouseMoveYY])
          else
        else MouseMoveXX := 0;
      SetSel(MouseMoveXX, MouseMoveYY);
      SetCaret(MouseMoveXX, MouseMoveYY);
    end;
    timerScroll.Enabled := (Y < 0) or (Y > ClientHeight) or (X < 0) or (X > ClientHeight);
    PaintCaret(true);
  end
  else
    if (Assigned(FOnMouseOver)) and (YinBounds(MouseMoveYY)) and (X >= FGutterWidth) then
    begin
      S := FLines.GetParagraphByIndex(MouseMoveYY, PY, PX);
      GetLineAttr(PY, MouseMoveYY, PX, length(S), 0, MouseMoveXX + 1 + PX, S);
      i := MouseMoveXX + PX + GetAttrDelta(PX, MouseMoveXX + PX, FLines.Paragraphs[PY].FAttrs) - 1;
      if i < SelAttrs_Size then
      begin
        C := crIBeam;
        FOnMouseOver(Self, FLines.Paragraphs[PY].FAttrs[i].ex_style, C);
        if C <> Cursor then Cursor := C;
      end
      else Cursor := crIBeam;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomMemoEx.ScrollTimer(Sender: TObject);

  procedure ModifyCoordinate(Coordinate1: integer; var Coordinate2: integer; Border, VisibleCount: integer);
  begin
    if (Coordinate1 < -20) then dec(Coordinate2, VisibleCount)
    else
      if (Coordinate1 < 0) then dec(Coordinate2)
      else
      if (Coordinate1 > Border + 20) then inc(Coordinate2, VisibleCount)
        else
        if (Coordinate1 > Border) then inc(Coordinate2);
  end;

begin
  if (MouseMoveY < 0) or (MouseMoveY > ClientHeight) or (MouseMoveX < 0) or (MouseMoveX > ClientWidth) then
  begin
    ModifyCoordinate(MouseMoveY, MouseMoveYY, ClientHeight, FVisibleRowCount);
    ModifyCoordinate(MouseMoveX, MouseMoveXX, ClientWidth, FVisibleColCount div 2);
    PaintCaret(false);
    SetSel(MouseMoveXX, MouseMoveYY);
    SetCaret(MouseMoveXX, MouseMoveYY);
    PaintCaret(true);
  end;
end;

function TCustomMemoEx.GetRealOffs(DefOffs, Index: integer): integer;
var
  l: integer;
begin
  Result := DefOffs;
  if (Index > -1) and (Index < FLines.FParaLinesCount) then
  begin
    l := length(FLines.ParaStrings[Index]);
    if l > 0 then
      if Result > l then Result := l
      else
    else Result := 0;
  end;
end;

function TCustomMemoEx.GetSelText: string;
var
  sb, se: integer;
begin
  Result := '';
  if not FSelected then exit;
  if not FSelBlock then
  begin
    if (FSelBegY < 0) or (FSelBegY > FLines.ParaLineCount - 1) or (FSelEndY < 0) or
      (FSelEndY > FLines.ParaLineCount - 1) then
    begin
      Err;
      Exit;
    end;
    sb := GetRealOffs(FSelBegX, FSelBegY);
    se := GetRealOffs(FSelEndX, FSelEndY);
    if (se = sb) and (FSelBegY = FSelEndY) then exit;
    sb := PosFromCaret(sb, FSelBegY);
    se := PosFromCaret(se, FSelEndY);
    Result := System.Copy(FLines.Text, sb + 1, se - sb + integer(FInclusive));
  end;
end;

procedure TCustomMemoEx.SetSelText(const AValue: string);
begin
  BeginUpdate;
  try
    BeginCompound;
    ClearSelection;
    if AValue <> '' then
    begin
      InsertText(AValue);
      FSelectedText := true;
      SelStart := PosFromCaret(FSelBegX, FSelBegY) + 1;
      SelLength := Length(AValue);
    end;
    EndCompound;
  finally
    EndUpdate;
  end;
end;

procedure TCustomMemoEx.ClipBoardCopy;
begin
  CopyToClipboard;
end;

procedure TCustomMemoEx.ReplaceWord(const NewString: string);
var
  iBeg, iEnd: integer;

  function GetWordOnPos2(S: string; P: integer): string;
  begin
    Result := '';
    if P < 1 then exit;
    if (S[P] in Separators) and ((P < 1) or (S[P - 1] in Separators)) then
      inc(P);
    iBeg := P;
    while iBeg >= 1 do
      if S[iBeg] in Separators then break
      else dec(iBeg);
    inc(iBeg);
    iEnd := P;
    while iEnd <= Length(S) do
      if S[iEnd] in Separators then break
      else inc(iEnd);
    if iEnd > iBeg then Result := Copy(S, iBeg, iEnd - iBeg)
    else Result := S[P];
  end;

var
  S, W: string;
  X: integer;
begin
  PaintCaret(false);
  BeginUpdate;
  S := FLines.ParaStrings[FCaretY];
  while FCaretX > Length(S) do
    S := S + ' ';
  W := _Trim(GetWordOnPos2(S, FCaretX));
  if W = '' then
  begin
    iBeg := FCaretX + 1;
    iEnd := FCaretX
  end;
  ClearUndo;
  Delete(S, iBeg, iEnd - iBeg);
  Insert(NewString, S, iBeg);
  FLines.InternalParaStrings[FCaretY] := S;
  X := iBeg + Length(NewString) - 1;
  PaintLine(FCaretY, -1, -1);
  SetCaretInternal(X, FCaretY);
  Changed;
  EndUpdate;
  PaintCaret(true);
end;

procedure TCustomMemoEx.InsertText(const Text: string);
var
  S: string;
  P: integer;
  X, Y: integer;
begin
  if Text <> '' then
  begin
    PaintCaret(false);
    BeginUpdate;
    Reline;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);

    TInsertUndo.Create(Self, FCaretX, FCaretY, Text);

    Insert(Text, S, P + 1);
    FLines.SetLockText(S);
    CaretFromPos(P + Length(Text), X, Y);
    SetCaretInternal(X, Y);
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TCustomMemoEx.InsertTextAtCurrentPos(const AText: string);
var
  S: string;
begin
  BeginUpdate;
  S := ExpandTabs(AdjustLineBreaks(AText));
  if Assigned(FOnTextInsert) then FOnTextInsert(Self, S);
  ClearSelection;
  InsertText(S);
  EndUpdate;
  scbVert.Position := FTopRow;
end;

procedure TCustomMemoEx.ClipBoardPaste;
begin
  PasteFromClipboard;
end;

procedure TCustomMemoEx.ClipBoardCut;
begin
  CutToClipboard;
end;

procedure TCustomMemoEx.DeleteSelected;
begin
  ClearSelection;
end;

procedure TCustomMemoEx.SetGutterWidth(AWidth: integer);
begin
  if FGutterWidth <> AWidth then
  begin
    FGutterWidth := AWidth;
    UpdateEditorSize;
    Invalidate;
  end;
end;

procedure TCustomMemoEx.SetGutterColor(AColor: TColor);
begin
  if FGutterColor <> AColor then
  begin
    FGutterColor := AColor;
    FGutter.Invalidate;
  end;
end;


procedure TCustomMemoEx.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;


function TCustomMemoEx.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TCustomMemoEx.SetLines(ALines: TStrings);
begin
  if ALines <> nil then FLines.Assign(ALines);
  ClearUndo;
end;

procedure TCustomMemoEx.TextAllChanged;
begin
  TextAllChangedInternal(true);
end;

procedure TCustomMemoEx.TextAllChangedInternal(const Unselect: Boolean);
begin
  if Unselect then FSelectedText := false;
  UpdateEditorSize(false);
  if (Showing) and (FUpdateLock = 0) then Invalidate;
end;

procedure TCustomMemoEx.SetCols(ACols: integer);
begin
  if FCols <> ACols then
  begin
    FCols := Max(ACols, 1);
    if FCols > FVisibleColCount then
    begin
      scbHorz.Max := FCols - 1;
      scbHorz.Min := 0;
    end
    else
      begin
        scbHorz.Min := 0;
        scbHorz.Max := 0;
      end;
    scbHorz.LargeChange := FVisibleColCount - 1;
    scbHorz.Page := FVisibleColCount;
  end;
end;

procedure TCustomMemoEx.SetRows(ARows: integer);
begin
  if FRows <> ARows then
  begin
    FRows := Max(ARows, 1);
    if FRows > FVisibleRowCount then
    begin
      scbVert.Max := FRows - 1;
      scbVert.Min := 0;
    end
    else
      begin
        scbVert.Min := 0;
        scbVert.Max := 0;
      end;
    scbVert.LargeChange := Max(1, FVisibleRowCount - 1);
    scbVert.Page := FVisibleRowCount;
  end;
end;

procedure TCustomMemoEx.SetLeftTop(ALeftCol, ATopRow: integer);
begin
  if ALeftCol < 0 then ALeftCol := 0;
  if (FLeftCol <> ALeftCol) then
  begin
    scbHorz.Position := ALeftCol;
    Scroll(false, ALeftCol);
  end;
  if ATopRow < 0 then ATopRow := 0;
  if (FTopRow <> ATopRow) then
  begin
    scbVert.Position := ATopRow;
    Scroll(true, ATopRow);
  end;
end;

procedure TCustomMemoEx.SetScrollBars(Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
    UpdateEditorSize;
  end;
end;

procedure TCustomMemoEx.SetRightMarginVisible(Value: boolean);
begin
  if FRightMarginVisible <> Value then
  begin
    FRightMarginVisible := Value;
    Invalidate;
  end;
end;

procedure TCustomMemoEx.SetRightMargin(Value: integer);
begin
  if FRightMargin <> Value then
  begin
    if Value < 8 then raise EInvalidRightMarginValue.Create('Invalid right margin value');
    FRightMargin := Value;
    FRealRightMargin := FRightMargin;
  end;
end;

procedure TCustomMemoEx.SetRightMarginColor(Value: TColor);
begin
  if FRightMarginColor <> Value then
  begin
    FRightMarginColor := Value;
    Invalidate;
  end;
end;

function TCustomMemoEx.ExpandTabs(const S: string): string;
var
  i: integer;
  Sp: string;
begin
  if Pos(#9, S) > 0 then
  begin
    Sp := Spaces(GetDefTabStop(0, true));
    Result := '';
    for i := 1 to Length(S) do
      if S[i] = #9 then Result := Result + Sp
      else Result := Result + S[i];
  end
  else Result := S;
end;

procedure TCustomMemoEx.Changed;
begin
  FModified := true;
  FPEditBuffer := nil;
  if Assigned(FOnChange) then FOnChange(Self);
  StatusChanged;
end;

procedure TCustomMemoEx.StatusChanged;
begin
  if not ((csDestroying in ComponentState) or (csLoading in ComponentState)) then
    if Assigned(FOnChangeStatus) then FOnChangeStatus(Self);
end;

{
  Convert text offset to the caret coordinates.
}
procedure TCustomMemoEx.CaretFromPos(const Pos: integer; var X, Y: integer);
var
  i, j, k: integer;
begin
  k := 0;
  X := -1;
  Y := -1;
  for i := 0 to FLines.Count - 1 do
  begin
    for j := 0 to FLines.Paragraphs[i].FCount - 1 do
    begin
      inc(Y);
      inc(k, length(FLines.Paragraphs[i].FStrings[j]));
      if k >= Pos then
      begin
        X := Pos - (k - length(FLines.Paragraphs[i].FStrings[j]));
        exit;
      end;
    end;
    inc(k, 2);
  end;
  Y := FLines.ParaLineCount - 1;
  if Y >= 0 then X := length(FLines.ParaStrings[Y]);
end;

{
  Convert caret coordinates to text offset.
}
function TCustomMemoEx.PosFromCaret(const X, Y: integer): integer;
var
  i, j, k, z: integer;
begin
  if Y > FLines.ParaLineCount - 1 then Result := length(FLines.Text)
  else
    if Y < 0 then Result := -1
    else
      begin
        Result := 0;
        k := 0;
        for i := 0 to FLines.Count - 1 do
          if k + (FLines.Paragraphs[i].FCount - 1) < Y then
          begin
            inc(Result, length(FLines[i]) + 2);
            inc(k, FLines.Paragraphs[i].FCount);
          end
          else
            begin
              for j := 0 to FLines.Paragraphs[i].FCount - 1 do
              begin
                z := length(FLines.Paragraphs[i].FStrings[j]);
                if k + j < Y then inc(Result, z)
                else
                  begin
                    if z > X then z := X;
                    inc(Result, z);
                    break;
                  end;
              end;
              break;
            end;
      end;
end;

function TCustomMemoEx.PosFromMouse(const X, Y: integer): integer;
var
  X1, Y1: integer;
begin
  Mouse2Caret(X, Y, X1, Y1);
  if (X1 < 0) or (Y1 < 0) then Result := -1
  else Result := PosFromCaret(X1, Y1);
end;

function TCustomMemoEx.GetTextLen: integer;
begin
  Result := Length(FLines.Text);
end;

function TCustomMemoEx.GetSelStart: integer;
begin
  if FSelectedText then Result := PosFromCaret(GetRealOffs(FSelBegX, FSelBegY), FSelBegY) + 1
  else Result := PosFromCaret(GetRealOffs(FCaretX, FCaretY), FCaretY) + 1;
end;

procedure TCustomMemoEx.SetSelStart(const ASelStart: integer);
begin
  FSelectedText := true;
  CaretFromPos(ASelStart - 1, FSelBegX, FSelBegY);
  SetCaretInternal(FSelBegX, FSelBegY);
  SetSelLength(0);
  MakeRowVisible(FSelBegY);
end;

procedure TCustomMemoEx.MakeRowVisible(ARow: integer);
begin
  if (ARow < FTopRow) or (ARow > FLastVisibleRow) then
  begin
    ARow := FCaretY - Trunc(VisibleRowCount / 2);
    if ARow < 0 then ARow := 0;
    SetLeftTop(FLeftCol, ARow);
  end;
end;

function TCustomMemoEx.GetSelLength: integer;
begin
  Result := Length(GetSelText);
end;

procedure TCustomMemoEx.SetSelLength(const ASelLength: integer);
begin
  FSelectedText := ASelLength > 0;
  CaretFromPos(SelStart + ASelLength - 1, FSelEndX, FSelEndY);
  FUpdateSelBegY := FSelBegY;
  FUpdateSelEndY := FSelEndY;
  SetCaretInternal(FSelEndX, FSelEndY);
  Invalidate;
end;

procedure TCustomMemoEx.SetLockText(const Text: string);
begin
  FLines.SetLockText(Text);
end;

procedure TCustomMemoEx.GutterPaint(Canvas: TCanvas);
begin
  Canvas.Font.Assign(FGutter.FFont);
  if Assigned(FOnPaintGutter) then FOnPaintGutter(Self, Canvas);
end;

procedure TCustomMemoEx.SetMode(index: integer; Value: boolean);
var
  PB: ^boolean;
begin
  case index of
    0: PB := @FInsertMode;
  else {1 :}
    PB := @FReadOnly;
  end;
  if PB^ <> Value then
  begin
    PB^ := Value;
    if index = 1 then Invalidate2;
    StatusChanged;
  end;
end;

function TCustomMemoEx.GetWordOnCaret: string;
begin
  Result := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX);
end;

function TCustomMemoEx.GetTabStop(const X, Y: integer; const What: TTabStop;
  const Next: Boolean): integer;

  procedure UpdateTabStops;
  var
    S: string;
    j, i, k: integer;

    function ProcessString: boolean;
    begin
      FLines.CheckLength(S);
      Result := false;
      if (What = tsTabStop) and (length(S) > 0) then FTabPos[length(S) - 1] := true;
      while i <= length(S) do
      begin
        if S[i] = ' ' then
        begin
          FTabPos[i - 1] := true;
          if (What = tsTabStop) and (i >= X) then Result := true;
        end;
        inc(i);
      end;
    end;

  begin
    FillChar(FTabPos[0], Max_X, false);
    if (What = tsTabStop) and (FSmartTab) then
    begin
      j := 1;
      i := 1;
      while Y - j >= 0 do
      begin
        S := _TrimRight(FLines.ParaStrings[Y - j]);
        if ProcessString then break;
        if i >= Max_X div 4 then break;
        if j >= FVisibleRowCount * 2 then break;
        inc(j);
      end;
    end
    else
      if What = tsAutoIndent then
      begin
        FLines.Index2ParaIndex(Y, j, i);
        k := 0;
        while j - k >= 0 do
        begin
          S := FLines[j - k];
          i := 1;
          if S > '' then
          begin
            ProcessString;
            break;
          end;
          if k >= FVisibleRowCount * 2 then break;
          inc(k);
        end;
      end;
  end;

var
  i: integer;
begin
  UpdateTabStops;
  Result := X;
  if Next then
  begin
    for i := X + 1 to High(FTabPos) do
      if (not FTabPos[i - 1]) and (What = tsAutoIndent) then
      begin
        Result := i - 1;
        exit;
      end
      else
        if (not FTabPos[i]) and (i > 0) then
          if FTabPos[i - 1] then
          begin
            Result := i;
            Exit;
          end;
    if Result = X then
      Result := GetDefTabStop(X, true);
  end
  else
    if Result = X then
      Result := GetDefTabStop(X, false);
end;

{
  Get default tab stop position depending on current.
}
function TCustomMemoEx.GetDefTabStop(const X: integer; const Next: Boolean): integer;
var
  i: integer;
begin
  i := FTabSize;
  if i = 0 then Result := X
  else
    if i > X then Result := i
    else
      if X mod i = 0 then Result := X + i
      else
        Result := ((X div i) + 1) * i;
end;

function TCustomMemoEx.GetBackStop(const X, Y: integer): integer;

  procedure UpdateBackStops;
  var
    S: string;
    j, i, k: integer;
  begin
    j := 1;
    i := X - 1;
    FillChar(FTabPos[0], Max_X, false);
    FTabPos[0] := true;
    while Y - j >= 0 do
    begin
      S := FLines.ParaStrings[Y - j];
      for k := 1 to Min(Length(S), i) do { Iterate }
        if S[k] <> ' ' then
        begin
          i := k;
          FTabPos[i - 1] := true;
          Break;
        end;
      if i = 1 then Break;
      if j >= FVisibleRowCount * 2 then Break;
      inc(j);
    end;
  end;

var
  i: integer;
  S: string;
begin
  Result := X - 1;
  S := _TrimRight(FLines.ParaStrings[Y]);
  if (_Trim(Copy(S, 1, X)) = '') and
    ((X + 1 > Length(S)) or (S[X + 1] <> ' ')) then
  begin
    UpdateBackStops;
    for i := X downto 0 do
      if FTabPos[i] then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

procedure TCustomMemoEx.BeginCompound;
begin
  inc(FCompound);
  TBeginCompoundUndo.Create(Self);
end;

procedure TCustomMemoEx.EndCompound;
begin
  TEndCompoundUndo.Create(Self);
  dec(FCompound);
end;

procedure TCustomMemoEx.BeginRecord;
begin
  FMacro := '';
  FRecording := true;
  StatusChanged;
end;

procedure TCustomMemoEx.EndRecord(var AMacro: TMacro);
begin
  FRecording := false;
  AMacro := FMacro;
  StatusChanged;
end;

procedure TCustomMemoEx.PlayMacro(const AMacro: TMacro);
var
  i: integer;
begin
  BeginUpdate;
  BeginCompound;
  try
    i := 1;
    while i < Length(AMacro) do
    begin
      Command(byte(AMacro[i]) + byte(AMacro[i + 1]) shl 8);
      inc(i, 2);
    end;
  finally
    EndCompound;
    EndUpdate;
  end;
end;

procedure TCustomMemoEx.CantUndo;
begin
  FUndoBuffer.Clear;
end;

procedure TCustomMemoEx.CompletionIdentifier(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomMemoEx.CompletionTemplate(var Cancel: boolean);
begin
  {abstract}
end;

procedure TCustomMemoEx.DoCompletionIdentifier(var Cancel: boolean);
begin
  CompletionIdentifier(Cancel);
  if Assigned(FOnCompletionIdentifier) then FOnCompletionIdentifier(Self, Cancel);
end;

procedure TCustomMemoEx.DoCompletionTemplate(var Cancel: boolean);
begin
  CompletionTemplate(Cancel);
  if Assigned(FOnCompletionTemplate) then FOnCompletionTemplate(Self, Cancel);
end;

function TCustomMemoEx.DoPreprocessCompletion(const ID, OldText: string): string;
begin
  if Assigned(FOnPreprocessCompletion) then Result := FOnPreprocessCompletion(Self, ID, OldText)
  else Result := OldText;
end;

{ TIEditReader support }

procedure TCustomMemoEx.ValidateEditBuffer;
begin
  if FPEditBuffer = nil then
  begin
    FEditBuffer := Lines.Text;
    FPEditBuffer := PChar(FEditBuffer);
    FEditBufferSize := Length(FEditBuffer);
  end;
end;

function TCustomMemoEx.GetText(Position: longint; Buffer: PChar;
  Count: longint): longint;
begin
  ValidateEditBuffer;
  if Position <= FEditBufferSize then
  begin
    Result := Min(FEditBufferSize - Position, Count);
    Move(FPEditBuffer[Position], Buffer[0], Result);
  end
  else
    Result := 0;
end;

procedure TCustomMemoEx.SetWordWrap(Value: boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    FLines.Reformat;
    ClearUndo;
  end;
end;

procedure TCustomMemoEx.SetStripInvisible(Value: boolean);
begin
  if Value <> FStripInvisible then
  begin
    FStripInvisible := Value;
    if FReadOnly then Invalidate2;
  end;
end;

{
  Ïîëíàÿ ïåðåðèñîâêà ñ ïåðåñ÷åòîì âñåõ àòðèáóòîâ.
}
procedure TCustomMemoEx.Invalidate2;
var
  i: integer;
begin
  for i := 0 to FLines.FCount - 1 do FLines.FList^[i].FChanged := true;
  Invalidate;
end;

procedure TCustomMemoEx.InvalidateGutter;
begin
  FGutter.Invalidate;
end;

procedure TCustomMemoEx.InvalidateLine(Index: integer);
var
  p, pi: integer;
begin
  FLines.Index2ParaIndex(Index, p, pi);
  FLines.FList^[p].FChanged := true;
end;

procedure TCustomMemoEx.FontChanged(Sender: TObject);
begin
  UpdateEditorSize;
  Invalidate2;
end;

{
  OnAfterLoad event
}
function TCustomMemoEx.GetAfterLoad: TNotifyEvent;
begin
  Result := FLines.FOnAfterLoad;
end;

procedure TCustomMemoEx.SetAfterLoad(Value: TNotifyEvent);
begin
  FLines.FOnAfterLoad := Value;
end;

{
  OnBeforeSave event
}
function TCustomMemoEx.GetBeforeSave: TNotifyEvent;
begin
  Result := FLines.FOnBeforeSave;
end;

procedure TCustomMemoEx.SetBeforeSave(Value: TNotifyEvent);
begin
  FLines.FOnBeforeSave := Value;
end;

procedure TCustomMemoEx.SetSelectedText(Value: boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    SelectionChanged;
  end;
end;

function TCustomMemoEx.GetPlainText: string;
begin
  Result := FLines.Text;
end;

procedure TCustomMemoEx.SetPlainText(const AValue: string);
begin
  if AValue <> FLines.Text then FLines.Text := AValue;
end;

function TCustomMemoEx.GetCanUndo: boolean;
begin
  Result := FUndoBuffer.FPtr >= 0;
end;

procedure TCustomMemoEx.Clear;
begin
  FLines.Clear;
  ClearUndo;
end;

procedure TCustomMemoEx.SetCaretPos(const AValue: TPoint);
begin
  SetCaret(AValue.X, AValue.Y);
end;

procedure TCustomMemoEx.ClearSelection;
var
  S, S1: string;
  iBeg, X, Y: integer;
begin
  if (FSelected) and (not FReadOnly) then
  begin
    S1 := GetSelText;
    FSelectedText := false;
    if S1 = '' then exit;
    PaintCaret(false);
    S := FLines.Text;
    iBeg := PosFromCaret(FSelBegX, FSelBegY);

    TDeleteSelectedUndo.Create(Self, FCaretX, FCaretY, S1, FSelBlock,
      FSelBegX, FSelBegY, FSelEndX, FSelEndY, iBeg);

    Delete(S, iBeg + 1, length(S1));
    FLines.SetLockText(S);
    CaretFromPos(iBeg, X, Y);
    SetCaretInternal(X, Y);
    Changed;
    UpdateEditorSize(false);
    if FUpdateLock = 0 then Invalidate;
    PaintCaret(true);
  end;
end;

procedure TCustomMemoEx.ClearUndo;
begin
  FUndoBuffer.Clear;
end;

procedure TCustomMemoEx.CopyToClipboard;
begin
  if not FSelBlock then
    _CopyToClipboard(Handle, GetSelText, Font.Charset);
end;

procedure TCustomMemoEx.CutToClipboard;
begin
  if not FReadOnly then
  begin
    CopyToClipboard;
    ClearSelection;
  end;
end;

procedure TCustomMemoEx.PasteFromClipboard;
var
  ClipS: string;
begin
  if not FReadOnly then
  begin
    ClipS := _PasteFromClipboard(Handle, Font.Charset, false);
    InsertTextAtCurrentPos(ClipS);
  end;
end;

procedure TCustomMemoEx.SelectAll;
begin
  Command(ecSelAll);
end;

procedure TCustomMemoEx.Undo;
begin
  Command(ecUndo);
end;

function TCustomMemoEx.GetCaretPos: TPoint;
begin
  Result.X := FCaretX;
  Result.Y := FCaretY;
end;



{ TEditKey }

constructor TEditKey.Create(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Command := ACommand;
end;

constructor TEditKey.Create2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  Key1 := AKey1;
  Shift1 := AShift1;
  Key2 := AKey2;
  Shift2 := AShift2;
  Command := ACommand;
end;

{ TKeyboard }

constructor TKeyboard.Create;
begin
  List := TList.Create;
end;

destructor TKeyboard.Destroy;
begin
  Clear;
  List.Free;
end;

procedure TKeyboard.Add(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState);
begin
  List.Add(TEditKey.Create(ACommand, AKey1, AShift1));
end;

procedure TKeyboard.Add2(const ACommand: TEditCommand; const AKey1: word;
  const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState);
begin
  List.Add(TEditKey.Create2(ACommand, AKey1, AShift1, AKey2, AShift2));
end;

procedure TKeyboard.Clear;
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free;
  List.Clear;
end;

function TKeyboard.Command(const AKey: word; const AShift: TShiftState):
  TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey) and (Shift1 = AShift) then
      begin
        if Key2 = 0 then
          Result := Command
        else
          Result := twoKeyCommand;
        Exit;
      end;
end;

function TKeyboard.Command2(const AKey1: word; const AShift1: TShiftState;
  const AKey2: word; const AShift2: TShiftState): TEditCommand;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to List.Count - 1 do
    with TEditKey(List[i]) do
      if (Key1 = AKey1) and (Shift1 = AShift1) and
        (Key2 = AKey2) and (Shift2 = AShift2) then
      begin
        Result := Command;
        Exit;
      end;
end;

procedure TKeyboard.SetDefLayout;
begin
  Clear;
  Add(ecLeft, VK_LEFT, []);
  Add(ecRight, VK_RIGHT, []);
  Add(ecUp, VK_UP, []);
  Add(ecDown, VK_DOWN, []);
  Add(ecSelLeft, VK_LEFT, [ssShift]);
  Add(ecSelRight, VK_RIGHT, [ssShift]);
  Add(ecSelUp, VK_UP, [ssShift]);
  Add(ecSelDown, VK_DOWN, [ssShift]);
  Add(ecBeginLine, VK_HOME, []);
  Add(ecSelBeginLine, VK_HOME, [ssShift]);
  Add(ecBeginDoc, VK_PRIOR, [ssCtrl]);
  Add(ecSelBeginDoc, VK_HOME, [ssCtrl, ssShift]);
  Add(ecEndLine, VK_END, []);
  Add(ecSelEndLine, VK_END, [ssShift]);
  Add(ecEndDoc, VK_NEXT, [ssCtrl]);
  Add(ecSelEndDoc, VK_END, [ssCtrl, ssShift]);
  Add(ecPrevWord, VK_LEFT, [ssCtrl]);
  Add(ecNextWord, VK_RIGHT, [ssCtrl]);
  Add(ecSelPrevWord, VK_LEFT, [ssCtrl, ssShift]);
  Add(ecSelNextWord, VK_RIGHT, [ssCtrl, ssShift]);
  Add(ecSelAll, ord('A'), [ssCtrl, ssShift]);

  Add(ecWindowTop, VK_HOME, [ssCtrl]);
  Add(ecWindowBottom, VK_END, [ssCtrl]);
  Add(ecPrevPage, VK_PRIOR, []);
  Add(ecNextPage, VK_NEXT, []);
  Add(ecSelPrevPage, VK_PRIOR, [ssShift]);
  Add(ecSelNextPage, VK_NEXT, [ssShift]);
  Add(ecScrollLineUp, VK_UP, [ssCtrl]);
  Add(ecScrollLineDown, VK_DOWN, [ssCtrl]);

  Add(ecChangeInsertMode, VK_INSERT, []);

  Add(ecInsertPara, VK_RETURN, []);
  Add(ecBackspace, VK_BACK, []);
  Add(ecBackspace, VK_BACK, [ssShift]);
  Add(ecBackword, VK_BACK, [ssCtrl]);
  Add(ecDelete, VK_DELETE, []);
  Add(ecTab, VK_TAB, []);
  Add(ecBackTab, VK_TAB, [ssShift]);
  Add(ecDeleteSelected, VK_DELETE, [ssCtrl]);
  Add(ecClipboardCopy, VK_INSERT, [ssCtrl]);
  Add(ecClipboardCut, VK_DELETE, [ssShift]);
  Add(ecClipBoardPaste, VK_INSERT, [ssShift]);

  Add(ecClipboardCopy, ord('C'), [ssCtrl]);
  Add(ecClipboardCut, ord('X'), [ssCtrl]);
  Add(ecClipBoardPaste, ord('V'), [ssCtrl]);


  Add(ecSetBookmark0, ord('0'), [ssCtrl, ssShift]);
  Add(ecSetBookmark1, ord('1'), [ssCtrl, ssShift]);
  Add(ecSetBookmark2, ord('2'), [ssCtrl, ssShift]);
  Add(ecSetBookmark3, ord('3'), [ssCtrl, ssShift]);
  Add(ecSetBookmark4, ord('4'), [ssCtrl, ssShift]);
  Add(ecSetBookmark5, ord('5'), [ssCtrl, ssShift]);
  Add(ecSetBookmark6, ord('6'), [ssCtrl, ssShift]);
  Add(ecSetBookmark7, ord('7'), [ssCtrl, ssShift]);
  Add(ecSetBookmark8, ord('8'), [ssCtrl, ssShift]);
  Add(ecSetBookmark9, ord('9'), [ssCtrl, ssShift]);

  Add(ecGotoBookmark0, ord('0'), [ssCtrl]);
  Add(ecGotoBookmark1, ord('1'), [ssCtrl]);
  Add(ecGotoBookmark2, ord('2'), [ssCtrl]);
  Add(ecGotoBookmark3, ord('3'), [ssCtrl]);
  Add(ecGotoBookmark4, ord('4'), [ssCtrl]);
  Add(ecGotoBookmark5, ord('5'), [ssCtrl]);
  Add(ecGotoBookmark6, ord('6'), [ssCtrl]);
  Add(ecGotoBookmark7, ord('7'), [ssCtrl]);
  Add(ecGotoBookmark8, ord('8'), [ssCtrl]);
  Add(ecGotoBookmark9, ord('9'), [ssCtrl]);

  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), []);
  Add2(ecSetBookmark0, ord('K'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), []);
  Add2(ecSetBookmark1, ord('K'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), []);
  Add2(ecSetBookmark2, ord('K'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), []);
  Add2(ecSetBookmark3, ord('K'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), []);
  Add2(ecSetBookmark4, ord('K'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), []);
  Add2(ecSetBookmark5, ord('K'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), []);
  Add2(ecSetBookmark6, ord('K'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), []);
  Add2(ecSetBookmark7, ord('K'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), []);
  Add2(ecSetBookmark8, ord('K'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), []);
  Add2(ecSetBookmark9, ord('K'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), []);
  Add2(ecGotoBookmark0, ord('Q'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), []);
  Add2(ecGotoBookmark1, ord('Q'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), []);
  Add2(ecGotoBookmark2, ord('Q'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), []);
  Add2(ecGotoBookmark3, ord('Q'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), []);
  Add2(ecGotoBookmark4, ord('Q'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), []);
  Add2(ecGotoBookmark5, ord('Q'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), []);
  Add2(ecGotoBookmark6, ord('Q'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), []);
  Add2(ecGotoBookmark7, ord('Q'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), []);
  Add2(ecGotoBookmark8, ord('Q'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), []);
  Add2(ecGotoBookmark9, ord('Q'), [ssCtrl], ord('9'), [ssCtrl]);

  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), [ssCtrl]);
  Add2(ecInsertMacro0, ord('S'), [ssCtrl], ord('0'), []);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), [ssCtrl]);
  Add2(ecInsertMacro1, ord('S'), [ssCtrl], ord('1'), []);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), [ssCtrl]);
  Add2(ecInsertMacro2, ord('S'), [ssCtrl], ord('2'), []);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), [ssCtrl]);
  Add2(ecInsertMacro3, ord('S'), [ssCtrl], ord('3'), []);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), [ssCtrl]);
  Add2(ecInsertMacro4, ord('S'), [ssCtrl], ord('4'), []);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), [ssCtrl]);
  Add2(ecInsertMacro5, ord('S'), [ssCtrl], ord('5'), []);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), [ssCtrl]);
  Add2(ecInsertMacro6, ord('S'), [ssCtrl], ord('6'), []);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), [ssCtrl]);
  Add2(ecInsertMacro7, ord('S'), [ssCtrl], ord('7'), []);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), [ssCtrl]);
  Add2(ecInsertMacro8, ord('S'), [ssCtrl], ord('8'), []);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), [ssCtrl]);
  Add2(ecInsertMacro9, ord('S'), [ssCtrl], ord('9'), []);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecInsertMacroA, ord('S'), [ssCtrl], ord('A'), []);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecInsertMacroB, ord('S'), [ssCtrl], ord('B'), []);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecInsertMacroC, ord('S'), [ssCtrl], ord('C'), []);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecInsertMacroD, ord('S'), [ssCtrl], ord('D'), []);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecInsertMacroE, ord('S'), [ssCtrl], ord('E'), []);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecInsertMacroF, ord('S'), [ssCtrl], ord('F'), []);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecInsertMacroG, ord('S'), [ssCtrl], ord('G'), []);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecInsertMacroH, ord('S'), [ssCtrl], ord('H'), []);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecInsertMacroI, ord('S'), [ssCtrl], ord('I'), []);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecInsertMacroJ, ord('S'), [ssCtrl], ord('J'), []);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecInsertMacroK, ord('S'), [ssCtrl], ord('K'), []);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecInsertMacroL, ord('S'), [ssCtrl], ord('L'), []);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecInsertMacroM, ord('S'), [ssCtrl], ord('M'), []);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecInsertMacroN, ord('S'), [ssCtrl], ord('N'), []);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecInsertMacroO, ord('S'), [ssCtrl], ord('O'), []);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecInsertMacroP, ord('S'), [ssCtrl], ord('P'), []);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecInsertMacroQ, ord('S'), [ssCtrl], ord('Q'), []);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecInsertMacroR, ord('S'), [ssCtrl], ord('R'), []);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecInsertMacroS, ord('S'), [ssCtrl], ord('S'), []);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecInsertMacroT, ord('S'), [ssCtrl], ord('T'), []);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecInsertMacroU, ord('S'), [ssCtrl], ord('U'), []);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecInsertMacroV, ord('S'), [ssCtrl], ord('V'), []);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecInsertMacroW, ord('S'), [ssCtrl], ord('W'), []);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecInsertMacroX, ord('S'), [ssCtrl], ord('X'), []);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecInsertMacroY, ord('S'), [ssCtrl], ord('Y'), []);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecInsertMacroZ, ord('S'), [ssCtrl], ord('Z'), []);

  Add(ecUndo, ord('Z'), [ssCtrl]);
  Add(ecUndo, VK_BACK, [ssAlt]);

  Add(ecCompletionIdentifiers, VK_SPACE, [ssCtrl]);
  Add(ecCompletionTemplates, ord('J'), [ssCtrl]);

  { cursor movement - default and classic }
  Add2(ecBeginDoc, ord('Q'), [ssCtrl], ord('R'), []);
  Add2(ecEndDoc, ord('Q'), [ssCtrl], ord('C'), []);

  Add2(ecBeginLine, ord('Q'), [ssCtrl], ord('S'), []);
  Add2(ecEndLine, ord('Q'), [ssCtrl], ord('D'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('E'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('X'), []);

  Add2(ecWindowTop, ord('Q'), [ssCtrl], ord('T'), []);
  Add2(ecWindowBottom, ord('Q'), [ssCtrl], ord('U'), []);

  Add(ecDeleteWord, ord('T'), [ssCtrl]);
  Add(ecInsertPara, ord('N'), [ssCtrl]);
  Add(ecDeleteLine, ord('Y'), [ssCtrl]);

  Add2(ecSelWord, ord('K'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecToUpperCase, ord('K'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecToLowerCase, ord('K'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecChangeCase, ord('O'), [ssCtrl], ord('U'), [ssCtrl]);

  Add2(ecInsertBlock, ord('K'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecSaveBlock, ord('K'), [ssCtrl], ord('W'), [ssCtrl]);

  Add(ecRecordMacro, ord('R'), [ssCtrl, ssShift]);
  Add(ecPlayMacro, ord('P'), [ssCtrl, ssShift]);

  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), [ssCtrl]);
  Add2(ecBlockOpA, ord('B'), [ssCtrl], ord('A'), []);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), [ssCtrl]);
  Add2(ecBlockOpB, ord('B'), [ssCtrl], ord('B'), []);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), [ssCtrl]);
  Add2(ecBlockOpC, ord('B'), [ssCtrl], ord('C'), []);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), [ssCtrl]);
  Add2(ecBlockOpD, ord('B'), [ssCtrl], ord('D'), []);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), [ssCtrl]);
  Add2(ecBlockOpE, ord('B'), [ssCtrl], ord('E'), []);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), [ssCtrl]);
  Add2(ecBlockOpF, ord('B'), [ssCtrl], ord('F'), []);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), [ssCtrl]);
  Add2(ecBlockOpG, ord('B'), [ssCtrl], ord('G'), []);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), [ssCtrl]);
  Add2(ecBlockOpH, ord('B'), [ssCtrl], ord('H'), []);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), [ssCtrl]);
  Add2(ecBlockOpI, ord('B'), [ssCtrl], ord('I'), []);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), [ssCtrl]);
  Add2(ecBlockOpJ, ord('B'), [ssCtrl], ord('J'), []);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), [ssCtrl]);
  Add2(ecBlockOpK, ord('B'), [ssCtrl], ord('K'), []);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), [ssCtrl]);
  Add2(ecBlockOpL, ord('B'), [ssCtrl], ord('L'), []);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), [ssCtrl]);
  Add2(ecBlockOpM, ord('B'), [ssCtrl], ord('M'), []);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), [ssCtrl]);
  Add2(ecBlockOpN, ord('B'), [ssCtrl], ord('N'), []);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), [ssCtrl]);
  Add2(ecBlockOpO, ord('B'), [ssCtrl], ord('O'), []);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), [ssCtrl]);
  Add2(ecBlockOpP, ord('B'), [ssCtrl], ord('P'), []);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), [ssCtrl]);
  Add2(ecBlockOpQ, ord('B'), [ssCtrl], ord('Q'), []);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), [ssCtrl]);
  Add2(ecBlockOpR, ord('B'), [ssCtrl], ord('R'), []);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), [ssCtrl]);
  Add2(ecBlockOpS, ord('B'), [ssCtrl], ord('S'), []);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), [ssCtrl]);
  Add2(ecBlockOpT, ord('B'), [ssCtrl], ord('T'), []);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), [ssCtrl]);
  Add2(ecBlockOpU, ord('B'), [ssCtrl], ord('U'), []);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), [ssCtrl]);
  Add2(ecBlockOpV, ord('B'), [ssCtrl], ord('V'), []);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), [ssCtrl]);
  Add2(ecBlockOpW, ord('B'), [ssCtrl], ord('W'), []);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), [ssCtrl]);
  Add2(ecBlockOpX, ord('B'), [ssCtrl], ord('X'), []);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), [ssCtrl]);
  Add2(ecBlockOpY, ord('B'), [ssCtrl], ord('Y'), []);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), [ssCtrl]);
  Add2(ecBlockOpZ, ord('B'), [ssCtrl], ord('Z'), []);
end;

procedure RedoNotImplemented;
begin
  raise EMemoExError.Create('Redo not yet implemented');
end;

{ TUndoBuffer }

constructor TUndoBuffer.Create;
begin
  FCancelUndo := false;
  FPtr := -1;
end;

procedure TUndoBuffer.Add(var AUndo: TUndo);
begin
  if InUndo then exit;
  if FCancelUndo then Clear
  else FMemoEx.StatusChanged;
  while (Count > 0) and (FPtr < Count - 1) do
  begin
    TUndo(Items[FPtr + 1]).Free;
    inherited Delete(FPtr + 1);
  end;
  inherited Add(AUndo);
  FPtr := Count - 1;
end;

procedure TUndoBuffer.Undo;
var
  UndoClass: TClass;
  Compound: integer;
begin
  InUndo := true;
  try
    if LastUndo <> nil then
    begin
      Compound := 0;
      UndoClass := LastUndo.ClassType;
      while (LastUndo <> nil) and
        ((UndoClass = LastUndo.ClassType) or
        (LastUndo is TDeleteTrailUndo) or
        (LastUndo is TReLineUndo) or
        (Compound > 0)) do
      begin
        if LastUndo.ClassType = TBeginCompoundUndo then
        begin
          dec(Compound);
          UndoClass := nil;
        end
        else
          if LastUndo.ClassType = TEndCompoundUndo then inc(Compound);
        LastUndo.Undo;
        dec(FPtr);
        if (UndoClass = TDeleteTrailUndo) or
          (UndoClass = TReLineUndo) then
          UndoClass := LastUndo.ClassType;
        if not FMemoEx.FGroupUndo then break;
        // FMemoEx.Paint; {DEBUG !!!!!!!!!}
      end;
      if FMemoEx.FUpdateLock = 0 then
      begin
        FMemoEx.TextAllChangedInternal(false);
        FMemoEx.Changed;
      end;
    end;
  finally
    InUndo := false;
  end;
end;

procedure TUndoBuffer.Redo;
begin
  { DEBUG !!!! }
  inc(FPtr);
  LastUndo.Redo;
end;

procedure TUndoBuffer.Clear;
begin
  while Count > 0 do
  begin
    TUndo(Items[0]).Free;
    inherited Delete(0);
  end;
  FCancelUndo := false;
  FMemoEx.StatusChanged;
end;

procedure TUndoBuffer.Delete;
begin
  if Count > 0 then
  begin
    TUndo(Items[Count - 1]).Free;
    inherited Delete(Count - 1);
    FPtr := Count - 1;
  end;
end;

function TUndoBuffer.LastUndo: TUndo;
begin
  if (FPtr >= 0) and (Count > 0) then Result := TUndo(Items[FPtr])
  else Result := nil;
end;

function TUndoBuffer.IsNewGroup(const AUndo: TUndo): boolean;
begin
  Result := (LastUndo = nil) or (LastUndo.ClassType <> AUndo.ClassType)
end;

{ TUndo }

constructor TUndo.Create(const AMemoEx: TCustomMemoEx);
begin
  FMemoEx := AMemoEx;
  UndoBuffer.Add(Self);
end;

function TUndo.UndoBuffer: TUndoBuffer;
begin
  if FMemoEx <> nil then Result := FMemoEx.FUndoBuffer
  else Result := nil;
end;

{ TCaretUndo }

constructor TCaretUndo.Create(const AMemoEx: TCustomMemoEx;
                              const ACaretX, ACaretY: integer);
begin
  inherited Create(AMemoEx);
  FCaretX := ACaretX;
  FCaretY := ACaretY;
end;

procedure TCaretUndo.Undo;
begin
  with UndoBuffer do
  begin
    dec(FPtr);
    while FMemoEx.FGroupUndo and (FPtr >= 0) and not IsNewGroup(Self) do
      dec(FPtr);
    inc(FPtr);
    with TCaretUndo(Items[FPtr]) do
      FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

procedure TCaretUndo.Redo;
begin
  RedoNotImplemented;
end;

{ TInsertUndo }

constructor TInsertUndo.Create(const AMemoEx: TCustomMemoEx;
                               const ACaretX, ACaretY: integer; const AText: string);
var
  i, j: integer;
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FText := AText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, j);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + j;
end;

procedure TInsertUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TInsertUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TInsertUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FMemoEx.FLines.Text;
    iBeg := FOffset;
    Delete(S, iBeg + 1, Length(Text));
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

{ TOverwriteUndo }

constructor TOverwriteUndo.Create(const AMemoEx: TCustomMemoEx;
  const ACaretX, ACaretY: integer; const AOldText, ANewText: string);
var
  i, j: integer;
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FOldText := AOldText;
  FNewText := ANewText;
  AMemoEx.FLines.Caret2Paragraph(ACaretX, ACaretY, i, j);
  FOffset := AMemoEx.FLines.GetParaOffs(i) + j;
end;

procedure TOverwriteUndo.Undo;
var
  S, OldText, NewText: string;
  iBeg: integer;
begin
  NewText := '';
  OldText := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      OldText := TOverwriteUndo(LastUndo).FOldText + OldText;
      NewText := TOverwriteUndo(LastUndo).FNewText + NewText;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TOverwriteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FMemoEx.FLines.Text;
    iBeg := FOffset;
    Delete(S, iBeg + 1, Length(NewText));
    Insert(OldText, S, iBeg + 1);
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

{ TDeleteUndo }

procedure TDeleteUndo.Undo;
var
  X, Y: integer;
  S, Text: string;
  iBeg: integer;
begin
  Text := '';
  X := -1;
  Y := -1;
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      if (X = -1) or (Y = -1) then
      begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      end;
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  if (X <> -1) and (Y <> -1) then
    with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
    begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg, X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    end;
end;

{ TBackspaceUndo }

procedure TBackspaceUndo.Undo;
var
  S, Text: string;
  iBeg: integer;
  X, Y: integer;
begin
  Text := '';
  X := -1;
  Y := -1;
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      if (X = -1) or (Y = -1) then
      begin
        X := TDeleteUndo(LastUndo).FCaretX;
        Y := TDeleteUndo(LastUndo).FCaretY;
      end;
      Text := Text + TDeleteUndo(LastUndo).FText;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  if (X <> -1) and (Y <> -1) then
    with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
    begin
      S := FMemoEx.FLines.Text;
      iBeg := FMemoEx.PosFromCaret(X, Y);
      Insert(Text, S, iBeg + 1);
      FMemoEx.FLines.SetLockText(S);
      FMemoEx.CaretFromPos(iBeg + length(Text), X, Y);
      FMemoEx.SetCaretInternal(X, Y);
    end;
end;

{ TReplaceUndo }

constructor TReplaceUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX, ACaretY:
  integer; const ABeg, AEnd: integer; const AText, ANewText: string);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FBeg := ABeg;
  FEnd := AEnd;
  FText := AText;
  FNewText := ANewText;
end;

procedure TReplaceUndo.Undo;
var
  S: string;
begin
  S := FMemoEx.FLines.Text;
  Delete(S, FBeg, Length(FNewText));
  Insert(FText, S, FBeg);
  FMemoEx.FLines.SetLockText(S);
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
end;

{ TDeleteSelectedUndo }

constructor TDeleteSelectedUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX,
  ACaretY: integer; const AText: string; const ASelBlock: boolean; const ASelBegX, ASelBegY,
  ASelEndX, ASelEndY, ASelOffs: integer);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY, AText);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
  FSelOffs := ASelOffs;
end;

procedure TDeleteSelectedUndo.Undo;
var
  S, Text: string;
begin
  Text := '';
  with UndoBuffer do
  begin
    while (FPtr >= 0) and not IsNewGroup(Self) do
    begin
      Text := TDeleteUndo(LastUndo).FText + Text;
      dec(FPtr);
      if not FMemoEx.FGroupUndo then break;
    end;
    inc(FPtr);
  end;
  with TDeleteUndo(UndoBuffer.Items[UndoBuffer.FPtr]) do
  begin
    S := FMemoEx.FLines.Text;
    Insert(Text, S, FSelOffs + 1);
    FMemoEx.FLines.SetLockText(S);
    FMemoEx.FSelBlock := FSelBlock;
    FMemoEx.FSelBegX := FSelBegX;
    FMemoEx.FSelBegY := FSelBegY;
    FMemoEx.FSelEndX := FSelEndX;
    FMemoEx.FSelEndY := FSelEndY;
    FMemoEx.FSelectedText := Length(FText) > 0;
    FMemoEx.SetCaretInternal(FCaretX, FCaretY);
  end;
end;

{ TSelectUndo }

constructor TSelectUndo.Create(const AMemoEx: TCustomMemoEx; const ACaretX,
  ACaretY: integer; const ASelBlock: boolean; const ASelBegX, ASelBegY, ASelEndX,
  ASelEndY: integer);
begin
  inherited Create(AMemoEx, ACaretX, ACaretY);
  FSelBlock := ASelBlock;
  FSelBegX := ASelBegX;
  FSelBegY := ASelBegY;
  FSelEndX := ASelEndX;
  FSelEndY := ASelEndY;
end;

procedure TSelectUndo.Undo;
begin
  FMemoEx.FSelectedText := (FSelBegX <> FSelEndX) or (FSelBegY <> FSelEndY);
  FMemoEx.FSelBegX := FSelBegX;
  FMemoEx.FSelBegY := FSelBegY;
  FMemoEx.FSelEndX := FSelEndX;
  FMemoEx.FSelEndY := FSelEndY;
  FMemoEx.FSelBlock := FSelBlock;
  FMemoEx.SetCaretInternal(FCaretX, FCaretY);
end;

procedure TBeginCompoundUndo.Undo;
begin
  { nothing }
end;

{ TIndentUndo }

constructor TIndentUndo.Create(const AMemoEx: TCustomMemoEx;
  const AIndentY1, AIndentY2, AIndentSize: integer);
begin
  FIndentY1 := AIndentY1;
  FIndentY2 := AIndentY2;
  FIndentSize := AIndentSize;
  inherited Create(AMemoEx);
end;

procedure TIndentUndo.Undo;
var
  i: integer;
  s: string;
begin
  FMemoEx.FLines.BeginUpdate;
  for i := FIndentY1 to FIndentY2 do
  begin
    s := FMemoEx.FLines[i];
    System.Delete(s, 1, FIndentSize);
    FMemoEx.FLines[i] := s;
  end;
  FMemoEx.FLines.EndUpdate;
end;

{ TUnindentUndo }

constructor TUnindentUndo.Create(const AMemoEx: TCustomMemoEx; const AIndentY, AIndentSize: integer);
begin
  FIndentY := AIndentY;
  FIndentSize := AIndentSize;
  inherited Create(AMemoEx);
end;

procedure TUnindentUndo.Undo;
begin
  FMemoEx.FLines[FIndentY] := Spaces(FIndentSize) + FMemoEx.FLines[FIndentY];
end;

type
  { TMemoExCompletionList }
  TMemoExCompletionList = class(TListBox)
  private
    FTimer: TTimer;
    YY: integer;
    // HintWindow : THintWindow;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure OnTimer(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure DrawItem(Index: integer; Rect: TRect; State: TOwnerDrawState);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TCompletion }

constructor TCompletion.Create2(AMemoEx: TCustomMemoEx);
begin
  inherited Create;
  FMemoEx := AMemoEx;
  FPopupList := TMemoExCompletionList.Create(FMemoEx);
  FItemHeight := FPopupList.ItemHeight;
  FDropDownCount := 6;
  FDropDownWidth := 300;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 800;
  FTimer.OnTimer := OnTimer;
  FIdentifiers := TStringList.Create;
  FTemplates := TStringList.Create;
  FItems := TStringList.Create;
  FAutoChange := TStringList.Create;
  TStringList(FAutoChange).OnChange := AutoChangeChanged;
  FAutoChangeList := TList.Create;
  FDefMode := cmIdentifiers;
  FCaretChar := '|';
  FCRLF := '/n';
  FSeparator := '=';
end;

destructor TCompletion.Destroy;
begin
  inherited Destroy;
  FPopupList.Free;
  FIdentifiers.Free;
  FTemplates.Free;
  FItems.Free;
  FAutoChange.Free;
  ClearAutoChangeList;
  FAutoChangeList.Free;
  FTimer.Free;
end;

function TCompletion.GetItems: TStrings;
begin
  case FMode of
    cmIdentifiers: Result := FIdentifiers;
    cmTemplates: Result := FTemplates;
  else
    Result := nil;
  end;
end;

procedure TCompletion.ReplaceWord(const ANewString: string);
var
  S, S1, W, NewString: string;
  P, X, Y: integer;
  iBeg, iEnd: integer;
  NewCaret, LNum, CX, CY, i: integer;
begin
  with FMemoEx do
  begin
    PaintCaret(false);
    BeginUpdate;
    ReLine;
    S := FLines.Text;
    P := PosFromCaret(FCaretX, FCaretY);
    W := _Trim(GetWordOnPosEx(S, P, iBeg, iEnd));
    LNum := 0;
    if W = '' then
    begin
      iBeg := P + 1;
      iEnd := P
    end;
    CaretFromPos(iBeg, CX, CY);
    if CX < 1 then CX := FCaretX + 1;
    NewString := DoPreprocessCompletion(W, ANewString);
    case FMode of
      cmIdentifiers:
        begin
          S1 := NewString;
          NewCaret := Length(NewString);
        end;
      cmTemplates:
        begin
          S1 := ReplaceWordByPhrase(NewString, FCRLF, #13#10 + Spaces(CX - 1));
          S1 := ReplaceWordByPhrase(S1, FCaretChar, '');
          NewCaret := Pos(FCaretChar, NewString) - 1;
          if NewCaret = -1 then NewCaret := Length(NewString);
          for i := 1 to NewCaret do
            if S1[i] = #13 then inc(LNum);
        end
    else
      raise EMemoExError.Create('Invalid MemoEx Completion Mode');
    end;
    TReplaceUndo.Create(FMemoEx, FCaretX, FCaretY, iBeg, iEnd, W, S1);
    Delete(S, iBeg, iEnd - iBeg);
    Insert(S1, S, iBeg);
    FLines.SetLockText(S);
    CaretFromPos(iBeg - 1 + (CX - 1) * LNum + NewCaret, X, Y);
    SetCaretInternal(X, Y);
    FMemoEx.TextAllChanged; // Invalidate; {!!!}
    Changed;
    EndUpdate;
    PaintCaret(true);
  end;
end;

procedure TCompletion.DoKeyPress(Key: Char);
begin
  if FVisible then
    if HasChar(Key, RAEditorCompletionChars) then SelectItem
    else CloseUp(true)
  else
    if FEnabled then FTimer.Enabled := true;
end;

function TCompletion.DoKeyDown(Key: Word; Shift: TShiftState): boolean;
begin
  Result := true;
  case Key of
    VK_ESCAPE: CloseUp(false);
    VK_RETURN: CloseUp(true);
    VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT:
      FPopupList.Perform(WM_KEYDOWN, Key, 0);
  else
    Result := false;
  end;
end;

procedure TCompletion.DoCompletion(const AMode: TCompletionList);
var
  Eq: boolean;
  Cancel: boolean;
begin
  if FMemoEx.FReadOnly then exit;
  if FPopupList.Visible then CloseUp(false);
  FMode := AMode;
  case FMode of
    cmIdentifiers: DropDown(AMode, true);
    cmTemplates:
      begin
        Cancel := false;
        FMemoEx.DoCompletionTemplate(Cancel);
        if Cancel or (FTemplates.Count = 0) then exit;
        MakeItems;
        FindSelItem(Eq);
        if Eq then ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator))
        else DropDown(AMode, true);
      end;
  end;
end;

procedure TCompletion.DropDown(const AMode: TCompletionList; const ShowAlways:
  boolean);
var
  ItemCount: integer;
  P: TPoint;
  Y: integer;
  PopupWidth, PopupHeight: integer;
  SysBorderWidth, SysBorderHeight: integer;
  R: TRect;
  Cancel: boolean;
  Eq: boolean;
begin
  CloseUp(false);
  FMode := AMode;
  with FMemoEx do
  begin
    Cancel := false;
    case FMode of
      cmIdentifiers: FMemoEx.DoCompletionIdentifier(Cancel);
      cmTemplates:
        FMemoEx.DoCompletionTemplate(Cancel)
    end;
    MakeItems;
    FindSelItem(Eq);
    if Cancel or (FItems.Count = 0) or (((ItemIndex = -1) or Eq) and not
      ShowAlways) then exit;
    FPopupList.Items := FItems;
    FPopupList.ItemHeight := FItemHeight;
    FVisible := true;
    SetItemIndex(FItemIndex);
    if FListBoxStyle in [lbStandard] then FPopupList.Style := lbOwnerDrawFixed
    else FPopupList.Style := FListBoxStyle;
    FPopupList.OnMeasureItem := FMemoEx.FOnCompletionMeasureItem;
    FPopupList.OnDrawItem := FMemoEx.FOnCompletionDrawItem;

    ItemCount := FItems.Count;
    SysBorderWidth := GetSystemMetrics(SM_CXBORDER);
    SysBorderHeight := GetSystemMetrics(SM_CYBORDER);
    R := CalcCellRect(FCaretX - FLeftCol, FCaretY - FTopRow + 1);
    P := R.TopLeft;
    P.X := ClientOrigin.X + P.X;
    P.Y := ClientOrigin.Y + P.Y;
    Dec(P.X, 2 * SysBorderWidth);
    Dec(P.Y, SysBorderHeight);
    if ItemCount > FDropDownCount then ItemCount := FDropDownCount;
    PopupHeight := ItemHeight * ItemCount + 2;
    Y := P.Y;
    if (Y + PopupHeight) > Screen.Height then
    begin
      Y := P.Y - PopupHeight - FCellRect.Height + 1;
      if Y < 0 then Y := P.Y;
    end;
    PopupWidth := FDropDownWidth;
    if PopupWidth = 0 then PopupWidth := Width + 2 * SysBorderWidth;
  end;
  FPopupList.Left := P.X;
  FPopupList.Top := Y;
  FPopupList.Width := PopupWidth;
  FPopupList.Height := PopupHeight;
  SetWindowPos(FPopupList.Handle, HWND_TOP, P.X, Y, 0, 0,
    SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
  FPopupList.Visible := true;
end;

{
  Compare of the lines.
  Lines S1 and S2 can be in undefined code page.
}
function TCompletion.Cmp1(const S1, S2: string): integer;
var
  T1, T2: string;
begin
  T1 := FMemoEx.DoChangeCase(S1, ME_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, ME_CASE_CONVERT_LOWER);
  Result := ANSIStrLIComp(PChar(T1), PChar(T2), Length(T2));
end;

{
  Compare of the lines.
  Lines S1 and S2 can be in undefined code page.
}
function TCompletion.Cmp2(const S1, S2: string): boolean;
var
  T1, T2: string;
begin
  T1 := FMemoEx.DoChangeCase(S1, ME_CASE_CONVERT_LOWER);
  T2 := FMemoEx.DoChangeCase(S2, ME_CASE_CONVERT_LOWER);
  Result := ANSICompareText(T1, T2) = 0;
end;

procedure TCompletion.MakeItems;
var
  i: integer;
  S: string;
begin
  FItems.Clear;
  case FMode of
    cmIdentifiers:
      for i := 0 to FIdentifiers.Count - 1 do
        FItems.Add(FIdentifiers[i]);
    cmTemplates:
      begin
        with FMemoEx do
          S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX);
        for i := 0 to FTemplates.Count - 1 do
          if Cmp1(FTemplates[i], S) = 0 then
            FItems.Add(FTemplates[i]);
        if FItems.Count = 0 then FItems.Assign(FTemplates);
      end;
  end;
end;

procedure TCompletion.FindSelItem(var Eq: boolean);

  function FindFirst(Ss: TSTrings; S: string): integer;
  var
    i: integer;
  begin
    for i := 0 to Ss.Count - 1 do
      if Cmp1(Ss[i], S) = 0 then
      begin
        Result := i;
        exit;
      end;
    Result := -1;
  end;

var
  S: string;
begin
  with FMemoEx do
    if FLines.Count > 0 then
      S := GetWordOnPos(FLines.ParaStrings[CaretY], CaretX)
      else S := '';
  if _Trim(S) = '' then ItemIndex := -1
  else ItemIndex := FindFirst(FItems, S);
  Eq := (ItemIndex > -1) and Cmp2(_Trim(SubStr(FItems[ItemIndex], 0, FSeparator)), S);
end;

procedure TCompletion.SelectItem;
var
  Cancel: boolean;
  Param: boolean;
begin
  FindSelItem(Param);
  Cancel := not Visible and (ItemIndex = -1);
  case FMode of
    cmIdentifiers: FMemoEx.DoCompletionIdentifier(Cancel);
    cmTemplates: FMemoEx.DoCompletionTemplate(Cancel);
  end;
  if Cancel or (GetItems.Count = 0) then CloseUp(false);
end;

procedure TCompletion.CloseUp(const Apply: boolean);
begin
  FItemIndex := ItemIndex;
  FPopupList.Visible := false;
  FVisible := false;
  FTimer.Enabled := false;
  if Apply and (ItemIndex > -1) then
    case FMode of
      cmIdentifiers: ReplaceWord(SubStr(FItems[ItemIndex], 0, FSeparator));
      cmTemplates: ReplaceWord(SubStr(FItems[ItemIndex], 2, FSeparator));
    end;
end;

procedure TCompletion.OnTimer(Sender: TObject);
begin
  DropDown(FDefMode, false);
end;

procedure TCompletion.ClearAutoChangeList;
var
  i: integer;
begin
  for i := 0 to FAutoChangeList.Count - 1 do Dispose(FAutoChangeList[i]);
  FAutoChangeList.Clear;
end;

procedure TCompletion.UpdateAutoChange;
begin
  AutoChangeChanged(FAutoChange);
end;

procedure TCompletion.AutoChangeChanged(Sender: TObject);

  procedure AddAutoChangeWord(const OldWord, NewWord: string);
  var
    ACW: PAutoChangeWord;
  begin
    if OldWord <> '' then
    begin
      New(ACW);
      ACW.OldWord := FMemoEx.DoChangeCase(OldWord, ME_CASE_CONVERT_LOWER);
      ACW.NewWord := NewWord;
      FAutoChangeList.Add(ACW);
    end;
  end;

var
  i: integer;
begin
  ClearAutoChangeList;
  for i := 0 to FAutoChange.Count - 1 do
    AddAutoChangeWord(SubStr(FAutoChange.Strings[i], 0, FSeparator),
                      SubStr(FAutoChange.Strings[i], 1, FSeparator));
  FAutoChangeList.Sort(AutoChangeCompare);
end;

procedure TCompletion.SetStrings(index: integer; AValue: TStrings);
begin
  case index of
    0: FIdentifiers.Assign(AValue);
    1: FTemplates.Assign(AValue);
    2: FAutoChange.Assign(AValue);
  end;
end;

function TCompletion.GetItemIndex: integer;
begin
  Result := FItemIndex;
  if FVisible then
    Result := FPopupList.ItemIndex;
end;

procedure TCompletion.SetItemIndex(AValue: integer);
begin
  FItemIndex := AValue;
  if FVisible then
    FPopupList.ItemIndex := FItemIndex;
end;

function TCompletion.GetInterval: cardinal;
begin
  Result := FTimer.Interval;
end;

procedure TCompletion.SetInterval(AValue: cardinal);
begin
  FTimer.Interval := AValue;
end;

{ TMemoExCompletionList }

constructor TMemoExCompletionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left := -1000;
  Visible := false;
  TabStop := false;
  ParentFont := false;
  Parent := Owner as TCustomMemoEx;
  Ctl3D := false;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := false;
  FTimer.Interval := 200;
  FTimer.OnTimer := OnTimer;
  Style := lbOwnerDrawFixed;
  ItemHeight := 13;
end;

destructor TMemoExCompletionList.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TMemoExCompletionList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := ExStyle or WS_EX_TOOLWINDOW;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
end;

procedure TMemoExCompletionList.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    Windows.SetParent(Handle, 0);
end;

procedure TMemoExCompletionList.DestroyWnd;
begin
  inherited DestroyWnd;
end;

procedure TMemoExCompletionList.MouseMove(Shift: TShiftState; X, Y: integer);
var
  F: integer;
begin
  YY := Y;
  if KeyPressed(VK_LBUTTON) then
  begin
    F := ItemAtPos(Point(X, Y), true);
    if F > -1 then ItemIndex := F;
    FTimer.Enabled := (Y < 0) or (Y > ClientHeight);
    if (Y < -ItemHeight) or (Y > ClientHeight + ItemHeight) then FTimer.Interval := 50
    else FTimer.Interval := 200;
  end;
end;

procedure TMemoExCompletionList.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
var
  F: integer;
begin
  MouseCapture := true;
  F := ItemAtPos(Point(X, Y), true);
  if F > -1 then ItemIndex := F;
end;

procedure TMemoExCompletionList.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: integer);
begin
  MouseCapture := false;
  (Owner as TCustomMemoEx).FCompletion.CloseUp(
    (Button = mbLeft) and PtInRect(ClientRect, Point(X, Y)));
end;

procedure TMemoExCompletionList.OnTimer(Sender: TObject);
begin
  if (YY < 0) then Perform(WM_VSCROLL, SB_LINEUP, 0)
  else
    if (YY > ClientHeight) then Perform(WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TMemoExCompletionList.WMCancelMode(var Message: TMessage);
begin
  (Owner as TCustomMemoEx).FCompletion.CloseUp(false);
end;

procedure TMemoExCompletionList.CMHintShow(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TMemoExCompletionList.DrawItem(Index: integer; Rect: TRect; State:
  TOwnerDrawState);
var
  Offset, W: integer;
  S: string;
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, Rect, State)
  else
    begin
      Canvas.FillRect(Rect);
      Offset := 3;
      with (Owner as TCustomMemoEx).FCompletion do
        case FMode of
          cmIdentifiers:
            Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
              Separator));
          cmTemplates:
            begin
              Canvas.TextOut(Rect.Left + Offset, Rect.Top, SubStr(Items[Index], 1,
                Separator));
              Canvas.Font.Style := [fsBold];
              S := SubStr(Items[Index], 0, Separator);
              W := Canvas.TextWidth(S);
              Canvas.TextOut(Rect.Right - 2 * Offset - W, Rect.Top, S);
            end;
        end;
    end;
end;

procedure Register;
begin
  RegisterComponents('tmemoex.com', [TMemoEx]);
end;

function TCustomMemoEx.LineHeight: integer;
var R: TRect;
begin
  R := CalcCellRect(0, 0);
  Result := R.Bottom - R.Top;
end;

end.

