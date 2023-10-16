unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math,
  StdCtrls, ImgList, IniFiles, Clipbrd, ActnList, Menus,
  CylinderMap, ComCtrls, ToolWin, ExtCtrls, Buttons, Grids,
  uKeys, MyStrings, DataStructs, Resources, PowerGrid, uManager, uHexMap,
  uVisualOrders, uOrderProcessor, uListFilter, AtlaDate, uGameSubs, uAvatars, ShellApi,
  uMail, uExport, uScript, uInterface, IntEdit, uTurnEvents, uUnitRecs,
  RepRead, uHistory, uSpecEdit, uTerrEdit, uTradeStructs, uTaxTrade,
  uUnitArmies, uRegionStat, uSpyReps, uUnitProduction, uGameStart,
  uUnitFilter, uShortcuts, uRoute, uGates, uItemStats, uWantedItems;

const
  crMove = crHandPoint;
  crLinkShaft = crHandPoint;

  UnitGridColCount = 16;
  UnitGridHeadIcons: array[0..UnitGridColCount-1] of integer =
    (0, 0, 0, 0, 0, 0, 0, bmpMen, bmpSilver, bmpMounts, 0, 0, 0, 0, 0,
    bmpSkill);
  FlagChars: array[0..UnitFlagsCount-1] of char =
    ('G', 'T', 'A', 'B', 'H', 'N', 'X');
  RevFlagChars: array[1..2] of char = ('U', 'F');
  ugcObject = 0;
  ugcObjectNum = 1;
  ugcFaction = 3;
  ugcFactionNum = 4;
  ugcFlags = 11;
  ugcUnitName = 5;
  ugcUnitNum = 6;
  ugcCustom = 13;
  ugcMonthOrder = 14;
  ugcLocal = 10;

  MsgLength = 30;

type
  TAdvisorState = set of (asFirstActivate, asProcessing, asCloseQuery,
    asReadingGame, asFirstProcessing);
  TWorkMode = (mNormal, mMove, mLinkShaft);

  TMainForm = class(TForm)
    InfoPanel: TPanel;
    InfoPControl: TPageControl;
    tsRegion: TTabSheet;
    tsUnit: TTabSheet;
    MapUnitsPanel: TPanel;
    UnitSplitter: TSplitter;
    MapToolPanel: TPanel;
    UnitsPanel: TPanel;
    RegionInfoPanel: TPanel;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    View1: TMenuItem;
    ShowToolbarItm: TMenuItem;
    Label9: TLabel;
    UnitGrid: TPowerGrid;
    MapToolbar: TToolBar;
    ToolButton4: TToolButton;
    ListBtn: TToolButton;
    ListDownBtn: TSpeedButton;
    ToolButton5: TToolButton;
    FogBtn: TToolButton;
    FogDownBtn: TSpeedButton;
    ToolButton6: TToolButton;
    FlagBtn: TToolButton;
    ToolButton11: TToolButton;
    MIcBtn: TToolButton;
    MIcDownBtn: TSpeedButton;
    ToolButton13: TToolButton;
    SIcBtn: TToolButton;
    SIcDownBtn: TSpeedButton;
    ToolButton15: TToolButton;
    STxBtn: TToolButton;
    ToolButton17: TToolButton;
    StructBtn: TToolButton;
    ToolButton19: TToolButton;
    LevelBtn: TToolButton;
    STxDownBtn: TSpeedButton;
    FlagDownBtn: TSpeedButton;
    GameManagerItm: TMenuItem;
    ActionList: TActionList;
    ManagerAction: TAction;
    OptionsAction: TAction;
    SaveOrderAction: TAction;
    CopyOrderAction: TAction;
    Help1: TMenuItem;
    About1: TMenuItem;
    N3: TMenuItem;
    SaveOrder1: TMenuItem;
    CopyOrder1: TMenuItem;
    Options1: TMenuItem;
    N5: TMenuItem;
    Exit1: TMenuItem;
    Toolbars1: TMenuItem;
    Maptools1: TMenuItem;
    HelpContents1: TMenuItem;
    PrevTurnAction: TAction;
    NextTurnAction: TAction;
    PreviousTurn1: TMenuItem;
    NextTurn1: TMenuItem;
    SaveDialog: TSaveDialog;
    Infopanel1: TMenuItem;
    TurnsItm: TMenuItem;
    MiniMapAction: TAction;
    N10: TMenuItem;
    OpenDialog: TOpenDialog;
    RegisterItm: TMenuItem;
    RegSplit: TMenuItem;
    UnitPControl: TPageControl;
    MsgSheet: TTabSheet;
    MsgGrid: TPowerGrid;
    OrderSheet: TTabSheet;
    Panel2: TPanel;
    ItemSplit: TSplitter;
    Map1: TMenuItem;
    N4: TMenuItem;
    ListItm: TMenuItem;
    ListEnableItm: TMenuItem;
    N11: TMenuItem;
    WantedListItm: TMenuItem;
    ForSaleListItm: TMenuItem;
    ProductsListItm: TMenuItem;
    FogItm: TMenuItem;
    FlagItm: TMenuItem;
    MIcItm: TMenuItem;
    SIcItm: TMenuItem;
    STxItm: TMenuItem;
    FogEnableItm: TMenuItem;
    N12: TMenuItem;
    FogVisibleItm: TMenuItem;
    FogVisitedItm: TMenuItem;
    FlagEnableItm: TMenuItem;
    N13: TMenuItem;
    SelfFlagItm: TMenuItem;
    OtherFlagsItm: TMenuItem;
    OldFlagsItm: TMenuItem;
    ScoutingFactionsItm: TMenuItem;
    N14: TMenuItem;
    UnitDiagramItm: TMenuItem;
    MenDiagramItm: TMenuItem;
    MIcEnableItm: TMenuItem;
    N15: TMenuItem;
    MIcMonstersItm: TMenuItem;
    MIcGatesItm: TMenuItem;
    MIcBattlesItm: TMenuItem;
    SIcEnableItm: TMenuItem;
    N16: TMenuItem;
    SIcSettlItm: TMenuItem;
    SIcWeatherItm: TMenuItem;
    SIcPeasantsItm: TMenuItem;
    SIcGuardItm: TMenuItem;
    STxEnableItm: TMenuItem;
    N17: TMenuItem;
    STxSettlItm: TMenuItem;
    STxPeasantsItm: TMenuItem;
    STxCountPeasantsItm: TMenuItem;
    STxTaxRateItm: TMenuItem;
    STxWagesItm: TMenuItem;
    STxEntertainItm: TMenuItem;
    LevelAction: TAction;
    StructItm: TMenuItem;
    LevelItm: TMenuItem;
    OldstyleFlagsItm: TMenuItem;
    N18: TMenuItem;
    DefineListfilter1: TMenuItem;
    ListFilterAction: TAction;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    MapPanel: TPanel;
    HexMap: TCylinderMap;
    StructSplitter: TSplitter;
    UnitDockPanel: TPanel;
    UnitNameBevel: TBevel;
    NumLabel: TLabel;
    UnitNameEdit: TEdit;
    UnitMainDataPanel: TPanel;
    UnitMainDataTextPanel: TPanel;
    Label3: TLabel;
    FactionLabel: TLabel;
    Label10: TLabel;
    StructImage: TImage;
    StructLabel: TLabel;
    UnitDescrBevel: TBevel;
    UnitDescrEdit: TEdit;
    UnitImagePanel: TPanel;
    UnitImage: TImage;
    UnitPageSplit: TSplitter;
    FlagBar: TToolBar;
    GuardFlagBtn: TToolButton;
    ToolButton33: TToolButton;
    AvoidFlagBtn: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    StructNameEdit: TEdit;
    StructNameBevel: TBevel;
    StructDescrEdit: TEdit;
    StructNumLabel: TLabel;
    GateImage: TImage;
    GateLabel: TLabel;
    LandLabel: TLabel;
    HexLabel: TLabel;
    SettlLabel: TLabel;
    PeasantsLabel: TLabel;
    TaxRateLabel: TLabel;
    WagesLabel: TLabel;
    EnterLabel: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    TradePanel: TPanel;
    Label18: TLabel;
    WantedGrid: TPowerGrid;
    Label17: TLabel;
    ForSaleGrid: TPowerGrid;
    MoveTypeLabel: TLabel;
    Label2: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    UnitWeightLabel: TLabel;
    HorsesWeightLabel: TLabel;
    TransportWeightLabel: TLabel;
    UnitWeightWarning: TImage;
    HorsesWeightWarning: TImage;
    TransportWeightWarning: TImage;
    Image5: TImage;
    WingWeightWarning: TImage;
    WingWeightLabel: TLabel;
    Label11: TLabel;
    Label13: TLabel;
    GroupBox1: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    StructDefLabel: TLabel;
    StructMenLabel: TLabel;
    StructSizeLabel: TLabel;
    Label22: TLabel;
    StructOwnerLabel: TLabel;
    OwnerFlagImage: TImage;
    StructLoadLabel: TLabel;
    StructControlLabel: TLabel;
    IncomplStructImage: TImage;
    Label19: TLabel;
    StructGroupLabel: TLabel;
    ProductGrid: TPowerGrid;
    Label12: TLabel;
    FactionFlagImage: TImage;
    SkillGrid: TPowerGrid;
    ToolBar: TToolBar;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton27: TToolButton;
    ToolButton26: TToolButton;
    ToolButton29: TToolButton;
    ToolButton18: TToolButton;
    ToolButton21: TToolButton;
    TurnCombo: TComboBox;
    ToolButton22: TToolButton;
    ToolButton20: TToolButton;
    CoordLabel: TPanel;
    LevelDownBtn: TSpeedButton;
    ToolButton7: TToolButton;
    BookmarkBtn: TToolButton;
    BookmarkAction: TAction;
    BmkDownBtn: TSpeedButton;
    BookmarksItm: TMenuItem;
    RegionNotesItm: TMenuItem;
    RegionPControl: TPageControl;
    ItemSheet: TTabSheet;
    NoteSheet: TTabSheet;
    NotesMemo: TMemo;
    Panel1: TPanel;
    WeatherLabel: TLabel;
    ToolButton8: TToolButton;
    FactionAction: TAction;
    ToolButton9: TToolButton;
    TurnEventsAction: TAction;
    ToolButton10: TToolButton;
    OrdersAction: TAction;
    ToolButton12: TToolButton;
    Factions1: TMenuItem;
    itmMiniMap: TMenuItem;
    N1: TMenuItem;
    Orders1: TMenuItem;
    urnEvents1: TMenuItem;
    StructEditAction: TAction;
    ItemEditAction: TAction;
    SkillEditAction: TAction;
    ToolButton32: TToolButton;
    BattlesAction: TAction;
    ItemEditor1: TMenuItem;
    SkillEditor1: TMenuItem;
    ObjectEditor1: TMenuItem;
    Battles1: TMenuItem;
    ToolButton40: TToolButton;
    AvatarsAction: TAction;
    Avatars1: TMenuItem;
    ToolButton41: TToolButton;
    MakeAvatarAction: TAction;
    GroupBox2: TGroupBox;
    lWayTo: TLabel;
    btnLinkShaft: TButton;
    ToolButton43: TToolButton;
    AdvisorWarnAction: TAction;
    GridModeAction: TAction;
    N2: TMenuItem;
    itmHideInvisRegions: TMenuItem;
    ToolButton45: TToolButton;
    SaveOrderAs: TMenuItem;
    GameRulesItm: TMenuItem;
    ExportMapItm: TMenuItem;
    N7: TMenuItem;
    PreprocessorErrors1: TMenuItem;
    MailOrderAction: TAction;
    ToolButton46: TToolButton;
    SendOrders1: TMenuItem;
    btnConsume: TToolButton;
    ToolButton48: TToolButton;
    btnReveal: TToolButton;
    btnSpoils: TToolButton;
    MIcWarningsItm: TMenuItem;
    DefineRegionwarning1: TMenuItem;
    ScriptEditAction: TAction;
    ToolButton47: TToolButton;
    ScriptSplit: TToolButton;
    PopMenu: TPopupMenu;
    ScriptsItm: TMenuItem;
    Scripts3: TMenuItem;
    N6: TMenuItem;
    ToolPanel: TPanel;
    StopProcessorAction: TAction;
    Panel3: TPanel;
    btnStopProcessor: TSpeedButton;
    SaveAsAction: TAction;
    StopOrderProcessing1: TMenuItem;
    ExportMapAction: TAction;
    RegionWarningAction: TAction;
    AddExportAction: TAction;
    Addcurrentregion1: TMenuItem;
    TownTradeAction: TAction;
    ools1: TMenuItem;
    ownTrade1: TMenuItem;
    imgWeatherNext: TImage;
    TaxMenLabel: TLabel;
    EnterMenLabel: TLabel;
    WorkLabel: TLabel;
    Label20: TLabel;
    MaxWagesLabel: TLabel;
    pItemGrid: TPanel;
    ItemGrid: TPowerGrid;
    StructEnabledItm: TMenuItem;
    N8: TMenuItem;
    StructDefItm: TMenuItem;
    StructTranspItm: TMenuItem;
    StructClosedItm: TMenuItem;
    StructTradeItm: TMenuItem;
    StructInnerItm: TMenuItem;
    StructDownBtn: TSpeedButton;
    StructRoadsItm: TMenuItem;
    pGiveTools: TPanel;
    ToolBar2: TToolBar;
    tbGiveAll: TToolButton;
    tbGiveBattle: TToolButton;
    tbGiveTrade: TToolButton;
    Label21: TLabel;
    eGiveAmt: TIntEdit;
    btnLocal: TSpeedButton;
    StructGrid: TPowerGrid;
    N19: TMenuItem;
    SoldiersAction: TAction;
    ToolButton42: TToolButton;
    ToolButton50: TToolButton;
    SoldiersinUnit1: TMenuItem;
    ToolBar3: TToolBar;
    ToolButton3: TToolButton;
    itmExportRuleset: TMenuItem;
    MicNotesItm: TMenuItem;
    Importmap1: TMenuItem;
    ImportMapAction: TAction;
    BattleSimAction: TAction;
    BattleSimulator1: TMenuItem;
    SpecEditAction: TAction;
    AbilityEditor1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton14: TToolButton;
    ToolButton23: TToolButton;
    TerrEditAction: TAction;
    SkillEditor2: TMenuItem;
    gAllItems: TPowerGrid;
    AllItemsAction: TAction;
    ItemsinRegion1: TMenuItem;
    TradeStructAction: TAction;
    radeStructures1: TMenuItem;
    SIcTaxTradeItm: TMenuItem;
    ScoutsItm: TMenuItem;
    TaxTradeAction: TAction;
    axTradeRegions1: TMenuItem;
    ToolButton16: TToolButton;
    ToolButton28: TToolButton;
    ExportRulesetAction: TAction;
    ForcesAction: TAction;
    Armies1: TMenuItem;
    RegionStatsAction: TAction;
    RegionStats1: TMenuItem;
    SpyRepsAction: TAction;
    SpyReports1: TMenuItem;
    EngineRunAction: TAction;
    Runnextturn1: TMenuItem;
    UnitProductionAction: TAction;
    UnitProduction1: TMenuItem;
    ToolButton34: TToolButton;
    UnitFilterAction: TAction;
    GridModeAction1: TMenuItem;
    UnitFilter1: TMenuItem;
    N20: TMenuItem;
    itmTerrain: TMenuItem;
    OrderMemo: TMemo;
    ReloadScriptsAction: TAction;
    ReloadScripts1: TMenuItem;
    Level1: TMenuItem;
    tbUnitTools: TToolBar;
    ToolButton38: TToolButton;
    cmFilterFaction: TComboBox;
    ToolButton39: TToolButton;
    btnMyFaction: TToolButton;
    btnAllFactions: TToolButton;
    ToolButton51: TToolButton;
    btnFactionMode: TToolButton;
    btnUnitFilter: TToolButton;
    itmUnitTools: TMenuItem;
    UnmodItemsAction: TAction;
    Showunmodifieditemamounts1: TMenuItem;
    ToolBar1: TToolBar;
    btnCheckOrder: TToolButton;
    btnClearOrder: TToolButton;
    imgMonthOrder: TImage;
    lMonthOut: TLabel;
    tbGiveSpoils: TToolButton;
    RegionRepAction: TAction;
    RegionReport1: TMenuItem;
    FogCustomItm: TMenuItem;
    MiniMap2: TMenuItem;
    N9: TMenuItem;
    N21: TMenuItem;
    itmMiniGeo: TMenuItem;
    itmMiniPolitical: TMenuItem;
    itmMiniVisible: TMenuItem;
    GatesAction: TAction;
    Gates1: TMenuItem;
    ExportSkillsAction: TAction;
    ExportskillstoClipboard1: TMenuItem;
    ImportSkillsAction: TAction;
    ExportMagesAction: TAction;
    Skills1: TMenuItem;
    ImportfromClipboard1: TMenuItem;
    ExportallmagestoClipboard1: TMenuItem;
    ArrivingUnitsAction: TAction;
    Showarrivingunits1: TMenuItem;
    ItemStatsAction: TAction;
    ItemStats1: TMenuItem;
    WantedItemsAction: TAction;
    WantedItems1: TMenuItem;
    ToolButton44: TToolButton;
    FindUnitAction: TAction;
    FindUnit1: TMenuItem;
    NextErrorAction: TAction;
    ToolButton49: TToolButton;
    ToolButton52: TToolButton;
    NextError1: TMenuItem;
    LoadPanel: TPanel;
    StructPanel: TPanel;
    pStructure: TPanel;
    pnStructureName: TPanel;
    spPanel1: TPanel;
    Panel6: TPanel;
    pnStrucDescription: TPanel;
    StructDescrBevel: TBevel;
    spPanel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pnStrucInfo: TPanel;
    pnRightSidebar: TPanel;
    Splitter1: TSplitter;
    procedure HexMapDrawHex(Sender: TObject; HX, HY: Integer;
      ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
    procedure HexMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HexMapDrawExtra(Sender: TObject; HX, HY: Integer;
      ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
    procedure TradePanelResize(Sender: TObject);
    procedure ShowToolbarItmClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure HexMapSelectHex(Sender: TObject; HX, HY: Integer);
    procedure FormEditEnter(Sender: TObject);
    procedure FormEditExit(Sender: TObject);
    procedure UnitGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure MsgGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure FormEditKeyPress(Sender: TObject; var Key: Char);
    procedure UnitGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure UnitGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure GroupItmClick(Sender: TObject);
    procedure FogBtnClick(Sender: TObject);
    procedure MIcBtnClick(Sender: TObject);
    procedure SIcBtnClick(Sender: TObject);
    procedure STxBtnClick(Sender: TObject);
    procedure FlagBtnClick(Sender: TObject);
    procedure LevelActionExecute(Sender: TObject);
    procedure ManagerActionExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OptionsActionExecute(Sender: TObject);
    procedure SaveOrderActionExecute(Sender: TObject);
    procedure CopyOrderActionExecute(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Maptools1Click(Sender: TObject);
    procedure ItemGridDblClick(Sender: TObject);
    procedure SkillGridDblClick(Sender: TObject);
    procedure TurnComboChange(Sender: TObject);
    procedure PrevTurnActionExecute(Sender: TObject);
    procedure NextTurnActionExecute(Sender: TObject);
    procedure HelpContents1Click(Sender: TObject);
    procedure OrderMemoKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ClearOrderBtnClick(Sender: TObject);
    procedure Infopanel1Click(Sender: TObject);
    procedure MiniMapActionExecute(Sender: TObject);
    procedure HexMapMoveMap(Sender: TObject; X, Y: Integer);
    procedure RegisterItmClick(Sender: TObject);
    procedure UnitImageDblClick(Sender: TObject);
    procedure MapToolEnableClick(Sender: TObject);
    procedure ListFilterActionExecute(Sender: TObject);
    procedure UnitMainDataTextPanelResize(Sender: TObject);
    procedure UnitFlagBtnClick(Sender: TObject);
    procedure OrderMemoExit(Sender: TObject);
    procedure CheckOrderBtnClick(Sender: TObject);
    procedure UnitGridDblClick(Sender: TObject);
    procedure BookmarkActionExecute(Sender: TObject);
    procedure RegionNotesItmClick(Sender: TObject);
    procedure NotesMemoExit(Sender: TObject);
    procedure FactionActionExecute(Sender: TObject);
    procedure TurnEventsActionExecute(Sender: TObject);
    procedure OrdersActionExecute(Sender: TObject);
    procedure StructEditActionExecute(Sender: TObject);
    procedure ItemEditActionExecute(Sender: TObject);
    procedure SkillEditActionExecute(Sender: TObject);
    procedure BattlesActionExecute(Sender: TObject);
    procedure AvatarsActionExecute(Sender: TObject);
    procedure MakeAvatarActionExecute(Sender: TObject);
    procedure btnLinkShaftClick(Sender: TObject);
    procedure HexMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AdvisorWarnActionExecute(Sender: TObject);
    procedure GridModeActionExecute(Sender: TObject);
    procedure HideInvisibleClick(Sender: TObject);
    procedure SaveOrderAsClick(Sender: TObject);
    procedure GameRulesItmClick(Sender: TObject);
    procedure ExportMapItmClick(Sender: TObject);
    procedure MailOrderActionExecute(Sender: TObject);
    procedure ExtFlagBtnClick(Sender: TObject);
    procedure DefineRegionwarning1Click(Sender: TObject);
    procedure ScriptEditActionExecute(Sender: TObject);
    procedure SkillGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StopProcessorActionExecute(Sender: TObject);
    procedure AddExportActionExecute(Sender: TObject);
    procedure TownTradeActionExecute(Sender: TObject);
    procedure UnitGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure StructBtnClick(Sender: TObject);
    procedure btnLocalClick(Sender: TObject);
    procedure StructGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure StructGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SoldiersActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure StructGridMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImportMapActionExecute(Sender: TObject);
    procedure BattleSimActionExecute(Sender: TObject);
    procedure SpecEditActionExecute(Sender: TObject);
    procedure ItemGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TerrEditActionExecute(Sender: TObject);
    procedure AllItemsActionExecute(Sender: TObject);
    procedure gAllItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure gAllItemsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ItemGridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure tbMaskGiveEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure HexMapDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemGridEnter(Sender: TObject);
    procedure TradeStructActionExecute(Sender: TObject);
    procedure TaxTradeActionExecute(Sender: TObject);
    procedure ExportRulesetActionExecute(Sender: TObject);
    procedure ForcesActionExecute(Sender: TObject);
    procedure RegionStatsActionExecute(Sender: TObject);
    procedure SpyRepsActionExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EngineRunActionExecute(Sender: TObject);
    procedure UnitProductionActionExecute(Sender: TObject);
    procedure UnitFilterActionExecute(Sender: TObject);
    procedure ReloadScriptsActionExecute(Sender: TObject);
    procedure itmUnitToolsClick(Sender: TObject);
    procedure btnMyFactionClick(Sender: TObject);
    procedure btnAllFactionsClick(Sender: TObject);
    procedure cmFilterFactionChange(Sender: TObject);
    procedure ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure HexMapMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UnmodItemsActionExecute(Sender: TObject);
    procedure RegionRepActionExecute(Sender: TObject);
    procedure MiniMapModeClick(Sender: TObject);
    procedure GatesActionExecute(Sender: TObject);
    procedure ExportSkillsActionExecute(Sender: TObject);
    procedure ImportSkillsActionExecute(Sender: TObject);
    procedure ExportMagesActionExecute(Sender: TObject);
    procedure ArrivingUnitsActionExecute(Sender: TObject);
    procedure ItemStatsActionExecute(Sender: TObject);
    procedure WantedItemsActionExecute(Sender: TObject);
    procedure FindUnitActionExecute(Sender: TObject);
    procedure NextErrorActionExecute(Sender: TObject);
  private
  public
    State: TAdvisorState;
    MapRectTopLeft: TPoint;
    MapDblClick: boolean;
    Mode: TWorkMode;
    ShiftState: TShiftState;
    OrdersChanged: boolean;
    ActingStruct: TStruct;
    ActingRegion: TRegion;
    ItemGridTop: integer;
    Filter: record
      Enabled, StructFilter: boolean;
      UnitName, UnitNum: string;
      FactionNum: integer;
      Skills: TSkillList;
      Items: TItemDataList;
      Mages, Any: boolean;
      RegCoords: TCoords;
      StructNum: integer;
    end;
    RerunRegions: TRegionList;
    RouteMode: record
      Enabled: boolean;
      Route: TRoute;
    end;
    LastError: integer;
    // Processor
    procedure ProcessAllOrders;
    procedure StartProcess(ARegion: TRegion; AScript: integer; Args: string;
      Startup: boolean);
    procedure ProcessOrders(ARegion: TRegion);
    procedure ProcessScript(AScript: integer; Args: string);
    procedure ProcessorThreadTerminate(Sender: TObject);
    // Global interface
    procedure ApplyConfig;
    procedure GlobalEnable(Value: boolean);
    procedure UnitEnable(Value: boolean);
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);
    function NoDraw: boolean;
    // Stuff
    procedure StartupGame;
    procedure StoreMemosInfo;
    procedure ClearRegionInfo;
    procedure ClearUnitInfo;
    procedure EndMoveMode; 
    procedure FillRegionInfo(ARegion: TRegion);
    procedure FillUnitGrid;
    procedure SelectUnit(U: TUnit);
    procedure FillUnitInfo(AUnit: TUnit);
    procedure FillAllItems(AUnit: TUnit);
    procedure ImplyRegistration(Registered: boolean);
    procedure SetCaption;
    procedure StartMoveMode(Army, Route: boolean);
    procedure TurnItmClick(Sender: TObject);
    procedure HexMapSetup;
    procedure HexMapGoto(C: TCoords; DefaultHex: boolean); overload;
    procedure HexMapGoto(C: TCoords); overload;
    procedure SetupMapLevels;
    procedure ChangeLevel(Sender: TObject);
    function GetMapToolItm(Index: integer): TMenuItem;
    function GetMapToolBtn(Index: integer): TToolButton;
    procedure FillStructGrid(ARegion: TRegion);
    procedure FillStructInfo(AStruct: TStruct);
    procedure ClearStructInfo;
    procedure SetTurns;
    function EditBevel(N: integer): TBevel;
    procedure GotoBookmark(Sender: TObject);
    procedure SetupBookmarks;
    procedure SetupScripts;
    procedure SetupEngine;
    procedure SetupTerrains;
    procedure ScriptClick(Sender: TObject);
    procedure StartLinkMode;
    procedure EndLinkMode(HX, HY: integer);
    procedure SetExtFlag(Sender: TObject);
    procedure ChangeTerrain(Sender: TObject);
    function SelectedCoords: TCoords;
  end;

var
  MainForm: TMainForm;

const
  dockItems = 0;
  dockSkills = 1;
  dockFlags = 2;
  dockUnit = 3;
  dockCount = 4;

implementation

uses uOptions, uAbout, uMiniMap, uFactions, uMemo, uStructEdit,
  uItemEdit, uSkillEdit, uAvatarEdit, uRegistration, RegCode, uBattle,
  uScriptEdit, uMapExport, uTownTrade, uSoldiers;

{$R *.DFM}

function AddMenuItem(AParent: TMenuItem; ACaption: string; AImage: integer;
  Handler: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(MainForm);
  Result.Caption := ACaption;
  if AImage <> -1 then Result.ImageIndex := AImage;
  Result.OnClick := Handler;
  AParent.Add(Result);
end;

function GetFactionNum(s: string): integer;
var Trace: TTrace;
begin
  if s = '*' then Result := -1
  else begin
    if s = '<unknown>' then Result := 0
    else begin
      Trace := TTrace.Create(s);
      Trace.Before('(');
      Result := Trace.Num;
      Trace.Free;
    end;
  end;
end;

procedure TMainForm.SetupMapLevels;
var i: integer;
    R: TRect;
    MaxP: TPoint;
begin
  LevelItm.Clear;
  // Load detection settings
  for i := 0 to Map.Levels.Count-1 do begin
    Map.Levels[i].MaxPoint := Point(GameConfig.ReadInteger('Map',
      'MaxX_' + Map.Levels[i].Name, 9999), GameConfig.ReadInteger('Map',
      'MaxY_' + Map.Levels[i].Name, 9999));
  end;
  // Set bounds for HexMap
  SetLength(MapBounds, Map.Levels.Count);
  for i := 0 to Map.Levels.Count-1 do begin
    if Map.Levels[i].Name <> 'nexus' then begin
      R := Map.Levels[i].Bounds;
      MaxP := Map.Levels[i].MaxPoint;
      R.Left := Max(R.Left - 4, 0);
      R.Top := Max(R.Top - 8, 0);
      R.Right := Max(R.Right, Min(R.Right + 4, MaxP.X - 1));
      R.Bottom := Max(R.Bottom, Min(R.Bottom + 8, MaxP.Y - 1));
      MapBounds[i] := R;
    end
    else MapBounds[i] := Rect(-1, -4, 2, 4);
  end;
  // Create menu items
  for i := 0 to Map.Levels.Count-1 do begin
    if not Map.Levels[i].Empty then
      with AddMenuItem(LevelItm, Map.Levels[i].Name, -1, ChangeLevel) do begin
        RadioItem := True;
      end;
  end;
  ApplyShortcuts(LevelItm);
end;

procedure TMainForm.SetupBookmarks;
var i, amt: integer;
    Lines: TStrings;
begin
  Lines := TStringList.Create;
  GameConfig.ReadSection('Bookmarks', Lines);
  BookmarksItm.Clear;
  if ProgOpened then amt := Lines.Count else amt := Min(Lines.Count, 1);
  for i := 0 to amt-1 do
    AddMenuItem(BookmarksItm, Lines[i], -1, GotoBookmark);
  Lines.Free;
  ApplyShortcuts(BookmarksItm);
end;

procedure TMainForm.GotoBookmark(Sender: TObject);
var s: string;
begin
  s := StringReplace(TMenuItem(Sender).Caption, '&', '', [rfReplaceAll]);
  s := GameConfig.ReadString('Bookmarks', s, '');
  if s <> '' then HexMapGoto(BookmarkCoords(s));
end;

procedure TMainForm.ChangeLevel(Sender: TObject);
begin
  Map.Level := Map.Levels.NumOf(StringReplace(TMenuItem(Sender).Caption,
    '&', '', [rfReplaceAll]));
  TMenuItem(Sender).Checked := True;
  GameConfig.WriteInteger('Map', 'Level', Map.Level);
  HexMapSetup;
end;

procedure TMainForm.ImplyRegistration(Registered: boolean);
begin
  RegisterItm.Visible := not Registered;
  RegSplit.Visible := not Registered;
  ScriptsItm.Visible := Registered;
  ScriptEditAction.Visible := Registered;
  ScriptSplit.Visible := Registered;
  SetCaption;
end;

procedure TMainForm.AppMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.message = WM_KEYDOWN then ScanKey := Msg.lParam shr 16;
end;

function TMainForm.NoDraw: boolean;
begin
  Result := ((Game = nil) or (VTurn = nil) or (asProcessing in State)
    or (asReadingGame in State));
end;


 { TMainForm }

 { -------------- Create and Destroy ------------------- }


procedure TMainForm.FormCreate(Sender: TObject);
var Base: string;
begin
  Application.OnMessage := AppMessage;
  State := State + [asFirstActivate];
  DecimalSeparator := '.';
  Base := ExtractFilePath(Application.ExeName);
  Filter.Skills := TSkillList.Create;
  Filter.Items := TItemDataList.Create;
  RerunRegions := TRegionList.Create;
  RouteMode.Route := TRoute.Create;

  // Registration
  // ProgOpened := CheckCode(Config.ReadString('Prog', 'RegName', ''),
  //   Config.ReadString('Prog', 'RegCode', ''));
  // No more registration
  ProgOpened := True;
  ImplyRegistration(ProgOpened);

  // Load stuff
  ResForm := TResForm.Create(Self);
  LoadConfigColors;
  ApplyShortcuts(MainMenu.Items);
  try
    LoadKeys(Base + 'english.key');
  except
    on E: Exception do begin
      Repaint;
      MessageDlg('Error loading keyfile english.key: ' + E.Message +
        '. Please reinstall Advisor.', mtError, [mbOk], 0);
      State := [];
      GlobalEnable(False);
    end;
  end;
  LoadIcons;
  ResForm.LoadPeasantExtras;

  tbGiveAll.Cursor := crHand;
  tbGiveBattle.Cursor := crHand;
  tbGiveTrade.Cursor := crHand;
  tbGiveSpoils.Cursor := crHand;
  gAllItems.Cols[1].Format := cfNumber;
  ItemGrid.Cols[1].Format := cfNumber;
  gAllItems.Cells[0, 0] := 'Amount';
  gAllItems.Cells[1, 0] := 'Item';
  gAllItems.Cells[2, 0] := 'Unit';

  // Makeup window
  ApplyConfig;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  if asFirstActivate in State then begin
    ManagerActionExecute(Self);
    State := State - [asFirstActivate];
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var mr: integer;
begin
  StoreMemosInfo;
  if asProcessing in State then begin
    CanClose := False;
    State := State + [asCloseQuery];
    StopProcessOrders;
  end
  else begin
    if not Config.ReadBool('MainWin', 'ConfirmSave', False)
      or (MessageDlg('Save changes?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
      StoreMemosInfo;
      if OrdersChanged and Config.ReadBool('MainWin', 'UnsavedOrders', True) then begin
        mr := MessageDlg('Orders was changed. Save order file?', mtConfirmation,
          [mbYes, mbNo, mbCancel], 0);
        if mr = mrYes then SaveOrderAction.Execute
        else if mr = mrCancel then begin
          CanClose := False;
          Exit;
        end;
      end;

      Screen.Cursor := crHourGlass;
      CloseGame;
      UnitGrid.SaveColumns(Config);
      SaveConfigColors;
      if Config.ReadBool('MiniMap', 'Visible', FALSE) = True then begin
        MiniMapForm.Close; // Just to pickup config changes from OnClose
        Config.WriteBool('MiniMap', 'Visible', True); // Override OnClose's Visible
      end;
      Config.WriteInteger('MainWin', 'MainTab', InfoPControl.ActivePageIndex);
      Config.WriteInteger('MainWin', 'UnitTab', UnitPControl.ActivePageIndex);
      Config.WriteInteger('MainWin', 'RegionTab', RegionPControl.ActivePageIndex);
      Config.WriteInteger('MainWin', 'SplitterPos', UnitsPanel.Height);
      Config.WriteInteger('MainWin', 'ItemSplitterPos', pItemGrid.Height);
      Config.WriteInteger('MainWin', 'UnitPageSplitterPos', UnitPControl.Height);
      Config.WriteInteger('MainWin', 'StructSplitterPos', StructGrid.Width);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ResForm.Free;
  Filter.Skills.ClearAndFree;
  Filter.Items.Free;
  RerunRegions.Free;
  RouteMode.Route.Free;
end;


  { ---------------- Order processor ---------------------- }


procedure TMainForm.ProcessOrders(ARegion: TRegion);
begin
  StartProcess(ARegion, -1, '', (asFirstProcessing in State) and
    not OrdersLoaded);
end;

procedure TMainForm.ProcessScript(AScript: integer; Args: string);
begin
  StartProcess(nil, AScript, Args, (asFirstProcessing in State) and
    not OrdersLoaded);
end;

procedure TMainForm.ProcessAllOrders;
begin
  ProcessOrders(nil);
end;

// Start processor
procedure TMainForm.StartProcess(ARegion: TRegion; AScript: integer; Args: string;
  Startup: boolean);
begin
  State := State + [asProcessing];
  OrdersChanged := True;
  // Disable all
  GlobalEnable(False);
  ItemGridTop := ItemGrid.TopRow;
  StopProcessorAction.Enabled := True;
  // Start order processor thread
  ThreadTerminate := ProcessorThreadTerminate;
  DoProcessOrders(ARegion, AScript, Args, Startup);
end;

// End processor
procedure TMainForm.ProcessorThreadTerminate(Sender: TObject);
var R: TRegion;
begin
  if (asFirstProcessing in State) and not OrdersLoaded then
    PostProcess(RerunRegions);

  State := State - [asProcessing, asFirstProcessing];
  if asCloseQuery in State then
    Close
  else begin
    if RerunRegions.Count > 0 then begin
      // Rerun modified region
      R := RerunRegions[0];
      RerunRegions.Delete(0);
      ProcessOrders(R);
    end
    else begin
      // Enable all
      GlobalEnable(True);
      NextErrorAction.Enabled := (ParseErrors.Count > 0);
      // Redraw all
      HexMap.Selected := HexMap.Selected;
      HexMap.Redraw;
      if MiniMapForm <> nil then
        MiniMapForm.MiniMap.Redraw;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TMainForm.StopProcessorActionExecute(Sender: TObject);
begin
  StopProcessOrders;
end;

  { ---------------- Globals ---------------------- }

procedure TMainForm.ApplyConfig;
var listmode: boolean;
    Flags: integer;
begin
  // Grids
  UnitGrid.LoadColumns(Config);
  ClearUnitFilter;

  // Tabs
  InfoPControl.ActivePageIndex := Config.ReadInteger('MainWin', 'MainTab', 0);
  UnitPControl.ActivePageIndex := Config.ReadInteger('MainWin', 'UnitTab', 0);
  RegionPControl.ActivePageIndex := Config.ReadInteger('MainWin', 'RegionTab', 0);

  // Controls setup
  GridModeAction.Checked := Config.ReadBool('MainWin', 'GridMode', False);
  ArrivingUnitsAction.Checked := Config.ReadBool('MainWin', 'ArrivingUnits', False);
  RegionNotesItm.Checked := Config.ReadBool('MainWin', 'RegionNotes', True);
  ShowToolbarItm.Checked := Config.ReadBool('MainWin', 'Toolbar', TRUE);
  ToolPanel.Visible := ShowToolbarItm.Checked;
  Maptools1.Checked := Config.ReadBool('MainWin', 'MapTools', TRUE);
  MapToolPanel.Visible := Maptools1.Checked;
  itmUnitTools.Checked := Config.ReadBool('MainWin', 'UnitTools', TRUE);
  tbUnitTools.Visible := itmUnitTools.Checked;
  InfoPanel1.Checked := Config.ReadBool('MainWin', 'InfoPanel', TRUE);
  InfoPanel.Visible := InfoPanel1.Checked;
  UnitsPanel.Height := Config.ReadInteger('MainWin', 'SplitterPos', 150);
  StructGrid.Width := Config.ReadInteger('MainWin', 'StructSplitterPos', 100);
  pItemGrid.Height := Config.ReadInteger('MainWin', 'ItemSplitterPos', 100);
  UnitPControl.Height := Config.ReadInteger('MainWin', 'UnitPageSplitterPos', 100);
  itmHideInvisRegions.Checked := Config.ReadBool('MainWin', 'HideInvisRegionSIC', False);
  btnLocal.Down := Config.ReadBool('MainWin', 'LocalDescriptions', False);
  gAllItems.Visible := Config.ReadBool('MainWin', 'AllItems', False);
  UnmodItemsAction.Checked := Config.ReadBool('MainWin', 'UnmodItemAmounts', False);

  // Mini map
  case Config.ReadInteger('MiniMap', 'Mode', mmGeo) of
    mmGeo:
      itmMiniGeo.Checked := True;
    mmPolitical:
      itmMiniPolitical.Checked := True;
    mmVisible:
      itmMiniVisible.Checked := True;
  end;

  // Map toolbar
  listmode := Config.ReadBool('Map', 'ListEnabled', False);
  ListBtn.Down := listmode;
  ListEnableItm.Checked := listmode;
  case Config.ReadInteger('Map', 'List', mliNone) of
    mliWanted:
      WantedListItm.Checked := TRUE;
    mliForSale:
      ForSaleListItm.Checked := TRUE;
    mliProducts:
      ProductsListItm.Checked := TRUE;
  end;
  FogBtn.Enabled := not listmode;
  FogDownBtn.Enabled := not listmode;
  FlagBtn.Enabled := not listmode;
  FlagDownBtn.Enabled := not listmode;
  MicBtn.Enabled := not listmode;
  MicDownBtn.Enabled := not listmode;
  SIcBtn.Enabled := not listmode;
  SIcDownBtn.Enabled := not listmode;
  STxBtn.Enabled := not listmode;
  STxDownBtn.Enabled := not listmode;
  StructBtn.Enabled := not listmode;

  FogBtn.Down := Config.ReadBool('Map', 'FogEnabled', False);
  case Config.ReadInteger('Map', 'FogType', fogNone) of
    fogNonVisited:
      FogVisitedItm.Checked := TRUE;
    fogNonVisible:
      FogVisibleItm.Checked := TRUE;
    fogCustom:
      FogCustomItm.Checked := TRUE;
  end;

  FlagBtn.Down := Config.ReadBool('Map', 'FlagsEnabled', True);
  Flags := Config.ReadInteger('Map', 'Flags', FLG_SELF + FLG_OTHER);
  SelfFlagItm.Checked := (Flags and FLG_SELF <> 0);
  OtherFlagsItm.Checked := (Flags and FLG_OTHER <> 0);
  OldFlagsItm.Checked := (Flags and FLG_OLD <> 0);
  ScoutsItm.Checked := (Flags and FLG_NOSELFSCOUTS <> 0);
  ScoutingFactionsItm.Checked := (Flags and FLG_NOSCOUTS <> 0);
  UnitDiagramItm.Checked := (Flags and FLG_UNIT_DIAGRAM <> 0);
  MenDiagramItm.Checked := (Flags and FLG_MEN_DIAGRAM <> 0);

  MIcBtn.Down := Config.ReadBool('Map', 'MIcEnabled', True);
  Flags := Config.ReadInteger('Map', 'MIc', MIC_MONSTER + MIC_GATE +
    MIC_BATTLE + MIC_WARNING + MIC_NOTES);
  MicMonstersItm.Checked := (Flags and MIC_MONSTER <> 0);
  MicGatesItm.Checked := (Flags and MIC_GATE <> 0);
  MicBattlesItm.Checked := (Flags and MIC_BATTLE <> 0);
  MicWarningsItm.Checked := (Flags and MIC_WARNING <> 0);
  MicNotesItm.Checked := (Flags and MIC_NOTES <> 0);

  SIcBtn.Down := Config.ReadBool('Map', 'SettlIconEnabled', True);
  case Config.ReadInteger('Map', 'SettlIcon', sicWeather) of
    sicSettlement:
      SicSettlItm.Checked := TRUE;
    sicWeather:
      SicWeatherItm.Checked := TRUE;
    sicPeasants:
      SicPeasantsItm.Checked := TRUE;
    sicGuard:
      SicGuardItm.Checked := TRUE;
    sicTaxTrade:
      SicTaxTradeItm.Checked := TRUE;
  end;

  STxBtn.Down := Config.ReadBool('Map', 'SettlTextEnabled', True);
  case Config.ReadInteger('Map', 'SettlText', stxSettlName) of
    stxSettlName:
      StxSettlItm.Checked := TRUE;
    stxPeasants:
      StxPeasantsItm.Checked := TRUE;
    stxPeasantsWCount:
      StxCountPeasantsItm.Checked := TRUE;
    stxTaxRate:
      StxTaxRateItm.Checked := TRUE;
    stxWages:
      StxWagesItm.Checked := TRUE;
    stxEntertain:
      StxEntertainItm.Checked := TRUE;
  end;

  StructBtn.Down := Config.ReadBool('Map', 'StructsEnabled', True);
  Flags := Config.ReadInteger('Map', 'Structs', ST_DEFENCE +
    ST_TRANSPORT + ST_CLOSED + ST_SHAFT + ST_ROAD + ST_UNKNOWN);
  StructTradeItm.Checked := Test(Flags, ST_UNKNOWN);
  StructDefItm.Checked := Test(Flags, ST_DEFENCE);
  StructTranspItm.Checked := Test(Flags, ST_TRANSPORT);
  StructClosedItm.Checked := Test(Flags, ST_CLOSED);
  StructInnerItm.Checked := Test(Flags, ST_SHAFT);
  StructRoadsItm.Checked := Test(Flags, ST_SHAFT);

  if Config.ReadBool('MiniMap', 'Visible', FALSE) = True then
    MiniMapAction.Execute;
end;

procedure TMainForm.GlobalEnable(Value: boolean);
var i: integer;
begin
  for i := 0 to ActionList.ActionCount-1 do
    TAction(ActionList.Actions[i]).Enabled := Value;
  StopProcessorAction.Enabled := False;
  if Value then AddExportAction.Enabled := (LastMapFile <> '');

  SetupEngine;

  InfoPControl.Enabled := Value;
  tsRegion.Enabled := Value;
  tsUnit.Enabled := Value;
  pStructure.Visible := Value;
  UnitsPanel.Enabled := Value;
  gAllItems.Enabled := Value;
  StructGrid.Enabled := Value;
  ToolBar.Enabled := Value;
  HexMap.Enabled := Value;
  for i := 0 to TurnsItm.Count-1 do
    TurnsItm.Items[i].Enabled := Value;
  for i := 0 to LevelItm.Count-1 do
    LevelItm.Items[i].Enabled := Value;
  for i := 0 to BookmarksItm.Count-1 do
    BookmarksItm.Items[i].Enabled := Value;
  for i := 0 to itmTerrain.Count-1 do
    itmTerrain.Items[i].Enabled := Value;
  if not Value then
    for i := 0 to ScriptsItm.Count-1 do
      if ScriptsItm.Items[i].Tag = 1 then ScriptsItm.Items[i].Enabled := Value;

  ItemGrid.NoRepaint := not Value;
  SkillGrid.NoRepaint := not Value;
  ProductGrid.NoRepaint := not Value;
  WantedGrid.NoRepaint := not Value;
  ForSaleGrid.NoRepaint := not Value;
end;

procedure TMainForm.UnitEnable(Value: boolean);
begin
  SoldiersAction.Enabled := Value;
  ExportSkillsAction.Enabled := Value and CurrUnit.Faction.Player;
end;

procedure TMainForm.StoreMemosInfo;
begin
  if OrderMemo.Focused then OrderMemoExit(OrderMemo);
  if NotesMemo.Focused then NotesMemoExit(NotesMemo);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var order: string;
    rep, dopop: boolean;
begin
  if CurrUnit = nil then Exit;
  if Mode <> mNormal then Exit;

  if (ActiveControl <> nil) and ((ActiveControl.ClassType = TMemo)
    or (ActiveControl.ClassType = TEdit)
    or (ActiveControl.ClassType = TIntEdit)) then Exit;

  order := FindOrder(RealKey(Key), Shift, rep);
  if order <> '' then begin
    if rep then Config.WriteBool('MainWin', 'RepeatingOrders',
      not Config.ReadBool('MainWin', 'RepeatingOrders', FALSE));

    if order = 'Distribute Needs' then Handlers.DistribNeeds(Self)
    else if order = 'Add as Attacker' then Handlers.AddAttacker(Self)
    else if order = 'Add Army as Attacker' then Handlers.AddArmyAttacker(Self)
    else if order = 'Add as Defender' then Handlers.AddDefender(Self)
    else if order = 'Clear Simulation' then Handlers.ClearSim(Self)

    else if CurrUnit.Faction.Player then begin
      if order = 'form new' then Handlers.Form(Self)
      else if order = 'continue build' then Handlers.Build(Self)
      else if order = 'teach' then Handlers.Teach(Self)
      else if order = 'claim' then Handlers.Claim(Self)
      else if order = 'tax' then Handlers.Tax(Self)
      else if order = 'entertain' then Handlers.Entertain(Self)
      else if order = 'work' then Handlers.Work(Self)
      else if order = 'leave' then Handlers.Leave(Self)
      else if order = 'advance' then Handlers.Advance(Self)
      else if order = 'move' then Handlers.Move(Self)
      else if order = 'sail' then Handlers.Sail(Self)
      else if order = 'advance army' then Handlers.ArmyAdvance(Self)
      else if order = 'move army' then Handlers.ArmyMove(Self)
      else if order = 'Declare Needs' then Handlers.DeclareNeeds(Self)
      else if order = 'Declare MOVE Route' then Handlers.DeclareRoute(Self)
      else if order = 'Declare SAIL Route' then Handlers.DeclareSailRoute(Self)
      else if order = 'Run Route' then Handlers.DoRunRoute(Self)
      else begin
        dopop := True;

        if order = 'enter' then
          MakeEnterMenu(CurrUnit, PopMenu.Items)
        else if order = 'promote' then
          MakeStructUnitsMenu(CurrUnit, PopMenu.Items, Handlers.Promote)
        else if order = 'evict' then
          MakeStructUnitsMenu(CurrUnit, PopMenu.Items, Handlers.Evict)
        else if order = 'study' then
          MakeStudyMenu(CurrUnit, PopMenu.Items)
        else if order = 'produce' then
          MakeProduceMenu(CurrUnit, PopMenu.Items)
        else if order = 'build' then
          MakeBuildMenu(CurrUnit, PopMenu.Items)
        else if order = 'cast' then
          MakeCastMenu(CurrUnit, PopMenu.Items)
        else if order = 'buy' then
          MakeBuyMenu(CurrUnit, PopMenu.Items)
        else if order = 'sell' then
          MakeSellMenu(CurrUnit, PopMenu.Items)
        else if order = 'attack' then
          MakeAttackMenu(CurrUnit, PopMenu.Items, Handlers.Attack)
        else if order = 'assassinate' then
          MakeAttackMenu(CurrUnit, PopMenu.Items, Handlers.Assassinate)
        else dopop := False;

        if dopop then PopMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
      end;
    end;

    if rep then Config.WriteBool('MainWin', 'RepeatingOrders',
      not Config.ReadBool('MainWin', 'RepeatingOrders', FALSE));
    Key := 0;
  end;
end;


  { ---------------- Modes ------------------------ }

procedure TMainForm.StartMoveMode(Army, Route: boolean);
begin
  if Mode = mMove then Mode := mNormal;
  HexMapGoto(CurrUnit.Region.Coords);
  GlobalEnable(False);
  // Enable level arrow to move thru shaft but disable level menu items
  HexMap.Enabled := True;
  StructGrid.Enabled := True;
  Mode := mMove;
  SetLength(Reached, 0);

  RouteMode.Enabled := Route;
  if Route then begin
    RouteMode.Route.Clear;
    SetLength(CurrUnit.Route, 0);
    RouteMode.Route.Order := MoveOrder;
    AddCoords(RouteMode.Route.Moves, CurrRegion.Coords);
  end
  else begin
    MoveArmy := Army;
    SetLength(Reached, 0);
    ClearMoves;
    AddCoords(CurrUnit.Moves, CurrRegion.Coords);
    MovePoints := ArmyMovesLeft(CurrUnit);
    DoCalcReachedRegions(CurrRegion.Coords, CurrUnit);
  end;

  with HexMap do begin
    Cursor := crMove;
    Options := Options - [hmShowSelection, hmDblClickCenter];
    Redraw;
  end;
end;

// Shaft parameter is number of non-linked shaft unit enters
procedure TMainForm.EndMoveMode;
begin
  if RouteMode.Enabled then begin
    SetRoute(CurrUnit, RouteMode.Route);
    RouteMode.Enabled := False;
    RunRoute(CurrUnit);
  end
  else SpreadMoveOrders(MoveOrder);

  GlobalEnable(True);
  Mode := mNormal;
  with HexMap do begin
    Cursor := crDefault;
    Options := Options + [hmShowSelection, hmDblClickCenter];
    HexMapGoto(CurrUnit.Region.Coords);
  end;

  MoveOrder := '';
  ProcessOrders(CurrUnit.Region);
end;

procedure TMainForm.btnLinkShaftClick(Sender: TObject);
begin
  StartLinkMode;
end;

procedure TMainForm.StartLinkMode;
var i: integer;
begin
  GlobalEnable(False);
  HexMap.Enabled := True;
  LevelAction.Enabled := True;
  for i := 0 to LevelItm.Count-1 do
    LevelItm.Items[i].Enabled := True;
  ActingStruct := CurrStruct;
  ActingRegion := CurrRegion;
  Mode := mLinkShaft;
  HexMap.Cursor := crLinkShaft;
end;

procedure TMainForm.EndLinkMode(HX, HY: integer);
var mapX, mapY, i: integer;
    Struct: TStruct;
    Region: TRegion;
begin
  GlobalEnable(True);
  Mode := mNormal;
  HexMap.Cursor := crDefault;
  CalcMapCoords(HX, HY, mapX, mapY);
  if not EqualCoords(ActingRegion.Coords, Coords(mapX, mapY, Map.Level)) then
    ActingStruct.Passage := Coords(mapX, mapY, Map.Level)
  else ActingStruct.Passage.z := -1;
  // Spread in all turns
  for i := 1 to Game.Turns.Count-1 do begin
    Map.TurnNum := Game.Turns[i].Num;
    Region := Map.Region(ActingRegion.Coords);
    if Region <> nil then begin
      Struct := Region.Structs.Find(ActingStruct.Num);
      if Struct <> nil then
        Struct.Passage := ActingStruct.Passage;
    end;
    Map.TurnNum := 0;
  end;
  ActingStruct := nil;
  ActingRegion := nil;
end;

  { ---------------- HexMap ------------------------ }

procedure TMainForm.HexMapSetup;
var i: integer;
begin
  uHexMap.HexMapSetup(HexMap);
  for i := 0 to LevelItm.Count-1 do
    if StringReplace(LevelItm[i].Caption, '&', '', []) =
      Map.Levels[Map.Level].Name then
      LevelItm[i].Checked := True;
  if MiniMapForm <> nil then MiniMapForm.OnActivate(nil);
end;

procedure TMainForm.HexMapGoto(C: TCoords; DefaultHex: boolean);
begin
  GameConfig.WriteInteger('Map', 'Level', C.z);
  if not DefaultHex then begin
    GameConfig.WriteInteger('Map', 'SelX_' + Map.Levels[C.z].Name, C.x);
    GameConfig.WriteInteger('Map', 'SelY_' + Map.Levels[C.z].Name, C.y);
  end;
  HexMapSetup;
end;

procedure TMainForm.HexMapGoto(C: TCoords);
begin
  HexMapGoto(C, False);
end;

procedure TMainForm.HexMapDrawExtra(Sender: TObject; HX, HY: Integer;
  ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
var mapX, mapY: integer;
    Region: TRegion;
begin
  if NoDraw then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);
  Region := Map.Region(mapX, mapY);
  if Mode = mMove then DrawMoveState(Region, ACanvas, mapX, mapY, cx, cy);
  if Region <> nil then DrawExtra(ACanvas, CX, CY, Region, (Mode = mMove));
  if CurrUnit <> nil then
    DrawPathArrows(ACanvas, mapX, mapY, cx, cy);
end;

procedure TMainForm.HexMapDrawHex(Sender: TObject; HX, HY: Integer;
  ACanvas: TCanvas; CX, CY: Integer; AState: TCylinderMapDrawState);
var mapX, mapY: integer;
    Region: TRegion;
begin
  if NoDraw then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);
  Region := Map.Region(mapX, mapY);
  if Region <> nil then
    DrawHex(ACanvas, CX, CY, Region);
end;

procedure TMainForm.HexMapMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var Hex: TPoint;
begin
  if Game = nil then Exit;
  Hex := HexMap.MouseToHex(X, Y);
  if (Hex.X >= 0) and (Hex.Y >= 0) and (Hex.X < HexMap.ColCount) and
    (Hex.Y < HexMap.RowCount) then begin
    CalcMapCoords(Hex.X, Hex.Y, Hex.X, Hex.Y);
    CoordLabel.Caption := IntToStr(Hex.X)+', '+IntToStr(Hex.Y);
    if Config.ReadBool('MainWin', 'RegionNotes', True) and
      (Map.Region(Hex.X, Hex.Y) <> nil) and (Map.Region(Hex.X,
      Hex.Y).Notes.Text <> '') then begin
      HexMap.Hint := Trim(Map.Region(Hex.X, Hex.Y).Notes.Text);
      HexMap.ShowHint := True;
    end
    else HexMap.ShowHint := False;
  end;
end;

procedure TMainForm.HexMapMoveMap(Sender: TObject; X, Y: Integer);
begin
  if MiniMapForm <> nil then MiniMapForm.MiniMap.Repaint;
end;

procedure TMainForm.HexMapDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if not ((Source = ItemGrid) or (Source is TToolButton)) then
    Accept := False;
end;

procedure TMainForm.HexMapMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P: TPoint;
begin
  if Button = mbLeft then begin
    P := HexMap.MouseToHex(X, Y);
    // End link mode
    if Mode = mLinkShaft then EndLinkMode(P.X, P.Y);
    // Disable struct filter
    Filter.StructFilter := False;
  end;
  MapDblClick := (ssDouble in Shift);
end;

procedure TMainForm.HexMapMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // End route declaration
  if MapDblClick and (Mode = mMove) and RouteMode.Enabled then begin
    if CtrlPressed then RouteMode.Route.OneWay := True;
    EndMoveMode;
  end;
end;

procedure TMainForm.HexMapSelectHex(Sender: TObject; HX, HY: Integer);
var mapX, mapY: integer;
begin
  if NoDraw then Exit;
  CalcMapCoords(HX, HY, mapX, mapY);

  if Mode = mNormal then begin
    // Setup CurrRegion variable
    CurrRegion := Map.Region(mapX, mapY);
    BattlesAction.Enabled := (CurrRegion <> nil) and (CurrRegion.Battles <> nil);
    // Write config
    GameConfig.WriteInteger('Map', 'SelX_' + Map.Levels[Map.Level].Name, mapX);
    GameConfig.WriteInteger('Map', 'SelY_' + Map.Levels[Map.Level].Name, mapY);
    // Fill panels for selected region
    if CurrRegion <> nil then FillRegionInfo(Map.Region(mapX, mapY))
    else ClearRegionInfo;
    FillStructGrid(CurrRegion);
    FillUnitGrid;
  end;

  if Mode = mMove then begin
    if SelectMoveModeHex(Coords(mapX, mapY, Map.Level)) then
      FillStructGrid(Map.Region(mapX, mapY));
  end;
end;



  { --------------- HexMap Toolbar ----------------  }


function TMainForm.GetMapToolItm(Index: integer): TMenuItem;
begin
  case Index of
    0:   Result := ListItm;
    1:   Result := FogItm;
    2:   Result := FlagItm;
    3:   Result := MIcItm;
    4:   Result := SIcItm;
    5:   Result := STxItm;
    6:   Result := LevelItm;
    7:   Result := BookmarksItm;
    else Result := StructItm;
  end;
end;

function TMainForm.GetMapToolBtn(Index: integer): TToolButton;
begin
  case Index of
    0:   Result := ListBtn;
    1:   Result := FogBtn;
    2:   Result := FlagBtn;
    3:   Result := MIcBtn;
    4:   Result := SIcBtn;
    5:   Result := STxBtn;
    6:   Result := LevelBtn;
    7:   Result := BookmarkBtn;
    else Result := StructBtn;
  end;
end;

procedure TMainForm.DownBtnClick(Sender: TObject);
var MenuItem: TMenuItem;
    NewItem: TMenuItem;
    i, start: integer;
begin
  MenuItem := GetMapToolItm(TMenuItem(Sender).Tag);
  // Create popup menu from MainMenu's submenu
  PopMenu.Items.Clear;
  if (MenuItem.Count > 0) and (MenuItem.Items[0].Caption = 'Enabled') then
    start := 2
  else start := 0;
  for i := start to MenuItem.Count-1 do begin
    NewItem := TMenuItem.Create(PopMenu);
    NewItem.Caption := MenuItem.Items[i].Caption;
    NewItem.Tag := MenuItem.Items[i].Tag;
    NewItem.RadioItem := MenuItem.Items[i].RadioItem;
    NewItem.Checked := MenuItem.Items[i].Checked;
    NewItem.OnClick := MenuItem.Items[i].OnClick;
    NewItem.Enabled := MenuItem.Items[i].Enabled;
    NewItem.ImageIndex := -1;
    PopMenu.Items.Add(NewItem);
  end;
  PopMenu.Images := nil;
  with GetMapToolBtn(TMenuItem(Sender).Tag) do
    PopMenu.Popup(ClientToScreen(Point(0, Height)).X,
      ClientToScreen(Point(0, Height)).Y);
end;

procedure TMainForm.GroupItmClick(Sender: TObject);
var MenuItem: TMenuItem;
    i: integer;
begin
  // Check in MainMenu
  MenuItem := GetMapToolItm(TMenuItem(Sender).Tag);
  for i := 0 to MenuItem.Count-1 do
    if MenuItem.Items[i].Caption = TMenuItem(Sender).Caption then begin
      if MenuItem.Items[i].RadioItem then
        MenuItem.Items[i].Checked := TRUE
      else MenuItem.Items[i].Checked := not MenuItem.Items[i].Checked;
    end;
  // Check in popup menu
  for i := 0 to PopMenu.Items.Count-1 do
    if PopMenu.Items[i].Caption = TMenuItem(Sender).Caption then
      if PopMenu.Items[i].RadioItem then
        PopMenu.Items[i].Checked := TRUE
      else PopMenu.Items[i].Checked := not PopMenu.Items[i].Checked;
  with GetMapToolBtn(TMenuItem(Sender).Tag) do begin
    Down := TRUE;
    Click;
  end;
end;

procedure TMainForm.MapToolEnableClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
  with GetMapToolBtn(TMenuItem(Sender).Tag) do begin
    Down := TMenuItem(Sender).Checked;
    Click;
  end;
end;

procedure TMainForm.ListBtnClick(Sender: TObject);
begin
  ListEnableItm.Checked := ListBtn.Down;
  Config.WriteBool('Map', 'ListEnabled', ListBtn.Down);
  if WantedListItm.Checked then
    Config.WriteInteger('Map', 'List', mliWanted);
  if ForSaleListItm.Checked then
    Config.WriteInteger('Map', 'List', mliForSale);
  if ProductsListItm.Checked then
    Config.WriteInteger('Map', 'List', mliProducts);

  // List mode
  FogBtn.Enabled := not ListBtn.Down;
  FogDownBtn.Enabled := not ListBtn.Down;
  FlagBtn.Enabled := not ListBtn.Down;
  FlagDownBtn.Enabled := not ListBtn.Down;
  MicBtn.Enabled := not ListBtn.Down;
  MicDownBtn.Enabled := not ListBtn.Down;
  SIcBtn.Enabled := not ListBtn.Down;
  SIcDownBtn.Enabled := not ListBtn.Down;
  STxBtn.Enabled := not ListBtn.Down;
  STxDownBtn.Enabled := not ListBtn.Down;
  StructBtn.Enabled := not ListBtn.Down;
  HexMap.Redraw;
end;

procedure TMainForm.FogBtnClick(Sender: TObject);
begin
  FogEnableItm.Checked := FogBtn.Down;
  Config.WriteBool('Map', 'FogEnabled', FogBtn.Down);
  if FogVisibleItm.Checked then
    Config.WriteInteger('Map', 'FogType', fogNonVisible);
  if FogVisitedItm.Checked then
    Config.WriteInteger('Map', 'FogType', fogNonVisited);
  if FogCustomItm.Checked then
    Config.WriteInteger('Map', 'FogType', fogCustom);
  HexMap.Redraw;
end;

procedure TMainForm.FlagBtnClick(Sender: TObject);
var Flags: DWord;
begin
  FlagEnableItm.Checked := FlagBtn.Down;
  Flags := 0;
  if SelfFlagItm.Checked then Flags := Flags or FLG_SELF;
  if OtherFlagsItm.Checked then Flags := Flags or FLG_OTHER;
  if OldFlagsItm.Checked then Flags := Flags or FLG_OLD;
  if ScoutsItm.Checked then Flags := Flags or FLG_NOSELFSCOUTS;
  if ScoutingFactionsItm.Checked then Flags := Flags or FLG_NOSCOUTS;
  if MenDiagramItm.Checked then Flags := Flags or FLG_MEN_DIAGRAM;
  if UnitDiagramItm.Checked then Flags := Flags or FLG_UNIT_DIAGRAM;
  Config.WriteBool('Map', 'FlagsEnabled', FlagBtn.Down);
  Config.WriteInteger('Map', 'Flags', Flags);
  HexMap.Redraw;
end;

procedure TMainForm.MIcBtnClick(Sender: TObject);
var Mic: DWord;
begin
  MIcEnableItm.Checked := MIcBtn.Down;
  Mic := 0;
  if MIcMonstersItm.Checked then Mic := Mic or MIC_MONSTER;
  if MIcGatesItm.Checked then Mic := Mic or MIC_GATE;
  if MIcBattlesItm.Checked then Mic := Mic or MIC_BATTLE;
  if MIcWarningsItm.Checked then Mic := Mic or MIC_WARNING;
  if MIcNotesItm.Checked then Mic := Mic or MIC_NOTES;
  Config.WriteBool('Map', 'MIcEnabled', MicBtn.Down);
  Config.WriteInteger('Map', 'MIc', Mic);
  HexMap.Redraw;
end;

procedure TMainForm.SIcBtnClick(Sender: TObject);
begin
  SIcEnableItm.Checked := SIcBtn.Down;
  Config.WriteBool('Map', 'SettlIconEnabled', SicBtn.Down);
  if SIcSettlItm.Checked then Config.WriteInteger('Map', 'SettlIcon', sicSettlement);
  if SIcWeatherItm.Checked then Config.WriteInteger('Map', 'SettlIcon', sicWeather);
  if SIcPeasantsItm.Checked then Config.WriteInteger('Map', 'SettlIcon', sicPeasants);
  if SIcGuardItm.Checked then Config.WriteInteger('Map', 'SettlIcon', sicGuard);
  if SIcTaxTradeItm.Checked then Config.WriteInteger('Map', 'SettlIcon', sicTaxTrade);
  HexMap.Redraw;
end;

procedure TMainForm.STxBtnClick(Sender: TObject);
begin
  STxEnableItm.Checked := STxBtn.Down;
  Config.WriteBool('Map', 'SettlTextEnabled', StxBtn.Down);
  if StxSettlItm.Checked then Config.WriteInteger('Map', 'SettlText', stxSettlName);
  if StxPeasantsItm.Checked then Config.WriteInteger('Map', 'SettlText', stxPeasants);
  if StxCountPeasantsItm.Checked then Config.WriteInteger('Map', 'SettlText', stxPeasantsWCount);
  if StxTaxRateItm.Checked then Config.WriteInteger('Map', 'SettlText', stxTaxRate);
  if StxWagesItm.Checked then Config.WriteInteger('Map', 'SettlText', stxWages);
  if StxEntertainItm.Checked then Config.WriteInteger('Map', 'SettlText', stxEntertain);
  HexMap.Redraw;
end;

procedure TMainForm.StructBtnClick(Sender: TObject);
var Flags: DWord;
begin
  StructEnabledItm.Checked := StructBtn.Down;
  Flags := 0;
  SetFlag(Flags, ST_UNKNOWN, StructTradeItm.Checked);
  SetFlag(Flags, ST_DEFENCE, StructDefItm.Checked);
  SetFlag(Flags, ST_TRANSPORT, StructTranspItm.Checked);
  SetFlag(Flags, ST_CLOSED, StructClosedItm.Checked);
  SetFlag(Flags, ST_SHAFT, StructInnerItm.Checked);
  SetFlag(Flags, ST_ROAD, StructRoadsItm.Checked);
  Config.WriteBool('Map', 'StructsEnabled', StructBtn.Down);
  Config.WriteInteger('Map', 'Structs', Flags);
  HexMap.Redraw;
end;

procedure TMainForm.LevelActionExecute(Sender: TObject);
begin
  repeat
    Inc(Map.Level);
    if Map.Level = Map.Levels.Count then Map.Level := 0;
  until not Map.Levels[Map.Level].Empty;
  HexMapGoto(Coords(0, 0, Map.Level), True);
end;

procedure TMainForm.ListFilterActionExecute(Sender: TObject);
begin
  ListFilterForm := TListFilterForm.Create(Self);
  ListFilterForm.ShowModal;
  ListFilterForm.Free;
  HexMap.Redraw;
end;

procedure TMainForm.MiniMapModeClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked; 
  if itmMiniGeo.Checked then
    Config.WriteInteger('MiniMap', 'Mode', mmGeo);
  if itmMiniPolitical.Checked then
    Config.WriteInteger('MiniMap', 'Mode', mmPolitical);
  if itmMiniVisible.Checked then
    Config.WriteInteger('MiniMap', 'Mode', mmVisible);
  if MiniMapForm <> nil then
    MiniMapForm.MiniMap.Redraw;
end;


  { --------------- Regions Tab ----------------------  }

procedure TMainForm.ClearRegionInfo;
begin
 // Header
  LandLabel.Caption := '';
  GateImage.Visible := FALSE;
  GateLabel.Caption := '';
 // Main
  HexLabel.Caption := 'unknown';
  SettlLabel.Caption := '';
  PeasantsLabel.Caption := '';
  TaxRateLabel.Caption := '';
  TaxMenLabel.Caption := '';
  WagesLabel.Caption := '';
  MaxWagesLabel.Caption := '';
  WorkLabel.Caption := '';
  EnterLabel.Caption := '';
  EnterMenLabel.Caption := '';
  WeatherLabel.Caption := '';
  imgWeatherNext.Picture.Bitmap.Canvas.FillRect(Rect(0, 0, 10, 10));
  NotesMemo.Lines.Clear;
  NotesMemo.Modified := False;
 // Grids
  WantedGrid.RowCount := 0;
  WantedGrid.Fixup;
  ForSaleGrid.RowCount := 0;
  ForSaleGrid.Fixup;
  ProductGrid.RowCount := 0;
  ProductGrid.Fixup;
end;

procedure TMainForm.FillRegionInfo(ARegion: TRegion);
var Weather: TWeatherData;
    RealRegion: TRegion;
    amt, tax_income, ent_income: integer;

  procedure FillDiffItemGrid(Grid: TPowerGrid; List, RealList: TItemList);
  var i: integer;
      Item: TItem;
  begin
    Grid.RowCount := 0;
    for i := 0 to RealList.Count-1 do begin
      Item := RealList[i];
      if Item.Amount > 0 then begin
        if Item.Amount = List[i].Amount then
          Grid.Cells[0, i] := IntToStr(Item.Amount)
        else Grid.Cells[0, i] := IntToStr(List[i].Amount) + '/' + IntToStr(Item.Amount);
      end;
      Grid.Cells[1, i] := Item.Name;
      if Grid.ColCount > 2 then Grid.Cells[2, i] := '$' + IntToStr(Item.Cost);
      Grid.Rows[i].Data := Item;
      if Item.Amount <> 0 then begin
        Grid.Rows[i].Color := clWindowText;
        Grid.Rows[i].FontStyle := [];
      end
      else begin
        Grid.Rows[i].Color := clGrayText;
        Grid.Rows[i].FontStyle := [fsStrikeOut];
      end;
    end;
    Grid.Fixup;
  end;

begin
  tax_income := GameConfig.ReadInteger('Settings', 'TaxIncome', 50);
  ent_income := GameConfig.ReadInteger('Settings', 'EntertainIncome', 20);

  with ARegion do begin
    RealRegion := Map.Region(X, Y, Z, Visited);
   // Panel header
    LandLabel.Caption := Land;
    if Gate <> 0 then begin
      GateImage.Visible := TRUE;
      if Gate > 0 then GateLabel.Caption := IntToStr(Gate)
      else GateLabel.Caption := '';
    end
    else begin
      GateImage.Visible := FALSE;
      GateLabel.Caption := '';
    end;

   // Main data
    HexLabel.Caption := Terrain.Name;
    if Map.Levels[z].Name <> 'nexus' then
      HexLabel.Caption := HexLabel.Caption + ' (' + IntToStr(x)+','+IntToStr(y) + ')';
    if Visited <> Turn.Num then
      HexLabel.Caption := HexLabel.Caption + ', ' + TurnToShortDate(Visited);
    if SettlementType <> 0 then
       SettlLabel.Caption := Settlement + ' ' + GetKey(s_Village, SettlementType - 1)
    else SettlLabel.Caption := '';
    if Peasants <> nil then
      PeasantsLabel.Caption := IntToStr(Peasants.Amount) + ' ' + Peasants.Data.MultiName
    else PeasantsLabel.Caption := '';

    TaxRateLabel.Caption := IntToStr(RealRegion.TaxRate);
    TaxMenLabel.Caption := '';
    amt := ARegion.Activity.Taxers;
    if amt > 0 then
      TaxMenLabel.Caption := TaxMenLabel.Caption + IntToStr(amt) + ' / ';
    TaxMenLabel.Caption := TaxMenLabel.Caption +
      IntToStr(Ceil(RealRegion.TaxRate / tax_income));
    if (amt > 0) and (amt * tax_income > RealRegion.TaxRate) then
      TaxMenLabel.Caption := TaxMenLabel.Caption + ' (' +
        IntToStr(RealRegion.TaxRate div amt) + ')';

    WagesLabel.Caption := IntToStr(Wages);
    if RealRegion.MaxWages > 0 then
      MaxWagesLabel.Caption := IntToStr(RealRegion.MaxWages)
    else MaxWagesLabel.Caption := '';
    WorkLabel.Caption := '';
    amt := ARegion.Activity.Workers;
    if amt > 0 then
      WorkLabel.Caption := WorkLabel.Caption + IntToStr(amt) + ' / ';
    if Wages > 0 then
      WorkLabel.Caption := WorkLabel.Caption +
        IntToStr(Ceil(RealRegion.MaxWages / Wages))
    else WorkLabel.Caption := WorkLabel.Caption + '0';
    if (amt > 0) and (amt * Wages > RealRegion.MaxWages) then
      WorkLabel.Caption := WorkLabel.Caption + ' (' +
        IntToStr(RealRegion.MaxWages div amt) + ')';

    EnterLabel.Caption := IntToStr(RealRegion.Entertainment);
    EnterMenLabel.Caption := '';
    amt := ARegion.Activity.Entertainers;
    if amt > 0 then
      EnterMenLabel.Caption := EnterMenLabel.Caption + IntToStr(amt) + ' / ';
    EnterMenLabel.Caption := EnterMenLabel.Caption +
      IntToStr(Ceil(RealRegion.Entertainment / ent_income));
    if (amt > 0) and (amt * ent_income > RealRegion.Entertainment) then
      EnterMenLabel.Caption := EnterMenLabel.Caption + ' ('
        + IntToStr(RealRegion.Entertainment div amt) + ')';

    Weather := nil;
    if (WeatherLast <> nil) and (WeatherNext <> nil) then begin
      WeatherLabel.Caption := WeatherLast.LastText + ' was ' +
        WeatherLast.Text + ' last month; ' + WeatherNext.NextText + ' will be ' +
        WeatherNext.Text + ' next month.';
      Weather := WeatherNext;
    end
    else WeatherLabel.Caption := '';
    if Weather = nil then Weather := GetWeather(Coords);
    with imgWeatherNext.Picture.Bitmap do begin
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect(Rect(0, 0, 10, 10));
      if Weather <> nil then ResForm.Extras.Draw(Canvas, 0, 0,
        bmp_extWeather + 1 + Game.WeatherData.IndexOf(Weather));
    end;
    NotesMemo.Lines.Assign(Notes);

   // Wanted, For Sale, Products
    FillDiffItemGrid(WantedGrid, Wanted, RealRegion.Wanted);
    FillDiffItemGrid(ForSaleGrid, ForSale, RealRegion.ForSale);
    FillDiffItemGrid(ProductGrid, Products, RealRegion.Products);
  end;
end;

procedure TMainForm.TradePanelResize(Sender: TObject);
var gh: integer;
begin
  gh := (TradePanel.Height - 45) div 3;
  if (gh > 0) then begin
    ProductGrid.Height := gh;
    WantedGrid.Height := gh;
    ForSaleGrid.Height := gh;
  end;
end;

procedure TMainForm.NotesMemoExit(Sender: TObject);
var i: integer;
    R: TRegion;
begin
  if NotesMemo.Modified then begin
    // Spread notes between all region copies
    for i := 0 to Game.Turns.Count-1 do begin
      Map.TurnNum := Game.Turns[i].Num;
      R := Map.Region(CurrRegion.x, CurrRegion.y);
      if R <> nil then R.Notes.Assign(NotesMemo.Lines);
    end;
    Map.TurnNum := 0;
    HexMap.Redraw;
  end;
end;

  { -------------- Structs and Struct Info ------------- }

procedure TMainForm.FillStructGrid(ARegion: TRegion);
var i, idx: integer;
    SData: TStructData;
begin
  ClearStructInfo;
  StructGrid.RowCount := 0;
  StructGrid.Cols[0].Format := cfNumber;
  if ARegion <> nil then begin
    for i := 0 to ARegion.Structs.Count-1 do
      if (ARegion.Visited = Turn.Num) or
        not Test(ARegion.Structs[i].Data.Flags, ST_TRANSPORT) then begin
      SData := ARegion.Structs[i].Data;
      idx := Game.StructData.IndexOf(SData) * 100000 + ARegion.Structs[i].Num;
      StructGrid.Cells[0, i] := IntToStr(idx);
      StructGrid.Cells[1, i] := ARegion.Structs[i].Name +
        ' [' + IntToStr(ARegion.Structs[i].Num) + '] : ' +
        ARegion.Structs[i].Data.Group;
      StructGrid.Rows[i].Data := ARegion.Structs[i];
    end;
  end;
  StructGrid.Fixup;
end;

procedure TMainForm.StructGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
var Struct: TStruct;
    fontname: string;
    fontsize: integer;
begin
  if NoDraw then Exit;
  Struct := TStruct(StructGrid.ImgRows[ARow].Data);
  with StructGrid.Canvas do begin
    if Struct <> CurrStruct then begin
      Brush.Color := clBtnFace;
      Font.Color := clWindowText;
      FillRect(Rect(TxtRect.Left, TxtRect.Top, TxtRect.Right + 2,
        TxtRect.Bottom + 1));
    end;
    if ACol = 1 then begin
      if not Test(Struct.Data.Flags, ST_SHAFT) or ((CurrRegion <> nil)
        and (CurrRegion.Visited = Turn.Num)) then
        DrawOwnedStruct(StructGrid.Canvas, TxtRect.Left+3, TxtRect.Top+1, Struct)
      else DrawStructIcon(StructGrid.Canvas, TxtRect.Left+3, TxtRect.Top+1,
        Struct.Data, Struct.HasExit);
      if Struct.Needs > 0 then begin
        fontname := Font.Name;
        fontsize := Font.Size;
        Font.Name := 'Small Fonts';
        Font.Size := 6;
          TextOut(TxtRect.Left + 12, TxtRect.Top + 10, IntToStr(Struct.Needs));
        Font.Name := fontname;
        Font.Size := fontsize;
      end;
      TxtRect.Left := TxtRect.Left + 23;
    end;
  end;
end;

procedure TMainForm.StructGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Struct: TStruct;
    i, mapX, mapY: integer;
    R: TRegion;
begin
  if StructGrid.MouseCell.Y < 0 then Exit;
  Struct := TStruct(StructGrid.ImgRows[StructGrid.MouseCell.Y].Data);

  // Move mode
  if (Mode = mMove) and (MoveOrder <> 'sail') then begin
    // Enter structure
    if RouteMode.Enabled then begin
      if Test(Struct.Data.Flags, ST_SHAFT) and Struct.HasExit then begin
        AddCoords(RouteMode.Route.Moves, Struct.Passage);
        if CtrlPressed then
          AddInt(MainForm.RouteMode.Route.Stops, High(RouteMode.Route.Moves) + 1);
        CurrUnit.Route := RouteMode.Route.Moves;
        HexMapGoto(Struct.Passage)
      end
      else begin
        if Test(Struct.Data.Flags, ST_SHAFT) then
          RouteMode.Route.Remaining := IntToStr(Struct.Num);
          RouteMode.Route.Remaining := RouteMode.Route.Remaining + ' IN';
        EndMoveMode;
      end;
    end
    else begin
      if CurrUnit.Struct <> Struct then begin
        if Length(CurrUnit.Moves) = 1 then GetArmyOut;
        AddOrder(MoveOrder + ' ' + IntToStr(Struct.Num), True);
        CurrUnit.MonthOrder := CurrUnit.Orders[CurrUnit.Orders.Count-1];
        AddCoords(CurrUnit.Moves, SelectedCoords);
      end;
      if Test(Struct.Data.Flags, ST_SHAFT) then begin
        // If inside unlinked shaft
        if (CurrUnit.Struct = Struct) and not Struct.HasExit then begin
          // Look for other shafts
          CalcMapCoords(HexMap.Selected.X, HexMap.Selected.Y, mapX, mapY);
          R := Map.Region(mapX, mapY);
          if R <> nil then begin
            i := R.Structs.Count-1;
            while (i >= 0) and (not Test(R.Structs[i].Data.Flags, ST_SHAFT)
              or (R.Structs[i] = Struct)) do Dec(i);
            if i >= 0 then begin
              // leave and re-enter to produce event
              AddOrder('leave', False);
              AddOrder(MoveOrder + ' ' + IntToStr(Struct.Num), True);
              CurrUnit.MonthOrder := CurrUnit.Orders[CurrUnit.Orders.Count-1];
              AddCoords(CurrUnit.Moves, SelectedCoords);
            end;
          end;
        end;
        // Move IN
        AddOrder(MoveOrder + ' IN', True);
        CurrUnit.MonthOrder := CurrUnit.Orders[CurrUnit.Orders.Count-1];
        if Struct.HasExit then HexMapGoto(Struct.Passage)
        else EndMoveMode;
      end
      else EndMoveMode;
    end;
  end

  // Normal mode
  else begin
    // Filter units in grid
    Filter.RegCoords := CurrRegion.Coords;
    Filter.StructNum := Struct.Num;
    Filter.StructFilter := True;
    FillUnitGrid;
    FillStructInfo(Struct);

    // Move through shaft
    if (ssDouble in Shift) and Struct.HasExit then begin
      Filter.StructFilter := False;
      HexMapGoto(Struct.Passage);
    end;
  end;
end;

procedure TMainForm.StructGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var Struct: TStruct;
begin
  with StructGrid do begin
    if (Mode = mMove) and (MoveOrder <> 'sail') and (MouseCell.Y >= 0)
      and (MouseCell.Y < RowCount) then
      Cursor := crMove
    else Cursor := crDefault;
    // Passage hint
    if (MouseCell.Y >= 0) and (MouseCell.Y < RowCount) then begin
      Struct := TStruct(ImgRows[MouseCell.Y].Data);
      if Struct.HasExit then begin
        Hint := MakeRegionName(Struct.Passage, True);
        ShowHint := True;
      end
      else begin
        Hint := '';
        ShowHint := False;
      end;
    end;
  end;
end;

procedure TMainForm.ClearStructInfo;
begin
  CurrStruct := nil;
  StructNameEdit.Text := '';
  StructNameEdit.ReadOnly := TRUE;
  StructDescrEdit.Text := '';
  StructDescrEdit.ReadOnly := TRUE;
  StructNumLabel.Caption := '';
  StructGroupLabel.Caption := '';
  StructOwnerLabel.Caption := '';
  OwnerFlagImage.Visible := FALSE;
  StructDefLabel.Caption := '';
  StructMenLabel.Caption := '';
  StructSizelabel.Caption := '';
  IncomplStructImage.Visible := FALSE;
  StructLoadLabel.Caption := '';
  StructControlLabel.Caption := '';
  btnLinkShaft.Enabled := False;
  lWayTo.Caption := '';
end;

procedure TMainForm.FillStructInfo(AStruct: TStruct);
var i, j, k, men, control: integer;
    AUnit: TUnit;
    R: TRegion;
begin
  CurrStruct := AStruct;
  with AStruct do begin
    if (CurrRegion <> nil) and (CurrRegion.Structs.IndexOf(AStruct) >= 0) then
      R := CurrRegion
    else if (CurrUnit <> nil)
      and (CurrUnit.Region.Structs.IndexOf(AStruct) >= 0) then
      R := CurrUnit.Region
    else R := nil;

    StructNameEdit.Text := Name;
    StructNameEdit.Modified := FALSE;
    StructNameEdit.ReadOnly := TRUE;
    StructNumLabel.Caption := IntToStr(Num);
    StructDescrEdit.Text := Description;
    StructDescrEdit.Modified := FALSE;
    StructDescrEdit.ReadOnly := TRUE;
    StructGroupLabel.Caption := Data.Group;
    btnLinkShaft.Enabled := Test(Data.Flags, ST_SHAFT);
    if Test(Data.Flags, ST_SHAFT) and HasExit then
      lWayTo.Caption := MakeRegionName(Passage, True)
    else lWayTo.Caption := '';

    if Owner = nil then begin
      StructOwnerLabel.Caption := '';
      OwnerFlagImage.Visible := FALSE;
    end
    else begin
      StructOwnerLabel.Caption := Owner.Name + ' (' + Owner.NumStr + ')';
      OwnerFlagImage.Visible := TRUE;
      DrawCExtra(extFlag, Owner.Faction, OwnerFlagImage.Canvas, 0, 0);
      StructNameEdit.ReadOnly := (Owner.Faction.Num <> Faction.Num);
      StructDescrEdit.ReadOnly := (Owner.Faction.Num <> Faction.Num);
    end;
    StructDefLabel.Caption := IntToStr(Data.Protection);
    men := 0;
    control := 0;
    if R <> nil then
      for i := 0 to R.Troops.Count-1 do
        for j := 0 to R.Troops[i].Units.Count-1 do begin
          AUnit := R.Troops[i].Units[j];
          if AUnit.Struct = AStruct then begin
            men := men + AUnit.Items.Amount(IT_MAN);
            for k := 0 to AUnit.Skills.Count-1 do
              if AUnit.Skills[k].Data.Short = Keys[s_Sailing] then
                control := control + AUnit.Items.Amount(IT_MAN) * AUnit.Skills[k].Level;
          end;
        end;
    StructMenLabel.Caption := IntToStr(men);
    if Needs > 0 then begin
      StructSizeLabel.Caption := IntToStr(Needs) + ' / ' + IntToStr(Data.Size);
      IncomplStructImage.Visible := TRUE;
    end
    else begin
      StructSizeLabel.Caption := IntToStr(Data.Size);
      IncomplStructImage.Visible := FALSE;
    end;
    if Data.Flags and ST_TRANSPORT <> 0 then begin
      StructLoadLabel.Caption := IntToStr(StructCarriedWeight(AStruct)) + ' / ' +
        IntToStr(Data.Capacity);
      StructControlLabel.Caption := IntToStr(control) + ' / ' +
        IntToStr(Data.Sailors);
    end
    else begin
      StructLoadLabel.Caption := '';
      StructControlLabel.Caption := '';
    end;
  end;
end;



  { -------------- Units Grid ------------- }

procedure TMainForm.FillUnitGrid;
var i, j: integer;
    cansel: boolean;
    U: TUnit;

  procedure SetItemsField(ACol, ARow: integer; U: TUnit; Mask: DWord);
  var i, amt: integer;
      s: string;
  begin
    s := '';
    amt := U.Items.Amount(Mask);
    if amt > 0 then begin
      s := s + IntToStr(amt);
      if Config.ReadBool('MainWin', 'LongITs', True) then begin
        for i := 0 to U.Items.Count-1 do
          if Test(U.Items[i].Data.Flags, Mask) then begin
            if Pos(' ', s) > 0 then s := s + ',';
            s := s + ' ' + U.Items[i].Name;
          end;
      end;
    end;
    UnitGrid.Cells[ACol, ARow] := s;
    UnitGrid.SortKeys[ACol, ARow] := IntToStr(amt);
  end;

  procedure AddUnitToGrid(AUnit: TUnit; Arriving: boolean);
  var i, row: integer;
      Fac: TFaction;
      s: string;
      Skill: TSkill;
  begin
    with AUnit do begin
      if Config.ReadBool('MainWin', 'GridMode', False) 
        and (Faction.Num <> Filter.FactionNum) then Exit;
      // Filters
      if Filter.StructFilter then begin
        // Struct
        if (Filter.RegCoords.z >= 0) and not EqualCoords(AUnit.Region.Coords,
          Filter.RegCoords) then Exit;
        if (Filter.StructNum >= 0) and ((AUnit.Struct = nil)
          or (AUnit.Struct.Num <> Filter.StructNum)) then Exit;
      end;
      if Filter.Enabled then begin
        // Unit
        if (Filter.UnitName <> '') and (Pos(Filter.UnitName, AUnit.Name) = 0) then
          Exit;
        if (Filter.UnitNum <> '') and (AUnit.NumStr <> Filter.UnitNum) then
          Exit;
        // Faction
        if Faction.Num <> Filter.FactionNum then Exit;
        // Mage
        if Filter.Mages and not Mage then Exit;
        // Skills, items
        if Filter.Any then begin
          if Filter.Items.Count > 0 then begin
            i := Filter.Items.Count-1;
            while (i >= 0) and (Items.Find(Filter.Items[i].Short) = nil) do Dec(i);
            if i < 0 then Exit;
          end;
          if Filter.Skills.Count > 0 then begin
            i := Filter.Skills.Count-1;
            while i >= 0 do begin
              Skill := Skills.Find(Filter.Skills[i].Data.Short);
              if (Skill <> nil) and (Skill.Level >= Filter.Skills[i].Level) then Break;
              Dec(i);
            end;
            if i < 0 then Exit;
          end;
        end
        else begin
          for i := 0 to Filter.Items.Count-1 do
            if Items.Find(Filter.Items[i].Short) = nil then Exit;
          for i := 0 to Filter.Skills.Count-1 do begin
            Skill := Skills.Find(Filter.Skills[i].Data.Short);
            if (Skill = nil) or (Skill.Level < Filter.Skills[i].Level) then Exit;
          end;
        end;
      end;

      // Fill
      row := UnitGrid.RowCount;
      if Struct <> nil then begin
        UnitGrid.Cells[0, row] := Struct.Name;
        UnitGrid.Cells[1, row] := IntToStr(Struct.Num);
        UnitGrid.Cells[2, row] := Struct.Data.Group;
      end;

      Fac := nil;
      if Faction.Num = 0 then
        Fac := FindFaction(UnitRecs.Faction(Num, Region.Coords));
      if Fac = nil then Fac := Faction;
      if Fac.Name = '' then UnitGrid.Cells[3, row] := ' '
      else UnitGrid.Cells[3, row] := Fac.Name;
      if Fac.Num <> 0 then
        UnitGrid.Cells[4, row] := IntToStr(Faction.Num);
      if Faction.Player then begin
        UnitGrid.SortKeys[3, row] := '';
        UnitGrid.SortKeys[4, row] := '-1';
      end;

      UnitGrid.Cells[5, row] := Name;
      UnitGrid.Cells[6, row] := NumStr;
      UnitGrid.SortKeys[6, row] := IntToStr(Num);
      {if Arriving then begin
        UnitGrid.SortKeys[5, row] := '';
        UnitGrid.SortKeys[6, row] := IntToStr(Num + 1000000);
      end;}

      SetItemsField(7, row, AUnit, IT_MAN + IT_MONSTER);
      UnitGrid.Cells[8, row] := IntToStr(AUnit.Items.Amount(IT_SILVER));
      SetItemsField(9, row, AUnit, IT_MOUNT);

      UnitGrid.Cells[10, row] := UnitRecs.Local(Num, Region.Coords);
      if (UnitGrid.Cells[10, row] = '')
        and (Config.ReadBool('MainWin', 'NameAsLocal', True)) then
        UnitGrid.Cells[10, row] := Name;
      for i := 0 to UnitFlagsCount-1 do
        if Flags[i] then
          UnitGrid.Cells[11, row] := UnitGrid.Cells[11, row] + FlagChars[i];
      if Revealing <> rtHide then
        UnitGrid.Cells[11, row] := UnitGrid.Cells[11, row] + RevFlagChars[Revealing];
      UnitGrid.Cells[12, row] := Region.Land;
      UnitGrid.Cells[13, row] := UnitString(AUnit,
        Config.ReadInteger('MainWin', 'CustomString', U_NAME + U_FACTION));
      UnitGrid.Cells[14, row] := AUnit.MonthOrder;
      for i := 0 to AUnit.Skills.Count-1 do begin
        if i > 0 then s := s + ', ';
        case Config.ReadInteger('MainWin', 'SkillColumn', 0) of
          0: s := s + AUnit.Skills[i].Data.Name + ' ' + IntToStr(AUnit.Skills[i].Level);
          1: s := s + AUnit.Skills[i].Data.Short + ' ' + IntToStr(AUnit.Skills[i].Level);
          2: s := s + Copy(AUnit.Skills[i].Data.Short, 1, 1) + ' ' + IntToStr(AUnit.Skills[i].Level);
        end;
      end;
      UnitGrid.Cells[15, row] := s;
      UnitGrid.Rows[row].Data := AUnit;
      if not Arriving then
        UnitGrid.Rows[row].Color := FactionColor(AUnit.Faction)
      else UnitGrid.Rows[row].Color := clGray;
    end;
  end;

begin
  ClearUnitInfo;
  UnitGrid.RowCount := 1;

  if Config.ReadBool('MainWin', 'GridMode', False) then begin
    if Filter.FactionNum = -1 then Filter.FactionNum := Faction.Num;
    // Add ALL units
    for i := 0 to VTurn.Factions.Count-1 do
      for j := 0 to VTurn.Factions[i].Units.Count-1 do
        AddUnitToGrid(VTurn.Factions[i].Units[j], False)
  end
  else begin
    // Add units from one region
    if (CurrRegion <> nil) and (CurrRegion.Visited = Turn.Num) then
      for i := 0 to CurrRegion.Troops.Count-1 do
        for j := 0 to CurrRegion.Troops[i].Units.Count-1 do
          AddUnitToGrid(CurrRegion.Troops[i].Units[j], False);
    // Add arriving units
    if Config.ReadBool('MainWin', 'ArrivingUnits', False) then
      for i := 0 to VFaction.Units.Count-1 do begin
        U := VFaction.Units[i];
        if U.ArrivingTo(SelectedCoords) then AddUnitToGrid(U, True);
      end;
  end;

  // Faction filter
  if Filter.Enabled then begin
    i := cmFilterFaction.Items.Count-1;
    while (i >= 1) and (TFactionData(cmFilterFaction.Items.Objects[i]).Num <>
      Filter.FactionNum) do Dec(i);
    cmFilterFaction.ItemIndex := i;
  end
  else cmFilterFaction.ItemIndex := 0;

  // Now a little messing with UnitGrid to speed up repainting units in new region
  with UnitGrid do begin
    OnSelectCell := nil;
    Fixup;
    OnSelectCell := UnitGridSelectCell;
    Repaint;
    if UnitGrid.ImgRowCount > 1 then
      UnitGridSelectCell(UnitGrid, UnitGrid.Col, UnitGrid.Row, cansel)
    else HexMap.Redraw; // to remove arrows
  end;
  // Clear AllItems if no faction units in region
  if (CurrRegion = nil) or (CurrRegion.PlayerTroop = nil) then
    FillAllItems(nil);
end;

procedure TMainForm.SelectUnit(U: TUnit);
var i: integer;
begin
  if U = nil then Exit;
  if not EqualCoords(SelectedCoords, U.Region.Coords) then
    HexMapGoto(U.Region.Coords);
  with UnitGrid do begin
    i := RowCount-1;
    while (i >= 1) and (TUnit(ImgRows[i].Data).Num <> U.Num) do Dec(i);
    if i >= 1 then Row := i;
  end;
end;

procedure TMainForm.btnMyFactionClick(Sender: TObject);
begin
  Filter.FactionNum := Faction.Num;
  Filter.Enabled := True;
  UnitFilterAction.Checked := True;
  FillUnitGrid;
end;

procedure TMainForm.btnAllFactionsClick(Sender: TObject);
begin
  Filter.FactionNum := -1;
  Filter.Enabled := not UnitFilterEmpty;
  UnitFilterAction.Checked := Filter.Enabled;
  FillUnitGrid;
end;

procedure TMainForm.cmFilterFactionChange(Sender: TObject);
begin
  with cmFilterFaction do begin
    if Items.Objects[ItemIndex] <> nil then
      Filter.FactionNum := TFactionData(Items.Objects[ItemIndex]).Num
    else
      Filter.FactionNum := -1;
  end;
  Filter.Enabled := not UnitFilterEmpty;
  UnitFilterAction.Checked := Filter.Enabled;
  FillUnitGrid;
end;

procedure TMainForm.UnitGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var intcol, namecol: integer;
    AUnit: TUnit;
begin
  if NoDraw then Exit;
  with UnitGrid do begin
    // Detect Column number
    intcol := 0;
    while (intcol < UnitGridColCount-1) and (Cells[intcol, 0] <>
      ImgCells[ACol, 0]) do Inc(intcol);
    if (ARow = 0) then begin
      // Top row
      if UnitGridHeadIcons[intcol] <> 0 then begin
        TxtRect.Left := TxtRect.Left + 18;
        ResForm.IconList.Draw(Canvas, TxtRect.Left - 17, TxtRect.Top-1,
          UnitGridHeadIcons[intcol]);
      end;
    end
    else begin
      AUnit := TUnit(ImgRows[ARow].Data);
      if (ImgCells[ACol, 0] = 'Object') and (AUnit.Struct <> nil) then
        with AUnit.Struct do begin
          // Struct with owner flag
          if Owner = AUnit then
            DrawOwnedStruct(Canvas, TxtRect.Left+1, TxtRect.Top, AUnit.Struct)
          // Simple struct
          else DrawStructIcon(Canvas, TxtRect.Left+1, TxtRect.Top,
            AUnit.Struct.Data, AUnit.Struct.HasExit);
          TxtRect.Left := TxtRect.Left + 17;
        end;
      // Unknown faction mark
      if (intcol = ugcFaction) and (AUnit.Faction.Num = 0)
        and (ImgCells[ACol, ARow] <> ' ') then begin
        ResForm.Extras.Draw(Canvas, TxtRect.Right - 8, TxtRect.Top + 1,
          bmp_extUnknown);
        TxtRect.Right := TxtRect.Right - 8;
      end;
      // Unit marks
      if Config.ReadBool('MainWin', 'NameAsLocal', True) then
        namecol := ugcLocal
      else namecol := ugcUnitName;
      if (intcol = namecol) then begin

        if (AUnit.UArmy <> nil) and (AUnit.UArmy.Color >= 0) and
          Config.ReadBool('MainWin', 'ArmyColors', True) then
          Canvas.Font.Color := AUnit.UArmy.Color;

        if Test(AUnit.Marks, UM_ALLY) then begin
          ResForm.Extras.Draw(Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
            bmp_extGrayFlag);
          TxtRect.Right := TxtRect.Right - 8;
        end;
        if Test(AUnit.Marks, UM_BATTLE) and not Test(AUnit.Marks, UM_DEADMEN) then begin
          ResForm.Extras.Draw(Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
            bmp_extBattle);
          TxtRect.Right := TxtRect.Right - 8;
        end;
        if Test(AUnit.Marks, UM_DEADMEN) then begin
          ResForm.Extras.Draw(Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
            bmp_extCross);
          TxtRect.Right := TxtRect.Right - 8;
        end;
        if Test(AUnit.Marks, UM_ATTACK) then begin
          ResForm.Extras.Draw(Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
            bmp_extAttack);
          TxtRect.Right := TxtRect.Right - 8;
        end;
        if AUnit.Mage then begin
          ResForm.Extras.Draw(Canvas, TxtRect.Right - 9, TxtRect.Top + 1,
            bmp_extMage);
          TxtRect.Right := TxtRect.Right - 8;
        end;
        if AUnit.Faction.Player and (AUnit.Items.Amount(IT_MAN) = 0) then
          Canvas.Font.Style := [fsStrikeout];
      end;
    end;
  end;
end;

procedure TMainForm.UnitGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (UnitGrid.MouseCell.Y > 0)
    and (UnitGrid.ImgRowCount > 1) then begin
    UnitGrid.PopupMenu := PopMenu;
    CustomizeUnitPopup(TUnit(UnitGrid.ImgRows[UnitGrid.MouseCell.Y].Data));
    UnitGrid.Row := UnitGrid.MouseCell.Y;
  end
  else UnitGrid.PopupMenu := nil;
end;

procedure TMainForm.UnitGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var i: integer;
begin
  CurrUnit := TUnit(UnitGrid.ImgRows[ARow].Data);
  if CurrUnit.NumStr <> NumLabel.Caption then
    FillUnitInfo(UnitGrid.ImgRows[ARow].Data);
  HexMap.Redraw; // for moving arrows
  // Disable manual scripts for non-player units
  for i := 0 to ScriptsItm.Count-1 do
    if (ScriptsItm.Items[i].Tag = 1) then
      ScriptsItm.Items[i].Enabled := (CurrUnit.Faction.Player and
        (CurrUnit.Num > 0));
end;

procedure TMainForm.UnitGridDblClick(Sender: TObject);
begin
  HexMapGoto(CurrUnit.Region.Coords);
end;

procedure TMainForm.UnitGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if not ((Source = ItemGrid) or (Source is TToolButton)
    or (Source = OrderMemo)) then
    Accept := False;
end;

procedure TMainForm.GridModeActionExecute(Sender: TObject);
begin
  GridModeAction.Checked := not GridModeAction.Checked;
  Config.WriteBool('MainWin', 'GridMode', GridModeAction.Checked);
  FillUnitGrid;
end;

procedure TMainForm.ArrivingUnitsActionExecute(Sender: TObject);
begin
  ArrivingUnitsAction.Checked := not ArrivingUnitsAction.Checked;
  Config.WriteBool('MainWin', 'ArrivingUnits', ArrivingUnitsAction.Checked);
  HexMap.Selected := HexMap.Selected;
end;

procedure TMainForm.AllItemsActionExecute(Sender: TObject);
begin
  gAllItems.Visible := not gAllItems.Visible;
  Config.WriteBool('MainWin', 'AllItems', gAllItems.Visible);
  if gAllItems.Visible then FillAllItems(CurrUnit);
end;

  { -------------- Unit Info Panel ----------------- }

procedure TMainForm.ClearUnitInfo;
var i: integer;
begin
  UnitEnable(False);
  CurrUnit := nil;
  if not Config.ReadBool('MainWin', 'BattleImages', True) then
    UnitImagePanel.Width := 0;
  UnitNameEdit.Text := '';
  UnitNameEdit.ReadOnly := TRUE;
  NumLabel.Caption := '';
  UnitDescrEdit.Text := '';
  UnitDescrEdit.ReadOnly := TRUE;
  FactionLabel.Caption := '';
  FactionFlagImage.Visible := FALSE;
  StructImage.Canvas.FillRect(StructImage.Canvas.ClipRect);
  StructLabel.Caption := '';
  for i := 0 to UnitFlagsCount-1 do
    with FlagBar.Buttons[i] do begin
      Down := FALSE;
      Enabled := FALSE;
    end;
  btnConsume.Enabled := False;
  btnReveal.Enabled := False;
  btnSpoils.Enabled := False;

  ItemGrid.RowCount := 0;
  ItemGrid.Fixup;
  eGiveAmt.Value := 0;
  tbGiveAll.Enabled := False;
  tbGiveBattle.Enabled := False;
  tbGiveTrade.Enabled := False;
  tbGiveSpoils.Enabled := False;
  MsgGrid.RowCount := 0;
  MsgGrid.Fixup;
  SkillGrid.RowCount := 0;
  SkillGrid.Fixup;
  OrderMemo.Lines.Clear;
  OrderMemo.ReadOnly := TRUE;
  OrderMemo.Color := clBtnFace;
  lMonthOut.Caption := '';
  imgMonthOrder.Picture.Bitmap.Height := 0;
end;

procedure TMainForm.FillUnitInfo(AUnit: TUnit);
var i, j, capacity, load, row: integer;
    pack, img, s: string;
    found: boolean;
    Skill: TSkill;
    SData: TSkillData;
    IData: TItemData;
    Route: TRoute;
    RealU: TUnit;
begin
  UnitEnable(True);

  Route := TRoute.Create;
  ReadRoute(AUnit, Route);
  AUnit.Route := Route.Moves;
  Route.Free;

  with AUnit do begin
    // Select structure
    if Struct <> nil then begin
      i := 0;
      while (i < Region.Structs.Count-1) and (Region.Structs[i] <> Struct) do
        Inc(i);
      if i < Region.Structs.Count then begin
        FillStructInfo(Region.Structs[i]);
        j := StructGrid.RowCount-1;
        while (j >= 0) and (StructGrid.ImgRows[j].Data <> CurrStruct) do Dec(j);
        if j >= 0 then StructGrid.Row := j;
      end;
      StructGrid.StickySelect := True;
    end
    else if CurrStruct <> nil then begin
      ClearStructInfo;
      StructGrid.StickySelect := False;
    end;

    // Avatar
    if not Config.ReadBool('MainWin', 'BattleImages', True) then s := '-'
    else s := UnitImg(AUnit, AUnit.Region);
    if DrawCombinedAvatar(AUnit, AUnit.Faction, s, UnitImage.Picture.Bitmap,
      True) then begin
      UnitImagePanel.Width := UnitImage.Width + 2;
      UnitImage.Invalidate;
    end
    else UnitImagePanel.Width := 0;

    UnitNameEdit.Text := Name;
    UnitNameEdit.Modified := FALSE;
    NumLabel.Caption := NumStr;
    UnitNameEdit.ReadOnly := (Faction.Num <> VFaction.Num);
    if not btnLocal.Down then begin
      UnitDescrEdit.Text := StripTags(Description);
      UnitDescrEdit.ReadOnly := not Faction.Player;
    end
    else begin
      UnitDescrEdit.Text := UnitRecs.Local(Num, Region.Coords);
      UnitDescrEdit.ReadOnly := False;
    end;
    UnitDescrEdit.Modified := FALSE;
    FactionLabel.Caption := Faction.Name;
    FactionFlagImage.Visible := TRUE;
    DrawCExtra(extFlag, Faction, FactionFlagImage.Canvas, 0, 0);
    // Struct
    StructImage.Canvas.Brush.Color := clBtnFace;
    StructImage.Canvas.FillRect(StructImage.Canvas.ClipRect);
    if Struct <> nil then begin
      StructLabel.Caption := Struct.Name + ' [' + IntToStr(Struct.Num) +
        '] : ' + Struct.Data.Group;
      if (Struct.Owner <> nil) and (Struct.Owner.Num = Num) then
        DrawOwnedStruct(StructImage.Canvas, 0, 0, Struct)
      else DrawStructIcon(StructImage.Canvas, 0, 0, Struct.Data, Struct.HasExit);
    end
    else StructLabel.Caption := '';
    // Flags
    for i := 0 to UnitFlagsCount-1 do
      with FlagBar.Buttons[i] do begin
        Down := Flags[i];
        Enabled := (Faction.Num = VFaction.Num);
      end;
    if Faction.Num = VFaction.Num then begin
      btnConsume.ImageIndex := Consuming + bmp_efConsume;
      btnConsume.Hint := ExtFlags[0, Consuming];
      btnReveal.ImageIndex := Revealing + bmp_efReveal;
      btnReveal.Hint := ExtFlags[1, Revealing];
      btnSpoils.ImageIndex := Spoils + bmp_efSpoils;
      btnSpoils.Hint := ExtFlags[2, Spoils];
    end
    else begin
      btnConsume.Hint := ExtFlags[0, 0];
      btnReveal.Hint := ExtFlags[1, 0];
      btnSpoils.Hint := ExtFlags[2, 0];
    end;
    btnConsume.Enabled := (Faction.Num = VFaction.Num);
    btnReveal.Enabled := (Faction.Num = VFaction.Num);
    btnSpoils.Enabled := (Faction.Num = VFaction.Num);

    // Fill items
    ItemGrid.NoRepaint := True;
    if Config.ReadBool('MainWin', 'UnmodItemAmounts', False) then begin
      RealU := Turn.Factions[1].Units.Find(AUnit.Num);
      if RealU <> nil then FillItemGrid(ItemGrid, RealU.Items)
      else FillItemGrid(ItemGrid, Items);
    end
    else FillItemGrid(ItemGrid, Items);
    // Incomes
    with ItemGrid do begin
      if WorkIncome > 0 then
        InsertItemGridRow(ItemGrid, 0, IntToStr(WorkIncome),
          Keys[s_WorkIncome], '-2', nil, clGrayText);
      if TradeIncome > 0 then
        InsertItemGridRow(ItemGrid, 0, IntToStr(TradeIncome),
          Keys[s_TradeIncome], '-1', nil, clGrayText);
      Fixup;
    end;
    eGiveAmt.Value := 0;
    // Position
    if ItemGrid.RowCount > 0 then
      ItemGrid.TopRow := Min(ItemGridTop, ItemGrid.RowCount -
        ItemGrid.VisibleRowCount);

    ItemGridTop := 0;
    ItemGrid.NoRepaint := False;
    ItemGrid.Invalidate;

    tbGiveAll.Enabled := Faction.Player;
    tbGiveBattle.Enabled := Faction.Player;
    tbGiveTrade.Enabled := Faction.Player;
    tbGiveSpoils.Enabled := Faction.Player;

   // Fill messages
    MsgGrid.RowCount := 0;
    for i := 0 to Events.Count-1 do begin
      row := MsgGrid.RowCount;
      if Pos('!', Events[i]) = 0 then begin
        MsgGrid.Cells[1, row] := Events[i];
        MsgGrid.Cells[0, MsgGrid.RowCount-1] := IntToStr(bmpInfo);
      end
      else begin
        MsgGrid.Cells[1, row] := Copy(Events[i], 2, Length(Events[i])-1);
        MsgGrid.Cells[0, MsgGrid.RowCount-1] := IntToStr(bmpError);
      end;
    end;
    MsgGrid.Fixup;

    if not Faction.Player then begin
      OrderMemo.ReadOnly := TRUE;
      OrderMemo.Color := clBtnFace;
    end
    else begin
      OrderMemo.ReadOnly := FALSE;
      OrderMemo.Color := clWindow;
    end;

    // Fill skills
    SkillGrid.RowCount := 0;
    with SkillGrid do begin
      i := 0;
      while i < Skills.Count do begin
        Cells[0, i] := Skills[i].Data.Name;
        Cells[1, i] := IntToStr(Skills[i].Level);
        Cells[2, i] := IntToStr(Skills[i].Points);
        Rows[i].Data := Skills[i].Data;
        if Skills[i].Data = CombatSpell then
          Rows[i].ImageIndex := bmpCombatSpell
        else Rows[i].ImageIndex := SkillIcon(Skills[i].Data);
        Inc(i);
      end;
      j := 0;
      while j < CanStudy.Count do begin
        Cells[0, i] := CanStudy[j].Name;
        Rows[i].Data := CanStudy[j];
        Rows[i].Color := clGray;
        Rows[i].ImageIndex := SkillIcon(CanStudy[j]);
        Inc(i);
        Inc(j);
      end;
      Fixup;
    end;

    // Orders
    OrderMemo.Lines.Assign(Orders);
    OrderMemo.Modified := FALSE;
    btnCheckOrder.Enabled := Faction.Player;
    btnClearOrder.Enabled := Faction.Player;

    // Fill weight
    i := MovementType(AUnit);
    case i of
      mtWalk..mtSwim: MoveTypeLabel.Caption := GetKey(s_Walking, i - 1);
      else MoveTypeLabel.Caption := 'none';
    end;
    // Walk
    capacity := UnitCapacity(AUnit, mtWalk);
    load := UnitLoad(AUnit, mtWalk);
    UnitWeightLabel.Caption := IntToStr(load);
    if capacity > 0 then begin
      UnitWeightLabel.Caption := UnitWeightLabel.Caption + ' (max ' +
      IntToStr(capacity) + ')';
      UnitWeightLabel.Font.Color := clWindowText;
    end
    else UnitWeightLabel.Font.Color := clGrayText;
    UnitWeightWarning.Visible := (load > capacity) and (capacity > 0);
    // Ride
    capacity := UnitCapacity(AUnit, mtRide);
    load := UnitLoad(AUnit, mtRide);
    HorsesWeightLabel.Caption := IntToStr(load);
    if capacity > 0 then begin
      HorsesWeightLabel.Caption := HorsesWeightLabel.Caption + ' (max ' +
      IntToStr(capacity) + ')';
      HorsesWeightLabel.Font.Color := clWindowText;
    end
    else HorsesWeightLabel.Font.Color := clGrayText;
    HorsesWeightWarning.Visible := (load > capacity) and (capacity > 0);
    // Fly
    capacity := UnitCapacity(AUnit, mtFly);
    load := UnitLoad(AUnit, mtFly);
    WingWeightLabel.Caption := IntToStr(load);
    if capacity > 0 then begin
      WingWeightLabel.Caption := WingWeightLabel.Caption + ' (max ' +
        IntToStr(capacity) + ')';
      WingWeightLabel.Font.Color := clWindowText;
    end
    else WingWeightLabel.Font.Color := clGrayText;
    WingWeightWarning.Visible := (load > capacity) and (capacity > 0);

    // Struct
    TransportWeightLabel.Caption := IntToStr(UnitLoad(AUnit, mtNone));
    if (Struct <> nil) and (Struct.Data.Flags and ST_TRANSPORT <> 0) then begin
      capacity := Struct.Data.Capacity;
      TransportWeightLabel.Caption := TransportWeightLabel.Caption + '/' +
        IntToStr(StructCarriedWeight(Struct)) + ' (max ' + IntToStr(capacity) + ')';
      TransportWeightWarning.Visible := (StructCarriedWeight(Struct) > capacity);
      TransportWeightLabel.Font.Color := clWindowText;
    end
    else begin
      TransportWeightWarning.Visible := FALSE;
      TransportWeightLabel.Font.Color := clGrayText;
    end;

    // Month order
    lMonthOut.Caption := '';
    PrepareBmp(imgMonthOrder.Picture.Bitmap, 16, 16, clNether);
    if Faction.Player then begin
      s := IntToStr(MonthInfo.Amount) + ' (max ' + IntToStr(MonthInfo.Max) + ') ';

      if ClearOrder(MonthOrder) = 'produce' then begin
        if MonthInfo.Data <> nil then begin
          s := s + TItemData(MonthInfo.Data).Name(MonthInfo.Amount <> 1);
          DrawItemIcon(imgMonthOrder.Picture.Bitmap.Canvas, 0, 0, MonthInfo.Data);
        end;
      end
      else if ClearOrder(MonthOrder) = 'build' then begin
        if MonthInfo.Data <> nil then begin
          s := s + TStructData(MonthInfo.Data).Group;
          DrawStructIcon(imgMonthOrder.Picture.Bitmap.Canvas, 0, 0,
            MonthInfo.Data, False);
        end;
      end
      else if ClearOrder(MonthOrder) = 'study' then begin
        s := IntToStr(MonthInfo.Amount) + ' days';
        if MonthInfo.Details <> '' then
          s := s + ': ' + MonthInfo.Details;
        ResForm.IconList.Draw(imgMonthOrder.Picture.Bitmap.Canvas, 0, 0, bmpGlasses);
      end
      else if (ClearOrder(MonthOrder) = 'entertain')
        or (ClearOrder(MonthOrder) = 'work')
        or (GameConfig.ReadBool('Settings', 'MonthTax', False) and
          (ClearOrder(MonthOrder) = 'tax')) then begin
        ResForm.IconList.Draw(imgMonthOrder.Picture.Bitmap.Canvas, 0, 0, bmpSilver);
      end
      else if ClearOrder(MonthOrder) = 'teach' then begin
        s := IntToStr(MonthInfo.Amount) + ' (max ' +
          IntToStr(MonthInfo.Max) + ') students';
        ResForm.IconList.Draw(imgMonthOrder.Picture.Bitmap.Canvas, 0, 0, bmpGlasses);
      end
      else begin
        s := '';
        imgMonthOrder.Picture.Bitmap.Height := 0;
      end;
      lMonthOut.Caption := s;
    end;

  end;
  FillAllItems(AUnit);
end;

procedure TMainForm.ItemGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  if NoDraw then Exit;
  if (ACol = 0) and Config.ReadBool('MainWin', 'UnmodItemAmounts', False) then
    ItemGrid.Canvas.Font.Color := clGrayText
  else uInterface.ItemGridDrawCell(Sender, ACol, ARow, TxtRect, 1);
end;

// If region items visible, fill grid with all units' items excluding given
procedure TMainForm.FillAllItems(AUnit: TUnit);
var i, j, row: integer;
    Troop: TTroop;
    Item: TItem;
begin
  if not gAllItems.Visible then Exit;
  if AUnit <> nil then Troop := AUnit.Region.PlayerTroop
  else Troop := nil;
  gAllItems.RowCount := 0;
  if Troop <> nil then begin
    row := 1;
    for i := 0 to Troop.Units.Count-1 do
      if Troop.Units[i] <> AUnit then
        for j := 0 to Troop.Units[i].Items.Count-1 do begin
          Item := Troop.Units[i].Items[j];
          if not Item.Bought and not Test(Item.Data.Flags, IT_MAN) then begin
            gAllItems.Cells[0, row] := IntToStr(Item.Amount);
            gAllItems.Cells[1, row] := Item.Name;
            gAllItems.SortKeys[1, row] := IntToStr(Game.ItemData.IndexOf(Item.Data));
            gAllItems.Cells[2, row] := Troop.Units[i].Name + ' (' +
              Troop.Units[i].NumStr + ')';
            gAllItems.Rows[row].Data := Item;
            Inc(row);
          end;
        end;
  end;
  gAllItems.Fixup;
end;

procedure TMainForm.btnLocalClick(Sender: TObject);
var value: boolean;
begin
  value := btnLocal.Down;
  btnLocal.Down := not btnLocal.Down;
  UnitDescrEdit.OnExit(UnitDescrEdit);
  btnLocal.Down := value;
  if btnLocal.Down then begin
    UnitDescrEdit.Text := UnitRecs.Local(CurrUnit.Num, CurrUnit.Region.Coords);
    UnitDescrEdit.ReadOnly := False;
  end
  else begin
    UnitDescrEdit.Text := CurrUnit.Description;
    UnitDescrEdit.ReadOnly := not CurrUnit.Faction.Player;
  end;
  Config.WriteBool('MainWin', 'LocalDescriptions', btnLocal.Down);
end;

procedure TMainForm.UnitImageDblClick(Sender: TObject);
begin
  AvatarsAction.Execute;
end;

procedure TMainForm.UnitMainDataTextPanelResize(Sender: TObject);
begin
  UnitDescrBevel.Width := UnitMainDataTextPanel.Width - btnLocal.Width - 4;
  UnitDescrEdit.Width := UnitDescrBevel.Width - 6;
end;

function TMainForm.EditBevel(N: integer): TBevel;
begin
  case N of
    0: Result := UnitNameBevel;
    1: Result := UnitDescrBevel;
    2: Result := StructNameBevel;
    else Result := StructDescrBevel;
  end;
end;

procedure TMainForm.FormEditEnter(Sender: TObject);
begin
  EditBevel(TEdit(Sender).Tag).Visible := TRUE;
end;

procedure TMainForm.FormEditExit(Sender: TObject);
var Key: Char;
begin
  EditBevel(TEdit(Sender).Tag).Visible := FALSE;
  if TEdit(Sender).Modified then begin
    Key := #13;
    FormEditKeyPress(Sender, Key);
  end;
end;

procedure TMainForm.FormEditKeyPress(Sender: TObject; var Key: Char);
var Text: string;
    i: integer;
begin
  Text := TEdit(Sender).Text;
  if Key = #13 then begin
    OrdersChanged := True;
    case TEdit(Sender).Tag of
      0: ExecOrder('name unit "' + Text + '"', FALSE);
      1: if btnLocal.Down then begin
           if CurrUnit.Faction.Player then begin
             if Text = '' then begin
               // Remove localdesc comment
               i := 0;
               while i < CurrUnit.Orders.Count do
                 if CurrUnit.Order(i) = '@;;' then CurrUnit.Orders.Delete(i)
                 else Inc(i);
             end
             else if CurrUnit.Num < 0 then AddOrder('@;;' + Text, False);
           end;
           UnitRecs.AddUnitRec(CurrUnit, Text);
           FillUnitGrid;
         end
         else if CurrUnit.Faction.Player then
           ExecOrder('describe unit "' + Text + '"', FALSE);
      2: ExecOrder('name object "' + Text + '"', False);
      else ExecOrder('describe object "' + Text + '"', False);
    end;
  end;
end;

procedure TMainForm.SkillGridDblClick(Sender: TObject);
begin
  with TSkillEditForm.Create(Self) do begin
    SelectItem(TSkillData(SkillGrid.ImgRows[SkillGrid.Row].Data));
    ShowModal;
    Free;
  end;
  ProcessAllOrders;
end;

procedure TMainForm.SkillGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var lv: integer;
begin
  if (Button = mbRight) and (SkillGrid.MouseCell.Y >= 0)
    and (SkillGrid.ImgRowCount > 0) then begin
    SkillGrid.PopupMenu := PopMenu;
    if SkillGrid.ImgCells[1, SkillGrid.MouseCell.Y] = '' then lv := 0
    else lv := StrToInt(SkillGrid.ImgCells[1, SkillGrid.MouseCell.Y]);
    CustomizeSkillPopup(
      TSkillData(SkillGrid.ImgRows[SkillGrid.MouseCell.Y].Data), lv);
    SkillGrid.Row := SkillGrid.MouseCell.Y;
  end
  else SkillGrid.PopupMenu := nil;
end;

procedure TMainForm.ItemGridDblClick(Sender: TObject);
var Grid: TPowerGrid;
    Item: TItem;
begin
  if Sender is TPowerGrid then Grid := TPowerGrid(Sender)
  else if ItemGrid.Focused then Grid := ItemGrid
  else if gAllItems.Focused then Grid := gAllItems
  else Exit;

  Item := TItem(Grid.ImgRows[Grid.Row].Data);
  if Item = nil then Exit;

  with TItemEditForm.Create(Self) do begin
    SelectItem(Item.Data);
    ShowModal;
    if Modified then ProcessAllOrders;
    Free;
  end;
end;

procedure TMainForm.ItemGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Item: TItem;
begin
  with TPowerGrid(Sender) do begin
    if MouseCell.Y < FixedRows then Exit;
    // mbRight: Popup menu
    PopupMenu := nil;
    if (Button = mbRight) and (MouseCell.Y >= 0) and (ImgRowCount > 0)
      and (SelectedRows = 1) then begin
      Row := MouseCell.Y;
      Item := TItem(ImgRows[Row].Data);
      if Item <> nil then begin
        PopupMenu := PopMenu;
        CustomizeItemPopup(Item);
      end;
    end;
    // mbLeft: Drag GIVE
    if (CurrUnit <> nil)
      and (Button = mbLeft) and (RowCount > 0)
      and (CurrUnit.Faction.Player or (Sender = gAllItems))
      and  not (ssDouble in Shift)
      and (ImgRows[Row].Data <> nil)
      and not TItem(ImgRows[Row].Data).Bought then begin
      GiveToMovement := mtNone;
      BeginDrag(False);
    end;
  end;
end;

procedure TMainForm.ItemGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Sender = Source then begin
    if InfoPControl.ActivePage = tsUnit then eGiveAmt.SetFocus;
  end
  else if Source <> gAllItems then Accept := False;
end;

procedure TMainForm.ItemGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var Item: TItem;
begin
  if (ARow <> TPowerGrid(Sender).Row) or (eGiveAmt.Value = 0) then begin
    Item := TItem(TPowerGrid(Sender).ImgRows[ARow].Data);
    if Item <> nil then eGiveAmt.Value := Item.Amount
    else eGiveAmt.Value := 0;
  end;
end;

procedure TMainForm.ItemGridEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var i: integer;
    Item: TItem;

  procedure DoGive(Item: TItem; amt: integer);
  var P: TGridCoord;
      AUnit: TUnit;
      give_amt: string;
  begin
    if Target = UnitGrid then begin
      P := UnitGrid.MouseCell;
      if P.Y >= 1 then begin
        AUnit := TUnit(UnitGrid.ImgRows[P.Y].Data);
        if AUnit = CurrUnit then Exit;
        // Adjust amount to allow unit move
        if (GiveToMovement <> mtNone) and (Item.Data.Weight > 0) then
          amt := Min(amt, (UnitCapacity(AUnit, GiveToMovement) -
            UnitLoad(AUnit, GiveToMovement)) div Item.Data.Weight);
        if AltPressed then give_amt := 'all'
        else give_amt := IntToStr(amt);
        ExecOrder('give ' + AUnit.NumStr + ' ' + give_amt +
          ' "' + Item.Data.Name + '"; ' + AUnit.Name, True);
      end;
    end
    else if Target = HexMap then begin
      if AltPressed then give_amt := 'all'
      else give_amt := IntToStr(amt);
      ExecOrder('give 0 ' + give_amt +
        ' "' + Item.Data.Name + '"; Nobody', True);
    end;
  end;

begin
  with Sender as TPowerGrid do begin
    if SelectedRows = 1 then
      DoGive(TItem(ImgRows[Row].Data), eGiveAmt.Value)
    else begin
      for i := 0 to RowCount-1 do
        if Rows[i].Selected then begin
          Item := TItem(Rows[i].Data);
          if (Item = nil) or Item.Bought then Continue;
          DoGive(Item, Item.Amount);
        end;
    end;
  end;
end;

procedure TMainForm.tbMaskGiveEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var P: TGridCoord;
    order, name, num: string;
    U: TUnit;
    i: integer;
    Item: TItem;
begin
  if Target = UnitGrid then begin
    P := UnitGrid.MouseCell;
    if P.Y >= 0 then begin
      U := TUnit(UnitGrid.ImgRows[P.Y].Data);
      if U = CurrUnit then Exit;
      name := U.Name;
      num := U.NumStr;
    end;
  end
  else if Target = HexMap then begin
    name := 'Nobody';
    num := '0';
  end
  else Exit;

  order := '';
  case TToolButton(Sender).Tag of
    1: begin
      order := 'give ' + num + ' all items';
      if CurrUnit.Flags[flgTax] then order := 'autotax 0' + #13#10 + order;
      if CurrUnit.Flags[flgGuard] then order := 'guard 0' + #13#10 + order;
    end;
    2: order := 'give ' + num + ' all weapon' + #13#10 +
      'give ' + num + ' all armor';
    3: order := 'give ' + num + ' all trade';
    4: for i := 0 to CurrUnit.Items.Count-1 do begin
        Item := CurrUnit.Items[i];
        if Item.Bought or not IsSpoils(Item.Data) then Continue;
        AddOrder('give ' + num + ' ' + IntToStr(Item.Amount) + ' "' + Item.Name +
          '"; ' + name, False);
      end;
  end;

  ExecOrder(order, True);
end;

procedure TMainForm.gAllItemsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Sender = Source then begin
    if InfoPControl.ActivePage = tsUnit then eGiveAmt.SetFocus;
  end
  else Accept := False;
end;

procedure TMainForm.gAllItemsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var Item: TItem;
    i: integer;

  procedure DoGive(Item: TItem; amt: integer);
  var i: integer;
      Troop: TTroop;
      U: TUnit;
  begin
    // Find item owner
    Troop := CurrRegion.PlayerTroop;
    if Troop <> nil then begin
      i := Troop.Units.Count-1;
      while (i >= 0) and (Troop.Units[i].Items.IndexOf(Item) = -1) do Dec(i);
      if i >= 0 then begin
        // Add command to give item
        U := CurrUnit;
        CurrUnit := Troop.Units[i]; // to allow GIVE optimization
        // Give to walking...
        if (GiveToMovement <> mtNone) and (Item.Data.Weight > 0) then
          amt := Min(amt, (UnitCapacity(U, GiveToMovement) -
            UnitLoad(U, GiveToMovement)) div Item.Data.Weight);
        ExecOrder('give ' + U.NumStr + ' ' + IntToStr(amt) +
          ' "' + Item.Name + '"', False);
        CurrUnit := U;
      end;
    end;
  end;

begin
  if Target <> ItemGrid then Exit;
  with Sender as TPowerGrid do begin
    if SelectedRows = 1 then
      DoGive(TItem(ImgRows[Row].Data), eGiveAmt.Value)
    else begin
      for i := 0 to RowCount-1 do
        if Rows[i].Selected then begin
          Item := TItem(Rows[i].Data);
          if (Item = nil) or Item.Bought then Continue;
          DoGive(Item, Item.Amount);
        end;
    end;
  end;
end;

procedure TMainForm.ItemGridEnter(Sender: TObject);
begin
  with TPowerGrid(Sender) do
    if (RowCount > FixedRows) and (ImgRows[Row].Data <> nil) then
      eGiveAmt.Value := TItem(ImgRows[Row].Data).Amount
    else eGiveAmt.Value := 0;
end;

procedure TMainForm.MsgGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  if NoDraw then Exit;
  with Sender as TPowerGrid do begin
    if ACol = 0 then begin
      ResForm.IconList.Draw(Canvas, TxtRect.Left + 2, TxtRect.Top + 1,
        StrToInt(ImgCells[ACol, ARow]));
      TxtRect.Right := TxtRect.Left;
    end;
  end;
end;

procedure TMainForm.OrderMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if UnitGrid.RowCount > 1 then
    TUnit(UnitGrid.ImgRows[UnitGrid.Row].Data).Orders.Text := OrderMemo.Text;
end;

procedure TMainForm.OrderMemoExit(Sender: TObject);
begin
  if OrderMemo.Modified and (CurrUnit <> nil) and (CurrUnit.Faction.Player) then begin
    OrdersChanged := True;
    OrderMemo.Modified := False;
    CurrUnit.Orders.Text := OrderMemo.Lines.Text;
    ProcessOrders(CurrRegion);
  end;
end;

procedure TMainForm.CheckOrderBtnClick(Sender: TObject);
begin
  OrderMemo.Modified := True;
  OrderMemoExit(OrderMemo);
end;

procedure TMainForm.ClearOrderBtnClick(Sender: TObject);
begin
  OrderMemo.Lines.Clear;
  OrderMemo.Modified := True;
  OrderMemoExit(OrderMemo);
end;

procedure TMainForm.UnitFlagBtnClick(Sender: TObject);
var value: string;
begin
  if TToolButton(Sender).Down then value := '1' else value := '0';
  ExecOrder(FlagOrders[TToolButton(Sender).Tag] + ' ' + value, False);
end;

procedure TMainForm.SetExtFlag(Sender: TObject);
begin
  ExecOrder(StringReplace(TMenuItem(Sender).Caption, '&', '', []), False);
end;

procedure TMainForm.ExtFlagBtnClick(Sender: TObject);
var NewItem: TMenuItem;
    Index, i, Flags: integer;
begin
  Index := TToolButton(Sender).Tag;
  case Index of
    0: Flags := CurrUnit.Consuming;
    1: Flags := CurrUnit.Revealing;
    else Flags := CurrUnit.Spoils;
  end;
  // Create popup menu from MainMenu's submenu
  PopMenu.Items.Clear;
  for i := 0 to 4 do
    if ExtFlags[Index, i] <> '' then begin
      NewItem := TMenuItem.Create(PopMenu);
      NewItem.Caption := ExtFlags[Index, i];
      NewItem.RadioItem := True;
      NewItem.Checked := (Flags = i);
      NewItem.OnClick := SetExtFlag;
      PopMenu.Items.Add(NewItem);
    end;
  PopMenu.Images := nil;
  with TToolButton(Sender) do
    PopMenu.Popup(ClientToScreen(Point(Width, Height)).X,
      ClientToScreen(Point(Width, Height)).Y);
end;


  { -------------- Main Menu ----------------------  }

{ Controls }

procedure TMainForm.ShowToolbarItmClick(Sender: TObject);
begin
  ShowtoolbarItm.Checked := not ShowtoolbarItm.Checked;
  ToolPanel.Visible := ShowtoolbarItm.Checked;
  Config.WriteBool('MainWin', 'Toolbar', ShowtoolbarItm.Checked);
end;

procedure TMainForm.Maptools1Click(Sender: TObject);
begin
  Maptools1.Checked := not Maptools1.Checked;
  MapToolPanel.Visible := Maptools1.Checked;
  Config.WriteBool('MainWin', 'MapTools', Maptools1.Checked);
end;

procedure TMainForm.itmUnitToolsClick(Sender: TObject);
begin
  itmUnitTools.Checked := not itmUnitTools.Checked;
  tbUnitTools.Visible := itmUnitTools.Checked;
  Config.WriteBool('MainWin', 'UnitTools', itmUnitTools.Checked);
end;

procedure TMainForm.Infopanel1Click(Sender: TObject);
begin
  Infopanel1.Checked := not Infopanel1.Checked;
  InfoPanel.Visible := Infopanel1.Checked;
  Config.WriteBool('MainWin', 'InfoPanel', Infopanel1.Checked);
end;

{ Actions }

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ManagerActionExecute(Sender: TObject);
begin
  if (Game <> nil) and OrdersChanged
    and Config.ReadBool('MainWin', 'UnsavedOrders', True) then begin
    case MessageDlg('Orders was changed. Save order file?', mtConfirmation,
       [mbYes, mbNo, mbCancel], 0) of
      mrYes: SaveOrderAction.Execute;
      mrCancel: Exit;
    end;
  end;

  State := State + [asReadingGame];
  GlobalEnable(False);
  ManagerAction.Enabled := True;
  ManagerForm := TManagerForm.Create(Self);
  ManagerForm.ShowModal;
  FreeAndNil(ManagerForm);
  // Do things if game opened
  if (Game <> nil) then StartupGame;
  State := State - [asReadingGame];
  if (Game <> nil) and Config.ReadBool('MainWin', 'TurnEventsStart', True) then
      TurnEventsActionExecute(Self);
end;

procedure TMainForm.StartupGame;
begin
  Screen.Cursor := crHourGlass;
  ResForm.LoadTerrainImages;
  UnitRecs.GatherInfo;
  SetupScripts;
  SetupMapLevels;
  SetupBookmarks;
  SetupTerrains;
  SetTurns;
  SetCaption;
  HexMapSetup;
  State := State + [asFirstProcessing];
  OrdersChanged := False;
  if not OrdersLoaded then PreProcess;
  ProcessAllOrders;
end;


procedure TMainForm.OptionsActionExecute(Sender: TObject);
begin
  OptionForm := TOptionForm.Create(Self);
  OptionForm.ShowModal;
  OptionForm.Free;
  if Game <> nil then begin
    SetCaption;
    SetupMapLevels;
    SetupBookmarks;
    SetupEngine;
    HexMapSetup;
    ProcessAllOrders;
  end;
end;

procedure TMainForm.SaveOrderActionExecute(Sender: TObject);
var Lines: TStrings;
begin
  OrdersChanged := False;
  Lines := TStringList.Create;
  CompileOrder(Lines, Config.ReadBool('MainWin', 'CompactOrders', False));
  Lines.SaveToFile(BaseDir + Game.Name + '\orders\' +
    MakeFilename(Turn.Num) + '.ord');
  if GameConfig.ReadBool('Game', 'Local', False) then begin
    ForceDirectories(GetLocalFolder(Turn.Num + 1));
    Lines.SaveToFile(GetLocalFolder(Turn.Num + 1) + 'orders.3');
  end;
  Lines.Free;
end;

procedure TMainForm.SaveOrderAsClick(Sender: TObject);
var Lines: TStrings;
    s: string;
begin
  OrdersChanged := False;
  SaveDialog.DefaultExt := '*.ord';
  SaveDialog.Filter := 'Orders (*.ord)|*.ord|All files|*.*';
  SaveDialog.InitialDir := BaseDir + Game.Name + '\orders';
  s := MakeFilename(Turn.Num);
  SaveDialog.FileName := s + '.ord';
  if SaveDialog.Execute then begin
    Lines := TStringList.Create;
    CompileOrder(Lines, Config.ReadBool('MainWin', 'CompactOrders', False));
    Lines.SaveToFile(SaveDialog.FileName);
    Lines.Free;
  end;
end;

procedure TMainForm.ExportRulesetActionExecute(Sender: TObject);
begin
  SaveDialog.DefaultExt := '*.dat';
  SaveDialog.Filter := 'Advisor Rulesets (*.dat)|*.dat|All files|*.*';
  SaveDialog.InitialDir := BaseDir + RuleFolder;
  SaveDialog.FileName := Game.Name + '.dat';
  if SaveDialog.Execute then
    WriteRuleset(SaveDialog.FileName);
end;

procedure TMainForm.ExportMapItmClick(Sender: TObject);
var Lines: TStrings;
    Parts: DWord;
begin
  with TMapExportForm.Create(Self) do begin
    if ShowModal = mrOk then begin
      // Set parameters
      Parts := 0;
      SetFlag(Parts, MAP_ADVRESOURCES, cbAdvances.Checked);
      SetFlag(Parts, MAP_BUILDINGS, cbBuildings.Checked);
      SetFlag(Parts, MAP_TRANSPORTS, cbTransports.Checked);
      SetFlag(Parts, MAP_PLAYERUNITS, cbPlayerUnits.Checked);
      SetFlag(Parts, MAP_PLAYERDETAILS, cbDetails.Checked);
      SetFlag(Parts, MAP_NONPLAYERUNITS, cbOtherUnits.Checked);
      SetFlag(Parts, MAP_GATES, cbGates.Checked);
      // Set file to export
      SaveDialog.DefaultExt := '*.map';
      SaveDialog.Filter := 'Maps (*.map)|*.map|All files|*.*';
      SaveDialog.InitialDir := BaseDir + Game.Name;
      SaveDialog.FileName := Game.Name + '.map';
      if SaveDialog.Execute then begin
        Lines := TStringList.Create;
        if rbMap.Checked then CompileCommonMap(Lines, Parts)
        else begin
          AddMapHeader(Lines);
          CompileRegion(Lines, Map.Region(CurrRegion.Coords, Turn.Num), Parts);
        end;
        Lines.SaveToFile(SaveDialog.FileName);
        Lines.Free;
        LastMapFile := SaveDialog.FileName;
        AddExportAction.Enabled := True;
      end;
    end;
    Free;
  end;
end;

procedure TMainForm.ImportMapActionExecute(Sender: TObject);
begin
  OpenDialog.DefaultExt := '*.map';
  OpenDialog.Filter := 'Maps (*.map, map.*)|*.map;map.*|All files|*.*';
  OpenDialog.InitialDir := BaseDir + Game.Name;
  if OpenDialog.Execute then begin
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    FreeAndNil(Game.VirtTurn);
    RepLines := TStringList.Create;
    RepLines.LoadFromFile(OpenDialog.FileName);
    RepPos := 0;
    ImportMap();
    RepLines.Free;
    Game.CreateVirtualTurn;
    SetupMapLevels;
    HexMapSetup;
    HexMap.Selected := HexMap.Selected;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ExportSkillsActionExecute(Sender: TObject);
var Lines: TStrings;
begin
  if CurrUnit = nil then Exit;
  Lines := TStringList.Create;
  CompileExportSkills(CurrUnit, Lines);
  Clipboard.SetTextBuf(PChar(Lines.Text));
  Lines.Free;
end;

procedure TMainForm.ImportSkillsActionExecute(Sender: TObject);
var Lines: TStrings;
    s: string;
begin
  Lines := TStringList.Create;
  Lines.Text := Clipboard.AsText;
  s := ImportSkills(Lines);
  if s = '' then MessageDlg('No units found', mtInformation, [mbOk], 0)
  else begin
    MessageDlg(s, mtInformation, [mbOk], 0);
    ProcessAllOrders;
  end;
  Lines.Free;
  FillUnitGrid;
end;

procedure TMainForm.ExportMagesActionExecute(Sender: TObject);
var i: integer;
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  for i := 0 to VFaction.Units.Count-1 do
    if VFaction.Units[i].Mage then
      CompileExportSkills(VFaction.Units[i], Lines);
  if Lines.Count > 0 then Clipboard.SetTextBuf(PChar(Lines.Text));
  Lines.Free;
end;

procedure TMainForm.AddExportActionExecute(Sender: TObject);
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(LastMapFile);
  CompileRegion(Lines, CurrRegion, LastMapParts);
  Lines.SaveToFile(LastMapFile);
  Lines.Free;
end;

procedure TMainForm.CopyOrderActionExecute(Sender: TObject);
var Lines: TStrings;
begin
  Lines := TStringList.Create;
  CompileOrder(Lines, Config.ReadBool('MainWin', 'CompactOrders', False));
  Clipboard.SetTextBuf(PChar(Lines.Text));
  Lines.Free;
end;

procedure TMainForm.MailOrderActionExecute(Sender: TObject);
var Lines: TStrings;
    s: string;
begin
  if not ProgOpened then begin
    MessageDlg('This function is not available in unregistered version.',
      mtWarning, [mbOk], 0);
    Exit;
  end;
  Lines := TStringList.Create;
  CompileOrder(Lines, Config.ReadBool('MainWin', 'CompactOrders', False));
  s := SendEMail(GameConfig.ReadString('Game', 'From', ''),
    GameConfig.ReadString('Game', 'Server', ''),
    GameConfig.ReadString('Game', 'Subject', ''),
    Lines.Text, GameConfig.ReadBool('Game', 'MailDialog', True));
  Application.BringToFront;
  if s <> '' then MessageDlg(s, mtError, [mbOk], 0);
  Lines.Free;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutForm := TAboutForm.Create(Self);
  AboutForm.ShowModal;
  AboutForm.Free;
end;

procedure TMainForm.HelpContents1Click(Sender: TObject);
begin
  ShellExecute(Application.Handle, PChar('open'),
    PChar(BaseDir + '\Help\help.html'), PChar(0), nil, SW_NORMAL);
end;

procedure TMainForm.GameRulesItmClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, PChar('open'),
    PChar(BaseDir + '\Help\rules.html'), PChar(0), nil, SW_NORMAL);
end;

procedure TMainForm.MiniMapActionExecute(Sender: TObject);
begin
  if MiniMapForm = nil then begin
    MiniMapForm := TMiniMapForm.Create(Self, HexMap);
    MiniMapForm.Show;
  end
  else MiniMapForm.Close;
end;

procedure TMainForm.RegisterItmClick(Sender: TObject);
begin
  RegForm := TRegForm.Create(Self);
  if RegForm.ShowModal = mrOK then begin
    ImplyRegistration(True);
    MessageDlg('Thanks for registration. All limits has been removed.',
      mtInformation, [mbOK], 0);
  end
  else
    MessageDlg('Sorry, but either the registration name or code was incorrect.',
      mtWarning, [mbOK], 0);
end;

{ Turns }

procedure TMainForm.SetTurns;
var i: integer;
begin
  TurnCombo.Items.Clear;
  TurnsItm.Clear;
  for i := Game.Turns.Count-1 downto 1 do begin
    TurnCombo.Items.Add(TurnToShortDate(Game.Turns[i].Num));
    with AddMenuItem(TurnsItm, TurnToDate(Game.Turns[i].Num), -1, TurnItmClick) do begin
      RadioItem := TRUE;
      GroupIndex := 1;
      Tag := (Game.Turns.Count-1) - i;
      if i = Game.TurnIndex then Checked := TRUE;
    end;
  end;
  ApplyShortcuts(TurnsItm);
  TurnCombo.ItemIndex := Game.Turns.Count - Game.TurnIndex - 1;
  NextTurnAction.Enabled := (TurnCombo.ItemIndex > 0);
  PrevTurnAction.Enabled := (TurnCombo.ItemIndex < TurnCombo.Items.Count-1);
  FillFactionsCombo(cmFilterFaction, False, True);
end;

procedure TMainForm.TurnComboChange(Sender: TObject);
begin
  NotesMemoExit(NotesMemo);
  if Game.TurnIndex <> Game.Turns.Count - TurnCombo.ItemIndex - 1 then begin
    Game.TurnIndex := Game.Turns.Count - TurnCombo.ItemIndex - 1;
    TurnsItm.Items[Game.Turns.Count - Game.TurnIndex - 1].Checked := TRUE;
  end;
  NextTurnAction.Enabled := (TurnCombo.ItemIndex > 0);
  PrevTurnAction.Enabled := (TurnCombo.ItemIndex < TurnCombo.Items.Count-1);
  SetCaption;
  ProcessAllOrders;
end;

procedure TMainForm.PrevTurnActionExecute(Sender: TObject);
begin
  if Game.TurnIndex > 1 then begin
    TurnCombo.ItemIndex := Game.Turns.Count - Game.TurnIndex;
    TurnComboChange(Self);
  end;
end;

procedure TMainForm.NextTurnActionExecute(Sender: TObject);
begin
  if Game.TurnIndex < Game.Turns.Count-1 then begin
    TurnCombo.ItemIndex := Game.Turns.Count - Game.TurnIndex - 2;
    TurnComboChange(Self);
  end;
end;

procedure TMainForm.TurnItmClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := TRUE;
  TurnCombo.ItemIndex := TMenuItem(Sender).Tag;
  TurnComboChange(Sender);
end;

{ Application }

procedure TMainForm.SetCaption;
begin
  Caption := 'Atlantis Advisor';
  if not ProgOpened then Caption := Caption + ' (Unregistered)';
  if Game <> nil then begin
    Caption := Caption + ' - ' + TurnToDate(Turn.Num);
    if Config.ReadBool('MainWin', 'TurnInTitle', False) then
      Caption := Caption + ', turn ' + IntToStr(Turn.Num);
    Caption := Caption + ' of ' + Game.Name;
  end;
end;

procedure TMainForm.BookmarkActionExecute(Sender: TObject);
var bmk: string;
    mapX, mapY: integer;
    Lines: TStrings;
begin
  if not ProgOpened then begin
    Lines := TStringList.Create;
    GameConfig.ReadSection('Bookmarks', Lines);
    if Lines.Count > 0 then begin
      Lines.Free;
      MessageDlg('Cannot create more than one bookmark in unregistered version',
        mtWarning, [mbOk], 0);
      Exit;
    end
    else Lines.Free;
  end;
  // Make bookmark
  CalcMapCoords(HexMap.Selected.X, HexMap.Selected.Y, mapX, mapY);
  bmk := MakeRegionName(Coords(mapX, mapY, Map.Level), True);
  if InputQuery('Set bookmark', 'Enter bookmark name:', bmk) then begin
    bmk := StringReplace(bmk, '=', '', [rfReplaceAll]);
    GameConfig.WriteString('Bookmarks', bmk, IntToStr(mapx) + ' ' +
      IntToStr(mapy) + ' ' + Map.Levels[Map.Level].Name);
    SetupBookmarks;
  end;
end;

procedure TMainForm.RegionNotesItmClick(Sender: TObject);
begin
  with TMenuItem(Sender) do begin
    Checked := not Checked;
    Config.WriteBool('MainWin', 'RegionNotes', Checked);
  end;
end;

procedure TMainForm.FactionActionExecute(Sender: TObject);
var s: string;
begin
  StoreMemosInfo; // exit from other controls
  with TFactionForm.Create(Self) do begin
    ShowModal;
    s := GetOrders;
    if s <> '' then begin
      AddOrderTo(FactionLeader, s, False);
      ProcessOrders(FactionLeader.Region);
    end;
    Free;
  end;
  HexMap.Redraw;
  HexMap.Selected := HexMap.Selected;
end;

procedure TMainForm.TurnEventsActionExecute(Sender: TObject);
begin
  StoreMemosInfo; // exit from other controls
  with TTurnEventsForm.Create(Self) do begin
    Setup(Turn.Events);
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.OrdersActionExecute(Sender: TObject);
begin
  StoreMemosInfo; // exit from other controls
  with TMemoForm.Create(Self) do begin
    Caption := 'Orders';
    CompileOrder(Memo.Lines, False);
    Memo.Modified := False;
    ShowModal;
    if Memo.Modified then begin
      FreeAndNil(Game.VirtTurn);
      RepLines := TStringList.Create;
      RepLines.Text := Memo.Lines.Text;
      RepPos := 0;
      ReadOrders;
      RepLines.Free;
      Game.CreateVirtualTurn;
      ProcessAllOrders;
    end;
    Free;
  end;
end;

procedure TMainForm.RegionRepActionExecute(Sender: TObject);
begin
  with TMemoForm.Create(Self) do begin
    Caption := 'Region Report - ';
    if CurrRegion <> nil then begin
      Caption := Caption + MakeRegionName(CurrRegion.Coords, True);
      CompileRegion(Memo.Lines, Map.Region(CurrRegion.Coords, Turn.Num), MAP_ALL);
    end
    else Caption := Caption + 'Unexplored region';
    Memo.ReadOnly := True;
    Width := 550;
    Font.Name := 'Courier New';
    Font.Size := 9;
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ItemEditActionExecute(Sender: TObject);
begin
  StoreMemosInfo;
  with TItemEditForm.Create(Self) do begin
    ShowModal;
    if Modified then ProcessAllOrders;
    Free;
  end;
end;

procedure TMainForm.SkillEditActionExecute(Sender: TObject);
begin
  StoreMemosInfo;
  with TSkillEditForm.Create(Self) do begin
    ShowModal;
    if Modified then ProcessAllOrders;
    Free;
  end;
end;

procedure TMainForm.StructEditActionExecute(Sender: TObject);
begin
  StoreMemosInfo;
  with TStructEditForm.Create(Self) do begin
    ShowModal;
    if Modified then ProcessAllOrders;
    Free;
  end;
end;

procedure TMainForm.SpecEditActionExecute(Sender: TObject);
begin
  StoreMemosInfo;
  with TSpecEditForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.TerrEditActionExecute(Sender: TObject);
begin
  StoreMemosInfo;
  with TTerrEditForm.Create(Self) do begin
    ShowModal;
    if Modified then ProcessAllOrders;
    Free;
  end;
end;

procedure TMainForm.BattlesActionExecute(Sender: TObject);
var i: integer;
begin
  if (CurrRegion <> nil) and (CurrRegion.Visited = VTurn.Num)
    and (CurrRegion.Battles <> nil) then
    if ProgOpened or ((Game.Name = 'sample game') and (CurrRegion.x = 27)
      and (CurrRegion.y = 45) and (CurrRegion.Land = 'Olenek')) then
      // Battle Viewer
      with TBattleForm.Create(Self, nil) do begin
        ShowModal;
        Free;
      end
    else
      // Text report
      with TMemoForm.Create(Self) do begin
        Caption := 'Battles in ' + MakeRegionName(CurrRegion.Coords, True);
        for i := 0 to CurrRegion.Battles.Count-1 do
          Memo.Lines.Text := Memo.Lines.Text +
            CurrRegion.Battles[i].Report.Text + #13#10;
        ShowModal;
        Free;
      end;
end;

procedure TMainForm.BattleSimActionExecute(Sender: TObject);
begin
  if not ProgOpened then begin
    MessageDlg('This function is not available in unregistered version.',
      mtWarning, [mbOk], 0);
    Exit;
  end;
  with TBattleForm.Create(Self, SimBattle) do begin
    ShowModal;
    Free;
  end
end;

procedure TMainForm.SoldiersActionExecute(Sender: TObject);
begin
  with TSoldiersForm.Create(Self) do begin
    Setup(CurrUnit, CurrUnit.Region);
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.AvatarsActionExecute(Sender: TObject);
begin
  StoreMemosInfo; // exit from other controls
  with TAvatarForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
  ProcessAllOrders;
end;

procedure TMainForm.MakeAvatarActionExecute(Sender: TObject);
begin
  StoreMemosInfo; // exit from other controls
  with TAvatarForm.Create(Self) do begin
    MakeUnitAvatar(CurrUnit);
    ShowModal;
    Free;
  end;
  ProcessAllOrders;
end;

procedure TMainForm.AdvisorWarnActionExecute(Sender: TObject);
begin
  with TTurnEventsForm.Create(Self) do begin
    Caption := 'Advisor Warnings';
    Setup(ParseErrors);
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.HideInvisibleClick(Sender: TObject);
begin
  itmHideInvisRegions.Checked := not itmHideInvisRegions.Checked;
  Config.WriteBool('MainWin', 'HideInvisRegionSIC', itmHideInvisRegions.Checked);
  HexMap.Redraw;
end;

procedure TMainForm.DefineRegionwarning1Click(Sender: TObject);
var s: string;
begin
  s := InputBox('Region Warning', 'Enter expression in region context (like "x = 10"):',
    GameConfig.ReadString('Map', 'Warning', ''));
  if s <> GameConfig.ReadString('Map', 'Warning', '') then begin
    GameConfig.WriteString('Map', 'Warning', s);
    HexMap.Redraw;
  end;
end;

procedure TMainForm.ScriptEditActionExecute(Sender: TObject);
begin
  with TScriptEditForm.Create(Self) do begin
    ShowModal;
    if Modified then begin
      SetupScripts;
      ProcessAllOrders;
    end;
    Free;
  end;
end;


{ -------------- Scripts ----------------- }

procedure TMainForm.SetupScripts;
var i: integer;
begin
  LoadScripts;
  // Clear manual items
  i := 0;
  while i < ScriptsItm.Count do begin
    if ScriptsItm.Items[i].Tag = 1 then ScriptsItm.Items[i].Free
    else Inc(i);
  end;
  // Add manual scripts to menu
  for i := 0 to Scripts.Count-1 do
    if (Pos('[manual]', Scripts[i]) > 0) and ((Pos('(', Scripts[i]) = 0)
      or (Pos('[manual]', Scripts[i]) < Pos('(', Scripts[i]))) then
      with AddMenuItem(ScriptsItm, ScriptName(Scripts[i]), -1, ScriptClick) do
        Tag := 1;
  ApplyShortcuts(ScriptsItm);
end;

procedure TMainForm.SetupEngine;
begin
  EngineRunAction.Enabled := (GameConfig <> nil) and
    (GameConfig.ReadBool('Game', 'Local', False));
end;

procedure TMainForm.SetupTerrains;
var i: integer;
begin
  for i := 0 to Game.TerrainData.Count-1 do
    AddMenuItem(itmTerrain, Game.TerrainData[i].Name, -1, ChangeTerrain);
  ApplyShortcuts(itmTerrain);
end;

procedure TMainForm.ChangeTerrain(Sender: TObject);
var Ter: TTerrainData;
    mapX, mapY: integer;
    R: TRegion;
begin
  Ter := Game.TerrainData.Find(StringReplace(TMenuItem(Sender).Caption, '&', '', []));
  if Ter = nil then Exit;
  CalcMapCoords(HexMap.Selected.X, HexMap.Selected.Y, mapX, mapY);
  R := Map.Region(mapX, mapY);
  if R = nil then R := Map.SeekRegion(mapX, mapY, Map.Level);
  R.Terrain := Ter;
  HexMap.Selected := HexMap.Selected; 
end;

procedure TMainForm.ScriptClick(Sender: TObject);
var idx, i: integer;
    args: string;
begin
  idx := ScriptIndex(StringReplace(TMenuItem(Sender).Caption, '&', '', []));
  i := Pos('(', Scripts[idx]);
  if i > 0 then begin
    args := ParensContents(Scripts[idx], i);
    if InputQuery('Script', 'Enter script arguments', args) then
      ProcessScript(idx, args);
  end
  else ProcessScript(idx, '');
end;

procedure TMainForm.TownTradeActionExecute(Sender: TObject);
begin
  with TTownTradeForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.WantedItemsActionExecute(Sender: TObject);
begin
  with TWantedItemsForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.TradeStructActionExecute(Sender: TObject);
begin
  with TTradeStructForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.TaxTradeActionExecute(Sender: TObject);
begin
  with TTaxTradeForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ForcesActionExecute(Sender: TObject);
begin
  with TUArmyForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.RegionStatsActionExecute(Sender: TObject);
begin
  with TRegStatForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.SpyRepsActionExecute(Sender: TObject);
begin
  with TSpyRepForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.EngineRunActionExecute(Sender: TObject);
var Lines: TStrings;
    Log: TStrings;
    rerun: boolean;
    dir: string;
begin
  Screen.Cursor := crHourGlass;

  // Save orders
  dir := GetLocalFolder(Turn.Num + 1);
  rerun := DirectoryExists(dir);
  if not rerun then ForceDirectories(dir);
  SaveOrderActionExecute(Self);
  // Run turn
  RunLocalTurn(Turn.Num + 1, True);
  // Modify game reports
  Reports.Add(dir + 'report.3');
  // Read report
  Lines := TStringList.Create;
  Log := TStringList.Create;
  Lines.LoadFromFile(dir + 'report.3');
  ReadRep(Lines, Log);
  Lines.Free;
  Log.Free;
  StartupGame;
end;

procedure TMainForm.UnitProductionActionExecute(Sender: TObject);
begin
  with TUnitProductionForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.GatesActionExecute(Sender: TObject);
begin
  with TGatesForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ItemStatsActionExecute(Sender: TObject);
begin
  with TItemStatsForm.Create(Self) do begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.UnitFilterActionExecute(Sender: TObject);
begin
  if not Filter.Enabled then
    with TUnitFilterForm.Create(Self) do begin
      if ShowModal = mrOk then begin
        Filter.Enabled := not UnitFilterEmpty;
        FillUnitGrid;
      end;
      Free;
    end
  else begin
    Filter.Enabled := False;
    FillUnitGrid;
  end;
  UnitFilterAction.Checked := Filter.Enabled;
end;

procedure TMainForm.FindUnitActionExecute(Sender: TObject);
var s: string;
    U: TUnit;
begin
  if not InputQuery('Find Unit', 'Enter unit number:', s) then Exit;
  if ToInt(s) <= 0 then Exit;
  U := Turn.FindUnit(ToInt(s));
  if U <> nil then SelectUnit(U)
  else MessageDlg('Unit ' + s + ' not found.', mtInformation, [mbOk], 0);
end;

procedure TMainForm.NextErrorActionExecute(Sender: TObject);
begin
  if ParseErrors.Count = 0 then Exit;
  if LastError >= ParseErrors.Count then LastError := 0;
  if (ParseErrors.Objects[LastError] = CurrUnit)
    or (ParseErrors.Objects[LastError].ClassType <> TUnit) then begin
    Inc(LastError);
    while (LastError < ParseErrors.Count)
      and (ParseErrors.Objects[LastError].ClassType <> TUnit) do
        Inc(LastError);
    if LastError = ParseErrors.Count then begin
      LastError := 0;
      while (LastError < ParseErrors.Count)
        and (ParseErrors.Objects[LastError].ClassType <> TUnit) do Inc(LastError);
    end;
  end;
  if (LastError < ParseErrors.Count)
    and (ParseErrors.Objects[LastError].ClassType = TUnit) then
    SelectUnit(TUnit(ParseErrors.Objects[LastError]));
end;

procedure TMainForm.ReloadScriptsActionExecute(Sender: TObject);
begin
  SetupScripts;
  ProcessAllOrders;
end;

procedure TMainForm.UnmodItemsActionExecute(Sender: TObject);
begin
  UnmodItemsAction.Checked := not UnmodItemsAction.Checked;
  Config.WriteBool('MainWin', 'UnmodItemAmounts', UnmodItemsAction.Checked);
  if CurrUnit <> nil then FillUnitInfo(CurrUnit);
end;

function TMainForm.SelectedCoords: TCoords;
begin
  CalcMapCoords(HexMap.Selected.X, HexMap.Selected.Y, Result.X, Result.Y);
  Result.z := Map.Level;
end;









end.
