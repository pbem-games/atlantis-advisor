{ Structures for player, factions, units and areas }

unit DataStructs;

interface

uses
  Classes, SysUtils, Windows, Math, Graphics;

const
  MaxInt = 2147483647;

  // Item flags
  IT_WAGON = $02;
  IT_SILVER = $04;
  IT_MAN = $08;
  IT_MONSTER = $10;
  IT_MAGIC = $20;
  IT_WEAPON = $40;
  IT_ARMOR = $80;
  IT_MOUNT = $100;
  IT_TOOL = $200;
  IT_CANTGIVE = $400;
  IT_RESOURCE = $800;
  IT_ADVANCED = $1000;
  IT_FOOD = $2000;
  IT_UNKNOWN = $4000;
  IT_ORMATERIALS = $8000;
  IT_TRADE =   $10000;
  IT_SPECIAL = $20000;
  IT_GOOD =    $40000;
  IT_EVIL =    $80000;
  IT_ALL = $FFFFFF;

  WPN_NEEDSKILL = $1; // No bonus or use unless skilled
  WPN_ALWAYSREADY = $2; // Ignore the 50% chance to attack
//  WPN_NODEFENSE = $4; // No combat defense against this weapon
  WPN_NOFOOT = $8; // Weapon cannot be used on foot (e.g. lance)
  WPN_NOMOUNT = $10; // Weapon cannot be used mounted (e.g. pike)
  WPN_SHORT = $20; // Short melee weapon (e.g. shortsword, hatchet)
  WPN_LONG = $40; // Long melee weapon (e.g. lance, pike)
  WPN_RANGED = $80; // Missile weapon
  WPN_NOATTACKERSKILL = $100; // Attacker gets no combat/skill defense.
  WPN_RIDINGBONUS = $200; // Unit gets riding bonus on att and def.
  WPN_RIDINGBONUSDEFENSE = $400; // Unit gets riding bonus on def only.
  WPN_NUMATTSKILL = $800; // Unit got number of attack = skill + NumAttacks
  WPN_NUMATTHALFSKILL = $1000; // Same, skill div 2 round up

  ARM_USEINASS = $01;

  // Skill flags
  SK_MAGIC = $01;
  SK_COMBATSPELL = $02;
  SK_CAST = $04;
  SK_FOUNDATION = $08;
  SK_UNKNOWN = $80;
  SK_ALL = $FF;

  // Struct flags
  ST_DEFENCE = $01;
  ST_TRANSPORT = $02;
  ST_FLYING = $04;
  ST_CLOSED = $08;
  ST_SHAFT = $10;
  ST_ROAD = $20;
  ST_UNKNOWN = $80;

  // Terrain flags
  TER_RIDINGMOUNTS = $01;
  TER_FLYINGMOUNTS = $02;
  TER_WATER = $04;

  // Effects
  EFF_ONESHOT = $01;

  // Specials
  SP_USE_LEV = $01;
  SP_NOBUILDING = $02;
  SP_SHIELD = $04;

  HIT_NOMONSTER =      $1000;
  HIT_EFFECTEXCEPT =   $2000;
  HIT_BUILDINGEXCEPT = $4000;
  HIT_ITEMEXCEPT =     $8000;
  HIT_ILLUSION =       $10000;

  // Unit marks
  UM_BATTLE = $01;
  UM_DEADMEN = $02;
  UM_ALLY = $04;
  UM_ATTACK = $08;
  UM_PROMOTED = $10;

  // Region marks
  RM_TAX = $01;
  RM_TRADE = $02;
  RM_FISHING = $04;
  RM_ROADBUILD = $08;

  // Attack types
  atMelee = 0;
  atEnergy = 1;
  atSpirit = 2;
  atWeather = 3;
  atRiding = 4;
  atRanged = 5;
  atNonResistable = 6;
  atCount = 7;

  // Weapon class
  wcSlashing = 0;
  wcPiercing = 1;
  wcCrushing = 2;
  wcCleaving = 3;
  wcArmorPiercing = 4;
  wcEnergy = 5;
  wcSpirit = 6;
  wcWeather = 7;
  wcCount = 8;

  // Movement types
  mtNone = 0;
  mtWalk = 1;
  mtRide = 2;
  mtFly = 3;
  mtSwim = 4;
  mtTransport = 5;

  // Consume types
  ctSilver = 0;
  ctUnit = 1;
  ctFaction = 2;

  // Reveal types
  rtHide = 0;
  rtUnit = 1;
  rtFaction = 2;

  // Spoil types
  spoilAll = 0;
  spoilNone = 1;
  spoilWalk = 2;
  spoilRide = 3;
  spoilFly = 4;

  // Settlement types
  setlVillage = 1;
  setlTown = 2;
  setlCity = 3;

  dirN = 1;
  dirNE = 2;
  dirSE = 3;
  dirS = 4;
  dirSW = 5;
  dirNW = 6;

 // Unit flags
  UnitFlagsCount = 8;
  FlagOrders: array[0..UnitFlagsCount-1] of string =
    ('guard', 'autotax', 'avoid', 'behind', 'hold', 'noaid', 'nocross', 'share');
  ExtFlags: array[0..2, 0..4] of string =
  ( ('consume', 'consume unit', 'consume faction', '', ''),
    ('reveal', 'reveal unit', 'reveal faction', '', ''),
    ('spoils all', 'spoils none', 'spoils walk', 'spoils ride', 'spoils fly') );

  flgGuard = 0;
  flgTax = 1;
  flgAvoid = 2;
  flgBehind = 3;
  flgHold = 4;
  flgNoaid = 5;
  flgNocross = 6;
  flgShare = 7;

  attHostile = 1;
  attUnfriendly = 2;
  attNeutral = 3;
  attFriendly = 4;
  attAlly = 5;

  sideAttack = False;
  sideDefence = True;

  alNeutral = 0;
  alGood = 1;
  alEvil = 2;

  weatherClear = 0;
  weatherWinter = 1;
  weatherMonsoon = 2;
  weatherCount = 3;

  areaNorth = 0;
  areaTropical = 1;
  areaSouth = 2;

  raCast = 0;
  raTakeHits = 1;
  raHeals = 2;
  raSideLoses = 3;
  raMeleeAttack = 4;
  raTacticsBonus = 5;

  ItemClassCount = 20;
  ItemClasses: array[0..ItemClassCount-1] of string = ('normal', 'advanced',
    'trade', 'man', 'men', 'monster', 'monsters', 'magic', 'weapon',
    'weapons', 'armor', 'mount', 'mounts', 'battle', 'special', 'tool',
    'tools', 'food', 'item', 'items');
  ItemFilters: array[0..ItemClassCount-1] of DWord = (IT_ALL - IT_ADVANCED,
    IT_ADVANCED, IT_TRADE, IT_MAN, IT_MAN, IT_MONSTER, IT_MONSTER,
    IT_MAGIC, IT_WEAPON, IT_WEAPON, IT_ARMOR, IT_MOUNT, IT_MOUNT,
    IT_MAGIC, IT_MAGIC, IT_TOOL, IT_TOOL, IT_FOOD, IT_ALL, IT_ALL);

  HealDefs: array[0..5, 0..1] of integer = ((0, 0), (10, 50), (10, 50),
    (25, 75), (25, 75), (100, 90));

  modStandard = 0;
  modTarmellion = 1;
  modMagicdeep = 2;
  modNewOrigins = 3;

  modFirst = modStandard;
  modLast = modNewOrigins;

  prMartial = 0;
  prWar = 1;
  prTrade = 2;
  prQMast = 3;
  prMagic = 4;
  prAppr = 5;
  prFish = 6;
  prRoad = 7;

  prFirst = prMartial;
  prLast = prRoad;
  prCount = 8;

  ProgressNames: array[prFirst..prLast] of string =
    ('Martial', 'War', 'Trade', 'Quartermasters', 'Magic', 'Apprentices', 'Fishing', 'Road Build');

  ModProgress: array[modFirst..modLast] of set of prFirst..prLast =
    ([prWar, prTrade, prMagic, prAppr],                     // modStandard
     [prWar, prTrade, prMagic, prAppr],                     // modTarmellion
     [prWar, prTrade, prMagic, prAppr, prFish, prRoad],     // modMagicdeep
     [prMartial, prQMast, prMagic, prAppr]);                // modNewOrigins


type
  TCoords = record
    x, y, z: integer;
  end;
  TCoordArray = array of TCoords;
  TPointArray = array of TPoint;
  TIntArray = array of integer;

  TFactionList = class;
  TFactionDataList = class;
  TTroopList = class;
  TItemList = class;
  TItemDataList = class;
  TSkillList = class;
  TSkillDataList = class;
  TStructList = class;
  TStructDataList = class;
  TTerrainDataList = class;
  TTurnList = class;
  TUnitList = class;
  TRegionList = class;
  TWeatherDataList = class;
  TEffectDataList = class;
  TSpecDataList = class;
  TMapLevelList = class;
  TBattleList = class;
  TBattleUnitList = class;
  TUArmyList = class;

  TData = class
    Requested: boolean;
    Incomplete: boolean;
  end;

  TSkill = class;
  TSkillData = class;
  TStructData = class;
  TSpecData = class;
  TItem = class;

  TItemData = class(TData)
    Flags: DWord;
    SingleName, MultiName, Short: string;
    Description: string;
    Weight: integer;
    Moves: array[1..4] of integer;
    BasePrice: integer;
    Special: TSpecData;
    SpecLevel: integer;
    Produce: record
      Skill: TSkill;
      Rate, ManMonths: integer;
      Materials: TItemList;
      Tool: TItemData;
      ToolBonus: integer;
      Byproducts: TItemList;
      RequiredStruct: TStructData;
    end;
    MagProduce: record
      Skill: TSkill;
      Materials: TItemList;
    end;
    // Additional holders
    Man: record
      Leader: boolean;
      DefLevel, MagDefLevel: integer;
      SpecSkills: TSkillList;
    end;
    Monster: record
      Attack: integer;
      Defence: array[0..atCount-1] of integer;
      NumAttacks, Hits, Regen, Tactics, Stealth, Observation: integer;
      Illusion: boolean;
    end;
    Weapon: record
      Flags: DWord;
      WpnClass: integer;
      Skill1, Skill2: TSkillData;
      AttackType: integer;
      AttackBonus, DefenceBonus, MountBonus: integer;
      // For numAttacks:
      // - A positive number is the number of attacks per round
      //    or + skill level for WPN_NUMATTSKILL
      // - A negative number is the number of rounds per attack.
      NumAttacks: integer;
    end;
    Armor: record
      Flags: DWord;
      Defence: array[0..wcCount-1] of integer;
    end;
    Mount: record
      RideSkill: TSkillData;
      MinBonus, MaxBonus, MaxHamperedBonus: integer;
    end;
    Wagon: record
      Hitched: TItemData;
      HitchWalk: integer;
    end;
    Magic: record
      MageOnly: boolean;
    end;
    constructor Create(AShort: string);
    destructor Destroy; override;
    function Name(Many: boolean): string; overload;
    function Name: string; overload;
  end;

  TItem = class
    Data: TItemData;
    Amount: integer;
    Cost: integer;
    Bought: boolean;
    procedure Assign(Source: TItem);
    function Name: string;
  end;

  TSkillData = class(TData)
    Name, Short: string;
    Flags: DWord;
    Cost: integer;
    BasedOn: TSkillList;
    Descriptions: TStrings;
    Special: TSpecData;
    constructor Create(AShort: string);
    destructor Destroy; override;
    function MakeName: string;
  end;

  TSkill = class
    Data: TSkillData;
    Level: integer;
    Points: integer;
    procedure Assign(Source: TSkill);
    function FullName: string;
  end;

  // Size: amount of material to build
  // Protection: max amount of men gains combat defence
  // Capacity: cargo capacity for ship
  // Sailors: men*skill level to steer
  TStructData = class(TData)
    Flags: DWord;
    Group: string;
    Description: string;
    Size, Protection: integer;
    Capacity, Sailors: integer;
    Material1, Material2: TItemData;
    BuildSkill: TSkill;
    Resource: TItemData;
    Tool: TItemData;
    ToolBonus: integer;
    Defence: array[0..atCount-1] of integer;
    constructor Create(AGroup: string);
    destructor Destroy; override;
  end;

  TRegion = class;
  TUnit = class;

  TStruct = class
    Data: TStructData;
    Name, Description: string;
    Num: integer;
    Needs: integer;
    Passage: TCoords; // for shafts
    Runes: integer;
    Owner: TUnit;
    Protection: integer; // for battle calculations
    constructor Create;
    procedure Assign(Source: TStruct);
    function HasExit: boolean;
  end;

  TTerrainData = class(TData)
    Name: string;
    Flags: DWord;
    BmpIndexes: array[0..weatherCount-1] of integer;
    MoveCost: integer;
    Color: TColor;
    AdvResources: TItemDataList;
    constructor Create(AName: string);
    destructor Destroy; override;
  end;

  TWeatherData = class(TData)
    LastText, NextText: string;
    Text: string;
    MoveCost: integer;
    constructor Create(AText: string);
  end;

  TEffectData = class(TData)
    Name: string;
    Flags: DWord;
    Attack: integer;
    Defence: array[0..atCount-1] of integer;
  end;

  TSpecData = class(TData)
    Name: string;
    Flags: DWord;
    Description: string;
    SpellText: string;
    Shields: array[0..atCount-1] of boolean;
    Attacks: array of record
      WeaponFlags: DWord;
      AttackType, MinAmt, MaxAmt, WeaponClass: integer;
      Effect: TEffectData;
    end;
    Defences: array of record
      AttackType, Bonus: integer;
    end;
    Effects: TEffectDataList;
    Structs: TStructDataList;
    Items: TItemDataList;
    constructor Create(Name: string);
    destructor Destroy; override;
  end;

  TFactionData = class(TData)
    Num, ColorIndex: integer;
    EMail: string;
    TurnNum: integer;
    constructor Create(Num: integer);
  end;

  TFaction = class
    Data: TFactionData;
    Name: string;
    Num: integer;
    Player: boolean;
    Attitude: integer;
    Units: TUnitList;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TFaction; CopyUnits: boolean);
  end;

  TTroop = class
    Faction: TFaction;
    Units: TUnitList;
    constructor Create;
    destructor Destroy; override;
  end;

  TRegion = class
    x, y, z: integer;
    FullData: boolean;
    Visited: integer;
    IsVirtual: boolean;
    HasExit: array[1..6] of boolean;

    Terrain: TTerrainData;
    Land: string;

    Settlement: string;
    SettlementType: integer;
    Peasants: TItem;
    TaxRate: integer;

    WeatherLast, WeatherNext: TWeatherData; // Reference to WeatherData
    Wages, MaxWages, Entertainment: integer;

    Gate: integer;
    Guard: TFaction;
    Marks: DWord;
    Activity: record
      Taxers, Workers, Entertainers: integer;
    end;
    OtherFactions: record
      Taxers, Workers, Entertainers: integer;
      Products: string;
    end;

    Wanted: TItemList;
    ForSale: TItemList;
    Products: TItemList;
    Structs: TStructList;
    Troops: TTroopList;
    Battles: TBattleList;
    Notes: TStrings;
  private
    function GetCoords: TCoords;
    procedure SetCoords(const Value: TCoords);
  public
    constructor Create(X, Y, Z, TurnNum: integer);
    destructor Destroy; override;
    procedure Assign(Source: TRegion; CopyTurnData: boolean);
    property Coords: TCoords read GetCoords write SetCoords;
    function PlayerTroop: TTroop;
    function FindUnit(Num: integer): TUnit;
  end;

  TMapLevel = class
    Name: string;
    Empty: boolean;
    MaxPoint: TPoint; // Limits for autodetect
    RCols: TList; // Sorted list of region collections (sort: x, y)
    Bounds: TRect;
    constructor Create(Name: string);
    destructor Destroy; override;
  end;

  TMap = class
    TurnNum: integer;
    Level: integer;
    Levels: TMapLevelList;
    function Region(X, Y: integer): TRegion; overload;
    function Region(X, Y, Z: integer): TRegion; overload;
    function Region(C: TCoords): TRegion; overload;
    function Region(C: TCoords; T: integer): TRegion; overload;
    function Region(X, Y, Z, T: integer): TRegion; overload;
    function SeekRegion(X, Y, Z: integer): TRegion; overload;
    function SeekRegion(C: TCoords): TRegion; overload;
    procedure CreateVirtualLayer;
    procedure FreeVirtualLayer;
    constructor Create;
    destructor Destroy; override;
  end;

  TUArmy = class
    Name: string;
    Color: TColor;
    UnitIds: TStrings;
    AutoDistribute, AutoGiveaway: boolean;
    Markitants: string;
    constructor Create(Name: string);
    destructor Destroy; override;
  end;

  TBaseUnit = class
    Name, Description: string;
    Num: integer;
    Mage: boolean;
    Faction: TFaction;
    Struct: TStruct;
    Items: TItemList;
    Skills: TSkillList;
    CombatSpell: TSkillData;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TBaseUnit);
    function NumStr: string;
  end;

  TUnit = class(TBaseUnit)
    Flags: array[0..UnitFlagsCount-1] of boolean;
    Marks: DWord;
    Revealing: integer;
    Consuming: integer;
    Spoils: integer;
    Events, Orders: TStrings;
    MonthOrder: string;
    MonthInfo: record
      Amount, Max: integer;
      Data: pointer;
      Details: string;
    end;
    Moves, Route: TCoordArray;
    FinalPoint: integer;
    Former: TUnit;  // for "new 1"... units
    TradeIncome, WorkIncome: integer;

    Region: TRegion;
    CanStudy: TSkillDataList;
    ReadyWeapons, ReadyArmor: TItemDataList;
    UArmy: TUArmy;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TUnit);
    function Order(Index: integer): string;
    procedure ChangeFaction(NewNum: integer);
    function Id: string;
    function FullName: string;
    function FinalCoords: TCoords;
    function ArrivingTo(C: TCoords): boolean;
  end;

  TBattleUnit = class(TBaseUnit)
    Behind, Damaged: boolean;
    SpellText: string;
    URef: TUnit;
    Loses: integer;
    Side: boolean;
    Viewer: record
      Picture: string;
    end;
    procedure Assign(Source: TBattleUnit);
  end;

  TGame = class;

  TTurn = class
    Num: integer;
    Martial, War, Trade, Magic: integer;
    FPUsed: array[0..prCount-1] of integer;
    GateCount: integer;
    Unclaimed: integer;
    Mana: integer;

    Factions: TFactionList;
    Events: TStringList;
    Regions: TRegionList;

    constructor Create(ANum: integer);
    destructor Destroy; override;
    procedure Assign(Source: TTurn);
    function FindUnit(Num: integer): TUnit;
  end;

  TGame = class
  private
    FTurnIndex: integer;
    procedure SetTurnIndex(Value: integer);
  public
    Name, Password: string;
    Turns: TTurnList;
    VirtTurn: TTurn;
    Map: TMap;

    FactionData: TFactionDataList;
    SkillData: TSkillDataList;
    StructData: TStructDataList;
    ItemData: TItemDataList;
    TerrainData: TTerrainDataList;
    WeatherData: TWeatherDataList;
    EffectData: TEffectDataList;
    SpecData: TSpecDataList;
    UArmies: TUArmyList;

    constructor Create(AName: string);
    destructor Destroy; override;
    property TurnIndex: integer read FTurnIndex write SetTurnIndex;
    procedure RecreateVRegion(C: TCoords);
    procedure CreateVirtualTurn;
    procedure SaveVTurnOrders;
  end;

  TBattleAction = record
    BUnit, Target: TBattleUnit;
    ActionType, Power: integer;
    Special: TSpecData;
  end;

  TRound = record
    Num: integer; // 0 = free
    Actions: array of TBattleAction;
  end;

  TBattle = class
    Name: string;
    Region: TRegion;
    Report: TStrings;
    Units: array[sideAttack..sideDefence] of TBattleUnitList;
    Rounds: array of TRound;
    Spoils: TItemList;
    Loses: array[sideAttack..sideDefence] of integer;
    constructor Create(Name: string);
    destructor Destroy; override;
    procedure Assign(Source: TBattle);
    function FindBUnit(Num: integer): TBattleUnit;
  end;

  { Type lists }

  TMapLevelList = class(TList)
  protected
    function Get(Index: Integer): TMapLevel;
    procedure Put(Index: Integer; Item: TMapLevel);
  public
    property Items[Index: Integer]: TMapLevel read Get write Put; default;
    function Seek(AName: string): TMapLevel;
    function NumOf(AName: string): integer;
    procedure ClearAndFree;
  end;

  TFactionList = class(TList)
  protected
    function Get(Index: Integer): TFaction;
    procedure Put(Index: Integer; Item: TFaction);
  public
    property Items[Index: Integer]: TFaction read Get write Put; default;
    function Seek(Num: integer): TFaction;
    function Find(Num: integer): TFaction;
    function SeekByName(Name: string): TFaction;
    procedure ClearAndFree;
    procedure AssignItems(Source: TFactionList; CopyUnits: boolean);
  end;

  TTroopList = class(TList)
  protected
    function Get(Index: Integer): TTroop;
    procedure Put(Index: Integer; Item: TTroop);
  public
    property Items[Index: Integer]: TTroop read Get write Put; default;
    function Find(Num: integer): TTroop;
    function Seek(Num: integer): TTroop;
    procedure ClearAndFree;
  end;

  TItemList = class(TList)
  protected
    function Get(Index: Integer): TItem;
    procedure Put(Index: Integer; Item: TItem);
  public
    property Items[Index: Integer]: TItem read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
    function Find(Short: string): TItem;
    function Seek(Short: string): TItem;
    procedure AssignItems(Source: TItemList);
    function Amount(Mask: DWord): integer;
  end;

  TSkillList = class(TList)
  protected
    function Get(Index: Integer): TSkill;
    procedure Put(Index: Integer; Item: TSkill);
  public
    function Find(Short: string): TSkill;
    function Seek(Short: string): TSkill;
    property Items[Index: Integer]: TSkill read Get write Put; default;
    procedure AssignItems(Source: TSkillList);
    procedure ClearAndFree;
    procedure ClearItems;
  end;

  TStructList = class(TList)
  protected
    function Get(Index: Integer): TStruct;
    procedure Put(Index: Integer; Item: TStruct);
  public
    property Items[Index: Integer]: TStruct read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
    function Find(Num: integer): TStruct;
    procedure AssignItems(Source: TStructList; CopyTurnData: boolean);
  end;

  TTurnList = class(TList)
  protected
    function Get(Index: Integer): TTurn;
    procedure Put(Index: Integer; Item: TTurn);
  public
    property Items[Index: Integer]: TTurn read Get write Put; default;
    function Seek(Num: integer): TTurn;
    function Find(Num: integer): TTurn;
    procedure ClearAndFree;
  end;

  TUnitList = class(TList)
  protected
    function Get(Index: Integer): TUnit;
    procedure Put(Index: Integer; Item: TUnit);
  public
    property Items[Index: Integer]: TUnit read Get write Put; default;
    function Find(ANum: integer): TUnit;
    function FindById(Id: string): TUnit;
    procedure ClearAndFree;
  end;

  TRegionList = class(TList)
  protected
    function Get(Index: Integer): TRegion;
    procedure Put(Index: Integer; Item: TRegion);
  public
    property Items[Index: Integer]: TRegion read Get write Put; default;
  end;

  TBattleUnitList = class(TList)
  protected
    function Get(Index: Integer): TBattleUnit;
    procedure Put(Index: Integer; Item: TBattleUnit);
  public
    property Items[Index: Integer]: TBattleUnit read Get write Put; default;
    procedure ClearAndFree;
    procedure ClearItems;
    function Find(Num: integer): TBattleUnit;
    function Seek(Num: integer): TBattleUnit;
    procedure AssignItems(Source: TBattleUnitList);
  end;

  TBattleList = class(TList)
  protected
    function Get(Index: Integer): TBattle;
    procedure Put(Index: Integer; Item: TBattle);
  public
    property Items[Index: Integer]: TBattle read Get write Put; default;
    procedure ClearItems;
    procedure ClearAndFree;
  end;

  TUArmyList = class(TList)
  protected
    function Get(Index: Integer): TUArmy;
    procedure Put(Index: Integer; Item: TUArmy);
  public
    property Items[Index: Integer]: TUArmy read Get write Put; default;
    procedure ClearAndFree;
  end;

  { Data lists }

  TFactionDataList = class(TList)
  protected
    function Get(Index: Integer): TFactionData;
    procedure Put(Index: Integer; Item: TFactionData);
  public
    property Items[Index: Integer]: TFactionData read Get write Put; default;
    function Seek(Num: integer): TFactionData;
    procedure ClearAndFree;
  end;

  TItemDataList = class(TList)
  protected
    function Get(Index: Integer): TItemData;
    procedure Put(Index: Integer; Item: TItemData);
  public
    property Items[Index: Integer]: TItemData read Get write Put; default;
    function Find(Short: string): TItemData;
    function Seek(Short: string): TItemData;
    function FindByName(AName: string): TItemData;
    procedure ClearAndFree;
  end;

  TSkillDataList = class(TList)
  protected
    function Get(Index: Integer): TSkillData;
    procedure Put(Index: Integer; Item: TSkillData);
  public
    property Items[Index: Integer]: TSkillData read Get write Put; default;
    function Find(Short: string): TSkillData;
    function FindByName(Name: string): TSkillData;
    function Seek(Short: string): TSkillData;
    procedure ClearAndFree;
  end;

  TStructDataList = class(TList)
  protected
    function Get(Index: Integer): TStructData;
    procedure Put(Index: Integer; Item: TStructData);
  public
    property Items[Index: Integer]: TStructData read Get write Put; default;
    function Find(Group: string): TStructData;
    function Seek(Group: string): TStructData;
    procedure ClearAndFree;
  end;

  TTerrainDataList = class(TList)
  protected
    function Get(Index: Integer): TTerrainData;
    procedure Put(Index: Integer; Item: TTerrainData);
  public
    property Items[Index: Integer]: TTerrainData read Get write Put; default;
    function Find(Name: string): TTerrainData;
    function Seek(Name: string): TTerrainData;
    procedure ClearAndFree;
  end;

  TWeatherDataList = class(TList)
  protected
    function Get(Index: Integer): TWeatherData;
    procedure Put(Index: Integer; Item: TWeatherData);
  public
    property Items[Index: Integer]: TWeatherData read Get write Put; default;
    function Seek(Text: string): TWeatherData;
    procedure ClearAndFree;
  end;

  TEffectDataList = class(TList)
  protected
    function Get(Index: Integer): TEffectData;
    procedure Put(Index: Integer; Item: TEffectData);
  public
    property Items[Index: Integer]: TEffectData read Get write Put; default;
    function Seek(Name: string): TEffectData;
    procedure ClearAndFree;
  end;

  TSpecDataList = class(TList)
  protected
    function Get(Index: Integer): TSpecData;
    procedure Put(Index: Integer; Item: TSpecData);
  public
    property Items[Index: Integer]: TSpecData read Get write Put; default;
    function FindByText(Text: string): TSpecData;
    function Seek(Name: string): TSpecData;
    procedure ClearAndFree;
  end;

var
  Game: TGame;
  Map: TMap;
  History: TTurn; // Game.Turns[0]
  Turn, VTurn: TTurn; // Current turn
  Faction, VFaction: TFaction; // Turn.Factions[1]; general player's faction

  CurrUnit: TUnit;
  CurrRegion: TRegion;
  CurrStruct: TStruct;

  SimBattle: TBattle; // Stand-alone battle for simulator
  SimRegion: TRegion;
  SilverData: TItemData;

  Progress: array[prFirst..prLast] of array of integer;

  WeatherMonths:  array[0..2, 1..12] of TWeatherData;

  // Flags
  function Test(A, B: DWord): boolean;
  procedure SetFlag(var Flags: DWord; Flag: DWord; Value: boolean); overload;
  procedure SetFlag(var Flags: DWord; Flag: DWord); overload;
  // Coords
  function Coords(X, Y, Z: integer): TCoords;
  function EqualPoints(A, B: TPoint): boolean;
  function EqualCoords(C1, C2: TCoords): boolean;
  function StrToCoords(s: string): TCoords;
  function CoordsToStr(C: TCoords): string;
  procedure AddCoords(var A: TCoordArray; C: TCoords);
  procedure AddInt(var A: TIntArray; I: integer);
  // Progress
  function ProgressCount(AMod: integer): integer;
  function IndexToProgress(AMod, AIndex: integer): integer;
  procedure SetProgress(pr, fp, value: integer);


implementation

function ProgressCount(AMod: integer): integer;
var
  i:  integer;
begin
  Result := 0;
  for i := prFirst to prLast do
    if i in ModProgress[AMod] then
      Inc(Result);
end;

function IndexToProgress(AMod, AIndex: integer): integer;
var
  pr: integer;
begin
  Result := -1;
  for pr := prFirst to prLast do
    if pr in ModProgress[AMod] then
    begin
      if AIndex = 0 then
      begin
        Result := pr;
        break;
      end
      else
        Dec(AIndex);
    end;
end;

procedure SetProgress(pr, fp, value: integer);
var i: integer;
begin
  for i := 0 to prCount-1 do
    if Length(Progress[i]) <= fp then SetLength(Progress[i], fp + 1);
  Progress[pr][fp] := value;
end;

function EqualPoints(A, B: TPoint): boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

function Coords(X, Y, Z: integer): TCoords;
begin
  Result.x := X;
  Result.y := Y;
  Result.z := Z;
end;

procedure AddCoords(var A: TCoordArray; C: TCoords);
begin
  SetLength(A, Length(A) + 1);
  A[Length(A) - 1] := C;
end;

procedure AddInt(var A: TIntArray; I: integer);
begin
  SetLength(A, Length(A) + 1);
  A[Length(A) - 1] := I;
end;

function Test(A, B: DWord): boolean;
begin
  Result := (A and B <> 0);
end;

procedure SetFlag(var Flags: DWord; Flag: DWord; Value: boolean);
begin
  if Value then Flags := Flags or Flag
  else Flags := Flags and not Flag;
end;

procedure SetFlag(var Flags: DWord; Flag: DWord);
begin
  SetFlag(Flags, Flag, True);
end;

function EqualCoords(C1, C2: TCoords): boolean;
begin
  Result := (C1.x = C2.x) and (C1.y = C2.y) and (C1.z = C2.z);
end;

function StrToCoords(s: string): TCoords;
var i: integer;
begin
  if s = '' then begin
    Result.x := 0;
    Result.y := 0;
    Result.z := -1;
  end
  else begin
    i := Pos(' ', s);
    Result.x := StrToInt(Copy(s, 1, i-1));
    s := Copy(s, i+1, Length(s));
    i := Pos(' ', s);
    Result.y := StrToInt(Copy(s, 1, i-1));
    s := Copy(s, i+1, Length(s));
    Result.z := StrToInt(Copy(s, 1, Length(s)));
  end;
end;

function CoordsToStr(C: TCoords): string;
begin
  Result := IntToStr(C.x) + ' ' + IntToStr(C.y) + ' ' + IntToStr(C.z);
end;

  { TGame }

constructor TGame.Create(AName: string);
begin
  Game := Self;
  Name := AName;
  Turns := TTurnList.Create;
  Map := TMap.Create;
  DataStructs.Map := Map;
  // Data lists
  FactionData := TFactionDataList.Create;
  ItemData := TItemDataList.Create;
  SkillData := TSkillDataList.Create;
  StructData := TStructDataList.Create;
  TerrainData := TTerrainDataList.Create;
  WeatherData := TWeatherDataList.Create;
  EffectData := TEffectDataList.Create;
  SpecData := TSpecDataList.Create;
  UArmies := TUArmyList.Create;
  // Create history turn
  History := TTurn.Create(0);
  Turns.Add(History);
end;

destructor TGame.Destroy;
begin
  FreeAndNil(VirtTurn);
  Turns.ClearAndFree;
  Map.Free;
  // Data lists
  FactionData.ClearAndFree;
  ItemData.ClearAndFree;
  SkillData.ClearAndFree;
  StructData.ClearAndFree;
  TerrainData.ClearAndFree;
  WeatherData.ClearAndFree;
  EffectData.Free;
  SpecData.ClearAndFree;
  UArmies.Free;
end;

procedure TGame.SetTurnIndex(Value: integer);
begin
  CurrUnit := nil;
  CurrRegion := nil;
  CurrStruct := nil;
  FTurnIndex := Value;
  // Make aliases for current turn
  Turn := Turns[FTurnIndex];
  Faction := Turn.Factions[1];
  // Setup map
  Map.TurnNum := Turn.Num;
end;

procedure TGame.SaveVTurnOrders;
var u: integer;
    NewFaction: TFaction;
    OldUnit, NewUnit: TUnit;

  function FindRealUnit(VirtUnit: TUnit; TurnNum: integer): TUnit;
  var Fac: TFaction;
      Turn: TTurn;
  begin
    Turn := Game.Turns.Find(TurnNum);
    Fac := Turn.Factions.Seek(VirtUnit.Faction.Num);
    Result := Fac.Units.Find(VirtUnit.Num);
  end;

  procedure InsertFormedOrders(NewNum: integer; FormerOrders, NewOrders: TStrings);
  var i, j: integer;
  begin
    i := 0;
    while (i < FormerOrders.Count) and (FormerOrders[i] <> 'form ' +
      IntToStr(Abs(NewNum))) do Inc(i);
    if i < FormerOrders.Count then // Do not store orders if former cleared FORM
      for j := 0 to NewOrders.Count-1 do
        FormerOrders.Insert(i + j + 1, '  ' + NewOrders[j]);
  end;

begin
  // Save orders from VTurn into underlying turn
  NewFaction := Game.VirtTurn.Factions[1];
  for u := 0 to NewFaction.Units.Count-1 do begin
    NewUnit := NewFaction.Units[u];
    if NewUnit.Num >= 0 then begin
      OldUnit := FindRealUnit(NewUnit, Game.VirtTurn.Num);
      OldUnit.Orders.Assign(NewUnit.Orders);
    end
    else begin // "new 1" units: insert orders into formers' ones
      InsertFormedOrders(NewUnit.Num, FindRealUnit(NewUnit.Former,
        Game.VirtTurn.Num).Orders, NewUnit.Orders);
    end;
  end;
end;

// Get region from real turn
procedure TGame.RecreateVRegion(C: TCoords);
var OldRegion, NewRegion: TRegion;
begin
  CurrUnit := nil;
  SaveVTurnOrders;
  OldRegion := Map.Region(C, Turn.Num);
  NewRegion := Map.Region(C, 0);
  NewRegion.Assign(OldRegion, True);
end;

// Create mirror of given turn to change during order issuing
procedure TGame.CreateVirtualTurn;
begin
  CurrUnit := nil;
  CurrStruct := nil;
  CurrRegion := nil;
  // Free turn if exists
  if VirtTurn <> nil then begin
    SaveVTurnOrders;
    FreeAndNil(VirtTurn);
  end;
  VirtTurn := TTurn.Create(Turn.Num);
  VirtTurn.Assign(Turn);
  // Setup virtual aliases
  VTurn := VirtTurn;
  VFaction := VirtTurn.Factions[1];
  // Create map layer for virtual turn
  Map.CreateVirtualLayer;
end;

{ TTurn }

constructor TTurn.Create(ANum: integer);
begin
  Num := ANum;
  Factions := TFactionList.Create;
  Events := TStringList.Create;
  Regions := TRegionList.Create;
  // Faction[0] - unknown
  Factions.Add(TFaction.Create);
  Factions[0].Data := Game.FactionData.Seek(0);
  // Faction[1] - player
  Factions.Add(TFaction.Create);
  if ANum = 0 then Factions[1].Data := Factions[0].Data // history
  else Factions[1].Assign(History.Factions[1], False);
  Factions[1].Player := True;
end;

destructor TTurn.Destroy;
begin
  Factions.ClearAndFree;
  Events.Free;
  Regions.Free;
end;

// Used to create virtual turn
procedure TTurn.Assign(Source: TTurn);
begin
  Martial := Source.Martial;
  War := Source.War;
  Trade := Source.Trade;
  Magic := Source.Magic;
  GateCount := Source.GateCount;
  Unclaimed := Source.Unclaimed;
  Mana := Source.Mana;
  Events.Assign(Source.Events);
  Factions.AssignItems(Source.Factions, True);
  // Assign Regions in CreateVirtualLayer
end;

function TTurn.FindUnit(Num: integer): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Factions.Count) and (Result = nil) do begin
    Result := Factions[i].Units.Find(Num);
    Inc(i);
  end;
end;

{ TStructData }

constructor TStructData.Create(AGroup: string);
begin
  Group := AGroup;
end;

destructor TStructData.Destroy;
begin
  FreeAndNil(BuildSkill);
end;

{ TTerrainData }

constructor TTerrainData.Create(AName: string);
begin
  Name := AName;
  AdvResources := TItemDataList.Create;
end;

destructor TTerrainData.Destroy;
begin
  AdvResources.Free;
end;

{ TWeatherData }

constructor TWeatherData.Create(AText: string);
begin
  Text := AText;
end;

{ TSpecData }

constructor TSpecData.Create(Name: string);
begin
  Self.Name := Name;
  Effects := TEffectDataList.Create;
  Structs := TStructDataList.Create;
  Items := TItemDataList.Create;
end;


destructor TSpecData.Destroy;
begin
  Effects.Free;
  Structs.Free;
  Items.Free;
end;

{ TSkillData }

constructor TSkillData.Create(AShort: string);
var i: integer;
begin
  Short := AShort;
  BasedOn := TSkillList.Create;
  Descriptions := TStringList.Create;
  for i := 0 to 5 do Descriptions.Add(''); // lv 1-5, 0 is dummy
end;

destructor TSkillData.Destroy;
begin
  BasedOn.ClearAndFree;
  Descriptions.Free;
end;

function TSkillData.MakeName: string;
begin
  if Name <> '' then Result := Name
  else Result := Short;
end;

{ TStruct }

procedure TStruct.Assign(Source: TStruct);
begin
  Data := Source.Data;
  Name := Source.Name;
  Description := Source.Description;
  Num := Source.Num;
  Needs := Source.Needs;
  Passage := Source.Passage;
  Runes := Source.Runes;
  // Assign owner outside
end;

constructor TStruct.Create;
begin
  Passage.z := -1;
end;

function TStruct.HasExit: boolean;
begin
  Result := (Passage.z > -1);
end;

{ TBaseUnit }

constructor TBaseUnit.Create;
begin
  Items := TItemList.Create;
  Skills := TSkillList.Create;
end;

destructor TBaseUnit.Destroy;
begin
  Items.ClearAndFree;
  Skills.ClearAndFree;
end;

procedure TBaseUnit.Assign(Source: TBaseUnit);
begin
  Name := Source.Name;
  Description := Source.Description;
  Num := Source.Num;
  Mage := Source.Mage;
  Items.AssignItems(Source.Items);
  Skills.AssignItems(Source.Skills);
  CombatSpell := Source.CombatSpell;
  // Assign Faction and Struct outside
end;

function TBaseUnit.NumStr: string;
begin
  if Num >= 0 then Result := IntToStr(Num)
  else Result := 'new ' + IntToStr(-Num);
end;

 { TUnit Methods }

constructor TUnit.Create;
begin
  inherited Create;
  CanStudy := TSkillDataList.Create;
  Events := TStringList.Create;
  Orders := TStringList.Create;
  ReadyWeapons := TItemDataList.Create;
  ReadyArmor := TItemDataList.Create;
  FinalPoint := -1;
end;

destructor TUnit.Destroy;
begin
  inherited Destroy;
  CanStudy.Free;
  Events.Free;
  Orders.Free;
  ReadyWeapons.Free;
  ReadyArmor.Free;
end;

function TUnit.Id: string;
begin
  if Num >= 0 then Result := IntToStr(Num)
  else Result := CoordsToStr(Region.Coords) + ' ' + IntToStr(Region.Visited) +
    ' new ' + IntToStr(-Num);
end;

function TUnit.FullName: string;
begin
  Result := Name + ' (' + NumStr + ')';
end;

function TUnit.FinalCoords: TCoords;
var transported: boolean;
begin
  Result := Region.Coords;
  transported := (Struct <> nil) and (Struct.Owner <> nil)
    and (Struct.Owner <> Self) and (Struct.Owner.FinalPoint >= 0);
  if (FinalPoint = -1) or (FinalPoint >= Length(Moves)) then begin
    if transported then Result := Struct.Owner.FinalCoords;
  end
  else begin
    if transported and EqualCoords(Moves[FinalPoint], Region.Coords) then
      Result := Struct.Owner.FinalCoords
    else Result := Moves[FinalPoint];
  end;
end;

function TUnit.ArrivingTo(C: TCoords): boolean;
begin
  Result := False;
  if EqualCoords(Region.Coords, C) then Exit
  else Result := EqualCoords(FinalCoords, C);
end;

procedure TUnit.ChangeFaction(NewNum: integer);
var SrcFac, DestFac: TFaction;
    SrcTroop, DestTroop: TTroop;
    idx: integer;
begin
  if NewNum = Self.Faction.Num then Exit;
  // Faction units
  SrcFac := Self.Faction;
  DestFac := Turn.Factions.Seek(NewNum);
  DestFac.Units.Add(Self);
  SrcFac.Units.Delete(SrcFac.Units.IndexOf(Self));
  // Troop units
  SrcTroop := Region.Troops.Find(SrcFac.Num);
  DestTroop := Region.Troops.Seek(NewNum);
  DestTroop.Units.Add(Self);
  SrcTroop.Units.Delete(SrcTroop.Units.IndexOf(Self));
  if SrcTroop.Units.Count = 0 then begin
    idx := Region.Troops.IndexOf(SrcTroop);
    Region.Troops[idx].Free;
    Region.Troops.Delete(idx);
  end;
  // Set faction
  Self.Faction := DestFac;
end;

procedure TUnit.Assign(Source: TUnit);
begin
  inherited Assign(Source);
  Flags := Source.Flags;
  Consuming := Source.Consuming;
  Revealing := Source.Revealing;
  Spoils := Source.Spoils;
  Events.Assign(Source.Events);
  Orders.Assign(Source.Orders);
  ReadyWeapons.Assign(Source.ReadyWeapons);
  ReadyArmor.Assign(Source.ReadyArmor);
  CanStudy.Assign(Source.CanStudy);
  Marks := Source.Marks;
  UArmy := Source.UArmy;
  MonthInfo := Source.MonthInfo;
  MonthOrder := Source.MonthOrder;
  // Faction, Region, Struct must be redeclared outside
end;

// Return normal orders without @ and "@;tokens" as is
function TUnit.Order(Index: integer): string;
begin
  if Index >= Orders.Count then Result := ''
  else begin
    Result := LowerCase(TrimLeft(Orders[Index]));
    if not (Pos('@;', Result) = 1) then begin
      if Pos('@', Result) = 1 then Result := Copy(Result, 2, Length(Result));
      if Pos(';', Result) > 0 then Result := Copy(Result, 1, Pos(';', Result)-1);
    end;
    if Pos('@;;', Result) = 1 then Result := '@;;'
    else if Pos(' ', Result) > 0 then Result := Copy(Result, 1, Pos(' ', Result)-1);
  end;
end;


{ TBattleUnit }

procedure TBattleUnit.Assign(Source: TBattleUnit);
begin
  inherited Assign(Source);
  Behind := Source.Behind;
  Damaged := Source.Damaged;
  SpellText := Source.SpellText;
  URef := Source.URef;
  Struct := Source.Struct;
  Side := Source.Side;
  Viewer.Picture := Source.Viewer.Picture;
  //  Assign Faction outside
end;

 { TRegion Methods }

constructor TRegion.Create(X, Y, Z, TurnNum: integer);
begin
  Self.x := X;
  Self.y := Y;
  Self.z := Z;
  Self.Visited := TurnNum;
  Wanted := TItemList.Create;
  ForSale := TItemList.Create;
  Products := TItemList.Create;
  Troops := TTroopList.Create;
  Structs := TStructList.Create;
  Notes := TStringList.Create;
end;

destructor TRegion.Destroy;
begin
  Wanted.ClearAndFree;
  ForSale.ClearAndFree;
  Products.ClearAndFree;
  Structs.ClearAndFree;
  if Battles <> nil then Battles.ClearAndFree;
  Troops.ClearAndFree;
  Peasants.Free;
  Notes.Free;
end;

// Copy Units only if Source and Destination's turns equal!
procedure TRegion.Assign(Source: TRegion; CopyTurnData: boolean);
var i, j: integer;
    side: boolean;
    NewUnit: TUnit;
    Battle: TBattle;
    RegTurn: TTurn;
    Fac: TFaction;
    OldF, NewF: TTroop;
    NewStruct: TStruct;
begin
  if Source = Self then Exit;
  x := Source.x;
  y := Source.y;
  z := Source.z;
  FullData := Source.FullData;
  Visited := Source.Visited;
  // IsVirtual := ... will not be assigned
  for i := 1 to 6 do HasExit[i] := Source.HasExit[i];
  Terrain := Source.Terrain;
  Land := Source.Land;
  Settlement := Source.Settlement;
  SettlementType := Source.SettlementType;
  Marks := Source.Marks;
  FreeAndNil(Peasants);
  if Source.Peasants <> nil then begin
    Peasants := TItem.Create;
    Peasants.Assign(Source.Peasants);
  end;
  TaxRate := Source.TaxRate;
  Wages := Source.Wages;
  MaxWages := Source.MaxWages;
  Entertainment := Source.Entertainment;
  Gate := Source.Gate;
  Wanted.AssignItems(Source.Wanted);
  ForSale.AssignItems(Source.ForSale);
  Products.AssignItems(Source.Products);
  Notes.Assign(Source.Notes);
  Structs.AssignItems(Source.Structs, CopyTurnData);
  Activity := Source.Activity;

  Guard := nil;

  if CopyTurnData then begin
    if IsVirtual then RegTurn := Game.VirtTurn
    else RegTurn := Game.Turns.Find(Visited);

    WeatherLast := Source.WeatherLast;
    WeatherNext := Source.WeatherNext;

    // Battles
    if Battles <> nil then begin
      Battles.ClearAndFree;
      Battles := nil;
    end;
    if Source.Battles <> nil then begin
      Battles := TBattleList.Create;
      for i := 0 to Source.Battles.Count-1 do begin
        Battle := TBattle.Create('');
        Battle.Assign(Source.Battles[i]);
        Battle.Region := Self;
        Battles.Add(Battle);
        for side := sideAttack to sideDefence do begin
          for j := 0 to Battle.Units[side].Count-1 do
            if Source.Battles[i].Units[side][j].Faction <> nil then
              Battle.Units[side][j].Faction :=
                RegTurn.Factions.Find(Source.Battles[i].Units[side][j].Faction.Num);
        end;
      end;
    end;

    // Remove region's units from this turn's factions
    for i := 0 to Troops.Count-1 do
      for j := 0 to Troops[i].Units.Count-1 do begin
        Fac := RegTurn.Factions.Find(Troops[i].Faction.Num);
        Fac.Units.Delete(Fac.Units.IndexOf(Troops[i].Units[j]))
      end;
    Troops.ClearAndFree;
    Troops := TTroopList.Create;
    // Create new units
    for i := 0 to Source.Troops.Count-1 do begin
      OldF := Source.Troops[i];
      NewF := TTroop.Create;
      NewF.Faction := RegTurn.Factions.Find(OldF.Faction.Num);
      // Set region and struct for this faction's units
      for j := 0 to OldF.Units.Count-1 do begin
        NewUnit := NewF.Faction.Units.Find(OldF.Units[j].Num);
        if NewUnit = nil then begin
          // Recreate units deleted above
          NewUnit := TUnit.Create;
          NewF.Faction.Units.Add(NewUnit);
        end;
        NewUnit.Assign(OldF.Units[j]);
        NewUnit.Region := Self;
        NewUnit.Faction := NewF.Faction;
        NewF.Units.Add(NewUnit);
        if OldF.Units[j].Struct <> nil then begin
          NewStruct := Structs.Find(OldF.Units[j].Struct.Num);
          NewUnit.Struct := NewStruct;
          if (NewStruct <> nil) and (NewStruct.Owner = nil) then NewStruct.Owner := NewUnit;
        end;
        // If unit is on guard, set region's flag
        if OldF.Units[j].Flags[0] then Guard := NewF.Faction;
      end;
      Troops.Add(NewF);
    end;
  end;
end;

function TRegion.GetCoords: TCoords;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

procedure TRegion.SetCoords(const Value: TCoords);
begin
  x := Value.x;
  y := Value.y;
  z := Value.z;
end;

function TRegion.PlayerTroop: TTroop;
begin
  if (Troops.Count = 0) or not Troops[0].Faction.Player then Result := nil
  else Result := Troops[0];
end;

function TRegion.FindUnit(Num: integer): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Troops.Count) and (Result = nil) do begin
    Result := Troops[i].Units.Find(Num);
    Inc(i);
  end;
end;

 { TFaction Methods }

procedure TFaction.Assign(Source: TFaction; CopyUnits: boolean);
var i: integer;
    NewUnit: TUnit;
begin
  Data := Source.Data;
  Name := Source.Name;
  Num := Source.Num;
  Player := Source.Player;
  Attitude := Source.Attitude;
  Units.ClearAndFree;
  Units := TUnitList.Create;
  if CopyUnits then begin
    for i := 0 to Source.Units.Count-1 do begin
      NewUnit := TUnit.Create;
      NewUnit.Assign(Source.Units[i]);
      NewUnit.Faction := Self;
      // Region and Struct will be assigned later in Region.Assign
      Units.Add(NewUnit);
    end;
  end;
end;

constructor TFaction.Create;
begin
  Units := TUnitList.Create;
end;

destructor TFaction.Destroy;
begin
  Units.ClearAndFree;
end;

constructor TFactionData.Create(Num: integer);
begin
  Self.Num := Num;
end;

 { TTroop }

constructor TTroop.Create;
begin
  Units := TUnitList.Create;
end;

destructor TTroop.Destroy;
begin
  Units.Free;
end;

{ TItemData }

constructor TItemData.Create(AShort: string);
begin
  Short := AShort;
  Produce.Materials := TItemList.Create;
  Produce.Byproducts := TItemList.Create;
  MagProduce.Materials := TItemList.Create;
  Man.SpecSkills := TSkillList.Create;
end;

destructor TItemData.Destroy;
begin
  FreeAndNil(Produce.Skill);
  Produce.Materials.ClearAndFree;
  Produce.Byproducts.ClearAndFree;
  FreeAndNil(MagProduce.Skill);
  MagProduce.Materials.ClearAndFree;
  Man.SpecSkills.ClearAndFree;
end;

function TItemData.Name(Many: boolean): string;
begin
  if Many and (MultiName <> '') then Result := MultiName
  else if SingleName <> '' then Result := SingleName
  else if not Many and (SingleName = '') then Result := MultiName
  else Result := Short;
end;

function TItemData.Name: string;
begin
  Result := Name(True);
end;

{ TMap }

constructor TMap.Create;
begin
  Levels := TMapLevelList.Create;
end;

destructor TMap.Destroy;
begin
  Levels.ClearAndFree;
end;

function TMap.Region(X, Y: integer): TRegion;
begin
  Result := Region(X, Y, Level, TurnNum);
end;

function TMap.Region(X, Y, Z: integer): TRegion;
begin
  Result := Region(X, Y, Z, TurnNum);
end;

function TMap.Region(C: TCoords): TRegion;
begin
  Result := Region(C.x, C.y, C.z, TurnNum);
end;

function TMap.Region(C: TCoords; T: integer): TRegion;
begin
  Result := Region(C.x, C.y, C.z, T);
end;

function TMap.Region(X, Y, Z, T: integer): TRegion;
var left, right, i: integer;
    R: TRegion;
    List: TRegionList;

  function FindRegion(List: TRegionList; turn: integer): TRegion;
  var j: integer;
  begin
    j := 0;
    while (j < List.Count) and ((List[j].Visited <= turn)
      or List[j].IsVirtual) do Inc(j);
    if j > 0 then Result := List[j-1]
    else Result := nil;
  end;

begin
  Result := nil;
  if (Z < Levels.Count) and not Levels[Z].Empty then
  with Levels[Z] do begin
    // Nexus starting cities
    if (T = 0) and (Name = 'nexus') then begin
      if (X = 0) and (Y = 0) then begin end
      else if (X = 0) and (Y = -2) then begin X := dirN; Y := 0; end
      else if (X = 1) and (Y = -1) then begin X := dirNE; Y := 0; end
      else if (X = 1) and (Y = 1)  then begin X := dirSE; Y := 0; end
      else if (X = 0) and (Y = 2)  then begin X := dirS; Y := 0; end
      else if (X = -1) and (Y = 1) then begin X := dirSW; Y := 0; end
      else if (X = -1) and (Y = -1) then begin X := dirNW; Y := 0; end
      else begin X := -1; Y := -1; end;
    end;

    // Normal regions
    if (X in [Bounds.Left .. Bounds.Right]) and (Y in [Bounds.Top .. Bounds.Bottom]) then begin
      // Find region in collections (binar method until max 3 regions)
      right := RCols.Count-1;
      left := 0;
      while right - left > 1 do begin
        i := left + (right - left) div 2;
        R := TRegionList(RCols[i])[0];
        if (R.x < X) or ((R.x = X) and (R.y < Y)) then
          left := i
        else if (R.x > X) or ((R.x = X) and (R.y > Y)) then
          right := i
        else begin
          right := i; left := i;
          Break;
        end
      end;
      // Get region from found 3
      for i := left to right do begin
        R := TRegionList(RCols[i])[0];
        if (R.x = X) and (R.y = Y) then begin
          List := TRegionList(RCols[i]);
          if T = 0 then begin
            if TRegion(List[0]).IsVirtual then Result := List[0]
            else Result := FindRegion(List, Turn.Num);
          end
          else Result := FindRegion(List, T);
        end;
      end;
    end;
  end;
end;

function TMap.SeekRegion(C: TCoords): TRegion;
begin
  Result := SeekRegion(C.x, C.y, C.z);
end;

function TMap.SeekRegion(X, Y, Z: integer): TRegion;
var i, j, left, right: integer;
    R: TRegion;
    List: TRegionList;
begin
  with Levels[Z] do begin
    // Expand Level's rect
    if Empty then begin
      Bounds := Rect(X, Y, X, Y);
      Empty := False;
    end
    else begin
      if X < Bounds.Left then Bounds.Left := X;
      if X > Bounds.Right then Bounds.Right := X;
      if Y < Bounds.Top then Bounds.Top := Y;
      if Y > Bounds.Bottom then Bounds.Bottom := Y;
    end;
    // Find region in collections (binar method until max 3 regions)
    right := RCols.Count-1;
    left := 0;
    while right - left > 1 do begin
      i := left + (right - left) div 2;
      R := TRegionList(RCols[i])[0];
      if (R.x < X) or ((R.x = X) and (R.y < Y)) then
        left := i
      else if (R.x > X) or ((R.x = X) and (R.y > Y)) then
        right := i
      else begin
        right := i; left := i;
        Break;
      end
    end;
    // Get region from found 3
    R := nil;
    i := left;
    while i <= right do begin
      R := TRegionList(RCols[i])[0];
      if (X > R.x) or ((X = R.x) and (Y > R.y)) then Inc(i)
      else Break; // Found needed or next region
    end;
    j := 0;
    if (R = nil) or (R.x <> X) or (R.y <> Y) then begin
      // Add new collection
      RCols.Insert(i, TList.Create);
      TRegionList(RCols[i]).Add(TRegion.Create(X, Y, Z, TurnNum));
    end
    else begin
      List := TRegionList(RCols[i]);
      // Find turn region in collection
      if (TurnNum = 0) and not List[0].IsVirtual then begin
        List.Insert(0, TRegion.Create(X, Y, Z, TurnNum));
        List[0].IsVirtual := True;
      end
      else begin
        if (TurnNum > 0) and List[0].IsVirtual then Inc(j);
        while (j < List.Count) and (List[j].Visited < TurnNum) do Inc(j);
        if (j = List.Count) or (List[j].Visited > TurnNum) then
          List.Insert(j, TRegion.Create(X, Y, Z, TurnNum));
      end;
    end;
    Result := TList(RCols[i])[j];
  end;
end;

// Flatten map data from current turn to one layer (turn 0)
procedure TMap.CreateVirtualLayer;
var i: integer;
    VRegion: TRegion;
begin
  TurnNum := 0;
  FreeVirtualLayer;
  for i := 0 to Turn.Regions.Count-1 do begin
    VRegion := SeekRegion(Turn.Regions[i].Coords);
    VRegion.Assign(Turn.Regions[i], True);
    VTurn.Regions.Add(VRegion);
  end;
end;

procedure TMap.FreeVirtualLayer;
var z, i: integer;
    List: TRegionList;
begin
  VTurn.Regions.Clear;
  for z := 0 to Levels.Count-1 do
    for i := 0 to Levels[z].RCols.Count-1 do begin
      List := TRegionList(Levels[z].RCols[i]);
      if List[0].IsVirtual then begin
        List[0].Free;
        List.Delete(0);
      end;
    end;
end;

{ TMapLevelList }

function TMapLevelList.Get(Index: integer): TMapLevel;
begin
  Result := TMapLevel(inherited Get(Index));
end;

procedure TMapLevelList.Put(Index: integer; Item: TMapLevel);
begin
  inherited Put(Index, Item);
end;

function TMapLevelList.NumOf(AName: string): integer;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Name <> AName) do Inc(i);
  if i < Count then Result := i
  else Result := -1;
end;

function TMapLevelList.Seek(AName: string): TMapLevel;
var i: integer;
begin
  if (StringReplace(AName, ' ', '', []) = 'underworld') then
    AName := 'underworld';
  i := 0;
  while (i < Count) and (Items[i].Name <> AName) do Inc(i);
  if i = Count then begin
    Result := TMapLevel.Create(AName);
    Add(Result);
  end
  else Result := Items[i];
end;

procedure TMapLevelList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

{ TMapLevel }

constructor TMapLevel.Create(Name: string);
begin
  Empty := True;
  Self.Name := Name;
  RCols := TList.Create;
  MaxPoint := Point(9999, 9999);
end;

destructor TMapLevel.Destroy;
var i, j: integer;
begin
  for i := 0 to RCols.Count - 1 do begin
    for j := 0 to TRegionList(RCols[i]).Count - 1 do
      TRegionList(RCols[i])[j].Free;
    TRegionList(RCols[i]).Free;
  end;
  RCols.Free;
end;

{ TBattle }

procedure TBattle.Assign(Source: TBattle);
var side: boolean;
    i, j: integer;
begin
  Name := Source.Name;
  Report.Assign(Source.Report);
  Spoils.AssignItems(Source.Spoils);
  for side := sideAttack to sideDefence do begin
    Loses[side] := Source.Loses[side];
    Units[side].AssignItems(Source.Units[side]);
  end;
  SetLength(Rounds, Length(Source.Rounds));
  for i := 0 to High(Source.Rounds) do begin
    Rounds[i].Num := Source.Rounds[i].Num;
    SetLength(Rounds[i].Actions, Length(Source.Rounds[i].Actions));
    for j := 0 to High(Source.Rounds[i].Actions) do begin
      Rounds[i].Actions[j] := Source.Rounds[i].Actions[j];
      Rounds[i].Actions[j].BUnit := FindBUnit(Source.Rounds[i].Actions[j].BUnit.Num);
    end;
  end;
end;

constructor TBattle.Create(Name: string);
begin
  Self.Name := Name;
  Report := TStringList.Create;
  Units[sideAttack] := TBattleUnitList.Create;
  Units[sideDefence] := TBattleUnitList.Create;
  Spoils := TItemList.Create;
end;

destructor TBattle.Destroy;
begin
  Report.Free;
  Units[sideAttack].ClearAndFree;
  Units[sideDefence].ClearAndFree;
  Spoils.ClearAndFree;
end;

function TBattle.FindBUnit(Num: integer): TBattleUnit;
begin
  Result := Units[sideAttack].Find(Num);
  if Result = nil then Result := Units[sideDefence].Find(Num);
end;

{ TItem }

procedure TItem.Assign(Source: TItem);
begin
  Data := Source.Data;
  Amount := Source.Amount;
  Cost := Source.Cost;
  Bought := Source.Bought;
end;

function TItem.Name: string;
begin
  Result := Data.Name(Amount <> 1);
end;

{ TSkill }

procedure TSkill.Assign(Source: TSkill);
begin
  Data := Source.Data;
  Level := Source.Level;
  Points := Source.Points;
end;

function TSkill.FullName: string;
begin
  Result := Data.MakeName + ', lv ' + IntToStr(Level);
end;

{ TUArmy }

constructor TUArmy.Create(Name: string);
begin
  Self.Name := Name;
  UnitIds := TStringList.Create;
  Color := -1;
end;

destructor TUArmy.Destroy;
begin
  UnitIds.Free;
end;
 { Type lists }

procedure TFactionList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

procedure TFactionList.AssignItems(Source: TFactionList; CopyUnits: boolean);
var i: integer;
    Faction: TFaction;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
  for i := 0 to Source.Count-1 do begin
    Faction := TFaction.Create;
    Faction.Assign(Source[i], CopyUnits);
    Add(Faction);
  end;
end;

function TFactionList.Get(Index: integer): TFaction;
begin
  Result := TFaction(inherited Get(Index));
end;

procedure TFactionList.Put(Index: integer; Item: TFaction);
begin
  inherited Put(Index, Item);
end;

function TFactionList.Find(Num: integer): TFaction;
var i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if Items[i].Num = Num then begin
      Result := Items[i];
      break;
    end;
end;

function TFactionList.Seek(Num: integer): TFaction;
begin
  Result := Find(Num);
  if Result = nil then begin
    Result := TFaction.Create;
    Result.Num := Num;
    Result.Data := Game.FactionData.Seek(Num);
    Add(Result);
  end;
end;

function TFactionList.SeekByName(Name: string): TFaction;
var i, maxnum: integer;
begin
  Result := nil;
  maxnum := 0;
  for i := 0 to Count-1 do begin
    if Items[i].Name = Name then begin
      Result := Items[i];
      break;
    end;
    if maxnum < Items[i].Num then maxnum := Items[i].Num;
  end;
  if Result = nil then begin
    Result := TFaction.Create;
    Result.Num := maxnum + 1;
    Result.Data := Game.FactionData.Seek(Result.Num);
    Add(Result);
  end;
end;

procedure TFactionDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TFactionDataList.Get(Index: integer): TFactionData;
begin
  Result := TFactionData(inherited Get(Index));
end;

procedure TFactionDataList.Put(Index: integer; Item: TFactionData);
begin
  inherited Put(Index, Item);
end;

function TFactionDataList.Seek(Num: integer): TFactionData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Num <> Num) do Inc(i);
  if i < Count then Result := Items[i];
  if Result = nil then begin
    Result := TFactionData.Create(Num);
    Add(Result);
  end;
end;

procedure TTroopList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TTroopList.Get(Index: integer): TTroop;
begin
  Result := TTroop(inherited Get(Index));
end;

procedure TTroopList.Put(Index: integer; Item: TTroop);
begin
  inherited Put(Index, Item);
end;

function TTroopList.Find(Num: integer): TTroop;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Faction.Num <> Num) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TTroopList.Seek(Num: integer): TTroop;
begin
  Result := Find(Num);
  if Result = nil then begin
    Result := TTroop.Create;
    Result.Faction := Turn.Factions.Seek(Num);
    Add(Result);
  end;
end;

function TItemList.Amount(Mask: DWord): integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if Test(Items[i].Data.Flags, Mask) then
      Result := Result + Items[i].Amount;
end;

procedure TItemList.AssignItems(Source: TItemList);
var i: integer;
    Item: TItem;
begin
  ClearItems;
  for i := 0 to Source.Count-1 do begin
    Item := TItem.Create;
    Item.Assign(Source[i]);
    Add(Item);
  end;
end;

procedure TItemList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TItemList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TItemList.Find(Short: string): TItem;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Data.Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TItemList.Seek(Short: string): TItem;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TItem.Create;
    Result.Data := Game.ItemData.Seek(Short);
    Add(Result);
  end;
end;

function TItemList.Get(Index: integer): TItem;
begin
  Result := TItem(inherited Get(Index));
end;

procedure TItemList.Put(Index: integer; Item: TItem);
begin
  inherited Put(Index, Item);
end;

procedure TItemDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TItemDataList.Get(Index: integer): TItemData;
begin
  Result := TItemData(inherited Get(Index));
end;

procedure TItemDataList.Put(Index: integer; Item: TItemData);
begin
  inherited Put(Index, Item);
end;

function TItemDataList.FindByName(AName: string): TItemData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].SingleName <> LowerCase(AName)) and
    (Items[i].MultiName <> LowerCase(AName)) and (Items[i].Short <>
    UpperCase(AName)) and (Items[i].SingleName + 's' <> LowerCase(AName))
    and (Items[i].MultiName <> LowerCase(AName) + 's') do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TItemDataList.Find(Short: string): TItemData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TItemDataList.Seek(Short: string): TItemData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TItemData.Create(Short);
    Add(Result);
  end;
end;

procedure TStructList.AssignItems(Source: TStructList; CopyTurnData: boolean);
var i: integer;
    Struct: TStruct;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
  for i := 0 to Source.Count-1 do
    if CopyTurnData or not Test(Source[i].Data.Flags, ST_TRANSPORT) then begin
      Struct := TStruct.Create;
      Struct.Assign(Source[i]);
      Add(Struct);
    end;
end;

procedure TStructList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TStructList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

function TStructList.Find(Num: integer): TStruct;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Num <> Num) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TStructList.Get(Index: integer): TStruct;
begin
  Result := TStruct(inherited Get(Index));
end;

procedure TStructList.Put(Index: integer; Item: TStruct);
begin
  inherited Put(Index, Item);
end;

procedure TStructDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TStructDataList.Get(Index: integer): TStructData;
begin
  Result := TStructData(inherited Get(Index));
end;

procedure TStructDataList.Put(Index: integer; Item: TStructData);
begin
  inherited Put(Index, Item);
end;

function TStructDataList.Find(Group: string): TStructData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Group <> Group) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TStructDataList.Seek(Group: string): TStructData;
begin
  Result := Find(Group);
  if Result = nil then begin
    Result := TStructData.Create(Group);
    Add(Result);
  end;
end;

procedure TSkillList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

procedure TSkillList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

procedure TSkillList.AssignItems(Source: TSkillList);
var i: integer;
    Skill: TSkill;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
  for i := 0 to Source.Count-1 do begin
    Skill := TSkill.Create;
    Skill.Assign(Source[i]);
    Add(Skill);
  end;
end;

function TSkillList.Find(Short: string): TSkill;
var i: integer;
begin
  i := Count-1;
  while (i >= 0) and (Items[i].Data.Short <> Short) do Dec(i);
  if i >= 0 then Result := Items[i]
  else Result := nil;
end;

function TSkillList.Seek(Short: string): TSkill;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TSkill.Create;
    Result.Data := Game.SkillData.Seek(Short);
    Add(Result);
  end;
end;

function TSkillList.Get(Index: integer): TSkill;
begin
  Result := TSkill(inherited Get(Index));
end;

procedure TSkillList.Put(Index: integer; Item: TSkill);
begin
  inherited Put(Index, Item);
end;

function TSkillDataList.Get(Index: integer): TSkillData;
begin
  Result := TSkillData(inherited Get(Index));
end;

procedure TSkillDataList.Put(Index: integer; Item: TSkillData);
begin
  inherited Put(Index, Item);
end;

function TSkillDataList.Find(Short: string): TSkillData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Short <> Short) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TSkillDataList.FindByName(Name: string): TSkillData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Short <> UpperCase(Name))
    and (Items[i].Name <> LowerCase(Name)) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TSkillDataList.Seek(Short: string): TSkillData;
begin
  Result := Find(Short);
  if Result = nil then begin
    Result := TSkillData.Create(Short);
    Add(Result);
  end;
end;

procedure TSkillDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

procedure TTerrainDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TTerrainDataList.Get(Index: integer): TTerrainData;
begin
  Result := TTerrainData(inherited Get(Index));
end;

procedure TTerrainDataList.Put(Index: integer; Item: TTerrainData);
begin
  inherited Put(Index, Item);
end;

function TTerrainDataList.Find(Name: string): TTerrainData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Name <> Name) do Inc(i);
  if i < Count then Result := Items[i]
  else Result := nil;
end;

function TTerrainDataList.Seek(Name: string): TTerrainData;
begin
  Result := Find(Name);
  if Result = nil then begin
    Result := TTerrainData.Create(Name);
    Add(Result);
  end;
end;

procedure TTurnList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TTurnList.Get(Index: integer): TTurn;
begin
  Result := TTurn(inherited Get(Index));
end;

procedure TTurnList.Put(Index: integer; Item: TTurn);
begin
  inherited Put(Index, Item);
end;

function TTurnList.Find(Num: integer): TTurn;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Num < Num) do Inc(i);
  if (i < Count) and (Items[i].Num = Num) then Result := Items[i]
  else Result := nil;
end;

function TTurnList.Seek(Num: integer): TTurn;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Num < Num) do Inc(i);
  if (i = Count) or (Items[i].Num <> Num) then begin
    Result := TTurn.Create(Num);
    Insert(i, Result);
  end
  else Result := Items[i];
  Game.TurnIndex := i;
end;

procedure TUnitList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TUnitList.Find(ANum: integer): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Num <> ANum) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TUnitList.FindById(Id: string): TUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Id <> Id) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TUnitList.Get(Index: integer): TUnit;
begin
  Result := TUnit(inherited Get(Index));
end;

procedure TUnitList.Put(Index: integer; Item: TUnit);
begin
  inherited Put(Index, Item);
end;

{ TRegionList }

function TRegionList.Get(Index: Integer): TRegion;
begin
  Result := TRegion(inherited Get(Index));
end;

procedure TRegionList.Put(Index: Integer; Item: TRegion);
begin
  inherited Put(Index, Item);
end;

procedure TWeatherDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TWeatherDataList.Get(Index: integer): TWeatherData;
begin
  Result := TWeatherData(inherited Get(Index));
end;

procedure TWeatherDataList.Put(Index: integer; Item: TWeatherData);
begin
  inherited Put(Index, Item);
end;

function TWeatherDataList.Seek(Text: string): TWeatherData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Text <> Text) do Inc(i);
  if i < Count then Result := Items[i];
  if Result = nil then begin
    Result := TWeatherData.Create(Text);
    Add(Result);
  end;
end;

{ Lists }

procedure TBattleUnitList.AssignItems(Source: TBattleUnitList);
var i: integer;
    Item: TBattleUnit;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
  for i := 0 to Source.Count-1 do begin
    Item := TBattleUnit.Create;
    Item.Assign(Source[i]);
    Add(Item);
  end;
end;

procedure TBattleUnitList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

procedure TBattleUnitList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

function TBattleUnitList.Get(Index: integer): TBattleUnit;
begin
  Result := TBattleUnit(inherited Get(Index));
end;

procedure TBattleUnitList.Put(Index: integer; Item: TBattleUnit);
begin
  inherited Put(Index, Item);
end;

procedure TBattleList.ClearItems;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Clear;
end;

procedure TBattleList.ClearAndFree;
begin
  ClearItems;
  Free;
end;

function TBattleList.Get(Index: integer): TBattle;
begin
  Result := TBattle(inherited Get(Index));
end;

procedure TBattleList.Put(Index: integer; Item: TBattle);
begin
  inherited Put(Index, Item);
end;

function TBattleUnitList.Find(Num: integer): TBattleUnit;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].Num <> Num) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TBattleUnitList.Seek(Num: integer): TBattleUnit;
begin
  Result := Find(num);
  if Result = nil then begin
    Result := TBattleUnit.Create;
    Result.Num := Num;
    Add(Result);
  end;
end;

{ TSpecDataList }

procedure TSpecDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TSpecDataList.Get(Index: Integer): TSpecData;
begin
  Result := TSpecData(inherited Get(Index));
end;

procedure TSpecDataList.Put(Index: Integer; Item: TSpecData);
begin
  inherited Put(Index, Item);
end;

function TSpecDataList.FindByText(Text: string): TSpecData;
var i: integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Items[i].SpellText <> Text) do Inc(i);
  if i < Count then Result := Items[i];
end;

function TSpecDataList.Seek(Name: string): TSpecData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Name <> Name) do Inc(i);
  if i < Count then Result := Items[i]
  else begin
    Result := TSpecData.Create(Name);
    Add(Result);
  end;
end;

{ TEffectDataList }

procedure TEffectDataList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TEffectDataList.Get(Index: Integer): TEffectData;
begin
  Result := TEffectData(inherited Get(Index));
end;

procedure TEffectDataList.Put(Index: Integer; Item: TEffectData);
begin
  inherited Put(Index, Item);
end;

function TEffectDataList.Seek(Name: string): TEffectData;
var i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].Name <> Name) do Inc(i);
  if i < Count then Result := Items[i]
  else begin
    Result := TEffectData.Create;
    Result.Name := Name;
    Add(Result);
  end;
end;


{ TArmyList }

procedure TUArmyList.ClearAndFree;
var i: integer;
begin
  for i := 0 to Count-1 do Items[i].Free;
  Free;
end;

function TUArmyList.Get(Index: Integer): TUArmy;
begin
  Result := TUArmy(inherited Get(Index));
end;

procedure TUArmyList.Put(Index: Integer; Item: TUArmy);
begin
  inherited Put(Index, Item);
end;




initialization
  SimRegion := TRegion.Create(0, 0, 0, 1);
  SimBattle := TBattle.Create('Simulation');
  SimBattle.Region := SimRegion;

finalization
  SimBattle.Free;
  SimRegion.Free;

end.

