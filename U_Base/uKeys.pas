unit uKeys;

interface

uses
  SysUtils, Classes, Dialogs;

const
  KeyfileVersion = 'Keyfile v3.19.2';

type
  EInvalidKeyfile = class(Exception);

  TKeyStrings = (

  s_KeyfileVersion,

  // Arrays
  s_North,
    s_NorthEast, s_SouthEast, s_South, s_SouthWest, s_NorthWest,
  s_N,
    s_NE, s_SE, s_S, s_SW, s_NW,
  s_January,
    s_February, s_March, s_April, s_Mayy, s_June, s_July, s_August,
    s_September, s_October, s_November, s_December,
  s_Jan,
    s_Feb, s_Mar, s_Apr, s_May, s_Jun, s_Jul, s_Aug, s_Sep, s_Oct,
    s_Nov, s_Dec,
  s_Hostile,
    s_Unfriendly, s_Neutral, s_Friendly, s_Ally,
  s_Village,
    s_Town, s_City,
  s_Walking,
    s_Riding, s_Flying, s_Swimming,
  s_flgOnGuard,
    s_flgTaxing, s_flgAvoiding, s_flgBehind, s_flgHolding, s_flgNoaid,
    s_flgNocross,
  s_flagSharing,
  s_RevealUnit,
    s_RevealFaction,
  s_ConsumeUnit,
    s_ConsumeFaction,
  s_SpoilWeightless,
    s_SpoilWalking, s_SpoilRiding, s_SpoilFlying,
  s_wcSlashing,
    s_wcPiercing, s_wcCrushing, s_wcCleaving, s_wcArmorPiercing,
    s_wcEnergy, s_wcSpirit, s_wcWeather,
  s_atMelee,
    s_atEnergy, s_atSpirit, s_atWeather, s_atRiding, s_atRanged,
    s_atNonResistable,
  s_VerySusceptible,
    s_Susceptible, s_TypicallyResistant, s_SlightlyResistant,
    s_VeryResistant,

  // List tags
  s_and,
  s_or,
  s_none,
  s_anyof,

  // Report header
  s_Header,
  s_Martial,
  s_War,
  s_Trade,
  s_Magic,
  s_Year,

  // Faction status
  s_Status,
  s_TaxRegions,
  s_TradeRegions,
  s_Mages,
  s_Apprentices,
  s_FishRegions,
  s_RoadRegions,

  // Battles
  s_Battles,
  s_Attacks,
  s_Assassinated,
  s_Spoils,
  s_NoSpoils,
  s_BTactics,
  s_BCombat,
  s_BRiding,
  s_MonCombat,
  s_MonAttacks,
  s_MonHits,
  s_MonTactics,
  s_Destroyed,
  s_Routed,
  s_Casualities,
  s_Loses,
  s_DamagedUnits,
  s_FreeRound,
  s_Round,
  s_Takes,
  s_Heals,
  s_BattleIndecisively,

  // Errors
  s_Errors,
  s_Starve,

  // Events
  s_Events,
  s_AddressOf,
  s_Is,
  s_Uses,
  s_Forbids,
  s_Earns,
  s_Entertaining,
  s_Working,
  s_Collects,
  s_Taxing,
  s_Produces,
  s_Enters,

  // Skill reports
  s_SkillReps,
  s_FoundationSkill,
  s_SkillRequire,
  s_SkillCost,
  s_UseInCombat,
  s_UseCast,

  // Item reports
  s_ItemReps,
  s_weight,
  s_capacity,
  s_IsACreature,
  s_IsAMan,
  s_CanWalk,
  s_CanFly,
  s_CanRide,
  s_itmGood,
  s_itmEvil,
  // men
  s_RaceMayStudy,
  s_AllSkills,
  s_Level,
  s_MultipleSkills,
  s_OtherMagic,
  // production
  s_UnitsWith,
  s_ProduceFrom,
  s_ProduceRate,
  s_ProducePer,
  s_CreateMagic,
  s_MagProdCost,
  s_ByProducts,
  s_StructToProduce,
  s_StructIsRequired,
  s_ProdIncBy,
  s_ProdIncUsing,
  // mount
  s_MountRequires,
  s_MinBonus,
  s_MaxBonus,
  s_HamperedBonus,
  // tool
  s_IncProduction,
  s_ProdEntertainment,
  s_IncBuilding,
  s_By,
  // weapon
  s_ThisIsA,
  s_Long,
  s_Short,
  s_Ranged,
  s_Weapon,
  s_KnowledgeOf,
  s_WpnGrants,
  s_wpnBonus,
  s_wpnPenalty,
  s_Attack,
  s_Defense,
  s_AgainstMounted,
  s_NoFoot,
  s_NoMount,
  s_RidingBonus,
  s_NoAttDefense,
  s_WpnAllows,
  s_WpnNumHalf,
  s_WpnNumSkill,
  s_Every,
  s_AttackVersus,
  // armor
  s_ArmorAss, // :)
  s_ArmorProtect,
  s_Versus,
  // monster
  s_MonsterSkill,
  s_MonResistanceOf,
  s_MonHas,
  s_MonTakes,
  s_MonRegen,
  s_MonTacticsRep,
  s_MonStealth,
  s_MonObservation,
  // misc
  s_TradeGood,
  s_CantGive,
  s_MageOnly,
  s_CombatItem,
  s_WhenHitched,

  // Special Ability reports
  s_MonsterCanCast,
  s_ItemCanCast,
  s_MageCanCast,
  s_MountCauses,
  s_InBattle,
  s_AtSkillLevel,
  s_ShieldAbility,
  s_AbilityDoes,
  s_MonsterIs,
  s_TargetAffected,
  s_EffToAttack,
  s_EffVersus,
  s_ForNextAttack,
  s_ANotTarget,
  s_AOnlyTarget,
  s_ATargetStructs,
  s_ATargetMounted,
  s_ATargetAffected,
  s_ATargetIllusions,
  s_ANoMonsters,
  s_ANoBuilding,
  s_AbilityProvides,
  s_DefBonus,
  s_BonusVersus,
  s_TimesMageLev,

  // Object reports
  s_ObjReps,
  s_ShipRequire,
  s_StructBuilt,
  s_Requires,
  s_DefenseAmt,
  s_IncAmount,
  s_AvaiInRegion,
  s_StructDefence,
  s_StructDefenceAgainst,

  // Attitudes
  s_DeclaredAttitudes,

  // Unclaimed silver
  s_Unclaimed,
  s_FactionMana,

  // Unit
  s_Skills,
  s_CombatSpell,
  s_CanStudy,
  s_ReadyWeapon,
  s_ReadyArmor,
  s_Faction,

  // Struct
  s_Needs,
  s_InnerLocation,
  s_ClosedToPlayer,
  s_RunesOfWarding,
  s_Road,

  // Region
  s_Unlimited,
  s_At,
  s_In,
  s_From,
  s_To,
  s_Contains,
  s_Peasants,
  s_Was,
  s_LastMonth,
  s_WillBe,
  s_NextMonth,
  s_Wages,
  s_MaxWages,
  s_Wanted,
  s_ForSale,
  s_Entertainment,
  s_Products,
  s_Exits,
  s_GateHere,
  s_Of,
  s_NexusGate,

  // Orders Template
  s_Orders,
  s_OrderStart,
  s_Unit,
  s_OrderEnd,

  // Advisor constants
  s_DefaultPeasants,
  s_Nexus,
  s_Surface,
  s_Combat,
  s_Sailing,
  s_Stealth,
  s_EntertainSkill,
  s_Tactics,
  s_Healing,
  s_MagicalHealing,
  s_RidingSkill,
  s_HealingPotions,
  s_Herbs,
  s_TradeIncome,
  s_WorkIncome,

  s_LastKey
  );

var
  Keys: array[s_KeyfileVersion .. s_LastKey] of string;

  procedure LoadKeys(Filename: string);
  function GetKey(Index: TKeyStrings; Shift: integer): string;
  function GetDir(Index: integer): string;
  function KeyIndex(s: string; base: TKeyStrings; count: integer): integer;

implementation

procedure LoadKeys(Filename: string);
var i, p: integer;
    Lines: TStrings;
    key: TKeyStrings;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(Filename);

  if Lines.Count <> Length(Keys) then
    raise EInvalidKeyfile.Create('Incorrect amount of lines');

  if Lines[Integer(s_KeyfileVersion)] <> KeyfileVersion then
    raise EInvalidKeyfile.Create('Old format');

  // Decode format
  for key := s_KeyfileVersion to s_LastKey do begin
    i := Integer(key);
    p := Pos('"', Lines[i]);
    if p = 1 then begin
      // Decode quoted values
      Lines[i] := Copy(Lines[i], 2, Length(Lines[i])-1);
      p := Pos('"', Lines[i]);
      Lines[i] := Copy(Lines[i], 1, p-1);
    end
    else begin
      // Strip comments, remove extra spaces
      p := Pos(';', Lines[i]);
      if p > 0 then Lines[i] := Trim(Copy(Lines[i], 1, p-1));
    end;
    Keys[key] := Lines[i];
  end;
  Lines.Free;
end;

function GetKey(Index: TKeyStrings; Shift: integer): string;
begin
  Result := Keys[TKeyStrings(Integer(Index) + Shift)];
end;

function GetDir(Index: integer): string;
begin
  if (Index >= 1) and (Index <= 6) then
    Result := Keys[TKeyStrings(Integer(s_N) + Index - 1)]
  else Result := '';
end;

function KeyIndex(s: string; base: TKeyStrings; count: integer): integer;
var i: integer;
begin
  i := Integer(base) + count - 1;
  while (i >= Integer(base)) and (Pos(Keys[TKeyStrings(i)], s) <> 1) do Dec(i);
  Result := i - Integer(base);
end;

end.

