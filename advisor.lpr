program advisor;

{$MODE Delphi}

{%File 'english.key'}

uses
  Forms,
  Interfaces,
  Main in 'Main.pas' {MainForm},
  uAbout in 'uAbout.pas' {AboutForm},
  Resources in 'Resources.pas' {ResForm},
  uOrderProcessor in 'U_Orders\uOrderProcessor.pas',
  uVisualOrders in 'U_Orders\uVisualOrders.pas',
  uGameSubs in 'U_Base\uGameSubs.pas',
  DataStructs in 'U_Base\DataStructs.pas',
  uHexMap in 'uHexMap.pas',
  uKeys in 'U_Base\uKeys.pas',
  RepRead in 'U_Base\RepRead.pas',
  uHistory in 'U_Base\uHistory.pas',
  uInterface in 'U_Base\uInterface.pas',
  uScript in 'U_Base\uScript.pas',
  uAvatars in 'U_Base\uAvatars.pas',
  uManager in 'U_Manager\uManager.pas' {ManagerForm},
  uMgrOptions,
  uNewGame,
  uMapExport in 'U_Tools\uMapExport.pas' {MapExportForm},
  uOrders in 'U_Orders\uOrders.pas',
  uUnitRecs in 'U_Base\uUnitRecs.pas',
  uBattle in 'uBattle.pas' {BattleForm},
  uOptions in 'uOptions.pas' {OptionForm},
  uSoldiers in 'U_Tools\uSoldiers.pas' {SoldiersForm},
  uExport in 'U_Base\uExport.pas',
  uListFilter in 'uListFilter.pas' {ListFilterForm},
  uAnalyzers in 'U_Base\uAnalyzers.pas',
  uSkillEdit in 'U_Tools\uSkillEdit.pas' {SkillEditForm},
  uSpecEdit in 'U_Tools\uSpecEdit.pas' {SpecEditForm},
  uItemEdit in 'U_Tools\uItemEdit.pas' {ItemEditForm},
  uStructEdit in 'U_Tools\uStructEdit.pas' {StructEditForm},
  uArmy in 'U_Base\uArmy.pas',
  uUnitArmies in 'U_Tools\uUnitArmies.pas' {UArmyForm},
  MyStrings in 'U_Base\MyStrings.pas',
  uUnitProduction in 'U_Tools\uUnitProduction.pas' {UnitProductionForm},
  uUnitFilter in 'U_Tools\uUnitFilter.pas' {UnitFilterForm},
  uSpyReps in 'U_Tools\uSpyReps.pas' {SpyRepForm},
  uNeeds in 'U_Base\uNeeds.pas',
  uUnitNeeds in 'U_Tools\uUnitNeeds.pas' {UnitNeedsForm},
  uNewUnit in 'U_Orders\uNewUnit.pas' {NewUnitForm},
  uShortcuts in 'uShortcuts.pas',
  uTerrEdit in 'U_Tools\uTerrEdit.pas' {TerrEditForm},
  uRoute in 'U_Base\uRoute.pas',
  uFactions in 'U_Tools\uFactions.pas' {FactionForm},
  uTeach in 'U_Orders\uTeach.pas' {TeachForm},
  uGameStart in 'U_Base\uGameStart.pas',
  uMiniMap in 'uMiniMap.pas', CylinderMap {MiniMapForm},
  uTurnEvents in 'U_Tools\uTurnEvents.pas' {TurnEventsForm},
  uTownTrade in 'U_Tools\uTownTrade.pas' {TownTradeForm},
  uBattleDraw in 'U_Base\uBattleDraw.pas',
  uTaxTrade in 'U_Tools\uTaxTrade.pas' {TaxTradeForm},
  uScriptEdit in 'U_Tools\uScriptEdit.pas' {ScriptEditForm},
  uWantedItems in 'U_Tools\uWantedItems.pas' {WantedItemsForm},
  uAvatarEdit in 'U_Tools\uAvatarEdit.pas' {AvatarForm},
  uPathFind in 'U_Base\uPathFind.pas',
  uRegistration in 'U_Base\uRegistration.pas', {RegForm}
  ImageBtn,
  AtlaDate;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Atlantis Advisor';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRegForm, RegForm);
  Application.Run;
end.
