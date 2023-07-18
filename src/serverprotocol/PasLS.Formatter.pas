unit PasLS.Formatter;

{$mode objfpc}{$H+}

interface

uses
  { RTL/FCL }
  Classes, SysUtils, SyncObjs,
  { JCF }
  ConvertTypes,FileConverter,
  { LSP }
  LSP.Messages;

Type

  { TFileFormatter }

  TFileFormatter = Class
  private
    FConfigurationFile: String;
    FConverter: TFileConverter;
    FFileName: String;
    FTransport: TMessageTransport;
    FLock : TCriticalSection;
    procedure DoMessage(const psUnit, psMessage: string;
      const peMessageType: TStatusMessageType; const piY, piX: integer);
  protected
    Procedure Lock;
    Procedure Unlock;
    function CreateConverter : TFileConverter; virtual;
    Property Converter : TFileConverter read FConverter;
    procedure LoadConfiguration; virtual;
    procedure DefaultConfiguration; virtual;
    Procedure DoProcess; virtual;
    // Only valid during process call
    Property FileName : String Read FFileName;
    Property ConfigurationFile : String Read FConfigurationFile;
  Public
    Constructor Create(aTransport : TMessageTransport);
    Destructor Destroy; override;
    Procedure Process(const aFileName : String; const aConfigurationFile : String);
    Property Transport : TMessageTransport read FTransport;
  end;

implementation

uses
  SetTransform, SetReturns, SettingsTypes, JcfSettings;

{ TFileFormatter }

procedure TFileFormatter.DoMessage(const psUnit, psMessage: string;
  const peMessageType: TStatusMessageType; const piY, piX: integer);

var
  S,Msg : String;

begin
  writestr(S,peMessageType);
  Delete(S,1,2); // remove prefix.
  Msg:=Format('[%s] %s(%d,%d): %s',[S,psUnit,piY-1,piX-1,psMessage]);
  Transport.SendDiagnostic(Msg);
end;

procedure TFileFormatter.Lock;
begin
  FLock.Enter;
end;

procedure TFileFormatter.Unlock;
begin
  FLock.Leave;
end;

function TFileFormatter.CreateConverter: TFileConverter;
begin
  Result:=TFileConverter.Create;
end;

procedure TFileFormatter.LoadConfiguration;

begin
  FormatSettingsFromFile(FConfigurationFile);
end;

procedure TFileFormatter.DefaultConfiguration;

Var
  Cfg : TFormattingSettings; // shortcut.
begin
  Cfg:=FormattingSettings;
  With Cfg.Obfuscate do
    begin
      Enabled:=False;
    end;
  with Cfg.Clarify do
    begin
      WarnUnusedParams:=False;
      // Maybe overkill ?
      IgnoreUnusedParams.Add('Sender');
    end;
  With Cfg.Indent do
    begin
      IndentSpaces:=2;
      IndentBeginEnd:=True;
      IndentBeginEndSpaces:=2;
      IndentLibraryProcs:=True;
      IndentProcedureBody:=False;
      KeepCommentsWithCodeInGlobals:=True;
      KeepCommentsWithCodeInProcs:=True;
      KeepCommentsWithCodeInClassDef:=True;
      KeepCommentsWithCodeElsewhere:=True;
      IndentElse:=False;
      IndentCaseElse:=True;
      IndentNestedTypes:=False;
      IndentVarAndConstInClass:=False;
    end;
  With Cfg.Spaces do
    begin
      TabsToSpaces:=True;
      SpacesToTabs:=False;
      SpacesPerTab:=2;
      SpacesForTab:=2;
      FixSpacing:=True;
      SpaceBeforeClassHeritage:=False;
      SpacesBeforeColonVar:=0;
      SpacesBeforeColonConst:=0;
      SpacesBeforeColonParam:=0;
      SpacesBeforeColonFn:=0;
      SpacesBeforeColonClassVar:=0;
      SpacesBeforeColonRecordField:=0;
      SpacesBeforeColonCaseLabel:=0;
      SpacesBeforeColonLabel:=0;
      SpacesBeforeColonInGeneric:=0;
      MaxSpacesInCode:=2;
      UseMaxSpacesInCode:=True;
      SpaceForOperator:=eAlways;
      SpaceBeforeOpenBracketsInFunctionDeclaration:=False;
      SpaceBeforeOpenBracketsInFunctionCall:=False;
      SpaceBeforeOpenSquareBracketsInExpression:=False;
      SpaceAfterOpenBrackets:=False;
      SpaceBeforeCloseBrackets:=False;
      MoveSpaceToBeforeColon:=False;
    end;
  With Cfg.Returns do
    begin
      RebreakLines:=rbUsually;
      MaxLineLength:=90;
      NumReturnsAfterFinalEnd:=1;
      RemoveBadReturns:=True;
      AddGoodReturns:=True;
      UsesClauseOnePerLine:=False;
      BreakAfterUses:=False;
      RemoveExpressionReturns:=True;
      RemoveVarReturns:=True;
      RemovePropertyReturns:=True;
      RemoveProcedureDefReturns:=True;
      RemoveBadReturns:=True;
      RemoveVarBlankLines:=True;
      RemoveProcHeaderBlankLines:=True;
      BlockBeginStyle:=eLeave;
      BlockStyle:=eAlways;
      LabelStyle:=eLeave;
      LabelBeginStyle:=eLeave;
      CaseLabelStyle:=eLeave;
      CaseBeginStyle:=eLeave;
      CaseElseStyle:=eAlways;
      CaseElseBeginStyle:=eAlways;
      EndElseStyle:=eAlways;
      ElseIfStyle:=eLeave;
      ElseBeginStyle:=eAlways;
      BeforeCompilerDirectUses:=eAlways;
      BeforeCompilerDirectStatements:=eAlways;
      BeforeCompilerDirectGeneral:=eLeave;
      AfterCompilerDirectUses:=eLeave;
      AfterCompilerDirectStatements:=eAlways;
      AfterCompilerDirectGeneral:=eLeave;
      ReturnChars:=rcLeaveAsIs;
      RemoveConsecutiveBlankLines:=True;
      MaxConsecutiveBlankLines:=4;
      MaxBlankLinesInSection:=1;
      LinesBeforeProcedure:=1;
    end;
  With Cfg.Caps do
    begin
      Enabled:=True;
      ReservedWords:=ctLower;
      Operators:=ctLower;
      Directives:=ctUpper;
      Constants:=ctLower;
      Types:=ctLeaveAlone;
    end;
  With Cfg.SpecificWordCaps do
    begin
      Enabled:=False;
    // Add here
    // Words.Add('');
    end;
  with Cfg.IdentifierCaps do
    begin
      Enabled:=True;
      Words.AddCommaText('ActivePage,AnsiCompareStr,AnsiCompareText,AnsiUpperCase,AsBoolean,AsDateTime,AsFloat,AsInteger,Assign,AsString,AsVariant,BeginDrag,Buttons,Caption,Checked,Classes,ClassName,Clear,Close,Components,Controls,Count,Create,Data,Dec,Delete,Destroy,Dialogs,Enabled,EndDrag,EOF,Exception,Execute,False,FieldByName,First,Forms,Free,FreeAndNil,GetFirstChild,Graphics,Height,idAbort,idCancel,idIgnore,IDispatch,idNo,idOk,idRetry,idYes,Inc,Initialize,IntToStr,ItemIndex,IUnknown,Lines,Math,MaxValue,mbAbort,mbAll,mbCancel,mbHelp,mbIgnore,mbNo,mbOK,mbRetry,mbYes,mbYesToAll,Messages,MinValue,mnNoToAll,mrAbort,mrAll,mrCancel,mrIgnore,mrNo,mrNone,mrNoToAll,mrOk,mrRetry,mrYes,mrYesToAll,mtConfirmation,mtCustom,mtError,mtInformation,mtWarning,Name,Next,Open,Ord,ParamStr,PChar,Perform,ProcessMessages,Read,ReadOnly,RecordCount,Register,Release,Result,Sender,SetFocus,Show,ShowMessage,Source,StdCtrls,StrToInt,SysUtils,TAutoObject,TButton,TComponent,TDataModule,Text,TForm,TFrame,TList,TNotifyEvent,TObject,TObjectList,TPageControl,TPersistent,True,TStringList,TStrings,TTabSheet,Unassigned,Value,Visible,WideString,Width,Windows,Write');
    end;
  with Cfg.NotIdentifierCaps do
    begin
      Enabled:=True;
      Words.AddCommaText('False,Name,nil,PChar,read,ReadOnly,True,WideString,write');
    end;
  with Cfg.UnitNameCaps do
    begin
      Enabled:=True;
      Words.AddCommaText('ActnColorMaps,ActnCtrls,ActnList,ActnMan,ActnMenus,ActnPopup,ActnRes,ADOConst,ADODB,ADOInt,AppEvnts,AxCtrls,BandActn,bdeconst,bdemts,Buttons,CheckLst,Classes,Clipbrd.pas,CmAdmCtl,ComCtrls,ComStrs,Consts,Controls,CtlConsts,CtlPanel,CustomizeDlg,DataBkr,DB,DBActns,dbcgrids,DBClient,DBClientActnRes,DBClientActns,DBCommon,DBConnAdmin,DBConsts,DBCtrls,DbExcept,DBGrids,DBLocal,DBLocalI,DBLogDlg,dblookup,DBOleCtl,DBPWDlg,DBTables,DBXpress,DdeMan,Dialogs,DrTable,DSIntf,ExtActns,ExtCtrls,ExtDlgs,FileCtrl,FMTBcd,Forms,Graphics,GraphUtil,Grids,HTTPIntr,IB,IBBlob,IBCustomDataSet,IBDatabase,IBDatabaseInfo,IBDCLConst,IBErrorCodes,IBEvents,IBExternals,IBExtract,IBGeneratorEditor,IBHeader,IBIntf,IBQuery,IBRestoreEditor,IBSecurityEditor,IBServiceEditor,IBSQL,IBSQLMonitor,IBStoredProc,IBTable,IBUpdateSQL,IBUtils,IBXConst,ImgList,Jcl8087,JclAbstractContainers,JclAlgorithms,JclAnsiStrings,JclAppInst,JclArrayLists,JclArraySets,JclBase,JclBinaryTrees,JclBorlandTools,JclCIL,JclCLR,JclCOM,JclComplex,JclCompression,JclConsole,JclContainerIntf,JclCounter,JclDateTime,JclDebug,JclDotNet,JclEDI,JclEDI_ANSIX12,JclEDI_ANSIX12_Ext,JclEDI_UNEDIFACT,JclEDI_UNEDIFACT_Ext,JclEDISEF,JclEDITranslators,JclEDIXML,JclExprEval,JclFileUtils,JclFont,JclGraphics,JclGraphUtils,JclHashMaps,JclHashSets,JclHookExcept,JclIniFiles,JclLANMan,JclLinkedLists,JclLocales,JclLogic,JclMapi,JclMath,JclMetadata,JclMIDI,JclMime,JclMiscel,JclMsdosSys,JclMultimedia,JclNTFS,JclPCRE,JclPeImage,JclPrint,JclQGraphics,JclQGraphUtils,JclQueues,JclRegistry,JclResources,JclRTTI,JclSchedule,JclSecurity,JclShell,JclSimpleXml,JclStacks,JclStatistics,JclStreams,JclStrHashMap,JclStringLists,JclStrings,JclStructStorage,JclSvcCtrl,JclSynch,JclSysInfo,JclSysUtils,JclTask,JclTD32,JclUnicode,JclUnitConv,JclUnitVersioning,JclUnitVersioningProviders,JclValidation,JclVectors,JclWideFormat,JclWideStrings,JclWin32,JclWin32Ex,JclWinMIDI,ListActns,Mask,MConnect,Menus,Midas,MidasCon,MidConst,MPlayer,MtsRdm,Mxconsts,ObjBrkr,OleAuto,OleConst,OleCtnrs,OleCtrls,OleDB,OleServer,Outline,Printers,Provider,recerror,ScktCnst,ScktComp,ScktMain,SConnect,ShadowWnd,SimpleDS,SMINTF,SqlConst,SqlExpr,SqlTimSt,StdActnMenus,StdActns,StdCtrls,StdStyleActnCtrls,SvcMgr,SysUtils,TabNotBk,Tabs,TConnect,Themes,ToolWin,ValEdit,VDBConsts,WinHelpViewer,XPActnCtrls,XPMan,XPStyleActnCtrls');
    end;
  with Cfg.SetAsm do
    begin
      Capitalisation:=ctUpper;
      BreaksAfterLabel:=1;
      BreaksAfterLabelEnabled:=True;
      StatementIndentEnabled:=True;
      StatementIndent:=7;
      ParamsIndentEnabled:=True;
      ParamsIndent:=15;
    end;
  With Cfg.PreProcessor do
    begin
    Enabled:=False;
    // DefinedSymbols.Add('');
    // DefinedOptions.Add('');
    end;
  With Cfg.Align do
    begin
      AlignAssign:=False;
      AlignConst:=False;
      AlignTypedef:=False;
      AlignVar:=False;
      AlignComment:=False;
      AlignField:=False;
      InterfaceOnly:=False;
      MinColumn:=2;
      MaxColumn:=60;
      MaxVariance:=5;
      MaxVarianceInterface:=5;
      MaxUnalignedStatements:=0;
    end;
  With Cfg.Replace do
    begin
    Enabled:=False;
    end;
  With Cfg.UsesClause do
    begin
    RemoveEnabled:=False ;
    InsertInterfaceEnabled:=False ;
    InsertImplementationEnabled:=False;
    FindReplaceEnabled:=False;
    // Remove.Add('');
    // InsertInterface.Add('');
    // InsertImplementation.Add('')
    // Find.Add('');
    // Replace.Add('')
    end;
  With Cfg.Transform do
    begin
    BeginEndStyle:= eLeave ;
    AddBlockEndSemicolon:= True ;
    SortInterfaceUses:=False;
    SortImplementationUses:=False;
    SortProgramUses:=False;
    BreakUsesSortOnComment:=False;
    BreakUsesSortOnReturn:=False;
    UsesSortOrder:=eAlpha;
    SortUsesNoComments:= False ;
    end;
end;

procedure TFileFormatter.DoProcess;
begin
  if (ConfigurationFile<>'') then
    LoadConfiguration
  else
    DefaultConfiguration;

  FConverter.ProcessFile(FileName);
end;

constructor TFileFormatter.Create(aTransport: TMessageTransport);
begin
  FTransport:=aTransport;
  FLock:=TCriticalSection.Create;
  FConverter:=CreateConverter;
  FConverter.GuiMessages:=False;
  FConverter.YesAll:=True;
  FConverter.OnStatusMessage:=@DoMessage;
end;

destructor TFileFormatter.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FConverter);
  inherited Destroy;
end;

procedure TFileFormatter.Process(const aFileName: String;
  const aConfigurationFile: String);
begin
  FFileName:=aFileName;
  FConfigurationFile:=aConfigurationFile;
  Lock;
  try
    DoProcess;
  finally
    UnLock;
    FFileName:='';
    FConfigurationFile:='';
  end;

end;

end.

