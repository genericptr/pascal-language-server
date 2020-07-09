// Pascal Language Server
// Copyright 2020 Arjan Adriaanse

// This file is part of Pascal Language Server.

// Pascal Language Server is free software: you can redistribute it
// and/or modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// Pascal Language Server is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Pascal Language Server.  If not, see
// <https://www.gnu.org/licenses/>.

unit general;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  lsp, basic, capabilities, documentSymbol, settings;

type

  { TWorkspaceFolder }

  TWorkspaceFolder = class(TCollectionItem)
  private
    fUri: TDocumentUri;
    fName: String;
  published
    // The associated URI for this workspace folder.
    property uri: TDocumentUri read fUri write fUri;
    // The name of the workspace folder. Used to refer to this
    // workspace folder in the user interface.
    property name: string read fName write fName;
  end;
  TWorkspaceFolderItems = specialize TGenericCollection<TWorkspaceFolder>;

  { TVoidParams }

  TVoidParams = class(TPersistent);

  { TInitializationOptions }

  TInitializationOptions = class(TServerSettings)
  end;

  { TInitializeParams }

  TInitializeParams = class(TPersistent)
  private type
    TClientInfo = class
      // The name of the client as defined by the client.
      clientInfo: string;
      // The client's version as defined by the client.
      version: string;
    end;
  private
    fProcessId: integer;
    fClientInfo: TClientInfo;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
    fInitializationOptions: TInitializationOptions;
    fWorkspaceFolders: TWorkspaceFolderItems;
  published
    property processId: integer read fProcessId write fProcessId;
    property clientInfo: TClientInfo read fClientInfo write fClientInfo;
    property rootUri: string read fRootUri write fRootUri;
    property capabilities: TClientCapabilities read fCapabilities write fCapabilities;
    property initializationOptions: TInitializationOptions read fInitializationOptions write fInitializationOptions;
    property workspaceFolders: TWorkspaceFolderItems read fWorkspaceFolders write fWorkspaceFolders;
  public
    procedure AfterConstruction; override;
  end;

  { TInitializeResult }

  TInitializeResult = class(TPersistent)
  private
    fCapabilities: TServerCapabilities;
  published
    property capabilities: TServerCapabilities read fCapabilities write fCapabilities;
  end;

  { TInitialize }

  TInitialize = class(specialize TLSPRequest<TInitializeParams, TInitializeResult>)
    function Process(var Params : TInitializeParams): TInitializeResult; override;
  end;

  { TInitialized }

  TInitialized = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancelParams }

  TCancelParams = class(TPersistent)
  private
    fId: Integer;
  published
    property id: Integer read fId write fId;
  end;

  { TShutdown }

  TShutdown = class(specialize TLSPRequest<TVoidParams, TPersistent>)
    function Process(var Params : TVoidParams): TPersistent; override;
  end;

  { TExit }

  TExit = class(specialize TLSPNotification<TVoidParams>)
    procedure Process(var Params : TVoidParams); override;
  end;

  { TCancel }

  TCancel = class(specialize TLSPNotification<TCancelParams>)
    procedure Process(var Params : TCancelParams); override;
  end;

implementation
uses
  SysUtils, RegExpr, DefineTemplates;

{ TInitializeParams }

procedure TInitializeParams.AfterConstruction;
begin
  inherited;

  if clientInfo = nil then
    clientInfo := TClientInfo.Create;
    
  if initializationOptions = nil then
    initializationOptions := TInitializationOptions.Create;

  workspaceFolders := TWorkspaceFolderItems.Create;
end;

{ TInitialize }

function TInitialize.Process(var Params : TInitializeParams): TInitializeResult;
var
  CodeToolsOptions: TCodeToolsOptions;
  Option, Path: String;
  URI: TURI;
  Item: TCollectionItem;
  re: TRegExpr;
  DirectoryTemplate,
  UnitPathTemplate: TDefineTemplate;
  ServerCapabilities: TServerCapabilities;
  Macros: TMacroMap;
begin with Params do
  begin
    CodeToolsOptions := TCodeToolsOptions.Create;

    // TODO: we need to copy this or implement ref counting
    // once we figure out how memory is going to work with
    // the JSON-RPC streaming model
    ServerSettings := initializationOptions;
    
    // replace macros in server settings
    Macros := TMacroMap.Create;
    Macros.Add('tmpdir', GetTempDir(true));
    Macros.Add('root', ParseURI(rootUri).path);

    ServerSettings.ReplaceMacros(Macros);

    // load the symbol manager if it's enabled
    if ServerSettings.options.documentSymbols or ServerSettings.options.workspaceSymbols then
      SymbolManager := TSymbolManager.Create;

    ServerCapabilities := TServerCapabilities.Create(initializationOptions);

    re := TRegExpr.Create('^(-(Fu|Fi)+)(.*)$');
    with CodeToolsOptions do
      begin
        // note: these should be added to ENV variables in the editor
        //FPCSrcDir := '/usr/local/share/fpcsrc';
        //FPCPath := '/usr/local/lib/fpc/3.0.4/ppcx64';

        InitWithEnvironmentVariables;

        writeln(StdErr, 'working diretory: ', GetCurrentDir);

        // attempt to load optional config file
        Path := ExpandFileName(initializationOptions.CodeToolsConfig);
        if FileExists(Path) then
          begin
            writeln(StdErr, 'Loading config file: ', Path);
            LoadFromFile(Path);
          end;

        // include workspace paths as search paths
        if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths or
          ServerSettings.options.includeWorkspaceFoldersAsIncludePaths then
          for Item in workspaceFolders do
            begin
              URI := ParseURI(TWorkspaceFolder(Item).uri);
              Path := URI.Path + URI.Document;
              
              if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths then
                begin
                  initializationOptions.FPCOptions.Add('-Fu'+Path);
                  initializationOptions.FPCOptions.Add('-Fi'+Path);
                end;

              // if the server supports workspace symbols then
              // cane the workspace folder for symbols
              if ServerCapabilities.workspaceSymbolProvider then
                SymbolManager.Scan(Path, false);
            end;

        for Option in initializationOptions.FPCOptions do
          begin
            // expand file names in switches with paths
            if re.Exec(Option) then
              begin
                writeln(stderr, 'switch: ', re.Match[2], ' = ', ExpandFileName(re.Match[3]));
                FPCOptions := FPCOptions + '-' + re.Match[2] + ExpandFileName(re.Match[3]) + ' ';
              end
            else
              begin
                writeln(stderr, 'switch: ', Option);
                FPCOptions := FPCOptions + Option + ' ';
              end;
          end;
        writeln(stderr, 'FPCOptions: ', FPCOptions);

        if ServerSettings.&program <> '' then
          begin
            Path := ExpandFileName(ServerSettings.&program);
            if FileExists(Path) then
              begin
                writeln(StdErr, 'Main program file: ', Path);
                ServerSettings.&program := Path;
              end
            else
              begin
                writeln(StdErr, 'Error: Main program file ', Path, ' can''t be found.');
                ServerSettings.&program := '';
              end;
          end;

        writeln(stderr, 'Symbol Database: ', ServerSettings.symbolDatabase);
        writeln(stderr, 'Maximum Completions: ', ServerSettings.maximumCompletions);
        writeln(stderr, 'Overload Policy: ', ServerSettings.overloadPolicy);

        Flush(stderr);

        if rootUri <> '' then
          ProjectDir := ParseURI(rootUri).Path;
      end;
    re.Free;
    
    with CodeToolBoss do
      begin
        Init(CodeToolsOptions);
        IdentifierList.SortForHistory := True;
        IdentifierList.SortForScope := True;
      end;

    Result := TInitializeResult.Create;
    Result.capabilities := ServerCapabilities;
  end;
end;

{ TInitialized }

procedure TInitialized.Process(var Params : TVoidParams);
begin
  // do nothing
end;

{ TShutdown }

function TShutdown.Process(var Params : TVoidParams): TPersistent;
begin
  // do nothing
  result := nil;
end;

{ TExit }

procedure TExit.Process(var Params : TVoidParams);
begin
  Halt(0);
end;

{ TCancel }

procedure TCancel.Process(var Params : TCancelParams);
begin
  // not supported
end;

initialization
  LSPHandlerManager.RegisterHandler('initialize', TInitialize);
  LSPHandlerManager.RegisterHandler('initialized', TInitialized);
  LSPHandlerManager.RegisterHandler('shutdown', TShutdown);
  LSPHandlerManager.RegisterHandler('exit', TExit);
  LSPHandlerManager.RegisterHandler('$/cancelRequest', TCancel);
end.

