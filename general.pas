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
  Classes, CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
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
  private
    //fProcessId: string;
    fRootUri: string;
    fCapabilities: TClientCapabilities;
    fInitializationOptions: TInitializationOptions;
    fWorkspaceFolders: TWorkspaceFolderItems;
  published
    //property processId: string read fProcessId write fProcessId;
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
  DirectoryTemplate: TDefineTemplate;
begin with Params do
  begin
    CodeToolsOptions := TCodeToolsOptions.Create;
    re := TRegExpr.Create('^(-\w+)(.*)$');
    with CodeToolsOptions do
    begin
      InitWithEnvironmentVariables;

      // attempt to load optional config file
      Path := ExpandFileName(initializationOptions.CodeToolsConfig);
      if FileExists(Path) then
        begin
          writeln(StdErr, 'Loading config file: ', Path);
          LoadFromFile(Path);
        end;

      // TODO: we need to copy this or implement ref counting
      // once we figure out how memory is going to work with
      // the JSON-RPC streaming model
      ServerSettings := initializationOptions;

      SymbolManager := TSymbolManager.Create;

      // include workspace paths as search paths
      if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths or
        ServerSettings.options.includeWorkspaceFoldersAsIncludePaths then
        for Item in workspaceFolders do
          begin
            URI := ParseURI(TWorkspaceFolder(Item).uri);
            Path := URI.Path + URI.Document;
            
            // todo: there is no include paths now!
            if ServerSettings.options.includeWorkspaceFoldersAsUnitPaths then
              initializationOptions.UnitPaths.Add(Path);

            if SymbolManager <> nil then
              SymbolManager.Scan(Path, true);
          end;

      for Option in initializationOptions.FPCOptions do
        begin
          // parse compiler switches and expand file names
          if re.Exec(Option) then
            FPCOptions := FPCOptions + re.Match[1] + ExpandFileName(re.Match[2]) + ' '
          else
            FPCOptions := FPCOptions + Option + ' ';
        end;


      for Path in initializationOptions.UnitPaths do
        begin
          writeln(StdErr, 'Added unit path: ', ExpandFileName(Path));
          DirectoryTemplate := TDefineTemplate.Create('Directory','','',ExpandFileName(Path),da_Directory);
          CodeToolBoss.DefineTree.Add(DirectoryTemplate);
        end;

      if FPCOptions <> '' then
        writeln(StdErr, 'FPC Options: ', FPCOptions);

      if ServerSettings.&program <> '' then
        begin
          Path := ExpandFileName(ServerSettings.&program);
          if FileExists(Path) then
            writeln(StdErr, 'Main program file: ', Path)
          else
            writeln(StdErr, 'Error: Main program file ', Path, ' can''t be found.');
        end;

      Flush(stderr);
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
    Result.capabilities := TServerCapabilities.Create;
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

