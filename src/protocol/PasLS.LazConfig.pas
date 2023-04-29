// Pascal Language Server
// Copyright 2021 Philip Zander

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

unit PasLS.LazConfig;

{$mode objfpc}{$H+}

interface
uses
  SysUtils, Classes, Contnrs, CodeToolManager, CodeToolsConfig, URIParser, LazUTF8,
  DefineTemplates, FileUtil, LazFileUtils, DOM, XMLRead;

procedure GuessCodeToolConfig(Options: TCodeToolsOptions);

implementation
uses
  iostream, streamex, StreamIO;

type
  TPaths = record
    // Search path for units (OtherUnitFiles)
    UnitPath:         string;
    // Search path for includes (IncludeFiles)
    IncludePath:      string;
    // Additional sources, not passed to compiler (SrcFiles)
    SrcPath:          string;
  end;

  TPackage = class;

  TDependency = record
    // Name of the package, e.g. 'LCLBase'
    Name:             string;

    // Projects may hardcode a path to a package. If a path was hardcoded, Path
    // will contain the expanded path, otherwise will be empty string.
    Path:             string;

    // Whether the hardcoded path should take precedence over a global package
    // of the same name.
    Prefer:           Boolean;

    // Once we have resolved the dependency, we cache a reference to the package
    // here:
    Package:          TPackage;
  end;

  { TPackage }

  TPackage = class
    // Name of the package / project
    //Name:             string;
    PkgFile:          string;

    // Home directory of the package / project
    Dir:              string;

    // Valid: True if the package was found, False otherwise. If False, this
    // is a dummy object whose only purpose is to prevent us from trying to load
    // a non-existing package multiple times.
    Valid:            Boolean;

    // The search path resolution process involves several stages:
    // 0. Compile all 1st party search paths defined in the package file and
    //    store them in "Paths".
    // 1. Resolve the dependencies (find file name for a given package name)
    // 2. Compile the search paths for all dependencies and add them to our own
    //    search paths (Resolved Paths)
    // 3. Announce the search paths for the package home directory to
    //    CodeToolBoss.

    DidResolveDeps:   Boolean; // True after step 1 is completed
    DidResolvePaths:  Boolean; // True after step 2 is completed
    Configured:       Boolean; // True after step 3 is completed

    Visited:          Boolean; // Temporary flag while guessing dependencies.

    // Absolute 1st-degree search paths for this package
    Paths:            TPaths;

    // List of dependencies of this package
    Dependencies:     array of TDependency;

    // List of packages requiring this package
    // (only 1st degree dependencies)
    RequiredBy:       array of TPackage;

    // Search paths including dependencies
    ResolvedPaths:    TPaths;

    constructor Create;
  end;

var
  DebugOutput: TStream;
  PkgNameToPath: TFPStringHashTable;
  // Map Path -> TPackage
  PkgCache:      TFPObjectHashTable;

procedure InitLog(Destination: TStream);
begin
  DebugOutput := Destination;
end;

procedure DebugLog(const Msg: string);
begin
  if (DebugOutput <> nil) and (Msg <> '') then
    DebugOutput.WriteBuffer(Msg[1], Length(Msg));
end;

procedure DebugLog(const Fmt: string; Args: array of const);
var
  s: string;
begin
  s := Format(Fmt, Args) + LineEnding;
  DebugLog(s);
end;

function MergePaths(Paths: array of string): string;
var
  i: Integer;
begin
  Result := '';
  for i := low(Paths) to high(Paths) do
  begin
    if (Result <> '') and (Paths[i] <> '') then
      Result := Result + ';' + Paths[i]
    else if (Result = '') and (Paths[i] <> '') then
      Result := Paths[i];
  end;
end;


// yuck
var
  _FakeAppName, _FakeVendorName: string;

function GetFakeAppName: string;
begin
  Result := _FakeAppName;
end;

function GetFakeVendorName: string;
begin
  Result := _FakeVendorName;
end;

function GetConfigDirForApp(AppName, Vendor: string; Global: Boolean): string;
var
  OldGetAppName:     TGetAppNameEvent;
  OldGetVendorName:  TGetVendorNameEvent;
begin
  _FakeAppName     := AppName;
  _FakeVendorName  := Vendor;
  OldGetAppName    := OnGetApplicationName;
  OldGetVendorName := OnGetVendorName;
  try
    OnGetApplicationName := @GetFakeAppName;
    OnGetVendorName      := @GetFakeVendorName;
    Result               := GetAppConfigDir(Global);
  finally
    OnGetApplicationName := OldGetAppName;
    OnGetVendorName      := OldGetVendorName;
  end;
end;

procedure PopulateGlobalPackages(const SearchPaths: array of string);
var
  Files:          TStringList;
  Dir, FileName, Name: string;
begin
  Files := TStringList.Create;
  try
    for Dir in SearchPaths do
    begin
      DebugLog('  %s/*.lpk', [Dir]);
      FindAllFiles(Files, Dir, '*.lpk');
    end;

    for FileName in Files do
    begin
      Name := ExtractFileNameOnly(FileName);
      PkgNameToPath[UpperCase(Name)] := FileName;
    end;
    DebugLog('  Found %d packages', [Files.Count]);

  finally
    Files.Free;
  end;
end;

procedure LoadPackageOrProject(const FileName: string);
var
  Doc:     TXMLDocument;
  Root:    TDomNode;
  Package: TPackage;

  function GetAdditionalPaths(
    SearchPaths: TDomNode; const What: string
  ): String;
  var
    Node: TDomNode;
    Segments: TStringArray;
    S, Segment, AbsSegment: string;
  begin
    Result := '';

    Node := SearchPaths.FindNode(What);
    if Assigned(Node) then
      Node := Node.Attributes.GetNamedItem('Value');
    if not Assigned(Node) then
      Exit;

    S := Node.NodeValue;
    Segments := S.Split([';'], TStringSplitOptions.ExcludeEmpty);

    for Segment in Segments do
    begin
      AbsSegment := CreateAbsolutePath(Segment, Package.Dir);
      Result     := Result + ';' + AbsSegment;
    end;
  end;

  procedure LoadPaths;
  var
    CompilerOptions, SearchPaths: TDomNode;
  begin
    Package.Paths.IncludePath := Package.Dir;
    Package.Paths.UnitPath    := Package.Dir;

    CompilerOptions := Root.FindNode('CompilerOptions');
    if not Assigned(CompilerOptions) then
      Exit;

    SearchPaths := CompilerOptions.FindNode('SearchPaths');
    if not Assigned(SearchPaths) then
      Exit;

    Package.Paths.IncludePath := MergePaths([
      Package.Paths.IncludePath, 
      GetAdditionalPaths(SearchPaths, 'IncludeFiles')
    ]);
    Package.Paths.UnitPath    := MergePaths([
      Package.Paths.UnitPath, 
      GetAdditionalPaths(SearchPaths, 'OtherUnitFiles')
    ]);
    Package.Paths.SrcPath     := GetAdditionalPaths(SearchPaths, 'SrcPath');
  end;

  procedure LoadDeps;
  var
    Deps, Item, Name, 
    Path, Prefer:      TDomNode;
    Dep:               TDependency;
    i, DepCount:       Integer;
  begin
    if UpperCase(ExtractFileExt(FileName)) = '.LPK' then
      Deps := Root.FindNode('RequiredPkgs')
    else
      Deps := Root.FindNode('RequiredPackages');

    if not Assigned(Deps) then
      Exit;

    DepCount := 0;
    SetLength(Package.Dependencies, Deps.ChildNodes.Count);

    for i := 0 to Deps.ChildNodes.Count - 1 do
    begin
      Item        := Deps.ChildNodes.Item[i];

      Name        := Item.FindNode('PackageName');
      if not Assigned(Name) then
        continue;

      Name        := Name.Attributes.GetNamedItem('Value');
      if not Assigned(Name) then
        continue;

      Dep.Name    := Name.NodeValue; 
      Dep.Prefer  := False;
      Dep.Package := nil;
      Dep.Path    := '';

      Path := Item.FindNode('DefaultFilename');

      if Assigned(Path) then
      begin
        Prefer := Path.Attributes.GetNamedItem('Prefer');
        Path   := Path.Attributes.GetNamedItem('Value');

        Dep.Prefer := Assigned(Prefer) and (Prefer.NodeValue = 'True');
        if Assigned(Path) then
          Dep.Path := CreateAbsolutePath(Path.NodeValue, Package.Dir);

        //DebugLog('HARDCODED DEP %s in %s', [Dep.Name, Dep.Path]);
        //DebugLog('  Dir: %s, Rel: %s', [Package.Dir, Path.NodeValue]);
      end;

      Package.Dependencies[DepCount] := Dep;
      Inc(DepCount);
    end;
  end;

begin
  if Assigned(PkgCache[FileName]) then
    Exit;

  DebugLog('Loading %s', [FileName]);

  Package       := TPackage.Create;
  Package.Valid := False;
  Package.Dir   := ExtractFilePath(FileName);
  Package.PkgFile := FileName;

  PkgCache[FileName] := Package;

  try
    try
      ReadXMLFile(doc, filename);

      Root := Doc.DocumentElement;
      if Root.NodeName <> 'CONFIG' then
        Exit;

      if UpperCase(ExtractFileExt(FileName)) = '.LPK' then
        Root := Root.FindNode('Package')
      else
        Root := Root.FindNode('ProjectOptions');

      if not Assigned(Root) then
        Exit;

      LoadPaths;
      LoadDeps;

      Package.Valid := True;
    except on E:Exception do
      // swallow
      DebugLog('Error loading %s: %s', [FileName, E.Message]);
    end;
  finally
    if Assigned(doc) then
      FreeAndNil(doc);
  end;
end;

function GetPackageOrProject(const FileName: String): TPackage;
begin
  Result := TPackage(PkgCache[FileName]);
  if not Assigned(Result) then
  begin
    LoadPackageOrProject(FileName);
    Result := TPackage(PkgCache[FileName]);
  end;
end;

function LookupGlobalPackage(const Name: String): String;
begin
  Result := PkgNameToPath[UpperCase(Name)];
end;

{ TPackage }

constructor TPackage.Create;
begin
  Valid                  := False;
  Configured             := False;
  DidResolvePaths        := False;
  DidResolveDeps := False;
end;

// Resolve the dependencies of Pkg, and then the dependencies of the
// dependencies and so on. Uses global registry and paths locally specified in
// the package/project file (.lpk/.lpi) as a data source.
procedure ResolveDeps(Pkg: TPackage);
var
  Dep:     ^TDependency;
  DepPath: string;
  i:       integer;
  function IfThen(Cond: Boolean; const s: string): string;
  begin
    if Cond then
      Result := s
    else
      Result := '';
  end;
begin
  if Pkg.DidResolveDeps then
    exit;

  Pkg.DidResolveDeps := True;

  for i := low(Pkg.Dependencies) to high(Pkg.Dependencies) do
  begin
    Dep := @Pkg.Dependencies[i];

    DepPath := LookupGlobalPackage(Dep^.Name);
    if (Dep^.Prefer) or (DepPath = '') then
      DepPath := Dep^.Path;

    if DepPath = '' then
    begin
      DebugLog('  Dependency %s: not found', [Dep^.Name]);
      continue;
    end;

    DebugLog(
      '  Dependency: %s -> %s%s',
      [Dep^.Name, DepPath, IfThen(DepPath = Dep^.Path, ' (hardcoded)')]
    );

    Dep^.Package := GetPackageOrProject(DepPath);

    // Add ourselves to the RequiredBy list of the dependency.
    SetLength(Dep^.Package.RequiredBy, Length(Dep^.Package.RequiredBy) + 1);
    Dep^.Package.RequiredBy[High(Dep^.Package.RequiredBy)] := Pkg;

    // Recurse
    ResolveDeps(Dep^.Package);
  end;
end;

// Try to fix missing dependencies.
//
// Consider the following scenario:
//
//   A requires: 
//     - B (found) 
//     - C (NOT found)
//   B requires:
//     - C (found)
//
// In other words, we could not find C for A, but did find C for B. (The
// reason for this might be that B specified a default or preferred path for
// dependency C). In this case we resolve the situation by using B's C also
// for A.
procedure GuessMissingDependencies(Pkg: TPackage);
var
  Dep: ^TDependency;
  i:   Integer;

  // Breadth-first search for a package of the specified name in the
  // dependencies of Node.
  function GuessDependency(Node: TPackage; DepName: String): TPackage;
  var
    j: integer;
  begin
    Result := nil;

    if Node.Visited then
      exit;

    Node.Visited := True;
    try
      for j := low(Node.Dependencies) to high(Node.Dependencies) do
      begin
        if (UpperCase(DepName) = UpperCase(Node.Dependencies[j].Name)) and
           Assigned(Node.Dependencies[j].Package) then
        begin
          Result := Node.Dependencies[j].Package;
          exit;
        end;
      end;

      // Not found, recurse
      for j := low(Node.RequiredBy) to high(Node.RequiredBy) do
      begin
        Result := GuessDependency(Node.RequiredBy[j], DepName);
        if Assigned(Result) then
          exit;
      end;

    finally
      Node.Visited := False;
    end;
  end;
begin
  for i := low(Pkg.Dependencies) to high(Pkg.Dependencies) do
  begin
    Dep := @Pkg.Dependencies[i];
    if Assigned(Dep^.Package) then
      continue;

    Dep^.Package := GuessDependency(Pkg, Dep^.Name);
  end;
end;

// Add the search paths of its dependencies to a package.
procedure ResolvePaths(Pkg: TPackage);
var
  Dep: TDependency;
begin
  if Pkg.DidResolvePaths then
    exit;

  Pkg.DidResolvePaths := True;

  Pkg.ResolvedPaths := Pkg.Paths;

  for Dep in Pkg.Dependencies do
  begin
    if not Assigned(Dep.Package) then
      continue;

    // Recurse
    ResolvePaths(Dep.Package);

    Pkg.ResolvedPaths.IncludePath := MergePaths([
      Pkg.ResolvedPaths.IncludePath{,
      Dep.Package.ResolvedPaths.IncludePath}
    ]);
    Pkg.ResolvedPaths.UnitPath := MergePaths([
      Pkg.ResolvedPaths.UnitPath,
      Dep.Package.ResolvedPaths.UnitPath
    ]);
    Pkg.ResolvedPaths.SrcPath := MergePaths([
      Pkg.ResolvedPaths.SrcPath{,
      Dep.Package.ResolvedPaths.SrcPath}
    ]);
  end;
end;

// Add required search paths to package's source directories (and their
// subdirectories).
procedure ConfigurePackage(Pkg: TPackage);
var
  Dep:      TDependency;
  OtherSrc: TStringArray;
  OtherDir: string;

  procedure ConfigureSearchPath(const Dir: string);
  var
    DirectoryTemplate,
    IncludeTemplate,
    UnitPathTemplate,
    SrcTemplate:       TDefineTemplate;
    Paths:             TPaths;
  begin
    DirectoryTemplate := TDefineTemplate.Create(
      'Directory', '',
      '', Dir,
      da_Directory
    );

    Paths.UnitPath    := MergePaths([UnitPathMacro,    Pkg.ResolvedPaths.UnitPath]);
    Paths.IncludePath := MergePaths([IncludePathMacro, Pkg.ResolvedPaths.IncludePath]);
    Paths.SrcPath     := MergePaths([SrcPathMacro,     Pkg.ResolvedPaths.SrcPath]);

    DebugLog('%s', [Dir]);
    DebugLog('  UnitPath:    %s', [Paths.UnitPath]);
    DebugLog('  IncludePath: %s', [Paths.IncludePath]);
    DebugLog('  SrcPath:     %s', [Paths.SrcPath]);

    UnitPathTemplate := TDefineTemplate.Create(
      'Add to the UnitPath', '',
      UnitPathMacroName, Paths.UnitPath,
      da_DefineRecurse
    );

    IncludeTemplate := TDefineTemplate.Create(
      'Add to the Include path', '',
      IncludePathMacroName, Paths.IncludePath,
      da_DefineRecurse
    );

    SrcTemplate := TDefineTemplate.Create(
      'Add to the Src path', '',
      SrcPathMacroName, Paths.SrcPath,
      da_DefineRecurse
    );

    DirectoryTemplate.AddChild(UnitPathTemplate);
    DirectoryTemplate.AddChild(IncludeTemplate);
    DirectoryTemplate.AddChild(SrcTemplate);

    CodeToolBoss.DefineTree.Add(DirectoryTemplate);
  end;
begin
  if Pkg.Configured then
    exit;
  Pkg.Configured := True;
 
  // Configure search path for package's (or project's) main source directory.
  ConfigureSearchPath(Pkg.Dir);

  // Configure search path for other listed source directories.
  OtherSrc := Pkg.Paths.SrcPath.Split([';'], TStringSplitOptions.ExcludeEmpty);
  for OtherDir in OtherSrc do
    ConfigureSearchPath(OtherDir);

  // Recurse
  for Dep in Pkg.Dependencies do
  begin
    if not Assigned(Dep.Package) then
      continue;
    ConfigurePackage(Dep.Package);
  end;
end;

// Don't load packages from directories with these names...
function IgnoreDirectory(const Dir: string): Boolean;
var
  DirName: string;
begin
  Dirname := lowercase(ExtractFileName(Dir));
  Result := 
    (DirName = '.git')                              or 
    ((Length(DirName) >= 1) and (DirName[1] = '.')) or
    (DirName = 'backup')                            or 
    (DirName = 'lib')                               or 
    (Pos('.dsym', DirName) > 0)                     or
    (Pos('.app', DirName) > 0);
end;

// Load all packages in a directory and its subdirectories.
procedure LoadAllPackagesUnderPath(const Dir: string);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;     
  Pkg:               TPackage;
begin
  if IgnoreDirectory(Dir) then
    Exit;

  try
    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ResolveDeps(Pkg);
    end;

    // Recurse into child directories

    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      LoadAllPackagesUnderPath(SubDirectories[i]);

  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

// Given a directory, fix missing deps for all packages in the directory.
procedure GuessMissingDepsForAllPackages(const Dir: string);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;
  Pkg:               TPackage;
begin
  if IgnoreDirectory(Dir) then
    Exit;

  try
    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      GuessMissingDependencies(Pkg);
    end;

    // Recurse into child directories
    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      GuessMissingDepsForAllPackages(SubDirectories[i]);

  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

// Use heuristic to add search paths to the directory 'Dir'.
// If there are any projects (.lpi) or packages (.lpk) in the directory, use
// (only) their search paths. Otherwise, inherit the search paths from the
// parent directory ('ParentPaths').
procedure ConfigurePaths(const Dir: string);
var
  Packages,
  SubDirectories:    TStringList;
  i:                 integer;
  Paths:             TPaths;

  DirectoryTemplate,
  IncludeTemplate,
  UnitPathTemplate,
  SrcTemplate:       TDefineTemplate;
  Pkg:               TPackage;

begin
  if IgnoreDirectory(Dir) then
    Exit;

  Packages       := nil;
  SubDirectories := nil;
  try
    // 1. Add local files to search path of current directory
    DirectoryTemplate := TDefineTemplate.Create(
      'Directory', '',
      '', Dir,
      da_Directory
    );
    UnitPathTemplate := TDefineTemplate.Create(
      'Add to the UnitPath', '',
      UnitPathMacroName, MergePaths([UnitPathMacro, Dir]),
      da_Define
    );
    IncludeTemplate := TDefineTemplate.Create(
      'Add to the Include path', '',
      IncludePathMacroName, MergePaths([IncludePathMacro, Dir]),
      da_Define
    );
    DirectoryTemplate.AddChild(UnitPathTemplate);
    DirectoryTemplate.AddChild(IncludeTemplate);
    CodeToolBoss.DefineTree.Add(DirectoryTemplate);

    // 2. Load all packages in the current directory and configure their
    //    paths.
    Packages := FindAllFiles(
      Dir, '*.lpi;*.lpk', False, faAnyFile and not faDirectory
    );

    // 2a. Recursively resolve search paths for each package.
    //     (Merge dependencies' search paths into own search path)
    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ResolvePaths(Pkg);
    end;

    // 2b. For each package in the dependency tree, apply the package's
    //     resulting search paths from the previous step to the package's source
    //     directories. ("apply" = add to the CodeTools Define Tree)
    for i := 0 to Packages.Count - 1 do
    begin
      Pkg := GetPackageOrProject(Packages[i]);
      ConfigurePackage(Pkg);
    end;

    // Recurse into child directories
    SubDirectories := FindAllDirectories(Dir, False);
    for i := 0 to SubDirectories.Count - 1 do
      ConfigurePaths(SubDirectories[i]);
  finally
    if Assigned(Packages) then
      FreeAndNil(Packages);
    if Assigned(Packages) then
      FreeAndNil(SubDirectories);
  end;
end;

// CodeTools needs to know the paths for the global packages, the FPC source
// files, the path of the compiler and the target architecture.
// Attempt to guess the correct settings from Lazarus config files.
procedure GuessCodeToolConfig(Options: TCodeToolsOptions);
var
  ConfigDirs:         TStringList;
  Dir:                string;
  Doc:                TXMLDocument;

  Root,
  EnvironmentOptions, 
  FPCConfigs, 
  Item1:              TDomNode;

  LazarusDirectory, 
  FPCSourceDirectory, 
  CompilerFilename, 
  OS, CPU:            string;

  function LoadLazConfig(Path: string): Boolean;
  begin
    Doc    := nil;
    Root   := nil;
    Result := false;
    try
      ReadXMLFile(Doc, Path);
      Root := Doc.DocumentElement;
      if Root.NodeName = 'CONFIG' then
        Result := true;
      DebugLog('Reading config from %s', [Path]);
    except
      // Swallow
    end;
  end;

  function GetVal(Parent: TDomNode; Ident: string; Attr: string='Value'): string;
  var
    Node, Value: TDomNode;
  begin
    Result := '';
    if Parent = nil then
      exit;
    Node := Parent.FindNode(DOMString(Ident));
    if Node = nil then
      exit;
    Value := Node.Attributes.GetNamedItem(DOMString(Attr));
    if Value = nil then
      exit;
    Result := string(Value.NodeValue);
  end;
begin
  ConfigDirs := TStringList.Create;
  try
    ConfigDirs.Add(GetConfigDirForApp('lazarus', '', False));
    ConfigDirs.Add(GetUserDir + DirectorySeparator + '.lazarus');
    ConfigDirs.Add(GetConfigDirForApp('lazarus', '', True));  ;
    for Dir in ConfigDirs do
    begin
      Doc := nil;
      try
        if LoadLazConfig(Dir + DirectorySeparator + 'environmentoptions.xml') then
        begin
          EnvironmentOptions := Root.FindNode('EnvironmentOptions');
          LazarusDirectory   := GetVal(EnvironmentOptions, 'LazarusDirectory');
          FPCSourceDirectory := GetVal(EnvironmentOptions, 'FPCSourceDirectory');
          CompilerFilename   := GetVal(EnvironmentOptions, 'CompilerFilename');
          if (Options.LazarusSrcDir = '') and (LazarusDirectory <> '') then
            Options.LazarusSrcDir := LazarusDirectory;
          if (Options.FPCSrcDir = '') and (FPCSourceDirectory <> '') then
            Options.FPCSrcDir := FPCSourceDirectory;
          if (Options.FPCPath = '') and (CompilerFilename <> '') then
            Options.FPCPath := CompilerFilename;
        end;
      finally
        FreeAndNil(Doc);
      end;

      Doc := nil;
      try
        if LoadLazConfig(Dir + DirectorySeparator + 'fpcdefines.xml') then
        begin
          FPCConfigs := Root.FindNode('FPCConfigs');
          Item1 := nil;
          if Assigned(FPCConfigs) and (FPCConfigs.ChildNodes.Count > 0) then
            Item1 := FPCConfigs.ChildNodes[0];
          OS  := GetVal(Item1, 'RealCompiler', 'OS');
          CPU := GetVal(Item1, 'RealCompiler', 'CPU');
          if (Options.TargetOS = '') and (OS <> '') then
            Options.TargetOS := OS;
          if (Options.TargetProcessor = '') and (CPU <> '') then
            Options.TargetProcessor := CPU;
        end;
      finally
        FreeAndNil(Doc);
      end;
    end;
  finally
    FreeAndNil(ConfigDirs);
  end;
end;

initialization
  InitLog(TIOStream.Create(iosError));

  PkgNameToPath := TFPStringHashTable.Create;
  PkgCache      := TFPObjectHashTable.Create;
end.