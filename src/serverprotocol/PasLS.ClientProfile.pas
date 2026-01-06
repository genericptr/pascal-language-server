// Pascal Language Server
// Copyright 2020 Ryan Joseph

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
unit PasLS.ClientProfile;

{$mode objfpc}{$H+}

interface

uses
  { RTL }
  Classes, SysUtils, Contnrs;

type
  { Feature flags for client-specific behaviors }
  TClientFeature = (
    cfFlatSymbolMode,              // Force flat symbol mode (SymbolInformation[])
    cfExcludeSectionContainers,    // Don't include interface/implementation section containers
    cfExcludeInterfaceMethodDecls, // Don't include method/function/procedure declarations from interface section
    cfExcludeImplClassDefs,        // Don't include class definitions from implementation section
    cfNullDocumentVersion,         // Use nil instead of 0 for document version
    cfFilterTextOnly,              // Only set filterText in completion, not label
    cfUseSecondaryText             // Use secondaryText field in completion items  
  );
  TClientFeatures = set of TClientFeature;

  { Client profile class }
  TClientProfile = class
  private
    FName: string;
    FFeatures: TClientFeatures;
    class var FCurrent: TClientProfile;
    class var FDefault: TClientProfile;
    class var FRegistry: TFPHashObjectList;
  public
    constructor Create(const AName: string; AFeatures: TClientFeatures);

    { Feature query - primary API }
    function HasFeature(F: TClientFeature): Boolean; inline;

    { Properties }
    property Name: string read FName;
    property Features: TClientFeatures read FFeatures write FFeatures;

    { Class methods for profile management }
    class procedure RegisterProfile(Profile: TClientProfile);
    class procedure SelectProfile(const ClientName: string);
    class procedure ApplyOverrides(EnableFeatures, DisableFeatures: TStrings);
    class function Current: TClientProfile;
    class procedure Finalize;
  end;

{ Helper function for string-to-feature conversion }
function TryStrToFeature(const S: string; out F: TClientFeature): Boolean;
function FeatureToStr(F: TClientFeature): string;

implementation

const
  { Feature name mapping for configuration }
  FeatureNames: array[TClientFeature] of string = (
    'flatSymbolMode',
    'excludeSectionContainers',
    'excludeInterfaceMethodDecls',
    'excludeImplClassDefs',
    'nullDocumentVersion',
    'filterTextOnly',
    'useSecondaryText'  
  );

function TryStrToFeature(const S: string; out F: TClientFeature): Boolean;
var
  I: TClientFeature;
begin
  Result := False;
  for I := Low(TClientFeature) to High(TClientFeature) do
    if SameText(S, FeatureNames[I]) then
    begin
      F := I;
      Exit(True);
    end;
end;

function FeatureToStr(F: TClientFeature): string;
begin
  Result := FeatureNames[F];
end;

{ TClientProfile }

constructor TClientProfile.Create(const AName: string; AFeatures: TClientFeatures);
begin
  inherited Create;
  FName := AName;
  FFeatures := AFeatures;
end;

function TClientProfile.HasFeature(F: TClientFeature): Boolean;
begin
  Result := F in FFeatures;
end;

class procedure TClientProfile.RegisterProfile(Profile: TClientProfile);
begin
  if FRegistry = nil then
    FRegistry := TFPHashObjectList.Create(True);  // Owns objects
  FRegistry.Add(Profile.Name, Profile);
end;

class procedure TClientProfile.SelectProfile(const ClientName: string);
begin
  // Free previous copied profile if exists (not in registry and not FDefault)
  if (FCurrent <> nil) and (FCurrent <> FDefault) then
    if (FRegistry = nil) or (FRegistry.Find(FCurrent.Name) <> FCurrent) then
      FreeAndNil(FCurrent);

  FCurrent := nil;
  if FRegistry <> nil then
    FCurrent := TClientProfile(FRegistry.Find(ClientName));
  if FCurrent = nil then
    FCurrent := FDefault;
end;

class procedure TClientProfile.ApplyOverrides(EnableFeatures, DisableFeatures: TStrings);
var
  I: Integer;
  Feature: TClientFeature;
  OriginalProfile: TClientProfile;
  OldCurrent: TClientProfile;
begin
  if FCurrent = nil then Exit;

  // Check if we need to create a mutable copy
  OriginalProfile := nil;
  if FRegistry <> nil then
    OriginalProfile := TClientProfile(FRegistry.Find(FCurrent.Name));

  // Need to create a copy if:
  // - FCurrent is the registered profile (OriginalProfile = FCurrent), OR
  // - FCurrent is FDefault (check by object identity, not name lookup)
  if (FCurrent = FDefault) or (FCurrent = OriginalProfile) then
  begin
    OldCurrent := FCurrent;
    // Create a copy so we don't modify the registered or default profile
    FCurrent := TClientProfile.Create(OldCurrent.Name, OldCurrent.Features);
    // Note: We don't free OldCurrent here as it's owned by registry or is FDefault
  end;

  // Apply enable overrides
  if EnableFeatures <> nil then
    for I := 0 to EnableFeatures.Count - 1 do
      if TryStrToFeature(EnableFeatures[I], Feature) then
        Include(FCurrent.FFeatures, Feature);

  // Apply disable overrides (takes precedence)
  if DisableFeatures <> nil then
    for I := 0 to DisableFeatures.Count - 1 do
      if TryStrToFeature(DisableFeatures[I], Feature) then
        Exclude(FCurrent.FFeatures, Feature);
end;

class function TClientProfile.Current: TClientProfile;
begin
  if FCurrent = nil then
    FCurrent := FDefault;
  Result := FCurrent;
end;

class procedure TClientProfile.Finalize;
begin
  // Free FCurrent if it's a copy (not in registry and not FDefault)
  if (FCurrent <> nil) and (FCurrent <> FDefault) then
  begin
    if (FRegistry = nil) or (FRegistry.Find(FCurrent.Name) <> FCurrent) then
      FreeAndNil(FCurrent);
  end;
  FCurrent := nil;
  FreeAndNil(FRegistry);
  FreeAndNil(FDefault);
end;

initialization
  // Create default profile (LSP-compliant, no special behaviors)
  TClientProfile.FDefault := TClientProfile.Create('Default', []);

  // Register Sublime Text LSP profile
  TClientProfile.RegisterProfile(
    TClientProfile.Create('Sublime Text LSP', [
      cfFlatSymbolMode,
      cfExcludeSectionContainers,
      cfExcludeInterfaceMethodDecls,
      cfExcludeImplClassDefs,
      cfNullDocumentVersion,
      cfFilterTextOnly
    ]));

  // Register VS Code profile (uses LSP defaults)
  TClientProfile.RegisterProfile(
    TClientProfile.Create('vscode', []));

finalization
  TClientProfile.Finalize;

end.
