unit PasLS.JSONSignaledQueue;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpjson, fgl, syncobjs, Classes;

type
  TJSONSignaledQueue = class
  private
    FList: specialize TFPGObjectList<TJSONData>;
    FLock: TCriticalSection;
    FReadyEvent: TEvent; // Event used to signal new data inQueue
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Enqueue(AItem: TJSONData);
    { DequeueWaiting blocks the calling thread until an item is available 
      or the timeout expires. Returns True if an item was successfully retrieved. }
    function DequeueWaiting(out AItem: TJSONData; TimeoutMS: Cardinal = INFINITE): Boolean;
    procedure SignalShutdown;
  end;

implementation

constructor TJSONSignaledQueue.Create;
begin
  inherited Create;
  // FList owns request JSON Data
  FList := specialize TFPGObjectList<TJSONData>.Create(True);
  FLock := TCriticalSection.Create;
  // ManualReset = False (auto-resets when a thread wakes up)
  // InitialState = False (starts unsignaled)
  FReadyEvent := TEvent.Create(nil, False, False, '');
end;

destructor TJSONSignaledQueue.Destroy;
begin
  FList.Free;
  FReadyEvent.Free;
  FLock.Free;
  inherited Destroy;
end;

procedure TJSONSignaledQueue.Enqueue(AItem: TJSONData);
var
  I: Integer;
  JSONObj: TJSONObject;
begin
  FLock.Acquire;
  try
    if AItem is TJSONObject then
      begin
        JSONObj := (AItem as TJSONObject);
        if JSONObj.Get('method', '') = 'textDocument/didChange' then
          begin
            for I := 0 to FList.Count - 1 do
              begin
                if Flist[I] is TJsonObject then
                  begin
                    if (Flist[I] as TJSONObject).Get('method', '') = 'textDocument/didChange' then
                      begin
                        FList.Remove(FList[I]);
                        break
                      end;
                  end;
              end;
          end;
      end;    
      FList.Add(AItem);
  finally
    FLock.Release;
  end;
  // Signal DequeueWaiting to resume waiting
  FReadyEvent.SetEvent; 
end;

function TJSONSignaledQueue.DequeueWaiting(out AItem: TJSONData; TimeoutMS: Cardinal): Boolean;
var
  WaitResult: TWaitResult;
begin
  AItem := nil;
  Result := False;

  while True do
  begin
    FLock.Acquire;
    try
      if FList.Count > 0 then
      begin
        AItem := FList[0];
        // Effectively transfers ownership to AItem
        FList.Extract(AItem);
        Result := True;
      end;
    finally
      FLock.Release;
    end;
    if Result then
      Exit; // Successfully retrieved an item

    // Queue was empty; sleep efficiently until an item is pushed or timeout occurs
    WaitResult := FReadyEvent.WaitFor(TimeoutMS);
    
    // If it timed out or the event was explicitly reset/abandoned, exit gracefully
    if WaitResult <> wrSignaled then
      Exit;
  end;
end;

procedure TJSONSignaledQueue.SignalShutdown;
begin
  // Broadcast to all waiting threads so they wake up and check terminate status
  FReadyEvent.SetEvent; 
end;

end.