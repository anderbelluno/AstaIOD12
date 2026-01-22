{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10329: AstaIOStatusBar.pas 
{
{   Rev 1.0    4/10/2003 6:32:22 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:20 PM  Steve    Version: 1.505
}
unit AstaIOStatusBar;
{*********************************************************}
{*     Copyright (c) 1997-2002 Asta Technology Group INC   *}
{*                 All rights reserved.                  *}
{*                 www.astatech.com                      *}
{*********************************************************}
{.$D+,L+}

{$I AstaIO.inc}

interface

uses
  SysUtils, Classes,AstaIOConst,AstaIOClientWire,
  {$IFDEF LINUX}
  QControls, QComCtrls;
  {$ELSE}
  Windows, Messages,Controls, ComCtrls;
  {$ENDIF}

type
  TAstaIOStatusBar = class(TStatusBar)
  private
    { Private declarations }
    FClientWire:TAstaIOClientWire;
    FAbout:String;
    FCaption:String;
  protected
    { Protected declarations}
    Procedure SetClientWire(Value:TAstaIOClientWire);
    Function GetClientWire:TAstaIOClientWire;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StatusBarChange(var Msg: TMessage); message WM_Status_BAR;
  published
    { Published declarations }
    property ClientWire:TAstaIOClientWire read GetClientWire write SetClientWire;
    property About:String read FAbout write FAbout;
    property Caption:String read FCaption write FCaption;
  end;


Var
  AstaStatusBarIOCaption :String;
implementation

constructor TAstaIOStatusBar.Create(AOwner: TComponent);
var
  BRSize, BLSize, X : Integer; // Bottom Right & Bottom Left panel sizes
begin
  inherited Create(AOwner);
  FClientWire:=nil;
  For X := 0 to 2 do
   Panels.Add;

  BLSize := 175; // size of the bottom left "Connect", "Disconnected" panel
  BRSize := 175;

  if (AOwner is TWinControl) AND (TWinControl(AOwner).Width > BLSize + BRSize)then begin
    Panels.Items[0].Width := BLSize;
    Panels.Items[1].Width := TWinControl(AOwner).Width - BLSize - BRSize;
    Panels.Items[2].Width := BRSize;
    if FCaption=''  then  Panels.Items[2].Text := AstaStatusBarIOCaption
     else Panels.Items[2].Text := FCaption
  end;
end;

procedure TAstaIOStatusBar.StatusBarChange(var Msg: TMessage);
begin
  {$ifdef Linux}
  Case Msg.Msg of 
  {$else}
  Case Msg.Lparam of
  {$endif}
    1: if SimplePanel then SimpleText := 'Connected' else
      Panels.Items[0].Text := 'Connected';
    2: if SimplePanel then SimpleText := 'Disconnected' else
        Panels.Items[0].Text := 'Disconnected';
  end;
end;

destructor TAstaIOStatusBar.Destroy;
begin
 // if (FClientWire<>nil)  then FClientwire.UnRegisterHandle(handle);
  ClientWire:=nil;
  inherited Destroy;
end;

Procedure TAstaIOStatusBar.SetClientWire(Value:TAstaIOClientWire);
begin
  FClientWire:=Value;
  if FClientWire <> nil then
    FClientWire.FreeNotification(Self);
 {$ifndef Linux}
  {$ifdef ActiveStatusBar}
    if FClientWire<>nil then FClientwire.RegisterHandle(Handle);
  {$endif}
 {$endif}
end;


Function TAstaIOStatusBar.GetClientWire:TAstaIOClientWire;
begin
 result:=FClientWire;
end;

procedure TAstaIOStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then  begin
    if (AComponent = FClientWire) then
      FClientWire := nil;
  end;
  if Operation = opInsert then begin
    if (AComponent is TAstaIOClientWire) and (FClientWire = nil) then
      FClientWire := AComponent as TAstaIOClientWire;
  end;
end;


initialization
  AstaStatusBarIOCaption := ' ASTA Technology Group ';

end.


