{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10578: client_log.pas 
{
{   Rev 1.0    4/10/2003 6:34:56 AM  Steve
}
unit client_log;

interface

uses
  AstaIOMessagePacker, AstaIONativeClientMsg,
  AstaIOConst, AstaIOClientMsgWire,
  Classes, SysUtils,AstaIOLowCore,
  {$ifdef Linux}
   QComCtrls, QButtons, QStdCtrls, QControls, QExtCtrls,QForms,
  {$else}
   ComCtrls, Buttons, StdCtrls, Controls,ExtCtrls,Forms,CheckLst, Menus,
  AstaIOStatusBar
  {$endif}
  ;

type
  TForm1 = class(TForm)
    MemoOut: TMemo;
    MainMenu1: TMainMenu;
    Options1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Connect1: TMenuItem;
    GroupBox1: TGroupBox;
    UserCheckBox: TCheckListBox;
    TestCodedMessage1: TMenuItem;
    Clientwire: TAstaIOClientLeanWire;
    StatusBar: TStatusBar;
    procedure ConnectButtonClick(Sender: TObject);
    procedure ClientWireCodedMessage(Sender: TObject; MsgID: Integer;
      Msg: String);
    procedure ClientWireConnect(Sender: TObject);
    procedure ClientWireDisconnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure TestCodedMessage1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
   procedure Logit(Msg:String);
  end;

var
  Form1: TForm1;

implementation
uses AstaIOutil;

{$R *.dfm}

procedure TForm1.Logit(Msg:String);
begin
 Memoout.lines.add(Msg);
end;


procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  ClientWire.UserName:='admin';
  ClientWire.Active:=Not Clientwire.Active;
end;

procedure TForm1.ClientWireCodedMessage(Sender: TObject; MsgID: Integer;
  Msg: String);
var
s:TStringList;
i:Integer;
begin
  with UsercheckBox do
  case msgid of
   adminLogDisconnect:items.delete(items.indexof(msg));
   adminLoginSuccess:items.add(Msg);
   AdminLogUserList:begin
          s:=TStringList.Create;
          s.text:=Msg;
          try
           for i:=0 to s.count-1 do
            items.add(s[i]);
           finally
           s.free;
          end;
        end;
   adminlogClientError:Logit('[ClientError]'+Msg);
  else LogIt(Msg);
  end
end;

procedure TForm1.ClientWireConnect(Sender: TObject);
begin
  StatusBar.SimpleText:='Connected '+ClientWire.Address;
  Connect1.Caption:='&Disconnect';
end;

procedure TForm1.ClientWireDisconnect(Sender: TObject);
begin
  statusBar.SimpleText:='Disconnected';
  Connect1.Caption:='&Connect';
  UserCheckBox.Items.Clear;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  with ClientWire do
   MessageFlags:=MessageFlags+[itLogClient];

end;


procedure TForm1.Exit1Click(Sender: TObject);
begin
 close;
end;

procedure TForm1.TestCodedMessage1Click(Sender: TObject);
begin
 ClientWire.SendCodedMessage(1000,'test');
end;

end.


