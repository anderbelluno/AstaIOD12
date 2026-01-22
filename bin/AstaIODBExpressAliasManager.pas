{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10127: AstaIODBExpressAliasManager.pas 
{
{   Rev 1.0    4/10/2003 6:30:42 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:06 PM  Steve    Version: 1.505
}
unit AstaIODBExpressAliasManager;

interface

uses

  {$ifdef mswindows}Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, DBCtrls, DBXpress, DB, SqlExpr, AstaIOCustomDataSet,
  {$else}
  QDBCtrls, QControls, QGrids, QDBGrids,QForms,QStdCtrls,QDialogs,
  {$endif}
   DBXpress, DB, SqlExpr, AstaIOCustomDataSet;

type
  TAstaIODBExpressForm = class(TForm)
    AliasDataSet: TAstaIODataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    AliasDataSetAlias: TStringField;
    AliasDataSetDatabase: TStringField;
    AliasDataSetUserName: TStringField;
    AliasDataSetPassword: TStringField;
    AliasDataSetBase: TMemoField;
    Button1: TButton;
    DBComboBox1: TDBComboBox;
    GroupBox1: TGroupBox;
    DBMemo1: TDBMemo;
    SQLConn: TSQLConnection;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


Function DbexpressRuntimeConfigure(Alias:String;SQLConn:TSQLConnection;ConfigFile:String):Boolean;overload;
Function DbexpressRuntimeConfigure(SQLConn:TSQLConnection;D:TAstaIODataSet):Boolean;overload;
Function DbexpressRuntimeConfigure(SQLConn:TSQLConnection;Driver,UserName,Password,Base:String):Boolean;overload;
procedure DBExpressAlias(ConfigFile:String);
implementation
uses AstaIOUtil,SysUtils;
{$R *.dfm}
procedure DBExpressAlias(ConfigFile:String);
var
  AstaIODBExpressForm: TAstaIODBExpressForm;
begin
  AstaIODBExpressForm:=TAstaIODBExpressForm.Create(nil);
  if ConfigFile<>'' then AstaIODBExpressForm.AliasDataSet.FileName:=configFile;
  if FileExists(AstaIODBExpressForm.AliasDataSet.FileName) then
   AstaIODBExpressForm.AliasDataSet.LoadFromFile;
  AstaIODBExpressForm.ShowModal;
  AstaIODBExpressForm.Free;
end;

Function DbexpressRuntimeConfigure(Alias:String;SQLConn:TSQLConnection;ConfigFile:String):Boolean;
var
 D:TAstaIODataSet;
begin
 result:=False;
 if not FileExists(ConfigFile) then Raise Exception.create(ConfigFile+' not found');
 d:=TAstaIODataSet.Create(nil);
 try
  D.LoadFromFile(ConfigFile);
  if D.Locate('Alias',Alias,[loCaseInsensitive]) then
   result:=DbexpressRunTimeConfigure(SQLConn,D);
  finally
   d.free;
 end;
end;

Function DbexpressRuntimeConfigure(SQLConn:TSQLConnection;D:TAstaIODataSet):Boolean;
begin
 result:=False;
 if D.Recordcount=0 then Raise Exception.Create('No Information Available!');
 with D do
 result:=DbexpressRuntimeConfigure(SQLConn,
  fieldByName('DataBase').AsString,FieldbyName('UserName').AsString,
  FieldbyName('Password').AsString,FieldByname('Base').AsString);

end;

Function DbexpressRuntimeConfigure(SQLConn:TSQLConnection;Driver,UserName,Password,Base:String):Boolean;
begin
 result:=False;
 if instring('Oracle',Driver) then
  begin
    SQLConn.ConnectionName:='Oracle';
    SQLConn.DriverName:='Oracle';
    SQLConn.GetDriverFunc:='getSQLDriverORACLE';
    SQLConn.LibraryName:='dbexpora.dll';
    SQLConn.VendorLIB:='OCI.DLL';
  end else
  if instring('Interbase',Driver) then
  begin  // Interbase
    SQLConn.ConnectionName:='Interbase';
    SQLConn.DriverName:='Interbase';
    SQLConn.GetDriverFunc:='getSQLDriverINTERBASE';
    {$ifdef mswindwos}
    SQLConn.LibraryName:='dbexpint.dll';
    SQLConn.VendorLIB:='GDS32.DLL';
    {$else}
    SQLConn.LibraryName:='libsqlib.so.1';
    SQLConn.VendorLIB:='libgds.so.0';
    {$endif}
  end else raise Exception.Create('Only Interbase and Oracle so far!');
    with SQLConn do
    begin
     Params.Values['User_Name']:=UserName;
     Params.Values['Database']:=Base;
     Params.Values['Password']:=PassWord;
    end;
   Result:=True;
end;

procedure TAstaIODBExpressForm.Button2Click(Sender: TObject);
begin
 AliasDataSet.SaveToFile;
end;


procedure TAstaIODBExpressForm.Button1Click(Sender: TObject);
begin
if AliasDataSet.Recordcount=0 then Raise Exception.Create('Please add an Alais!');
if DbexpressRuntimeConfigure(SQLConn,AliasDataSet) then begin
 SQLConn.Connected:=True;
 ShowMessage('connected');
end;

end;

procedure TAstaIODBExpressForm.FormCreate(Sender: TObject);
begin
 AliasDataSet.LoadFromFile;
end;

end.
