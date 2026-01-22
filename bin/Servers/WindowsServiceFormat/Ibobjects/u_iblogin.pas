unit u_iblogin;

interface

uses
  Classes, Forms, StdCtrls, DBCtrls, Db, Grids, DBGrids, Buttons, ExtCtrls,
  Dialogs, Controls, Registry, SysUtils, Windows, AstaIOCustomDataSet;

const
  IBRegKey = 'Software\Borland\Interbase\IBConsole\Servers\Local Server\Databases';
  AstaIbServers = 'AstaIbOServer.dat';

type
  Tf_ibconnect = class(TForm)
    ds_alias: TDataSource;
    DBGrid1: TDBGrid;
    m_databasefile: TDBMemo;
    btn_insert: TButton;
    btn_delete: TButton;
    btn_update: TButton;
    e_database: TEdit;
    e_username: TEdit;
    e_password: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btn_browse: TButton;
    OpenDialog: TOpenDialog;
    Bevel1: TBevel;
    btn_ok: TBitBtn;
    btn_cancel: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    ServerEdit: TEdit;
    EncryptPWchkbox: TCheckBox;
    Ads_alias: TAstaIODataSet;
    Ads_aliasAlias: TStringField;
    Ads_aliasDataBaseFile: TMemoField;
    procedure btn_browseClick(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btn_updateClick(Sender: TObject);
    procedure btn_deleteClick(Sender: TObject);
    procedure btn_insertClick(Sender: TObject);
    procedure ads_aliasAfterScroll(DataSet: TDataSet);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    FDatabase   :String;
    FUserName   :String;
    FPassword   :String;
    FEncryptPassword : Boolean;
    procedure FindDatabase(GDBFile:String);
  end;


Function IBDatabase(Var Server,GDBFile,UserName,Password:String; var EncryptPW : boolean):Boolean;
implementation
{$R *.DFM}
Function IBDatabase(Var Server,GDBFile,UserName,Password:String; var EncryptPW : boolean):Boolean;
var
  f_ibconnect: Tf_ibconnect;
begin
  result:=False;
  f_ibconnect:=Tf_ibconnect.Create(nil);
  f_ibconnect.FindDatabase(GDBFile);
  try
  f_ibconnect.ShowModal;
  if f_ibconnect.Modalresult=mrok then begin
   result:=True;
   Server:=f_ibconnect.ServerEdit.Text;
   GDBFile:=f_ibconnect.e_Database.Text;
   UserName:=f_ibconnect.FUserName;
   Password:=f_ibconnect.FPassWord;
   EncryptPW := f_ibconnect.FEncryptPassword;
  end;
  finally
   f_ibconnect.Free;
 end;
end;

procedure Tf_ibconnect.btn_browseClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  e_database.Text:=OpenDialog.FileName;
end;

procedure Tf_ibconnect.btn_okClick(Sender: TObject);
begin
  ads_alias.SaveToFile(AstaIbServers);
  FDatabase:=e_database.Text;
  FUserName:=e_username.Text;
  FPassword:=e_password.Text;
  FEncryptPassword := EncryptPWchkbox.Checked;
end;

procedure Tf_ibconnect.FindDatabase(GDBFile:String);
begin
 if ads_alias.RecordCount=0 then exit;
 ads_alias.First;
 while not ads_alias.eof  do begin
  if comparetext(gdbfile,ads_alias.FieldByName('databaseFile').AsString)=0
    then exit;
  ads_alias.next;
 end;
 ads_alias.First;

end;

procedure Tf_ibconnect.FormCreate(Sender: TObject);
var Reg             :TRegIniFile;
    SubKey          :String;
    L               :TStringList;
    i               :Integer;
    DataBaseFiles   :String;
begin
  if FileExists(AstaIbServers) then
    ads_alias.LoadFromFile(AstaIbServers);
  Reg:=TRegIniFile.Create('');
  try
    Reg.RootKey:=HKEY_CURRENT_USER;
    Reg.OpenKey(IBRegKey, TRUE);
    l:=TStringList.Create;
    Reg.GetKeyNames(l);
    for i:=0 to l.count-1 do
    begin
      SubKey:=l[i];
      DatabaseFiles:=Trim(Reg.ReadString(SubKey, 'DatabaseFiles', ''));
      FUsername:=Reg.ReadString(SubKey, 'Username', 'SYSDBA');
      if not ads_alias.Locate('Alias', l[i], []) then
      begin
        ads_alias.AppendRecord([l[i], DatabaseFiles]);
      end;
    end;

  finally
    Reg.Free;
    Ads_Alias.First;
  end;
end;

procedure Tf_ibconnect.btn_updateClick(Sender: TObject);
begin
  ads_alias.Post;
end;

procedure Tf_ibconnect.btn_deleteClick(Sender: TObject);
begin
  ads_alias.Delete;
end;

procedure Tf_ibconnect.btn_insertClick(Sender: TObject);
begin
  ads_alias.Insert;
end;

procedure Tf_ibconnect.ads_aliasAfterScroll(DataSet: TDataSet);
begin
  e_database.Text:=ads_alias.Fields[1].AsString;
  Caption:='Alias : ' + ads_alias.Fields[0].AsString;
end;

procedure Tf_ibconnect.Button1Click(Sender: TObject);
begin
  ads_alias.Cancel;
end;

procedure Tf_ibconnect.Button2Click(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  m_databasefile.Text:=OpenDialog.FileName;
end;

end.
