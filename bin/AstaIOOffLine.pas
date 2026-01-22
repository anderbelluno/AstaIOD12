{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10255: AstaIOOffLine.pas 
{
{   Rev 1.0    4/10/2003 6:31:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:52 PM  Steve    Version: 1.505
}
unit AstaIOOffLine;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  QDialogs, QForms, QStdCtrls, QControls, QButtons, QComCtrls, QExtCtrls,
  StdCtrls, Controls, Buttons, ComCtrls, ExtCtrls;
{$ELSE}
  ComCtrls, Dialogs, Forms, StdCtrls, Controls, ExtCtrls, Buttons;
{$ENDIF}

type
  TOffLineDialog = class(TForm)
    pan_bot: TPanel;
    btn_ok: TBitBtn;
    btn_cancel: TBitBtn;
    pan_top: TPanel;
    pan_left: TPanel;
    pan_right: TPanel;
    pg_props: TPageControl;
    ts_PrimeFields: TTabSheet;
    ts_NoSQLFields: TTabSheet;
    ts_RefetchFields: TTabSheet;
    ts_general: TTabSheet;
    lb_PrimeFields: TListBox;
    e_PrimeFields: TEdit;
    sb_PrimeFields: TSpeedButton;
    sb_PrimeFields_up: TSpeedButton;
    sb_PrimeFields_down: TSpeedButton;
    btn_PrimeFields_replace: TButton;
    btn_PrimeFields_add: TButton;
    btn_PrimeFields_delete: TButton;
    lb_NoSQLFields: TListBox;
    sb_NoSQLFields_up: TSpeedButton;
    sb_NoSQLFields_down: TSpeedButton;
    SpeedButton3: TSpeedButton;
    e_NoSQLFields: TEdit;
    btn_NoSQLFields_add: TButton;
    btn_NoSQLFields_replace: TButton;
    btn_NoSQLFields_delete: TButton;
    lb_RefetchFields: TListBox;
    e_RefetchFields: TEdit;
    SpeedButton1: TSpeedButton;
    sb_RefetchSQLFields_down: TSpeedButton;
    sb_RefetchSQLFields_up: TSpeedButton;
    btn_RefetchSQLFields_delete: TButton;
    btn_RefetchSQLFields_add: TButton;
    btn_RefetchSQLFields_replace: TButton;
    BitBtn1: TBitBtn;
    lab_database: TLabel;
    lab_tablename: TLabel;
    lab_updatetablename: TLabel;
    e_database: TEdit;
    e_tablename: TEdit;
    e_updatetablename: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cmb_updatemode: TComboBox;
    cmb_updatemethod: TComboBox;
    e_AutoIncrement: TEdit;
    Label3: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sb_PrimeFields_upClick(Sender: TObject);
    procedure sb_PrimeFields_downClick(Sender: TObject);
    procedure btn_PrimeFields_deleteClick(Sender: TObject);
    procedure btn_PrimeFields_replaceClick(Sender: TObject);
    procedure btn_PrimeFields_addClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lb_PrimeFieldsClick(Sender: TObject);
    procedure btn_okClick(Sender: TObject);
    procedure sb_PrimeFieldsClick(Sender: TObject);
    procedure btn_NoSQLFields_replaceClick(Sender: TObject);
    procedure btn_NoSQLFields_addClick(Sender: TObject);
    procedure btn_NoSQLFields_deleteClick(Sender: TObject);
    procedure sb_NoSQLFields_upClick(Sender: TObject);
    procedure sb_NoSQLFields_downClick(Sender: TObject);
    procedure lb_NoSQLFieldsClick(Sender: TObject);
    procedure lb_RefetchFieldsClick(Sender: TObject);
    procedure btn_RefetchSQLFields_replaceClick(Sender: TObject);
    procedure btn_RefetchSQLFields_addClick(Sender: TObject);
    procedure btn_RefetchSQLFields_deleteClick(Sender: TObject);
    procedure sb_RefetchSQLFields_upClick(Sender: TObject);
    procedure sb_RefetchSQLFields_downClick(Sender: TObject);
  private
    procedure MoveItemUp(ListBox: TListBox);
    procedure MoveItemDown(ListBox: TListBox);
    procedure DeleteItem(ListBox: TListBox);
    procedure ReplaceItem(ListBox: TListBox; Value:String);
    procedure AddItem(ListBox: TListBox; Value:String);

    procedure Apply;
  public
    NoSQLFields: TStrings;
    PrimeFields: TStrings;
    RefetchFields: TStrings;
  end;

var
  OffLineDialog: TOffLineDialog;

implementation

{$R *.dfm}

procedure TOffLineDialog.FormDestroy(Sender: TObject);
begin
  NoSQLFields.Free;
  PrimeFields.Free;
  RefetchFields.Free;
end;

procedure TOffLineDialog.FormCreate(Sender: TObject);
begin
  NoSQLFields:=TStringList.Create;
  PrimeFields:=TStringList.Create;
  RefetchFields:=TStringList.Create;
end;

procedure TOffLineDialog.MoveItemDown(ListBox: TListBox);
var idx :Integer;
begin
  if ListBox.Items.Count = 0 then exit;
  if ListBox.ItemIndex = -1 then ListBox.ItemIndex:=0;

  idx:=ListBox.ItemIndex;
  if idx+1 >= ListBox.Items.Count then exit;
  ListBox.Items.Exchange(idx, idx + 1);
end;

procedure TOffLineDialog.MoveItemUp(ListBox: TListBox);
var idx :Integer;
begin
  if ListBox.Items.Count = 0 then exit;
  if ListBox.ItemIndex = -1 then ListBox.ItemIndex:=0;

  idx:=ListBox.ItemIndex;
  if idx-1 < 0 then exit;
  ListBox.Items.Exchange(idx, idx - 1);
end;

procedure TOffLineDialog.sb_PrimeFields_upClick(Sender: TObject);
begin
  MoveItemUp(lb_PrimeFields);
end;

procedure TOffLineDialog.sb_PrimeFields_downClick(Sender: TObject);
begin
  MoveItemDown(lb_PrimeFields);
end;

procedure TOffLineDialog.btn_PrimeFields_deleteClick(Sender: TObject);
begin
  DeleteItem(lb_PrimeFields);
end;

procedure TOffLineDialog.DeleteItem(ListBox: TListBox);
begin
  if ListBox.Items.Count = 0 then exit;
  ListBox.Items.Delete(ListBox.ItemIndex);
end;

procedure TOffLineDialog.btn_PrimeFields_replaceClick(Sender: TObject);
begin
  ReplaceItem(lb_PrimeFields, e_PrimeFields.Text);
end;

procedure TOffLineDialog.ReplaceItem(ListBox: TListBox; Value:String);
begin
  if Trim(Value) = '' then Exit;
  if ListBox.Items.Count = 0 then exit;
  ListBox.Items[ListBox.ItemIndex]:=Value;
end;

procedure TOffLineDialog.AddItem(ListBox: TListBox; Value: String);
begin
  if Trim(Value) = '' then Exit;
  ListBox.Items.Add(Value);
  ListBox.ItemIndex:=ListBox.Items.Count-1;
end;

procedure TOffLineDialog.btn_PrimeFields_addClick(Sender: TObject);
begin
  AddItem(lb_PrimeFields, e_PrimeFields.Text);
end;

procedure TOffLineDialog.FormActivate(Sender: TObject);
begin
  lb_PrimeFields.Items.Assign(PrimeFields);
  lb_NoSQLFields.Items.Assign(NoSQLFields);
  lb_RefetchFields.Items.Assign(RefetchFields);
end;

procedure TOffLineDialog.lb_PrimeFieldsClick(Sender: TObject);
begin
  if lb_PrimeFields.ItemIndex < 0 then exit;
  e_PrimeFields.Text:=lb_PrimeFields.Items[lb_PrimeFields.ItemIndex];
end;

procedure TOffLineDialog.Apply;
begin
  PrimeFields.Assign(lb_PrimeFields.Items);
  NoSQLFields.Assign(lb_NoSQLFields.Items);
  RefetchFields.Assign(lb_RefetchFields.Items);
end;

procedure TOffLineDialog.btn_okClick(Sender: TObject);
begin
  Apply;
end;

procedure TOffLineDialog.sb_PrimeFieldsClick(Sender: TObject);
begin
  showmessage('This will display a dialog that contains the cached fields, from which they can select a field'); 
end;

procedure TOffLineDialog.btn_NoSQLFields_replaceClick(Sender: TObject);
begin
  ReplaceItem(lb_NoSQLFields, e_NoSQLFields.Text);
end;

procedure TOffLineDialog.btn_NoSQLFields_addClick(Sender: TObject);
begin
  AddItem(lb_NoSQLFields, e_NoSQLFields.Text);
end;

procedure TOffLineDialog.btn_NoSQLFields_deleteClick(Sender: TObject);
begin
  DeleteItem(lb_NoSQLFields);
end;

procedure TOffLineDialog.sb_NoSQLFields_upClick(Sender: TObject);
begin
  MoveItemUp(lb_NoSQLFields);
end;

procedure TOffLineDialog.sb_NoSQLFields_downClick(Sender: TObject);
begin
  MoveItemDown(lb_NoSQLFields);
end;

procedure TOffLineDialog.lb_NoSQLFieldsClick(Sender: TObject);
begin
  if lb_NoSQLFields.ItemIndex < 0 then exit;
  e_NoSQLFields.Text:=lb_NoSQLFields.Items[lb_NoSQLFields.ItemIndex];
end;

procedure TOffLineDialog.lb_RefetchFieldsClick(Sender: TObject);
begin
  if lb_RefetchFields.ItemIndex < 0 then exit;
  e_RefetchFields.Text:=lb_RefetchFields.Items[lb_RefetchFields.ItemIndex];
end;

procedure TOffLineDialog.btn_RefetchSQLFields_replaceClick(Sender: TObject);
begin
  ReplaceItem(lb_RefetchFields, e_RefetchFields.Text);
end;

procedure TOffLineDialog.btn_RefetchSQLFields_addClick(Sender: TObject);
begin
  AddItem(lb_RefetchFields, e_RefetchFields.Text);
end;

procedure TOffLineDialog.btn_RefetchSQLFields_deleteClick(Sender: TObject);
begin
  DeleteItem(lb_RefetchFields);
end;

procedure TOffLineDialog.sb_RefetchSQLFields_upClick(Sender: TObject);
begin
  MoveItemUp(lb_RefetchFields);
end;

procedure TOffLineDialog.sb_RefetchSQLFields_downClick(Sender: TObject);
begin
  MoveItemDown(lb_RefetchFields);
end;

end.
