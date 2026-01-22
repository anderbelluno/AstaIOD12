unit AstaSoap_WizardForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ActnList, AstaHTTPConnection;

type
  TWSDLWizardForm = class(TForm)
    OpenDialogWSDL: TOpenDialog;
    CancelButton: TButton;
    NextButton: TButton;
    PriorButton: TButton;
    WizardPages: TPageControl;
    TabSheetGetWSDL: TTabSheet;
    TabSheetOptions: TTabSheet;
    ServerRadioButton: TRadioButton;
    FileRadioButton: TRadioButton;
    PageControlWSDL: TPageControl;
    TabSheetHTTP: TTabSheet;
    ServerAddressEdit: TEdit;
    ServerUsernameEdit: TEdit;
    ServerPasswordEdit: TEdit;
    ProxyAddressEdit: TEdit;
    ProxyPortEdit: TEdit;
    ProxyPortUpDown: TUpDown;
    TabSheetFile: TTabSheet;
    FileNameEdit: TEdit;
    FileBrowseBtn: TButton;
    TabSheetResults: TTabSheet;
    CodeListBox: TMemo;
    StringsCheckBox: TCheckBox;
    ActionList: TActionList;
    NextAction: TAction;
    PriorAction: TAction;
    TabSheetProgress: TTabSheet;
    ProgressListBox: TListBox;
    AstaHTTPConnection: TAstaHTTPConnection;
    procedure NextActionUpdate(Sender: TObject);
    procedure PriorActionUpdate(Sender: TObject);
    procedure PriorActionExecute(Sender: TObject);
    procedure NextActionExecute(Sender: TObject);
    procedure ServerRadioButtonClick(Sender: TObject);
    procedure AstaHTTPConnectionConnect(Sender: TObject);
    procedure AstaHTTPConnectionDisconnect(Sender: TObject);
    procedure AstaHTTPConnectionRequest(Sender: TObject; Data: String);
    procedure AstaHTTPConnectionResponse(Sender: TObject; Data: String);
    procedure FileBrowseBtnClick(Sender: TObject);
  private
    FStream: TStream;
    FUnitName: String;
    FUnitText: String;
  public
    function Execute(var UnitName, UnitText: String): Boolean; 
  end;

var
  WSDLWizardForm: TWSDLWizardForm;

implementation

uses
  ASTASoap_uImport, AstaSoap_uParseURL;

{$R *.DFM}

{ TWSDLWizardForm }

function TWSDLWizardForm.Execute(var UnitName, UnitText: String): Boolean;
begin
  WizardPages.ActivePageIndex := 0;
  ActiveControl := ServerAddressEdit;
  Result := ShowModal = mrOK;
  if Result then
    begin
      UnitName := FUnitName;
      UnitText := FUnitText;
      //WSDLToPas(FStream, FUnitName, FUnitText);
    end;
  FStream.Free;  
end;

procedure TWSDLWizardForm.NextActionUpdate(Sender: TObject);
begin
  with WizardPages do
    begin
      NextAction.Enabled := (ActivePageIndex <> 1) and ((ActivePageIndex > 0) or
        (ServerRadioButton.Checked and (ServerAddressEdit.Text <> '')) or
        (FileRadioButton.Checked and (FileNameEdit.Text <> '')));
      if ActivePageIndex = 3 then
        NextAction.Caption := '&Finish'
      else
        NextAction.Caption := '&Next >';
    end;
end;

procedure TWSDLWizardForm.NextActionExecute(Sender: TObject);
var
  puProto, puUser, puPass, puHost, puPort, puPathFN: String;
begin
  case WizardPages.ActivePageIndex of
  0:
    begin
      if ServerRadioButton.Checked then
        begin
          FStream := TMemoryStream.Create;
          WizardPages.ActivePageIndex := 1;
          Repaint;
          try
            AstaHTTPConnection.KeepAlive := False;

            ParseURL(ServerAddressEdit.Text, puProto, puUser, puPass, puHost,
              puPort, puPathFN);

            AstaHTTPConnection.KeepAlive := False;
            AstaHTTPConnection.Host := puHost;
            AstaHTTPConnection.Page := puPathFN;
            AstaHTTPConnection.Port := StrToIntDef(puPort, 80);
            AstaHTTPConnection.UserName := ServerUsernameEdit.Text;
            AstaHTTPConnection.Password := ServerPasswordEdit.Text;
            AstaHTTPConnection.ProxyHost := ProxyAddressEdit.Text;
            AstaHTTPConnection.ProxyPort := ProxyPortUpDown.Position;

            AstaHTTPConnection.Execute;

            puProto := AstaHTTPConnection.ResponseData;
            if puProto <> '' then
              FStream.WriteBuffer(PChar(puProto)^, Length(puProto));
            FStream.Position := 0;
            WizardPages.ActivePageIndex := 2;
          except
            on E: Exception do
              begin
                ProgressListBox.Items.Add('Error: ' + E.Message);
                Repaint;
              end;
          end;
        end
      else
        begin
          FStream := TFileStream.Create(FileNameEdit.Text,
            fmOpenRead or fmShareDenyWrite);
          WizardPages.ActivePageIndex := 2;
        end;
    end;
  2:
    begin
      WSDLToPas(FStream, FUnitName, FUnitText, StringsCheckBox.Checked);
      CodeListBox.Lines.Text := FUnitText;
      WizardPages.ActivePageIndex := 3;
    end;
  3: ModalResult := mrOK;
  end;
end;

procedure TWSDLWizardForm.PriorActionUpdate(Sender: TObject);
begin
  PriorAction.Enabled := WizardPages.ActivePageIndex > 0;
end;

procedure TWSDLWizardForm.PriorActionExecute(Sender: TObject);
begin
  with WizardPages do
    begin
      ActivePageIndex := ActivePageIndex - 1;
      if ActivePageIndex = 1 then
        ActivePageIndex := 0;
    end;
end;

procedure TWSDLWizardForm.ServerRadioButtonClick(Sender: TObject);
begin
  if ServerRadioButton.Checked then
    PageControlWSDL.ActivePageIndex := 0
  else
    PageControlWSDL.ActivePageIndex := 1;
end;

procedure TWSDLWizardForm.AstaHTTPConnectionConnect(Sender: TObject);
begin
  ProgressListBox.Items.Add('Connected');
  Repaint;
end;

procedure TWSDLWizardForm.AstaHTTPConnectionDisconnect(Sender: TObject);
begin
  ProgressListBox.Items.Add('Desconnected');
  Repaint;
end;

procedure TWSDLWizardForm.AstaHTTPConnectionRequest(Sender: TObject;
  Data: String);
begin
  ProgressListBox.Items.Add('Sending request');
  Repaint;
end;

procedure TWSDLWizardForm.AstaHTTPConnectionResponse(Sender: TObject;
  Data: String);
begin
  ProgressListBox.Items.Add('Receiving response');
  Repaint;
end;

procedure TWSDLWizardForm.FileBrowseBtnClick(Sender: TObject);
var
  CurDir: String;
begin
  CurDir := GetCurrentDir;
  try
    OpenDialogWSDL.FileName := FileNameEdit.Text;
    if OpenDialogWSDL.Execute then
      FileNameEdit.Text := OpenDialogWSDL.FileName;
  finally
    SetCurrentDir(CurDir);
  end;
end;

end.
