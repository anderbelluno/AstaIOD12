unit AstaIODirectoryDataSet;

interface
uses Classes, AstaIOCustomDataSet, SysUtils, db;

type
  TAstaIODirectoryDataSet = class(TAstaIODataSet)
  private
    FAttrib: Integer;
    FTotalSize: Int64;
    FTimeStampIndex: Boolean;
    FLoadData: Boolean;
    FSearchRec: TSearchRec;
    FMaskList: TStrings;
    FDirectoryOnly: Boolean;
    function GetMasks: TStrings;
    procedure SetMasks(Value: TStrings);
  protected
    procedure DoAfterOpen; override;
    procedure AppendfromSearchRec(AMask: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFordirectory;
  published
    property Mask: TStrings read GetMasks write SetMasks;
    property LoadFile: Boolean read FLoadData write FLoadData default False;
    property TimeStampIndex: Boolean read FTimeStampIndex write FTimeStampIndex;
    property TotalSize: Int64 read FTotalSize write FTotalSize;
  end;
procedure Register;
implementation

procedure TAstaIODirectoryDataSet.AppendfromSearchRec(AMask: string);
begin
  Append;
  FieldByName('FileName').AsString := ExtractFileName(FSearchRec.Name);
  FieldByName('Ext').AsString := lowercase(copy(ExtractFileExt(FSearchRec.Name),
    2, 3));
  FieldByName('Size').AsInteger := FSearchRec.Size;
  FieldByName('TimeStamp').AsDateTime := FileDatetoDateTime(FSearchRec.Time);
  FieldByName('Directory').AsString := ExtractFilePath(AMask);
  FTotalSize := FTotalSize + FSearchRec.size;
  if FLoadData then
    TBlobField(FieldbyName('Data')).LoadFromfile(ExtractFilePath(AMask) +
      FSearchRec.Name);
  Post;
end;

procedure TAstaIODirectoryDataSet.DoAfterOpen;
var
  i: Integer;
begin
  inherited DoAfterOpen;
  Empty;
  FTotalSize := 0;
  DisableControls;
  try
    for i := 0 to FMaskList.Count - 1 do
    begin
      if sysutils.FindFirst(FMaskList[i], FAttrib, FSearchRec) = 0 then
        repeat
          AppendFromSearchRec(FMaskList[i]);
        until sysutils.findnext(FSearchRec) <> 0;
    end;
  finally
    sysutils.findclose(FSearchRec);
    EnableControls;
    AddIndex('FileName', false);
    if FTimeStampindex then
      AddIndex('TimeStamp', False);
  end;
end;

procedure TAstaIODirectoryDataSet.SetFordirectory;
begin
  FAttrib := $00000010;
end;

constructor TAstaIODirectoryDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoadData := False;
  FMaskList := TStringList.Create;
  AddField('FileName', ftstring, 50);
  AddField('Ext', ftstring, 3);
  AddField('TimeStamp', ftdatetime, 0);
  AddField('Size', ftinteger, 0);
  AddField('Directory', ftmemo, 0);
  AddField('Data', ftblob, 0);
  FTimeStampIndex := False;
  FAttrib := 0;
end;

destructor TAstaIODirectoryDataSet.Destroy;
begin
  FMaskList.Free;
  inherited Destroy;
end;

function TAstaIODirectoryDataSet.GetMasks: TStrings;
begin
  result := FMaskList;
end;

procedure TAstaIODirectoryDataSet.SetMasks(Value: TStrings);
begin
  FMaskList.Assign(Value);
end;

procedure Register;
begin
  RegisterComponents('AstaIO Client', [TAstaIODirectoryDataSet]);
end;

end.

