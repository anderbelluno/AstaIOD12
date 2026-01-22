unit xmlv_dump;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, astaxml_dom, astaxml_xmlv, astaxml_xmlw, StdCtrls, ComCtrls,
  Grids;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    GroupBox1: TGroupBox;
    LoadButton: TButton;
    SaveButton: TButton;
    NSEdit: TEdit;
    Label1: TLabel;
    TypeEdit: TEdit;
    ValueEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    attrlist: TStringGrid;
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  xmld:TXMLDocument=nil;

procedure add_leaves(tns:TTreeNodes;cn:TTreeNode;n:TDOMNode;sbl:boolean);
var
  tn:TTreeNode;
begin
  if sbl then tn:=tns.AddObject(cn,n.NodeName,n)
    else tn:=tns.AddChildObject(cn,n.NodeName,n);
  if assigned(n.FirstChild) then
    add_leaves(tns,tn,n.FirstChild,false);
  if assigned(n.NextSibling) then
    add_leaves(tns,tn,n.NextSibling,true);
end;

procedure dump_nodes(xd:TXMLDocument;tv:TTreeView);
begin
  add_leaves(tv.Items,nil,xd,true);
end;

procedure TForm1.LoadButtonClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute then
    begin
      if assigned(xmld) then xmld.free;
        ReadXMLFile(xmld,OpenDialog1.FileName);
      dump_nodes(xmld,treeview1);
      treeview1.FullExpand;
      SaveButton.Enabled:=true;
    end;
  except
    on e:exception do
      Application.MessageBox(pchar(e.message),'Parser error',mb_Ok);
  end;
end;

procedure TForm1.SaveButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    WriteXMLFile(xmld,SaveDialog1.FileName);
  end;
end;

const
  NodeNames: array[ELEMENT_NODE..NOTATION_NODE] of String = (
    'Element',
    'Attribute',
    'Text',
    'CDATA section',
    'Entity reference',
    'Entity',
    'Processing instruction',
    'Comment',
    'Document',
    'Document type',
    'Document fragment',
    'Notation'
  );

procedure update_attrs(l:TStringGrid;n:TDOMNode);
var
  i:integer;
  attr:TDOMNode;
begin
  if l.RowCount>0 then
  begin
    l.Cells[0,0]:='';
    l.Cells[1,0]:='';
  end;
  l.RowCount:=0;
  l.Enabled:=false;
  if (n.Attributes <> nil) and (n.Attributes.Length > 0) then
  begin
    l.Enabled:=true;
    for i := 0 to n.Attributes.Length - 1 do begin
      attr := n.Attributes.Item[i];
      l.Cells[0,i]:=attr.NodeName;
      l.Cells[1,i]:=attr.NodeValue;
    end;
    l.RowCount:=n.Attributes.Length;
  end;
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  n:TDOMNode;
begin
  n:=TDOMNode(Node.Data);
  NSEdit.Text:=n.NamespaceURI;
  TypeEdit.Text:=NodeNames[n.NodeType];
  ValueEdit.Text:=n.NodeValue;
  update_attrs(attrlist,n);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  attrlist.ColWidths[1]:=attrlist.Width-attrlist.DefaultColWidth;
end;

end.

