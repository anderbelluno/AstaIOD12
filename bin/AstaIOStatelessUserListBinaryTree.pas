{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10327: AstaIOStatelessUserListBinaryTree.pas 
{
{   Rev 1.0    4/10/2003 6:32:22 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:18 PM  Steve    Version: 1.505
}
unit AstaIOStatelessUserListBinaryTree;
(* Based on work by Dr Mark Brittingham and modifield for ASTA *)


{

	The MDWeb ISAPI Session Management Components

  Dr. Mark Brittingham
	mdbritt@acm.org

	This is a BTree implementation used for storing session data.  It is indexed by
  the session ID and holds a stringlist for the user data.  It is also timestamped
  so that we can periodically "expire" old nodes.  This structure is hidden
  from the user (within the SessMgr object) so that it can be swapped out if
  another data access method is preferred.  It is quite fast, however, and thread-
  safe.  If you define "WINDRAW" in the compiler then you can include this in a
  windows app and use the "DrawTreeNodes" function to draw this tree to a Canvas.
  //removed ASTA 08.11.2001
}
{.$define WinDraw}
interface

uses
  SysUtils,
  Classes,
  Math,
  SyncObjs,
  AstaIOParamList,
  AstaIOUserList
  {$ifdef WINDRAW}
	, Graphics
   {$endif};

type
	PTNode = ^TNode;
	TNode = Record
     ID : Cardinal;  // The Unique session identifier - chosen from random between 1 and 2,000,000,000.
     TimeStamp : TDateTime;
     User:TUserRecord;
     FPendingMessages:TAstaParamList;
     // We use a stringlist to be consistent with the Query/Content fields approach of WebBroker.
     //changed to a TAstaParamList
     LeftChild, RightChild : PTNode;
  end;
  TExpiredDeletionEvent=Procedure(Sender:TObject;User:TuserRecord;Var TimeStamp:TDateTime) of object;
  TAstaIOStatelessBTreeUserList = class(TObject)
  private
    //ASTA moved from global so that multiple Trees can be used
    FCheckInterval : TDateTime;  // How often check for deleted nodes
    FExpireInterval : TDateTime;  // How old nodes must be to delete them (Now - ExpireInterval).
    FLastCheck : TDateTime;   //Last time we checked the nodes.
    FDeleteEvent:TExpiredDeletionEvent;
    MDBTreeCS : TCriticalSection;
  	Head : PTNode;
   {$ifdef WINDRAW}
     theCanvas : TCanvas;
		function DrawTreeNode(aNode : PTNode; x, y : Integer; LeftSide : Boolean) : integer;
   {$endif}
		function AddNode(aNode : PTNode) : Boolean;
		function FindNode(idVal : Cardinal; curNode : PTNode) : PTNode;
		procedure DeleteNode(idVal : Cardinal; aNode, parent : PTNode; leftSide, deleteObj : boolean);
		function FindMaxNode(curNode : PTNode) : PTNode;
		procedure DeleteExp(curNode, parent : PTNode; aDateTime : TDateTime; leftSide : Boolean);
		procedure DeleteAll(curNode : PTNode);
     // Called only from AddToTree or FindNodeByID - within a critical section (important - MUST be within a critical section).
		procedure DeleteExpired(aDateTime : TDateTime);
    Procedure DoDeleteEvent(User:TuserRecord;Var TimeStamp:TDateTime);
  public
    property CheckInterval : TDateTime read FCheckInterval write FCheckInterval;
    property ExpireInterval : TDateTime read FExpireInterval write FExpireInterval;
    property LastCheck : TDateTime read FLastCheck write FLastCheck;   //Last time we checked the nodes.
    Constructor Create(MinutesForExpiration,MinutesToCheck:Word);
    property OnDeleteEvent:TExpiredDeletionEvent read FDeleteEvent write FDeleteEvent;
    Destructor Destroy; Override;
    function CreateNode : PTNode;
    Function AddUser(U:TUserRecord):Cardinal;
    function AddToTree(aNode : PTNode) : Boolean;
    function FindNodeByID(ID : Cardinal) : PTNode;
    Function GetUserRecord(ID : Cardinal) : TUserRecord;
    Procedure AddPendingMessage(ID:Cardinal;Msgid:Integer;Params:TAstaParamList);
    Function GetParamList(ID : Cardinal) : TAstaParamList;overload;
    procedure DeleteNodeByID(ID : Cardinal);
   {$ifdef WINDRAW}
     // If you want to test the tree in a Windows app - enable Windraw and then call this FN with a Canvas to draw on.
		procedure DrawTreeNodes(aCanvas : TCanvas);
   {$endif}
  end;



implementation

//******************************************************************************
// The Binary Tree Implementation.  First, the Interface (public) routines.
//******************************************************************************
Procedure DisposeOfNode(p:PTNode);
begin
p^.User.Free;
p^.FPendingMessages.Free;
dispose(p);
end;

Function TAstaIOStatelessBTreeUserList.AddUser(U:TUserRecord):Cardinal;
var
 p:PTNode;
begin
 result:=0;
 P:=CreateNode;
 if p<>nil then begin
  P.User:=U.CopyUserRecord;
  result:=P^.ID
 end;
end;

Procedure TAstaIOStatelessBTreeUserList.AddPendingMessage(ID:Cardinal;Msgid:Integer;Params:TAstaParamList);
var
 p:PTNode;
begin
  p:=findNodeById(Id);
  if p<>nil then begin
   if p^.FPendingMessages=nil then p^.FPendingMessages:=TAstaParamList.Create;
   p^.FPendingMessages.FastAdd(IntToStr(msgid),Params.AsTokenizedString(False));
 end;
end;
Function TAstaIOStatelessBTreeUserList.GetParamList(ID : Cardinal) : TAstaParamList;
var
 p:PTNode;
begin
  result:=nil;
  p:=findNodeById(Id);
  if p<>nil then result:=p^.User.ParamList;
end;

Function TAstaIOStatelessBTreeUserList.GetUserRecord(ID : Cardinal) : TUserRecord;
var
 p:PTNode;
begin
  result:=nil;
  p:=findNodeById(Id);
  if p<>nil then result:=p^.User;
end;


Constructor TAstaIOStatelessBTreeUserList.Create(MinutesForExpiration,MinutesToCheck:Word);
begin
	MDBTreeCS := TCriticalSection.Create;
	Head := Nil;
  Randomize;
  if MinutestoCheck=0 then CheckInterval := 0.0021   // Every three minutes
   else CheckInterval:=EncodeTime(0,MinutesToCheck,0,0);
  if MinutesForExpiration=0 then  ExpireInterval := 0.0139  // Twenty minutes.
   else ExpireInterval:=EncodeTime(0,MinutesForExpiration,0,0);
  {$ifdef TestMode}
  CheckInterval := EncodeTime(0,0,30,0);
  ExpireInterval := EncodeTime(0,0,30,0);
  {$endif}
  LastCheck := Now;
  Inherited Create;
end;

//------------------------------------------------------------------------------
Destructor TAstaIOStatelessBTreeUserList.Destroy;
begin
	DeleteAll(Head);
	MDBTreeCS.Free;
  Inherited Destroy;
end;


//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.CreateNode : PTNode;
var
  aNode : PTNode;
begin
	try
     New(aNode);
     aNode^.ID := Random(2000000000);
     aNode^.TimeStamp := Now;
     aNode^.User:=nil;
     ANode^.FPendingMessages:=nil;
     aNode^.LeftChild := Nil;
     aNode^.RightChild := Nil;
     while (not AddToTree(aNode)) do  // Should almost NEVER be needed...
        aNode^.ID := Random(2000000000);
     result := aNode;
  except
  	result := Nil;
  end;
end;

//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.AddToTree(aNode : PTNode) : Boolean;
begin
	MDBTreeCS.Acquire;
  try
  // Every time we get a AddToTree request (CreateNode), we see if we should
  // check for expiring nodes.  We don't do it *every* time because it would
  // be too expensive.
  if (Now - LastCheck > CheckInterval) then
  begin
		DeleteExpired(Now - ExpireInterval);
  	LastCheck := Now;
  end;
  try
		result := AddNode(aNode);
  except;
  	result := False;
  end;
  finally
   MDBTreeCS.Release;
  end;
end;

//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.FindNodeByID(ID : Cardinal) : PTNode;
begin
	MDBTreeCS.Acquire;
  try
  // Every time we get a FindNodeByID request, we see if we should
  // check for expiring nodes.  We don't do it *every* time because it would
  // be too expensive.
  if (Now - LastCheck > CheckInterval) then
  begin
		DeleteExpired(Now - ExpireInterval);
  	LastCheck := Now;
  end;
	try
		result := FindNode(ID, Head);
  except
    	result := Nil;
  end;
  finally
   MDBTreeCS.Release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DeleteNodeByID(ID : Cardinal);
begin
	MDBTreeCS.Acquire;
  try
  try
		DeleteNode(ID, Head, Nil, True, True);
  except;
  end;
  finally
   MDBTreeCS.Release;
  end;
end;
{$ifdef WINDRAW}
//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DrawTreeNodes(aCanvas : TCanvas);
begin
	MDBTreeCS.Acquire;
  try
  try
     theCanvas := aCanvas;
     DrawTreeNode(Head, 1, 5, True);
  except
  end;
  finally
   MDBTreeCS.Release;
  end;
end;
{$endif}
{$ifdef WINDRAW}
//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.DrawTreeNode(aNode : PTNode; x, y : Integer; LeftSide : Boolean) : integer;
var
	tempX : integer;
begin
	if (aNode = Nil) then  //Only happens if there are no nodes...
  begin
     result := x;
  	Exit;
  end;
	tempX := x - 40;  // Middle of parent.
  // Recurse to the left
  if (aNode^.LeftChild <> Nil) then
  	x := DrawTreeNode(aNode^.LeftChild, x, y + 30, True);
	// Draw This node using the rightmost point of the child tree as the start point.
	with theCanvas do
  begin
     Rectangle(x, y, x + 75, y + 20);
    	TextOut(x+1, y+3, IntToStr(aNode^.ID));
     if (Not Leftside) and (y > 5) then
     begin
     	MoveTo(tempX, y - 10);
       	LineTo(x + 40, y);
     end;
  end;
  tempX := x + 40;  // THIS node's current position
  x := x + 80;
  // Recurse to the right
  if (aNode^.RightChild <> Nil) then
  	x := DrawTreeNode(aNode^.RightChild, x, y + 30, False);

  // Draw the connector line for Leftside nodes.
  if Leftside and (y > 5) then
  begin
		with theCanvas do
     begin
     	MoveTo(tempX, y);
       	LineTo(x + 40, y - 10);
     end;
  end;
 	result := x;
end;
{$endif}

//------------------------------------------------------------------------------
// Scans through all nodes and calls DeleteNode for any whose time is up.  It
// will remove from the tree any node whose timestamp (the last time is was
// touched) is older than the TDateTime value passed in.
//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DeleteExpired(aDateTime : TDateTime);
begin
	// This will always be called within a critical section.
  try
		DeleteExp(Head, Nil, aDateTime, True);
  except
  end;
end;


//******************************************************************************
// The Binary Tree Implementation.  These are the Private routines.
//******************************************************************************
function TAstaIOStatelessBTreeUserList.AddNode(aNode : PTNode) : Boolean;
var
	curNode : PTNode;
begin
	if (Head = Nil) then
  begin
  	Head := aNode;
     result := True;
     Exit;
  end;
  curNode := Head;
  // In contrast with other routines, this one is implemented as an iterative rather
  // than a recursive algorithm.  To use iterative algorithms in most of the other
  // routines, you'd have to add links back to the parent for each node.
  while 1=1 do
  begin
  	// Fail if this node is already in the tree...unless it is marked as "Deleted"
  	if (aNode^.ID = CurNode^.ID) then
     begin
    		Result := False;
       	Exit;
     end;
     // Check if this belongs on the left or the right.  If either and the slot
     // is available, then just place it there.  Otherwise, make the occupant of the
     // slot the next "CurNode" and iterate.
  	if (aNode^.ID < CurNode^.ID) then
     begin
     	if (CurNode^.LeftChild = Nil) then
        begin
        	CurNode.LeftChild := aNode;
           Result := True;
           Exit;
        end
        else
        	CurNode := CurNode.LeftChild;
     end
     else
     begin
     	if (CurNode^.RightChild = Nil) then
        begin
        	CurNode.RightChild := aNode;
           Result := True;
           Exit;
        end
        else
        	CurNode := CurNode.RightChild;
     end;
  end;
end;

//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.FindNode(idVal : Cardinal; curNode : PTNode) : PTNode;
begin
	if (curNode = Nil) then
  	result := Nil
	else if (idVal = curNode^.ID) then
  begin
  	result := curNode;
     // We stamp a node every time it is searched on - to indicate that the surfer
     // is still actively using his record.
    	result^.TimeStamp := Now;
  end
  else if (idVal < curNode^.ID) then
 		result := FindNode(idVal, curNode^.LeftChild)
  else
 		result := FindNode(idVal, curNode^.RightChild);
end;

//------------------------------------------------------------------------------
// Goes through tree and finds the node matching idVal and deletes it.  After
// deletion, it has to re-arrange the children under the deleted node. "LeftSide" is a boolean
// which tells us which side of the parent this node is on (true if left, false if right).
//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DeleteNode(idVal : Cardinal; aNode, parent : PTNode; leftSide, deleteObj : boolean);
var
	tempNode : PTNode;
begin
	tempNode := Nil;
	if (aNode^.ID = idVal) then  //*THIS* is the node to delete.
  begin
  	if (aNode^.LeftChild = Nil) and (aNode^.RightChild = Nil) then
     begin
     	if deleteObj then
        begin
         DisposeOfNode(ANode);
        end;
     end
     else if (aNode^.LeftChild = Nil) then
     begin
     	tempNode := aNode^.RightChild;
     	if deleteObj then
        begin
          DisposeOfNode(ANode);
        end;
     end
     else if (aNode^.RightChild = Nil) then
     begin
       	tempNode := aNode^.LeftChild;
     	if deleteObj then
        begin
           DisposeOfNode(ANode);
        end;
     end
     else
     begin
     	// The harder case...node has both left and right children. (won't ever be true in recursion spawned by finding max nodes)
        // What we do is replace the current node with the Largest of its smallest (leftmost) children.
        // This node will be larger than nodes on its (new) left and smaller than all those on its right.
     	tempNode := FindMaxNode(aNode^.LeftChild); //This will *NOT* have a right child (or else it wouldn't be max).
			if (tempNode = Nil) then //Error Case.
        begin
        	// LogErr(mdErr1, 'Error finding MaxNode during delete', Now);
        	Exit;  //Just cancel processing this portion of the tree.
        end;
        //Remove tempNode from tree so it can be moved up here.  Start at the LeftChild
        // to minimize the time needed to search - note also this is a recursive call and
        // will not delete TempNode (this is why we need the "deleteObj" parameter...).
        DeleteNode(tempNode^.ID, aNode^.LeftChild, aNode, True, False);
        tempNode^.RightChild := aNode^.RightChild;
        tempNode^.LeftChild := aNode^.LeftChild;
     	if deleteObj then
        begin
           DisposeOfNode(ANode);
        end;
     end;
     // Now, reset the parent link.
     if (parent = Nil) then
     	Head := tempNode
     else
     begin
	    	if (leftSide) then
        	parent^.LeftChild := tempNode
        else
        	parent^.RightChild := tempNode;
     end;
  end
  else
  begin
  	if (aNode^.LeftChild <> Nil) then
  		DeleteNode(idVal, aNode^.LeftChild, aNode, True, deleteObj);
  	if (aNode^.RightChild <> Nil) then
     	DeleteNode(idVal, aNode^.RightChild, aNode, False, deleteObj);
  end;
end;

//------------------------------------------------------------------------------
// To Find Max, descend down the right-hand side: higher numbers are always stored
// in the right child (and its descendants)
//------------------------------------------------------------------------------
function TAstaIOStatelessBTreeUserList.FindMaxNode(curNode : PTNode) : PTNode;
begin
	if (curNode = Nil) then  //Shouldn't ever be needed...
  	result := Nil
  else if (curNode^.RightChild = Nil) then
  	result := curNode  //Normal case...
  else
  	result := FindMaxNode(curNode^.RightChild);
end;

Procedure TAstaIOStatelessBTreeUserList.DoDeleteEvent(User:TuserRecord;Var TimeStamp:TDateTime);
begin
 if assigned(FDeleteEvent) then
  FDeleteEvent(Self,User,TimeStamp);                                                        
end;


//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DeleteExp(curNode, parent : PTNode; aDateTime : TDateTime; leftSide : Boolean);
begin
	// A breadth-first search so that we do the minimum number of tree rearrangements
  // Also, a depth-first can delete/rearrange curNode and its children before we've
  // had a chance to work on them and thus screw things up.
	if (curNode <> Nil) then
  begin
		DeleteExp(curNode^.LeftChild, curNode, aDateTime, True);
		DeleteExp(curNode^.RightChild, curNode, aDateTime, False);
  	if (curNode^.TimeStamp < aDateTime) then  //Start delete at current node to save search...
      begin
      DoDeleteEvent(curnode^.User,ADateTime);
     	if (curNode^.TimeStamp < aDateTime) then
 			DeleteNode(curNode^.ID, curNode, parent, leftSide, True);
      end;
	end;
end;

//------------------------------------------------------------------------------
// Scans through all nodes and calls DeleteNode for any whose time is up.
//------------------------------------------------------------------------------
procedure TAstaIOStatelessBTreeUserList.DeleteAll(curNode : PTNode);
begin
	// A breadth-first traversal: start from the bottom and delete everything as we go up.
	if (curNode <> Nil) then
  begin
  	// Get rid of all this node's children...
		DeleteAll(curNode^.LeftChild);
		DeleteAll(curNode^.RightChild);
     // Then delete this node itself.
    DisposeOfNode(curnode);
	end;
end;


end.

