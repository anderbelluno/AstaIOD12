Known Issues

1. Accessing float fields with AsNull calls under Kylix freezes Kylix or takes down X Windows. We have
coded around this.
2. ftTimeStamp fields under delphi 6 not implemented yet. We map to ftDateTimeFields
There are issues in translating between delphi 5 clients and Delphi 6 and Kylix servers since Delphi 5 has no knowledge of fttimestamp.
3. ServerMethods with Providers do not supplyApplyUpdates yet.

AstaIO 0.95- 4 July 2001
1. mapped ftfmtBCD (delphi 6) to ftFloat to allow for delphi 5 clients to read data.
2. Added AstaIOVersion function to AstaIOUtil.

AstaIO 0.951- 6 August 2001
1. changed default behavior of TAstaIOCustomDataSets to set StoreFieldDefs to true to support design
time opened datasets with dataaware controls.
2. Clientwire.WireParams passed through to the server UserRecord.ParamsList on Login

AstaIO 0.953- 12 August 2001
1. Allowed AstaIOProviders to handle applyupdates with no primekeyfields
or update table name for ServerResultMethod tutorial.

AstaIO 0.954- 22 August 2001
1. multiple datamodule supported added to server
2. servermethods and params from providers
3. UserRecord added to all server method events.
4. different event for result set and exec server methods
5. socks support
6. calc field null fix from asta 2.
7. optimized ServerMethod Tester

AstaIO 0.955- 25 August 2001
1. forced all server datasets to close after result set streaming.

AstaIO 0.956- 4 Sept 2001
1. speed up to both windows and linux ASTA native sockets.
2. added Log call to UserRecord
3. published AstaIProviderParams
4. native BCD support
5. SOCKS support
6. surfaced nagle option on ASTA native sockets.
7. added performance tutorial as well as provider testers

AstaIO 0.957- 5 Sept 2001
1. fixed bug in returning ServerMethodExec params
2. updated server tutorials to use changed server method prototypes

AstaIO 0.958- 6 Sept 2001
1. added server broadcast methods (broadcastcodedmessage, broadcastcodedParams)
admin clients are excluded from receiving broadcasts.

2. changes to servermethod params

AstaIO 0.959- 9 Sept 2001
1. changed AstaServerWire to AstaIOServerWire and AstaClient wire to AstaIOClientWire
and added to AstaIOReg.pas in order to allow for abstract wires to be used
2. Changed AstaIOServerWire.OnShowServerMessage to OnLogEvent and overload
recordserver activity with custom integer routines
3. IPWorks SSL implementation
4. Fix for required field in AstaIOCustomDataSet
5. Removed CheckListBox from AstaIOServer Wire and added VisualUserList:TStrings
6. Included Indy tests in this build although indy support is not complete
7. disablestringtrim property.
8. DXSock and Indy Transports extended but not fully tested/implemented.
9. added Logging call to UserRecord
10. Add Var to DataBaseSession for ServerWire.AssignPersistentSessions event

AstaIO 0.96- 14 Sept 2001
1. AstaIOServerWire and AstaIOClient wire included on pallette so that demos
and servers can be coded to be transport neutral.
2. AstaIODOAServer coded (Windows). (Direct Oracle Access)

AstaIO 0.97- 18 Sept 2001
1. MySQL Server support with Zeos and extended properties for AstaIOdBInfo component
for Oracle, MySQL, PostgreSQL and DB2
2. Updated indy transport
3. IbObjects Server
4. updated ServerMethod tester clients.
5. updated of AstaIOLowCore for FreeONTerminate support.
6. Delphi 6 issues warnings on properties that have a private variable, which
used to be standard way of coding. they want it moved to protected. ugh!
7. Created multi transport IBExpress server for use with Astanative,Indy and
IPworks sockets. this will be the new model for servers and clients. 

AstaIO 0.98 4 October
1. SecurityProfile property added to UserList to allow for no clientside SQL.
2. provider DOMethods virtual created for override
3. re-restructure of AstaIODataSets for better inheritance.
4. host property support added for Native Sockets.
5. progress on nested dataset support
6. InternalLoadFromStreamWithFields bug fixed.
7. dbinfo components extended
8. session available now in Login if AstaIOServerWire.ProvideDataBaseSessionForLogin set to true
9. auto password validiation with no code added to AstaDBPlugin.AutoDBLogin
10. bug fixed in dbexpress use of ftFmtBCD fields

AstaIO 1.0- Enough features we can call it 1.0 now 18 October
1. Provider Broadcasts started.
2. NestedDataSet updates added, still WIP but new NestedDataSetManager
componeted added.
3. OnClientError interfaced changed on Server wire and implemented in
AstaNative. Logging now broadcasts client errors. needs more testing.
4. Variant Supported Added to AstaParamLists
5. fixed output params on server methods yet again
6. Indexes coming next from ASTA 2/3.
7. All client wire events, are now NOT executed within a thread. Internally
windows messaging is used to allow the events (ClientLogin,CodedXXXXX events)
to execute in the main thread so that VCL components can be created with no
problems.
8. Added AnObject:TObject to UserRecord
9. Internal messaging format has been changed to add Signature to each messagge
to insure the correct message is being processed on SendGetCalls. other transports
will have to support this.
10. fix to AstaIOParamList for delphi 6 patch

AstaIO 1.01 23 October
1. Windows handled moved to create to support non visual Clientwires where
no loaded method is called.

AstaIO 1.02 23 October
1. fixed ClientMsgWire issue with passing a long string as part of a windows
message
2. addsignature compiler directived activated.
3. http tunneling available
4. add soap to the install

AstaIO 1.06 24 December
1. added DES Encryption

AstaIO 1.061 25 December
1. AstaIOClientWire.SendDataSetTransactions

AstaIO 1.101 28 January 2002
1. added Database String to DatabasePlugin SupplyDBComponent Event
2. AstaIO Conversion Wizard Added
3. RSA Encryption

AstaIO 1.115 27 Feb 2002
1. fixed calc field sql generation
2. udp transport

AstaIO 1.12 19 March
1. indexes and findkey on calc fields


AstaIO 1.27 3 may
1. Blades

AstaIO 1.31 10 may
1. servermethod exec support for skywire clients
2. ftFmtBCD support.
3. fixed filter problemo

AstaIO 1.32 12 may
1. provider refetches.
2. paramlist re-write for d6 types

AstaIO 1.33 12 may
1. AstaIOClientDataSet
2. Refetches for Providers.

AstaIO 1.331
1. paramlist and dataset adjustments for d6 types

AstaIO 1.332
1. midas migration work, nesteddataset

AstaIO 1.341
1. fix for lookup bug
2. AstaIO QA tests included in demo

AstaIO 1.342
1. index change for streaming indexes
2. qa suite updated.

AstaIO 1.343
1. update status fix. added check for field and fkdata

AstaIO 1.344
1. added handled check in AstaIOProvider for refetches

AstaIO 1.345
1. SQLDataSetOnly compiler directive updated for webcacheDataSet
2. non-visual ibobjects server

AstaIO 1.41
1. Index updates
2. stored proc param and  ib  adjustmemtns

AstaIO 1.42
1. multi table update SQL

AstaIO 1.43.
1. database name on server side components
2. fix for blank appname on TUserRecord
3. Paramsevent on remotedataSet

AstaIO 1.44
1. added runtime/design time packages.

AstaIO 1.45
1. nested dataset and change to inheritance on AstaIOClientDataSet

AstaIO 1.47
1. isapi testing

AstaIOUtil.pas

AstaIO 1.48
1. Adjustment in AStaIODatabaseplugin and AStaIOClientRemoteDataSet
for servermethods with providers used in sendProviderTransaction

AstaIO 1.481
1. packet fetch adjust for recordcount<rowstoreturn

AstaIO 1.5
1. for dionsyus
2. AstaIOClientWire.TransactionStart and transactionend;
3. TAstaIOClientQuery multiple parameterzied queries.
4. Delphi 7 support
5. XML Routines add to AstaIOCustomDataSet
6. Soap package required.

AstaIO 1.501
1. change for AstaIOCustomDataSet.CurrentValueDataSetRecord
2. kylix compression fix in AstaIOZLibCompress

AstaIO 1.502
1. memory leak fix for execsql when used in transactions
2. adjustment for bad decryption on the server.

AstaIO 1.503
1. adjustment to Storedprocedure support
2. null option in AstaSQLGenerator

AstaIO 1.504
1. fixed kylix zlib compression

AstaIO 1.505
1. {$ifdef LowCoreCOM} for CoInitialize in AstaIOLowCore.pas
2. adjustment for desencryption

AstaIO 1.506
1. added pda routines to AstaIOReg.pas to force dcu builds for skywire support.
2. added WinSockSocketInit to AstaIOWinBase for 10093 error.
3. encryption fix/change  in AstaIOClientMsgWire  FActualEncryption := FEncryption;

AstaIO 1.507
1. added AstaIOServerwire.UserRecordValid

AstaIO 1.508
1. provider refetches supported with handled set to true
2. adjustment to sockets for network cable pulled
3. oracle zeos server
4. postgressql support

AstaIO 1.509
1. Streaming fix for saving indexes.

AstaIO 1.510
1. change to procedure TServerUserList.DeleteClient(Client: TObject);
when clients disconnect while a server side database process is in progress

AstaIO 1.511
1. string wire adjustment and isapi/http example client for dbisam in isapi dll

AstaIO 1.512
1. fix to change made to #1 in 1.511
2. fix for AstaSQLGenerator

AstaIO 1.513
1. fix for AstaSQLGenerator problem with sending string params over that should be blobs.
Value was being assigned after the datatype was being set.

AstaIO 1.514
1. added quote in tablenames and fieldnames option

AstaIO 1.515
1. version forced update

AstaIO 1.516
1. Added check for classes for runtime package extended support per hardata.

AstaIO 1.517
1. Suspend Events set for AfterPost being called an extra time

AstaIO 1.518
1. DataSets.SetStatelessWire support for stateless support.
when running stateless, each dataset needs it's own copy of a wire
for rapid fire thread safe operation.

AstaIO 1.519
1. added to AstaIOParamList ParamItemAssignField(Dest:TParam;Source: TField);
to be used in
TAstaParamsDataSet.SetParamsFromMaster(DataSet: TDataSet; Index: Integer);

AstaIO 1.520
1. packet fix

AstaIO 1.521
1. adjustment for UDP

AstaIO 1.522
1. offline edits for
2. AstaIOCustomDataset dataset adjustments

AstaIO 1.523.
1. provider transaction changes
2. adjustments for string wire
3. bde size adjustments in AstaIOCustomDataSet.Internal Open
4. Skywire update/tests
5. IBInfo adjustments

AstaIO 1.524
1. Added ServerWire.DisconnectAllUsers method and FBlockUsers:boolean to
stop any clients from connecting during that call.

AstaIO 1.525.
1. added BlockUsers:Boolean as public
2. fixed AstaIOPareamLIst ParamItemAssignField for md problem. datatype must
be assigned first before value on TParamItem.

User Contributions included in this build:

3. Report Builder Dade Example included
4. New Multi Server Example included
5. Anchor Server included

AstaIO 1.526
1. TAstaParamsDataSet.Reload added FLastBookmark:=FAstaList.count 
2. added NOSqLFields to AstaIOProvider.

AstaIO 1.527
1. Serverwire Additions:
    procedure DisconnectClientsByLastActivity(SecondsToAllow:Integer);
    procedure RequestPing(SecondsToAllow:Integer);
new serverping tutorial to show how to use this

AstaIO 1.528
1. FNULLSyntax added as public
2. FAllowPackets:=False;//changed from true
caused some CloseQuery on server falsely be called on closes when running
stateless.

AstaIO 1.529
1. Changes to AstaIOThread and AstaIOSessionList to handle
Database Session pool expansion and an exception when the database
components cannot return a good datamodule/connection.

2. Update to RSA/AES support and updated to SkyWire Emulator
to be able to test AES/RSA. Requires Additional strong encryption
source units and AstaAES and AstaRSA compiler directives to be set.

3. Added UseNullSyntax public property to :
 server side:
 TAstaIOIProvider
 TAstaIOProvider
 client side
  TAstaIOClientQuery
  TAstaIOClientTable
  TAstaIOStoredProc
when True IS NULL will be used in all where clauses where the param value is
NULL when False, regular Param values will be used for null Values.
Most Databases require IS NULL to function properly

This is the first pass and we plan to add a centralized spot to set this
on both client and server.

4. Started to code Large file streaming in chunks from server to client support ported
from Asta 3.

5. daAstaIO.pas included with ReportBuilder 7 support.

AstaIO 1.53
1. fix for RSA Encryption to encrypt password by delaying authentication
until after the keyexchange has been performed.

AstaIO 1.531
1. Added AstaIODataSetPackUtils to remove client dependency on server
units when using AstaIODatasetTransport.
2. Servermethod/provider fix which sometimes caused an invalid index.
3. added code to insure thate UpdtaeWherechange included bookmark fields.
4. adjusted delta dataset to not send appendAndDelete delta records to keep it
in sync with the currentvalues dataset.
5. coded client side broadcast implementation but still not fully tested.
6. TAstaCustomeClientRemoteDataSet.destroy adjusted to check for freenotification
when client wire is destroyed before dataset.

AstaIO 1.532
1. WideString changes to AstaIOCustomDataSet.pas for unicode support.

AstaIO 1.533
1. Adjustment to AstaIOClientRemoteDataSet.pas AutoFetchPacket to true
2. procedure TAstaIODataBasePlugin.DoProcessMultipleExecSQL(U: TUserRecord; DataBaseStr,
                                 SQLQueries:String);
adjusted for failed SQL to rollback

AstaIO 1.534
1. Adjustment to some server logging routines.

AstaIO 1.535
1. ExpresswaySQL
2. More unicode Adjustments

AstaIO 1.536
1. fix for master/detail for provider/query
2. provider broadcasts first pass working.

AstaIO 1.537
1. added freenotification check to AStaIOSocketServer.SetActive

AstaIO 1.538
1. changes for nested dataset support.

AstaIO 1.539
1.provider broadcasts completed
2.WideStrChange undefined for unicode support.

AstaIO 1.54
1. Added Lock calls to AstaIOServerWire.userList routines
2. AstaIOParamlist adjustments for Unicode Support.
3. Load/Save from File added to AstaIOParamLists

AstaIO 1.55
1. xml fixes for ftLargeInt and ftmemo
2. AstaIOClientWire memory leak fixed in SendDataSetTransactions
3. Calc and lookup fields now supported on server side result sets

AstaIO 1.56
1. provider broadcast adjustments

AstaIOUtil.pas


