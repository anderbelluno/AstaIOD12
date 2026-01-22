{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10095: AstaIOConst.pas 
{
{   Rev 1.0    4/10/2003 6:30:24 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:52 PM  Steve    Version: 1.505
}
unit AstaIOConst;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
{$ifdef mswindows} uses messages; {$endif}
const
  AstaBaseToken = -30000;
  ATCodedMessage = AstaBaseToken + 1;
  ATCodedStream = AstaBaseToken + 2;
  ATClientLogin = AstaBaseToken + 3;
  ATClientSendGet = AstaBaseToken + 4;
  ATCodedParams = AstaBaseToken + 5;
  ATUpgradeRequest = AstaBaseToken + 6;
  ATSendGetCodedParams = AstaBaseToken + 7;

  // Added by EM, 16 Jan 2001
  ATPDAParamList = AstaBaseToken + 8;
  ATPDAStream    = AstaBaseToken + 9;
  ATPDAInternal  = AstaBaseToken + 10;
  ATPDAGetFile   = AstaBaseToken + 11;
  ATPalmUpdateReq= AstaBaseToken + 12;

  ATCodedMessages    = AstaBaseToken + 16;
  ATMessagePacker    = AstaBaseToken + 17;
  ATNamedCodedParams = AstaBaseToken + 18;
  ATFileSegmentSend = AstaBaseToken +  19;
  ATInternalParams   = AstaBaseToken + 29;//internal ParamList Messaging

  ATIPWaitingMailReceived = 1;

  //?????
  ATJavaLogin        = AstaBaseToken + 30;
  ATJavaParamList    = AstaBaseToken + 31;

  ATKeysExchange = AstaBaseToken + 32;
  ATPDASendFile      = AstaBaseToken + 33;
  ATPalmSendDB       = AstaBaseToken + 34;
  AtAsyncExec    = AstaBaseToken  + 35;
  AstaBaseDBToken = -40000;
  ATDBSelect = AstaBaseDBToken + 1;
  ATDBException = AstaBaseDBToken + 2;
  ATMetadata = AstaBaseDBToken + 3;
  ATDBExec = AstaBaseDBToken + 4;
  ATDBExecProc = AstaBaseDBToken + 5;
  ATDBProcSelect = AstaBaseDBToken + 6;
  ATDBProvider = AstaBaseDBToken + 7;
  ATDBServerMethod = AstaBaseDBToken + 8;
  ATDBDataSetTransaction = AstaBaseDBToken + 9;
  ATDTransactionCommit = AstaBaseDBToken + 10;
  ATDBServerMethodExec = AstaBaseDBToken + 11;
  ATDBProviderTransaction = AstaBaseDBToken + 12;

  ATDBIProvider = AstaBaseDBToken + 13;
  ATDBIProviderExec = AstaBaseDBToken + 14;
  ATDBIProviderFetchParams = AstaBaseDBToken + 15;
  ATDBIProviderModify = AstaBaseDBToken + 16;
  ATDBCloseQuery    = AstaBaseDBToken + 17;
  ATDBGetNextPacket = AstaBaseDBToken + 18;

  ATDBServerMethodTransaction = AstaBaseDBToken + 19;
  ATDBMultipleExec            = AstaBaseDBToken + 20;
  ATDBBroadCast               = AstaBaseDBToken + 21;
  ATDBMultiDataSetTransaction = AstaBaseDBToken + 22;
  ATDMultiTransactionCommit   = AstaBaseDBToken + 23;
  ATDBPersistentTransactionStart= AstaBaseDBToken +24;
  ATDBPersistentTransactionEnd  = AstaBaseDBToken +25;
  ATDBBroadCastUnregister       = AstaBaseDBToken + 26;
  ATDBExpressWayDataSetSelect   = AstaBaseDBToken  + 27;

  PLInternalParamToken          = 1000;
  PLAutoUpgradeToken            = PLInternalParamToken+1;

  AstaLoginSuccess = 1;
  AstaLoginFail = 0;
  AstaExecSuccess = 1;

  apPacketReturn = -1;
  apNoMorePackets = -2;
  CustomDataSetParamPad = 5;

  AstaIOServerPingRequest = -2000;
  
  astaFakePDAFlag   = -2;
  AstaFakeJavaFlag  = -3;
  adminLogCodeBase    =1000;
  adminLogDisconnect  =1002;
  adminLoginSuccess   =1003;
  adminLogUserList    =1004;
  adminLogClientError =1005;

  //
  serverAuthSuccess           = 0;
  serverAuthErrorUndefined    = 1;
  serverAuthErrorNoLic	      = 2;
  serverAuthErrorInvPass      = 3;
  serverAuthErrorNoRegToken   = 4;
  serverAuthErrorNoAuth       = 5;
  serverResponseTooBig        = 6;

  MsgAstaPDAAcquireToken      = 0;
  MsgAstaPDARevokeToken       = 1;

  flagPasswordSecure	      = 0;
  flagPasswordPlainText	      = 255;

  ErrAstaPDAUpdateNoReply     = -1;
  ErrAstaPDAUpdateCoreMissing = -2;
  MsgAstaPDANoUpdateCore      = 0;
  MsgAstaPDAUpdateCore        = LongInt($FFFFFFFF);

  AstaIOFullFileReceived      = -1;
{$IFDEF LINUX}
  WM_USER = $0400;
{$ENDIF}
 WM_WireBase       = WM_USER + 12623;
 WM_Coded_Msg      = WM_WireBase +1;
 WM_Coded_Stream   = WM_WireBase +2;
 WM_Coded_ParamList= WM_WireBase +3;
 WM_Login          = WM_WireBase +4;
 WM_Disconnect     = WM_WireBase +5;
 WM_Connect        = WM_WireBase +6;
 WM_Upgrade        = WM_WireBase +7;
 WM_Status_bar     = WM_WireBase +8;
 WM_ProviderBroadcast= WM_WireBase+9;
 WM_ProviderParamList= WM_WireBase+10;

 ClientWireLoginParams='~ClientWireInfo';
 UpgradeInfoConst     ='~UpgradeInfo';
 UpgradeMsgConst      ='~UpgradeMsg';
 UpgradeFileConst     ='~UpgradeFile';
 UpgradeExtraParams   ='~UpgradeParams';
 UpgradeInfoParamConst='~UpgradeParamList';
 UpgradeHostconst     ='~UpgradeHost';
 sfld_BookMark = 'BookMark';
 sfld_Delta = 'Delta';
 sfld_SavePoint = 'SavePoint';
 sfld_ErrorCode = 'ErrorCode';
 sfld_ErrorInfo = 'ErrorInfo';
 OldValsSpecFields = 5;
type
  TLoginType = (ltLoginDlg, ltLoginNoDlg, ltNoChallenge);

  TAstaEncryption=(etNoEncryption,etUserDefined,etAESEncrypt,etDESEncrypt);
  TAstaCompression=(acNoCompression,acUserDefined,acAstaZLib);
  TAstaKeysExchange=(keNoKeysExchange,keRSA);
  TZlibCompressLevel= 0..9;
  PtrInteger = ^Integer;
  TAutoUpgradeResponseBits = (tauNoUpgrade, tauAutoUpgrade, tauUpgradeAvailable,
    tauHttpLocation, tauShowMessage, tauUserChoice);
  TAutoUpgradeResponse = set of TAutoUpgradeResponseBits;

  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  PSmallInt     = ^SmallInt;
  PInteger      = ^Integer;
  PSingle       = ^Single;
  PDouble       = ^Double;
  PDate         = ^Double;
  PDispatch     = ^IDispatch;
  PPDispatch    = ^PDispatch;
  PError        = ^LongWord;
  PWordBool     = ^WordBool;
  PUnknown      = ^IUnknown;
  PPUnknown     = ^PUnknown;
  PByte         = ^Byte;
  PPWideChar    = ^PWideChar;

implementation

end.

