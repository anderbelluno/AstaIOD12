{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10179: AstaIOIntf.pas 
{
{   Rev 1.0    4/10/2003 6:31:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:26 PM  Steve    Version: 1.505
}
unit AstaIOIntf;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses Windows, ActiveX, SysUtils, Classes;

type
  IAstaServerPluginFunction = interface;
  {:
  }
  IAstaParamItem = interface(IUnknown)
    ['{79243079-51B4-44C0-9700-BB47ED4B8F6C}']
    function GetAsBoolean: Boolean; stdcall;
    function GetAsDate: TDateTime; stdcall;
    function GetAsDateTime: TDateTime; stdcall;
    function GetAsFloat: Double; stdcall;
    function GetAsInteger: Integer; stdcall;
    function GetAsMemoX: PWideChar; stdcall;
    function GetAsOLEVariant: OLEVariant; stdcall;
    function GetAsSmallInt: SmallInt; stdcall;
    function GetAsStringX: PWideChar; stdcall;
    function GetAsTime: TDateTime; stdcall;
    function GetAsWord: Word; stdcall;
    function GetBlob(Buffer: Pointer; var BufferLength: Integer): HRESULT;
      stdcall;
    function GetDataTypeX: Integer; stdcall;
    function GetIsNullX: Boolean; stdcall;
    function GetNameX: PWideChar; stdcall;
    function GetParamTypeX: Integer; stdcall;
    procedure SetAsBoolean(Value: Boolean); stdcall;
    procedure SetAsDate(Value: TDateTime); stdcall;
    procedure SetAsDateTime(Value: TDateTime); stdcall;
    procedure SetAsFloat(Value: Double); stdcall;
    procedure SetAsInteger(Value: Integer); stdcall;
    procedure SetAsMemoX(Value: PWideChar); stdcall;
    procedure SetAsOLEVariant(Value: OLEVariant); stdcall;
    procedure SetAsSmallInt(Value: SmallInt); stdcall;
    procedure SetAsStringX(Value: PWideChar); stdcall;
    procedure SetAsTime(Value: TDateTime); stdcall;
    procedure SetAsWord(Value: Word); stdcall;
    function SetBlob(Buffer: Pointer; BufferLength: Integer): HRESULT;
      stdcall;
    procedure SetDataTypeX(Value: Integer); stdcall;
    procedure SetIsNullX(Value: Boolean); stdcall;
    procedure SetNameX(Value: PWideChar); stdcall;
    procedure SetParamTypeX(Value: Integer); stdcall;
    procedure SetText(AText: PWideChar); stdcall;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsMemoX: PWideChar read GetAsMemoX write SetAsMemoX;
    property AsOLEVariant: OLEVariant read GetAsOLEVariant write
    SetAsOLEVariant;
    property AsSmallInt: SmallInt read GetAsSmallInt write SetAsSmallInt;
    property AsStringX: PWideChar read GetAsStringX write SetAsStringX;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsWord: Word read GetAsWord write SetAsWord;
    {:
    Determines the type of the data.
    Can be one of following:

    0 - ftUnknown (VT_UNKNOWN)
    2 - ftSmallint (VT_I2)
    3 - ftInteger (VT_I4)
    4 - ftWord (VT_UI2)
    5 - ftBoolean (VT_BOOL)
    6 - ftFloat (VT_R8)
    7 - ftCurrency (VT_CY)
    9 - ftDate (VT_DATE)
    10- ftTime (VT_DATE)
    11- ftDateTime (VT_DATE)
    15- ftBlob (VT_BLOB)
    16- ftMemo (VT_BSTR)
    24- ftWideString (VT_BSTR)
    29- ftVariant (VT_VARIANT)
    30- ftInterface (VT_INTERFACE)
    31- ftIDispatch (VT_DISPATCH)
    32- ftGuid (VT_CLSID)
    }
    property DataTypeX: Integer read GetDataTypeX write SetDataTypeX;
    property IsNullX: Boolean read GetIsNullX write SetIsNullX;
    property NameX: PWideChar read GetNameX write SetNameX;
    {:
    Defines the type of the parameter item.
    Can be one of the following:
    0 - ptUnknown
    1 - ptInput
    2 - ptOutput
    3 - ptInputOutput
    4 - ptResult
    }
    property ParamTypeX: Integer read GetParamTypeX write SetParamTypeX;
  end;

  {:
  }
  IAstaParamList = interface(IUnknown)
    ['{91C7B739-BCBD-49C5-93ED-E0B97FCA1F54}']
    {:
    Adds new item and returns reference to it.
    }
    function AddItem(out pItm: IAstaParamItem): HRESULT; stdcall;
    {:
    Adds new item and returns reference to it. Sets value for the item.
    }
    function AddVariantItem(AValue: OleVariant; out pItm: IAstaParamItem):
    HRESULT; stdcall;
    {:
    Clears the list of parameters
    }
    function ClearItems: HRESULT; stdcall;
    {:
    Copies parameters to another list, optionally clearing source list (whose
    CopyTo is called)
    }
    function CopyItemsTo(pLst: IAstaParamList; ClearThis: boolean): HRESULT;
      stdcall;
    {:
    Creates transport block that can be later sent to server or to client
    }
    function CreateTransportBlock(out Buffer: Pointer; out BufferLength:
      integer): HRESULT; stdcall;
    function DeleteItem(pItm: IAstaParamItem): HRESULT; stdcall;
    {:
    Looks for parameter with given name.
    Returns S_OK if the parameter is found, S_FALSE if there is no parameter
    with the given name, or E_INVALIDARG if the name is invalid.
    }
    function FindItem(ParamName: PWideChar; out pItm: IAstaParamItem):
    HRESULT; stdcall;
    function GetItemsCount: Integer; stdcall;
    {:
    Parses transport block and adds data from this block. The list is not
    cleared before the data is set.
    }
    function ParseTransportBlock(Buffer: Pointer; BufferLength: integer):
    HRESULT; stdcall;
  end;

  {:
  }
  IAstaUserRecord = interface(IUnknown)
    ['{9F69DFDC-7AE2-413C-9B7E-DEC4AB3655A7}']
    {:
    Returns application name
    }
    function GetApplicationName: PWideChar; stdcall;
    {:
    returns FConnectTime
    }
    function GetConnectionTime: TDateTime; stdcall;
    function GetParamList(out ParamList: IAstaParamList): HRESULT; stdcall;
  end;

  {:
  }
  IAstaServer = interface(IUnknown)
    ['{3781EB37-3C23-4630-A783-619431B9A297}']
    {:
    Adds handler for custom messages.
    }
    function RegisterPluginFunction(TokenID: Integer; AFunction:
      IAstaServerPluginFunction): HRESULT; stdcall;
    {:
    Adds plugin GUID string to the list of plugins. Server attempts to load
    plugin, identified by the given GUID.
    Plugin must support IAstaServerPlugin interface.
    }
    function RegisterPluginGUID(GUIDStr: string): HRESULT; stdcall;
    function SendParamList(User: IAstaUserRecord; ParamList: IAstaParamList;
      Token: integer): HRESULT; stdcall;
    function SendTransportBlock(User: IAstaUserRecord; Buffer: Pointer;
      BufferLength: integer; Token: integer): HRESULT; stdcall;
    {:
    Removes custom function from the list
    }
    function UnregisterPluginFunction(TokenID: Integer; AFunction:
      IAstaServerPluginFunction): HRESULT; stdcall;
  end;

  {:
  }
  IAstaServerPlugin = interface(IUnknown)
    ['{15D8B159-D5BD-43EA-A6D1-59BDAB4C2604}']
    {:
    Initializes plugin. This method is called after plugin instance is created.
    Most likely plugin will addone or more functions to server by calling
    AstaServer.RegisterPluginFunction
    }
    function InitializePlugin(AServer: IAstaServer): HRESULT; stdcall;
  end;

  {:
  }
  IAstaServerPluginFunction = interface(IUnknown)
    ['{4B80C3A0-20B6-4572-87CD-543B15B2AD63}']
    {:
    This method is called by the server when the message with given Token has
    been received from the client.
    All functions which have been registered for the given token are called
    through this method in order.
    If the function performs final handling, it must set Handled to true. Final
    handling means that no other
    plugin will be invoked and server built-in processing won't be used too.

    This lets us create intermediate plugins, which change the request in some
    way and pass it further to other
    plugins or to built-in handling mechanisms.

    Plugin function is allowed to change request Token ID.
    }
    function Perform(const AToken: Integer; AUserRecord: IAstaUserRecord; out
      Handled: LongBool): HRESULT; stdcall;
  end;

  {:
  }
  IAstaClientCore = interface(IUnknown)
    ['{968AA677-4D1D-4094-BC99-6E14D8C1D492}']
    function Connect: HRESULT; stdcall;
    function CreateParamList(out ParamList: IAstaParamList): HRESULT; stdcall;
    function Disconnect: HRESULT; stdcall;
    function GetCompressionType: Integer; stdcall;
    function GetEncryptionType: Integer; stdcall;
    function GetHost: PWideChar; stdcall;
    function GetISAPIHost: PWideChar; stdcall;
    function GetISAPIPath: PWideChar; stdcall;
    function GetISAPIPort: Word; stdcall;
    function GetPassword: PWideChar; stdcall;
    function GetPort: Word; stdcall;
    function GetProtocol: Integer; stdcall;
    function GetProxyPassword: PWideChar; stdcall;
    function GetProxyUserName: PWideChar; stdcall;
    function GetSocksPassword: PWideChar; stdcall;
    function GetSocksPort: Word; stdcall;
    function GetSocksServer: PWideChar; stdcall;
    function GetSocksUserName: PWideChar; stdcall;
    function GetUserName: PWideChar; stdcall;
    function SendParamList(ParamList: IAstaParamList; Token: DWORD;
      ResponceExpected: boolean): HRESULT; stdcall;
    function SendTransportBlock(Buffer: Pointer; BufferLength: DWORD; Token:
      DWORD; ResponceExpected: boolean): HRESULT; stdcall;
    procedure SetCompressionType(Value: Integer); stdcall;
    procedure SetEncryptionType(Value: Integer); stdcall;
    procedure SetHost(Value: PWideChar); stdcall;
    procedure SetISAPIHost(Value: PWideChar); stdcall;
    procedure SetISAPIPath(Value: PWideChar); stdcall;
    procedure SetISAPIPort(Value: Word); stdcall;
    procedure SetPassword(Value: PWideChar); stdcall;
    procedure SetPort(Value: Word); stdcall;
    procedure SetProtocol(Value: Integer); stdcall;
    procedure SetProxyPassword(Value: PWideChar); stdcall;
    procedure SetProxyUserName(Value: PWideChar); stdcall;
    procedure SetSocksPassword(Value: PWideChar); stdcall;
    procedure SetSocksPort(Value: Word); stdcall;
    procedure SetSocksServer(Value: PWideChar); stdcall;
    procedure SetSocksUserName(Value: PWideChar); stdcall;
    procedure SetUserName(Value: PWideChar); stdcall;
    {:
    Defines compression. Can be one of the following:
    0 - acNoCompression
    1 - acAstaCompress
    2 - (not implemented)
    3 - acAstaZLib
    }
    property CompressionType: Integer read GetCompressionType write
    SetCompressionType;
    {:
    Type of encryption. Can be one of the following:
    0 - etNoEncryption
    1 - etAstaEncrypt
    2 - (not implemented)
    }
    property EncryptionType: Integer read GetEncryptionType write
    SetEncryptionType;
    {:
    Defines the address of the server
    }
    property Host: PWideChar read GetHost write SetHost;
    property ISAPIHost: PWideChar read GetISAPIHost write SetISAPIHost;
    property ISAPIPath: PWideChar read GetISAPIPath write SetISAPIPath;
    {:
    Defines port of ISAPI server
    }
    property ISAPIPort: Word read GetISAPIPort write SetISAPIPort;
    property Password: PWideChar read GetPassword write SetPassword;
    {:
    Defines server port
    }
    property Port: Word read GetPort write SetPort;
    {:
    Defines protocol. Possible values are:

    0 - apTCPIP
    1 - apHTTPTunnel
    2 - apIsapi
    }
    property Protocol: Integer read GetProtocol write SetProtocol;
    property ProxyPassword: PWideChar read GetProxyPassword write
    SetProxyPassword;
    property ProxyUserName: PWideChar read GetProxyUserName write
    SetProxyUserName;
    property SocksPassword: PWideChar read GetSocksPassword write
    SetSocksPassword;
    property SocksPort: Word read GetSocksPort write SetSocksPort;
    property SocksServer: PWideChar read GetSocksServer write SetSocksServer;
    property SocksUserName: PWideChar read GetSocksUserName write
    SetSocksUserName;
    property UserName: PWideChar read GetUserName write SetUserName;
  end;

  {:
  This interface is used to get individual items from the list. It is
  implemented by the same object as IAstaParamList.
  }
  IEnumAstaParamItems = interface(IUnknown)
    ['{178C805B-6AD8-41AA-A783-0FF816AA739E}']
    function Clone(out enm: IEnumAstaParamItems): HRESULT; stdcall;
    function Next(celt: DWORD; out elt; pceltFetched: PDWORD): HResult; stdcall;
    function Reset: HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
  end;

  {:
  This interface must be implemented by IAstaClientCore users.
  It is used by IAstaClientCore to notify the client about incoming data.
  }
  IAstaClientCallback = interface(IUnknown)
    ['{D6B78EEA-FBEB-497B-A83F-DB5E719FFF24}']
    {:
    The message is called when data have been received from server.
    }
    procedure OnMessage(ParamList: IAstaParamList); stdcall;
    {:
    This method is called when connection state of the client core changes.
    }
    procedure OnStateChange(NewState: DWORD; Error: DWORD); stdcall;
  end;

procedure Register;

implementation

procedure Register;
begin
end;

initialization
end.


