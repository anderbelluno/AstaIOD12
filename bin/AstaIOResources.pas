{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10289: AstaIOResources.pas 
{
{   Rev 1.0    4/10/2003 6:32:00 AM  Steve
}
{
{   Rev 1.0    11/8/2002 5:48:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:06 PM  Steve    Version: 1.505
}
unit AstaIOResources;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

resourcestring
  SSecurity ='Not Authorized';
  SFieldOutOfRange = 'Value of field ''%s'' is out of range';
  SBadFieldType = 'Field ''%s'' is of an unsupported type';
  SClientDisconnect='Disconnect from Server';
  SNotConnected = 'Not Connected to Server';
  SEmptySQLStatement = 'No SQL statement available';
  SNoCachedUpdates = 'Not in cached update mode';
  SLoginError = 'Cannot login to server ''%s''';
  SDataSetOpen = 'Cannot perform this operation on an open dataset';
  SDataSetClosed = 'Cannot perform this operation on a closed dataset';
  SDatabaseOpen = 'Cannot perform this operation on an open database';
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SCircularDataLink = 'Circular datalinks are not allowed';

  SFieldNotFound = 'Field ''%s'' not found';
  SNoFileName = 'No FileName specified';

  SDataSetEditMode = 'Dataset is not in edit mode';

  SFileAccessError = 'Error Accessing File ''%s''';
  SDatabaseNameMissing = 'Database name missing';
  SNoTableName = 'Missing TableName property';
  SNoSProcName = 'Missing StoredProcName property';
  SNoProviderName = 'Missing ProviderName property';
  SNoServerMethodName = 'Missing ServerMethodName property';
  SNoMSrcDSrc = 'Missing MasterSource or DataSource';
  SCompoExists = 'A ''%s'' component named ''%s'' already exist';

  STableReadOnly = 'Table ''%s'' is read only';

  SPRovDataSetReadOnly = 'DataSet is read only';
  SPRovDataSetNoInsert = 'Insert is not allowed';
  SPRovDataSetNoDelete = 'Delete is not allowed';
  SPRovDataSetNoEdit = 'Edit is not allowed';
  SProvCannotChangeCommandText = 'CommandText changes are not allowed';

  SNotInEditMode = 'Not in edit or Insert Mode';

  SNoFoundOneExp = 'No record found, but one expected';
  SMulFoundOneExp = 'Multiple records found, but only one expected';

  SRecordChanged = 'Record changed by another user';

  SDelete = 'Record was not deleted. The underlying data could have been changed since retrieval';
  SInsert = 'Record was not inserted. The underlying data could have been changed since retrieval';
  SPost = 'Record was not posted. The underlying data could have been changed since retrieval';

  SDeleteRec = 'Error encountered while deleting record';
  SInsertRec = 'Error encountered while inserting record';
  SPostRec = 'Error encountered while posting record';

  STranBegin = 'Error encountered while beginning transaction';
  STranCommit = 'Error encountered while committing transaction';
  STranRollBack = 'Error encountered while rolling back transaction';

  SFieldIndexError = 'Field index out of range';

  SParamListToParamDataSetProblem = 'ParamListToParamDataSet problem with ''%s''';
  SOpenQuote = 'There is an open quote in your SQL String';
  SNoSortName = '''%s'' has not been defined';
  SLoginCanceled = 'Login Canceled';
  SNoSortsDefined = 'No Sorts have been defined';
  SUnAbleToLoadFromFile = 'Unable to load from ''%s''';
  SFieldNamesDescValues = 'FieldNames and Descending Values must both be equal in count';
  SFieldOffSetError = 'Field Offset Error ''%s''';
  SNilListToPrepare = 'Nil list pass to prepare from design list';
  SDuplicateParamName = 'Duplicate Param Name';
  SCheckSyntax = 'Please check Syntax :param:param must be :param:param';
  SUserRecordNotAvailableFromLogin = 'User Record Not Available from Client Login';
  SNoAssignedSocket ='No ClientWire Assigned.';
  SNoAssignedWireAndFields ='No ClientWire and Persistent fields Assigned.';

  SNoUpdateTable = 'No UpdateTable specified';
  SNoPrimeFieldsGen = 'No Prime fields defined';
  SNoPrimeFields = 'No Prime fields defined for ''%s''';

  SErrorFindExecComp = 'Error finding an Exec Component';
  SMissingDataSet = 'Missing DataSet property';
  SNoProvider = 'No provider available';
  SNoProviderAssigned = 'No provider assigned for updates';
  SNotAQuery = 'Dataset is not a query';
  SMissingCurrentDataSet = 'Missing Current DataSet property';
  SMissingOldValuesDataSet = 'Missing OldValues DataSet property';
  SProviderNotFound = 'Provider ''%s'' not found';
  SServerMethodNotFound = 'ServerMethod ''%s'' not found';

  SNothingToSend = 'Nothing to Send!';
  SNoDataFromServer = 'No Data From Server';
  SDataSetNotActive = 'DataSet Not active';
  SCircularDSReference = 'Circular Reference to DataSource';
  SFieldOffSet = 'Field Offset Error ''%s''';
  SFieldNDescValue = 'FieldNames and Descending Values must both be equal in count!';
  SFieldDescArray = 'Fields and descending Array must be the same size!';
  SDataSetFieldUndefined = 'DataSetField is undefined !';
  SMissingUnaryFilterEval = 'Missing Unary Filter Eval:';
  SExpectingFieldNode = 'Expecting Field node';
  SMissingBinaryFilterEval = 'Missing Binary Filter Eval:';
  SMissingExtendedFilterEval = 'Missing Extended Filter Eval:';
  SMissingFunctionalFilterEval = 'Missing Functional Expression Eval: ';
  SMissingnodeTypeFilterEval = 'Missing node Type Filter Eval:';
  SUnknownFunciton = 'Unknown function call: ';
  SMissingHashItem = 'AstaHash:Missing HashItem!';
  SInfiniteLoop = 'Infinte Loop';
  SAlreadyAssembled = 'Assemble has already been called!';
  SProblemCreatedParams = 'Problem created params from ''%s''';
  SParameterNotFound = 'Parameter Not Found ''%s''';
  SNoMetadataDS = 'No MetaData DataSet';
  SNoTransactionComponentSupplied='No Component to start a transaction available';
  SNoTransactionBeginEvent='No Transaction Begin Event Supplied';
  SNoTransactionEndEvent = 'No Transaction End Event Supplied';
  SProvParamsCountMisMatch = 'Number of parameters between server provider and client does not match';
  SNoEventOnCreatedPooledSession = 'No Event Assigned to OnCreatedPooledSession';
  SEnterUserName='Enter User Name and Password';

  SUseOffLine = 'DataSet not active.' + #13 + #10 + 'Select OffLine to edit this property while not connected to a server';
  SPrimary='Prime Key Not Found';
  SMissingDatASource='Data source missing';
  SLinkDesigner='Designer Link Eerror';
  SThreadResReq = 'Thread Destroy response required';

  SNoValuesToUpdate = 'No Values found for SQL';
  SInsertConstructNotSupported = 'Construct for INSERT not supported';
  SNoQueryOpen = 'No Open Query On Server';
  SFailedLogin ='The Server could not authenticate the Username/Password.'+#13#10+'Login Failed';
  SNo10061 = '10061: Connection refused.  Verify that a server is available.';

  SNo10065= '10065: The client cannot reach the destination network. ' +
                    'Verify that your target network is available and that a '+
                    'firewall is not blocking your protocol and port';
  SNo10053='10053: Software caused connection abort.  The server terminated the connection. ' +
                       'It is possible that the server has terminated.';
  SNo10054='10054: Software caused connection abort.  The server terminated the connection. ' +
                       'It is possible that the server has terminated.';

  SSocketError = 'Socket Error: %d on %s';
  SValidValues = 'Valid values are %s through %s';
  SNoLoginUserName = 'Please enter the UserName';

  SDbInfoRdbmsNotSupported = 'RdbmsInfoKind not supported';
  SNoDBPlugin = 'DataBasePlugin is not assigned';
  SInvalidPropertyValue = 'Invalid property value';
  SRequestDenied        = 'Request not allowed';
  SNoStatelessUserListAvailable='No Stateless UserList available';

  SApplyUpdatesNotImplemented = 'Apply Updates not implemented';
  SFailedCompileCustRecConstr = 'Failed to compile custom record constraint'#10'[%s]';
  SFailedCompileImpRecConstr = 'Failed to compile imported record constraint'#10'[%s]';
  SFailedCompileCustFldConstr1 = 'Failed to compile field [';
  SFailedCompileCustFldConstr2 = '] custom constraint'#10'[%s]';
  SFailedCompileImpFldConstr1 = 'Failed to compile field [';
  SFailedCompileImpFldConstr2 = '] imported constraint'#10'[%s]';
  SFailedCompileDefFldExpr1 = 'Failed to compile field [';
  SFailedCompileDefFldExpr2 = '] default expression'#10'[%s]';
  SDefExprNotFound = 'Default expression for field [%s] does not found';
  SDefExprEvalFailed = 'Default expression evaluation for field [%s] failed.'#10'[%s]';
  SCheckNotFound = 'Check expression does not found';
  SFailedFieldCustConstr1 = 'Field [';
  SFailedFieldCustConstr2 = '] custom constraint failed.'#10'[%s]';
  SFailedFieldImpConstr1 = 'Field [';
  SFailedFieldImpConstr2 = '] imported constraint failed.'#10'[%s]';
  SFailedRecCustConstr = 'Record custom constraint failed.'#10'[%s]';
  SFailedRecImpConstr = 'Record imported constraint failed.'#10'[%s]';

  SViolUniqueIndex = 'Violation of unique index %s(%s)';
  SDuplRecInUniqueIndex = 'Duplicate record in unique index %s(%s)';
  SIndexNotFound = 'Index with name ''%s'' does not found';
  SNoRecAtPos = 'No record at position %d';
  SAggNotFound = 'Aggregate with name ''%s'' does not found';
  STooManyAggs = 'Too many aggregates';
  SNoRecords = 'No Records';
  FNoFieldName = 'FieldName is required';
  SNoCircularReference = 'Circular provider references not allowed';
  SRefreshError = 'Must apply updates before refreshing data';
  SInfoProvMismatch = 'Info provider mismatch';
  SNoParameterizedTransactions='No Parameterized Queries defined';
  SPersistentSessionRequired='PersistentSession required';
  SUserRecordInvalid = 'Invalid TUserRecord';
implementation

end.



