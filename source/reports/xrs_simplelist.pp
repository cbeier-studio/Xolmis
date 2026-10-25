unit xrs_simplelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, data_types, data_schema, xrs_core, fpjson, SynCommons;

type
  { TReportSimpleList }

  TReportSimpleList = class(TXolmisReport)
  private
    FContextJSON: TJSONObject;
    FDataSet: TDataSet;
    FTableSchema: TTableSchema;
    FReportTitle: string;
    FFilterLines: TStringList;
    FParentTitle: string;
    FParentItems: TStringList;
    function DescribeField(AField: TSearchField): string;
    procedure AddGroupLines(AGroups: TSearchGroups);
    procedure Initialize(ADataSet: TDataSet; ATableSchema: TTableSchema; const AReportTitle: string);
    procedure AddVisibleColumns(AColumns: TJSONArray);
    procedure StackColumns(AColumns: TJSONArray);
    procedure AddRows(const AColumns: TJSONArray; ARows: TJSONArray);
  protected
    procedure FetchData; override;
    procedure BuildContent; override;
  public
    constructor Create(ADataSet: TDataSet; ATableType: TTableType); reintroduce; overload;
    constructor Create(ADataSet: TDataSet; const AReportTitle: string); reintroduce; overload;
    destructor Destroy; override;

    { Describes the active filters in the report header; nothing is shown when there are none. }
    procedure SetSearch(ASearch: TCustomSearch);

    { Describes the current record of the parent table in a box above the list, when the
      list is a submodule of that parent. The record is read immediately; nothing is shown
      when the dataset is nil or empty. }
    procedure SetParentRecord(AParentTable: TTableType; AParentDataSet: TDataSet);

    class function GetReportTitle: string; override;
    class function GetContexts: TModuleContexts; override;
    class function IsGlobal: Boolean; override;
    class function CreateForDataSet(ADataSet: TDataSet; ATableType: TTableType): TXolmisReport; override;
  end;

implementation

uses
  xrs_mustache,
  utils_locale;

{ TReportSimpleList }

constructor TReportSimpleList.Create(ADataSet: TDataSet; ATableType: TTableType);
var
  TableSchema: TTableSchema;
begin
  inherited Create;
  if not Assigned(DBSchema) then
    raise Exception.Create('The database schema is not loaded.');

  TableSchema := DBSchema.GetTable(ATableType);
  if not Assigned(TableSchema) then
    raise Exception.CreateFmt('No schema is registered for table type %d.', [Ord(ATableType)]);

  Initialize(ADataSet, TableSchema, Format(rsReportTitleSimpleList, [TableSchema.DisplayName]));
end;

constructor TReportSimpleList.Create(ADataSet: TDataSet; const AReportTitle: string);
begin
  inherited Create;
  Initialize(ADataSet, nil, AReportTitle);
end;

procedure TReportSimpleList.Initialize(ADataSet: TDataSet; ATableSchema: TTableSchema;
  const AReportTitle: string);
begin
  if not Assigned(ADataSet) then
    raise EArgumentNilException.Create('The report dataset cannot be nil.');
  if not ADataSet.Active then
    raise EDatabaseError.Create('The report dataset must be open.');
  if AReportTitle = EmptyStr then
    raise EArgumentException.Create('The report title cannot be empty.');

  FDataSet := ADataSet;
  FTableSchema := ATableSchema;
  FReportTitle := AReportTitle;
  FContextJSON := TJSONObject.Create;
  FFilterLines := TStringList.Create;
  FParentItems := TStringList.Create;

  LoadTemplate(REPORT_TEMPLATE_SIMPLE_LIST);
end;

destructor TReportSimpleList.Destroy;
begin
  FParentItems.Free;
  FFilterLines.Free;
  FContextJSON.Free;
  inherited Destroy;
end;

function TReportSimpleList.DescribeField(AField: TSearchField): string;
var
  V1, V2: string;
begin
  V1 := AField.Value1;
  V2 := AField.Value2;
  if AField.DataType = sdtBoolean then
  begin
    if (V1 = '1') or SameText(V1, 'true') then
      V1 := rsCaptionYes
    else
      V1 := rsCaptionNo;
  end;

  case AField.Criteria of
    crNull:     Result := AField.DisplayName + ': ' + rsReportFilterEmpty;
    crNotNull:  Result := AField.DisplayName + ': ' + rsReportFilterNotEmpty;
    crNotEqual: Result := AField.DisplayName +     ' <> ' + V1;
        crBetween:  Result := AField.DisplayName + ': ' + V1 + ' - ' + V2;
        crMoreThan: Result := AField.DisplayName + ' >= ' + V1;
        crLessThan: Result := AField.DisplayName + ' <= ' + V1;
  else
    Result := AField.DisplayName + ': ' + V1;
  end;
end;

procedure TReportSimpleList.AddGroupLines(AGroups: TSearchGroups);
var
  G, F: Integer;
  Line, S: string;
begin
  for G := 0 to AGroups.Count - 1 do
  begin
    Line := EmptyStr;
    for F := 0 to AGroups[G].Fields.Count - 1 do
    begin
      if AGroups[G].Fields[F].DisplayName = EmptyStr then
        Continue;
      S := DescribeField(AGroups[G].Fields[F]);
      if Line <> EmptyStr then
        Line := Line + '  /  ';
      Line := Line + S;
    end;
    if Line <> EmptyStr then
      FFilterLines.Add(Line);
  end;
end;

procedure TReportSimpleList.SetSearch(ASearch: TCustomSearch);
begin
  FFilterLines.Clear;
  if not Assigned(ASearch) then
    Exit;
  AddGroupLines(ASearch.TextFilters);
  AddGroupLines(ASearch.QuickFilters);
end;

procedure TReportSimpleList.SetParentRecord(AParentTable: TTableType; AParentDataSet: TDataSet);
var
  ParentSchema: TTableSchema;
  FieldSchema: TFieldSchema;
  Field: TField;
  Ordered: TFieldSchemaList;
  I, J: Integer;
  Text, Caption: string;
begin
  FParentTitle := EmptyStr;
  FParentItems.Clear;
  if not Assigned(AParentDataSet) or not AParentDataSet.Active or AParentDataSet.IsEmpty or
    not Assigned(DBSchema) then
    Exit;

  ParentSchema := DBSchema.GetTable(AParentTable);
  if not Assigned(ParentSchema) then
    Exit;

  // Ordered fields first (by ReportOrder), then the remaining ones in schema order
  Ordered := TFieldSchemaList.Create(False);
  try
    for FieldSchema in ParentSchema.Fields do
      if FieldSchema.ReportVisible and (FieldSchema.ReportOrder > 0) then
      begin
        J := Ordered.Count;
        while (J > 0) and (Ordered[J - 1].ReportOrder > FieldSchema.ReportOrder) do
          Dec(J);
        Ordered.Insert(J, FieldSchema);
      end;
    for FieldSchema in ParentSchema.Fields do
      if FieldSchema.ReportVisible and (FieldSchema.ReportOrder <= 0) then
        Ordered.Add(FieldSchema);

    for I := 0 to Ordered.Count - 1 do
    begin
      FieldSchema := Ordered[I];
      Field := AParentDataSet.FindField(FieldSchema.Name);
      if Field = nil then
        Continue;
      if Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo] then
        Text := Trim(Field.AsString)
      else
        Text := Trim(Field.DisplayText);
      if Text = EmptyStr then
        Continue;

      Caption := FieldSchema.ReportLabel;
      if Caption = EmptyStr then
        Caption := Field.DisplayLabel;
      if Caption = EmptyStr then
        Caption := FieldSchema.Name;
      FParentItems.Add(Caption + '=' + Text);
    end;
  finally
    Ordered.Free;
  end;

  if FParentItems.Count > 0 then
    FParentTitle := Format(rsReportParentRecord, [ParentSchema.DisplayName]);
end;

procedure TReportSimpleList.AddVisibleColumns(AColumns: TJSONArray);
var
  I, J: Integer;
  Field: TField;
  FieldSchema: TFieldSchema;
  Column, PreviousColumn: TJSONObject;
  Order, PreviousOrder: Integer;
  UseSchemaVisibility: Boolean;
begin
  UseSchemaVisibility := False;
  if Assigned(FTableSchema) then
    for FieldSchema in FTableSchema.Fields do
      if FieldSchema.ReportVisible then
      begin
        UseSchemaVisibility := True;
        Break;
      end;

  for I := 0 to FDataSet.FieldCount - 1 do
  begin
    Field := FDataSet.Fields[I];
    if not Field.Visible then
      Continue;

    FieldSchema := nil;
    if Assigned(FTableSchema) then
      FieldSchema := FTableSchema.GetField(Field.FieldName);
    if UseSchemaVisibility and (not Assigned(FieldSchema) or not FieldSchema.ReportVisible) then
      Continue;

    Column := TJSONObject.Create;
    Column.Add('field_name', Field.FieldName);
    if Field.DisplayLabel <> EmptyStr then
      Column.Add('label', Field.DisplayLabel);
    if Field.DisplayLabel = EmptyStr then
    begin
      if Assigned(FieldSchema) and (FieldSchema.ReportLabel <> EmptyStr) then
        Column.Add('label', FieldSchema.ReportLabel)
      else
        Column.Add('label', Field.FieldName);
    end;

    if Assigned(FieldSchema) then
    begin
      Column.Add('css_class', FieldSchema.ReportCssClass);
      Column.Add('report_order', FieldSchema.ReportOrder);
      Column.Add('stack_with', FieldSchema.ReportStackWith);
    end
    else
    begin
      Column.Add('css_class', EmptyStr);
      Column.Add('report_order', 0);
    end;

    // Long text columns get a max width (see .memo in the template) so they do not squeeze the others
    if Field.DataType in [ftMemo, ftWideMemo, ftFmtMemo] then
      Column.Strings['css_class'] := Trim(Column.Get('css_class', '') + ' memo');

    Order := 0;
    if Assigned(FieldSchema) then
      Order := FieldSchema.ReportOrder;

    J := AColumns.Count;
    while J > 0 do
    begin
      PreviousColumn := TJSONObject(AColumns.Items[J - 1]);
      PreviousOrder := PreviousColumn.Get('report_order', 0);
      if Order = 0 then
      begin
        if PreviousOrder = 0 then
          Break;
      end
      else if (PreviousOrder = 0) or (PreviousOrder > Order) then
        Break;
      Dec(J);
    end;
    AColumns.Insert(J, Column);
  end;

  StackColumns(AColumns);
end;

procedure TReportSimpleList.StackColumns(AColumns: TJSONArray);
var
  I, J: Integer;
  Column, Primary, Item: TJSONObject;
  Stack: TJSONArray;
  PrimaryName: String;
begin
  // Backwards, so removing a column does not shift the ones still to be visited
  for I := AColumns.Count - 1 downto 0 do
  begin
    Column := TJSONObject(AColumns.Items[I]);
    PrimaryName := Column.Get('stack_with', '');
    if PrimaryName = EmptyStr then
      Continue;

    Primary := nil;
    for J := 0 to AColumns.Count - 1 do
      if (J <> I) and SameText(TJSONObject(AColumns.Items[J]).Get('field_name', ''), PrimaryName) and
        (TJSONObject(AColumns.Items[J]).Get('stack_with', '') = EmptyStr) then
      begin
        Primary := TJSONObject(AColumns.Items[J]);
        Break;
      end;
    if Primary = nil then
      Continue;

    Item := TJSONObject.Create;
    Item.Add('field_name', Column.Get('field_name', ''));
    Item.Add('label', Column.Get('label', ''));
    Item.Add('css_class', Trim(Column.Get('css_class', '') + ' stack-secondary'));

    Stack := Primary.Get('stack', TJSONArray(nil));
    if Stack = nil then
    begin
      Stack := TJSONArray.Create;
      Primary.Add('stack', Stack);
    end;
    Stack.Insert(0, Item);
    AColumns.Delete(I);
  end;

  for I := 0 to AColumns.Count - 1 do
  begin
    Primary := TJSONObject(AColumns.Items[I]);
    Stack := Primary.Get('stack', TJSONArray(nil));
    if Stack = nil then
      Continue;
    for J := 0 to Stack.Count - 1 do
      Primary.Strings['label'] := Primary.Get('label', '') + ' / ' +
        TJSONObject(Stack.Items[J]).Get('label', '');
  end;
end;

procedure TReportSimpleList.AddRows(const AColumns: TJSONArray; ARows: TJSONArray);

  function GetFieldText(const AFieldName: String): String;
  var
    F: TField;
  begin
    F := FDataSet.FieldByName(AFieldName);
    if F.DataType in [ftMemo, ftWideMemo, ftFmtMemo] then
      Result := F.AsString
    else
      Result := F.DisplayText;
  end;

var
  I, J: Integer;
  Bookmark: TBookmark;
  HasBookmark: Boolean;
  Row, Cell, Item: TJSONObject;
  Cells, Values, Stack: TJSONArray;
  Text: String;
  Total, Index, LastPercent, Percent: Integer;
begin
  Total := FDataSet.RecordCount;
  Index := 0;
  LastPercent := -1;
  HasBookmark := not FDataSet.IsEmpty;
  if HasBookmark then
    Bookmark := FDataSet.GetBookmark;
  FDataSet.DisableControls;
  try
    FDataSet.First;
    while not FDataSet.EOF do
    begin
      Cells := TJSONArray.Create;
      try
        for I := 0 to AColumns.Count - 1 do
        begin
          Row := TJSONObject(AColumns.Items[I]);
          Values := TJSONArray.Create;
          Cell := TJSONObject.Create;
          Cell.Add('css_class', Row.Get('css_class', ''));
          Cell.Add('values', Values);
          Cells.Add(Cell);

          Item := TJSONObject.Create;
          Item.Add('value', GetFieldText(Row.Get('field_name', '')));
          Item.Add('css_class', Row.Get('css_class', ''));
          Values.Add(Item);

          Stack := Row.Get('stack', TJSONArray(nil));
          if Stack <> nil then
            for J := 0 to Stack.Count - 1 do
            begin
              Text := GetFieldText(TJSONObject(Stack.Items[J]).Get('field_name', ''));
              if Text = EmptyStr then
                Continue;
              Item := TJSONObject.Create;
              Item.Add('value', Text);
              Item.Add('css_class', TJSONObject(Stack.Items[J]).Get('css_class', ''));
              Values.Add(Item);
            end;
        end;
        Row := TJSONObject.Create;
        Row.Add('cells', Cells);
        ARows.Add(Row);
      except
        Cells.Free;
        raise;
      end;
      FDataSet.Next;
      Inc(Index);
      if Total > 0 then
      begin
        Percent := Index * 100 div Total;
        if Percent <> LastPercent then
        begin
          LastPercent := Percent;
          DoProgress(rsProgressPreparing, Percent);
        end;
      end;
    end;
  finally
    try
      if HasBookmark and FDataSet.BookmarkValid(Bookmark) then
        FDataSet.GotoBookmark(Bookmark);
    finally
      try
        if HasBookmark then
          FDataSet.FreeBookmark(Bookmark);
      finally
        FDataSet.EnableControls;
      end;
    end;
  end;
end;

procedure TReportSimpleList.FetchData;
var
  ColArray, RowArray, FilterArray, ParentArray: TJSONArray;
  FilterItem: TJSONObject;
  I: Integer;
begin
  FreeAndNil(FContextJSON);
  FContextJSON := TJSONObject.Create;
  FContextJSON.Add('report_title', FReportTitle);
  FContextJSON.Add('theme_css', GetThemeCSS);
  FContextJSON.Add('page_size_css', GetPageSizeCSS);
  FContextJSON.Add('page_body_width', GetPageBodyWidthCSS);
  FContextJSON.Add('page_body_min_height', GetPageBodyMinHeightCSS);

  ColArray := TJSONArray.Create;
  AddVisibleColumns(ColArray);
  if ColArray.Count = 0 then
  begin
    ColArray.Free;
    raise Exception.Create('The report dataset has no visible fields.');
  end;
  FContextJSON.Add('columns', ColArray);
  // Reduz a fonte da tabela conforme o número de colunas
  if ColArray.Count <= 8 then
    FContextJSON.Add('table_font_size', '8pt')
  else if ColArray.Count <= 12 then
    FContextJSON.Add('table_font_size', '7pt')
  else if ColArray.Count <= 16 then
    FContextJSON.Add('table_font_size', '6pt')
  else
    FContextJSON.Add('table_font_size', '5pt');
  RowArray := TJSONArray.Create;
  AddRows(ColArray, RowArray);
  FContextJSON.Add('rows', RowArray);

  if FFilterLines.Count > 0 then
  begin
    FContextJSON.Add('has_filters', True);
    FContextJSON.Add('filters_title', rsReportFiltersApplied);
    FilterArray := TJSONArray.Create;
    for I := 0 to FFilterLines.Count - 1 do
    begin
      FilterItem := TJSONObject.Create;
      FilterItem.Add('text', FFilterLines[I]);
      FilterArray.Add(FilterItem);
    end;
    FContextJSON.Add('filters', FilterArray);
  end;

  if FParentItems.Count > 0 then
  begin
    FContextJSON.Add('has_parent', True);
    FContextJSON.Add('parent_title', FParentTitle);
    ParentArray := TJSONArray.Create;
    for I := 0 to FParentItems.Count - 1 do
    begin
      FilterItem := TJSONObject.Create;
      FilterItem.Add('label', FParentItems.Names[I]);
      FilterItem.Add('value', FParentItems.ValueFromIndex[I]);
      ParentArray.Add(FilterItem);
    end;
    FContextJSON.Add('parent_fields', ParentArray);
  end;
end;

procedure TReportSimpleList.BuildContent;
var
  MustacheRenderer: TXolmisMustacheRenderer;
begin
  MustacheRenderer := TXolmisMustacheRenderer.Create(UTF8Encode(FTemplateContent));
  try
    MustacheRenderer.SetContextJSON(RawUTF8(FContextJSON.AsJSON));
    HTMLContent := MustacheRenderer.RenderHTML;
  finally
    MustacheRenderer.Free;
  end;
end;

class function TReportSimpleList.GetReportTitle: string;
begin
  Result := rsReportSimpleList;
end;

class function TReportSimpleList.GetContexts: TModuleContexts;
begin
  Result := [mcGlobal, mcCaptures, mcTaxonomy, mcLocations];
end;

class function TReportSimpleList.IsGlobal: Boolean;
begin
  Result := False;
end;

class function TReportSimpleList.CreateForDataSet(ADataSet: TDataSet; ATableType: TTableType): TXolmisReport;
begin
  Result := TReportSimpleList.Create(ADataSet, ATableType);
end;

initialization
  ReportRegistry.RegisterReport(TReportSimpleList);

end.
