{
 /***************************************************************************
                                grids.pas
                                ---------

                   Initial Revision : Fri Oct 30 CST 2020

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Grids;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Types, Web, Graphics, StdCtrls;

const
  DEFCOLWIDTH         = 64;

type
  TCustomGrid = class;
  TGridColumn = class;

  EGridException = class(Exception);

  { TGridColumnTitle }

  TGridColumnTitle = class(TPersistent)
  private
    fColumn: TGridColumn;
    fAlignment: TAlignment;
    fCaption: TCaption;
    fIsDefaultAlignment: Boolean;
    fIsDefaultCaption: Boolean;
    fIsDefaultLayout: Boolean;
    fLayout: TTextLayout;
    function GetAlignment: TAlignment;
    function GetLayout: TTextLayout;
    function IsAlignmentStored: Boolean;
    function IsCaptionStored: Boolean;
    function IsLayoutStored: Boolean;
    procedure SetAlignment(aValue: TAlignment);
    procedure SetLayout(aValue: TTextLayout);
    procedure WriteCaption(aWriter: TWriter);
  protected
    function GetDefaultAlignment: TAlignment;
    function GetDefaultCaption: String; virtual;
    function GetDefaultLayout: TTextLayout;
    function GetCaption: TCaption;
    procedure SetCaption(const aValue: TCaption); virtual;
  public
    constructor Create(aColumn: TGridColumn); virtual;
    procedure Assign(aSource: TPersistent); override;

    function IsDefault: Boolean;

    property Column: TGridColumn read fColumn;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
    property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
  end;

  { TGridColumn }

  TGridColumn = class(TCollectionItem)
  private
    fAlignment: TAlignment;
    fLayout: TTextLayout;
    fTitle: TGridColumnTitle;
    function GetGrid: TCustomGrid;
    procedure SetAlignment(aValue: TAlignment);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetTitle(aValue: TGridColumnTitle);
  protected
    function GetDefaultAlignment: TAlignment; virtual;
    function GetDefaultLayout: TTextLayout; virtual;

    procedure ColumnChanged; virtual;
    function CreateTitle: TGridColumnTitle; virtual;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;

    function IsDefault: Boolean;

    property Grid: TCustomGrid read GetGrid;
  published
    property Alignment: TAlignment read fAlignment write SetAlignment;
    property Layout: TTextLayout read fLayout write SetLayout;
    property Title: TGridColumnTitle read fTitle write SetTitle;
  end;

  { TGridColumns }

  TGridColumns = class(TCollection)
  private
    fGrid: TCustomGrid;
    function GetColumn(Index: Integer): TGridColumn;
    function GetEnabled: Boolean;
    function GetVisibleCount: Integer;
    procedure SetColumn(Index: Integer; AValue: TGridColumn);
  protected
    procedure Update(aItem: TCollectionItem); override;
  public
    constructor Create(aGrid: TCustomGrid; aItemClass: TCollectionItemClass);

    function Add: TGridColumn;
    procedure Clear;
    function ColumnByTitle(const aTitle: String): TGridColumn;
    function IndexOf(aColumn: TGridColumn): Integer;
    function IsDefault: Boolean;
    function RealIndex(aIndex: Integer): Integer;

    property Enabled: Boolean read GetEnabled;
    property Grid: TCustomGrid read fGrid;
    property Items[Index: Integer]: TGridColumn read GetColumn write SetColumn; default;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  { TCustomGrid }

  TCustomGrid = class(TCustomControl)
  private
    fBorderColor: TColor;
    fCols: TIntegerList;
    fColumns: TGridColumns;
    fContentTable: TJSHTMLTableElement;
    fDefColWidth: Integer;
    fDefRowHeight: Integer;
    fEditorMode: Boolean;
    fFixedColor: TColor;
    fFixedCols: Integer;
    fFixedColsTable: TJSHTMLTableElement;
    fFixedGridLineColor: TColor;
    fFixedRows: Integer;
    fFixedRowsTable: TJSHTMLTableElement;
    fFixedTopLeftTable: TJSHTMLTableElement;
    fFlat: Boolean;
    fGridBorderStyle: TBorderStyle;
    fGridLineColor: TColor;
    fGridLineStyle: TPenStyle;
    fGridLineWidth: Integer;
    fOnTopLeftChanged: TNotifyEvent;
    fRealizedDefColWidth: Integer;
    fRealizedDefRowHeight: Integer;
    fRows: TIntegerList;
    fScrollBars: TScrollStyle;
    fTopLeft: TPoint;
    fUpdateCount: Integer;
    procedure AdjustGrid(aIsColumn: Boolean; aOld, aNew: Integer);
    procedure CheckFixed(aCols, aRows, aFixedCols, aFixedRows: Integer);
    function DefaultColWidthIsStored: Boolean;
    function DefaultRowHeightIsStored: Boolean;
    procedure DoTopLeftChanged;
    function GetColCount: Integer;
    function GetColumns: TGridColumns;
    function GetColWidth(aCol: Integer): Integer;
    function GetDefColWidth: Integer;
    function GetDefRowHeight: Integer;
    function GetFixedColor: TColor; virtual;
    function GetRowCount: Integer;
    function GetRowHeights(aRow: Integer): Integer;
    function IsColumnsStored: Boolean;
    procedure SetBorderColor(aValue: TColor);
    procedure SetBorderStyle(aValue: TBorderStyle);
    procedure SetColCount(aValue: Integer);
    procedure SetColumns(aValue: TGridColumns);
    procedure SetColWidth(aCol: Integer; aValue: Integer);
    procedure SetDefColWidth(aValue: Integer);
    procedure SetDefRowHeight(aValue: Integer);
    procedure SetEditorMode(aValue: Boolean);
    procedure SetFixedColor(aValue: TColor); virtual;
    procedure SetFixedCols(aValue: Integer);
    procedure SetFixedGridLineColor(AValue: TColor);
    procedure SetFixedRows(aValue: Integer);
    procedure SetFlat(aValue: Boolean);
    procedure SetGridLineColor(aValue: TColor);
    procedure SetGridLineStyle(aValue: TPenStyle);
    procedure SetGridLineWidth(aValue: Integer);
    procedure SetRowCount(aValue: Integer);
    procedure SetRowHeights(aRow: Integer; aValue: Integer);
    procedure SetScrollBars(aValue: TScrollStyle);
  protected
    function GetCells(aCol, aRow: Integer): String; virtual;
    procedure Changed; override;
    function ColumnFromGridColumn(aColumn: Integer): TGridColumn;
    function ColumnIndexFromGridColumn(aColumn: Integer): Integer;
    procedure ColumnsChanged(aColumn: TGridColumn);
    function CreateColumns: TGridColumns; virtual;
    function CreateHandleElement: TJSHTMLElement; override;
    procedure DoScroll; override;
    function FirstGridColumn: Integer; virtual;
    function GetDefaultRowHeight: Integer; virtual;
    function GridColumnFromColumnIndex(aColumnIndex: Integer): Integer;
    procedure InternalSetColCount(aCount: Integer);
    procedure InvalidateCell(aCol, aRow: Integer; aRedraw: Boolean); overload;
    function IsColumnIndexValid(aIndex: Integer): Boolean;
    function IsColumnIndexVariable(aIndex: Integer): Boolean;
    function IsRowIndexValid(aIndex: Integer): Boolean;
    function IsRowIndexVariable(aIndex: Integer): Boolean;
    procedure SizeChanged(aOldColCount, aOldRowCount: Integer); virtual;
    procedure TopLeftChanged; virtual;
    procedure UpdateBorderStyle;
    procedure VisualChange; virtual;

    property BorderColor: TColor read fBorderColor write SetBorderColor default cl3DDKShadow;
    property BorderStyle: TBorderStyle read fGridBorderStyle write SetBorderStyle default bsSingle;
    property ColCount: Integer read GetColCount write SetColCount default 5;
    property ColWidths[Col: Integer]: Integer read GetColWidth write SetColWidth;
    property Columns: TGridColumns read GetColumns write SetColumns stored IsColumnsStored;
    property DefaultColWidth: Integer read GetDefColWidth write SetDefColWidth stored DefaultColWidthIsStored;
    property DefaultRowHeight: Integer read GetDefRowHeight write SetDefRowHeight stored DefaultRowHeightIsStored;
    property FixedColor: TColor read GetFixedColor write SetFixedColor default clBtnFace;
    property FixedGridLineColor: TColor read fFixedGridLineColor write SetFixedGridLineColor default cl3DDKShadow;
    property Flat: Boolean read fFlat write SetFlat default False;
    property GridLineColor: TColor read fGridLineColor write SetGridLineColor default clSilver;
    property GridLineStyle: TPenStyle read fGridLineStyle write SetGridLineStyle;
    property GridLineWidth: Integer read fGridLineWidth write SetGridLineWidth default 1;
    property RowCount: Integer read GetRowCount write SetRowCount default 5;
    property RowHeights[Row: Integer]: Integer read GetRowHeights write SetRowHeights;
    property ScrollBars: TScrollStyle read fScrollBars write SetScrollBars default ssAutoBoth;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    function ClearCols: Boolean;
    function ClearRows: Boolean;
    procedure InvalidateCell(aCol, aRow: Integer); overload;

    property EditorMode: Boolean read fEditorMode write SetEditorMode;
    property FixedCols: Integer read fFixedCols write SetFixedCols default 1;
    property FixedRows: Integer read fFixedRows write SetFixedRows default 1;

    property OnTopLeftChanged: TNotifyEvent read fOnTopLeftChanged write fOnTopLeftChanged;
  end;

  TCellProps = class
    Data: TObject;
    Text: String;
  end;

  TColRowProps = class
    Size: Integer;
  end;

  { TVirtualGrid }

  TVirtualGrid = class
  private type
    TCellPropsArray = array of TCellProps;
    TCellPropsArrayArray = array of TCellPropsArray;
    TColRowPropsArray = array of TColRowProps;
  private
    fColCount: Integer;
    fRowCount: Integer;
    fCellArr: TCellPropsArrayArray;
    fColArr, fRowArr: TColRowPropsArray;
    function GetCells(aCol, aRow: Integer): TCellProps;
    function GetCols(aCol: Integer): TColRowProps;
    function GetRows(aRow: Integer): TColRowProps;
    procedure SetCells(aCol, aRow: Integer; aValue: TCellProps);
    procedure SetColCount(aValue: Integer);
    procedure SetCols(aCol: Integer; aValue: TColRowProps);
    procedure SetRowCount(aValue: Integer);
    procedure SetRows(aRow: Integer; aValue: TColRowProps);
  protected

  public
    procedure Clear;
    function GetDefaultCell: TCellProps;
    function GetDefaultColRow: TColRowProps;

    property ColCount: Integer read fColCount write SetColCount;
    property RowCount: Integer read fRowCount write SetRowCount;

    property Celda[Col, Row: Integer]: TCellProps read GetCells write SetCells;
    property Cols[Col: Integer]: TColRowProps read GetCols write SetCols;
    property Rows[Row: Integer]: TColRowProps read GetRows write SetRows;
  end;

  { TCustomDrawGrid }

  TCustomDrawGrid = class(TCustomGrid)
  protected
    FGrid: TVirtualGrid;
    function CreateVirtualGrid: TVirtualGrid; virtual;
    procedure SizeChanged(aOldColCount, aOldRowCount: Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    property ColCount;
    property RowCount;
  end;

  { TCustomStringGrid }

  TCustomStringGrid = class(TCustomDrawGrid)
  private
    fModified: Boolean;
  protected
    function GetCells(aCol, aRow: Integer): String; override;
    procedure SetCells(aCol, aRow: Integer; const aValue: String); virtual;

    property Modified: Boolean read fModified write fModified;
  public
    property Cells[Col, Row: Integer]: String read GetCells write SetCells;
    {property Cols[Index: Integer]: TStrings read GetCols write SetCols;
    property Objects[Col, Row: Integer]: TObject read GetObjects write SetObjects;
    property Rows[Index: Integer]: TStrings read GetRows write SetRows;}
  end;

  TWStringGrid = class(TCustomStringGrid)
  published
    property ColCount;
    property Columns;
    property DefaultColWidth;
    property DefaultRowHeight;
    property RowCount;
  end;

implementation

uses
  TypInfo, WCLStrConsts;

{ TCustomStringGrid }

function TCustomStringGrid.GetCells(aCol, aRow: Integer): String;
var
  c: TCellProps;
begin
  Result := '';
  c := fGrid.Celda[aCol, aRow];
  if Assigned(c) then
    Result := c.Text;
end;

procedure TCustomStringGrid.SetCells(aCol, aRow: Integer; const aValue: String);

  procedure UpdateCell;
  begin
    InvalidateCell(aCol, aRow);
  end;

var
  c: TCellProps;
begin
  c := fGrid.Celda[aCol, aRow];
  if Assigned(c) then begin
    c.Text := aValue;
    UpdateCell;
    fModified := True;
  end else if aValue <> '' then begin
    c := TCellProps.Create;
    c.Text := aValue;
    fGrid.Celda[aCol, aRow] := c;
    UpdateCell;
    fModified := True;
  end;
end;

{ TCustomDrawGrid }

function TCustomDrawGrid.CreateVirtualGrid: TVirtualGrid;
begin
  Result := TVirtualGrid.Create;
end;

procedure TCustomDrawGrid.SizeChanged(aOldColCount, aOldRowCount: Integer);
begin
  if aOldColCount <> ColCount then begin
    fGrid.ColCount := ColCount;
  end;

  if aOldRowCount <> RowCount then begin
    fGrid.RowCount := RowCount;
  end;
end;

constructor TCustomDrawGrid.Create(aOwner: TComponent);
begin
  fGrid := CreateVirtualGrid;
  inherited Create(aOwner);
end;

destructor TCustomDrawGrid.Destroy;
begin
  fGrid.Free;
  inherited Destroy;
end;

{ TVirtualGrid }

procedure TVirtualGrid.SetColCount(aValue: Integer);
begin
  if fColCount = aValue then
    Exit;

  fColCount := aValue;

  SetLength(fColArr, fColCount);
  SetLength(fCellArr, fRowCount, fRowCount);
end;

function TVirtualGrid.GetCells(aCol, aRow: Integer): TCellProps;
begin
  Result := fCellArr[aCol, aRow];
end;

function TVirtualGrid.GetCols(aCol: Integer): TColRowProps;
begin
  Result := fColArr[aCol];
end;

function TVirtualGrid.GetRows(aRow: Integer): TColRowProps;
begin
  Result := fRowArr[aRow];
end;

procedure TVirtualGrid.SetCells(aCol, aRow: Integer; aValue: TCellProps);
begin
  fCellArr[aCol, aRow].Free;
  fCellArr[aCol, aRow] := aValue;
end;

procedure TVirtualGrid.SetCols(aCol: Integer; aValue: TColRowProps);
begin
  fColArr[aCol].Free;
  fColArr[aCol] := aValue;
end;

procedure TVirtualGrid.SetRowCount(aValue: Integer);
begin
  if fRowCount = AValue then
    Exit;

  fRowCount := aValue;

  SetLength(fRowArr, fRowCount);
  SetLength(fCellArr, fRowCount, fRowCount);
end;

procedure TVirtualGrid.SetRows(aRow: Integer; aValue: TColRowProps);
begin
  fRowArr[aRow].Free;
  fRowArr[aRow] := aValue;
end;

procedure TVirtualGrid.Clear;
begin
  fRowCount := 0;
  fColCount := 0;
end;

function TVirtualGrid.GetDefaultCell: TCellProps;
begin
  Result := TCellProps.Create;
end;

function TVirtualGrid.GetDefaultColRow: TColRowProps;
begin
  Result := TColRowProps.Create;
end;

{ TGridColumnTitle }

function TGridColumnTitle.IsCaptionStored: Boolean;
begin
  Result := not fIsDefaultCaption;
end;

function TGridColumnTitle.GetAlignment: TAlignment;
begin
  if fIsDefaultAlignment then
    Result := GetDefaultAlignment
  else
    Result := fAlignment;
end;

function TGridColumnTitle.GetLayout: TTextLayout;
begin
  if fIsDefaultLayout then
    Result := GetDefaultLayout
  else
    Result := fLayout;
end;

function TGridColumnTitle.IsAlignmentStored: Boolean;
begin
  Result := not fIsDefaultAlignment;
end;

function TGridColumnTitle.IsLayoutStored: Boolean;
begin
  Result := not fIsDefaultLayout;
end;

procedure TGridColumnTitle.SetAlignment(aValue: TAlignment);
begin
  if fIsDefaultAlignment or (fAlignment <> aValue) then begin
    fIsDefaultAlignment := False;
    fAlignment := aValue;
    fColumn.ColumnChanged;
  end;
end;

procedure TGridColumnTitle.SetLayout(aValue: TTextLayout);
begin
  if fIsDefaultLayout or (fLayout <> aValue) then begin
    fIsDefaultLayout := False;
    fLayout := aValue;
    fColumn.ColumnChanged;
  end;
end;

procedure TGridColumnTitle.WriteCaption(aWriter: TWriter);
var
  s: TCaption;
  pi: TTypeMemberProperty;
begin
  s := Caption;
  if Assigned(aWriter.OnWriteStringProperty) then begin
    pi := GetPropInfo(Self, 'Caption');
    aWriter.OnWriteStringProperty(aWriter, Self, pi, s);
  end;
  aWriter.WriteString(s);
end;

function TGridColumnTitle.GetDefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TGridColumnTitle.GetCaption: TCaption;
begin
  if fIsDefaultCaption then
    Result := GetDefaultCaption
  else
    Result := fCaption;
end;

procedure TGridColumnTitle.SetCaption(const aValue: TCaption);
begin
  if fIsDefaultCaption or (fCaption <> aValue) then begin
    fIsDefaultCaption := False;
    fCaption := aValue;
    fColumn.ColumnChanged;
  end;
end;

function TGridColumnTitle.GetDefaultCaption: String;
begin
  Result := 'Title';
end;

function TGridColumnTitle.GetDefaultLayout: TTextLayout;
begin
  Result := tlCenter;
end;

constructor TGridColumnTitle.Create(aColumn: TGridColumn);
begin
  fColumn := aColumn;

  fIsDefaultAlignment := True;
  fIsDefaultCaption := True;
  fIsDefaultLayout := True;

  fAlignment := taLeftJustify;
  fLayout := tlCenter;
end;

procedure TGridColumnTitle.Assign(aSource: TPersistent);
begin
  if aSource is TGridColumnTitle then begin
    Caption := TGridColumnTitle(aSource).Caption;
  end else
    inherited Assign(aSource);
end;

function TGridColumnTitle.IsDefault: Boolean;
begin
  Result := fIsDefaultCaption and
            fIsDefaultAlignment and
            fIsDefaultLayout;
end;

{ TGridColumn }

function TGridColumn.GetGrid: TCustomGrid;
begin
  if Collection is TGridColumns then
    Result := (Collection as TGridColumns).Grid
  else
    Result := nil;
end;

procedure TGridColumn.SetAlignment(aValue: TAlignment);
begin
  if fAlignment <> aValue then begin
    fAlignment := aValue;
    ColumnChanged;
  end;
end;

procedure TGridColumn.SetLayout(AValue: TTextLayout);
begin
  if fLayout = aValue then
    Exit;
  fLayout := aValue;
  ColumnChanged;
end;

procedure TGridColumn.SetTitle(aValue: TGridColumnTitle);
begin
  fTitle.Assign(aValue);
end;

function TGridColumn.GetDefaultAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TGridColumn.GetDefaultLayout: TTextLayout;
begin
  Result := tlCenter;
end;

procedure TGridColumn.ColumnChanged;
begin
  Changed(False);
end;

function TGridColumn.CreateTitle: TGridColumnTitle;
begin
  Result := TGridColumnTitle.Create(Self);
end;

constructor TGridColumn.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);

  fTitle := CreateTitle;

  fAlignment := GetDefaultAlignment;
end;

destructor TGridColumn.Destroy;
begin
  fTitle.Free;
  inherited Destroy;
end;

function TGridColumn.IsDefault: Boolean;
begin
  Result := fTitle.IsDefault;
end;

{ TGridColumns }

function TGridColumns.GetColumn(Index: Integer): TGridColumn;
begin
  Result := TGridColumn(inherited Items[Index]);
end;

function TGridColumns.GetEnabled: Boolean;
begin
  Result := VisibleCount > 0;
end;

function TGridColumns.GetVisibleCount: Integer;
begin
  Result := Count;
end;

procedure TGridColumns.SetColumn(Index: Integer; AValue: TGridColumn);
begin
  Items[Index].Assign(aValue);
end;

procedure TGridColumns.Update(aItem: TCollectionItem);
begin
  fGrid.ColumnsChanged(TGridColumn(aItem));
end;

constructor TGridColumns.Create(aGrid: TCustomGrid;
  aItemClass: TCollectionItemClass);
begin
  inherited Create(aItemClass);
  fGrid := aGrid;
end;

function TGridColumns.Add: TGridColumn;
begin
  Result := TGridColumn(inherited Add);
end;

procedure TGridColumns.Clear;
begin
  BeginUpdate;
  try
    inherited Clear;
  finally
    EndUpdate;
  end;
end;

function TGridColumns.ColumnByTitle(const aTitle: String): TGridColumn;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Title.Caption, aTitle) then begin
      Result := Items[i];
      Break;
    end;
end;

function TGridColumns.IndexOf(aColumn: TGridColumn): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i] = aColumn then begin
      Result := i;
      Break;
    end;
end;

function TGridColumns.IsDefault: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do begin
    Result := Result and Items[i].IsDefault;
    if not Result then
      Break;
  end;
end;

function TGridColumns.RealIndex(aIndex: Integer): Integer;
begin
  if aIndex >= Count then
    Result := -1
  else
    Result := aIndex;
end;

{ TCustomGrid }

procedure TCustomGrid.AdjustGrid(aIsColumn: Boolean; aOld, aNew: Integer);

  procedure AdjustList(aList: TIntegerList; aCount: Integer);
  begin
    { add new elements with the default size }
    while aList.Count < aCount do
      aList.Add(-1);

    aList.Count := aCount;
  end;

var
  oldcount: Integer;
begin
  if aIsColumn then begin
    AdjustList(fCols, aNew);

    oldcount := RowCount;

    SizeChanged(aOld, oldcount);
  end else begin
    AdjustList(fRows, aNew);

    oldcount := ColCount;

    SizeChanged(oldcount, aOld);
  end;
end;

procedure TCustomGrid.CheckFixed(aCols, aRows, aFixedCols, aFixedRows: Integer);
begin
  if aFixedCols < 0 then
    raise EGridException.Create('FixedCols < 0');
  if aFixedRows < 0 then
    raise EGridException.Create('FixedRows < 0');

  if csLoading in ComponentState then
    Exit;

  if aFixedCols > aCols then
    raise EGridException.Create(rsFixedColsTooBig);
  if aFixedRows > aRows then
    raise EGridException.Create(rsFixedRowsTooBig);
end;

function TCustomGrid.DefaultColWidthIsStored: Boolean;
begin
  Result := fDefColWidth >= 0;
end;

function TCustomGrid.DefaultRowHeightIsStored: Boolean;
begin
  Result := fDefRowHeight >= 0;
end;

procedure TCustomGrid.DoTopLeftChanged;
begin
  TopLeftChanged;
  VisualChange;
end;

function TCustomGrid.GetColCount: Integer;
begin
  Result := fCols.Count;
end;

function TCustomGrid.GetColumns: TGridColumns;
begin
  Result := fColumns;
end;

function TCustomGrid.GetColWidth(aCol: Integer): Integer;
begin
  if IsColumnIndexValid(aCol) then
    Result := FCols[aCol]
  else
    Result := -1;

  if Result < 0 then
    Result := DefaultColWidth;
end;

function TCustomGrid.GetDefColWidth: Integer;
begin
  if fDefColWidth < 0 then begin
    if fRealizedDefColWidth <= 0 then
      fRealizedDefColWidth := DEFCOLWIDTH;
    Result := fRealizedDefColWidth;
  end else
    Result := fDefColWidth;
end;

function TCustomGrid.GetDefRowHeight: Integer;
begin
  if fDefRowHeight < 0 then begin
    if fRealizedDefRowHeight <= 0 then
      fRealizedDefRowHeight := GetDefaultRowHeight;
    Result := fRealizedDefRowHeight;
  end else
    Result := fDefRowHeight;
end;

function TCustomGrid.GetFixedColor: TColor;
begin
  Result := fFixedColor;
end;

function TCustomGrid.GetRowCount: Integer;
begin
  Result := fRows.Count;
end;

function TCustomGrid.GetRowHeights(aRow: Integer): Integer;
begin
  if IsRowIndexValid(aRow) then
    Result := FRows[aRow]
  else
    Result := -1;

  if Result < 0 then
    Result := DefaultRowHeight;
end;

function TCustomGrid.IsColumnsStored: Boolean;
begin
  Result := Columns.Enabled;
end;

procedure TCustomGrid.SetBorderColor(aValue: TColor);
begin
  if fBorderColor = aValue then
    Exit;
  fBorderColor := aValue;
  if BorderStyle <> bsNone then
    Changed;
end;

procedure TCustomGrid.SetBorderStyle(aValue: TBorderStyle);
begin
  if fGridBorderStyle = aValue then
    Exit;

  fGridBorderStyle := aValue;
  UpdateBorderStyle;
end;

procedure TCustomGrid.SetColCount(aValue: Integer);
begin
  if Columns.Enabled then
    raise EGridException.Create('Use Columns property to add/remove columns');
  InternalSetColCount(AValue);
end;

procedure TCustomGrid.SetColumns(aValue: TGridColumns);
begin
  fColumns.Assign(aValue);
end;

procedure TCustomGrid.SetColWidth(aCol: Integer; aValue: Integer);
begin
  if not IsColumnIndexValid(aCol) then
    Exit;

  if aValue < 0 then
    aValue := -1;

  if fCols[aCol] = aValue then
    Exit;

  fCols[aCol] := aValue;
  Changed;
end;

procedure TCustomGrid.SetDefColWidth(aValue: Integer);
begin
  if fDefColWidth = aValue then
    Exit;

  fDefColWidth := aValue;
  Changed;
end;

procedure TCustomGrid.SetDefRowHeight(aValue: Integer);
begin
  if fDefRowHeight = aValue then
    Exit;

  fDefRowHeight := aValue;
  Changed;
end;

procedure TCustomGrid.SetEditorMode(aValue: Boolean);
begin
  if fEditorMode = aValue then
    Exit;

  fEditorMode := aValue;
end;

procedure TCustomGrid.SetFixedColor(aValue: TColor);
begin
  if fFixedColor = aValue then
    Exit;
  fFixedColor := aValue;
  Invalidate;
end;

procedure TCustomGrid.SetFixedCols(aValue: Integer);
begin
  if fFixedCols = aValue then
    Exit;

  CheckFixed(ColCount, RowCount, aValue, FixedRows);

  if EditorMode then
    EditorMode := False;

  fFixedCols := aValue;
  fTopLeft.x := aValue;

  if not (csLoading in ComponentState) then
    DoTopLeftChanged;
end;

procedure TCustomGrid.SetFixedGridLineColor(AValue: TColor);
begin
  if FFixedGridLineColor=AValue then Exit;
  FFixedGridLineColor:=AValue;
end;

procedure TCustomGrid.SetFixedRows(aValue: Integer);
begin
  if fFixedRows = aValue then
    Exit;

  CheckFixed(ColCount, RowCount, FixedCols, aValue);

  if EditorMode then
    EditorMode := False;

  fFixedRows := aValue;
  fTopLeft.y := aValue;

  if not (csLoading in ComponentState) then
    DoTopLeftChanged;
end;

procedure TCustomGrid.SetFlat(aValue: Boolean);
begin
  if fFlat = aValue then
    Exit;

  fFlat := aValue;
end;

procedure TCustomGrid.SetGridLineColor(aValue: TColor);
begin
  if fGridLineColor = aValue then
    Exit;
  fGridLineColor := aValue;
  Invalidate;
end;

procedure TCustomGrid.SetGridLineStyle(aValue: TPenStyle);
begin
  if fGridLineStyle = aValue then
    Exit;
  fGridLineStyle:=AValue;
  Invalidate;
end;

procedure TCustomGrid.SetGridLineWidth(aValue: Integer);
begin
  if fGridLineWidth = aValue then
    Exit;
  fGridLineWidth := aValue;
  Invalidate;
end;

procedure TCustomGrid.SetRowCount(aValue: Integer);
var
  old: Integer;
begin
  old := fRows.Count;
  if aValue = old then
    Exit;

  if aValue >= 0 then begin
    if EditorMode and (aValue < old) then
      EditorMode := False;

    AdjustGrid(False, old, aValue);
  end else
    ClearRows;
end;

procedure TCustomGrid.SetRowHeights(aRow: Integer; aValue: Integer);
begin
  if not IsRowIndexValid(aRow) then
    Exit;

  if aValue < 0 then
    aValue := -1;

  if fRows[aRow] = aValue then
    Exit;

  fRows[aRow] := aValue;
  Changed;
end;

procedure TCustomGrid.SetScrollBars(aValue: TScrollStyle);
begin
  if fScrollBars = aValue then
    Exit;
  fScrollBars := aValue;
  Changed;
end;

function TCustomGrid.GetCells(aCol, aRow: Integer): String;
begin
  Result := '';
end;

procedure TCustomGrid.Changed;

  procedure AdjustRows(aTable: TJSHTMLTableElement; aCount: LongInt);
  begin
    if aTable.rows.length <> aCount then begin
      while aTable.rows.length > aCount do
        aTable.deleteRow(aTable.rows.length - 1);

      while aTable.rows.length < aCount do
        aTable.insertRow(aTable.rows.length);
    end;
  end;

  procedure AdjustCells(aRow: TJSHTMLTableRowElement; aCount: LongInt);
  var
    cell: TJSHTMLTableDataCellElement;
  begin
    if aRow.cells.length <> aCount then begin
      while aRow.cells.length > aCount do
        aRow.deleteCell(aRow.cells.length - 1);

      while aRow.cells.length < aCount do begin
        cell := TJSHTMLTableDataCellElement(aRow.insertCell(aRow.cells.length));
        cell.appendChild(document.createElement('div'));
      end;
    end;
  end;

  procedure UpdateCell(aCell: TJSHTMLTableDataCellElement; aCol, aRow: LongInt; aColumn: TGridColumn; aIsLastCol, aIsLastRow: Boolean);
  var
    content: TJSHTMLElement;
    w, h: LongInt;
    style: TJSCSSStyleDeclaration;
    bs: String;
    alignment: TAlignment;
    layout: TTextLayout;
  begin
    content := TJSHTMLElement(aCell.getElementsByTagName('div')[0]);

    if Assigned(aColumn) and (aRow = 0) then
      content.textContent := aColumn.Title.Caption
    else
      content.textContent := GetCells(aCol, aRow);

    alignment := taLeftJustify;
    layout := tlCenter;
    if Assigned(aColumn) then begin
      if aRow = 0 then begin
        alignment := aColumn.Title.Alignment;
        layout := aColumn.Title.Layout;
      end else begin
        alignment := aColumn.Alignment;
        layout := aColumn.Layout;
      end;
    end;

    w := fCols[aCol];
    if w < 0 then
      w := DefaultColWidth;

    if w < 0 then
      content.style.removeProperty('width')
    else
      content.style.setProperty('width', IntToStr(w) + 'px');

    h := fRows[aRow];
    if h < 0 then
      h := DefaultRowHeight;

    if h < 0 then begin
      content.style.removeProperty('height');
      content.style.removeProperty('line-height');
    end else begin
      content.style.setProperty('height', IntToStr(h) + 'px');
      content.style.setProperty('line-height', IntToSTr(h) + 'px');
    end;

    style := aCell.style;

    style.SetProperty('white-space', 'nowrap');

    content.style.setProperty('text-align', AlignmentToCSSAlignment(alignment));
    { does not yet work :/ }
    content.style.setProperty('vertical-align', TextLayoutToCSSVerticalAlign(layout));

    bs := PenStyleToCSSBorderStyle(fGridLineStyle);
    style.SetProperty('border-left-style', bs);
    style.SetProperty('border-top-style', bs);
    if aIsLastCol then
      style.SetProperty('border-right-style', bs)
    else
      style.RemoveProperty('border-right-style');
    if aIsLastRow then
      style.SetProperty('border-bottom-style', bs)
    else
      style.RemoveProperty('border-bottom-style');
    style.SetProperty('border-width', IntToStr(fGridLineWidth) + 'px');

    style.SetProperty('border-color', JSColor(fGridLineColor));
  end;

  procedure UpdateFixedCell(aCell: TJSHTMLTableDataCellElement; aCol, aRow: LongInt; aColumn: TGridColumn; aIsLastCol, aIsLastRow: Boolean);
  var
    c: TColor;
  begin
    UpdateCell(aCell, aCol, aRow, aColumn, aIsLastCol, aIsLastRow);

    c := FixedColor;
    if c <> clNone then
      aCell.style.SetProperty('background-color', JSColor(c))
    else
      aCell.style.removeProperty('background-color');

    aCell.style.SetProperty('border-color', JSColor(fFixedGridLineColor));

    if (aCol = 0) then
      aCell.style.setProperty('border-left-color', JSColor(fGridLineColor));
    if (aRow = 0) then
      aCell.style.setProperty('border-top-color', JSColor(fGridLineColor));
  end;

var
  row, rowtop, rowleft, rowtopleft: TJSHTMLTableRowElement;
  i, j: LongInt;
  cell: TJSHTMLTableDataCellElement;
  container: TJSHTMLElement;
  usecolumns: Boolean;
  column: TGridColumn;
begin
  inherited Changed;

  container := TJSHTMLElement(HandleElement);

  ApplyScrollStyleToStyle(container.style, ScrollBars);

  if BorderStyle = bsSingle then begin
    container.style.setProperty('border-color', JSColor(BorderColor));
    container.style.setProperty('border-style', 'solid');
    container.style.setProperty('border-width', '1px');
  end;

  if not Assigned(fContentTable) then begin
    fContentTable := TJSHTMLTableElement(document.createElement('table'));
    fContentTable.style.setProperty('position', 'relative');
    fContentTable.style.setProperty('z-index', '1');
    fContentTable.cellSpacing := '0px';

    { always add the content table }
    container.appendChild(fContentTable);
  end;

  if not Assigned(fFixedColsTable) then begin
    fFixedColsTable := TJSHTMLTableElement(document.createElement('table'));
    fFixedColsTable.style.setProperty('position', 'absolute');
    fFixedColsTable.style.setProperty('z-index', '2');
    fFixedColsTable.style.setProperty('left', '0px');
    fFixedColsTable.style.setProperty('top', '0px');
    fFixedColsTable.cellSpacing := '0px';
  end;

  if not Assigned(fFixedRowsTable) then begin
    fFixedRowsTable := TJSHTMLTableElement(document.createElement('table'));
    fFixedRowsTable.style.setProperty('position', 'absolute');
    fFixedRowsTable.style.setProperty('z-index', '3');
    fFixedRowsTable.style.setProperty('left', '0px');
    fFixedRowsTable.style.setProperty('top', '0px');
    fFixedRowsTable.cellSpacing := '0px';
  end;

  if not Assigned(fFixedTopLeftTable) then begin
    fFixedTopLeftTable := TJSHTMLTableElement(document.createElement('table'));
    fFixedTopLeftTable.style.setProperty('position', 'absolute');
    fFixedTopLeftTable.style.setProperty('z-index', '4');
    fFixedTopLeftTable.style.setProperty('left', '0px');
    fFixedTopLeftTable.style.setProperty('top', '0px');
    fFixedTopLeftTable.cellSpacing := '0px';
  end;

  if FixedRows > 0 then
    container.appendChild(fFixedRowsTable)
  else
    container.removeChild(fFixedRowsTable);
  if FixedCols > 0 then
    container.appendChild(fFixedColsTable)
  else
    container.removeChild(fFixedColsTable);
  if (FixedRows > 0) and (FixedCols > 0) then
    container.appendChild(fFixedTopLeftTable)
  else
    container.removeChild(fFixedTopLeftTable);

  AdjustRows(fContentTable, fRows.Count);
  AdjustRows(fFixedColsTable, fRows.Count);
  AdjustRows(fFixedRowsTable, FixedRows);
  AdjustRows(fFixedTopLeftTable, FixedRows);

  usecolumns := columns.Enabled;

  for i := 0 to fContentTable.rows.length - 1 do begin
    row := TJSHTMLTableRowElement(fContentTable.rows[i]);

    AdjustCells(row, fCols.Count);

    if i < FixedRows then begin
      rowtop := TJSHTMLTableRowElement(fFixedRowsTable.rows[i]);
      AdjustCells(rowtop, fCols.Count);
    end else
      rowtop := Nil;

    if FixedCols > 0 then begin
      rowleft := TJSHTMLTableRowElement(fFixedColsTable.rows[i]);
      AdjustCells(rowleft, FixedCols);
    end else
      rowleft := Nil;

    if (i < FixedRows) and (FixedCols > 0) then begin
      rowtopleft := TJSHTMLTableRowElement(fFixedTopLeftTable.rows[i]);
      AdjustCells(rowtopleft, FixedCols);
    end else
      rowtopleft := Nil;

    for j := 0 to row.cells.length - 1 do begin
      cell := TJSHTMLTableDataCellElement(row.cells[j]);

      column := Nil;
      if usecolumns and IsColumnIndexVariable(j) then
        column := ColumnFromGridColumn(j);

      UpdateCell(cell, j, i, column, j = ColCount - 1, i = RowCount - 1);

      if j < FixedCols then begin
        cell := TJSHTMLTableDataCellElement(rowleft.cells[j]);
        UpdateFixedCell(cell, j, i, column, j = FixedCols - 1, (i = FixedRows - 1) and (RowCount = 1))
      end;

      if i < FixedRows then begin
        cell := TJSHTMLTableDataCellElement(rowtop.cells[j]);
        UpdateFixedCell(cell, j, i, column, (j = FixedCols - 1) and (ColCount = 1), i = FixedRows - 1);
      end;

      if (j < FixedCols) and (i < FixedRows) then begin
        cell := TJSHTMLTableDataCellElement(rowtopleft.cells[j]);
        UpdateFixedCell(cell, j, i, column, (j = FixedCols - 1) and (ColCount = 1), (i = FixedRows - 1) and (RowCount = 1));
      end;
    end;
  end;
end;

function TCustomGrid.ColumnFromGridColumn(aColumn: Integer): TGridColumn;
var
  idx: Integer;
begin
  idx := ColumnIndexFromGridColumn(aColumn);
  if idx >= 0 then
    Result := Columns[idx]
  else
    Result := Nil;
end;

function TCustomGrid.ColumnIndexFromGridColumn(aColumn: Integer): Integer;
begin
  if Columns.Enabled and (aColumn >= FirstGridColumn) then
    Result := Columns.RealIndex(aColumn - FirstGridColumn)
  else
    Result := -1;
end;

procedure TCustomGrid.ColumnsChanged(aColumn: TGridColumn);
begin
  if csDestroying in ComponentState then
    Exit;

  if not Assigned(aColumn) then begin
    if FirstGridColumn + Columns.VisibleCount <> ColCount then
      InternalSetColCount(FirstGridColumn + Columns.VisibleCount)
    else
      Changed;
  end else begin
    if Columns.IndexOf(aColumn) >= 0 then
      Changed;
  end;
end;

function TCustomGrid.CreateColumns: TGridColumns;
begin
  Result := TGridColumns.Create(Self, TGridColumn);
end;

function TCustomGrid.CreateHandleElement: TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.createElement('div'));
end;

procedure TCustomGrid.DoScroll;
var
  container: TJSHTMLElement;
begin
  inherited DoScroll;

  container := HandleElement;

  if Assigned(fFixedColsTable) then
    fFixedColsTable.style.setProperty('left', IntToStr(container.scrollLeft) + 'px');

  if Assigned(fFixedColsTable) then
    fFixedRowsTable.style.setProperty('top', IntToStr(container.scrollTop) + 'px');

  if Assigned(fFixedTopLeftTable) then begin
    fFixedTopLeftTable.style.setProperty('top', IntToStr(container.scrollTop) + 'px');
    fFixedTopLeftTable.style.setProperty('left', IntToStr(container.scrollLeft) + 'px');
  end;
end;

function TCustomGrid.FirstGridColumn: Integer;
begin
  Result := FixedCols;
end;

function TCustomGrid.GetDefaultRowHeight: Integer;
begin
  Result := Font.TextHeight('Xy') + 7;
end;

function TCustomGrid.GridColumnFromColumnIndex(aColumnIndex: Integer): Integer;
begin
  Result := aColumnIndex + FirstGridColumn;
  if Result > ColCount - 1 then
    Result := -1;
end;

procedure TCustomGrid.InternalSetColCount(aCount: Integer);
var
  old: Integer;
begin
  old := fCols.Count;

  if old = aCount then
    Exit;

  if aCount < 1 then
    Clear
  else begin
    if EditorMode and (aCount < old) then
      EditorMode := False;

    CheckFixed(aCount, RowCount, FixedCols, FixedRows);
    AdjustGrid(True, old, aCount);
  end;
end;

procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer; aRedraw: Boolean);
begin
  { ToDo }
end;

function TCustomGrid.IsColumnIndexValid(aIndex: Integer): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < ColCount);
end;

function TCustomGrid.IsColumnIndexVariable(aIndex: Integer): Boolean;
begin
  Result := (aIndex >= fFixedCols) and (aIndex < ColCount);
end;

function TCustomGrid.IsRowIndexValid(aIndex: Integer): Boolean;
begin
  Result := (aIndex >= 0) and (aIndex < RowCount);
end;

function TCustomGrid.IsRowIndexVariable(aIndex: Integer): Boolean;
begin
  Result := (aIndex >= fFixedRows) and (aIndex < RowCount);
end;

procedure TCustomGrid.SizeChanged(aOldColCount, aOldRowCount: Integer);
begin
  { empty }
end;

procedure TCustomGrid.TopLeftChanged;
begin
  if Assigned(OnTopLeftChanged) and not (csDesigning in ComponentState) then
    OnTopLeftChanged(Self);
end;

procedure TCustomGrid.UpdateBorderStyle;
var
  bs: TBorderStyle;
begin
  if not Flat and (fGridBorderStyle = bsSingle) then
    bs := bsSingle
  else
    bs := bsNone;

  inherited SetBorderStyle(bs);

  if [csDestroying, csLoading] * ComponentState = [] then begin
    VisualChange;
  end;
end;

procedure TCustomGrid.VisualChange;
begin
  if fUpdateCount <> 0 then
    Exit;

  Invalidate;
end;

constructor TCustomGrid.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTopLeft := Point(1, 1);
  fCols := TIntegerList.Create;
  fRows := TIntegerList.Create;
  fColumns := CreateColumns;

  fDefColWidth := -1;
  fDefRowHeight := -1;

  fScrollBars := ssAutoBoth;

  fGridLineColor := clSilver;
  fGridLineWidth := 1;
  fGridLineStyle := psSolid;
  fFixedColor := clBtnFace;
  fFixedGridLineColor := cl3DDkShadow;
  fBorderColor := cl3DDkShadow;

  fFlat := False;
  fGridBorderStyle := bsSingle;
  UpdateBorderStyle;

  ColCount := 5;
  RowCount := 5;
  FixedRows := 1;
  FixedCols := 1;
end;

destructor TCustomGrid.Destroy;
begin
  fColumns.Free;
  fCols.Free;
  fRows.Free;
  inherited Destroy;
end;

procedure TCustomGrid.Clear;
var
  rowschanged, colschanged: Boolean;
begin
  rowschanged := ClearRows;
  colschanged := ClearCols;
  if not (rowschanged or colschanged) then
    Exit;
  Changed;
end;

function TCustomGrid.ClearCols: Boolean;
begin
  Result := False;
  if fCols.Count = 0 then
    Exit;
  fFixedCols := 0;
  fCols.Clear;
  Result := True;
end;

function TCustomGrid.ClearRows: Boolean;
begin
  Result := False;
  if fRows.Count = 0 then
    Exit;
  fFixedRows := 0;
  fRows.Clear;
  Result := True;
end;

procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer);
begin
  InvalidateCell(aCol, aRow, True);
end;

end.

