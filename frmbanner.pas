{ Freepascal implementation of rendering Figlet fonts  www.figlet.org.

  Copyright (C) 2021 Domingo Galmés  dgalmesp@gmail.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
{
//*                    ____                                                  *//
//*                   |  _ \                                                 *//
//*                   | |_) | __ _ _ __  _ __   ___ _ __                     *//
//*                   |  _ < / _` | '_ \| '_ \ / _ \ '__|                    *//
//*                   | |_) | (_| | | | | | | |  __/ |                       *//
//*                   |____/ \__,_|_| |_|_| |_|\___|_|                       *//
//*                                                                          *//
//*                                                                          *//
}

unit frmbanner;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, EditBtn;

{$IFDEF IDE_PLUGIN}
const
  BannerFormWindowName = 'IDEBannerWindow';
{$ENDIF}


type

  { TfrmBanner }

  TfrmBanner = class(TForm)
    btnInsert: TButton;
    btnCopy: TButton;
    btnPrint: TButton;
    btnUnzipFont: TButton;
    btnSaveDefaults: TButton;
    btnScan: TButton;
    cbCloseAfter: TCheckBox;
    cbInsertAfterPrint: TCheckBox;
    edFontDir: TDirectoryEdit;
    cbFonts: TComboBox;
    edLineAfter: TEdit;
    edLineStart: TEdit;
    edLineBefore: TEdit;
    edTextAfter: TEdit;
    edTexto: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo1: TMemo;
    rgAlign: TRadioGroup;
    rgDrawMode: TRadioGroup;
    seSpaces: TSpinEdit;
    seLineLength: TSpinEdit;
    procedure btnCopyClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnSaveDefaultsClick(Sender: TObject);
    procedure btnScanClick(Sender: TObject);
    procedure btnUnzipFontClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure cbFontsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadSettings;
    procedure ReScanFonts(const aFontName: string);
    procedure SaveSettings;

  public

  end;

{$IFDEF IDE_PLUGIN}
procedure ShowBannerDialog;
procedure CreateIDEBannerForm(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
{$ENDIF}

var
  frmBanner1: TfrmBanner;

implementation

uses
  Math, figletfont,
  {$IFDEF IDE_PLUGIN}
  LazConfigStorage,lazbanneruiconsts, BaseIDEIntf, IDEMsgIntf, IDEExternToolIntf, lazideintf,
    // IdeIntf
  IDEWindowIntf, IDEHelpIntf, IDEImagesIntf,
  SrcEditorIntf,
  {$ENDIF}
  laz_xmlcfg;

{$R *.lfm}


{ TfrmBanner }

procedure TfrmBanner.btnPrintClick(Sender: TObject);
var
  wF: TFigletFont;
  wI: integer;
begin
  Memo1.Lines.Clear;
  wF := TFigletFont.Create(IncludeTrailingPathDelimiter(edFontDir.Text) + cbFonts.Text);
  try
    wF.LineStartText := edLineStart.Text;
    wF.LineEndText := edTextAfter.Text;
    wF.LineWidth := seLineLength.Value;
    wF.Align := TFigletAlign(rgAlign.ItemIndex);
    wF.DrawMode := TFigletDrawMode(rgDrawMode.ItemIndex);
    wF.AdditionalSpaces := seSpaces.Value;
    wF.DrawText(edTexto.Text);
    if edLineBefore.Text <> '' then
      Memo1.Lines.Add(edLineBefore.Text);
    for wI := 0 to Pred(wF.Output.Count) do
      Memo1.Lines.Add(wF.Output[wI]);
    if edLineAfter.Text <> '' then
      Memo1.Lines.Add(edLineAfter.Text);
    {$IFDEF IDE_PLUGIN}
    if cbInsertAfterPrint.Checked then
      btnInsertClick(Self);
    {$ENDIF}
  finally
    wF.Free;
  end;
end;

procedure TfrmBanner.cbFontsChange(Sender: TObject);
begin
  btnPrintClick(Self);
end;

procedure TfrmBanner.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  {$IFDEF IDE_PLUGIN}
  CloseAction := caFree;
  {$ENDIF}
end;

procedure TfrmBanner.btnUnzipFontClick(Sender: TObject);
var
  wF: TFigletFont;
  wS: string;
begin
  wF := TFigletFont.Create(IncludeTrailingPathDelimiter(edFontDir.Text) + cbFonts.Text);
  try
    wS := wF.GetFontSource;
    Memo1.Lines.Text := wS;
  finally
    wF.Free;
  end;
end;

procedure TfrmBanner.btnCopyClick(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TfrmBanner.btnInsertClick(Sender: TObject);
{$IFDEF IDE_PLUGIN}
var
  srcEditor: TSourceEditorInterface;
  wCursor: TPoint;
  wI: integer;
begin
  if (SourceEditorManagerIntf = nil) or (SourceEditorManagerIntf.ActiveEditor = nil) then
  begin
    //LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;
  srcEditor := SourceEditorManagerIntf.ActiveEditor;
  if srcEditor.ReadOnly then
    Exit;
  wCursor := srcEditor.CursorTextXY;
  try
    srcEditor.BeginUpdate;
    srcEditor.BeginUndoBlock;
    for wI := 0 to Pred(Memo1.Lines.Count) do
    begin
      srcEditor.InsertLine(wCursor.Y + wI, Memo1.Lines[wI], True);
    end;
  finally
    srcEditor.EndUndoBlock;
    srcEditor.EndUpdate;
  end;
  if cbCloseAfter.Checked then
    Close;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure TfrmBanner.btnSaveDefaultsClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfrmBanner.btnScanClick(Sender: TObject);
begin
  ReScanFonts(cbFonts.Text);
end;

procedure TfrmBanner.FormCreate(Sender: TObject);
begin
  LoadSettings;
  rgAlign.OnClick := @cbFontsChange;
  rgDrawMode.OnClick := @cbFontsChange;
  seSpaces.OnChange := @cbFontsChange;
  seLineLength.OnChange := @cbFontsChange;
  {$IFNDEF IDE_PLUGIN}
  btnInsert.Visible := False;
  cbInsertAfterPrint.Visible := False;
  cbCloseAfter.Visible := False;
  {$ELSE}
  edTexto.Text:='';
  {$ENDIF}
end;

procedure TfrmBanner.ReScanFonts(const aFontName: string);
var
  srSearch: TSearchRec;
  wIndex: integer;
begin
  wIndex := 0;
  cbFonts.OnChange := nil;
  cbFonts.Items.BeginUpdate;
  cbFonts.Items.Clear;
  try
    if FindFirst(IncludeTrailingPathDelimiter(edFontDir.Text) + '*.flf', faAnyFile, srSearch) = 0 then
      repeat
        if ((srSearch.Attr and faDirectory) = 0) and (srSearch.Name <> '.') and (srSearch.Name <> '..') then
        begin
          if srSearch.Name = aFontName then
            wIndex := cbFonts.Items.Count;
          cbFonts.Items.Add(srSearch.Name);
        end;
      until (FindNext(srSearch) <> 0);
    FindClose(srSearch);
  finally
    cbFonts.Items.EndUpdate;
  end;
  cbFonts.OnChange := @cbFontsChange;
  cbFonts.ItemIndex := wIndex;
end;

const
  CONFIGURATION_FILE = 'banner.xml';

procedure TfrmBanner.SaveSettings;
var
  {$IFDEF IDE_PLUGIN}
  Config: TConfigStorage;
  {$ELSE}
  Config: TXMLConfig;
  {$ENDIF}
const
  Version = 1;
begin
  try
    {$IFDEF IDE_PLUGIN}
    Config := GetIDEConfigStorage(CONFIGURATION_FILE, False);
    {$ELSE}
    //    Config := TConfigStorage.Create(CONFIGURATION_FILE, False);
    Config := TXMLConfig.Create(CONFIGURATION_FILE);
    {$ENDIF}
    try
      // store the version number so future extensions can handle old config files
      Config.SetDeleteValue('Version', Version, 0);
      // store string variable "SomeValue"
      // if SomeValue has the default value the entry is not stored,
      // so only the differences to the default are stored.
      // This way the xml is kept short and defaults may change in future.
      Config.SetDeleteValue('FontDir', edFontDir.Text, 'fonts');
      Config.SetDeleteValue('FontName', cbFonts.Text, 'standard.tlf');
      Config.SetDeleteValue('HorizontalAlign', rgAlign.ItemIndex, 0);
      Config.SetDeleteValue('DrawMode', rgDrawMode.ItemIndex, 0);
      Config.SetDeleteValue('AdditionalSpaces', seSpaces.Value, 0);
      Config.SetDeleteValue('LineLength', seLineLength.Value, 80);
      Config.SetDeleteValue('InsertAfterPrint', cbInsertAfterPrint.Checked, False);
      Config.SetDeleteValue('CloseAfterInsert', cbCloseAfter.Checked, False);
      Config.SetDeleteValue('LineBefore', edLineBefore.Text, '');
      Config.SetDeleteValue('LineAfter', edLineAfter.Text, '');
      Config.SetDeleteValue('StartOfLine', edLineStart.Text, '');
      Config.SetDeleteValue('EndOfLine', edTextAfter.Text, '');
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF IDE_PLUGIN}
      AddIDEMessage(mluWarning, 'Saving baner.xml failed: ' + E.Message);
      {$ENDIF}
    end;
  end;
  {$IFDEF IDE_PLUGIN}
  IDEDialogLayoutList.SaveLayout(Self);
  {$ENDIF}
end;

procedure TfrmBanner.LoadSettings;
var
  {$IFDEF IDE_PLUGIN}
  Config: TConfigStorage;
  {$ELSE}
  Config: TXMLConfig;
  {$ENDIF}
  Version: integer;
  wFontName: string;
begin
  try
    {$IFDEF IDE_PLUGIN}
    Config := GetIDEConfigStorage(CONFIGURATION_FILE, True);
    {$ELSE}
    //Config := TConfigStorage.Create(CONFIGURATION_FILE,True);
    Config := TXMLConfig.Create(CONFIGURATION_FILE);
    {$ENDIF}
    try
      Version := Config.GetValue('Version', 1);
      wFontName := Config.GetValue('FontName', 'standard.tlf');
      edFontDir.Text := Config.GetValue('FontDir', 'fonts');
      ReScanFonts(wFontName);
      rgAlign.ItemIndex := Config.GetValue('HorizontalAlign', 0);
      rgDrawMode.ItemIndex := Config.GetValue('DrawMode', 0);
      seSpaces.Value := Config.GetValue('AdditionalSpaces', 0);
      seLineLength.Value := Config.GetValue('LineLength', 80);
      cbInsertAfterPrint.Checked := Config.GetValue('InsertAfterPrint', False);
      cbCloseAfter.Checked := Config.GetValue('CloseAfterInsert', False);
      edLineBefore.Text := Config.GetValue('LineBefore', '');
      edLineAfter.Text := Config.GetValue('LineAfter', '');
      edLineStart.Text := Config.GetValue('StartOfLine', '');
      edTextAfter.Text := Config.GetValue('EndOfLine', '');
    finally
      Config.Free;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF IDE_PLUGIN}
      AddIDEMessage(mluWarning, 'Loading banner.xml failed: ' + E.Message);
      {$ENDIF}
    end;
  end;
end;

{$IFDEF IDE_PLUGIN}
procedure CreateIDEBannerForm(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
var
  lForm: TfrmBanner;
begin
  if CompareText(aFormName, BannerFormWindowName) <> 0 then exit;
  lForm := nil;
  IDEWindowCreators.CreateForm(lForm, TfrmBanner, DoDisableAutoSizing, LazarusIDE.OwningComponent);
  //Name required for docking
  lForm.Name := BannerFormWindowName;
  AForm := lForm;
end;

procedure ShowBannerDialog;
begin
  IDEWindowCreators.ShowForm(BannerFormWindowName, True);
end;
{$ENDIF}


end.
