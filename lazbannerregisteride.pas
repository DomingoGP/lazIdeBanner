{
  Author: Domingo Galmés
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Register the package in the lazarus IDE
}
unit lazbannerregisteride;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType,
  // IdeIntf
  LazIDEIntf, MenuIntf, IdeCommands;

procedure Register;

implementation

uses
  frmBanner, lazbannerUIConsts,IDEWindowIntf,controls;

procedure DoCompareFilesMenu(Sender: TObject);
begin
  ShowBannerDialog;
end;

procedure Register;
var
  Key: TIDEShortCut;
  Cat: TIDECommandCategory;
  CmdMyTool: TIDECommand;
begin
  // register IDE shortcut and menu item
  Key := IDEShortCut(VK_UNKNOWN, [], VK_UNKNOWN, []);
  Cat := IDECommandList.FindCategoryByName(CommandCategoryToolMenuName);
  CmdMyTool := RegisterIDECommand(Cat, FORMAT_CURRENT_MENU2, MENU_CMD_DESC2,
    Key, nil, @DoCompareFilesMenu);
  //  RegisterIDEMenuCommand(itmSecondaryTools, 'LazBanner', FORMAT_CURRENT_MENU2, nil, nil, CmdMyTool);
  RegisterIDEMenuCommand(itmSourceTools, 'LazBanner', FORMAT_CURRENT_MENU2, nil, nil, CmdMyTool);
  // register window creator
  IDEWindowCreators.Add(BannerFormWindowName,@CreateIDEBannerForm,nil,'1050','510','','','',alNone,true);
end;

end.
