unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    ButtonPattern: TButton;
    ButtonTexture1: TButton;
    ButtonTexture2: TButton;
    ButtonObject: TButton;
    ButtonExecute: TButton;
    ButtonSave: TButton;
    ImagePattern: TImage;
    ImageTexture1: TImage;
    ImageTexture2: TImage;
    ImageObject: TImage;
    Image5: TImage;
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

end.

