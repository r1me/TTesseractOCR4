object TesseractOCRImageForm: TTesseractOCRImageForm
  Left = 0
  Top = 0
  Caption = 'TTesseractOCR4'
  ClientHeight = 379
  ClientWidth = 692
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 700
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    692
    379)
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 358
    Width = 692
    Height = 21
    Panels = <
      item
        Style = psOwnerDraw
        Width = 250
      end
      item
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
  end
  object panTop: TPanel
    Left = 0
    Top = 0
    Width = 692
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object labAnalysisMode: TLabel
      Left = 297
      Top = 18
      Width = 30
      Height = 13
      Caption = 'Mode:'
    end
    object btnRecognize: TButton
      Left = 108
      Top = 13
      Width = 92
      Height = 25
      Caption = 'Recognize'
      Enabled = False
      TabOrder = 1
      OnClick = btnRecognizeClick
    end
    object btnCancel: TButton
      Left = 206
      Top = 13
      Width = 77
      Height = 25
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 2
      OnClick = btnCancelClick
    end
    object btnOpenFile: TButton
      Left = 10
      Top = 13
      Width = 92
      Height = 25
      Caption = 'Open image'
      TabOrder = 0
      OnClick = btnOpenFileClick
    end
    object cbPageSegMode: TComboBox
      Left = 333
      Top = 15
      Width = 346
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        'Orientation and script detection only'
        
          'Automatic page segmentation with orientation and script detectio' +
          'n'
        'Automatic page segmentation, but no OSD, or OCR'
        'Fully automatic page segmentation, but no OSD'
        'Assume a single column of text of variable sizes'
        'Assume a single uniform block of vertically aligned text'
        'Assume a single uniform block of text'
        'Treat the image as a single text line'
        'Treat the image as a single word'
        'Treat the image as a single word in a circle'
        'Treat the image as a single character'
        'Find as much text as possible in no particular order'
        'Sparse text with orientation and script det.'
        'Treat the image as a single text line, Tesseract-specific'
        'Number of enum entries')
    end
  end
  object pgTabs: TPageControl
    Left = 0
    Top = 50
    Width = 692
    Height = 308
    ActivePage = tabImage
    Align = alClient
    TabOrder = 2
    object tabImage: TTabSheet
      Caption = 'Image'
      object pbImage: TPaintBox
        Left = 0
        Top = 0
        Width = 684
        Height = 280
        Cursor = crCross
        Align = alClient
        OnMouseDown = pbImageMouseDown
        OnMouseMove = pbImageMouseMove
        OnMouseUp = pbImageMouseUp
        OnPaint = pbImagePaint
        ExplicitLeft = 10
        ExplicitTop = 6
        ExplicitWidth = 469
        ExplicitHeight = 328
      end
    end
    object tabText: TTabSheet
      Caption = 'Text'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memText: TMemo
        Left = 0
        Top = 0
        Width = 684
        Height = 280
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tabHOCR: TTabSheet
      Caption = 'HOCR'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memHOCR: TMemo
        Left = 0
        Top = 0
        Width = 684
        Height = 280
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object tabLayout: TTabSheet
      Caption = 'Layout'
      ImageIndex = 3
      object pbLayout: TPaintBox
        Left = 217
        Top = 0
        Width = 467
        Height = 280
        Cursor = crCross
        Align = alClient
        OnMouseDown = pbLayoutMouseDown
        OnMouseMove = pbLayoutMouseMove
        OnPaint = pbLayoutPaint
        ExplicitLeft = 10
        ExplicitTop = 6
        ExplicitWidth = 469
        ExplicitHeight = 328
      end
      object panLayoutLeft: TPanel
        Left = 0
        Top = 0
        Width = 217
        Height = 280
        Align = alLeft
        BevelKind = bkTile
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          213
          276)
        object tvLayoutItems: TTreeView
          Left = 8
          Top = 96
          Width = 195
          Height = 171
          Anchors = [akLeft, akTop, akRight, akBottom]
          HideSelection = False
          Indent = 19
          RowSelect = True
          TabOrder = 0
          OnChange = tvLayoutItemsChange
        end
        object gbPage: TGroupBox
          Left = 8
          Top = 8
          Width = 195
          Height = 78
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Page'
          TabOrder = 1
          object labMeanWordConf: TLabel
            Left = 10
            Top = 52
            Width = 112
            Height = 13
            Caption = 'Mean word confidence:'
          end
          object labOrientation: TLabel
            Left = 10
            Top = 16
            Width = 58
            Height = 13
            Caption = 'Orientation:'
          end
          object labWritingDirect: TLabel
            Left = 10
            Top = 34
            Width = 82
            Height = 13
            Caption = 'Writing direction:'
          end
        end
      end
    end
  end
  object pbRecognizeProgress: TProgressBar
    Left = 0
    Top = 361
    Width = 249
    Height = 16
    Anchors = [akLeft, akBottom]
    Smooth = True
    TabOrder = 3
  end
  object OpenDialogImage: TOpenDialog
    Filter = 
      'Image files (jpg, bmp, png, gif, tiff, emf, wmf, webp)|*.jpg;*jp' +
      'eg;*.bmp;*.png;*.gif;*.tif;*.tiff;*.emf;*.wmf;*.webp'
    Left = 50
    Top = 96
  end
end
