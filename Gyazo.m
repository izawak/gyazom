BeginPackage["Gyazo`"];

Gyazo::usage = "Gyazo[g_Graphics] or Gyazo[img_Image]";
GyazoInstall::usage = "Install Gyazo.m";

Options[Gyazo] = {};

Begin["`Private`"];

GyazoInstall::good = "Your token is saved to LocalObject[\"Gyazo.AccessToken\"]";
GyazoInstall::bad = "Bad token: ``";

GyazoInstall[]:=Block[{token},
  token = InputString["Input your access token of Gyazo.com"];
  If[StringQ[token],
    token = StringReplace[token, "\""->""];];
  If[StringQ[token] && StringLength[token]==64,
    Put[token, LocalObject["Gyazo.AccessToken"]];
    Message[GyazoInstall::good];
    (* else *),
    Message[GyazoInstall::bad, token];
  ];
];

Gyazo[img_Image, opts:OptionsPattern[]]:= Gyazo[Show[img], opts];

ImageFile[g_Graphics] := Block[{tmpf},
  tmpf = CreateTemporary[];
  Export[tmpf, g, "PNG"];
  tmpf
  ];

AccessToken[]:=Block[{lo,token},
  lo = LocalObject["Gyazo.AccessToken"];
  token = If[FileExistsQ[lo], Get[lo], None];
  If[StringQ[token]&&StringLength[token]==64,
    token,
    None
  ]
];

Gyazo::notoken = "Access token is not found. Executing GyazoInstall[]...";

Gyazo[g_Graphics, opts:OptionsPattern[]] := Block[{imgf, req, token, result},
    token = AccessToken[];
    If[Not@StringQ[token],
      Message[Gyazo::notoken];
      GyazoInstall[];
      token = AccessToken[];
      If[Not@StringQ[token],
        Return[];
      ];
    ];
    imgf = ImageFile[g];
    req = HTTPRequest["https://upload.gyazo.com/api/upload", <|
      "Method" -> "POST",
      "ContentType" -> "multipart/form-data",
      "Body" -> <|
        "access_token" -> token,
        "imagedata" -> <|"MIMEType" -> "image/png", 
          "Content" -> File[imgf]|>
        |>
      |>
     ];

    result = URLExecute[req];
    DeleteFile[imgf];
    result
];

End[];
EndPackage[];
