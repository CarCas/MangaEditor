module CommonTypes

open Microsoft.FSharp.Reflection
open System.Windows.Forms
open System.Drawing
open LWC

type Shp = PolyIn | PolyOut | EllIn | EllOut | NoSh

type KndBalloon = Speech | Whisper | Thought | ThoughtNarutoStyle | Scream | NoBalloon

type FontTypez =
| Arial
| Calibri
| Cambria
| ComicSansMS
| Ebrima
| Gadugi
| Georgia
| SimSun
| Verdana
| Webdings
| Wingdings

type MatchTypez() =
  let mutable fontFamily : FontTypez = FontTypez.ComicSansMS
  let mutable fontStyle : System.Drawing.FontStyle = System.Drawing.FontStyle.Regular
  let mutable fontSize = 20.f

  let getType() =
    match fontFamily with
    | FontTypez.Arial -> "Arial" 
    | FontTypez.Calibri -> "Calibri"
    | FontTypez.Cambria -> "Cambria"
    | FontTypez.ComicSansMS -> "Comic Sans MS"
    | FontTypez.Ebrima -> "Ebrima"
    | FontTypez.Gadugi -> "Gadugi"
    | FontTypez.Georgia -> "Georgia"
    | FontTypez.SimSun -> "SimSun"
    | FontTypez.Verdana -> "Verdana"
    | FontTypez.Webdings -> "Webdings"
    | FontTypez.Wingdings -> "Wingdings"

  let setType(v : string) =
    match v with
    | "Arial" -> FontTypez.Arial
    | "Calibri" -> FontTypez.Calibri
    | "Cambria" -> FontTypez.Cambria
    | "Comic Sans MS" -> FontTypez.ComicSansMS
    | "Ebrima" -> FontTypez.Ebrima
    | "Gadugi" -> FontTypez.Gadugi
    | "Georgia" -> FontTypez.Georgia
    | "SimSun" -> FontTypez.SimSun
    | "Verdana" -> FontTypez.Verdana
    | "Webdings" -> FontTypez.Webdings
    | "Wingdings" -> FontTypez.Wingdings
    | _ -> FontTypez.ComicSansMS

  member this.GetFontFamily() = getType()
  member this.SetFontFamily(v:FontTypez) = fontFamily <- v
  member this.SetFontFamily(v:string) = fontFamily <- setType(v)

  member this.FontSize with get() = fontSize and set(v:single) = fontSize <- v
  member this.GetTypes() =
    let fonts = FSharpType.GetUnionCases typeof<FontTypez>
    let mutable typez = new ResizeArray<obj>()
    for f in fonts do typez.Add(f.Name)
    typez.ToArray()