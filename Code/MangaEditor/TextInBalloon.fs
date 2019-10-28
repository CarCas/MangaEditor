module TextInBalloon

open System.Drawing
open System.Drawing.Drawing2D
open CommonTypes

type TextInBalloon() =
  
  let mutable text : string = ""
  let mutable gpText = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable regText : Region = null
  let mutable fontFamily = new MatchTypez()
  let mutable toUp = false
  let mutable rct_inscript = RectangleF()

  let updText() =
    if text <> null then
      if gpText <> null then gpText.Dispose()
      if regText <> null then regText.Dispose()
      gpText <- new GraphicsPath(FillMode=FillMode.Winding)
      if rct_inscript.Location.X <= rct_inscript.Size.Width + rct_inscript.Location.X &&
         rct_inscript.Location.Y <= rct_inscript.Size.Height + rct_inscript.Location.Y &&
         fontFamily.FontSize > 0.f then
        if toUp then text <- text.ToUpper()
        gpText.AddString(
          text,
          new FontFamily(fontFamily.GetFontFamily()),
          int (FontStyle.Regular),
          fontFamily.FontSize,
          RectangleF(rct_inscript.Location, rct_inscript.Size),
          StringFormat.GenericTypographic)
        regText <- new Region(gpText)

  member this.Txt with get() = text and set(v) = text <- v
  member this.GPText with get() = gpText
  member this.RegText with get() = regText
  member this.FontFamily with get() = fontFamily and set(v) = fontFamily <- v
  member this.ToUpper with get() = toUp and set(v) = toUp <- v
  member this.RctInscript with get() = rct_inscript and set(v) = rct_inscript <- v
  member this.UpdText() = updText()