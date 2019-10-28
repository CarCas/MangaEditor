module Balloons

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open LWC
open CommonTypes
open Shapes
open TextInBalloon

type TalkingBalloon() as this =
  inherit LWC()

  let h = 7.5f
  let mutable gp = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable reg : Region = null
  let mutable e = new Ellipse(Incl=true)
  let mutable talkVtx = [|
    PointF(e.Size.Width / 4.f + e.Location.X, e.Size.Height / 2.f + e.Location.Y)
    PointF(3.f * e.Size.Width / 4.f + e.Location.X, e.Size.Height / 2.f + e.Location.Y)
    PointF(e.Size.Width + e.Location.X, e.Size.Height + e.Location.Y)
  |]

  let mutable endDraw = false
  let mutable mvi, mvf, mvt = false, false, false
  let mutable offsetDrag = PointF()
  let mutable whisper = false
  let mutable tib = new TextInBalloon()
  let mutable mtx = new Matrix()

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let updInsRect() =
    tib.RctInscript <-
      RectangleF(
        e.Location.X + (e.Size.Width / 2.f) * (1.f - 1.f / sqrt 2.f),
        e.Location.Y + (e.Size.Height / 2.f) * (1.f - 1.f / sqrt 2.f),
        (e.Size.Width / sqrt 2.f),
        (e.Size.Height / sqrt 2.f)
      )
    talkVtx.[0] <- PointF(e.Size.Width / 4.f + e.Location.X, e.Size.Height / 2.f + e.Location.Y)
    talkVtx.[1] <- PointF(3.f * e.Size.Width / 4.f + e.Location.X, e.Size.Height / 2.f + e.Location.Y)

  let update () =
    if gp <> null then gp.Dispose()
    if reg <> null then reg.Dispose()
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    updInsRect()
    gp.AddEllipse(RectangleF(e.Location, e.Size))
    reg <- new Region(gp)
    use gpt = new GraphicsPath(FillMode=FillMode.Winding)
    gpt.AddLines(talkVtx)
    gpt.CloseFigure()
    gp.AddPath(gpt, false)
    reg.Union(gpt)
    tib.UpdText()
    this.Invalidate()

  let initTalk (pf:PointF) (s:SizeF) =
    e.AddEll(pf, s)
    talkVtx.[0] <- PointF((e.FinPt.X - e.Location.X) / 4.f + e.Location.X, (e.FinPt.Y - e.Location.Y) / 2.f + e.Location.Y)
    talkVtx.[1] <- PointF((3.f * (e.FinPt.X - e.Location.X) / 4.f) + e.Location.X, (e.FinPt.Y - e.Location.Y) / 2.f + e.Location.Y)
    talkVtx.[2] <- PointF(e.Location.X, (e.FinPt.Y - e.Location.Y) + e.Location.Y)
    update()
    
  member this.AddTalk ((l:PointF), (s:SizeF), (b:bool)) =
    e.Location <- l
    e.FinPt <- PointF(l.X + s.Width, l.Y + s.Height)
    whisper <- b
    initTalk l s

  member this.UpdInsRect() = updInsRect()
  member this.Update() = update()
  member this.UpdTalk (st:string) = tib.Txt <- st; update()
  member this.Upd() = tib.UpdText()
  member this.HandleHitTest (p : PointF) (k:PointF) =
    let q x = x * x
    q (p.X - k.X) + q (p.Y - k.Y) < q h

  member this.RctInside with get() = tib.RctInscript
  member this.Size      with get() = e.Size
  member this.RegText   with get() = tib.RegText
  member this.GPText    with get() = tib.GPText

  member this.GraphicsTransform with get() = mtx          and set(v) = mtx <- v
  member this.Reg               with get() = reg          and set(v) = reg <- v
  member this.GP                with get() = gp           and set(v) = gp <- v
  member this.TlkPt             with get() = talkVtx.[2]  and set(v) = talkVtx.[2] <- v
  member this.EndDraw           with get() = endDraw      and set(v) = endDraw <- v
  member this.MVi               with get() = mvi          and set(v) = mvi <- v
  member this.MVf               with get() = mvf          and set(v) = mvf <- v
  member this.MVt               with get() = mvt          and set(v) = mvt <- v
  member this.Whisper           with get() = whisper      and set(v) = whisper <- v

  member this.Location with get() = e.Location and set(v) = e.Location <- v; updInsRect()
  member this.FinPt    with get() = e.FinPt    and set(v) = e.FinPt <- v; updInsRect()
  
  member this.GetStyleFont with get() = tib.FontFamily.GetFontFamily
  member this.SetStyleFont with set(v : string) = tib.FontFamily.SetFontFamily(v); this.Invalidate()
  member this.FontSize with get() = tib.FontFamily.FontSize and set(v : single) = tib.FontFamily.FontSize <- v; this.Invalidate()
  member this.Txt      with get() = tib.Txt                 and set(v) = tib.Txt <- v; this.Invalidate()
  member this.ToUpper  with get() = tib.ToUpper             and set(v) = tib.ToUpper <- v; tib.UpdText()

  override this.OnPaint evt =
    base.OnPaint evt
    let g = evt.Graphics
    use p = new Pen(Brushes.Black, 4.5f)
    
    g.SmoothingMode <- SmoothingMode.HighQuality
    
    if whisper then p.DashStyle <- DashStyle.Dash; p.DashCap <- DashCap.Round
    else p.DashStyle <- DashStyle.Solid

    g.Transform <- mtx
    g.DrawPath(p, gp)
    p.DashStyle <- DashStyle.Solid
    g.FillRegion(Brushes.White, reg)
    if tib.RegText <> null &&
      tib.RctInscript.Location.X <= tib.RctInscript.Location.X + tib.RctInscript.Size.Width &&
      tib.RctInscript.Location.Y <= tib.RctInscript.Location.Y + tib.RctInscript.Size.Height then
        p.Brush <- Brushes.White
        g.FillRegion(p.Brush, tib.RegText)
    if tib.RctInscript.Location.X <= tib.RctInscript.Size.Width + tib.RctInscript.Location.X &&
       tib.RctInscript.Location.Y <= tib.RctInscript.Size.Height + tib.RctInscript.Location.Y &&
       tib.FontFamily.FontSize > 0.f then
      p.Brush <- Brushes.White
      g.FillRegion(p.Brush, reg)
      p.Brush <- Brushes.Black
      p.Width <- 1.f
      g.DrawPath(p, tib.GPText)
      p.Brush <- Brushes.White
      g.FillPath(p.Brush, tib.GPText)
    if not(endDraw) then
      let r (x:PointF, h:single) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
      p.Width <- 2.f
      p.Brush <- Brushes.Green
      g.DrawEllipse(p, r (e.Location, h))
      g.DrawEllipse(p, r (e.FinPt, h))
      p.Brush <- Brushes.Gold
      g.FillRectangle(p.Brush, r (talkVtx.[0], h/4.f))
      g.FillRectangle(p.Brush, r (talkVtx.[1], h/4.f))
      p.Width <- 1.5f
      p.Brush <- Brushes.LawnGreen
      g.DrawEllipse(p, r (talkVtx.[2], h * 1.25f))
      p.Brush <- Brushes.DarkGreen

      if mvi then g.FillEllipse(p.Brush, r (e.Location, h))
      if mvf then g.FillEllipse(p.Brush, r (e.FinPt, h))
      p.Brush <- Brushes.Green
      if mvt then g.FillEllipse(p.Brush, r (talkVtx.[2], h * 1.25f))

type ThoughtBalloon() =
  inherit LWC()

  let h = 7.5f

  let mutable gpLarge = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable gpMiddle = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable gpSmall = new GraphicsPath(FillMode=FillMode.Winding)

  let mutable regLarge : Region = null
  let mutable regMiddle : Region = null
  let mutable regSmall : Region = null

  let mutable gp = new GraphicsPath(FillMode=FillMode.Winding)

  let mutable e = new Ellipse(Incl=true)
  let mutable thinkVtx = [|
    PointF(e.Size.Width + e.Location.X, e.Size.Height + e.Location.Y)
    PointF(e.Location.X, e.Location.Y + e.Size.Height)
  |]
  let mutable endDraw = false
  let mutable mvi, mvf, mvt = false, false, false
  let mutable offsetDrag = PointF()
  let mutable tib = new TextInBalloon()
  let mutable mtx = new Matrix()

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let updInsRect() =
    tib.RctInscript <-
      RectangleF(
        e.Location.X + (e.Size.Width / 2.f) * (1.f - 1.f / sqrt 2.f),
        e.Location.Y + (e.Size.Height / 2.f) * (1.f - 1.f / sqrt 2.f),
        (e.Size.Width / sqrt 2.f),
        (e.Size.Height / sqrt 2.f))

  let updLocAndSize() =
    let tmp = e.FinPt
    if e.Location.X > e.FinPt.X && e.Location.Y <= e.FinPt.Y then
      e.FinPt <- PointF(e.Location.X, e.FinPt.Y)
      e.Location <- PointF(tmp.X, e.Location.Y)
    elif e.Location.X > e.FinPt.X && e.Location.Y > e.FinPt.Y then
      e.FinPt <- e.Location
      e.Location <- tmp
    elif e.Location.X <= e.FinPt.X && e.Location.Y > e.FinPt.Y then
      e.FinPt <- PointF(e.FinPt.X, e.Location.Y)
      e.Location <- PointF(e.Location.X, tmp.Y)
    else
      e.FinPt <- PointF(e.FinPt.X, e.FinPt.Y)
      e.Location <- PointF(e.Location.X, e.Location.Y)
    updInsRect()

  let disposeRes() =
    if gpLarge  <> null then gpLarge.Dispose()
    if gpMiddle <> null then gpMiddle.Dispose()
    if gpSmall  <> null then gpSmall.Dispose()
    if gp       <> null then gp.Dispose()

    if regLarge  <> null then regLarge.Dispose()
    if regMiddle <> null then regMiddle.Dispose()
    if regSmall  <> null then regSmall.Dispose()

  let updEll() =
    disposeRes()
    gpLarge <- new GraphicsPath(FillMode=FillMode.Winding)
    gpLarge.AddEllipse(RectangleF(e.Location, SizeF(e.FinPt.X - e.Location.X, e.FinPt.Y - e.Location.Y)))
    regLarge <- new Region(gpLarge)
    let mutable smallEll = new RectangleF(Size=SizeF(e.Size.Width / 6.f, e.Size.Height / 6.f))
    smallEll.Location <- PointF(thinkVtx.[1].X - smallEll.Size.Width / 2.f, thinkVtx.[1].Y - smallEll.Size.Height / 2.f)
    let mutable middleEll = new RectangleF(Size=SizeF(e.Size.Width / 2.f, e.Size.Height / 2.f))
    thinkVtx.[0] <-
      PointF((smallEll.Location.X + e.Location.X + smallEll.Size.Width / 2.f + e.Size.Width / 2.f) / 2.f,
             (smallEll.Location.Y + e.Location.Y + smallEll.Size.Height / 2.f + e.Size.Height / 2.f) / 2.f)
    middleEll.Location <- PointF(thinkVtx.[0].X - middleEll.Size.Width / 2.f, thinkVtx.[0].Y - middleEll.Size.Height / 2.f)
    gpMiddle <- new GraphicsPath(FillMode=FillMode.Winding)
    gpMiddle.AddEllipse(middleEll)
    regMiddle <- new Region(gpMiddle)
    gpSmall <- new GraphicsPath(FillMode=FillMode.Winding)
    gpSmall.AddEllipse(smallEll)
    regSmall <- new Region(gpSmall)
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    gp.AddPath(gpSmall, false)
    gp.AddPath(gpMiddle, false)
    gp.AddPath(gpLarge, false)

  let update() = 
    disposeRes()
    gpLarge <- new GraphicsPath(FillMode=FillMode.Winding)
    gpLarge.AddEllipse(RectangleF(e.Location, e.Size))
    regLarge <- new Region(gpLarge)
    let mutable smallEll = new RectangleF(Size=SizeF(e.Size.Width / 6.f, e.Size.Height / 6.f))
    smallEll.Location <- PointF(thinkVtx.[1].X - smallEll.Size.Width / 2.f, thinkVtx.[1].Y - smallEll.Size.Height / 2.f)
    let mutable middleEll = new RectangleF(Size=SizeF(e.Size.Width / 2.f, e.Size.Height / 2.f))
    thinkVtx.[0] <-
      PointF((smallEll.Location.X + e.Location.X + smallEll.Size.Width / 2.f + e.Size.Width / 2.f) / 2.f,
             (smallEll.Location.Y + e.Location.Y + smallEll.Size.Height / 2.f + e.Size.Height / 2.f) / 2.f)
    middleEll.Location <- PointF(thinkVtx.[0].X - middleEll.Size.Width / 2.f, thinkVtx.[0].Y - middleEll.Size.Height / 2.f)
    gpMiddle <- new GraphicsPath(FillMode=FillMode.Winding)
    gpMiddle.AddEllipse(middleEll)
    regMiddle <- new Region(gpMiddle)
    gpSmall <- new GraphicsPath(FillMode=FillMode.Winding)
    gpSmall.AddEllipse(smallEll)
    regSmall <- new Region(gpSmall)
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    gp.AddPath(gpSmall, false)
    gp.AddPath(gpMiddle, false)
    gp.AddPath(gpLarge, false)
    
    updInsRect()
    tib.UpdText()

  member this.AddThink ((l:PointF), (s:SizeF)) =
    e.Location <- l
    e.FinPt <- PointF(l.X + s.Width, l.Y + s.Height)
    disposeRes()
    e.AddEll(l, s)
    thinkVtx.[0] <- PointF(e.Size.Width + e.Location.X, e.Size.Height + e.Location.Y)
    thinkVtx.[1] <- PointF(e.Location.X, e.Location.Y + e.Size.Height)
    updInsRect()
    updEll()

  member this.UpdThink (st:string) = tib.Txt <- st; update()
  member this.HandleHitTest (p : PointF) (k:PointF) = (p.X - k.X) * (p.X - k.X) + (p.Y - k.Y) * (p.Y - k.Y) < h * h
  member this.Update() = update()
  member this.Upd() = tib.UpdText()

  member this.RctInside with get() = tib.RctInscript
  member this.RegLarge  with get() = regLarge
  member this.RegMiddle with get() = regMiddle
  member this.RegSmall  with get() = regSmall
  member this.RegText   with get() = tib.RegText
  member this.GPText    with get() = tib.GPText
  member this.Size      with get() = e.Size

  member this.GPLarge     with get() = gpLarge      and set(v) = gpLarge <- v
  member this.GPMiddle    with get() = gpMiddle     and set(v) = gpMiddle <- v
  member this.GPSmall     with get() = gpSmall      and set(v) = gpSmall <- v
  member this.GP          with get() = gp           and set(v) = gp <- v
  member this.Location    with get() = e.Location   and set(v) = e.Location <- v
  member this.FinPt       with get() = e.FinPt      and set(v) = e.FinPt <- v
  member this.Think       with get() = thinkVtx.[1] and set(v) = thinkVtx.[1] <- v
  member this.EndDraw     with get() = endDraw      and set(v) = endDraw <- v
  member this.MVLocation  with get() = mvi          and set(v) = mvi <- v
  member this.MVFinPt     with get() = mvf          and set(v) = mvf <- v
  member this.MVThink     with get() = mvt          and set(v) = mvt <- v

  member this.GraphicsTransform with get() = mtx and set(v) = mtx <- v

  member this.GetStyleFont with get() = tib.FontFamily.GetFontFamily
  member this.SetStyleFont with set(v : string) = tib.FontFamily.SetFontFamily(v); this.Invalidate()
  member this.FontSize with get() = tib.FontFamily.FontSize and set(v : single) = tib.FontFamily.FontSize <- v; this.Invalidate()
  member this.Txt with get() = tib.Txt and set(v) = tib.Txt <- v; this.Invalidate()
  member this.ToUpper with get() = tib.ToUpper and set(v) = tib.ToUpper <- v; tib.UpdText(); this.Invalidate()

  override this.OnPaint evt =
    base.OnPaint evt
    let g = evt.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    g.Transform <- mtx
    use p = new Pen(Brushes.Black, 4.5f)

    let drawAndFill(gp : GraphicsPath, r : Region) =
      g.DrawPath(p, gp)
      p.Brush <- Brushes.White
      g.FillRegion(p.Brush, r)
      p.Brush <- Brushes.Black

    drawAndFill(gp, regSmall)
    drawAndFill(gpSmall, regMiddle)
    drawAndFill(gpMiddle, regLarge)

    g.DrawPath(p, gpLarge)

    if tib.RctInscript.Location.X <= tib.RctInscript.Size.Width + tib.RctInscript.Location.X &&
       tib.RctInscript.Location.Y <= tib.RctInscript.Size.Height + tib.RctInscript.Location.X &&
       tib.FontFamily.FontSize > 0.f then
      p.Width <- 1.f
      p.Brush <- Brushes.Black
      g.DrawPath(p, tib.GPText)
      p.Brush <- Brushes.White
      g.FillPath(p.Brush, tib.GPText)

    if not(endDraw) then
      let r (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
      p.Width <- 2.f
      p.Brush <- Brushes.Green
      g.DrawEllipse(p, r e.Location)
      g.DrawEllipse(p, r e.FinPt)
      p.Brush <- Brushes.Gold
      g.DrawEllipse(p, r thinkVtx.[1])
      p.Brush <- Brushes.DarkGreen

      if mvi then g.FillEllipse(p.Brush, r e.Location)
      if mvf then g.FillEllipse(p.Brush, r e.FinPt)
      if mvt then p.Brush <- Brushes.Goldenrod; g.FillEllipse(p.Brush, r thinkVtx.[1])


type ThoughtNarutoBalloon() as this =
  inherit LWC()

  let h = 7.5f
  let mutable e = new Ellipse(Incl=true)
  let mutable endDraw = false
  let mutable offsetDrag = PointF()
  let mutable tib = new TextInBalloon()
  let mutable mtx = new Matrix()

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let handleHitTest (p : PointF) (k : PointF) = (p.X - k.X) * (p.X - k.X) + (p.Y - k.Y) * (p.Y - k.Y) < h * h

  let updText() =
    if e.Location.X <= e.Size.Width + e.Location.X && 
      e.Location.Y <= e.Size.Height + e.Location.Y &&
      tib.FontFamily.FontSize > 0.f then
        tib.RctInscript <- RectangleF(e.Location, e.Size)
        tib.UpdText()
        this.Invalidate()
    
  member this.AddThink ((l:PointF), (s:SizeF), (t:string)) =
    e.Location <- l
    e.FinPt <- PointF(l.X + s.Width, l.Y + s.Height)
    tib.Txt <- t
    e.AddEll(l, s)
    updText()
  
  member this.UpdThink (t:string) = tib.Txt <- t; updText()
  member this.HandleHitTest (p : PointF) (k:PointF) = handleHitTest p k
  member this.Update() = updText()
  member this.GetRect() =
    Rectangle(int e.Location.X, int e.Location.Y, int e.FinPt.X - int e.Location.X, int e.FinPt.Y - int e.Location.Y)

  member this.Size      with get() = e.Size
  member this.Reg       with get() = tib.RegText
  member this.GP        with get() = tib.GPText
  member this.Location  with get() = e.Location and set(v) = e.Location <- v
  member this.FinPt     with get() = e.FinPt    and set(v) = e.FinPt <- v
  member this.EndDraw   with get() = endDraw    and set(v) = endDraw <- v; e.EndDraw <- v
  member this.MVi       with get() = e.MVi      and set(v) = e.MVi <- v
  member this.MVf       with get() = e.MVf      and set(v) = e.MVf <- v
  member this.Txt       with get() = tib.Txt    and set(v) = tib.Txt <- v; this.Invalidate()

  member this.GraphicsTransform with get() = mtx and set(v) = mtx <- v

  member this.GetStyleFont  with get() = tib.FontFamily.GetFontFamily
  member this.SetStyleFont  with set(v : string) = tib.FontFamily.SetFontFamily(v); this.Invalidate()
  member this.FontSize      with set(v : single) = tib.FontFamily.FontSize <- v; this.Invalidate()

  member this.ToUpper
    with get() =
      tib.ToUpper
    and set(v) =
      tib.ToUpper <- v
      tib.RctInscript <- RectangleF(e.Location, e.Size)
      tib.UpdText()
      this.Invalidate()

  override this.OnPaint evt =
    base.OnPaint evt
    let g = evt.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    g.Transform <- mtx
    use p = new Pen(Brushes.White, 5.f)
    g.DrawPath(p, tib.GPText)

    if tib.RegText <> null then g.FillRegion(p.Brush, tib.RegText)

    p.Brush <- Brushes.Black
    p.Width <- 2.5f
    g.DrawPath(p, tib.GPText)

    if not(endDraw) then
      let rc (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
      p.Brush <- Brushes.Blue
      g.DrawRectangle(p, e.Location.X, e.Location.Y, e.FinPt.X - e.Location.X, e.FinPt.Y - e.Location.Y)
      p.Brush <- Brushes.Green
      g.DrawEllipse(p, rc e.Location)
      g.DrawEllipse(p, rc e.FinPt)
      
      p.Brush <- Brushes.DarkGreen

      if e.MVi then g.FillEllipse(p.Brush, rc e.Location)
      if e.MVf then g.FillEllipse(p.Brush, rc e.FinPt)


type ScreamBalloon() as this =
  inherit LWC()

  let max_vertex = 50
  let min_vertex = 3

  let h = 7.5f
  let mutable gpell = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable gpclick = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable regclick : Region = null
  let mutable e = new Ellipse(Incl=true)
  let mutable endDraw = false
  let mutable mvi, mvf = false, false
  let mutable offsetDrag = PointF()
  let mutable clickpts : PointF[] = null
  let mutable idx = -1
  let mutable tib = new TextInBalloon()
  let mutable no_vtx = -1
  let mutable ins_vtx = 0
  let mutable closed = false
  let mutable mtx = new Matrix()

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let updInsRect() =
    tib.RctInscript <-
      RectangleF(
        e.Location.X + (e.Size.Width / 2.f) * (1.f - 1.f / sqrt 2.f),
        e.Location.Y + (e.Size.Height / 2.f) * (1.f - 1.f / sqrt 2.f),
        (e.Size.Width / sqrt 2.f),
        (e.Size.Height / sqrt 2.f))

  let updScream() =
    updInsRect()
    if gpell <> null then gpell.Dispose()
    gpell <- new GraphicsPath(FillMode=FillMode.Winding)
    gpell.AddEllipse(RectangleF(e.Location, e.Size))
    
    if clickpts <> null then
      if gpclick <> null then gpclick.Dispose()
      gpclick <- new GraphicsPath(FillMode=FillMode.Winding)
      if clickpts.Length >= min_vertex then
        gpclick.AddPolygon(clickpts)
        gpclick.CloseFigure()
        if regclick <> null then regclick.Dispose()
        regclick <- new Region(gpclick)
      closed <- true
      tib.UpdText()
    this.Invalidate()

  member this.AddScream ((l:PointF), (s:SizeF)) =
    e.Location <- l
    e.FinPt <- PointF(l.X + s.Width, l.Y + s.Height)
    e.AddEll(l, s)
    if gpclick <> null then gpclick.Dispose()
    gpclick <- new GraphicsPath(FillMode=FillMode.Winding)
    updInsRect()
    if gpell <> null then gpell.Dispose()
    gpell <- new GraphicsPath(FillMode=FillMode.Winding)
    gpell.AddEllipse(RectangleF(e.Location, e.Size))
    tib.UpdText()

  member this.AddPt(p : PointF) = 
    let tmpts = new ResizeArray<PointF>()
    if clickpts <> null then
      for i in 0 .. 1 .. clickpts.Length - 1 do
        tmpts.Add(clickpts.[i])
    tmpts.Add(p)
    clickpts <- tmpts.ToArray()

  member this.HandleHitTest (p : PointF) (k:PointF) = (p.X - k.X) * (p.X - k.X) + (p.Y - k.Y) * (p.Y - k.Y) < h * h
  member this.UpdScream (st:string) = tib.Txt <- st; updScream()
  member this.UpdScream() = updScream()
  member this.GP
    with get() = gpclick
    and set(v) = 
      gpclick <- v
      if regclick <> null then
        regclick.Dispose()
      regclick <- new Region(gpclick)

  member this.RctInside with get() = tib.RctInscript
  member this.Size      with get() = e.Size
  member this.Reg       with get() = regclick
  member this.GPText    with get() = tib.GPText
  member this.GPEll     with get() = gpell
  member this.ClickPts  with get() = clickpts

  member this.Location    with get() = e.Location and set(v) = e.Location <- v; updInsRect()
  member this.FinPt       with get() = e.FinPt    and set(v) = e.FinPt <- v; updInsRect()
  member this.EndDraw     with get() = endDraw    and set(v) = endDraw <- v
  member this.MVidx       with get() = idx        and set(v) = idx <- v
  member this.MVi         with get() = mvi        and set(v) = mvi <- v
  member this.MVf         with get() = mvf        and set(v) = mvf <- v
  member this.Pts         with get() = clickpts   and set(v) = clickpts <- v
  member this.Closed      with get() = closed     and set(v) = closed <- v
  member this.VtxScrmb    with get() = no_vtx     and set(v) = no_vtx <- v
  member this.DrawingVtx  with get() = ins_vtx    and set(v) = ins_vtx <- v

  member this.GraphicsTransform with get() = mtx and set(v) = mtx <- v

  member this.Txt          with get() = tib.Txt                 and set(v) = tib.Txt <- v
  member this.ToUpper      with get() = tib.ToUpper             and set(v) = tib.ToUpper <- v; tib.UpdText()
  member this.FontSize     with get() = tib.FontFamily.FontSize and set(v : single) = tib.FontFamily.FontSize <- v; this.Invalidate()
  member this.GetStyleFont with get() = tib.FontFamily.GetFontFamily
  member this.SetStyleFont with set(v : string) = tib.FontFamily.SetFontFamily(v); this.Invalidate()

  override this.OnPaint evt =
    base.OnPaint evt
    let g = evt.Graphics
    g.SmoothingMode <- SmoothingMode.HighQuality
    g.Transform <- mtx
    use p = new Pen(Brushes.Black, 4.5f)
    g.DrawPath(p, gpclick)
    if regclick <> null then
      p.Brush <- Brushes.White
      g.FillRegion(p.Brush, regclick)
    if tib.RctInscript.Location.X <= tib.RctInscript.Size.Width + tib.RctInscript.Location.X &&
       tib.RctInscript.Location.Y <= tib.RctInscript.Size.Height + tib.RctInscript.Location.X &&
       tib.FontFamily.FontSize > 0.f then
      p.Brush <- Brushes.Black
      p.Width <- 1.f
      g.DrawPath(p, tib.GPText)
      p.Brush <- Brushes.White
      g.FillPath(p.Brush, tib.GPText)

    if not(endDraw) then
      
      p.Brush <- Brushes.Black
      p.Width <- 1.5f
      p.DashStyle <- DashStyle.Dot
      g.DrawPath(p, gpell)
      
      let r (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
      
      p.DashStyle <- DashStyle.Solid
      p.Brush <- Brushes.Blue
      g.DrawRectangle(p, tib.RctInscript.X, tib.RctInscript.Y, tib.RctInscript.Size.Width, tib.RctInscript.Size.Height)
      
      if clickpts <> null then
        if not(closed) then
          p.Brush <- Brushes.Crimson
          g.DrawEllipse(p, r clickpts.[0])
        else
          p.Brush <- Brushes.Lime
          g.DrawEllipse(p, r clickpts.[0])
        p.Brush <- Brushes.Lime
        for i in 1 .. 1 .. clickpts.Length - 1 do
          g.DrawEllipse(p, r clickpts.[i])
      
      if not(closed) then p.Brush <- Brushes.Firebrick
      else p.Brush <- Brushes.LimeGreen
      if idx <> -1 then g.FillEllipse(p.Brush, r clickpts.[idx])

      p.Brush <- Brushes.Green
      g.DrawEllipse(p, r e.Location)
      g.DrawEllipse(p, r e.FinPt)
      p.Brush <- Brushes.DarkGreen
      if mvi then
        g.FillEllipse(p.Brush, r (e.Location))
      if mvf then
        g.FillEllipse(p.Brush, r (e.FinPt))