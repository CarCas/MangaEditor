module Shapes

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open LWC
open CommonTypes

type Rect() =
  inherit LWC()

  let mutable gp : GraphicsPath = null
  let mutable reg : Region = null
  let mutable pts = [| PointF(60.f, 60.f); PointF(80.f, 80.f) |]
  let mutable sz = SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
  let mutable endDraw = false
  let mutable mvi, mvf = false, false

  member this.AddRect ((l:PointF), (s:SizeF)) =
    pts.[0] <- l
    pts.[1] <- PointF(l.X + s.Width, l.Y + s.Height)
    sz <- s
    if gp <> null then gp.Dispose()
    if reg <> null then reg.Dispose()
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    gp.AddRectangle(RectangleF(pts.[0], sz))
    reg <- new Region(gp)

  member this.Location
    with get() = pts.[0]
    and set(v) =
      pts.[0] <- v
      sz <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
      base.Location <- pts.[0]
      base.Size <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
  member this.FinPt
    with get() = pts.[1]
    and set(v) =
      pts.[1] <- v
      sz <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
      base.Size <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
  member this.Size with get() = sz and set(v) = sz <- v; pts.[1] <- PointF(pts.[0].X + sz.Width, pts.[0].Y + sz.Height)
  member this.EndDraw with get() = endDraw and set(v) = endDraw <- v
  member this.MVi with get() = mvi and set(v) = mvi <- v
  member this.MVf with get() = mvf and set(v) = mvf <- v
  member this.GetRect with get() = new Rectangle(Point(int pts.[0].X, int pts.[0].Y), Size(int sz.Width, int sz.Height))

type Polygon() =
  inherit LWC()

  let max_vertex = 50
  let min_vertex = 3

  let mutable pts : PointF [] = null
  let mutable no_vtx = -1
  let mutable incl = true
  let mutable gp = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable reg = new Region()
  let mutable endDraw = false
  let mutable endIns = false
  let mutable mv = -1
  let mutable ins_vtx = 0

  member this.MV with get() = mv and set(v) = mv <- v
  member this.Incl with get() = incl and set(v) = incl <- v
  member this.EndDraw with get() = endDraw and set(v) = endDraw <- v
  member this.EndIns with get() = endIns
  member this.DrawingPoly with get() = ins_vtx and set(v) = ins_vtx <- v
  member this.VtxPoly with get() = no_vtx and set(v) = no_vtx <- v
  member this.Reg with get() = reg and set(v) = reg <- v
  member this.GP
    with get() = gp
    and set(v) = 
      gp <- v
      if reg <> null then reg.Dispose()
      reg <- new Region(gp)
  member this.Pts with get() = pts and set(v) = pts <- v
    
  member this.Update() =
    if gp <> null then gp.Dispose()
    if reg <> null then reg.Dispose()
    if ins_vtx = no_vtx then
      gp <- new GraphicsPath(FillMode=FillMode.Winding)
      gp.AddPolygon(pts)
      reg <- new Region(gp)

  member this.MaxVtx with get() = max_vertex

  member this.AddPt(p : PointF) = 
    let tmpts = new ResizeArray<PointF>()
    if pts <> null then
      for pt in pts do
        tmpts.Add(pt)
    tmpts.Add(p)
    pts <- tmpts.ToArray()    

  member this.AddPoly() =
    if pts <> null then
      if gp <> null then gp.Dispose()
      gp <- new GraphicsPath(FillMode=FillMode.Winding)
      gp.AddPolygon(pts)
      endIns <- true
      if reg <> null then reg.Dispose()
      reg <- new Region(gp)
      this.Invalidate()

type Ellipse() =
  inherit LWC()

  let h = 5.f
  let mutable gp : GraphicsPath = null
  let mutable reg : Region = null
  let mutable sz = SizeF(40.f, 40.f)
  let mutable xi, yi = 50.f, 50.f
  let mutable pts = [| PointF(xi, yi); PointF(xi + sz.Width, yi + sz.Height) |]
  let mutable incl = false
  let mutable endDraw = false
  let mutable endIns = false
  let mutable mvi, mvf = false, false
  let mutable borderWidth = 5.f
  let mutable bord = false
  let mutable rct_inscript =
    RectangleF(pts.[0].X + (sz.Width / 2.f) * (1.f - 1.f / sqrt 2.f), pts.[0].Y + (sz.Height / 2.f) * (1.f - 1.f / sqrt 2.f),
      (sz.Width / sqrt 2.f), (sz.Height / sqrt 2.f))

  let initEll (pf:PointF) =
    if gp <> null then gp.Dispose()
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    gp.AddEllipse(RectangleF(pf, sz))
    if reg <> null then reg.Dispose()
    reg <- new Region(gp)

  member this.AddEll ((l:PointF), (s:SizeF)) =
    pts.[0] <- l
    pts.[1] <- PointF(l.X + s.Width, l.Y + s.Height)
    sz <- s
    initEll l

  member this.AddEll (l:PointF) =
    pts.[0] <- l
    initEll l
    pts.[1] <- PointF(l.X + sz.Width, l.Y + sz.Height)

  member this.Update() =
    if gp <> null then gp.Dispose()
    if reg <> null then reg.Dispose()
    gp <- new GraphicsPath(FillMode=FillMode.Winding)
    gp.AddEllipse(RectangleF(pts.[0], sz))
    reg <- new Region(gp)

  member this.HandleHitTest (p : PointF) (k:PointF) = (p.X - k.X)*(p.X - k.X) + (p.Y - k.Y)*(p.Y - k.Y) < h*h
  member this.Incl with get() = incl and set(v) = incl <- v
  member this.Reg with get() = reg and set(v) = reg <- v
  member this.GP with get() = gp and set(v) = gp <- v
  member this.Location with get() = pts.[0] and set(v) = pts.[0] <- v; sz <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
  member this.FinPt with get() = pts.[1] and set(v) = pts.[1] <- v; sz <- SizeF(pts.[1].X - pts.[0].X, pts.[1].Y - pts.[0].Y)
  member this.Size with get() = sz and set(v) = sz <- v
  member this.EndDraw with get() = endDraw and set(v) = endDraw <- v
  member this.EndIns with get() = endIns and set(v) = endIns <- v
  member this.MVi with get() = mvi and set(v) = mvi <- v
  member this.MVf with get() = mvf and set(v) = mvf <- v
  member this.RectInside with get() = rct_inscript