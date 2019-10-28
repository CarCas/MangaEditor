module Paper

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open LWC
open CommonTypes
open Shapes
open TextInBalloon
open Balloons
open Cartoon

type Paper() as this =
  inherit UserControl()
  
  let h = 30.f
  let maxItemsToShow = 5
  let crt = new ResizeArray<Cartoon>()
  let mutable buffer : Bitmap = null
  let mutable tmpbuffer : Bitmap = null
  let mutable endPaper = false
  let mutable minNoVtx, maxNoVtx = 3.m, 50.m
  let mutable minFont, maxFont = 5.m, 70.m
  let mutable created = false
  let mutable okCloseCreate = false
  let mutable selectionMode, creationMode = false, false

  let mutable selected = -2               (* vignetta selezionata *)
  let mutable secondClick = false         (* al primo click si seleziona, al secondo si deseleziona *)
  let mutable lastMoved = new PointF()    (* ultima posizione rilevata dalla OnMouseMove *)

  let mutable offsetDrag = PointF()
  let mutable idx = -1

  let mutable paper = new Rect()

  let mutable saving = false
  let mutable fileName : string = null

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]
  
  let handleHitTest (p:Point) (k:Point) =
    (p.X - k.X)*(p.X - k.X) + (p.Y - k.Y)*(p.Y - k.Y) < int h * int h

  (* buffer è un oggetto Bitmap che servirà a realizzare il double buffering in modo da non avere flickering nel programma. *)

  let correlateCartoon (c:Cartoon) (p:Point) =
    let bounds = [|
      c.CartoonLocation;
      PointF(c.CartoonLocation.X, c.CartoonLocation.Y + c.CartoonSize.Height);
      PointF(c.CartoonLocation.X + c.CartoonSize.Width, c.CartoonLocation.Y + c.CartoonSize.Height);
      PointF(c.CartoonLocation.X + c.CartoonSize.Width, c.CartoonLocation.Y)
    |]
    c.W2V.TransformPoints(bounds)
    use pa = new GraphicsPath()
    pa.AddLines(bounds)
    pa.CloseFigure()
    pa.IsVisible(p)

  do 
    this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)

  member this.AddCartoon() =
    if crt.Count <> 0 then
      let c = crt.[crt.Count - 1]
      if c.CartoonSize <> SizeF() then
        crt.Add(new Cartoon())
    else
      crt.Add(new Cartoon())
  
  member this.Selection with get() = selectionMode and set(v) = selectionMode <- v

  member this.Creation with get() = creationMode and set(v) = creationMode <- v

  member this.EndPaper
    with get() = endPaper
    and set(v) =
      endPaper <- v
      paper.AddRect(
          PointF(single this.ClientRectangle.Width / 4.f, 0.f),
          SizeF(single this.ClientRectangle.Width * 2.f / 4.f, single this.ClientRectangle.Height)
        )
      this.Invalidate()

  member this.Save() =
    let sfd = new SaveFileDialog(CheckPathExists=true)
    sfd.Filter <- " JPEG Files (*.jpeg) |*.jpeg| JPG Files (*.jpg) |*.jpg| TIFF Files (*.tiff) |*.tiff| PICT Files (*.pict) |*.pict| PNG Files (*.png) |*.png;";
    if sfd.ShowDialog() = DialogResult.OK then
      fileName <- sfd.FileName
      saving <- true
      this.Invalidate()      

  override this.OnMouseDown e =
    base.OnMouseDown e

    if creationMode && not created && not endPaper then
      created <- true
      okCloseCreate <- false
      let f = new Form(Text="Create Cartoon", Size=Size(500, 400))
      f.Show()
      
      let toon = new Cartoon(Dock=DockStyle.Fill, DrawShape=NoSh, DrawBalloon=NoBalloon, NoVtxPolyIn=int minNoVtx, NoVtxPolyOut=int minNoVtx)
     
      let gbShapes = new GroupBox(Location=Point(0, 15), Size=Size(150, 200), Text="Kind of Shapes")
      let rbPI = new RadioButton(Location=Point(5, 30), Size=Size(130, 20), Text="Polygon Included")
      let rbPO = new RadioButton(Location=Point(5, 60), Size=Size(130, 20), Text="Polygon Excluded")
      let rbEI = new RadioButton(Location=Point(5, 90), Size=Size(130, 20), Text="Ellipse Included")
      let rbEO = new RadioButton(Location=Point(5, 120), Size=Size(130, 20), Text="Ellipse Excluded")
      let rbNS = new RadioButton(Location=Point(5, 150), Size=Size(130, 20), Text="No Shape")

      gbShapes.Controls.Add(rbPI)
      gbShapes.Controls.Add(rbPO)
      gbShapes.Controls.Add(rbEI)
      gbShapes.Controls.Add(rbEO)
      gbShapes.Controls.Add(rbNS)

      let lblPI = new Label(Location=Point(170, 45), Size=Size(170, 20), Text="No. Vertex of Polygons Included")
      let svpi = new NumericUpDown(Location=Point(340, 40), Size=Size(40, 10), Maximum=maxNoVtx, Minimum=minNoVtx, Value=minNoVtx)
      let lblPO = new Label(Location=Point(170, 75), Size=Size(170, 20), Text="No. Vertex of Polygons Excluded")
      let svpo = new NumericUpDown(Location=Point(340, 70), Size=Size(40, 10), Maximum=maxNoVtx, Minimum=minNoVtx, Value=minNoVtx)

      let benddraw = new Button(Text="Finish Draw", Location=Point(220, 115), Size=Size(100, 30))

      svpi.ValueChanged.Add(fun v -> if svpi.Value > minNoVtx && svpi.Value < maxNoVtx then toon.NoVtxPolyIn <- int svpi.Value)
      svpo.ValueChanged.Add(fun v -> if svpo.Value > minNoVtx && svpo.Value < maxNoVtx then toon.NoVtxPolyOut <- int svpo.Value)

      rbPI.CheckedChanged.Add(fun ef ->
        toon.DrawShape <- PolyIn
        toon.PressBtnPI <- true
        toon.PressBtnPO <- false
        toon.PressBtnEI <- false
        toon.PressBtnEO <- false
      )

      rbPO.CheckedChanged.Add(fun ef ->
        toon.DrawShape <- PolyOut
        toon.PressBtnPO <- true
        toon.PressBtnPI <- false
        toon.PressBtnEI <- false
        toon.PressBtnEO <- false
      )

      rbEI.CheckedChanged.Add(fun ef ->
        toon.DrawShape <- EllIn
        toon.PressBtnEI <- true
        toon.PressBtnPO <- false
        toon.PressBtnPI <- false
        toon.PressBtnEO <- false
      )

      rbEO.CheckedChanged.Add(fun ef ->
        toon.DrawShape <- EllOut
        toon.PressBtnEO <- true
        toon.PressBtnPO <- false
        toon.PressBtnEI <- false
        toon.PressBtnPI <- false
      )

      rbNS.CheckedChanged.Add(fun ef ->
        toon.DrawShape <- NoSh
        toon.PressBtnEO <- false
        toon.PressBtnPO <- false
        toon.PressBtnEI <- false
        toon.PressBtnPI <- false
      )

      let gbBalloon = new GroupBox(Location=Point(0, 15), Size=Size(150, 210), Text="Kind of Balloons", Visible=false)
      let rbTlk = new RadioButton(Location=Point(5, 30), Size=Size(140, 20), Text="Talkin' Balloon")
      let rbThg = new RadioButton(Location=Point(5, 60), Size=Size(140, 20), Text="Though Balloon")
      let rbThgn = new RadioButton(Location=Point(5, 90), Size=Size(140, 20), Text="Though Naruto Balloon")
      let rbScrm = new RadioButton(Location=Point(5, 120), Size=Size(140, 20), Text="Scream Balloon")
      let rbWhisp = new RadioButton(Location=Point(5, 150), Size=Size(140, 20), Text="Whisper Balloon")
      let rbNoBall = new RadioButton(Location=Point(5, 180), Size=Size(140, 20), Text="No Balloon")

      let gpText = new GroupBox(Location=Point(160, 15), Size=Size(240, 130), Text="Text In Balloons", Visible=false)
      let txtBox = new TextBox(Location=Point(5, 30), Size=Size(140, 20))
      let okButt = new Button(Location=Point(145, 30), Size=Size(30, 20), Text="OK")

      let lblSize = new Label(Text="Size", Location=Point(5, 65), Size=Size(30, 20))
      let fontSize = new NumericUpDown(Location=Point(35, 60), Size=Size(40, 10), Maximum=maxFont, Minimum=minFont, Value=12.m)

      fontSize.ValueChanged.Add(fun x ->
        if fontSize.Value > minFont && fontSize.Value < maxFont then
          toon.FontSize <- single fontSize.Value
      )

      let lblStyles = new Label(Text="Font", Location=Point(80, 65), Size=Size(30, 20))
      let listOfStyles =
        new ComboBox(Location=Point(120, 60), Size=Size(100, 20),
          AutoCompleteMode=AutoCompleteMode.Append, DropDownStyle=ComboBoxStyle.DropDownList,
          MaxDropDownItems=maxItemsToShow)
      let cboxItems = listOfStyles.Items
      cboxItems.AddRange(MatchTypez().GetTypes())

      listOfStyles.SelectedItem <- listOfStyles.Items.IndexOf("Comic Sans MS")

      listOfStyles.SelectedValueChanged.Add(fun evt -> toon.StyleFont <- string listOfStyles.SelectedItem)

      let bendins = new Button(Text="Finish Cartoon", Location=Point(5, 90), Size=Size(100, 20))

      gpText.Controls.Add(txtBox)
      gpText.Controls.Add(okButt)
      gpText.Controls.Add(lblSize)
      gpText.Controls.Add(fontSize)
      gpText.Controls.Add(lblStyles)
      gpText.Controls.Add(listOfStyles)
      gpText.Controls.Add(bendins)

      gbBalloon.Controls.Add(rbTlk)
      gbBalloon.Controls.Add(rbThg)
      gbBalloon.Controls.Add(rbThgn)
      gbBalloon.Controls.Add(rbScrm)
      gbBalloon.Controls.Add(rbWhisp)
      gbBalloon.Controls.Add(rbNoBall)

      txtBox.KeyDown.Add(fun k ->
        if k.KeyCode = Keys.Enter then
          toon.TextB <- txtBox.Text
          txtBox.Text <- ""
          toon.Invalidate()
      )

      okButt.Click.Add(fun k ->
        if txtBox.Text <> null && txtBox.Text <> "" then
          toon.TextB <- txtBox.Text
          txtBox.Text <- ""
          toon.Invalidate()
      )

      benddraw.Click.Add(fun ef ->
        if toon.PolyIn.Count <> 0 || toon.EllIn.Count <> 0 then
          toon.EndDraw <- true
          toon.DrawShape <- NoSh
          toon.PressBtnEO <- false
          toon.PressBtnPO <- false
          toon.PressBtnEI <- false
          toon.PressBtnPI <- false

          toon.DrawBalloon <- NoBalloon
          toon.PressBtnTlkB <- false
          toon.PressBtnThgNB <- false
          toon.PressBtnThgB <- false
          toon.PressBtnScrmB <- false
          toon.IsWhisper <- false

          gbShapes.Visible <- false
          lblPI.Visible <- false
          lblPO.Visible <- false
          svpi.Visible <- false
          svpo.Visible <- false
          benddraw.Visible <- false

          gbBalloon.Visible <- true
          gpText.Visible <- true
      )

      rbTlk.CheckedChanged.Add(fun ef ->
        toon.DrawBalloon <- Speech
        toon.PressBtnTlkB <- true
        toon.PressBtnThgNB <- false
        toon.PressBtnThgB <- false
        toon.PressBtnScrmB <- false
        toon.IsWhisper <- false
      )

      rbThg.Click.Add(fun ef ->
        toon.DrawBalloon <- Thought
        toon.PressBtnTlkB <- false
        toon.PressBtnThgNB <- false
        toon.PressBtnThgB <- true
        toon.PressBtnScrmB <- false
        toon.IsWhisper <- false
      )

      rbThgn.Click.Add(fun ef ->
        toon.DrawBalloon <- ThoughtNarutoStyle
        toon.PressBtnTlkB <- false
        toon.PressBtnThgNB <- true
        toon.PressBtnThgB <- false
        toon.PressBtnScrmB <- false
        toon.IsWhisper <- false
      )

      rbScrm.Click.Add(fun ef ->
        toon.DrawBalloon <- Scream
        toon.PressBtnTlkB <- false
        toon.PressBtnThgNB <- false
        toon.PressBtnThgB <- false
        toon.PressBtnScrmB <- true
        toon.IsWhisper <- false
      )

      rbWhisp.Click.Add(fun ef ->
        toon.DrawBalloon <- Whisper
        toon.PressBtnTlkB <- true
        toon.PressBtnThgNB <- false
        toon.PressBtnThgB <- false
        toon.PressBtnScrmB <- false
        toon.IsWhisper <- true
      )
      
      rbNoBall.Click.Add(fun ef ->
        toon.DrawBalloon <- NoBalloon
        toon.PressBtnTlkB <- false
        toon.PressBtnThgNB <- false
        toon.PressBtnThgB <- false
        toon.PressBtnScrmB <- false
        toon.IsWhisper <- false
      )
      
      bendins.Click.Add(fun ef ->
        if toon.Img <> null then
          toon.EndDraw <- true
          toon.EndIns <- true

          toon.DrawBalloon <- NoBalloon
          toon.PressBtnTlkB <- false
          toon.PressBtnThgNB <- false
          toon.PressBtnThgB <- false
          toon.PressBtnScrmB <- false
          toon.IsWhisper <- false

          gbBalloon.Visible <- false
          okCloseCreate <- true
          crt.Add(toon)
          created <- false
          f.Close()
          crt.[crt.Count - 1].Invalidate()
          this.Invalidate()
      )

      f.Controls.Add(gbBalloon)
      f.Controls.Add(gbShapes)
      f.Controls.Add(benddraw)
      f.Controls.Add(lblPI)
      f.Controls.Add(svpi)
      f.Controls.Add(lblPO)
      f.Controls.Add(svpo)
      f.Controls.Add(gpText)

      f.Controls.Add(toon)

      f.FormClosing.Add(fun e ->
        if not okCloseCreate then
          let mb =
            MessageBox.Show("Do you wanna close without finishing?", "Wait ...", MessageBoxButtons.YesNo, MessageBoxIcon.Warning)
          if mb = DialogResult.Yes then okCloseCreate <- true; e.Cancel <- false
          else e.Cancel <- true
          created <- false
          creationMode <- true

      )

    if crt.Count <> 0 && selectionMode && not endPaper then
      let mutable el = -1
      let mutable i = crt.Count - 1
      while el = -1 && i >= 0 do
        if correlateCartoon crt.[i] e.Location then
          el <- i
        i <- i - 1
      if el <> -1 then
        secondClick <- el = selected
        selected <- el
        lastMoved <- transformP crt.[el].V2W e.Location

    if endPaper then
      if handleHitTest (Point(int paper.Location.X, int paper.Location.Y)) e.Location then
        offsetDrag <- PointF(paper.Location.X - single e.Location.X, paper.Location.Y - single e.Location.Y)
        paper.MVi <- true
      elif handleHitTest (Point(int paper.FinPt.X, int paper.FinPt.Y)) e.Location then
        offsetDrag <- PointF(paper.FinPt.X - single e.Location.X, paper.FinPt.Y - single e.Location.Y)
        paper.MVf <- true
    this.Invalidate()

  override this.OnMouseMove e =
    base.OnMouseMove e
    if selectionMode then
      if selected <> -2 then
        secondClick <- true
        let p = transformP crt.[selected].V2W e.Location
        crt.[selected].W2V.Translate(single (p.X - lastMoved.X), single (p.Y - lastMoved.Y))
        lastMoved <- p
        crt.[selected].Invalidate()
        this.Invalidate()
    if endPaper then
      if paper.MVi then
        if offsetDrag.X + single e.Location.X <= paper.FinPt.X - 10.f &&
          offsetDrag.Y + single e.Location.Y <= paper.FinPt.Y - 10.f then
          paper.Location <- PointF(offsetDrag.X + single e.Location.X, offsetDrag.Y + single e.Location.Y)
          this.Invalidate()
      elif paper.MVf then
        if offsetDrag.X + paper.FinPt.X >= paper.Location.X + 10.f &&
          offsetDrag.Y + paper.FinPt.Y >= paper.Location.Y + 10.f then
          paper.FinPt <- PointF(offsetDrag.X + single e.Location.X, offsetDrag.Y + single e.Location.Y)
          this.Invalidate()

  override this.OnMouseUp e =
    base.OnMouseUp e
    if selectionMode then
      if selected <> -2 then
        crt.[selected].Invalidate()
        if secondClick then selected <- -2; secondClick <- false; this.Invalidate()
    if endPaper then
      if paper.MVi then paper.MVi <- false; this.Invalidate()
      elif paper.MVf then paper.MVf <- false; this.Invalidate()

  override this.OnKeyDown e =
    base.OnKeyDown e
    if selected <> -2 then crt.[selected].HandleCommand(e.KeyCode)
    else crt |> Seq.iter(fun c -> c.HandleCommand(e.KeyCode))
    this.Invalidate()

  override this.OnKeyUp e =
    base.OnKeyUp e
    if selected <> -2 then
      if e.KeyCode = Keys.X then crt.RemoveAt(selected); selected <- -2; this.Invalidate()
    
  override this.OnPaintBackground e = ()

  override this.OnPaint e =
    base.OnPaint e

    if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
      if buffer <> null then buffer.Dispose()
      buffer <- new Bitmap(this.Width, this.Height)

    let g = Graphics.FromImage(buffer)
    g.SmoothingMode <- SmoothingMode.HighQuality
    use bg = new SolidBrush(this.BackColor)
    g.FillRectangle(bg, 0, 0, buffer.Width, buffer.Height)

    if endPaper then g.FillRectangle(Brushes.White, paper.GetRect)

    if crt.Count = 0 then e.Graphics.DrawImage(buffer, 0, 0)
    else
      for i in 0 .. 1 .. crt.Count - 1 do
        let vg = e.Graphics
        let save = g.Save()
        let c = crt.[i]
        g.MultiplyTransform(c.W2V)
        let hsave = c.HandlerSize

        let mk_rct (pnt : PointF) = RectangleF(pnt.X - c.HandlerSize, pnt.Y - c.HandlerSize, 2.f * c.HandlerSize, 2.f * c.HandlerSize)

        if c.GP <> null && not(c.EndIns) then g.DrawPath(Pens.Black, c.GP)
        if c.EndDraw && c.RegToon <> null then
          g.Clip <- c.RegToon

          if c.Img <> null then
            if not(c.PolyIn.Count = 0 && c.PolyOut.Count = 0 && c.EllIn.Count = 0 && c.EllOut.Count = 0) then
              g.DrawImage(c.Img, RectangleF(c.CartoonLocation, c.CartoonSize))

        g.DrawRectangle(Pens.LimeGreen, c.CartoonLocation.X, c.CartoonLocation.Y, c.CartoonSize.Width, c.CartoonSize.Height)

        if not c.EndIns then
          if selected <> -2 then
            g.DrawRectangle(Pens.Blue, c.CartoonLocation.X, c.CartoonLocation.Y, c.CartoonSize.Width, c.CartoonSize.Height)
          else
            g.DrawRectangle(Pens.Red, c.CartoonLocation.X, c.CartoonLocation.Y, c.CartoonSize.Width, c.CartoonSize.Height)

        let drawHandlesNotEndIns(rp : ResizeArray<Polygon>) =
          if not(Seq.isEmpty(rp)) then
            let p = rp.[rp.Count - 1]
            if not p.EndDraw then
              if p.DrawingPoly > 0 && p.DrawingPoly < p.VtxPoly then
                c.HandlerSize <- 10.f
                for i in 0 .. 1 .. p.Pts.Length - 1 do
                  if not c.PressX then
                    g.DrawEllipse(Pens.Green, mk_rct(p.Pts.[i]))

        if not c.EndIns then
          drawHandlesNotEndIns(c.PolyIn)
          drawHandlesNotEndIns(c.PolyOut)

          c.HandlerSize <- hsave
          
          let drawHandlesPolyEndIns(rp : ResizeArray<Polygon>, s : Shp) =
            if rp <> null then
              for i in rp.Count - 1 .. -1 .. 0 do
                if rp.[i].EndIns then
                  for pt in rp.[i].Pts do
                    g.DrawEllipse(Pens.Green, mk_rct(pt))
                  if rp.[i].MV <> -1 && not c.PressX && c.HndlMV = s then
                    g.FillEllipse(Brushes.DarkGreen, mk_rct(rp.[i].Pts.[rp.[i].MV]))

          let drawHandlesEllEndIns(rp : ResizeArray<Ellipse>, s : Shp) =
            if rp <> null then
              for i in rp.Count - 1 .. -1 .. 0 do
                let e = rp.[i]
                if not e.EndIns then
                  g.DrawEllipse(Pens.Green, mk_rct(e.Location))
                g.DrawEllipse(Pens.Green, mk_rct(e.FinPt))
                if fst c.PressEllIn = 1 && not(c.PressX) && c.HndlMV = EllIn then
                  g.FillEllipse(Brushes.DarkGreen, mk_rct(c.EllIn.[snd c.PressEllIn].Location))
                elif fst c.PressEllIn = 2 && not(c.PressX) && c.HndlMV = EllIn then
                  g.FillEllipse(Brushes.DarkGreen, mk_rct(c.EllIn.[snd c.PressEllIn].FinPt))

          let drawHandles(s : Shp) =
            match s with
            | PolyIn -> drawHandlesPolyEndIns(c.PolyIn, PolyIn)
            | PolyOut -> drawHandlesPolyEndIns(c.PolyOut, PolyOut)
            | EllIn -> drawHandlesEllEndIns(c.EllIn, EllIn)
            | EllOut -> drawHandlesEllEndIns(c.EllOut, EllOut)
            | NoSh -> ()

          drawHandles(PolyIn)
          drawHandles(PolyOut)
          drawHandles(EllIn)
          drawHandles(EllOut)

        g.Restore(save)
        vg.DrawImage(buffer, 0, 0)

      for i in 0 .. 1 .. crt.Count - 1 do
        let vg = e.Graphics
        let save = g.Save()
        for j in crt.[i].TlkB.Count - 1 .. -1 .. 0 do

          let t = crt.[i].TlkB.[j]
          use p = new Pen(Brushes.Black, 4.5f)
          let rct = t.RctInside

          let gSaveForTlkB = g.Save()
            
          if crt.[i].EndIns then t.EndDraw <- true
          else t.EndDraw <- false
            
          if t.Whisper then
            p.DashStyle <- DashStyle.Dash
            p.DashCap <- DashCap.Round
          else
            p.DashStyle <- DashStyle.Solid
            
          g.Transform <- t.GraphicsTransform
          g.DrawPath(p, t.GP)
          p.DashStyle <- DashStyle.Solid
          g.FillRegion(Brushes.White, t.Reg)
          if t.RegText <> null &&
            rct.Location.X <= rct.Location.X + rct.Size.Width &&
            rct.Location.Y <= rct.Location.Y + rct.Size.Height then
              p.Brush <- Brushes.White
              g.FillRegion(p.Brush, t.RegText)
          if rct.Location.X <= rct.Size.Width + rct.Location.X &&
              rct.Location.Y <= rct.Size.Height + rct.Location.Y &&
              t.FontSize > 0.f then
            p.Brush <- Brushes.White
            g.FillRegion(p.Brush, t.Reg)
            p.Brush <- Brushes.Black
            p.Width <- 1.f
            g.DrawPath(p, t.GPText)
            p.Brush <- Brushes.White
            g.FillPath(p.Brush, t.GPText)
          if not(t.EndDraw) then
            let r (x:PointF, h:single) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
            p.Width <- 2.f
            p.Brush <- Brushes.Green
            g.DrawEllipse(p, r (t.Location, h))
            g.DrawEllipse(p, r (t.FinPt, h))
            p.Brush <- Brushes.Gold
            p.Width <- 1.5f
            p.Brush <- Brushes.LawnGreen
            g.DrawEllipse(p, r (t.TlkPt, h * 1.25f))
            p.Brush <- Brushes.DarkGreen
            if t.MVi then g.FillEllipse(p.Brush, r (t.Location, h))
            if t.MVf then g.FillEllipse(p.Brush, r (t.FinPt, h))
            p.Brush <- Brushes.Green
            if t.MVt then g.FillEllipse(p.Brush, r (t.TlkPt, h * 1.25f))

          g.Restore(gSaveForTlkB)

        for j in crt.[i].ThgB.Count - 1 .. -1 .. 0 do

          let t = crt.[i].ThgB.[j]
          let rct = t.RctInside
          use p = new Pen(Brushes.Black, 4.5f)

          let gSaveForThgB = g.Save()

          if crt.[i].EndIns then t.EndDraw <- true
          else t.EndDraw <- false

          g.Transform <- t.GraphicsTransform

          let drawAndFill(gp : GraphicsPath, r : Region) =
            g.DrawPath(p, gp)
            p.Brush <- Brushes.White
            g.FillRegion(p.Brush, r)
            p.Brush <- Brushes.Black

          drawAndFill(t.GP, t.RegSmall)
          drawAndFill(t.GPSmall, t.RegMiddle)
          drawAndFill(t.GPMiddle, t.RegLarge)
                    
          g.DrawPath(p, t.GPLarge)

          if rct.Location.X <= rct.Size.Width + rct.Location.X &&
              rct.Location.Y <= rct.Size.Height + rct.Location.X &&
              t.FontSize > 0.f then
            p.Brush <- Brushes.Black
            p.Width <- 1.f
            g.DrawPath(p, t.GPText)
            p.Brush <- Brushes.White
            g.FillPath(p.Brush, t.GPText)

          if not(t.EndDraw) then
            let r (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
            p.Width <- 2.f
            p.Brush <- Brushes.Green
            g.DrawEllipse(p, r t.Location)
            g.DrawEllipse(p, r t.FinPt)
            p.Brush <- Brushes.Gold
            g.DrawEllipse(p, r t.Think)
            p.Brush <- Brushes.DarkGreen
            if t.MVLocation then g.FillEllipse(p.Brush, r (t.Location))
            if t.MVFinPt then g.FillEllipse(p.Brush, r (t.FinPt))
            if t.MVThink then p.Brush <- Brushes.Goldenrod; g.FillEllipse(p.Brush, r t.Think)

          g.Restore(gSaveForThgB)

        for j in crt.[i].ThgNB.Count - 1 .. -1 .. 0 do

          let t = crt.[i].ThgNB.[j]
          use p = new Pen(Brushes.White, 5.f)
          let gSaveForThgNB = g.Save()

          if crt.[i].EndIns then crt.[i].ThgNB.[j].EndDraw <- true
          else crt.[i].ThgNB.[j].EndDraw <- false

          g.Transform <- t.GraphicsTransform
          g.DrawPath(p, t.GP)
          if t.Reg <> null then g.FillRegion(p.Brush, t.Reg)

          p.Brush <- Brushes.Black
          p.Width <- 2.5f
          g.DrawPath(p, t.GP)
          if not(t.EndDraw) then
            let rc (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
            p.Brush <- Brushes.Blue
            g.DrawRectangle(p, t.GetRect())
            p.Brush <- Brushes.Green
            g.DrawEllipse(p, rc t.Location)
            g.DrawEllipse(p, rc t.FinPt)
            p.Brush <- Brushes.DarkGreen
            if t.MVi then g.FillEllipse(p.Brush, rc t.Location)
            if t.MVf then g.FillEllipse(p.Brush, rc t.FinPt)
          g.Restore(gSaveForThgNB)

        for j in crt.[i].ScrmB.Count - 1 .. -1 .. 0 do

          let s = crt.[i].ScrmB.[j]
          use p = new Pen(Brushes.Black, 4.5f)
          let gSaveForScrmB = g.Save()

          if crt.[i].EndIns then crt.[i].ScrmB.[i].EndDraw <- true
          else crt.[i].ScrmB.[i].EndDraw <- false

          g.Transform <- s.GraphicsTransform
          g.DrawPath(p, s.GP)
          if s.Reg <> null then
            p.Brush <- Brushes.White
            g.FillRegion(p.Brush, s.Reg)
          if s.RctInside.Location.X <= s.RctInside.Size.Width + s.RctInside.Location.X &&
              s.RctInside.Location.Y <= s.RctInside.Size.Height + s.RctInside.Location.X &&
              s.FontSize > 0.f then
            p.Brush <- Brushes.Black
            p.Width <- 1.f
            g.DrawPath(p, s.GPText)
            p.Brush <- Brushes.White
            g.FillPath(p.Brush, s.GPText)

          if not(s.EndDraw) then
            p.Brush <- Brushes.Black
            p.Width <- 2.5f
            p.DashStyle <- DashStyle.Dot
            g.DrawPath(p, s.GPEll)
            let r (x:PointF) = RectangleF(x.X - h, x.Y - h, 2.f * h, 2.f * h)
            p.DashStyle <- DashStyle.Solid
            p.Brush <- Brushes.Blue
            g.DrawRectangle(p, s.RctInside.X, s.RctInside.Y, s.RctInside.Size.Width, s.RctInside.Size.Height)
            if s.ClickPts <> null then
              if not(s.Closed) then p.Brush <- Brushes.Crimson; g.DrawEllipse(p, r s.ClickPts.[0])
              else p.Brush <- Brushes.Lime; g.DrawEllipse(p, r s.ClickPts.[0])
              p.Brush <- Brushes.Lime
              for i in 1 .. 1 .. s.ClickPts.Length - 1 do g.DrawEllipse(p, r s.ClickPts.[i])
            if not(s.Closed) then p.Brush <- Brushes.Firebrick
            else p.Brush <- Brushes.LimeGreen
            if idx <> -1 then g.FillEllipse(p.Brush, r s.ClickPts.[idx])
            p.Brush <- Brushes.Green
            g.DrawEllipse(p, r s.Location)
            g.DrawEllipse(p, r s.FinPt)
            p.Brush <- Brushes.DarkGreen
            if s.MVi then g.FillEllipse(p.Brush, r s.Location)
            if s.MVf then g.FillEllipse(p.Brush, r s.FinPt)
          g.Restore(gSaveForScrmB)

        g.Restore(save)
        vg.DrawImage(buffer, 0, 0)

    if endPaper then

      if saving then
        let mutable bmp = new Bitmap(paper.GetRect.Width, paper.GetRect.Height)
        let g = Graphics.FromImage(bmp)
        g.DrawImage(buffer, 0, 0, paper.GetRect, GraphicsUnit.Pixel)
        bmp.Save(fileName)
        saving <- false
        g.Dispose()
        bmp.Dispose()

      let mk_rct (pnt : PointF) = Rectangle(int pnt.X - int h, int pnt.Y - int h, 2 * int h, 2 * int h)

      g.FillRectangle(Brushes.White, paper.GetRect)
      tmpbuffer <- new Bitmap(this.Width, this.Height)
      use tmpg = Graphics.FromImage(tmpbuffer)
      use penEndPaper = new Pen(Brushes.Black, 5.f)
      tmpg.SmoothingMode <- SmoothingMode.HighQuality
      tmpg.DrawRectangle(penEndPaper, paper.GetRect)

      penEndPaper.Brush <- Brushes.Green
      tmpg.DrawEllipse(penEndPaper, mk_rct(paper.Location))
      tmpg.DrawEllipse(penEndPaper, mk_rct(paper.FinPt))

      if paper.MVi then
        penEndPaper.Brush <- Brushes.DarkGreen
        tmpg.FillEllipse(penEndPaper.Brush, mk_rct(paper.Location))
      if paper.MVf then
        penEndPaper.Brush <- Brushes.DarkGreen
        tmpg.FillEllipse(penEndPaper.Brush, mk_rct(paper.FinPt))
      e.Graphics.DrawImage(tmpbuffer, 0, 0)