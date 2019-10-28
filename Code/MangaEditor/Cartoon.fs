module Cartoon

open System.Windows.Forms
open System.Drawing
open System.Drawing.Drawing2D
open System.IO
open LWC
open CommonTypes
open Shapes
open TextInBalloon
open Balloons

type public Cartoon() as this =
  inherit LWContainer()
  
  let mutable h = 7.5f

  let mutable w2v = new Matrix()
  let mutable v2w = new Matrix()

  (* SHAPES *)
  let def_vtx = 3
  let mutable endIns = false
  let mutable location = PointF()
  let mutable size = SizeF()
  let mutable vtxPolyIn = -1
  let mutable vtxPolyOut = -1
  let mutable pressX = false
  let mutable drawShape : Shp = NoSh
  let mutable offsetDrag = PointF()
  (*  il primo -1 indica il punto:
        1 => iniziale;
        2 => finale;
        i => gli altri (solo per poligono);
        -1 => nessun punto;
      il secondo indica la figura.
  *)
  let mutable pressEllIn = (-1, -1)
  let mutable pressEllOut = (-1, -1)
  let mutable pressPolyIn = (-1, -1)
  let mutable pressPolyOut = (-1, -1)
  let mutable endDraw = false
  let mutable gp = new GraphicsPath(FillMode=FillMode.Winding)
  let mutable polyIn = new ResizeArray<Polygon>()
  let mutable polyOut = new ResizeArray<Polygon>()
  let mutable ellIn = new ResizeArray<Ellipse>()
  let mutable ellOut = new ResizeArray<Ellipse>()
  let mutable pressbtnPI = false
  let mutable pressbtnPO = false
  let mutable pressbtnEI = false
  let mutable pressbtnEO = false
  let mutable hndlMV : Shp = NoSh
  let mutable regToon : Region = null
  let mutable finished = false

  (* BALLOONS *)
  let mutable tlkb = new ResizeArray<TalkingBalloon>()
  let mutable thgb = new ResizeArray<ThoughtBalloon>()
  let mutable thgnb = new ResizeArray<ThoughtNarutoBalloon>()
  let mutable scrmb = new ResizeArray<ScreamBalloon>()
  let mutable pressbtnTlkB = false
  let mutable pressbtnThgB = false
  let mutable pressbtnThgNB = false
  let mutable pressbtnScrmB = false
  let mutable isWhisper = false
  let mutable pressTlk = (-1, -1)
  let mutable pressScrm = (-1, -1)
  let mutable pressThg = (-1, -1)
  let mutable pressThgN = (-1, -1)

  let mutable text : string = null
  let mutable styleFont : string = null
  let mutable fontSize : single = 12.f
  let mutable toUpper = false

  let mutable selFromOut = false
  let mutable selB = (NoBalloon, -1) (* KndBalloon, whichShape *)
  let mutable drawBalloon : KndBalloon = NoBalloon
  let mutable hndlMVB = (NoBalloon, -1, new PointF(-1.f, -1.f)) (* KndBalloon, whichShape, MVvertex *)
  let minVtx4Scrm = 3
  let mutable img : Image = null

  let transformP (m:Matrix) (p:Point) =
    let a = [| PointF(single p.X, single p.Y) |]
    m.TransformPoints(a)
    a.[0]

  let getBounds() =
    if polyIn.Count = 0 && ellIn.Count = 0 then
      location <- PointF()
      size <- SizeF()
    else
      let mutable minX = 0.f
      let mutable minY = 0.f
      let mutable maxX = 0.f
      let mutable maxY = 0.f
      if polyIn.Count <> 0 then
        minX <- polyIn.Item(0).Pts.[0].X
        minY <- polyIn.Item(0).Pts.[0].Y
        maxX <- polyIn.Item(0).Pts.[0].X
        maxY <- polyIn.Item(0).Pts.[0].Y
      else
        if ellIn.Count <> 0 then
          minX <- ellIn.Item(0).Location.X
          minY <- ellIn.Item(0).Location.Y
          maxX <- ellIn.Item(0).FinPt.X
          maxY <- ellIn.Item(0).FinPt.Y
      for p in polyIn do
        for i in 0 .. 1 .. p.Pts.Length - 1 do
          let pt = p.Pts.[i]
          if pt.X < minX then minX <- pt.X
          elif pt.X > maxX then maxX <- pt.X
          if pt.Y < minY then minY <- pt.Y
          elif pt.Y > maxY then maxY <- pt.Y
      for e in ellIn do
        if e.Location.X < minX then minX <- e.Location.X
        elif e.Location.X > maxX then maxX <- e.Location.X
        if e.Location.Y < minY then minY <- e.Location.Y
        elif e.Location.Y > maxY then maxY <- e.Location.Y
        if e.FinPt.X < minX then minX <- e.FinPt.X
        elif e.FinPt.X > maxX then maxX <- e.FinPt.X
        if e.FinPt.Y < minY then minY <- e.FinPt.Y
        elif e.FinPt.Y > maxY then maxY <- e.FinPt.Y
      location <- PointF(minX, minY)
      size <- SizeF(maxX - minX, maxY - minY)

  let correlateImg (m : Matrix) (p : Point) =
    let bounds = [|
      location;
      PointF(location.X, location.Y + size.Height);
      PointF(location.X + size.Width, location.Y + size.Height);
      PointF(location.X + size.Width, location.Y)
    |]
    m.TransformPoints(bounds)
    use tmpgp = new GraphicsPath()
    tmpgp.AddLines(bounds)
    tmpgp.CloseFigure()
    tmpgp.IsVisible(p)

  (* Si gestisce l'inserimento dell'immagine all'interno della vignetta *)
  let loadImg () = 
    if regToon <> null && hndlMV = NoSh then
      if img = null then
        let ofd = new OpenFileDialog(CheckFileExists=true)
        if ofd.ShowDialog() = DialogResult.OK then
          let mutable stream = ofd.OpenFile()
          while img = null do
            if stream <> null then
              try
                img <- Image.FromStream(stream)
              with
                | :? System.ArgumentException -> stream <- null; img <- null
          getBounds()
      else
        getBounds()
      this.Invalidate()            

  let handleHitTest (p:PointF) (k:PointF) = (p.X - k.X)*(p.X - k.X) + (p.Y - k.Y)*(p.Y - k.Y) < h * h

  do this.SetStyle(ControlStyles.OptimizedDoubleBuffer ||| ControlStyles.AllPaintingInWmPaint, true)
  
  (* buffer è un oggetto Bitmap che servirà a realizzare il double buffering in modo da non avere flickering nel programma. 
     La funzione updateBuffer si assicura che la Bitmap mantenga le stesse dimensioni del controllo. *)
  let mutable buffer : Bitmap = null
  let updateBuffer () =
    if buffer = null || buffer.Width <> this.Width || buffer.Height <> this.Height then
      if buffer <> null then buffer.Dispose()
      buffer <- new Bitmap(this.Width, this.Height)

  let updRegAndGP() =
    if gp <> null then gp.Dispose()
    if regToon <> null then regToon.Dispose()
    gp <- new GraphicsPath(FillMode=FillMode.Winding) 
    regToon <- new Region()
    regToon.MakeEmpty()
    polyIn |> Seq.iter(fun p ->
      if p.DrawingPoly = p.VtxPoly then
        use gpl = new GraphicsPath(FillMode=FillMode.Winding)
        gpl.AddPolygon(p.Pts)
        gp.AddPath(gpl, false)
        regToon.Union(gpl)
    )
    ellIn |> Seq.iter(fun e ->
      use gpe = new GraphicsPath(FillMode=FillMode.Winding)
      gpe.AddEllipse(RectangleF(e.Location, e.Size))
      gp.AddPath(gpe, false)
      regToon.Union(gpe)
    )
    polyOut |> Seq.iter(fun p ->
      if p.DrawingPoly = p.VtxPoly then
        use gpl = new GraphicsPath(FillMode=FillMode.Winding)
        gpl.AddPolygon(p.Pts)
        gp.AddPath(gpl, false)
        regToon.Exclude(gpl)
    )
    ellOut |> Seq.iter(fun e ->
      use gpe = new GraphicsPath(FillMode=FillMode.Winding)
      gpe.AddEllipse(RectangleF(e.Location, e.Size))
      gp.AddPath(gpe, false)
      regToon.Exclude(gpe)
    )
    getBounds()

  let deleteB(k : KndBalloon, i : int) =
    match k with
    | Speech | Whisper ->
      this.LWControls.Remove(tlkb.[i]) |> ignore
      tlkb.RemoveAt(i) |> ignore
    | Thought ->
      this.LWControls.Remove(thgb.[i]) |> ignore
      thgb.RemoveAt(i) |> ignore
    | ThoughtNarutoStyle ->
      this.LWControls.Remove(thgnb.[i]) |> ignore
      thgnb.RemoveAt(i) |> ignore
    | Scream ->
      this.LWControls.Remove(scrmb.[i]) |> ignore
      scrmb.RemoveAt(i) |> ignore
    | NoBalloon -> ()
    this.Invalidate()

  let deleteSh() =
    let spi = snd pressPolyIn
    let spo = snd pressPolyOut
    let sei = snd pressEllIn
    let seo = snd pressEllOut
    if spi <> -1 then
      this.LWControls.Remove(polyIn.[spi]) |> ignore
      polyIn.RemoveAt(spi) |> ignore
    elif spo <> -1 then
      this.LWControls.Remove(polyOut.[spo]) |> ignore
      polyOut.RemoveAt(spo) |> ignore
    elif sei <> -1 then
      this.LWControls.Remove(ellIn.[sei]) |> ignore
      ellIn.RemoveAt(sei) |> ignore
    elif seo <> -1 then
      this.LWControls.Remove(ellOut.[seo]) |> ignore
      ellOut.RemoveAt(seo) |> ignore
    updRegAndGP()
    if polyIn.Count = 0 && polyOut.Count = 0 && ellIn.Count = 0 && ellOut.Count = 0 then
      if img <> null then img.Dispose()
    this.Invalidate()
   
  let handleCommand k =
    let cx = this.ClientSize.Width / 2
    let cy = this.ClientSize.Height / 2
    match k with
    | Keys.W ->
      w2v.Translate(0.f, -10.f, MatrixOrder.Append)
      v2w.Translate(0.f, 10.f)
    | Keys.S ->
      w2v.Translate(0.f, 10.f, MatrixOrder.Append)
      v2w.Translate(0.f, -10.f)
    | Keys.A ->
      w2v.Translate(-10.f, 0.f, MatrixOrder.Append)
      v2w.Translate(10.f, 0.f)
    | Keys.D ->
      w2v.Translate(10.f, 0.f, MatrixOrder.Append)
      v2w.Translate(-10.f, 0.f)
    | Keys.E ->
      w2v.Translate(single(-cx), single(-cy), MatrixOrder.Append)
      w2v.Rotate(10.f, MatrixOrder.Append)
      w2v.Translate(single(cx), single(cy), MatrixOrder.Append)
      v2w.Translate(single(cx), single(cy))
      v2w.Rotate(-10.f)
      v2w.Translate(single(-cx), single(-cy))
    | Keys.Q -> 
      w2v.Translate(single(-cx), single(-cy), MatrixOrder.Append)
      w2v.Rotate(-10.f, MatrixOrder.Append)
      w2v.Translate(single(cx), single(cy), MatrixOrder.Append)
      v2w.Translate(single(cx), single(cy))
      v2w.Rotate(10.f)
      v2w.Translate(single(-cx), single(-cy))
    | Keys.Z ->
      w2v.Scale(1.1f, 1.1f, MatrixOrder.Append)
      v2w.Scale(1.f/1.1f, 1.f/1.1f)
    | Keys.C ->
      w2v.Scale(1.f/1.1f, 1.f/1.1f, MatrixOrder.Append)
      v2w.Scale(1.1f, 1.1f)
    | Keys.X ->
      pressX <- true
    | _      -> ()
    if not(pressX) then updRegAndGP()
    this.Invalidate()
  
  let selectingBalloon(p : PointF) =
    let mutable sel = (NoBalloon, -1)
    for i in tlkb.Count - 1 .. -1 .. 0 do
      if tlkb.[i].GP.IsOutlineVisible(p, Pens.Black) then sel <- (Speech, i)
    for i in thgb.Count - 1 .. -1 .. 0 do
      if thgb.[i].GP.IsOutlineVisible(p, Pens.Black) then sel <- (Thought, i)
    for i in thgnb.Count - 1 .. -1 .. 0 do
      if thgnb.[i].GP.IsOutlineVisible(p, Pens.Black) then sel <- (ThoughtNarutoStyle, i)
    for i in scrmb.Count - 1 .. -1 .. 0 do
      if scrmb.[i].GP.IsOutlineVisible(p, Pens.Black) then sel <- (Scream, i)
    sel

  member this.UpdGPandRegToon() = updRegAndGP()

  member this.HitTest(p:Point) =
    let l = transformP v2w p
    l.X > location.X && l.Y > location.Y && l.X < location.X + size.Width && l.Y < location.Y + size.Height

  member this.NoVtxPolyIn   with get() = vtxPolyIn    and set(v) = vtxPolyIn <- v
  member this.NoVtxPolyOut  with get() = vtxPolyOut   and set(v) = vtxPolyOut <- v
  member this.DrawShape     with get() = drawShape    and set(v) = drawShape <- v
  member this.DrawBalloon   with get() = drawBalloon  and set(v) = drawBalloon <- v

  member this.PressBtnPI with get() = pressbtnPI and set(v) = pressbtnPI <- v
  member this.PressBtnPO with get() = pressbtnPO and set(v) = pressbtnPO <- v
  member this.PressBtnEI with get() = pressbtnEI and set(v) = pressbtnEI <- v
  member this.PressBtnEO with get() = pressbtnEO and set(v) = pressbtnEO <- v
  member this.PolyIn  with get() = polyIn 
  member this.PolyOut with get() = polyOut
  member this.EllIn   with get() = ellIn 
  member this.EllOut  with get() = ellOut

  member this.PressBtnTlkB  with get() = pressbtnTlkB   and set(v) = pressbtnTlkB <- v
  member this.PressBtnThgB  with get() = pressbtnThgB   and set(v) = pressbtnThgB <- v
  member this.PressBtnThgNB with get() = pressbtnThgNB  and set(v) = pressbtnThgNB <- v
  member this.PressBtnScrmB with get() = pressbtnScrmB  and set(v) = pressbtnScrmB <- v
  member this.TlkB  with get() = tlkb 
  member this.ThgB  with get() = thgb 
  member this.ThgNB with get() = thgnb 
  member this.ScrmB with get() = scrmb 

  member this.CartoonLocation with get() = location and set(v) = location <- v; this.Invalidate()
  member this.CartoonSize with get() = size and set(v) = size <- v

  member this.EndDraw
    with get() = endDraw
    and set(v) =
      endDraw <- v
      polyIn |> Seq.iter(fun p -> p.EndDraw <- v)
      polyOut |> Seq.iter(fun p -> p.EndDraw <- v)
      ellIn |> Seq.iter(fun e -> e.EndDraw <- v)
      ellOut |> Seq.iter(fun e -> e.EndDraw <- v)
      if not(v) then loadImg()
      this.Invalidate()

  member this.EndIns          with get() = endIns     and set(v) = endIns <- v; this.Invalidate()
  member this.IsWhisper       with get() = isWhisper  and set(v) = isWhisper <- v
  member this.TextB           with get() = text       and set(v) = text <- v
  member this.StyleFont       with get() = styleFont  and set(v) = styleFont <- v; this.Invalidate()
  member this.FontSize        with get() = fontSize   and set(v) = fontSize <- v; this.Invalidate()
  member this.SelectedBalloon with get() = selB       and set(v) = selB <- v
  member this.Selected        with get() = selFromOut and set(v) = selFromOut <- v
  member this.Finished        with get() = finished   and set(v) = finished <- v
  member this.HandlerSize     with get() = h          and set(v) = h <- v

  member this.V2W         with get() = v2w 
  member this.W2V         with get() = w2v 
  member this.RegToon     with get() = regToon 
  member this.GP          with get() = gp 
  member this.Img         with get() = img
  member this.PressX      with get() = pressX
  member this.HndlMV      with get() = hndlMV
  member this.PressEllIn  with get() = pressEllIn
  member this.PressEllOut with get() = pressEllOut

  member this.HandleCommand(k : Keys) = handleCommand(k)

  (* Si inizia l'interazione con l'utente, facendo in modo che la scelta dell'ultima figura da disegnare sia la NoShape,
    ovvero, non si disegna niente. All'inizio si controlla se l'utente abbia eseguito un MouseDown su di un handler, evento
    che comporta il setting delle variabili: offsetDrag, hndlMV, pressPolyIn, createPI e il campo MV del poligono con l'handler
    cliccato. In seguito, l'utente può scegliere se disegnare un poligono il cui interno deve essere riempito da una figura,
    cioè il PolyIn; se viene scelto questo, allora se il numero di vertici era stato stabilito, fin quando il numero di MouseDown
    non è uguale al numero di vertici stabilito, si inserisce il punto il cui il MouseDown è avvenuto nell'array di punti del
    nuovo PolyIn, il quale viene aggiunto nell'array dei polyIn. Lo stesso procedimento viene utilizzato con il PolyOut che ha
    gli stessi compiti e proprietà del PolyIn eccetto quella che il suo interno deve essere escluso dalla figura; per quanto
    riguarda le ellissi, EllIn differisce da EllOut per lo stesso motivo per cui PolyIn differisce da PolyOut e si gestisce
    l'inserimento con un'ellissi dalle dimensioni predefinite, che viene aggiunta nell'array di ellissi, con due handler agli
    angoli, pronta per essere spostata e ridimensionata a piacimento dell'utente. *)
  override this.OnMouseDown e =
    base.OnMouseDown e
    hndlMV <- NoSh
    let mutable createPI = false
    let mutable createPO = false
    let mutable createEI = false
    let mutable createEO = false
    let mutable createTlkB = false
    let mutable createThgB = false
    let mutable createThgNB = false
    let mutable createScrmB = false

    let l = transformP v2w e.Location

    if not(endIns) then

      let hitHandlePoly(rp : ResizeArray<Polygon>, s : Shp) =
        let mutable pressed = (-1, -1)
        let mutable create = false
        if not(Seq.isEmpty(rp)) then
          create <- true
          for j in 0 .. 1 .. rp.Count - 1 do
            for i in 0 .. 1 .. rp.[j].Pts.Length - 1 do
              if handleHitTest l rp.[j].Pts.[i] then
                pressed <- (i, j)
                offsetDrag <- PointF(rp.[j].Pts.[i].X - l.X, rp.[j].Pts.[i].Y - l.Y)
                hndlMV <- s
                rp.[j].MV <- i
                this.Invalidate()
                create <- false
        else create <- true
        (create, pressed)

      let hitHandleEll(rp : ResizeArray<Ellipse>, s : Shp) =
        let mutable create = false
        let mutable pressed = (-1 , -1)
        if not(Seq.isEmpty(rp)) then
          create <- true
          for j in 0 .. 1 .. rp.Count - 1 do
            if handleHitTest l rp.[j].Location then
              pressed <- (1, j) (* 1 => punto iniziale *)
              offsetDrag <- PointF(rp.[j].Location.X - l.X, rp.[j].Location.Y - l.Y)
              rp.[j].MVi <- true
              rp.[j].MVf <- false
              hndlMV <- s
              create <- false
              this.Invalidate()
            elif handleHitTest l rp.[j].FinPt then
              pressed <- (2, j) (* 2 => punto finale *)
              offsetDrag <- PointF(rp.[j].FinPt.X - l.X, rp.[j].FinPt.Y - l.Y)
              rp.[j].MVf <- true
              rp.[j].MVi <- false
              hndlMV <- s
              create <- false
              this.Invalidate()
        else create <- true
        (create, pressed)

      let hitHandle(s : Shp) =
        match s with
        | PolyIn -> hitHandlePoly(polyIn, PolyIn)
        | PolyOut -> hitHandlePoly(polyOut, PolyOut)
        | EllIn -> hitHandleEll(ellIn, EllIn)
        | EllOut -> hitHandleEll(ellOut, EllOut)
        | NoSh -> (false, (-1, -1))

      let res = hitHandle(PolyIn)
      createPI <- fst res
      pressPolyIn <- snd res

      let res = hitHandle(PolyOut)
      createPO <- fst res
      pressPolyOut <- snd res

      let res = hitHandle(EllIn)
      createEI <- fst res
      pressEllIn <- snd res

      let res = hitHandle(EllOut)
      createEO <- fst res
      pressEllOut <- snd res

      if not(Seq.isEmpty(tlkb)) then
        createTlkB <- true
        for j in 0 .. 1 .. tlkb.Count - 1 do
          if handleHitTest l tlkb.[j].Location then
            offsetDrag <- PointF(tlkb.[j].Location.X - l.X, tlkb.[j].Location.Y - l.Y)
            tlkb.[j].MVi <- true
            tlkb.[j].MVf <- false
            tlkb.[j].MVt <- false
            selB <- (Speech, j)
            hndlMVB <- (Speech, j, tlkb.[j].Location)
            createTlkB <- false
          elif handleHitTest l tlkb.[j].FinPt then
            offsetDrag <- PointF(tlkb.[j].FinPt.X - l.X, tlkb.[j].FinPt.Y - l.Y)
            tlkb.[j].MVf <- true
            tlkb.[j].MVi <- false
            tlkb.[j].MVt <- false
            selB <- (Speech, j)
            hndlMVB <- (Speech, j, tlkb.[j].FinPt)
            createTlkB <- false
          elif handleHitTest l tlkb.[j].TlkPt then
            offsetDrag <- PointF(tlkb.[j].TlkPt.X - l.X, tlkb.[j].TlkPt.Y - l.Y)
            tlkb.[j].MVt <- true
            tlkb.[j].MVf <- false
            tlkb.[j].MVi <- false
            selB <- (Speech, j)
            hndlMVB <- (Speech, j, tlkb.[j].TlkPt)
            createTlkB <- false
          tlkb.[j].Invalidate()
        this.Invalidate()
      else createTlkB <- true

      if not(Seq.isEmpty(thgb)) then
        createThgB <- true
        for j in 0 .. 1 .. thgb.Count - 1 do
          if handleHitTest l thgb.[j].Location then
            offsetDrag <- PointF(thgb.[j].Location.X - l.X, thgb.[j].Location.Y - l.Y)
            thgb.[j].MVLocation <- true
            thgb.[j].MVFinPt <- false
            thgb.[j].MVThink <- false
            selB <- (Thought, j)
            hndlMVB <- (Thought, j, thgb.[j].Location)
            createThgB <- false
          elif handleHitTest l thgb.[j].FinPt then
            offsetDrag <- PointF(thgb.[j].FinPt.X - l.X, thgb.[j].FinPt.Y - l.Y)
            thgb.[j].MVFinPt <- true
            thgb.[j].MVThink <- false
            thgb.[j].MVLocation <- false
            selB <- (Thought, j)
            hndlMVB <- (Thought, j, thgb.[j].FinPt)
            createThgB <- false
          elif handleHitTest l thgb.[j].Think then
            offsetDrag <- PointF(thgb.[j].Think.X - l.X, thgb.[j].Think.Y - l.Y)
            thgb.[j].MVThink <- true
            thgb.[j].MVFinPt <- false
            thgb.[j].MVLocation <- false
            selB <- (Thought, j)
            hndlMVB <- (Thought, j, thgb.[j].Think)
            createThgB <- false
          thgb.[j].Invalidate()
        this.Invalidate()
      else createThgB <- true

      if not(Seq.isEmpty(thgnb)) then
        createThgNB <- true
        for j in 0 .. 1 .. thgnb.Count - 1 do
          if handleHitTest l thgnb.[j].Location then
            offsetDrag <- PointF(thgnb.[j].Location.X - l.X, thgnb.[j].Location.Y - l.Y)
            thgnb.[j].MVi <- true
            thgnb.[j].MVf <- false
            hndlMVB <- (ThoughtNarutoStyle, j, thgnb.[j].Location)
            selB <- (ThoughtNarutoStyle, j)
            createThgNB <- false
          elif handleHitTest l thgnb.[j].FinPt then
            offsetDrag <- PointF(thgnb.[j].FinPt.X - l.X, thgnb.[j].FinPt.Y - l.Y)
            thgnb.[j].MVi <- false
            thgnb.[j].MVf <- true
            hndlMVB <- (ThoughtNarutoStyle, j, thgnb.[j].FinPt)
            selB <- (ThoughtNarutoStyle, j)
            createThgNB <- false
          else
            thgnb.[j].MVi <- false
            thgnb.[j].MVf <- false
          thgnb.[j].Invalidate()
        this.Invalidate()
      else createThgNB <- true

      if not(Seq.isEmpty(scrmb)) then
        createScrmB <- true
        for j in 0 .. 1 .. scrmb.Count - 1 do
          let s = scrmb.[j]
          if handleHitTest l s.Location then
            offsetDrag <- PointF(s.Location.X - l.X, s.Location.Y - l.Y)
            s.MVi <- true
            createScrmB <- false
            hndlMVB <- (Scream, j, s.Location)
            selB <- (Scream, j)
            s.Invalidate()
          elif handleHitTest l s.FinPt then
            offsetDrag <- PointF(s.FinPt.X - l.X, s.FinPt.Y - l.Y)
            s.MVf <- true
            createScrmB <- false
            hndlMVB <- (Scream, j, s.FinPt)
            selB <- (Scream, j)
            s.Invalidate()
          elif s.Pts <> null then
            for i in 0 .. 1 .. s.Pts.Length - 1 do
              if handleHitTest l s.Pts.[i] then
                offsetDrag <- PointF(s.Pts.[i].X - l.X, s.Pts.[i].Y - l.Y)
                s.MVidx <- i
                createScrmB <- false
                hndlMVB <- (Scream, j, s.Pts.[i])
                selB <- (Scream, j)
                s.UpdScream()
                s.Invalidate()
                this.Invalidate()
        this.Invalidate()
      else createScrmB <- true
     
      let knd, idx, pnt = hndlMVB
      if hndlMV = NoSh && knd = NoBalloon then
        match drawShape with
        | PolyIn ->
          if createPI && pressbtnPI && not(pressX) then
            if Seq.isEmpty(polyIn) then
              let p = new Polygon(Parent=this, Incl=true, EndDraw=false, DrawingPoly=0)
              this.LWControls.Add(p)
              polyIn.Add(p)
              if vtxPolyIn <> -1 && not(pressX) then
                p.VtxPoly <- vtxPolyIn
                p.AddPt(transformP v2w e.Location)
                p.DrawingPoly <- p.DrawingPoly + 1
              updRegAndGP()
              this.Invalidate()
            else
              let p = polyIn.[polyIn.Count - 1]
              if p.DrawingPoly < p.VtxPoly && not(pressX) then
                let mutable diff = true
                for i in 0 .. 1 .. p.Pts.Length - 1 do
                  if (p.Pts.[i] = (transformP v2w e.Location)) then
                    diff <- false
                if diff then
                  p.AddPt(transformP v2w e.Location)
                  if p.DrawingPoly = p.VtxPoly - 1 then
                    p.AddPoly()
                    p.DrawingPoly <- p.VtxPoly
                    updRegAndGP()
                  else
                    p.DrawingPoly <- p.DrawingPoly + 1
                    getBounds()
                  this.Invalidate()
              (* altrimenti, se il numero dei vertici disegnati è uguale a quello dei vertici stabiliti, allora vuol dire
                  che si vuole aggiungere un nuovo poligono *)
              elif p.DrawingPoly = p.VtxPoly && not(pressX) then
                let p1 = new Polygon(Parent=this, Incl=true, EndDraw=false, DrawingPoly=0)
                this.LWControls.Add(p1)
                polyIn.Add(p1)
                if vtxPolyIn <> -1 then
                  p1.VtxPoly <- vtxPolyIn
                  p1.AddPt(transformP v2w e.Location)
                  p1.DrawingPoly <- p1.DrawingPoly + 1
                  updRegAndGP()
                this.Invalidate()
          hndlMV <- PolyIn
        | PolyOut ->
          if createPO && pressbtnPO then
            if Seq.isEmpty(polyOut) then
              let p = new Polygon(Parent=this, Incl=false, EndDraw=false, DrawingPoly=0)
              this.LWControls.Add(p)
              polyOut.Add(p)
              if vtxPolyOut <> -1 && not(pressX) then
                p.VtxPoly <- vtxPolyOut
                p.AddPt(transformP v2w e.Location)
                p.DrawingPoly <- p.DrawingPoly + 1
              this.Invalidate()
            else
              let p = polyOut.[polyOut.Count - 1]
              if p.DrawingPoly < p.VtxPoly && not(pressX) then
                let mutable diff = true
                for i in 0 .. 1 .. p.Pts.Length - 1 do
                  if (p.Pts.[i] = (transformP v2w e.Location)) then
                    diff <- false
                if diff then
                  p.AddPt(transformP v2w e.Location)
                  if p.DrawingPoly = p.VtxPoly - 1 then
                    p.AddPoly()
                    p.DrawingPoly <- p.VtxPoly
                    updRegAndGP()
                  else
                    p.DrawingPoly <- p.DrawingPoly + 1
                    getBounds()
                  this.Invalidate()
              (* altrimenti, se il numero dei vertici disegnati è uguale a quello dei vertici stabiliti, allora vuol dire
                  che si vuole aggiungere un nuovo poligono *)
              elif p.DrawingPoly = p.VtxPoly && not(pressX) then
                let p1 = new Polygon(Parent=this, Incl=false, EndDraw=false, DrawingPoly=0)
                this.LWControls.Add(p1)
                polyOut.Add(p1)
                if vtxPolyOut <> -1 then
                  p1.VtxPoly <- vtxPolyOut
                  p1.AddPt(transformP v2w e.Location)
                  p1.DrawingPoly <- p1.DrawingPoly + 1
                  updRegAndGP()
                this.Invalidate()
          hndlMV <- PolyOut
        | EllIn ->
          if createEI && pressbtnEI then
            let el = new Ellipse(Parent=this, Incl=true, EndDraw=false)
            el.AddEll(transformP v2w e.Location)
            ellIn.Add(el)
            this.LWControls.Add(el)
            updRegAndGP()
            this.Invalidate()
          hndlMV <- EllIn
        | EllOut ->
          if createEO && pressbtnEO then
            let el = new Ellipse(Parent=this, Incl=false, EndDraw=false)
            el.AddEll(transformP v2w e.Location)
            ellOut.Add(el)
            this.LWControls.Add(el)
            updRegAndGP()
            this.Invalidate()
          hndlMV <- EllOut
        | NoSh -> ()
        if polyIn.Count <> 0 then
          let p = polyIn.[polyIn.Count - 1]
          if p.DrawingPoly < p.VtxPoly && drawShape <> PolyIn && not(pressX) then
            this.LWControls.Remove(polyIn.[polyIn.Count - 1]) |> ignore
            polyIn.RemoveAt(polyIn.Count - 1) |> ignore
        if polyOut.Count <> 0 then
          let p = polyOut.[polyOut.Count - 1]
          if p.DrawingPoly < p.VtxPoly && drawShape <> PolyOut && not(pressX) then
            this.LWControls.Remove(polyOut.[polyOut.Count - 1]) |> ignore
            polyOut.RemoveAt(polyOut.Count - 1) |> ignore
        (* potrebbe andare all'interno del ramo NoSh -> () *)
      let knd, idx, pnt = hndlMVB
      if hndlMV = NoSh && knd = NoBalloon && drawShape = NoSh then
        match drawBalloon with
        | Speech | Whisper ->
          if createTlkB && pressbtnTlkB then
            let tb = new TalkingBalloon(Parent=this, EndDraw=false)
            tb.AddTalk((transformP v2w e.Location), SizeF(40.f, 50.f), isWhisper)
            tlkb.Add(tb)
            this.LWControls.Add(tb)
            selB <- (Speech, tlkb.Count - 1)
            this.Invalidate()
        | Thought ->
          if createThgB && pressbtnThgB then
            let tb = new ThoughtBalloon(Parent=this, EndDraw=false)
            tb.AddThink((transformP v2w e.Location), SizeF(40.f, 50.f))
            thgb.Add(tb)
            this.LWControls.Add(tb)
            selB <- (Thought, thgb.Count - 1)
            this.Invalidate()
        | ThoughtNarutoStyle ->
          if createThgNB && pressbtnThgNB then
            let tb = new ThoughtNarutoBalloon(Parent=this, EndDraw=false)
            tb.AddThink((transformP v2w e.Location), SizeF(40.f, 50.f), "Preview")
            thgnb.Add(tb)
            this.LWControls.Add(tb)
            selB <- (ThoughtNarutoStyle, thgnb.Count - 1)
            this.Invalidate()
        | Scream ->
          if createScrmB && pressbtnScrmB then
            if scrmb.Count = 0 then
              let sb = new ScreamBalloon(Parent=this, EndDraw=false, Closed=false)
              sb.AddScream((transformP v2w e.Location), SizeF(80.f, 100.f))
              scrmb.Add(sb)
              this.LWControls.Add(sb)
              selB <- (Scream, scrmb.Count - 1)
              sb.Invalidate()
              this.Invalidate()
            else
              let s = scrmb.[scrmb.Count - 1]
              if not(s.Closed) && not(pressX) then
                if s.MVidx = -1 then
                  s.AddPt(transformP v2w e.Location)
                  s.Invalidate()
                  this.Invalidate()
              elif s.MVidx = -1 && s.Closed && not(pressX) && s.Pts.Length >= minVtx4Scrm then
                let sb1 = new ScreamBalloon(Parent=this, EndDraw=false, Closed=false)
                sb1.AddScream((transformP v2w e.Location), SizeF(40.f, 50.f))
                this.LWControls.Add(sb1)
                scrmb.Add(sb1)
                sb1.Invalidate()
                this.Invalidate()
        | NoBalloon -> ()
      if scrmb.Count <> 0 then
        let s = scrmb.[scrmb.Count - 1]
        if (not(s.Closed) || s.Pts.Length < minVtx4Scrm) && drawBalloon <> Scream && not(pressX) then
          this.LWControls.Remove(scrmb.[scrmb.Count - 1]) |> ignore
          scrmb.RemoveAt(scrmb.Count - 1) |> ignore
      
      if text <> "" then
        let knd, idx = selB
        if knd <> NoBalloon then
          match knd with
          | Speech | Whisper ->
            tlkb.[idx].SetStyleFont <- styleFont
            tlkb.[idx].FontSize <- fontSize
            tlkb.[idx].UpdTalk(text)
            tlkb.[idx].Invalidate()
          | Thought ->
            thgb.[idx].SetStyleFont <- styleFont
            thgb.[idx].FontSize <- fontSize
            thgb.[idx].UpdThink(text)
            thgb.[idx].Invalidate()
          | ThoughtNarutoStyle ->
            thgnb.[idx].SetStyleFont <- styleFont
            thgnb.[idx].FontSize <- fontSize
            thgnb.[idx].UpdThink(text)
            thgnb.[idx].Invalidate()
          | Scream ->
            scrmb.[idx].SetStyleFont <- styleFont
            scrmb.[idx].FontSize <- fontSize
            scrmb.[idx].UpdScream(text)
            scrmb.[idx].Invalidate()
          | NoBalloon -> ()
          text <- ""
      this.Invalidate()

  override this.OnMouseMove e =
    base.OnMouseMove e
    let hlp() =
      updRegAndGP()
      this.Invalidate()

    let l = transformP v2w e.Location

    (* se hndlMV è diverso da NoShape, cioè se una maniglietta è stata selezionata, si aggiorna la relativa figura *)
    if hndlMV <> NoSh then
      let wch_poly = snd pressPolyIn
      if wch_poly <> -1 then
        let wch_pt = fst pressPolyIn
        if wch_pt <> -1 then
          polyIn.[wch_poly].Pts.[wch_pt] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
          polyIn.[wch_poly].Update()
          hlp()
      else
        let wch_poly = snd pressPolyOut
        if wch_poly <> -1 then
          let wch_pt = fst pressPolyOut
          if wch_pt <> -1 then
            polyOut.[wch_poly].Pts.[wch_pt] <- PointF(l.X + offsetDrag.X, l.Y + offsetDrag.Y)
            polyOut.[wch_poly].Update()
            hlp()
        else
          let wch_ell = snd pressEllIn
          if wch_ell <> -1 then
            if fst pressEllIn = 1 then
              if offsetDrag.X + l.X <= ellIn.[wch_ell].FinPt.X - 10.f &&
                offsetDrag.Y + l.Y <= ellIn.[wch_ell].FinPt.Y - 10.f then
                  ellIn.[wch_ell].Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
                  ellIn.[wch_ell].Update()
                  hlp()
            elif fst pressEllIn = 2 then
              if offsetDrag.X + l.X >= ellIn.[wch_ell].Location.X + 10.f &&
                offsetDrag.Y + l.Y >= ellIn.[wch_ell].Location.Y + 10.f then
                  ellIn.[wch_ell].FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
                  ellIn.[wch_ell].Update()
                  hlp()
          else
            let wch_ell = snd pressEllOut
            if wch_ell <> -1 then
              if fst pressEllOut = 1 then
                ellOut.[wch_ell].Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
                ellOut.[wch_ell].Update()
                hlp()
              elif fst pressEllOut = 2 then
                ellOut.[wch_ell].FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
                ellOut.[wch_ell].Update()
                hlp()
      
    else
      let knd, idx, MVvtx = hndlMVB
      if MVvtx <> PointF(-1.f, -1.f) then
        match knd with

        | Speech | Whisper ->
          let hlp(i) =
            selB <- (Speech, i)
            tlkb.[i].Update()
            tlkb.[i].Invalidate()
          if tlkb.[idx].MVi then
            tlkb.[idx].Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx)
          elif tlkb.[idx].MVf then
            tlkb.[idx].FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx)
          elif tlkb.[idx].MVt then
            tlkb.[idx].TlkPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx)

        | Thought ->
          let hlp(i, b) =
            selB <- (Thought, i)
            thgb.[i].Update()
            thgb.[i].Invalidate()
          if thgb.[idx].MVLocation then
            thgb.[idx].Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx, true)
          elif thgb.[idx].MVFinPt then
            thgb.[idx].FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx, false)
          elif thgb.[idx].MVThink then
            thgb.[idx].Think <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            thgb.[idx].Update()
            thgb.[idx].Invalidate()

        | ThoughtNarutoStyle ->
          let hlp(i) =
            selB <- (ThoughtNarutoStyle, i)
            thgnb.[i].Update()
            thgnb.[i].Invalidate()
          if thgnb.[idx].MVi then
            thgnb.[idx].Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx)
          elif thgnb.[idx].MVf then
            thgnb.[idx].FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
            hlp(idx)

        | Scream ->
          let s = scrmb.[idx]
          if s.MVi then s.Location <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
          elif s.MVf then s.FinPt <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
          elif s.MVidx <> -1 then s.Pts.[s.MVidx] <- PointF(offsetDrag.X + l.X, offsetDrag.Y + l.Y)
          selB <- (Scream, idx)
          s.UpdScream()
          s.Invalidate()

        | NoBalloon -> ()
        this.Invalidate()
  
  override this.OnMouseUp e =
    base.OnMouseUp e
    let mutable canceled = false

    if hndlMV = NoSh then
      let knd, idx, MVvtx = hndlMVB
      match knd with

      | Speech | Whisper ->
        let t = tlkb.[idx]
        if t.MVi then t.MVi <- false
        elif t.MVf then t.MVf <- false
        elif t.MVt then t.MVt <- false
        t.Invalidate()

      | Thought ->
        let t = thgb.[idx]
        if t.MVLocation then t.MVLocation <- false
        elif t.MVFinPt then t.MVFinPt <- false
        elif t.MVThink then t.MVThink <- false
        t.Invalidate()

      | ThoughtNarutoStyle ->
        let t = thgnb.[idx]
        if t.MVi then t.MVi <- false
        elif t.MVf then t.MVf <- false
        t.Invalidate()

      | Scream ->
        let s = scrmb.[idx]
        if s.MVi then s.MVi <- false
        elif s.MVf then s.MVf <- false
        elif idx <> -1 then s.MVidx <- -1
        s.Invalidate()

      | NoBalloon -> ()

      if pressX then
        if knd <> NoBalloon then
          deleteB(knd, idx)
          selB <- (NoBalloon, -1)
        hndlMVB <- (NoBalloon, -1, new PointF(-1.f, -1.f))

    if (hndlMV <> NoSh) && pressX then
      deleteSh()
      pressPolyIn <- (-1, -1)
      pressPolyOut <- (-1, -1)
      pressEllIn <- (-1, -1)
      pressEllOut <- (-1, -1)
      hndlMV <- NoSh
      canceled <- true

    if (hndlMV <> NoSh) && not(canceled) then
      let mutable wch_poly = snd pressPolyIn
      let mutable wch_pt = fst pressPolyIn
      if wch_poly <> -1 then polyIn.[wch_poly].MV <- -1
      pressPolyIn <- (-1, -1)

      wch_poly <- snd pressPolyOut
      wch_pt <- fst pressPolyOut
      if wch_poly <> -1 then polyOut.[wch_poly].MV <- -1
      pressPolyOut <- (-1, -1)

      let mutable sp = snd pressEllIn
      if sp <> -1 then
        ellIn.[sp].MVi <- false
        ellIn.[sp].MVf <- false
      pressEllIn <- (-1, -1)

      sp <- snd pressEllOut
      if sp <> -1 then
        ellOut.[sp].MVi <- false
        ellOut.[sp].MVf <- false
      pressEllOut <- (-1, -1)
    
    if endDraw && hndlMV = NoSh then
      if regToon <> null then
        if correlateImg w2v e.Location then
          loadImg()

    hndlMV <- NoSh
    hndlMVB <- (NoBalloon, -1, new PointF(-1.f, -1.f))

    this.Invalidate()

  override this.OnKeyDown e =
    base.OnKeyDown e
    this.HandleCommand(e.KeyCode)

  override this.OnKeyUp e =
    base.OnKeyUp e
    if e.KeyCode = Keys.X then pressX <- false

  override this.OnPaintBackground e = ()

  override this.OnPaint e =
    base.OnPaint e
    updateBuffer()
    let g = Graphics.FromImage(buffer)
    g.SmoothingMode <- SmoothingMode.HighQuality
    let vg = e.Graphics
    use bg = new SolidBrush(this.BackColor)
    g.FillRectangle(bg, 0, 0, buffer.Width, buffer.Height)
    let save = g.Save()
    g.MultiplyTransform(w2v)
    let hsave = h
    let mk_rct (pnt : PointF) = RectangleF(pnt.X - h, pnt.Y - h, 2.f * h, 2.f * h)

    if gp <> null && not(endIns) then g.DrawPath(Pens.Black, gp)
    if regToon <> null && img = null then g.FillRegion(Brushes.White, regToon)

    if endDraw && regToon <> null then
      let s = g.Save()      
      g.Clip <- regToon

      if img <> null then
        if not(polyIn.Count = 0 && polyOut.Count = 0 && ellIn.Count = 0 && ellOut.Count = 0) then
          if size.Width > 0.f && size.Height > 0.f then
            g.DrawImage(img, RectangleF(location, size))
      g.Restore(s) 

    if location <> PointF() && size <> SizeF() && not(finished) then
      g.DrawRectangle(Pens.Salmon, Rectangle(Point(int location.X, int location.Y), Size(int size.Width, int size.Height)))

    let ctx = g.Save()

    if not(endIns) then
      if not(Seq.isEmpty(polyIn)) then
        let p = polyIn.[polyIn.Count - 1]
        if not(p.EndDraw) then
          if p.DrawingPoly > 0 && p.DrawingPoly < p.VtxPoly then
            h <- 10.f
            for i in 0 .. 1 .. p.Pts.Length - 1 do
              if not(pressX) then
                g.DrawEllipse(Pens.Green, mk_rct(p.Pts.[i]))
      if not(Seq.isEmpty(polyOut)) then
        let p = polyOut.[polyOut.Count - 1]
        if not(p.EndDraw) then
          if p.DrawingPoly > 0 && p.DrawingPoly < p.VtxPoly then  
            h <- 10.f
            for i in 0 .. 1 .. p.Pts.Length - 1 do
              if not(pressX) then
                g.DrawEllipse(Pens.Red, mk_rct(p.Pts.[i]))
      g.Restore(ctx)

      h <- hsave
   
      if polyIn <> null then
        let s = g.Save()
        g.Transform <- w2v
        for i in polyIn.Count - 1 .. -1 .. 0 do
          let p = polyIn.[i]
          if p.EndIns then
            for pt in p.Pts do
              g.DrawEllipse(Pens.Green, mk_rct(pt))
            if p.MV <> -1 && hndlMV = PolyIn then
              g.FillEllipse(Brushes.DarkGreen, mk_rct(p.Pts.[p.MV]))
        g.Restore(s)

      if polyOut <> null then
        let s = g.Save()
        g.Transform <- w2v
        for i in polyOut.Count - 1 .. -1 .. 0 do
          let p = polyOut.[i]
          if p.EndIns then
            for pt in p.Pts do
              g.DrawEllipse(Pens.Red, mk_rct(pt))
            if p.MV <> -1 && hndlMV = PolyOut then
              g.FillEllipse(Brushes.DarkRed, mk_rct(p.Pts.[p.MV]))
        g.Restore(s)

      if ellIn <> null then
        let s = g.Save()
        g.Transform <- w2v
        for i in ellIn.Count - 1 .. -1 .. 0 do
          let e = ellIn.[i]
          if not(e.EndIns) then
            g.DrawEllipse(Pens.Green, mk_rct(e.Location))
            g.DrawEllipse(Pens.Green, mk_rct(e.FinPt))
            if fst pressEllIn = 1 && hndlMV = EllIn then
              g.FillEllipse(Brushes.DarkGreen, mk_rct(ellIn.[snd pressEllIn].Location))
            elif fst pressEllIn = 2 && hndlMV = EllIn then
              g.FillEllipse(Brushes.DarkGreen, mk_rct(ellIn.[snd pressEllIn].FinPt))
        g.Restore(s)

      if ellOut <> null then
        let s = g.Save()
        g.Transform <- w2v
        for i in ellOut.Count - 1 .. -1 .. 0 do
          let e = ellOut.[i]
          if not(e.EndIns) then
            g.DrawEllipse(Pens.Red, mk_rct(e.Location))
            g.DrawEllipse(Pens.Red, mk_rct(e.FinPt))
            if fst pressEllOut = 1 && hndlMV = EllOut then
              g.FillEllipse(Brushes.DarkRed, mk_rct(ellOut.[snd pressEllOut].Location))
            elif fst pressEllOut = 2 && hndlMV = EllOut then
              g.FillEllipse(Brushes.DarkRed, mk_rct(ellOut.[snd pressEllOut].FinPt))
        g.Restore(s)
    g.Restore(save)
    vg.DrawImage(buffer, 0, 0)

    if tlkb <> null then
      let s = g.Save()
      for i in tlkb.Count - 1 .. -1 .. 0 do
        tlkb.[i].GraphicsTransform <- w2v
        if endIns then tlkb.[i].EndDraw <- true
        else tlkb.[i].EndDraw <- false
        tlkb.[i].OnPaint e
      g.Restore(s)

    if thgb <> null then
      let s = g.Save()
      g.Transform <- w2v
      for i in thgb.Count - 1 .. -1 .. 0 do
        thgb.[i].GraphicsTransform <- w2v
        if endIns then thgb.[i].EndDraw <- true
        else thgb.[i].EndDraw <- false
        thgb.[i].OnPaint e
      g.Restore(s)

    if thgnb <> null then
      let s = g.Save()
      g.Transform <- w2v
      for i in thgnb.Count - 1 .. -1 .. 0 do
        thgnb.[i].GraphicsTransform <- w2v
        if endIns then thgnb.[i].EndDraw <- true
        else thgnb.[i].EndDraw <- false
        thgnb.[i].OnPaint e
      g.Restore(s)

    if scrmb <> null then
      let s = g.Save()
      g.Transform <- w2v
      for i in scrmb.Count - 1 .. -1 .. 0 do
        scrmb.[i].GraphicsTransform <- w2v
        if endIns then scrmb.[i].EndDraw <- true
        else scrmb.[i].EndDraw <- false
        scrmb.[i].OnPaint e
      g.Restore(s)

    if not(endIns) then g.DrawRectangle(Pens.Salmon, location.X, location.Y, size.Width, size.Height)